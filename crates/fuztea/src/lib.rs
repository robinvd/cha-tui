use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use chatui::buffer::CellAttributes;
use chatui::components::input::{InputMsg, InputState, InputStyle};
use chatui::components::virtual_list::{
    VirtualListAction, VirtualListEvent, VirtualListState,
    default_keybindings as default_list_keybindings, virtual_list,
};
use chatui::dom::{Color, Node, Style, TextSpan, column, row, text as dom_text};
use chatui::event::{Event, Key, KeyCode, Size};
use chatui::program::TaskFn;
use chatui::render::RenderContext;
use chatui::{ScrollMsg, block_with_title, default_input_keybindings, input};
use nucleo::pattern::{CaseMatching, Normalization};
use nucleo::{Config, Injector, Matcher, Nucleo, Utf32String};
use smol::channel;
use taffy::prelude::TaffyZero;
use taffy::style::Dimension;
use unicode_width::UnicodeWidthChar;

const HEADER_HEIGHT: usize = 1;
const INPUT_HEIGHT: usize = 3;
const FOOTER_HEIGHT: usize = 1;

/// Configuration for multi-column fuzzy finding.
#[derive(Clone, Debug)]
pub struct ColumnConfig {
    /// Separator to split input lines into columns (default: "\t")
    pub separator: String,
    /// Column names for header display (optional)
    pub column_names: Vec<String>,
    /// Which columns to search (1-indexed, empty means all). Similar to fzf --nth.
    pub search_columns: Vec<usize>,
}

impl Default for ColumnConfig {
    fn default() -> Self {
        Self {
            separator: "\t".to_string(),
            column_names: Vec::new(),
            search_columns: Vec::new(),
        }
    }
}

impl ColumnConfig {
    pub fn with_separator(mut self, sep: impl Into<String>) -> Self {
        self.separator = sep.into();
        self
    }

    pub fn with_column_names(mut self, names: Vec<String>) -> Self {
        self.column_names = names;
        self
    }

    pub fn with_search_columns(mut self, cols: Vec<usize>) -> Self {
        self.search_columns = cols;
        self
    }

    /// Returns the number of matcher columns based on search_columns or column_names
    pub fn matcher_column_count(&self) -> usize {
        if !self.search_columns.is_empty() {
            self.search_columns.len()
        } else if !self.column_names.is_empty() {
            self.column_names.len()
        } else {
            1
        }
    }

    /// Map a column name to its index (1-indexed for user, 0-indexed internally)
    pub fn column_name_to_index(&self, name: &str) -> Option<usize> {
        self.column_names
            .iter()
            .position(|n| n.eq_ignore_ascii_case(name))
    }
}

/// Parsed query with per-column search terms.
/// Supports helix-style syntax: "main search %column other search"
#[derive(Clone, Debug, Default)]
pub struct ParsedQuery {
    /// Map from column index (0-indexed) to search term
    pub column_queries: Vec<(usize, String)>,
}

impl ParsedQuery {
    /// Parse a query string with %column_name syntax.
    /// Returns queries per column. Column 0 is the default.
    pub fn parse(query: &str, config: &ColumnConfig) -> Self {
        let mut result = ParsedQuery::default();
        let mut current_col = 0usize;
        let mut current_query = String::new();

        let parts: Vec<&str> = query.split_whitespace().collect();
        for part in parts {
            if let Some(col_name) = part.strip_prefix('%') {
                if !current_query.is_empty() {
                    result
                        .column_queries
                        .push((current_col, current_query.trim().to_string()));
                    current_query.clear();
                }
                if let Some(idx) = config.column_name_to_index(col_name) {
                    current_col = idx;
                }
            } else {
                if !current_query.is_empty() {
                    current_query.push(' ');
                }
                current_query.push_str(part);
            }
        }

        if !current_query.is_empty() {
            result
                .column_queries
                .push((current_col, current_query.trim().to_string()));
        }

        result
    }

    pub fn is_empty(&self) -> bool {
        self.column_queries.is_empty() || self.column_queries.iter().all(|(_, q)| q.is_empty())
    }
}

/// A function that extracts searchable columns from an item of type T.
/// Similar to nucleo's approach, this function takes a reference to an item
/// and returns a vector of strings to search across (one per column).
/// This is also used for display rendering.
///
/// # Example
/// ```ignore
/// fn extract_file_columns(file: &FileEntry) -> Vec<String> {
///     vec![
///         file.name.clone(),
///         file.path.clone(),
///     ]
/// }
/// ```
pub type ColumnExtractor<T> = fn(&T) -> Vec<String>;

#[derive(Clone)]
pub struct FuzzyFinderInput<T> {
    injector: Injector<T>,
    extractor: ColumnExtractor<T>,
    search_columns: Vec<usize>,
    num_cols: usize,
}

impl<T: Clone + Send + Sync + 'static> FuzzyFinderInput<T> {
    pub fn push_item(&self, item: T) {
        let extractor = self.extractor;
        let search_columns = self.search_columns.clone();
        let num_cols = self.num_cols;

        self.injector.push(item, move |item_ref, cols| {
            let columns = extractor(item_ref);

            if search_columns.is_empty() {
                // No search_columns filtering: pass all extracted columns
                for (i, col) in cols.iter_mut().enumerate().take(num_cols) {
                    *col = Utf32String::from(columns.get(i).map(|s| s.as_str()).unwrap_or(""));
                }
            } else {
                // With search_columns: only pass the specified columns, remapped
                for (output_idx, &input_col_idx) in search_columns.iter().enumerate().take(num_cols)
                {
                    let input_idx = input_col_idx.saturating_sub(1);
                    cols[output_idx] =
                        Utf32String::from(columns.get(input_idx).map(|s| s.as_str()).unwrap_or(""));
                }
            }
        });
    }
}

#[derive(Clone)]
pub struct FuzzyFinderHandle<T> {
    input: FuzzyFinderInput<T>,
}

impl<T: Clone + Send + Sync + 'static> FuzzyFinderHandle<T> {
    pub fn push_item(&self, item: T) {
        self.input.push_item(item);
    }
}

pub struct FuzzyFinder<T: Send + Sync + 'static> {
    matcher: Rc<RefCell<Nucleo<T>>>,
    injector: Injector<T>,
    extractor: ColumnExtractor<T>,
    config: Config,
    column_config: ColumnConfig,
    input: InputState,
    input_style: InputStyle,
    matched_count: usize,
    notify_rx: channel::Receiver<()>,
    list: VirtualListState,
    last_query: String,
    listening_matcher: bool,
    size: Size,
    submitted: Option<T>,
}

#[derive(Clone, Debug)]
pub enum FuzzyFinderMsg {
    KeyPressed(Key),
    Input(InputMsg),
    List(VirtualListAction),
    MatcherTick,
    MatcherListenerClosed,
    Resize(Size),
}

pub fn map_event(event: Event) -> Option<FuzzyFinderMsg> {
    match event {
        Event::Key(key) => Some(FuzzyFinderMsg::KeyPressed(key)),
        Event::Resize(size) => Some(FuzzyFinderMsg::Resize(size)),
        _ => None,
    }
}

impl<T: Clone + Send + Sync + PartialEq + 'static> FuzzyFinder<T> {
    pub fn new(extractor: ColumnExtractor<T>) -> (Self, FuzzyFinderHandle<T>) {
        Self::with_config(extractor, ColumnConfig::default())
    }

    pub fn with_config(
        extractor: ColumnExtractor<T>,
        column_config: ColumnConfig,
    ) -> (Self, FuzzyFinderHandle<T>) {
        let config = Config::DEFAULT;
        let (notify_tx, notify_rx) = channel::bounded(1);
        let notify = Arc::new(move || {
            let _ = notify_tx.try_send(());
        });
        let num_cols = column_config.matcher_column_count() as u32;
        let matcher = Nucleo::new(config.clone(), notify, None, num_cols);
        let injector = matcher.injector();

        let input = FuzzyFinderInput {
            injector: injector.clone(),
            extractor,
            search_columns: column_config.search_columns.clone(),
            num_cols: column_config.matcher_column_count(),
        };
        let finder = Self {
            injector,
            matcher: Rc::new(RefCell::new(matcher)),
            extractor,
            config,
            column_config,
            input: InputState::new(),
            input_style: InputStyle::default(),
            matched_count: 0,
            notify_rx,
            list: VirtualListState::new(1),
            last_query: String::new(),
            listening_matcher: false,
            size: Size {
                width: 80,
                height: 24,
            },
            submitted: None,
        };

        let handle = FuzzyFinderHandle { input };
        (finder, handle)
    }

    #[cfg(test)]
    pub fn new_with_items(items: Vec<String>) -> (FuzzyFinder<String>, FuzzyFinderHandle<String>) {
        FuzzyFinder::<String>::new_with_items_and_config(items, ColumnConfig::default())
    }

    #[cfg(test)]
    pub fn new_with_items_and_config(
        items: Vec<String>,
        config: ColumnConfig,
    ) -> (FuzzyFinder<String>, FuzzyFinderHandle<String>) {
        // Use a simple extractor that just returns the full string
        let (mut finder, handle) =
            FuzzyFinder::<String>::with_config(test_string_extractor, config);
        for item in items {
            handle.push_item(item);
        }
        finder.refresh_matches(true);
        (finder, handle)
    }

    pub fn set_query(&mut self, query: impl Into<String>) {
        self.input.set_value(query.into());
        let _ = self.refresh_matches(true);
    }

    pub fn tick(&mut self) {
        let _ = self.refresh_matches(true);
    }

    pub fn submission(&self) -> Option<&T> {
        self.submitted.as_ref()
    }

    pub fn column_config(&self) -> &ColumnConfig {
        &self.column_config
    }

    fn apply_input(&mut self, msg: InputMsg) -> bool {
        if self.input.update(msg) {
            return self.refresh_matches(true);
        }
        false
    }

    fn refresh_matches(&mut self, reset_selection: bool) -> bool {
        let previous_selection = self.current_selection();
        let query = self.input.value();
        let query_changed = query != self.last_query;
        let matched_count = {
            let mut matcher = self.matcher.borrow_mut();
            if query_changed {
                let (case_matching, normalization) = self.matching_options();
                let parsed = ParsedQuery::parse(&query, &self.column_config);

                for col_idx in 0..self.column_config.matcher_column_count() {
                    let col_query: String = parsed
                        .column_queries
                        .iter()
                        .filter(|(c, _)| *c == col_idx)
                        .map(|(_, q)| q.as_str())
                        .collect::<Vec<_>>()
                        .join(" ");

                    let append = query.starts_with(&self.last_query) && !col_query.is_empty();
                    matcher.pattern.reparse(
                        col_idx,
                        &col_query,
                        case_matching,
                        normalization,
                        append,
                    );
                }
                self.last_query = query;
            }

            matcher.tick(10);
            if self.last_query.is_empty() {
                self.injector.injected_items() as usize
            } else {
                matcher.snapshot().matched_item_count() as usize
            }
        };
        self.matched_count = matched_count;

        if reset_selection || query_changed {
            self.list.reset();
        }
        let selection_event = self.list.set_item_count(matched_count);
        let selection_changed = selection_event.is_some();

        let current_selection = self.current_selection();
        selection_changed || current_selection != previous_selection
    }

    fn matching_options(&self) -> (CaseMatching, Normalization) {
        let case_matching = if self.config.ignore_case {
            CaseMatching::Ignore
        } else {
            CaseMatching::Respect
        };
        let normalization = if self.config.normalize {
            Normalization::Smart
        } else {
            Normalization::Never
        };
        (case_matching, normalization)
    }

    fn list_height(&self) -> usize {
        let height = self.size.height as usize;
        let header_extra = if self.column_config.column_names.is_empty() {
            0
        } else {
            1
        };
        height.saturating_sub(HEADER_HEIGHT + INPUT_HEIGHT + FOOTER_HEIGHT + header_extra)
    }

    fn update_list_viewport(&mut self) {
        let view_height = self.list_height();
        let _ = self
            .list
            .update(VirtualListAction::Scroll(ScrollMsg::Resize {
                viewport: Size {
                    width: self.size.width,
                    height: view_height.min(u16::MAX as usize) as u16,
                },
                content: Size {
                    width: self.size.width,
                    height: self.matched_count.min(u16::MAX as usize) as u16,
                },
            }));
    }

    fn submit_selection(&mut self) -> bool {
        let selected = self.list.selection();
        let item = if self.last_query.is_empty() {
            self.injector
                .get(selected as u32)
                .map(|item| item.data.clone())
        } else {
            let matcher = self.matcher.borrow();
            matcher
                .snapshot()
                .get_matched_item(selected as u32)
                .map(|item| item.data.clone())
        };

        let Some(item) = item else {
            return false;
        };

        self.submitted = Some(item);
        true
    }

    pub fn current_selection(&self) -> Option<T> {
        let selected = self.list.selection();
        if self.last_query.is_empty() {
            self.injector
                .get(selected as u32)
                .map(|item| item.data.clone())
        } else {
            let matcher = self.matcher.borrow();
            matcher
                .snapshot()
                .get_matched_item(selected as u32)
                .map(|item| item.data.clone())
        }
    }
}

pub enum FuzzyFinderEvent<T, Msg> {
    Continue,
    Cancel,
    Select(Option<T>),
    Activate,
    Task(TaskFn<Msg>),
}

pub fn update<T, Msg>(
    model: &mut FuzzyFinder<T>,
    msg: FuzzyFinderMsg,
    map_msg: impl Fn(FuzzyFinderMsg) -> Msg + 'static,
) -> FuzzyFinderEvent<T, Msg>
where
    T: Clone + Send + Sync + PartialEq + 'static,
    Msg: 'static,
{
    match msg {
        FuzzyFinderMsg::KeyPressed(key) => return handle_key(model, key),
        FuzzyFinderMsg::Input(input_msg) => {
            if model.apply_input(input_msg) {
                return FuzzyFinderEvent::Select(model.submitted.clone());
            }
        }
        FuzzyFinderMsg::List(action) => return handle_list_action(model, action),
        FuzzyFinderMsg::MatcherTick => {
            model.refresh_matches(false);
            return FuzzyFinderEvent::Task(listen_for_matcher_updates(model, map_msg));
        }
        FuzzyFinderMsg::MatcherListenerClosed => {}
        FuzzyFinderMsg::Resize(size) => {
            model.size = size;
            model.update_list_viewport();

            if !model.listening_matcher {
                model.listening_matcher = true;
                return FuzzyFinderEvent::Task(listen_for_matcher_updates(model, map_msg));
            }
        }
    };
    FuzzyFinderEvent::Continue
}

fn listen_for_matcher_updates<T, Msg>(
    model: &FuzzyFinder<T>,
    map_msg: impl Fn(FuzzyFinderMsg) -> Msg + 'static,
) -> TaskFn<Msg>
where
    T: Send + Sync + 'static,
    Msg: 'static,
{
    let receiver = model.notify_rx.clone();
    Box::pin(async move {
        map_msg(match receiver.recv().await {
            Ok(()) => FuzzyFinderMsg::MatcherTick,
            Err(_) => FuzzyFinderMsg::MatcherListenerClosed,
        })
    })
}

fn handle_key<T, Msg>(model: &mut FuzzyFinder<T>, key: Key) -> FuzzyFinderEvent<T, Msg>
where
    T: Clone + Send + Sync + PartialEq + 'static,
{
    if key.ctrl && matches!(key.code, KeyCode::Char('c') | KeyCode::Char('q')) {
        return FuzzyFinderEvent::Cancel;
    }

    match key.code {
        KeyCode::Esc => FuzzyFinderEvent::Cancel,
        _ => {
            if let Some(action) = default_list_keybindings(key, |msg| msg) {
                return handle_list_action(model, action);
            }
            if let Some(input_msg) = default_input_keybindings(&model.input, key, |msg| msg)
                && model.apply_input(input_msg)
            {
                return FuzzyFinderEvent::Select(model.submitted.clone());
            }
            FuzzyFinderEvent::Continue
        }
    }
}

fn handle_list_action<T, Msg>(
    model: &mut FuzzyFinder<T>,
    action: VirtualListAction,
) -> FuzzyFinderEvent<T, Msg>
where
    T: Clone + Send + Sync + PartialEq + 'static,
{
    let event = model.list.update(action);
    match event {
        Some(VirtualListEvent::Activated(_)) => {
            if model.submit_selection() {
                FuzzyFinderEvent::Activate
            } else {
                FuzzyFinderEvent::Continue
            }
        }
        Some(VirtualListEvent::SelectionChanged(_)) => {
            FuzzyFinderEvent::Select(model.submitted.clone())
        }
        None => FuzzyFinderEvent::Continue,
    }
}

pub fn view<T, Msg>(
    model: &FuzzyFinder<T>,
    map_msg: impl Fn(FuzzyFinderMsg) -> Msg + Clone + 'static,
) -> Node<Msg>
where
    T: Clone + Send + Sync + 'static,
    Msg: 'static,
{
    let header_style = Style {
        fg: Some(Color::BrightBlack),
        ..Style::default()
    };
    let header = dom_text::<Msg>("fuztea  Esc quits  Enter selects  Ctrl-n/p move  Ctrl-d/u page")
        .with_style(header_style)
        .with_flex_shrink(0.);

    let input_label = dom_text::<Msg>("> ")
        .with_style(Style {
            bold: true,
            ..Style::default()
        })
        .with_flex_grow(0.0);

    let map_input = map_msg.clone();
    let input_field = input("query", &model.input, &model.input_style, move |msg| {
        map_input(FuzzyFinderMsg::Input(msg))
    })
    .with_flex_grow(1.0);

    let input_row = row(vec![input_label, input_field]).with_flex_shrink(0.);
    let input_row = block_with_title("", vec![input_row]);

    let matched_count = model.matched_count;
    let items_count = model.injector.injected_items() as usize;
    let footer = dom_text::<Msg>(format!("{matched_count}/{items_count} matches"))
        .with_style(Style {
            fg: Some(Color::BrightBlack),
            dim: true,
            ..Style::default()
        })
        .with_flex_shrink(0.);

    let mut content: Vec<Node<Msg>> = vec![header, input_row];

    if !model.column_config.column_names.is_empty() {
        let col_header = render_column_header(model);
        content.push(col_header);
    }

    let list = if matched_count == 0 {
        dom_text::<Msg>("No matches")
            .with_style(Style {
                fg: Some(Color::BrightBlack),
                dim: true,
                ..Style::default()
            })
            .with_flex_grow(1.0)
            .with_min_height(Dimension::ZERO)
            .with_flex_basis(Dimension::ZERO)
    } else {
        let list_content = render_list_mapped(model, map_msg);
        list_content
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.0)
            .with_flex_basis(Dimension::ZERO)
    };

    content.push(list);
    content.push(footer);

    column(content).with_fill()
}

fn render_column_header<T, Msg>(model: &FuzzyFinder<T>) -> Node<Msg>
where
    T: Clone + Send + Sync + 'static,
    Msg: 'static,
{
    let names = &model.column_config.column_names;
    let num_cols = names.len();
    if num_cols == 0 {
        return dom_text::<Msg>("").with_flex_shrink(0.);
    }

    let width = model.size.width as usize;
    let col_width = width / num_cols;

    let mut header_text = String::new();
    for (i, name) in names.iter().enumerate() {
        let formatted = if i < num_cols - 1 {
            format!("{:width$}", name, width = col_width)
        } else {
            name.clone()
        };
        header_text.push_str(&formatted);
    }

    dom_text::<Msg>(header_text)
        .with_style(Style {
            fg: Some(Color::Cyan),
            bold: true,
            ..Style::default()
        })
        .with_flex_shrink(0.)
}

fn render_list_mapped<T, Msg>(
    model: &FuzzyFinder<T>,
    map_msg: impl Fn(FuzzyFinderMsg) -> Msg + 'static,
) -> Node<Msg>
where
    T: Clone + Send + Sync + 'static,
    Msg: 'static,
{
    let injector = model.injector.clone();
    let matcher_nucleo = Rc::clone(&model.matcher);
    let query_empty = model.last_query.is_empty();
    let column_config = model.column_config.clone();
    let view_width = model.size.width as usize;
    let extractor = model.extractor;

    virtual_list(
        "fuzzy-list",
        &model.list,
        move |action| map_msg(FuzzyFinderMsg::List(action)),
        move |index, selected, ctx| {
            if query_empty {
                if let Some(item) = injector.get(index as u32) {
                    // For unfiltered items, use the extractor to get columns
                    let columns = extractor(item.data);
                    render_item_display(ctx, &columns, selected, &[], &column_config, view_width);
                }
                return;
            }

            let matcher_ref = matcher_nucleo.borrow();
            let snapshot = matcher_ref.snapshot();

            if let Some(matched_item) = snapshot.get_matched_item(index as u32) {
                let mut all_indices: Vec<(usize, Vec<u32>)> = Vec::new();
                let mut matcher = Matcher::default();
                matcher.config = Config::DEFAULT;

                for col_idx in 0..column_config.matcher_column_count() {
                    let mut indices = Vec::new();
                    snapshot.pattern().column_pattern(col_idx).indices(
                        matched_item.matcher_columns[col_idx].slice(..),
                        &mut matcher,
                        &mut indices,
                    );
                    indices.sort_unstable();
                    indices.dedup();
                    if !indices.is_empty() {
                        all_indices.push((col_idx, indices));
                    }
                }

                // For matched items, we also use the extractor
                let columns = extractor(matched_item.data);
                render_item_display(
                    ctx,
                    &columns,
                    selected,
                    &all_indices,
                    &column_config,
                    view_width,
                );
            }
        },
    )
}

fn render_item_display(
    ctx: &mut RenderContext<'_>,
    columns: &[String],
    selected: bool,
    match_indices: &[(usize, Vec<u32>)],
    column_config: &ColumnConfig,
    view_width: usize,
) {
    render_list_item_internal(
        ctx,
        columns,
        selected,
        match_indices,
        column_config,
        view_width,
    );
}

fn render_list_item_internal(
    ctx: &mut RenderContext<'_>,
    columns: &[String],
    selected: bool,
    match_indices: &[(usize, Vec<u32>)],
    column_config: &ColumnConfig,
    view_width: usize,
) {
    let base_style = if selected {
        Style {
            fg: Some(Color::White),
            bg: Some(Color::BrightBlue),
            bold: true,
            ..Style::default()
        }
    } else {
        Style::default()
    };

    let match_style = Style {
        fg: Some(Color::Magenta),
        bold: true,
        ..base_style
    };

    let area = ctx.area();
    if area.width == 0 || area.height == 0 {
        return;
    }

    if selected {
        let attrs = ctx.style_to_attributes(&base_style);
        fill_row(ctx, &attrs);
    }

    let num_cols = column_config.matcher_column_count();
    let has_columns = num_cols > 1 || !column_config.column_names.is_empty();

    if !has_columns {
        let text = columns.first().map(|s| s.as_str()).unwrap_or("");
        let spans = build_spans_with_highlights(text, match_indices, 0, base_style, match_style);
        render_spans(ctx, &spans, area.x, area.y, area.width);
        return;
    }

    let col_width = view_width / num_cols.max(1);
    let mut cursor_x = area.x;

    for col_idx in 0..num_cols {
        let part_idx = if column_config.search_columns.is_empty() {
            col_idx
        } else {
            column_config
                .search_columns
                .get(col_idx)
                .map(|i| i.saturating_sub(1))
                .unwrap_or(col_idx)
        };

        let col_text = columns.get(part_idx).map(|s| s.as_str()).unwrap_or("");
        let col_indices = match_indices
            .iter()
            .find(|(c, _)| *c == col_idx)
            .map(|(_, indices)| indices.as_slice())
            .unwrap_or(&[]);

        let spans = build_spans_with_highlights(
            col_text,
            &[(col_idx, col_indices.to_vec())],
            col_idx,
            base_style,
            match_style,
        );

        let max_col_width = if col_idx < num_cols - 1 {
            col_width
        } else {
            area.x + area.width - cursor_x
        };

        let used = render_spans(ctx, &spans, cursor_x, area.y, max_col_width);

        if col_idx < num_cols - 1 && used < col_width {
            let padding = col_width - used;
            let attrs = ctx.style_to_attributes(&base_style);
            for i in 0..padding {
                ctx.write_char(cursor_x + used + i, area.y, ' ', &attrs);
            }
        }

        cursor_x += col_width;
        if cursor_x >= area.x + area.width {
            break;
        }
    }
}

fn build_spans_with_highlights(
    text: &str,
    match_indices: &[(usize, Vec<u32>)],
    target_col: usize,
    base_style: Style,
    match_style: Style,
) -> Vec<TextSpan> {
    use unicode_segmentation::UnicodeSegmentation;

    let indices: Vec<u32> = match_indices
        .iter()
        .filter(|(c, _)| *c == target_col)
        .flat_map(|(_, idxs)| idxs.iter().copied())
        .collect();

    if indices.is_empty() {
        return vec![TextSpan::new(text, base_style)];
    }

    let mut spans = Vec::new();
    let mut current_text = String::new();
    let mut current_is_match = false;
    let mut indices_iter = indices.iter().peekable();

    for (grapheme_idx, grapheme) in text.graphemes(true).enumerate() {
        let grapheme_idx = grapheme_idx as u32;
        let is_match = indices_iter.peek().is_some_and(|&&idx| idx == grapheme_idx);

        if is_match {
            indices_iter.next();
        }

        if is_match != current_is_match && !current_text.is_empty() {
            let style = if current_is_match {
                match_style
            } else {
                base_style
            };
            spans.push(TextSpan::new(&current_text, style));
            current_text.clear();
        }

        current_text.push_str(grapheme);
        current_is_match = is_match;
    }

    if !current_text.is_empty() {
        let style = if current_is_match {
            match_style
        } else {
            base_style
        };
        spans.push(TextSpan::new(&current_text, style));
    }

    spans
}

fn render_spans(
    ctx: &mut RenderContext<'_>,
    spans: &[TextSpan],
    start_x: usize,
    y: usize,
    max_width: usize,
) -> usize {
    let mut cursor_x = start_x;
    let end_x = start_x + max_width;

    for span in spans {
        if cursor_x >= end_x {
            break;
        }
        let attrs = ctx.style_to_attributes(&span.style);
        let remaining = end_x - cursor_x;
        let used = write_span(ctx, cursor_x, y, &span.content, &attrs, remaining);
        cursor_x += used;
    }

    cursor_x - start_x
}

fn fill_row(ctx: &mut RenderContext<'_>, attrs: &CellAttributes) {
    let area = ctx.area();
    let end_y = area.y.saturating_add(area.height);
    let end_x = area.x.saturating_add(area.width);
    for y in area.y..end_y {
        for x in area.x..end_x {
            ctx.write_char(x, y, ' ', attrs);
        }
    }
}

fn write_span(
    ctx: &mut RenderContext<'_>,
    x: usize,
    y: usize,
    text: &str,
    attrs: &CellAttributes,
    max_cells: usize,
) -> usize {
    if max_cells == 0 {
        return 0;
    }

    let mut used = 0;
    let mut buffer = String::new();
    for ch in text.chars() {
        let width = UnicodeWidthChar::width(ch).unwrap_or(0).max(1);
        if used + width > max_cells {
            break;
        }
        buffer.push(ch);
        used += width;
    }

    ctx.write_text_length(x, y, &buffer, attrs, max_cells);
    used
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::test_utils::render_node_to_string;

    fn make_finder_with_items(
        items: Vec<String>,
    ) -> (FuzzyFinder<String>, FuzzyFinderHandle<String>) {
        FuzzyFinder::<String>::new_with_items(items)
    }

    #[test]
    fn view_renders_for_empty_and_filtered_queries() {
        let items = vec!["alpha".to_string(), "beta".to_string()];
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;
        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 8,
            }),
            map_msg,
        );

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render default view");
        assert!(rendered.contains("alpha"), "rendered output:\n{rendered}");
        assert!(rendered.contains("beta"), "rendered output:\n{rendered}");

        model.set_query("be");
        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render filtered view");
        assert!(!rendered.contains("alpha"), "rendered output:\n{rendered}");
        assert!(rendered.contains("beta"), "rendered output:\n{rendered}");

        model.set_query("zzz");
        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render empty view");
        assert!(rendered.contains("No matches"));
    }

    #[test]
    fn typing_filters_emits_selection_change_event() {
        let items = vec!["alpha".to_string(), "beta".to_string()];
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 8,
            }),
            map_msg,
        );

        let event = update(
            &mut model,
            FuzzyFinderMsg::Input(InputMsg::InsertChar('b')),
            map_msg,
        );

        assert!(matches!(event, FuzzyFinderEvent::Select(_)));
        assert_eq!(model.current_selection().as_deref(), Some("beta"));
    }

    #[test]
    fn scrolling_shows_correct_items_after_selection_moves() {
        let items: Vec<String> = (0..20).map(|i| format!("item-{i:02}")).collect();
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 8,
            }),
            map_msg,
        );

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render");
        assert!(
            rendered.contains("item-00"),
            "should show item-00:\n{rendered}"
        );
        assert!(
            rendered.contains("item-02"),
            "should show item-02:\n{rendered}"
        );
        assert!(
            !rendered.contains("item-10"),
            "should NOT show item-10:\n{rendered}"
        );

        for _ in 0..10 {
            update(
                &mut model,
                FuzzyFinderMsg::KeyPressed(Key::new(KeyCode::Down)),
                map_msg,
            );
        }

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render after scroll");
        assert!(
            rendered.contains("item-10"),
            "should show item-10 after scrolling:\n{rendered}"
        );
        assert!(
            !rendered.contains("item-00"),
            "should NOT show item-00 after scrolling:\n{rendered}"
        );
    }

    #[test]
    fn scrolling_up_shows_earlier_items() {
        let items: Vec<String> = (0..20).map(|i| format!("item-{i:02}")).collect();
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 8,
            }),
            map_msg,
        );

        for _ in 0..15 {
            update(
                &mut model,
                FuzzyFinderMsg::KeyPressed(Key::new(KeyCode::Down)),
                map_msg,
            );
        }

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render");
        assert!(
            rendered.contains("item-15"),
            "should show item-15:\n{rendered}"
        );

        for _ in 0..10 {
            update(
                &mut model,
                FuzzyFinderMsg::KeyPressed(Key::new(KeyCode::Up)),
                map_msg,
            );
        }

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render after scroll up");
        assert!(
            rendered.contains("item-05"),
            "should show item-05 after scrolling up:\n{rendered}"
        );
    }

    #[test]
    fn page_down_scrolls_half_page() {
        let items: Vec<String> = (0..30).map(|i| format!("item-{i:02}")).collect();
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        let ctrl_d = Key::with_modifiers(KeyCode::Char('d'), true, false, false, false);
        update(&mut model, FuzzyFinderMsg::KeyPressed(ctrl_d), map_msg);

        let selected = model.list.selection();
        assert!(selected >= 2, "selection should jump: {}", selected);

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 10).expect("render");
        let selected_item = format!("item-{:02}", selected);
        assert!(
            rendered.contains(&selected_item),
            "should show selected item {}:\n{}",
            selected_item,
            rendered
        );
    }

    #[test]
    fn scroll_state_updates_on_resize() {
        let items: Vec<String> = (0..50).map(|i| format!("line-{i:02}")).collect();
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        for _ in 0..30 {
            update(
                &mut model,
                FuzzyFinderMsg::KeyPressed(Key::new(KeyCode::Down)),
                map_msg,
            );
        }

        assert_eq!(model.list.selection(), 30);

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 20,
            }),
            map_msg,
        );

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 20).expect("render");
        assert!(
            rendered.contains("line-30"),
            "selected item should still be visible:\n{rendered}"
        );
    }

    #[test]
    fn selection_clamps_when_filtering_reduces_matches() {
        let items: Vec<String> = (0..20).map(|i| format!("item-{i:02}")).collect();
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        for _ in 0..15 {
            update(
                &mut model,
                FuzzyFinderMsg::KeyPressed(Key::new(KeyCode::Down)),
                map_msg,
            );
        }
        assert_eq!(model.list.selection(), 15);

        model.set_query("item-0");
        assert!(
            model.list.selection() < model.matched_count,
            "selection {} should be less than matched_count {}",
            model.list.selection(),
            model.matched_count
        );
    }

    #[test]
    fn scrollable_content_clips_to_viewport_height() {
        let items: Vec<String> = (0..100).map(|i| format!("item-{i:03}")).collect();
        let (mut model, _handle) = make_finder_with_items(items);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 10).expect("render");

        let lines: Vec<&str> = rendered.lines().collect();
        assert_eq!(
            lines.len(),
            10,
            "output should have exactly 10 lines:\n{rendered}"
        );

        assert!(
            rendered.contains("item-000"),
            "should show first item:\n{rendered}"
        );
        assert!(
            !rendered.contains("item-050"),
            "should NOT show middle item:\n{rendered}"
        );
    }

    // Multi-column tests

    #[test]
    fn parsed_query_single_column() {
        let config = ColumnConfig::default();
        let parsed = ParsedQuery::parse("hello world", &config);
        assert_eq!(parsed.column_queries.len(), 1);
        assert_eq!(parsed.column_queries[0], (0, "hello world".to_string()));
    }

    #[test]
    fn parsed_query_with_column_names() {
        let config =
            ColumnConfig::default().with_column_names(vec!["name".to_string(), "path".to_string()]);

        let parsed = ParsedQuery::parse("foo %path bar", &config);
        assert_eq!(parsed.column_queries.len(), 2);
        assert_eq!(parsed.column_queries[0], (0, "foo".to_string()));
        assert_eq!(parsed.column_queries[1], (1, "bar".to_string()));
    }

    #[test]
    fn parsed_query_multiple_column_switches() {
        let config = ColumnConfig::default().with_column_names(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]);

        let parsed = ParsedQuery::parse("first %b second %c third %a fourth", &config);
        assert_eq!(parsed.column_queries.len(), 4);
        assert_eq!(parsed.column_queries[0], (0, "first".to_string()));
        assert_eq!(parsed.column_queries[1], (1, "second".to_string()));
        assert_eq!(parsed.column_queries[2], (2, "third".to_string()));
        assert_eq!(parsed.column_queries[3], (0, "fourth".to_string()));
    }

    #[test]
    fn parsed_query_unknown_column_ignored() {
        let config =
            ColumnConfig::default().with_column_names(vec!["name".to_string(), "path".to_string()]);

        let parsed = ParsedQuery::parse("foo %unknown bar", &config);
        assert_eq!(parsed.column_queries.len(), 2);
        assert_eq!(parsed.column_queries[0], (0, "foo".to_string()));
        assert_eq!(parsed.column_queries[1], (0, "bar".to_string()));
    }

    #[test]
    fn multi_column_rendering_shows_columns() {
        let config = ColumnConfig::default()
            .with_separator("\t".to_string())
            .with_column_names(vec!["Name".to_string(), "Type".to_string()]);

        let items = vec![
            "Alice\tPerson".to_string(),
            "Bob\tPerson".to_string(),
            "Cat\tAnimal".to_string(),
        ];

        let (mut model, _handle) = FuzzyFinder::<String>::new_with_items_and_config(items, config);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 10).expect("render");

        assert!(
            rendered.contains("Name"),
            "should show column header 'Name':\n{rendered}"
        );
        assert!(
            rendered.contains("Type"),
            "should show column header 'Type':\n{rendered}"
        );
        assert!(rendered.contains("Alice"), "should show Alice:\n{rendered}");
        assert!(
            rendered.contains("Person"),
            "should show Person:\n{rendered}"
        );
    }

    #[test]
    fn multi_column_filtering_by_second_column() {
        let config = ColumnConfig::default()
            .with_separator("\t".to_string())
            .with_column_names(vec!["Name".to_string(), "Type".to_string()]);

        let items = vec![
            "Alice\tPerson".to_string(),
            "Bob\tPerson".to_string(),
            "Cat\tAnimal".to_string(),
        ];

        let (mut model, _handle) = FuzzyFinder::<String>::new_with_items_and_config(items, config);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        model.set_query("%Type Animal");

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 10).expect("render");

        assert!(
            rendered.contains("Cat"),
            "should show Cat (Animal type):\n{rendered}"
        );
        assert!(
            !rendered.contains("Alice"),
            "should NOT show Alice:\n{rendered}"
        );
        assert!(
            !rendered.contains("Bob"),
            "should NOT show Bob:\n{rendered}"
        );
    }

    #[test]
    fn multi_column_filtering_combined() {
        let config = ColumnConfig::default()
            .with_separator("\t".to_string())
            .with_column_names(vec!["Name".to_string(), "Type".to_string()]);

        let items = vec![
            "Alice\tPerson".to_string(),
            "Alfred\tPerson".to_string(),
            "Cat\tAnimal".to_string(),
        ];

        let (mut model, _handle) = FuzzyFinder::<String>::new_with_items_and_config(items, config);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        model.set_query("Al %Type Person");

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 40, 10).expect("render");

        assert!(
            rendered.contains("Alice") || rendered.contains("Alfred"),
            "should show Alice or Alfred:\n{rendered}"
        );
        assert!(
            !rendered.contains("Cat"),
            "should NOT show Cat:\n{rendered}"
        );
    }

    #[test]
    fn column_header_is_displayed_with_even_spacing() {
        let config = ColumnConfig::default()
            .with_separator("\t".to_string())
            .with_column_names(vec![
                "Col1".to_string(),
                "Col2".to_string(),
                "Col3".to_string(),
            ]);

        let items = vec!["a\tb\tc".to_string()];
        let (mut model, _handle) = FuzzyFinder::<String>::new_with_items_and_config(items, config);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 30,
                height: 10,
            }),
            map_msg,
        );

        let mut node = view(&model, |msg| msg);
        let rendered = render_node_to_string(&mut node, 30, 10).expect("render");

        let lines: Vec<&str> = rendered.lines().collect();
        let header_line = lines
            .iter()
            .find(|l| l.contains("Col1"))
            .expect("header line");
        assert!(
            header_line.contains("Col2"),
            "header should contain Col2:\n{rendered}"
        );
        assert!(
            header_line.contains("Col3"),
            "header should contain Col3:\n{rendered}"
        );
    }

    #[test]
    fn nth_selects_specific_columns_for_matching() {
        let config = ColumnConfig::default()
            .with_separator("\t".to_string())
            .with_search_columns(vec![2]);

        let items = vec!["findme\tfoo".to_string(), "other\tbar".to_string()];

        let (mut model, _handle) = FuzzyFinder::<String>::new_with_items_and_config(items, config);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        // Search for "foo" which only appears in column 2 of first item
        model.set_query("foo");

        assert_eq!(
            model.matched_count, 1,
            "should match only item with 'foo' in column 2"
        );

        // Search for "findme" which appears in column 1 (not searched)
        model.set_query("findme");
        assert_eq!(
            model.matched_count, 0,
            "should not match 'findme' because column 1 is not searched"
        );
    }

    #[test]
    fn column_config_case_insensitive_lookup() {
        let config =
            ColumnConfig::default().with_column_names(vec!["Name".to_string(), "PATH".to_string()]);

        assert_eq!(config.column_name_to_index("name"), Some(0));
        assert_eq!(config.column_name_to_index("NAME"), Some(0));
        assert_eq!(config.column_name_to_index("path"), Some(1));
        assert_eq!(config.column_name_to_index("Path"), Some(1));
        assert_eq!(config.column_name_to_index("unknown"), None);
    }

    #[test]
    fn empty_query_shows_all_items_multi_column() {
        let config = ColumnConfig::default()
            .with_separator("\t".to_string())
            .with_column_names(vec!["A".to_string(), "B".to_string()]);

        let items = vec!["x\ty".to_string(), "z\tw".to_string()];
        let (mut model, _handle) = FuzzyFinder::<String>::new_with_items_and_config(items, config);
        let map_msg = |msg| msg;

        update(
            &mut model,
            FuzzyFinderMsg::Resize(Size {
                width: 40,
                height: 10,
            }),
            map_msg,
        );

        assert_eq!(model.matched_count, 2);
    }

    #[test]
    fn parsed_query_is_empty_for_empty_string() {
        let config = ColumnConfig::default();
        let parsed = ParsedQuery::parse("", &config);
        assert!(parsed.is_empty());
    }

    #[test]
    fn parsed_query_is_empty_for_whitespace() {
        let config = ColumnConfig::default();
        let parsed = ParsedQuery::parse("   ", &config);
        assert!(parsed.is_empty());
    }
    #[test]
    fn render_list_item_handles_multicolumn_with_missing_columns() {
        let config = ColumnConfig::default()
            .with_separator(",")
            .with_search_columns(vec![2, 3]);

        let mut node = render_list_item_internal_wrapper(&["col1".to_string()], &[], &config, 80);
        let rendered = render_node_to_string(&mut node, 80, 1).expect("render");
        // It should render empty strings for missing columns, not panic or show nothing unexpectedly
        assert_eq!(rendered.trim(), "");
    }

    fn render_list_item_internal_wrapper(
        columns: &[String],
        match_indices: &[(usize, Vec<u32>)],
        column_config: &ColumnConfig,
        view_width: usize,
    ) -> Node<()> {
        use chatui::dom::{Renderable, renderable};
        use chatui::render::RenderContext;
        use std::any::Any;

        #[derive(Debug)]
        struct ItemRenderer {
            columns: Vec<String>,
            match_indices: Vec<(usize, Vec<u32>)>,
            column_config: ColumnConfig,
            view_width: usize,
        }

        impl Renderable for ItemRenderer {
            fn render(&self, ctx: &mut RenderContext<'_>) {
                render_list_item_internal(
                    ctx,
                    &self.columns,
                    false,
                    &self.match_indices,
                    &self.column_config,
                    self.view_width,
                );
            }

            fn measure(
                &self,
                _style: &taffy::Style,
                _known_dimensions: taffy::Size<Option<f32>>,
                _available_space: taffy::Size<taffy::AvailableSpace>,
            ) -> taffy::Size<f32> {
                taffy::Size {
                    width: self.view_width as f32,
                    height: 1.0,
                }
            }

            fn as_any(&self) -> &dyn Any {
                self
            }
        }

        renderable(ItemRenderer {
            columns: columns.to_vec(),
            match_indices: match_indices.to_vec(),
            column_config: column_config.clone(),
            view_width,
        })
    }
}

#[cfg(test)]
fn test_string_extractor(s: &String) -> Vec<String> {
    // Simple extractor for tests: splits by common separators
    // First try tab, then space
    if s.contains('\t') {
        s.split('\t').map(|p| p.to_string()).collect()
    } else if s.contains(',') {
        s.split(',').map(|p| p.to_string()).collect()
    } else if s.contains('|') {
        s.split('|').map(|p| p.to_string()).collect()
    } else {
        // No separator found, return the full string as one column
        vec![s.clone()]
    }
}
