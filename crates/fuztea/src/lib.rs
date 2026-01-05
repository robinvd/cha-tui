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

#[derive(Clone)]
pub struct FuzzyFinderInput {
    injector: Injector<String>,
}

impl FuzzyFinderInput {
    pub fn push_item(&self, line: impl Into<String>) {
        let line = line.into();
        self.injector.push(line, |text, cols| {
            cols[0] = Utf32String::from(text.as_str());
        });
    }
}

#[derive(Clone)]
pub struct FuzzyFinderHandle {
    input: FuzzyFinderInput,
}

impl FuzzyFinderHandle {
    pub fn push_item(&self, line: impl Into<String>) {
        self.input.push_item(line);
    }
}

pub struct FuzzyFinder {
    matcher: Rc<RefCell<Nucleo<String>>>,
    injector: Injector<String>,
    config: Config,
    input: InputState,
    input_style: InputStyle,
    matched_count: usize,
    notify_rx: channel::Receiver<()>,
    list: VirtualListState,
    last_query: String,
    listening_matcher: bool,
    size: Size,
    submitted: Option<String>,
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

impl FuzzyFinder {
    pub fn new() -> (Self, FuzzyFinderHandle) {
        let config = Config::DEFAULT;
        let (notify_tx, notify_rx) = channel::bounded(1);
        let notify = Arc::new(move || {
            let _ = notify_tx.try_send(());
        });
        let matcher = Nucleo::new(config.clone(), notify, None, 1);
        let injector = matcher.injector();

        let input = FuzzyFinderInput {
            injector: injector.clone(),
        };
        let finder = Self {
            injector,
            matcher: Rc::new(RefCell::new(matcher)),
            config,
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
    pub fn new_with_items(items: Vec<String>) -> (Self, FuzzyFinderHandle) {
        let (mut finder, handle) = Self::new();
        for item in items {
            handle.push_item(item);
        }
        finder.refresh_matches(true);
        (finder, handle)
    }

    pub fn set_query(&mut self, query: impl Into<String>) {
        self.input.set_value(query.into());
        self.refresh_matches(true);
    }

    pub fn submission(&self) -> Option<&str> {
        self.submitted.as_ref().map(|s| s.as_str())
    }

    fn apply_input(&mut self, msg: InputMsg) {
        if self.input.update(msg) {
            self.refresh_matches(true);
        }
    }

    fn refresh_matches(&mut self, reset_selection: bool) {
        let query = self.input.value();
        let query_changed = query != self.last_query;
        let matched_count = {
            let mut matcher = self.matcher.borrow_mut();
            if query_changed {
                let append = query.starts_with(&self.last_query);
                let (case_matching, normalization) = self.matching_options();
                matcher
                    .pattern
                    .reparse(0, &query, case_matching, normalization, append);
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
        self.list.set_item_count(matched_count);
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
        height.saturating_sub(HEADER_HEIGHT + INPUT_HEIGHT + FOOTER_HEIGHT)
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
}

pub enum FuzzyFinderEvent<Msg> {
    Continue,
    Cancel,
    Select,
    Activate,
    Task(TaskFn<Msg>),
}

pub fn update<Msg>(
    model: &mut FuzzyFinder,
    msg: FuzzyFinderMsg,
    map_msg: impl Fn(FuzzyFinderMsg) -> Msg + 'static,
) -> FuzzyFinderEvent<Msg> {
    match msg {
        FuzzyFinderMsg::KeyPressed(key) => return handle_key(model, key),
        FuzzyFinderMsg::Input(input_msg) => model.apply_input(input_msg),
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

fn listen_for_matcher_updates<Msg>(
    model: &FuzzyFinder,
    map_msg: impl Fn(FuzzyFinderMsg) -> Msg + 'static,
) -> TaskFn<Msg> {
    let receiver = model.notify_rx.clone();
    Box::pin(async move {
        map_msg(match receiver.recv().await {
            Ok(()) => FuzzyFinderMsg::MatcherTick,
            Err(_) => FuzzyFinderMsg::MatcherListenerClosed,
        })
    })
}

fn handle_key<Msg>(model: &mut FuzzyFinder, key: Key) -> FuzzyFinderEvent<Msg> {
    if key.ctrl && matches!(key.code, KeyCode::Char('c') | KeyCode::Char('q')) {
        return FuzzyFinderEvent::Cancel;
    }

    match key.code {
        KeyCode::Esc => FuzzyFinderEvent::Cancel,
        _ => {
            if let Some(action) = default_list_keybindings(key, |msg| msg) {
                return handle_list_action(model, action);
            }
            if let Some(input_msg) = default_input_keybindings(&model.input, key, |msg| msg) {
                model.apply_input(input_msg);
            }
            FuzzyFinderEvent::Continue
        }
    }
}

fn handle_list_action<Msg>(
    model: &mut FuzzyFinder,
    action: VirtualListAction,
) -> FuzzyFinderEvent<Msg> {
    let event = model.list.update(action);
    match event {
        Some(VirtualListEvent::Activated(_)) => {
            if model.submit_selection() {
                FuzzyFinderEvent::Activate
            } else {
                FuzzyFinderEvent::Continue
            }
        }
        Some(VirtualListEvent::SelectionChanged(_)) | None => FuzzyFinderEvent::Continue,
    }
}

pub fn view(model: &FuzzyFinder) -> Node<FuzzyFinderMsg> {
    let header_style = Style {
        fg: Some(Color::BrightBlack),
        ..Style::default()
    };
    let header = dom_text::<FuzzyFinderMsg>(
        "fuztea  Esc quits  Enter selects  Ctrl-n/p move  Ctrl-d/u page",
    )
    .with_style(header_style)
    .with_flex_shrink(0.);

    let input_label = dom_text::<FuzzyFinderMsg>("> ")
        .with_style(Style {
            bold: true,
            ..Style::default()
        })
        .with_flex_grow(0.0);

    let input_field = input(
        "query",
        &model.input,
        &model.input_style,
        FuzzyFinderMsg::Input,
    )
    .with_flex_grow(1.0);

    let input_row = row(vec![input_label, input_field]).with_flex_shrink(0.);
    let input_row = block_with_title("", vec![input_row]);

    let matched_count = model.matched_count;
    let items_count = model.injector.injected_items() as usize;
    let footer = dom_text::<FuzzyFinderMsg>(format!("{matched_count}/{items_count} matches"))
        .with_style(Style {
            fg: Some(Color::BrightBlack),
            dim: true,
            ..Style::default()
        })
        .with_flex_shrink(0.);

    let list = if matched_count == 0 {
        dom_text::<FuzzyFinderMsg>("No matches")
            .with_style(Style {
                fg: Some(Color::BrightBlack),
                dim: true,
                ..Style::default()
            })
            .with_flex_grow(1.0)
            .with_min_height(Dimension::ZERO)
            .with_flex_basis(Dimension::ZERO)
    } else {
        let list_content = render_list(model);
        list_content
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.0)
            .with_flex_basis(Dimension::ZERO)
    };

    column(vec![header, input_row, list, footer]).with_fill()
}

fn render_list(model: &FuzzyFinder) -> Node<FuzzyFinderMsg> {
    let injector = model.injector.clone();
    let matcher_nucleo = Rc::clone(&model.matcher);
    let query_empty = model.last_query.is_empty();

    virtual_list(
        "fuzzy-list",
        &model.list,
        FuzzyFinderMsg::List,
        move |index, selected, ctx| {
            if query_empty {
                if let Some(item) = injector.get(index as u32) {
                    render_list_item(ctx, item.data, selected, None);
                }
                return;
            }

            let matcher_ref = matcher_nucleo.borrow();
            let snapshot = matcher_ref.snapshot();

            if let Some(matched_item) = snapshot.get_matched_item(index as u32) {
                let mut indices = Vec::new();
                let mut matcher = Matcher::default();
                matcher.config = Config::DEFAULT;

                snapshot.pattern().column_pattern(0).indices(
                    matched_item.matcher_columns[0].slice(..),
                    &mut matcher,
                    &mut indices,
                );
                indices.sort_unstable();
                indices.dedup();

                render_list_item(ctx, matched_item.data, selected, Some(indices));
            }
        },
    )
}

fn render_list_item(
    ctx: &mut RenderContext<'_>,
    item_text: &str,
    selected: bool,
    match_indices: Option<Vec<u32>>,
) {
    use unicode_segmentation::UnicodeSegmentation;

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

    let spans = if let Some(indices) = match_indices
        && !indices.is_empty()
    {
        let mut spans = Vec::new();
        let mut current_text = String::new();
        let mut current_is_match = false;
        let mut indices_iter = indices.iter().peekable();

        for (grapheme_idx, grapheme) in item_text.graphemes(true).enumerate() {
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
    } else {
        vec![TextSpan::new(item_text, base_style)]
    };

    let area = ctx.area();
    if area.width == 0 || area.height == 0 {
        return;
    }

    if selected {
        let attrs = ctx.style_to_attributes(&base_style);
        fill_row(ctx, &attrs);
    }

    let mut cursor_x = area.x;
    for span in spans {
        if cursor_x >= area.x + area.width {
            break;
        }
        let attrs = ctx.style_to_attributes(&span.style);
        let max_width = area.x + area.width - cursor_x;
        let used = write_span(ctx, cursor_x, area.y, &span.content, &attrs, max_width);
        cursor_x = cursor_x.saturating_add(used);
    }
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

    fn make_finder_with_items(items: Vec<String>) -> (FuzzyFinder, FuzzyFinderHandle) {
        FuzzyFinder::new_with_items(items)
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

        let mut node = view(&model);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render default view");
        assert!(rendered.contains("alpha"), "rendered output:\n{rendered}");
        assert!(rendered.contains("beta"), "rendered output:\n{rendered}");

        model.set_query("be");
        let mut node = view(&model);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render filtered view");
        assert!(!rendered.contains("alpha"), "rendered output:\n{rendered}");
        assert!(rendered.contains("beta"), "rendered output:\n{rendered}");

        model.set_query("zzz");
        let mut node = view(&model);
        let rendered = render_node_to_string(&mut node, 40, 8).expect("render empty view");
        assert!(rendered.contains("No matches"));
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

        let mut node = view(&model);
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

        let mut node = view(&model);
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

        let mut node = view(&model);
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

        let mut node = view(&model);
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

        let mut node = view(&model);
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

        let mut node = view(&model);
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

        let mut node = view(&model);
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
}
