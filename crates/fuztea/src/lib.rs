use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use chatui::components::input::{InputMsg, InputState, InputStyle};
use chatui::components::virtualized_column::virtualized_column;
use chatui::dom::{Color, Node, Style, column, row, text};
use chatui::event::{Event, Key, KeyCode, Size};
use chatui::render::RenderContext;
use chatui::{Transition, default_input_keybindings, input};
use nucleo::pattern::{CaseMatching, Normalization};
use nucleo::{Config, Matcher, Nucleo, Utf32String};
use smol::channel;
use taffy::style::AvailableSpace;
use taffy::{Size as TaffySize, Style as TaffyStyle};
use unicode_segmentation::UnicodeSegmentation;

const HEADER_HEIGHT: usize = 1;
const INPUT_HEIGHT: usize = 1;
const FOOTER_HEIGHT: usize = 1;

#[derive(Clone)]
pub struct FuzzyFinderHandle {
    submitted: Rc<RefCell<Option<String>>>,
}

impl FuzzyFinderHandle {
    pub fn take_submission(&self) -> Option<String> {
        self.submitted.borrow_mut().take()
    }
}

#[derive(Clone, Debug, Default)]
struct SnapshotState {
    matched_count: usize,
}

pub struct FuzzyFinder {
    items: Arc<Vec<String>>,
    matcher: Rc<RefCell<Nucleo<usize>>>,
    config: Config,
    input: InputState,
    input_style: InputStyle,
    snapshot: SnapshotState,
    notify_rx: channel::Receiver<()>,
    selected: usize,
    scroll_offset: f32,
    last_query: String,
    listening: bool,
    size: Size,
    submitted: Rc<RefCell<Option<String>>>,
}

#[derive(Clone, Debug)]
pub enum Msg {
    KeyPressed(Key),
    Input(InputMsg),
    MatcherTick,
    ListenerClosed,
    Resize(Size),
}

pub fn map_event(event: Event) -> Option<Msg> {
    match event {
        Event::Key(key) => Some(Msg::KeyPressed(key)),
        Event::Resize(size) => Some(Msg::Resize(size)),
        _ => None,
    }
}

impl FuzzyFinder {
    pub fn new(items: Vec<String>) -> (Self, FuzzyFinderHandle) {
        let items = Arc::new(items);
        let config = Config::DEFAULT;
        let (notify_tx, notify_rx) = channel::bounded(1);
        let notify = Arc::new(move || {
            let _ = notify_tx.try_send(());
        });
        let matcher = Nucleo::new(config.clone(), notify, None, 1);
        let injector = matcher.injector();

        for (index, item) in items.iter().enumerate() {
            let text = item.as_str();
            injector.push(index, |_, cols| {
                cols[0] = Utf32String::from(text);
            });
        }

        let submitted = Rc::new(RefCell::new(None));
        let mut finder = Self {
            items,
            matcher: Rc::new(RefCell::new(matcher)),
            config,
            input: InputState::new(),
            input_style: InputStyle::default(),
            snapshot: SnapshotState::default(),
            notify_rx,
            selected: 0,
            scroll_offset: 0.0,
            last_query: String::new(),
            listening: false,
            size: Size {
                width: 80,
                height: 24,
            },
            submitted: Rc::clone(&submitted),
        };
        finder.refresh_matches(true);

        let handle = FuzzyFinderHandle { submitted };
        (finder, handle)
    }

    pub fn set_query(&mut self, query: impl Into<String>) {
        self.input.set_value(query.into());
        self.refresh_matches(true);
    }

    fn apply_input(&mut self, msg: InputMsg) -> Transition<Msg> {
        if self.input.update(msg) {
            self.refresh_matches(true);
        }
        Transition::Continue
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
                self.items.len()
            } else {
                matcher.snapshot().matched_item_count() as usize
            }
        };
        self.snapshot.matched_count = matched_count;

        if reset_selection || query_changed {
            self.selected = 0;
        }
        self.clamp_selection();
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

    fn clamp_selection(&mut self) {
        if self.snapshot.matched_count == 0 {
            self.selected = 0;
            self.scroll_offset = 0.0;
            return;
        }

        if self.selected >= self.snapshot.matched_count {
            self.selected = self.snapshot.matched_count.saturating_sub(1);
        }
        self.ensure_selection_visible();
    }

    fn ensure_selection_visible(&mut self) {
        let view_height = self.list_height();
        if view_height == 0 || self.snapshot.matched_count == 0 {
            self.scroll_offset = 0.0;
            return;
        }

        let mut scroll = self.scroll_offset as usize;
        if self.selected < scroll {
            scroll = self.selected;
        } else if self.selected >= scroll + view_height {
            scroll = self.selected.saturating_sub(view_height.saturating_sub(1));
        }
        self.scroll_offset = scroll as f32;
    }

    fn move_selection(&mut self, delta: isize) {
        if self.snapshot.matched_count == 0 {
            return;
        }

        let max = self.snapshot.matched_count as isize - 1;
        let next = (self.selected as isize + delta).clamp(0, max);
        self.selected = next as usize;
        self.ensure_selection_visible();
    }

    fn submit_selection(&mut self) -> bool {
        let index = if self.last_query.is_empty() {
            self.items.get(self.selected)
        } else {
            let matcher = self.matcher.borrow();
            matcher
                .snapshot()
                .get_matched_item(self.selected as u32)
                .and_then(|item| self.items.get(*item.data))
        };

        let Some(item) = index else {
            return false;
        };

        *self.submitted.borrow_mut() = Some(item.clone());
        true
    }
}

pub fn update(model: &mut FuzzyFinder, msg: Msg) -> Transition<Msg> {
    match msg {
        Msg::KeyPressed(key) => handle_key(model, key),
        Msg::Input(input_msg) => model.apply_input(input_msg),
        Msg::MatcherTick => {
            model.refresh_matches(false);
            Transition::Multiple(vec![Transition::Continue, listen_for_updates(model)])
        }
        Msg::ListenerClosed => Transition::Continue,
        Msg::Resize(size) => {
            model.size = size;
            model.ensure_selection_visible();
            if model.listening {
                Transition::Continue
            } else {
                model.listening = true;
                Transition::Multiple(vec![Transition::Continue, listen_for_updates(model)])
            }
        }
    }
}

fn listen_for_updates(model: &FuzzyFinder) -> Transition<Msg> {
    let receiver = model.notify_rx.clone();
    Transition::Task(Box::pin(async move {
        match receiver.recv().await {
            Ok(()) => Msg::MatcherTick,
            Err(_) => Msg::ListenerClosed,
        }
    }))
}

fn handle_key(model: &mut FuzzyFinder, key: Key) -> Transition<Msg> {
    if key.ctrl && matches!(key.code, KeyCode::Char('c') | KeyCode::Char('q')) {
        return Transition::Quit;
    }

    match key.code {
        KeyCode::Esc => Transition::Quit,
        KeyCode::Enter => {
            if model.submit_selection() {
                Transition::Quit
            } else {
                Transition::Continue
            }
        }
        KeyCode::Up => {
            model.move_selection(-1);
            Transition::Continue
        }
        KeyCode::Down => {
            model.move_selection(1);
            Transition::Continue
        }
        KeyCode::Char('n') if key.ctrl => {
            model.move_selection(1);
            Transition::Continue
        }
        KeyCode::Char('p') if key.ctrl => {
            model.move_selection(-1);
            Transition::Continue
        }
        KeyCode::Char('d') if key.ctrl => {
            let jump = (model.list_height().max(2)) / 2;
            model.move_selection(jump as isize);
            Transition::Continue
        }
        KeyCode::Char('u') if key.ctrl => {
            let jump = (model.list_height().max(2)) / 2;
            model.move_selection(-(jump as isize));
            Transition::Continue
        }
        _ => {
            if let Some(input_msg) = default_input_keybindings(&model.input, key, |msg| msg) {
                return model.apply_input(input_msg);
            }
            Transition::Continue
        }
    }
}

pub fn view(model: &FuzzyFinder) -> Node<Msg> {
    let header_style = Style {
        fg: Some(Color::BrightBlack),
        ..Style::default()
    };
    let header = text::<Msg>("fuztea  Esc quits  Enter selects  Ctrl-n/p move  Ctrl-d/u page")
        .with_style(header_style);

    let input_label = text::<Msg>("Query:")
        .with_style(Style {
            bold: true,
            ..Style::default()
        })
        .with_flex_grow(0.0);

    let input_field =
        input("query", &model.input, &model.input_style, Msg::Input).with_flex_grow(1.0);

    let input_row = row(vec![input_label, input_field]);

    let matched_count = model.snapshot.matched_count;
    let items_count = model.items.len();
    let footer = text::<Msg>(format!("{matched_count}/{items_count} matches")).with_style(Style {
        fg: Some(Color::BrightBlack),
        dim: true,
        ..Style::default()
    });

    let list = if matched_count == 0 {
        text::<Msg>("No matches")
            .with_style(Style {
                fg: Some(Color::BrightBlack),
                dim: true,
                ..Style::default()
            })
            .with_flex_grow(1.0)
    } else {
        let items = Arc::clone(&model.items);
        let matcher_nucleo = Rc::clone(&model.matcher);
        let selected = model.selected;
        let query_empty = model.last_query.is_empty();

        let render_item = move |index: usize, ctx: &mut RenderContext<'_>| {
            if query_empty {
                if let Some(text) = items.get(index) {
                    render_list_item(ctx, text, index == selected, None, None);
                }
            } else {
                let matcher_ref = matcher_nucleo.borrow();
                let snapshot = matcher_ref.snapshot();
                if let Some(item) = snapshot.get_matched_item(index as u32)
                    && let Some(text) = items.get(*item.data)
                {
                    render_list_item(ctx, text, index == selected, Some(item), Some(snapshot));
                }
            }
        };

        let measure_item = |_index: usize,
                            _style: &TaffyStyle,
                            known: TaffySize<Option<f32>>,
                            available: TaffySize<AvailableSpace>| {
            let width = known.width.unwrap_or(match available.width {
                AvailableSpace::Definite(value) => value,
                AvailableSpace::MinContent => 1.0,
                AvailableSpace::MaxContent => 1.0,
            });
            TaffySize { width, height: 1.0 }
        };

        virtualized_column(matched_count, measure_item, render_item)
            .with_scroll(model.scroll_offset)
            .with_flex_grow(1.0)
    };

    column(vec![header, input_row, list, footer]).with_fill()
}

fn render_list_item(
    ctx: &mut RenderContext<'_>,
    text: &str,
    selected: bool,
    item: Option<nucleo::Item<'_, usize>>,
    snapshot: Option<&nucleo::Snapshot<usize>>,
) {
    let area = ctx.area();
    if area.width == 0 || area.height == 0 {
        return;
    }

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

    let (origin_x, origin_y) = ctx.origin();

    if let (Some(item), Some(snapshot)) = (item, snapshot) {
        let mut indices = Vec::new();
        let mut matcher = Matcher::default();
        let config = Config::DEFAULT;
        matcher.config = config;

        snapshot.pattern().column_pattern(0).indices(
            item.matcher_columns[0].slice(..),
            &mut matcher,
            &mut indices,
        );

        if !indices.is_empty() {
            indices.sort_unstable();
            indices.dedup();

            let mut x = origin_x;
            let mut indices_iter = indices.iter().peekable();

            for (grapheme_idx, grapheme) in text.graphemes(true).enumerate() {
                let grapheme_idx = grapheme_idx as u32;
                let is_match = indices_iter.peek().is_some_and(|&&idx| idx == grapheme_idx);

                if is_match {
                    indices_iter.next();
                }

                let style = if is_match {
                    Style {
                        fg: Some(Color::Magenta),
                        bold: true,
                        ..base_style
                    }
                } else {
                    base_style
                };

                let attrs = ctx.style_to_attributes(&style);
                let grapheme_width = grapheme.chars().count().min(1);

                if x + grapheme_width > origin_x + area.width {
                    break;
                }

                ctx.write_text_length(x, origin_y, grapheme, &attrs, grapheme_width);
                x += grapheme_width;
            }
            return;
        }
    }

    let attrs = ctx.style_to_attributes(&base_style);
    ctx.write_text_length(origin_x, origin_y, text, &attrs, area.width);
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::test_utils::render_node_to_string;

    #[test]
    fn view_renders_for_empty_and_filtered_queries() {
        let items = vec!["alpha".to_string(), "beta".to_string()];
        let (mut model, _handle) = FuzzyFinder::new(items);
        update(
            &mut model,
            Msg::Resize(Size {
                width: 40,
                height: 8,
            }),
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
}
