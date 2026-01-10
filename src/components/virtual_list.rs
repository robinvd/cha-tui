use std::rc::Rc;

use taffy::{AvailableSpace, Size as TaffySize, Style as TaffyStyle};

use crate::components::scroll::{ScrollMsg, ScrollState};
use crate::components::virtualized_column::virtualized_column;
use crate::dom::Node;
use crate::event::{Key, KeyCode, LocalMouseEvent, Size};
use crate::render::RenderContext;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VirtualListAction {
    MoveUp,
    MoveDown,
    PageUp,
    PageDown,
    Select(usize),
    Activate,
    ActivateAt(usize),
    Scroll(ScrollMsg),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VirtualListEvent {
    SelectionChanged(usize),
    Activated(usize),
}

#[derive(Clone, Debug)]
pub struct VirtualListState {
    selection: usize,
    item_count: usize,
    row_height: u16,
    scroll: ScrollState,
    viewport: Size,
}

impl VirtualListState {
    pub fn new(row_height: u16) -> Self {
        Self {
            selection: 0,
            item_count: 0,
            row_height: row_height.max(1),
            scroll: ScrollState::vertical(),
            viewport: Size {
                width: 0,
                height: 0,
            },
        }
    }

    pub fn selection(&self) -> usize {
        self.selection
    }

    pub fn item_count(&self) -> usize {
        self.item_count
    }

    pub fn row_height(&self) -> u16 {
        self.row_height
    }

    pub fn scroll(&self) -> &ScrollState {
        &self.scroll
    }

    pub fn reset(&mut self) {
        self.selection = 0;
        self.scroll.reset();
    }

    pub fn set_item_count(&mut self, item_count: usize) -> Option<VirtualListEvent> {
        let previous_selection = self.selection;
        self.item_count = item_count;
        if item_count == 0 {
            self.selection = 0;
            self.scroll.reset();
            self.sync_scroll();
            return None;
        }

        if self.selection >= item_count {
            self.selection = item_count.saturating_sub(1);
        }

        self.sync_scroll();
        self.ensure_selection_visible();

        if self.selection != previous_selection {
            Some(VirtualListEvent::SelectionChanged(self.selection))
        } else {
            None
        }
    }

    pub fn set_selection(&mut self, selection: usize) -> Option<VirtualListEvent> {
        self.set_selection_internal(selection)
    }

    pub fn update(&mut self, action: VirtualListAction) -> Option<VirtualListEvent> {
        match action {
            VirtualListAction::MoveUp => self.move_selection(-1),
            VirtualListAction::MoveDown => self.move_selection(1),
            VirtualListAction::PageUp => {
                let jump = self.page_jump() as isize;
                self.move_selection(-jump)
            }
            VirtualListAction::PageDown => {
                let jump = self.page_jump() as isize;
                self.move_selection(jump)
            }
            VirtualListAction::Select(index) => self.set_selection_internal(index),
            VirtualListAction::Activate => self.activate_selection(),
            VirtualListAction::ActivateAt(index) => {
                self.set_selection_internal(index);
                self.activate_selection()
            }
            VirtualListAction::Scroll(msg) => {
                if let ScrollMsg::Resize {
                    viewport,
                    content: _,
                } = msg
                {
                    self.viewport = viewport;
                    self.sync_scroll();
                    self.ensure_selection_visible();
                }
                self.scroll.update(msg);
                None
            }
        }
    }

    fn set_selection_internal(&mut self, selection: usize) -> Option<VirtualListEvent> {
        if self.item_count == 0 {
            self.selection = 0;
            self.scroll.reset();
            return None;
        }

        let next = selection.min(self.item_count.saturating_sub(1));
        let changed = next != self.selection;
        self.selection = next;
        self.ensure_selection_visible();

        if changed {
            Some(VirtualListEvent::SelectionChanged(self.selection))
        } else {
            None
        }
    }

    fn activate_selection(&self) -> Option<VirtualListEvent> {
        if self.item_count == 0 {
            None
        } else {
            Some(VirtualListEvent::Activated(self.selection))
        }
    }

    fn move_selection(&mut self, delta: isize) -> Option<VirtualListEvent> {
        if self.item_count == 0 {
            return None;
        }

        let max = self.item_count.saturating_sub(1) as isize;
        let next = (self.selection as isize + delta).clamp(0, max) as usize;
        self.set_selection_internal(next)
    }

    fn sync_scroll(&mut self) {
        let content_height = self
            .item_count
            .saturating_mul(self.row_height as usize)
            .min(u16::MAX as usize) as u16;
        self.scroll.update(ScrollMsg::Resize {
            viewport: self.viewport,
            content: Size {
                width: self.viewport.width,
                height: content_height,
            },
        });
    }

    fn ensure_selection_visible(&mut self) {
        if self.item_count == 0 {
            self.scroll.reset();
            return;
        }

        let view_height = self.viewport.height as usize;
        if view_height == 0 {
            return;
        }

        let row_height = self.row_height.max(1) as usize;
        let selection_top = self.selection.saturating_mul(row_height);
        let selection_bottom = selection_top.saturating_add(row_height);

        let current_offset = self.scroll.offset().max(0.0).round() as usize;
        let mut new_offset = current_offset;

        if selection_top < current_offset {
            new_offset = selection_top;
        } else if selection_bottom > current_offset.saturating_add(view_height) {
            new_offset = selection_bottom.saturating_sub(view_height);
        }

        if new_offset != current_offset {
            self.scroll.set_offset(new_offset as f32);
        }
    }

    fn page_jump(&self) -> usize {
        let row_height = self.row_height.max(1) as usize;
        let visible = (self.viewport.height as usize) / row_height;
        visible.max(2) / 2
    }
}

pub fn default_keybindings<UpdateMsg>(
    key: Key,
    map: impl Fn(VirtualListAction) -> UpdateMsg,
) -> Option<UpdateMsg> {
    let action = match key.code {
        KeyCode::Up if !key.ctrl && !key.alt => VirtualListAction::MoveUp,
        KeyCode::Down if !key.ctrl && !key.alt => VirtualListAction::MoveDown,
        KeyCode::Char('p') if key.ctrl => VirtualListAction::MoveUp,
        KeyCode::Char('n') if key.ctrl => VirtualListAction::MoveDown,
        KeyCode::Char('u') if key.ctrl => VirtualListAction::PageUp,
        KeyCode::Char('d') if key.ctrl => VirtualListAction::PageDown,
        KeyCode::PageUp => VirtualListAction::PageUp,
        KeyCode::PageDown => VirtualListAction::PageDown,
        KeyCode::Enter => VirtualListAction::Activate,
        _ => return None,
    };

    Some(map(action))
}

pub fn virtual_list<'a, Msg>(
    id: &'static str,
    state: &VirtualListState,
    map_msg: impl Fn(VirtualListAction) -> Msg + 'static,
    render_item: impl for<'r> Fn(usize, bool, &mut RenderContext<'r>) + 'static,
) -> Node<'a, Msg>
where
    Msg: 'static,
{
    let row_height = state.row_height.max(1);
    let item_count = state.item_count;
    let selected = state.selection;
    let map_msg = Rc::new(map_msg);
    let map_scroll = Rc::clone(&map_msg);
    let map_mouse = Rc::clone(&map_msg);

    let measure_item = move |_index: usize,
                             _style: &TaffyStyle,
                             known: TaffySize<Option<f32>>,
                             available: TaffySize<AvailableSpace>| {
        let width = known.width.or(match available.width {
            AvailableSpace::Definite(width) => Some(width),
            _ => None,
        });

        TaffySize {
            width: width.unwrap_or(0.0),
            height: row_height as f32,
        }
    };

    let render_item = move |index: usize, ctx: &mut RenderContext<'_>| {
        render_item(index, index == selected, ctx);
    };

    let list = virtualized_column(item_count, measure_item, render_item);
    let mut node = crate::scrollable_content(
        id,
        &state.scroll,
        1,
        move |msg| map_scroll(VirtualListAction::Scroll(msg)),
        list,
    );

    let scroll_offset = state.scroll.offset().max(0.0).round() as usize;
    let row_height = row_height as usize;

    node = node.on_mouse(move |event: LocalMouseEvent| {
        if item_count == 0 {
            return None;
        }

        let local_y = event.local_position.y as usize;
        let index = scroll_offset
            .saturating_add(local_y)
            .saturating_div(row_height);
        if index >= item_count {
            return None;
        }

        if event.is_double_click() {
            Some(map_mouse(VirtualListAction::ActivateAt(index)))
        } else if event.is_single_click() {
            Some(map_mouse(VirtualListAction::Select(index)))
        } else {
            None
        }
    });

    node
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dom::RetainedNode;
    use crate::dom::rounding::round_layout;
    use crate::event::{Key, MouseButton, MouseEvent, MouseEventKind};
    use crate::test_utils::render_node_to_string;
    use taffy::{AvailableSpace, compute_root_layout};

    fn render_row(ctx: &mut RenderContext<'_>, label: &str) {
        let area = ctx.area();
        let attrs = ctx.style_to_attributes(&crate::Style::default());
        ctx.write_text_length(area.x, area.y, label, &attrs, area.width);
    }

    #[test]
    fn virtual_list_renders_selected_row() {
        let mut state = VirtualListState::new(1);
        state.set_item_count(3);
        state.set_selection(1);

        let node = virtual_list(
            "list",
            &state,
            |msg| msg,
            |index, selected, ctx| {
                let label = if selected {
                    format!("> row-{index}")
                } else {
                    format!("  row-{index}")
                };
                render_row(ctx, &label);
            },
        );

        let mut node: RetainedNode<VirtualListAction> = node.into();
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            TaffySize {
                width: AvailableSpace::Definite(12.0),
                height: AvailableSpace::Definite(3.0),
            },
        );
        round_layout(&mut node);

        let rendered = render_node_to_string(node, 12, 3).expect("render list");
        assert!(rendered.contains("> row-1"), "rendered:\n{rendered}");
    }

    #[test]
    fn mouse_click_selects_item() {
        let mut state = VirtualListState::new(1);
        state.set_item_count(5);

        let node = virtual_list("list", &state, |msg| msg, |_index, _selected, _ctx| {});
        let mut event = MouseEvent::new(0, 2, MouseEventKind::Down(MouseButton::Left));
        event.click_count = 1;

        let message = node.mouse_message(LocalMouseEvent::new(event, 0, 2));
        assert_eq!(message, Some(vec![VirtualListAction::Select(2)]));
    }

    #[test]
    fn mouse_double_click_activates_item() {
        let mut state = VirtualListState::new(1);
        state.set_item_count(5);

        let node = virtual_list("list", &state, |msg| msg, |_index, _selected, _ctx| {});
        let mut event = MouseEvent::new(0, 3, MouseEventKind::Down(MouseButton::Left));
        event.click_count = 2;

        let message = node.mouse_message(LocalMouseEvent::new(event, 0, 3));
        assert_eq!(message, Some(vec![VirtualListAction::ActivateAt(3)]));
    }

    #[test]
    fn update_emits_selection_and_activation_events() {
        let mut state = VirtualListState::new(1);
        state.set_item_count(3);

        let event = state.update(VirtualListAction::MoveDown);
        assert_eq!(event, Some(VirtualListEvent::SelectionChanged(1)));

        let event = state.update(VirtualListAction::Activate);
        assert_eq!(event, Some(VirtualListEvent::Activated(1)));
    }

    #[test]
    fn default_keybindings_map_list_navigation() {
        let down = Key::new(KeyCode::Down);
        let mapped = default_keybindings(down, |msg| msg);
        assert_eq!(mapped, Some(VirtualListAction::MoveDown));

        let ctrl_u = Key::with_modifiers(KeyCode::Char('u'), true, false, false, false);
        let mapped = default_keybindings(ctrl_u, |msg| msg);
        assert_eq!(mapped, Some(VirtualListAction::PageUp));

        let enter = Key::new(KeyCode::Enter);
        let mapped = default_keybindings(enter, |msg| msg);
        assert_eq!(mapped, Some(VirtualListAction::Activate));
    }
}
