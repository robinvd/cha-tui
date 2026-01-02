use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;

use crate::dom::{Node, TextSpan};
use crate::event::LocalMouseEvent;
use crate::{Style, column, rich_text, row};

/// The type of node represented in the tree.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TreeNodeKind {
    Leaf,
    Branch,
}

/// Immutable description of a node in the tree.
#[derive(Clone, Debug)]
pub struct TreeNode<Id> {
    pub id: Id,
    pub label: Vec<TextSpan>,
    pub kind: TreeNodeKind,
    pub children: Vec<TreeNode<Id>>,
    pub row_style: Option<Style>,
    pub selected_row_style: Option<Style>,
}

impl<Id> TreeNode<Id> {
    pub fn leaf(id: Id, label: Vec<TextSpan>) -> Self {
        Self {
            id,
            label,
            kind: TreeNodeKind::Leaf,
            children: Vec::new(),
            row_style: None,
            selected_row_style: None,
        }
    }

    pub fn branch(id: Id, label: Vec<TextSpan>, children: Vec<TreeNode<Id>>) -> Self {
        Self {
            id,
            label,
            kind: TreeNodeKind::Branch,
            children,
            row_style: None,
            selected_row_style: None,
        }
    }

    pub fn with_row_style(mut self, style: Style) -> Self {
        self.row_style = Some(style);
        self
    }

    pub fn with_selected_row_style(mut self, style: Style) -> Self {
        self.selected_row_style = Some(style);
        self
    }
}

#[derive(Clone, Debug)]
pub struct VisibleNode<Id> {
    pub id: Id,
    pub depth: usize,
    pub has_children: bool,
    pub is_expanded: bool,
    pub label: Vec<TextSpan>,
    pub row_style: Option<Style>,
    pub selected_row_style: Option<Style>,
}

pub struct TreeState<Id> {
    roots: Vec<TreeNode<Id>>,
    expanded: HashSet<Id>,
    visible: Vec<VisibleNode<Id>>,
    selected: Option<Id>,
}

impl<Id> TreeState<Id>
where
    Id: Eq + Hash + Clone,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_items(&mut self, roots: Vec<TreeNode<Id>>) {
        let previous_expanded = std::mem::take(&mut self.expanded);
        let previous_selected = self.selected.clone();
        let previous_visible = self.visible.clone();

        self.roots = roots;
        self.expanded = previous_expanded;
        self.selected = previous_selected;

        self.rebuild_visible();
        self.prune_state(&previous_visible);
    }

    pub fn visible(&self) -> &[VisibleNode<Id>] {
        &self.visible
    }

    pub fn select(&mut self, id: Id) {
        if self.visible.iter().any(|node| node.id == id) {
            self.selected = Some(id);
        }
    }

    pub fn selected(&self) -> Option<&Id> {
        self.selected.as_ref()
    }

    pub fn ensure_selected(&mut self) {
        if self.selected.is_none()
            && let Some(first) = self.visible.first()
        {
            self.selected = Some(first.id.clone());
        }
    }

    pub fn select_last(&mut self) {
        if let Some(last) = self.visible.last() {
            self.selected = Some(last.id.clone());
        }
    }

    pub fn select_first(&mut self) {
        if let Some(first) = self.visible.first() {
            self.selected = Some(first.id.clone());
        }
    }

    pub fn select_next(&mut self) {
        if self.visible.is_empty() {
            return;
        }

        let current_index = self
            .selected()
            .and_then(|selected| self.visible.iter().position(|node| &node.id == selected));

        let next_index = match current_index {
            Some(idx) if idx + 1 < self.visible.len() => Some(idx + 1),
            Some(_) => Some(self.visible.len() - 1),
            None => Some(0),
        };

        if let Some(index) = next_index {
            self.selected = Some(self.visible[index].id.clone());
        }
    }

    pub fn select_prev(&mut self) {
        if self.visible.is_empty() {
            return;
        }

        let current_index = self
            .selected()
            .and_then(|selected| self.visible.iter().position(|node| &node.id == selected));

        match current_index {
            Some(0) | None => {
                if let Some(first) = self.visible.first() {
                    self.selected = Some(first.id.clone());
                }
            }
            Some(idx) => {
                self.selected = Some(self.visible[idx - 1].id.clone());
            }
        }
    }

    pub fn expand_all(&mut self) {
        let mut ids = Vec::new();
        for root in &self.roots {
            Self::collect_branch_ids(root, &mut ids);
        }

        let mut changed = false;
        for id in ids {
            if self.expanded.insert(id) {
                changed = true;
            }
        }

        if changed {
            self.rebuild_visible();
        }
    }

    pub fn toggle_expanded(&mut self, id: &Id) {
        if let Some(node) = self.visible.iter().find(|node| &node.id == id)
            && node.has_children
        {
            if self.expanded.contains(id) {
                self.expanded.remove(id);
            } else {
                self.expanded.insert(node.id.clone());
            }
            self.rebuild_visible();
        }
    }

    pub fn is_expanded(&self, id: &Id) -> bool {
        self.expanded.contains(id)
    }

    pub fn set_expanded(&mut self, id: Id, expanded: bool) {
        if expanded {
            self.expanded.insert(id);
        } else {
            self.expanded.remove(&id);
        }
        self.rebuild_visible();
    }

    fn prune_state(&mut self, previous_visible: &[VisibleNode<Id>]) {
        let visible_ids: HashSet<_> = self.visible.iter().map(|node| node.id.clone()).collect();
        self.expanded.retain(|id| visible_ids.contains(id));
        if let Some(selected) = &self.selected
            && !visible_ids.contains(selected)
        {
            if let Some(replacement) =
                Self::replacement_selection(previous_visible, selected, &visible_ids)
            {
                self.selected = Some(replacement);
            } else {
                self.selected = self.visible.first().map(|node| node.id.clone());
            }
        }
    }

    fn replacement_selection(
        previous_visible: &[VisibleNode<Id>],
        previous_selected: &Id,
        current_visible_ids: &HashSet<Id>,
    ) -> Option<Id> {
        let previous_index = previous_visible
            .iter()
            .position(|node| &node.id == previous_selected)?;

        for candidate in previous_visible.iter().skip(previous_index + 1) {
            if current_visible_ids.contains(&candidate.id) {
                return Some(candidate.id.clone());
            }
        }

        for candidate in previous_visible.iter().take(previous_index).rev() {
            if current_visible_ids.contains(&candidate.id) {
                return Some(candidate.id.clone());
            }
        }

        None
    }

    fn collect_branch_ids(node: &TreeNode<Id>, ids: &mut Vec<Id>)
    where
        Id: Clone,
    {
        if matches!(node.kind, TreeNodeKind::Branch) && !node.children.is_empty() {
            ids.push(node.id.clone());
            for child in &node.children {
                Self::collect_branch_ids(child, ids);
            }
        }
    }

    fn rebuild_visible(&mut self) {
        let mut visible = Vec::new();

        for root in &self.roots {
            self.collect_visible(root, 0, &mut visible);
        }

        self.visible = visible;
    }

    fn collect_visible(
        &self,
        node: &TreeNode<Id>,
        depth: usize,
        output: &mut Vec<VisibleNode<Id>>,
    ) {
        let has_children = !node.children.is_empty();
        let is_expanded = has_children && self.expanded.contains(&node.id);

        output.push(VisibleNode {
            id: node.id.clone(),
            depth,
            has_children,
            is_expanded,
            label: node.label.clone(),
            row_style: node.row_style,
            selected_row_style: node.selected_row_style,
        });

        if is_expanded {
            for child in &node.children {
                self.collect_visible(child, depth + 1, output);
            }
        }
    }
}

impl<Id> Default for TreeState<Id>
where
    Id: Eq + Hash + Clone,
{
    fn default() -> Self {
        Self {
            roots: Vec::new(),
            expanded: HashSet::new(),
            visible: Vec::new(),
            selected: None,
        }
    }
}

#[derive(Clone)]
pub struct TreeStyle {
    pub indent: usize,
    pub inactive_row: Style,
    pub active_row: Style,
    pub inactive_selected_row: Style,
    pub active_selected_row: Style,
    pub branch_collapsed: String,
    pub branch_expanded: String,
    pub leaf_bullet: String,
}

impl Default for TreeStyle {
    fn default() -> Self {
        Self {
            indent: 2,
            inactive_row: Style::default(),
            active_row: Style::default(),
            inactive_selected_row: Style::default(),
            active_selected_row: Style::default(),
            branch_collapsed: String::from(">"),
            branch_expanded: String::from("v"),
            leaf_bullet: String::from("-"),
        }
    }
}

/// Messages emitted by tree interactions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TreeMsg<Id> {
    /// Toggle the expanded state of a branch node.
    ToggleExpand(Id),
    /// Activate a node (select, focus, etc.).
    Activate(Id),
    /// Triggered on row double-clicks.
    DoubleClick(Id),
}

pub fn tree_view<Msg, Id>(
    id_prefix: &'static str,
    state: &TreeState<Id>,
    style: &TreeStyle,
    map_msg: impl Fn(TreeMsg<Id>) -> Msg + 'static,
    is_active: bool,
) -> Node<Msg>
where
    Id: Clone + Eq + Hash + 'static,
    Msg: 'static,
{
    let map_msg = Rc::new(map_msg);
    let mut rows = Vec::new();

    for (index, visible) in state.visible().iter().enumerate() {
        let mut spans = Vec::new();
        let indent = " ".repeat(style.indent * visible.depth);
        if !indent.is_empty() {
            spans.push(TextSpan::new(indent, Style::default()));
        }

        let indicator = if visible.has_children {
            if visible.is_expanded {
                style.branch_expanded.clone()
            } else {
                style.branch_collapsed.clone()
            }
        } else {
            style.leaf_bullet.clone()
        };
        spans.push(TextSpan::new(indicator, Style::default()));
        spans.push(TextSpan::new(" ", Style::default()));
        spans.extend_from_slice(&visible.label);

        let selected = state
            .selected()
            .map(|selected| selected == &visible.id)
            .unwrap_or(false);

        let base_style = if selected {
            if is_active {
                style.active_selected_row
            } else {
                style.inactive_selected_row
            }
        } else if is_active {
            style.active_row
        } else {
            style.inactive_row
        };

        let row_style = if selected {
            let mut merged = base_style;
            if let Some(custom) = visible.selected_row_style.as_ref() {
                merged = merged.merged(custom);
            } else if let Some(custom) = visible.row_style.as_ref() {
                merged = merged.merged(custom);
            }
            merged
        } else {
            let mut merged = base_style;
            if let Some(custom) = visible.row_style.as_ref() {
                merged = merged.merged(custom);
            }
            merged
        };

        let row_text = rich_text::<Msg>(spans)
            .with_style(row_style)
            .with_width(taffy::Dimension::percent(1.));

        let mut node = row(vec![row_text])
            .with_id(id_prefix)
            .with_id_mixin(id_prefix, index as u64);

        let is_branch = visible.has_children;
        let mouse_map = Rc::clone(&map_msg);
        let item_id = visible.id.clone();
        node = node.on_mouse(move |event: LocalMouseEvent| {
            if event.is_double_click() {
                Some(mouse_map(TreeMsg::DoubleClick(item_id.clone())))
            } else if event.is_single_click() {
                if is_branch {
                    Some(mouse_map(TreeMsg::ToggleExpand(item_id.clone())))
                } else {
                    Some(mouse_map(TreeMsg::Activate(item_id.clone())))
                }
            } else {
                None
            }
        });

        rows.push(node);
    }

    column(rows)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Style;
    use crate::buffer::DoubleBuffer;
    use crate::dom::rounding::round_layout;
    use crate::dom::{Color, TextSpan};
    use crate::event::{LocalMouseEvent, MouseButton, MouseEvent, MouseEventKind, Size};
    use crate::palette::{Palette, Rgba};
    use crate::render::Renderer;
    use taffy::{self, AvailableSpace, Dimension, compute_root_layout};

    type Id = &'static str;

    fn indicator_style() -> TreeStyle {
        TreeStyle {
            indent: 2,
            active_row: Style::default(),
            inactive_row: Style::default(),
            active_selected_row: Style::fg(Color::Green),
            inactive_selected_row: Style::fg(Color::Yellow),
            branch_collapsed: String::from("[+]"),
            branch_expanded: String::from("[-]"),
            leaf_bullet: String::from("-"),
        }
    }

    fn render_buffer(
        state: &TreeState<Id>,
        style: &TreeStyle,
        is_active: bool,
        width: u16,
        height: u16,
    ) -> DoubleBuffer {
        let mut root = tree_view("tree", state, style, |msg| msg, is_active)
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));

        compute_root_layout(
            &mut root,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(width as f32),
                height: AvailableSpace::Definite(height as f32),
            },
        );
        round_layout(&mut root);

        let mut buffer = DoubleBuffer::new(width as usize, height as usize);
        let palette = Palette::default();
        {
            let mut renderer = Renderer::new(&mut buffer, &palette);
            renderer.render(&root, Size::new(width, height)).unwrap();
        }
        buffer
    }

    fn render_lines(
        state: &TreeState<Id>,
        style: &TreeStyle,
        is_active: bool,
        width: u16,
        height: u16,
    ) -> Vec<String> {
        let buffer = render_buffer(state, style, is_active, width, height);
        buffer
            .to_string()
            .lines()
            .map(|line| line.to_string())
            .collect()
    }

    fn tree() -> TreeState<Id> {
        let mut state = TreeState::new();
        state.set_items(vec![TreeNode::branch(
            "root",
            vec![TextSpan::new("Root", Style::default())],
            vec![
                TreeNode::leaf("file", vec![TextSpan::new("file.rs", Style::default())]),
                TreeNode::branch(
                    "dir",
                    vec![TextSpan::new("dir", Style::default())],
                    vec![TreeNode::leaf(
                        "nested",
                        vec![TextSpan::new("nested.rs", Style::default())],
                    )],
                ),
            ],
        )]);
        state
    }

    fn leading_spaces(value: &str) -> usize {
        value.chars().take_while(|ch| ch.is_whitespace()).count()
    }

    #[test]
    fn expands_and_flattens_visible_nodes() {
        let mut state = tree();
        assert_eq!(state.visible().len(), 1);
        assert!(!state.is_expanded(&"root"));

        state.toggle_expanded(&"root");
        assert_eq!(state.visible().len(), 3);

        let dir = state
            .visible()
            .iter()
            .find(|node| node.id == "dir")
            .unwrap();
        assert_eq!(dir.depth, 1);
        assert!(!dir.is_expanded);
    }

    #[test]
    fn select_navigation_wraps_at_bounds() {
        let mut state = tree();
        state.toggle_expanded(&"root");
        state.select("root");

        state.select_prev();
        assert_eq!(state.selected(), Some(&"root"));

        state.select_next();
        assert_eq!(state.selected(), Some(&"file"));

        state.select_next();
        assert_eq!(state.selected(), Some(&"dir"));

        state.select_prev();
        assert_eq!(state.selected(), Some(&"file"));
    }

    #[test]
    fn collapsed_branch_uses_collapsed_indicator() {
        let state = tree();
        let lines = render_lines(&state, &indicator_style(), true, 40, 5);
        assert!(lines[0].contains("[+] Root"));
    }

    #[test]
    fn expanded_branch_uses_expanded_indicator() {
        let mut state = tree();
        state.toggle_expanded(&"root");
        let lines = render_lines(&state, &indicator_style(), true, 40, 5);
        assert!(lines[0].contains("[-] Root"));
        assert!(lines[1].contains("- file.rs"));
    }

    #[test]
    fn nested_depth_adds_expected_indentation() {
        let mut state = tree();
        state.toggle_expanded(&"root");
        state.toggle_expanded(&"dir");
        let lines = render_lines(&state, &indicator_style(), true, 40, 5);
        assert_eq!(leading_spaces(&lines[3]), indicator_style().indent * 2);
    }

    #[test]
    fn renders_empty_tree_as_blank() {
        let state: TreeState<Id> = TreeState::new();
        let lines = render_lines(&state, &TreeStyle::default(), true, 10, 4);
        assert!(lines.iter().all(|line| line.trim().is_empty()));
    }

    #[test]
    fn expand_all_expands_every_branch() {
        let mut state = tree();
        state.expand_all();
        assert!(state.is_expanded(&"root"));
        assert!(state.visible().iter().any(|node| node.id == "dir"));
        assert!(state.visible().iter().any(|node| node.id == "nested"));
    }

    #[test]
    fn long_labels_are_clipped_to_viewport() {
        let mut state = TreeState::new();
        state.set_items(vec![TreeNode::leaf(
            "long",
            vec![TextSpan::new("very_long_label", Style::default())],
        )]);
        let lines = render_lines(&state, &TreeStyle::default(), true, 8, 1);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].len(), 8);
        assert!(!lines[0].contains("very_long_label"));
    }

    #[test]
    fn selected_row_uses_active_style_when_active() {
        let mut state = tree();
        state.select("root");
        let mut style = TreeStyle::default();
        style.branch_collapsed = String::from("[+]");
        style.branch_expanded = String::from("[-]");
        style.leaf_bullet = String::from("-");
        style.active_selected_row = Style::fg(Color::Green);
        style.inactive_selected_row = Style::fg(Color::Red);

        let buffer = render_buffer(&state, &style, true, 20, 4);
        let first_cell = &buffer.back_buffer()[0][0];
        assert_eq!(first_cell.attrs.foreground(), Some(Rgba::opaque(0, 205, 0)));
    }

    #[test]
    fn selected_row_uses_inactive_style_when_inactive() {
        let mut state = tree();
        state.select("root");
        let mut style = TreeStyle::default();
        style.branch_collapsed = String::from("[+]");
        style.branch_expanded = String::from("[-]");
        style.leaf_bullet = String::from("-");
        style.active_selected_row = Style::fg(Color::Green);
        style.inactive_selected_row = Style::fg(Color::Red);

        let buffer = render_buffer(&state, &style, false, 20, 4);
        let first_cell = &buffer.back_buffer()[0][0];
        assert_eq!(first_cell.attrs.foreground(), Some(Rgba::opaque(205, 0, 0)));
    }

    #[test]
    fn mouse_double_click_on_branch_emits_double_click() {
        let state = tree();
        let style = TreeStyle::default();
        let root_node = tree_view("tree", &state, &style, |msg| msg, true);
        let mut column = root_node.into_element().unwrap();
        let row = column.children.remove(0);

        let mut event = MouseEvent::new(0, 0, MouseEventKind::Down(MouseButton::Left));
        event.click_count = 2;

        let message = row.mouse_message(LocalMouseEvent::new(event, 0, 0));
        assert_eq!(message, Some(vec![TreeMsg::DoubleClick("root")]));
    }

    #[test]
    fn mouse_single_click_on_branch_emits_toggle_expand() {
        let state = tree();
        let style = TreeStyle::default();
        let root_node = tree_view("tree", &state, &style, |msg| msg, true);
        let mut column = root_node.into_element().unwrap();
        let row = column.children.remove(0);

        let mut event = MouseEvent::new(0, 0, MouseEventKind::Down(MouseButton::Left));
        event.click_count = 1;

        let message = row.mouse_message(LocalMouseEvent::new(event, 0, 0));
        assert_eq!(message, Some(vec![TreeMsg::ToggleExpand("root")]));
    }

    #[test]
    fn mouse_single_click_on_leaf_emits_activate() {
        let mut state = tree();
        state.expand_all();
        let style = TreeStyle::default();
        let root_node = tree_view("tree", &state, &style, |msg| msg, true);
        let mut column = root_node.into_element().unwrap();
        // After expand_all, the first child is root, second is "file".
        let leaf_row = column.children.remove(1);

        let mut event = MouseEvent::new(0, 0, MouseEventKind::Down(MouseButton::Left));
        event.click_count = 1;

        let message = leaf_row.mouse_message(LocalMouseEvent::new(event, 0, 0));
        assert_eq!(message, Some(vec![TreeMsg::Activate("file")]));
    }

    #[test]
    fn mouse_double_click_on_leaf_emits_double_click() {
        let mut state = tree();
        state.expand_all();
        let style = TreeStyle::default();
        let root_node = tree_view("tree", &state, &style, |msg| msg, true);
        let mut column = root_node.into_element().unwrap();
        let leaf_row = column.children.remove(1);

        let mut event = MouseEvent::new(0, 0, MouseEventKind::Down(MouseButton::Left));
        event.click_count = 2;

        let message = leaf_row.mouse_message(LocalMouseEvent::new(event, 0, 0));
        assert_eq!(message, Some(vec![TreeMsg::DoubleClick("file")]));
    }

    #[test]
    fn row_style_sets_background() {
        let mut state = TreeState::new();
        let node = TreeNode::leaf("leaf", vec![TextSpan::new("leaf", Style::default())])
            .with_row_style(Style::bg(Color::Green));
        state.set_items(vec![node]);

        let buffer = render_buffer(&state, &TreeStyle::default(), true, 8, 1);
        let first_cell = &buffer.back_buffer()[0][0];
        assert_eq!(first_cell.attrs.background(), Some(Rgba::opaque(0, 205, 0)));
    }

    #[test]
    fn selected_row_style_overrides_row_style_when_present() {
        let mut state = TreeState::new();
        let node = TreeNode::leaf("leaf", vec![TextSpan::new("leaf", Style::default())])
            .with_row_style(Style::bg(Color::Green))
            .with_selected_row_style(Style::bg(Color::Red));
        state.set_items(vec![node]);
        state.select("leaf");

        let buffer = render_buffer(&state, &TreeStyle::default(), true, 8, 1);
        let first_cell = &buffer.back_buffer()[0][0];
        assert_eq!(first_cell.attrs.background(), Some(Rgba::opaque(205, 0, 0)));
    }

    #[test]
    fn selected_row_falls_back_to_row_style_when_not_overridden() {
        let mut state = TreeState::new();
        let node = TreeNode::leaf("leaf", vec![TextSpan::new("leaf", Style::default())])
            .with_row_style(Style::bg(Color::Green));
        state.set_items(vec![node]);
        state.select("leaf");

        let buffer = render_buffer(&state, &TreeStyle::default(), true, 8, 1);
        let first_cell = &buffer.back_buffer()[0][0];
        assert_eq!(first_cell.attrs.background(), Some(Rgba::opaque(0, 205, 0)));
    }
}
