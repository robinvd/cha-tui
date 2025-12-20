//! Sidebar component with project/session tree.

use chatui::dom::Color;
use chatui::{
    Node, Style, TextSpan, TreeMsg, TreeNode, TreeState, TreeStyle, block_with_title, text,
    tree_view,
};
use taffy::Dimension;

use super::project::{Project, ProjectId};
use super::session::SessionId;

/// Identifier for tree nodes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TreeId {
    Project(ProjectId),
    Session(ProjectId, SessionId),
}

/// Style configuration for the tree.
pub fn tree_style() -> TreeStyle {
    TreeStyle {
        indent: 2,
        ..TreeStyle::default()
    }
}

/// Rebuild the tree state from projects.
pub fn rebuild_tree(
    projects: &[Project],
    tree: &mut TreeState<TreeId>,
    active: &mut Option<(ProjectId, SessionId)>,
) {
    *active = None;
    let prev_selected = tree
        .selected()
        .cloned()
        .or_else(|| active.map(|(pid, sid)| TreeId::Session(pid, sid)));

    let selection_style = Style::bold()
        .with_bg(Color::PaletteFg)
        .with_fg(Color::PaletteBg);

    let mut items = Vec::new();
    for project in projects {
        let children = project
            .sessions
            .iter()
            .map(|session| {
                let row_style = Style::default();

                let mut label_spans = vec![TextSpan::new(session.display_name(), Style::default())];
                if session.has_unread_output && !session.bell {
                    let dot_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
                    label_spans.push(TextSpan::new(" •", dot_style));
                }

                TreeNode::leaf(TreeId::Session(project.id, session.id), label_spans)
                    .with_row_style(row_style)
                    .with_selected_row_style(selection_style)
            })
            .collect::<Vec<_>>();

        let label = vec![TextSpan::new(project.name.clone(), Style::default())];

        items.push(
            TreeNode::branch(TreeId::Project(project.id), label, children)
                .with_selected_row_style(selection_style),
        );
    }

    tree.set_items(items);
    tree.expand_all();

    if let Some(selected) = prev_selected
        && tree.visible().iter().any(|v| v.id == selected)
    {
        tree.select(selected);
    }

    if tree.selected().is_none()
        && let Some(first) = first_session_id(projects)
    {
        tree.select(TreeId::Session(first.0, first.1));
    }

    if let Some(TreeId::Session(pid, sid)) = tree.selected().cloned() {
        *active = Some((pid, sid));
    }
}

/// Get the first session ID from projects.
pub fn first_session_id(projects: &[Project]) -> Option<(ProjectId, SessionId)> {
    projects
        .first()
        .and_then(|p| p.sessions.first().map(|s| (p.id, s.id)))
}

/// Get the next session in the tree order.
pub fn next_session(
    tree: &TreeState<TreeId>,
    active: Option<(ProjectId, SessionId)>,
) -> Option<(ProjectId, SessionId)> {
    let (active_pid, active_sid) = active?;
    let mut seen_active = false;
    let mut first_session = None;

    for node in tree.visible() {
        if let TreeId::Session(pid, sid) = node.id {
            if first_session.is_none() {
                first_session = Some((pid, sid));
            }
            if seen_active {
                return Some((pid, sid));
            }
            if pid == active_pid && sid == active_sid {
                seen_active = true;
            }
        }
    }

    first_session
}

/// Get the previous session in the tree order.
pub fn prev_session(
    tree: &TreeState<TreeId>,
    active: Option<(ProjectId, SessionId)>,
) -> Option<(ProjectId, SessionId)> {
    let (active_pid, active_sid) = active?;
    let mut previous = None;

    for node in tree.visible() {
        if let TreeId::Session(pid, sid) = node.id {
            if pid == active_pid && sid == active_sid && previous.is_none() {
                let mut last_session = None;
                for n in tree.visible() {
                    if let TreeId::Session(p, s) = n.id {
                        last_session = Some((p, s));
                    }
                }
                return last_session;
            }
            if pid == active_pid && sid == active_sid {
                break;
            }
            previous = Some((pid, sid));
        }
    }

    previous
}

/// Move a project up in the list. Returns true if moved.
pub fn move_project_up(projects: &mut [Project], pid: ProjectId) -> bool {
    let Some(idx) = projects.iter().position(|p| p.id == pid) else {
        return false;
    };
    if idx == 0 {
        return false;
    }
    projects.swap(idx, idx - 1);
    true
}

/// Move a project down in the list. Returns true if moved.
pub fn move_project_down(projects: &mut [Project], pid: ProjectId) -> bool {
    let Some(idx) = projects.iter().position(|p| p.id == pid) else {
        return false;
    };
    if idx + 1 >= projects.len() {
        return false;
    }
    projects.swap(idx, idx + 1);
    true
}

/// Render the sidebar.
pub fn sidebar_view<Msg: Clone + 'static>(
    tree: &TreeState<TreeId>,
    auto_hide: bool,
    focused: bool,
    wrap_tree: impl Fn(TreeMsg<TreeId>) -> Msg + 'static,
    on_click: impl Fn() -> Msg + 'static,
) -> Node<Msg> {
    if auto_hide && !focused {
        return chatui::column(vec![]);
    }

    let style = tree_style();
    let tree_node = if tree.visible().is_empty() {
        text::<Msg>("No projects yet. Press p to add one.")
    } else {
        tree_view("session-tree", tree, &style, wrap_tree, focused)
    };

    block_with_title("Sessions", vec![tree_node.with_fill()])
        .with_flex_grow(0.35)
        .with_flex_shrink(0.0)
        .with_flex_basis(Dimension::length(0.))
        .with_max_width(Dimension::length(40.))
        .with_style(section_style(focused))
        .on_click(on_click)
}

/// Style for a section based on focus state.
pub fn section_style(active: bool) -> Style {
    let mut style = Style {
        border: true,
        ..Style::default()
    };
    if active {
        style.fg = Some(Color::Green);
    } else {
        style.fg = Some(Color::rgb(80, 80, 80));
    }
    style
}
