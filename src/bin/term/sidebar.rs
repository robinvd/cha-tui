//! Sidebar component with project/session tree.

use chatui::dom::Color;
use chatui::{
    Node, Style, TextSpan, TreeMsg, TreeNode, TreeState, TreeStyle, block_with_title, text,
    tree_view,
};
use taffy::Dimension;

use super::project::{Project, ProjectId, SessionKey, WorktreeId};
use super::session::SessionId;

/// Identifier for tree nodes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TreeId {
    Project(ProjectId),
    Worktree(ProjectId, WorktreeId),
    Session(ProjectId, Option<WorktreeId>, SessionId),
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
    active: &mut Option<SessionKey>,
) {
    *active = None;
    let prev_selected = tree
        .selected()
        .cloned()
        .or_else(|| active.map(|key| TreeId::Session(key.project, key.worktree, key.session)));

    let selection_style = Style::bold()
        .with_bg(Color::PaletteFg)
        .with_fg(Color::PaletteBg);

    let mut items = Vec::new();
    for project in projects {
        let mut children = Vec::new();

        for session in &project.sessions {
            let row_style = Style::default();

            let mut label_spans = vec![TextSpan::new(session.display_name(), Style::default())];
            if session.has_unread_output && !session.bell {
                let dot_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
                label_spans.push(TextSpan::new(" •", dot_style));
            }

            children.push(
                TreeNode::leaf(TreeId::Session(project.id, None, session.id), label_spans)
                    .with_row_style(row_style)
                    .with_selected_row_style(selection_style),
            );
        }

        for worktree in &project.worktrees {
            let wt_children = worktree
                .sessions
                .iter()
                .map(|session| {
                    let row_style = Style::default();
                    let mut label_spans =
                        vec![TextSpan::new(session.display_name(), Style::default())];
                    if session.has_unread_output && !session.bell {
                        let dot_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
                        label_spans.push(TextSpan::new(" •", dot_style));
                    }

                    TreeNode::leaf(
                        TreeId::Session(project.id, Some(worktree.id), session.id),
                        label_spans,
                    )
                    .with_row_style(row_style)
                    .with_selected_row_style(selection_style)
                })
                .collect::<Vec<_>>();

            let label = vec![TextSpan::new(worktree.name.clone(), Style::default())];
            children.push(
                TreeNode::branch(TreeId::Worktree(project.id, worktree.id), label, wt_children)
                    .with_selected_row_style(selection_style),
            );
        }

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
        tree.select(TreeId::Session(first.project, first.worktree, first.session));
    }

    if let Some(TreeId::Session(pid, worktree, sid)) = tree.selected().cloned() {
        *active = Some(SessionKey {
            project: pid,
            worktree,
            session: sid,
        });
    }
}

/// Get the first session ID from projects.
pub fn first_session_id(projects: &[Project]) -> Option<SessionKey> {
    for project in projects {
        if let Some(session) = project.sessions.first() {
            return Some(SessionKey {
                project: project.id,
                worktree: None,
                session: session.id,
            });
        }
        for worktree in &project.worktrees {
            if let Some(session) = worktree.sessions.first() {
                return Some(SessionKey {
                    project: project.id,
                    worktree: Some(worktree.id),
                    session: session.id,
                });
            }
        }
    }
    None
}

/// Get the next session in the tree order.
pub fn next_session(
    tree: &TreeState<TreeId>,
    active: Option<SessionKey>,
) -> Option<SessionKey> {
    let active = active?;
    let mut seen_active = false;
    let mut first_session = None;

    for node in tree.visible() {
        if let TreeId::Session(pid, worktree, sid) = node.id {
            if first_session.is_none() {
                first_session = Some(SessionKey {
                    project: pid,
                    worktree,
                    session: sid,
                });
            }
            if seen_active {
                return Some(SessionKey {
                    project: pid,
                    worktree,
                    session: sid,
                });
            }
            if pid == active.project && sid == active.session && worktree == active.worktree {
                seen_active = true;
            }
        }
    }

    first_session
}

/// Get the previous session in the tree order.
pub fn prev_session(
    tree: &TreeState<TreeId>,
    active: Option<SessionKey>,
) -> Option<SessionKey> {
    let active = active?;
    let mut previous = None;

    for node in tree.visible() {
        if let TreeId::Session(pid, worktree, sid) = node.id {
            if pid == active.project
                && sid == active.session
                && worktree == active.worktree
                && previous.is_none()
            {
                let mut last_session = None;
                for n in tree.visible() {
                    if let TreeId::Session(p, w, s) = n.id {
                        last_session = Some(SessionKey {
                            project: p,
                            worktree: w,
                            session: s,
                        });
                    }
                }
                return last_session;
            }
            if pid == active.project && sid == active.session && worktree == active.worktree {
                break;
            }
            previous = Some(SessionKey {
                project: pid,
                worktree,
                session: sid,
            });
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
