//! Sidebar component with project/session tree.

use chatui::dom::Color;
use chatui::{
    Node, ScrollMsg, ScrollState, Style, TextSpan, TreeMsg, TreeNode, TreeState, TreeStyle,
    block_with_title, scrollable_content, text, tree_view,
};
use taffy::Dimension;
use taffy::prelude::TaffyZero;

use crate::app::Msg as AppMsg;
use crate::filter;

use super::project::{Project, ProjectId, SessionKey, StartupState, WorktreeId};
use super::session::SessionId;

/// Identifier for tree nodes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TreeId {
    Project(ProjectId),
    Worktree(ProjectId, WorktreeId),
    Session(ProjectId, Option<WorktreeId>, SessionId),
}

pub struct Model {
    tree: TreeState<TreeId>,
    tree_scroll: ScrollState,
    auto_hide: bool,
    filter: Option<filter::FilterExpression>,
    filter_text: String,
    active: Option<SessionKey>,
}

impl Model {
    pub fn new() -> Self {
        Self {
            tree: TreeState::new(),
            tree_scroll: ScrollState::vertical(),
            auto_hide: false,
            filter: None,
            filter_text: String::new(),
            active: None,
        }
    }

    pub fn tree(&self) -> &TreeState<TreeId> {
        &self.tree
    }

    pub fn tree_mut(&mut self) -> &mut TreeState<TreeId> {
        &mut self.tree
    }

    pub fn auto_hide(&self) -> bool {
        self.auto_hide
    }

    pub fn toggle_auto_hide(&mut self) {
        self.auto_hide = !self.auto_hide;
    }

    pub fn set_auto_hide(&mut self, auto_hide: bool) {
        self.auto_hide = auto_hide;
    }

    pub fn filter_text(&self) -> &str {
        &self.filter_text
    }

    pub fn set_filter(&mut self, filter: Option<filter::FilterExpression>, text: String) {
        self.filter = filter;
        self.filter_text = text;
    }

    pub fn active(&self) -> Option<SessionKey> {
        self.active
    }

    pub fn select_session(&mut self, key: SessionKey) {
        self.active = Some(key);
        self.tree
            .select(TreeId::Session(key.project, key.worktree, key.session));
    }

    pub fn sync_active_from_tree_selection(&mut self) {
        if let Some(TreeId::Session(pid, worktree, sid)) = self.tree.selected().cloned() {
            self.active = Some(SessionKey {
                project: pid,
                worktree,
                session: sid,
            });
        }
    }

    pub fn rebuild_tree(&mut self, projects: &[Project]) {
        rebuild_tree(
            projects,
            &mut self.tree,
            &mut self.active,
            self.filter.as_ref(),
        );
    }
}

impl Default for Model {
    fn default() -> Self {
        Self::new()
    }
}

pub enum Event {
    ActivateSession(SessionKey),
    ActivateProject(ProjectId),
    ActivateWorktree(ProjectId, WorktreeId),
    FocusSidebar,
}

#[derive(Clone, Debug)]
pub enum Msg {
    Tree(TreeMsg<TreeId>),
    Scroll(ScrollMsg),
}

pub fn update(model: &mut Model, msg: Msg) -> Option<Event> {
    match msg {
        Msg::Tree(tree_msg) => match tree_msg {
            TreeMsg::ToggleExpand(id) => {
                model.tree.toggle_expanded(&id);
                Some(Event::FocusSidebar)
            }
            TreeMsg::Activate(TreeId::Session(pid, worktree, sid))
            | TreeMsg::DoubleClick(TreeId::Session(pid, worktree, sid)) => {
                Some(Event::ActivateSession(SessionKey {
                    project: pid,
                    worktree,
                    session: sid,
                }))
            }
            TreeMsg::Activate(TreeId::Project(pid))
            | TreeMsg::DoubleClick(TreeId::Project(pid)) => {
                model.tree.toggle_expanded(&TreeId::Project(pid));
                Some(Event::ActivateProject(pid))
            }
            TreeMsg::Activate(TreeId::Worktree(pid, wid))
            | TreeMsg::DoubleClick(TreeId::Worktree(pid, wid)) => {
                model.tree.toggle_expanded(&TreeId::Worktree(pid, wid));
                Some(Event::ActivateWorktree(pid, wid))
            }
        },
        Msg::Scroll(scroll_msg) => {
            model.tree_scroll.update(scroll_msg);
            None
        }
    }
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
    filter: Option<&crate::filter::FilterExpression>,
) {
    *active = None;
    let prev_selected = tree
        .selected()
        .cloned()
        .or_else(|| active.map(|key| TreeId::Session(key.project, key.worktree, key.session)));

    let selection_style = Style::bold()
        .with_bg(Color::PaletteFg)
        .with_fg(Color::PaletteBg);

    // Get active session project/worktree for current-project query
    let active_project_worktree = prev_selected.and_then(|sel| match sel {
        TreeId::Session(pid, worktree, _) => Some((pid, worktree)),
        _ => None,
    });

    let mut items = Vec::new();
    for project in projects {
        // Skip inactive projects entirely
        if project.startup_state == StartupState::Inactive {
            continue;
        }
        let mut children = Vec::new();
        let mut project_has_hidden_unread = false;

        for session in &project.sessions {
            let is_active = prev_selected
                .map(|sel| {
                    matches!(sel, TreeId::Session(pid, None, sid) if pid == project.id && sid == session.id)
                })
                .unwrap_or(false);
            let should_show = is_active
                || filter
                    .map(|f| {
                        crate::filter::matches_filter(
                            f,
                            project,
                            None,
                            session,
                            active_project_worktree,
                        )
                    })
                    .unwrap_or(true);

            if should_show {
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
            } else {
                // Session is hidden by filter, bubble up indicators
                if session.has_unread_output && !session.bell {
                    project_has_hidden_unread = true;
                }
            }
        }

        for worktree in &project.worktrees {
            let mut wt_has_hidden_unread = false;

            let wt_children = worktree
                .sessions
                .iter()
                .filter_map(|session| {
                    let is_active = prev_selected
                        .map(|sel| {
                            matches!(sel, TreeId::Session(pid, Some(wid), sid)
                                if pid == project.id && wid == worktree.id && sid == session.id)
                        })
                        .unwrap_or(false);
                    let should_show = is_active
                        || filter
                            .map(|f| {
                                crate::filter::matches_filter(
                                    f,
                                    project,
                                    Some(worktree),
                                    session,
                                    active_project_worktree,
                                )
                            })
                            .unwrap_or(true);

                    if should_show {
                        let row_style = Style::default();
                        let mut label_spans =
                            vec![TextSpan::new(session.display_name(), Style::default())];
                        if session.has_unread_output && !session.bell {
                            let dot_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
                            label_spans.push(TextSpan::new(" •", dot_style));
                        }

                        Some(
                            TreeNode::leaf(
                                TreeId::Session(project.id, Some(worktree.id), session.id),
                                label_spans,
                            )
                            .with_row_style(row_style)
                            .with_selected_row_style(selection_style),
                        )
                    } else {
                        // Session is hidden by filter, bubble up indicators
                        if session.has_unread_output && !session.bell {
                            wt_has_hidden_unread = true;
                        }
                        None
                    }
                })
                .collect::<Vec<_>>();

            let mut label = vec![TextSpan::new(worktree.name.clone(), Style::default())];

            // Add bubbled-up indicators
            if wt_has_hidden_unread {
                let dot_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
                label.push(TextSpan::new(" •", dot_style));
            }

            match worktree.startup_state {
                StartupState::Inactive => {
                    let state_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
                    label.push(TextSpan::new(" inactive", state_style));
                }
                StartupState::Loading => {
                    let state_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
                    label.push(TextSpan::new(" loading", state_style));
                }
                StartupState::Active => {}
            }
            children.push(
                TreeNode::branch(
                    TreeId::Worktree(project.id, worktree.id),
                    label,
                    wt_children,
                )
                .with_selected_row_style(selection_style),
            );

            // Bubble up worktree's hidden indicators to project
            if wt_has_hidden_unread {
                project_has_hidden_unread = true;
            }
        }

        let mut label = vec![TextSpan::new(project.name.clone(), Style::default())];

        // Add bubbled-up indicators
        if project_has_hidden_unread {
            let dot_style = Style::dim().with_fg(Color::rgb(140, 140, 140));
            label.push(TextSpan::new(" •", dot_style));
        }

        if !project.worktrees_loaded {
            let loading_style = Style::dim();
            label.push(TextSpan::new(" ⟳", loading_style));
        }
        if project.startup_state == StartupState::Loading {
            let state_style = Style::dim();
            label.push(TextSpan::new(" loading", state_style));
        }

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

    if tree.selected().is_none() {
        if let Some(first) = first_session_id(projects) {
            tree.select(TreeId::Session(
                first.project,
                first.worktree,
                first.session,
            ));
        } else if let Some(project) = projects.first() {
            tree.select(TreeId::Project(project.id));
        }
    }

    if let Some(TreeId::Session(pid, worktree, sid)) = tree.selected().cloned() {
        *active = Some(SessionKey {
            project: pid,
            worktree,
            session: sid,
        });
    }
}

/// Get the first session ID from projects (only from active projects).
pub fn first_session_id(projects: &[Project]) -> Option<SessionKey> {
    for project in projects {
        // Skip inactive projects
        if project.startup_state == StartupState::Inactive {
            continue;
        }
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

/// Check if there are any active projects.
pub fn has_active_projects(projects: &[Project]) -> bool {
    projects
        .iter()
        .any(|p| p.startup_state != StartupState::Inactive)
}

/// Get the next session in the tree order.
pub fn next_session(tree: &TreeState<TreeId>, active: Option<SessionKey>) -> Option<SessionKey> {
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
pub fn prev_session(tree: &TreeState<TreeId>, active: Option<SessionKey>) -> Option<SessionKey> {
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
pub fn sidebar_view<'a>(
    model: &'a Model,
    focused: bool,
    map_tree_msg: impl Fn(TreeMsg<TreeId>) -> AppMsg + 'static,
    on_click: impl Fn() -> AppMsg + 'static,
) -> Node<'a, AppMsg> {
    if model.auto_hide && !focused {
        return chatui::column(vec![]);
    }

    let style = tree_style();
    let tree_node = if model.tree.visible().is_empty() {
        text::<AppMsg>("No projects yet. Press p to add one.")
    } else {
        tree_view("session-tree", &model.tree, &style, map_tree_msg, focused)
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO)
            .into_node()
    };

    let scroll_node = scrollable_content(
        "tree-scroll",
        &model.tree_scroll,
        3,
        |msg| AppMsg::Sidebar(Msg::Scroll(msg)),
        tree_node,
    );

    let title = if model.filter.is_some() {
        "Sessions (filtered)"
    } else {
        "Sessions"
    };

    block_with_title(title, vec![scroll_node])
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
