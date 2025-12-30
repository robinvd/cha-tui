//! Session manager demo with a sidebar tree and a single terminal pane.

mod focus;
mod git;
mod jj;
mod keymap;
mod modal;
mod persistence;
mod project;
mod session;
mod sidebar;
mod status;
mod vcs;

use std::io;
use std::path::PathBuf;

#[cfg(test)]
use once_cell::sync::Lazy;
#[cfg(test)]
use std::sync::Mutex;
use taffy::Dimension;

use chatui::event::{Event, Key};
use chatui::{
    InputMsg, InputState, Node, Program, ScrollMsg, ScrollState, TerminalMsg, Transition, TreeMsg,
    TreeState, block_with_title, column, default_terminal_keybindings, row, terminal, text,
};
use smol::channel::Receiver;
use tracing::warn;

use focus::Focus;
use keymap::{Action, Keymap, Scope};
use modal::{ModalMsg, ModalResult, ModalState, modal_handle_key, modal_update, modal_view};
use persistence::{load_projects, save_projects};
use project::{Project, ProjectId, SessionKey, WorktreeId};
use session::{Session, SessionId};
use sidebar::{
    TreeId, move_project_down, move_project_up, next_session, prev_session, rebuild_tree,
    section_style, sidebar_view,
};
use status::{StatusMessage, status_bar_view};
use vcs::VcsKind;

#[cfg(test)]
static TEST_MODEL_GUARD: Lazy<Mutex<()>> = Lazy::new(|| Mutex::new(()));

struct Model {
    projects: Vec<Project>,
    tree: TreeState<TreeId>,
    tree_scroll: ScrollState,
    focus: Focus,
    terminal_locked: bool,
    auto_hide: bool,
    active: Option<SessionKey>,
    modal: Option<ModalState>,
    initial_update: bool,
    next_project_id: u64,
    next_session_id: u64,
    /// When true, skip writing to the config file (used in tests).
    skip_persist: bool,
    /// When true, skip worktree discovery (used in tests).
    skip_worktrees: bool,
    status: Option<StatusMessage>,
    keymap: Keymap,
    #[cfg(test)]
    _test_guard: Option<std::sync::MutexGuard<'static, ()>>,
}

impl Model {
    fn new() -> std::io::Result<Self> {
        Self::new_with_options(false)
    }

    #[cfg(test)]
    fn new_without_persistence() -> std::io::Result<Self> {
        Self::new_with_options(true)
    }

    fn new_with_options(skip_persist: bool) -> std::io::Result<Self> {
        #[cfg(test)]
        let test_guard = if skip_persist {
            Some(TEST_MODEL_GUARD.lock().expect("test model guard poisoned"))
        } else {
            None
        };

        let mut model = Self {
            projects: Vec::new(),
            tree: TreeState::new(),
            tree_scroll: ScrollState::vertical(),
            focus: Focus::Sidebar,
            terminal_locked: false,
            auto_hide: false,
            active: None,
            modal: None,
            initial_update: true,
            next_project_id: 1,
            next_session_id: 1,
            skip_persist,
            skip_worktrees: skip_persist,
            status: None,
            keymap: Keymap::default(),
            #[cfg(test)]
            _test_guard: test_guard,
        };

        let persisted = if skip_persist {
            Vec::new()
        } else {
            match load_projects() {
                Ok(data) => data,
                Err(err) => {
                    warn!(?err, "failed to load saved projects");
                    Vec::new()
                }
            }
        };

        if persisted.is_empty() {
            let cwd = std::env::current_dir()?;
            let name = cwd
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("project")
                .to_string();
            model.add_project(cwd, name)?;
        } else {
            for project in persisted {
                if let Err(err) = model.add_project(project.path, project.name) {
                    warn!(?err, "failed to restore project");
                }
            }
            if model.projects.is_empty() {
                let cwd = std::env::current_dir()?;
                let name = cwd
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or("project")
                    .to_string();
                model.add_project(cwd, name)?;
            }
        }
        Ok(model)
    }

    fn add_project(&mut self, path: PathBuf, name: String) -> std::io::Result<()> {
        let path = match path.canonicalize() {
            Ok(p) => p,
            Err(_) => path,
        };
        if !path.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("{} is not a directory", path.display()),
            ));
        }

        let project_id = ProjectId(self.next_project_id);
        self.next_project_id += 1;

        let mut project = Project::new(project_id, name, path);
        project.worktrees_loaded = self.skip_worktrees;
        let session = self.spawn_session_for(&project, None)?;
        project.add_session(None, session);

        self.projects.push(project);
        rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
        self.focus = Focus::Sidebar;

        if !self.skip_persist
            && let Err(err) = save_projects(&self.projects)
        {
            warn!(?err, "failed to save projects");
        }
        Ok(())
    }

    fn spawn_session(&mut self, path: &std::path::Path, number: usize) -> std::io::Result<Session> {
        let id = SessionId(self.next_session_id);
        self.next_session_id += 1;
        Session::spawn(path, number, id)
    }

    fn spawn_session_for(
        &mut self,
        project: &Project,
        worktree: Option<WorktreeId>,
    ) -> std::io::Result<Session> {
        match worktree {
            None => self.spawn_session(&project.path, project.next_session_number),
            Some(wid) => {
                let Some(worktree) = project.worktree(wid) else {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        "worktree not found",
                    ));
                };
                self.spawn_session(&worktree.path, worktree.next_session_number)
            }
        }
    }

    fn start_worktree_load(&mut self, project_id: ProjectId) -> Option<Transition<Msg>> {
        if self.skip_worktrees {
            return None;
        }
        let project = self.project_mut(project_id)?;
        if project.worktrees_loaded || project.worktrees_loading {
            return None;
        }
        project.worktrees_loading = true;
        let path = project.path.clone();
        Some(Transition::Task(Box::pin(async move {
            let vcs = vcs::detect_async(&path).await;
            let result = vcs::list_worktrees_async(&path, vcs)
                .await
                .map_err(|err| err.to_string());
            Msg::WorktreesLoaded {
                project: project_id,
                result,
            }
        })))
    }

    fn apply_worktrees_loaded(
        &mut self,
        project_id: ProjectId,
        result: Result<Vec<vcs::Worktree>, String>,
        transitions: &mut Vec<Transition<Msg>>,
    ) {
        let Some(project_index) = self.projects.iter().position(|p| p.id == project_id) else {
            return;
        };
        self.projects[project_index].worktrees_loading = false;
        let worktrees = match result {
            Ok(worktrees) => worktrees,
            Err(err) => {
                warn!(?err, "failed to load worktrees");
                return;
            }
        };

        self.projects[project_index].worktrees_loaded = true;
        for worktree in worktrees {
            let path = worktree.path.clone();
            let wid = self.projects[project_index].add_worktree(worktree.name, worktree.path);
            let number = self.projects[project_index]
                .worktree(wid)
                .map(|wt| wt.next_session_number)
                .unwrap_or(1);
            let session = match self.spawn_session(&path, number) {
                Ok(session) => session,
                Err(err) => {
                    warn!(?err, "failed to create worktree session");
                    continue;
                }
            };
            let tree_id = TreeId::Session(project_id, Some(wid), session.id);
            self.projects[project_index].add_session(Some(wid), session);
            if let Some(receiver) = self.wakeup_for(&tree_id) {
                transitions.push(arm_wakeup(tree_id, receiver));
            }
        }

        rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
    }

    fn add_worktree(
        &mut self,
        project_id: ProjectId,
        name: String,
        vcs: VcsKind,
    ) -> Option<TreeId> {
        let project_index = self.projects.iter().position(|p| p.id == project_id)?;
        if self.projects[project_index]
            .worktree_by_name(&name)
            .is_some()
        {
            self.status = Some(StatusMessage::error("Worktree name already exists"));
            return None;
        }

        let repo_path = self.projects[project_index].path.clone();
        let path = match vcs::add_worktree(&repo_path, &name, vcs) {
            Ok(path) => path,
            Err(err) => {
                self.status = Some(StatusMessage::error(format!(
                    "Failed to add worktree: {err}"
                )));
                return None;
            }
        };
        copy_optional_files(&repo_path, &path, &mut self.status);

        let wid = self.projects[project_index].add_worktree(name, path);
        let (path, number) = match self.projects[project_index].worktree(wid) {
            Some(worktree) => (worktree.path.clone(), worktree.next_session_number),
            None => {
                self.status = Some(StatusMessage::error("Worktree no longer exists"));
                return None;
            }
        };
        let session = match self.spawn_session(&path, number) {
            Ok(session) => session,
            Err(err) => {
                self.status = Some(StatusMessage::error(format!(
                    "Failed to create worktree session: {err}"
                )));
                return None;
            }
        };

        let sid = session.id;
        self.projects[project_index].add_session(Some(wid), session);
        rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
        self.select_session(SessionKey {
            project: project_id,
            worktree: Some(wid),
            session: sid,
        });
        self.set_focus(Focus::Sidebar);

        Some(TreeId::Session(project_id, Some(wid), sid))
    }

    fn active_session(&self) -> Option<(&Project, &Session)> {
        let key = self.active?;
        let project = self.projects.iter().find(|p| p.id == key.project)?;
        let session = project.session(key.worktree, key.session)?;
        Some((project, session))
    }

    fn project(&self, pid: ProjectId) -> Option<&Project> {
        self.projects.iter().find(|p| p.id == pid)
    }

    fn project_mut(&mut self, pid: ProjectId) -> Option<&mut Project> {
        self.projects.iter_mut().find(|p| p.id == pid)
    }

    fn select_session(&mut self, key: SessionKey) {
        let mut cleared_bell = false;
        let mut cleared_unread = false;

        if let Some(project) = self.project_mut(key.project)
            && let Some(session) = project.session_mut(key.worktree, key.session)
        {
            cleared_bell = session.clear_bell();
            cleared_unread = session.mark_seen();
        }

        self.active = Some(key);
        self.tree
            .select(TreeId::Session(key.project, key.worktree, key.session));

        if cleared_bell || cleared_unread {
            rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
        }
    }

    fn set_focus(&mut self, focus: Focus) {
        self.focus = focus;
    }

    fn sync_active_from_tree_selection(&mut self) {
        if let Some(TreeId::Session(pid, worktree, sid)) = self.tree.selected().cloned() {
            let mut cleared_bell = false;
            let mut cleared_unread = false;

            if let Some(project) = self.project_mut(pid)
                && let Some(session) = project.session_mut(worktree, sid)
            {
                cleared_bell = session.clear_bell();
                cleared_unread = session.mark_seen();
            }

            self.active = Some(SessionKey {
                project: pid,
                worktree,
                session: sid,
            });

            if cleared_bell || cleared_unread {
                rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
            }
        }
    }

    fn wakeup_for(&self, id: &TreeId) -> Option<Receiver<()>> {
        match id {
            TreeId::Session(pid, worktree, sid) => {
                let project = self.project(*pid)?;
                let session = project.session(*worktree, *sid)?;
                Some(session.wakeup.clone())
            }
            TreeId::Project(_) | TreeId::Worktree(_, _) => None,
        }
    }

    fn sync_session_state(&mut self, id: &TreeId) -> bool {
        let TreeId::Session(pid, worktree, sid) = id else {
            return false;
        };

        let is_active = self
            .active
            .map(|active| {
                active.project == *pid && active.session == *sid && active.worktree == *worktree
            })
            .unwrap_or(false);

        let Some(project) = self.project_mut(*pid) else {
            return false;
        };
        let Some(session) = project.session_mut(*worktree, *sid) else {
            return false;
        };

        let mut changed = false;
        changed |= session.sync_title();
        changed |= session.sync_bell(is_active);
        changed |= session.sync_exited();
        changed |= session.sync_activity(is_active);
        changed
    }

    fn ensure_container_has_session(
        &mut self,
        project_index: usize,
        worktree: Option<WorktreeId>,
    ) -> std::io::Result<Option<TreeId>> {
        let (path, number, is_empty) = {
            let project = &self.projects[project_index];
            match worktree {
                None => (
                    project.path.clone(),
                    project.next_session_number,
                    project.sessions.is_empty(),
                ),
                Some(wid) => {
                    let Some(worktree) = project.worktree(wid) else {
                        return Ok(None);
                    };
                    (
                        worktree.path.clone(),
                        worktree.next_session_number,
                        worktree.sessions.is_empty(),
                    )
                }
            }
        };

        if !is_empty {
            return Ok(None);
        }

        let session = self.spawn_session(&path, number)?;
        let tree_id = TreeId::Session(self.projects[project_index].id, worktree, session.id);

        self.projects[project_index].add_session(worktree, session);
        rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
        Ok(Some(tree_id))
    }

    fn save(&self) {
        if self.skip_persist {
            return;
        }
        if let Err(err) = save_projects(&self.projects) {
            warn!(?err, "failed to save projects");
        }
    }
}

#[derive(Clone, Debug)]
enum Msg {
    FocusSidebar,
    Key(Key),
    Paste(String),
    Tree(TreeMsg<TreeId>),
    TreeScroll(ScrollMsg),
    Terminal {
        project: ProjectId,
        worktree: Option<WorktreeId>,
        session: SessionId,
        msg: TerminalMsg,
    },
    SessionWake(TreeId),
    FocusGained,
    FocusLost,
    WorktreesLoaded {
        project: ProjectId,
        result: Result<Vec<vcs::Worktree>, String>,
    },
    OpenNewProject,
    OpenNewWorktree {
        project: ProjectId,
    },
    Modal(ModalMsg),
    NewSession,
}

fn arm_wakeup(tree_id: TreeId, receiver: Receiver<()>) -> Transition<Msg> {
    Transition::Task(Box::pin(async move {
        let _ = receiver.recv().await;
        Msg::SessionWake(tree_id)
    }))
}

fn clear_status_on_input(model: &mut Model, msg: &Msg) {
    if matches!(
        msg,
        Msg::Key(_)
            | Msg::Paste(_)
            | Msg::Tree(_)
            | Msg::Modal(_)
            | Msg::FocusSidebar
            | Msg::OpenNewProject
            | Msg::OpenNewWorktree { .. }
            | Msg::NewSession
    ) {
        model.status = None;
    }
}

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    let mut transitions = Vec::new();

    clear_status_on_input(model, &msg);

    if model.initial_update {
        model.initial_update = false;
        for project in &model.projects {
            for session in &project.sessions {
                transitions.push(arm_wakeup(
                    TreeId::Session(project.id, None, session.id),
                    session.wakeup.clone(),
                ));
            }
            for worktree in &project.worktrees {
                for session in &worktree.sessions {
                    transitions.push(arm_wakeup(
                        TreeId::Session(project.id, Some(worktree.id), session.id),
                        session.wakeup.clone(),
                    ));
                }
            }
        }
        let project_ids = model
            .projects
            .iter()
            .map(|project| project.id)
            .collect::<Vec<_>>();
        for project_id in project_ids {
            if let Some(transition) = model.start_worktree_load(project_id) {
                transitions.push(transition);
            }
        }
    }

    match msg {
        Msg::FocusSidebar => {
            if model.focus == Focus::Terminal
                && let Some(active) = model.active
            {
                transitions.push(update(
                    model,
                    Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg: TerminalMsg::FocusLost,
                    },
                ));
            }
            model.set_focus(Focus::Sidebar);
        }
        Msg::Paste(text) => {
            if model.modal.is_some() {
                return update(
                    model,
                    Msg::Modal(ModalMsg::Input(InputMsg::InsertText(text))),
                );
            }

            if model.focus == Focus::Terminal
                && let Some(active) = model.active
                && model.active_session().is_some()
            {
                return update(
                    model,
                    Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg: TerminalMsg::Paste(text),
                    },
                );
            }

            transitions.push(Transition::Continue);
            return Transition::Multiple(transitions);
        }
        Msg::Key(key) => {
            // Modal key handling takes precedence
            if model.modal.is_some() {
                if let Some(modal_msg) = modal_handle_key(key) {
                    return update(model, Msg::Modal(modal_msg));
                }
                transitions.push(Transition::Continue);
                return Transition::Multiple(transitions);
            }

            // Determine scopes in priority order
            let scopes = if model.focus == Focus::Terminal && model.terminal_locked {
                vec![Scope::TerminalLocked]
            } else if model.focus == Focus::Terminal {
                vec![Scope::Terminal, Scope::Global]
            } else {
                vec![Scope::Sidebar, Scope::Global]
            };

            if let Some(action) = model.keymap.resolve(&scopes, key) {
                return handle_action(model, action, key, transitions);
            }

            // Fallback: when terminal focused, send to terminal
            if model.focus == Focus::Terminal
                && let Some(active) = model.active
                && let Some((_, _session)) = model.active_session()
                && let Some(msg) =
                    default_terminal_keybindings(key, move |term_msg| Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg: term_msg,
                    })
            {
                return update(model, msg);
            }

            transitions.push(Transition::Continue);
            return Transition::Multiple(transitions);
        }
        Msg::Tree(tree_msg) => match tree_msg {
            TreeMsg::ToggleExpand(id) => {
                model.tree.toggle_expanded(&id);
                model.set_focus(Focus::Sidebar);
            }
            TreeMsg::Activate(TreeId::Session(pid, worktree, sid))
            | TreeMsg::DoubleClick(TreeId::Session(pid, worktree, sid)) => {
                if let Some(active) = model.active {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: active.project,
                            worktree: active.worktree,
                            session: active.session,
                            msg: TerminalMsg::FocusLost,
                        },
                    ));
                }
                model.select_session(SessionKey {
                    project: pid,
                    worktree,
                    session: sid,
                });
                model.set_focus(Focus::Terminal);
                transitions.push(update(
                    model,
                    Msg::Terminal {
                        project: pid,
                        worktree,
                        session: sid,
                        msg: TerminalMsg::FocusGained,
                    },
                ));
                return Transition::Multiple(transitions);
            }
            TreeMsg::Activate(TreeId::Project(pid))
            | TreeMsg::DoubleClick(TreeId::Project(pid)) => {
                model.tree.toggle_expanded(&TreeId::Project(pid));
            }
            TreeMsg::Activate(TreeId::Worktree(pid, wid))
            | TreeMsg::DoubleClick(TreeId::Worktree(pid, wid)) => {
                model.tree.toggle_expanded(&TreeId::Worktree(pid, wid));
            }
        },
        Msg::TreeScroll(scroll_msg) => model.tree_scroll.update(scroll_msg),
        Msg::Terminal {
            project,
            worktree,
            session,
            msg,
        } => {
            if let Some(proj) = model.project_mut(project)
                && let Some(sess) = proj.session_mut(worktree, session)
            {
                sess.update(msg);
                let wakeup = sess.wakeup.clone();
                transitions.push(Transition::Continue);
                transitions.push(arm_wakeup(
                    TreeId::Session(project, worktree, session),
                    wakeup,
                ));
            }
        }
        Msg::SessionWake(id) => {
            if model.sync_session_state(&id) {
                rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
            }
            if let Some(receiver) = model.wakeup_for(&id) {
                transitions.push(arm_wakeup(id, receiver));
            }
        }
        Msg::FocusGained => {
            if model.focus == Focus::Terminal
                && let Some(active) = model.active
            {
                return update(
                    model,
                    Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg: TerminalMsg::FocusGained,
                    },
                );
            }
        }
        Msg::FocusLost => {
            if model.focus == Focus::Terminal
                && let Some(active) = model.active
            {
                return update(
                    model,
                    Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg: TerminalMsg::FocusLost,
                    },
                );
            }
        }
        Msg::WorktreesLoaded { project, result } => {
            model.apply_worktrees_loaded(project, result, &mut transitions);
        }
        Msg::OpenNewProject => {
            model.modal = Some(ModalState::NewProject {
                input: InputState::new(),
                scroll: ScrollState::horizontal(),
            });
            model.set_focus(Focus::Sidebar);
        }
        Msg::OpenNewWorktree { project } => {
            let vcs = model
                .project(project)
                .map(|project| vcs::detect(&project.path))
                .unwrap_or(VcsKind::Git);
            model.modal = Some(ModalState::NewWorktree {
                input: InputState::new(),
                scroll: ScrollState::horizontal(),
                project,
                vcs,
            });
            model.set_focus(Focus::Sidebar);
        }
        Msg::Modal(modal_msg) => {
            if let Some(mut modal_state) = model.modal.take() {
                match modal_update(&mut modal_state, modal_msg) {
                    ModalResult::Continue => {
                        model.modal = Some(modal_state);
                    }
                    ModalResult::Cancelled => {
                        model.modal = None;
                    }
                    ModalResult::ProjectSubmitted(path_str) => {
                        let path = PathBuf::from(&path_str);
                        let name = path
                            .file_name()
                            .and_then(|s| s.to_str())
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| path_str.clone());

                        if let Err(err) = model.add_project(path, name) {
                            warn!(?err, "failed to add project");
                        } else if let Some(project_id) = model.projects.last().map(|p| p.id) {
                            if let Some(transition) = model.start_worktree_load(project_id) {
                                transitions.push(transition);
                            }
                            if let Some(session) = model
                                .project(project_id)
                                .and_then(|project| project.sessions.first())
                            {
                                model.select_session(SessionKey {
                                    project: project_id,
                                    worktree: None,
                                    session: session.id,
                                });
                            }
                        }
                        model.modal = None;
                    }
                    ModalResult::WorktreeSubmitted { project, name, vcs } => {
                        if let Some(tree_id) = model.add_worktree(project, name, vcs)
                            && let Some(receiver) = model.wakeup_for(&tree_id)
                        {
                            transitions.push(arm_wakeup(tree_id, receiver));
                        }
                        model.modal = None;
                    }
                    ModalResult::SessionRenamed {
                        project,
                        worktree,
                        session,
                        name,
                    } => {
                        if let Some(project) = model.project_mut(project)
                            && let Some(session) = project.session_mut(worktree, session)
                        {
                            session.custom_title = Some(name);
                            session.sync_display_name();
                            rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
                        } else {
                            model.status = Some(StatusMessage::error("Session no longer exists"));
                        }
                        model.modal = None;
                    }
                }
            }
        }
        Msg::NewSession => {
            let Some(selected) = model.tree.selected().cloned() else {
                return Transition::Continue;
            };
            let (project_id, worktree, insert_position) = match selected {
                TreeId::Project(pid) => (pid, None, InsertPosition::Top),
                TreeId::Worktree(pid, wid) => (pid, Some(wid), InsertPosition::Top),
                TreeId::Session(pid, worktree, sid) => (pid, worktree, InsertPosition::After(sid)),
            };

            create_session(
                model,
                project_id,
                worktree,
                insert_position,
                &mut transitions,
            );
        }
    }

    if transitions.is_empty() {
        Transition::Continue
    } else if transitions.len() == 1 {
        transitions.pop().unwrap()
    } else {
        Transition::Multiple(transitions)
    }
}

fn handle_action(
    model: &mut Model,
    action: Action,
    _key: Key,
    mut transitions: Vec<Transition<Msg>>,
) -> Transition<Msg> {
    match action {
        Action::ToggleFocus => {
            let old_focus = model.focus;
            let new_focus = model.focus.toggle();
            model.set_focus(new_focus);

            if old_focus != new_focus
                && let Some(active) = model.active
            {
                let msg = match new_focus {
                    Focus::Terminal => TerminalMsg::FocusGained,
                    Focus::Sidebar => TerminalMsg::FocusLost,
                };
                transitions.push(update(
                    model,
                    Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg,
                    },
                ));
            }
            transitions.push(Transition::Continue);
            Transition::Multiple(transitions)
        }
        Action::Quit => Transition::Quit,
        Action::ToggleAutoHide => {
            model.auto_hide = !model.auto_hide;
            Transition::Continue
        }
        Action::FocusTerminal => {
            if model.focus != Focus::Terminal
                && let Some(active) = model.active
            {
                model.set_focus(Focus::Terminal);
                transitions.push(update(
                    model,
                    Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg: TerminalMsg::FocusGained,
                    },
                ));
                Transition::Multiple(transitions)
            } else {
                model.set_focus(Focus::Terminal);
                Transition::Continue
            }
        }
        Action::MoveSelectionUp => {
            model.tree.ensure_selected();
            model.tree.select_prev();
            model.sync_active_from_tree_selection();
            Transition::Continue
        }
        Action::MoveSelectionDown => {
            model.tree.ensure_selected();
            model.tree.select_next();
            model.sync_active_from_tree_selection();
            Transition::Continue
        }
        Action::MoveItemUp => {
            if let Some(selected) = model.tree.selected().cloned() {
                let moved = match selected {
                    TreeId::Project(pid) => move_project_up(&mut model.projects, pid),
                    TreeId::Worktree(_, _) => false,
                    TreeId::Session(pid, worktree, sid) => {
                        if let Some(project) = model.project_mut(pid) {
                            project.move_session_up(worktree, sid)
                        } else {
                            false
                        }
                    }
                };
                if moved {
                    rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
                    model.tree.select(selected);
                }
            }
            Transition::Continue
        }
        Action::MoveItemDown => {
            if let Some(selected) = model.tree.selected().cloned() {
                let moved = match selected {
                    TreeId::Project(pid) => move_project_down(&mut model.projects, pid),
                    TreeId::Worktree(_, _) => false,
                    TreeId::Session(pid, worktree, sid) => {
                        if let Some(project) = model.project_mut(pid) {
                            project.move_session_down(worktree, sid)
                        } else {
                            false
                        }
                    }
                };
                if moved {
                    rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
                    model.tree.select(selected);
                }
            }
            Transition::Continue
        }
        Action::ActivateSelected => {
            if let Some(selected) = model.tree.selected().cloned() {
                match selected {
                    TreeId::Project(pid) => {
                        model.tree.toggle_expanded(&selected);
                        if let Some(project) = model.project(pid)
                            && let Some(session) = project.sessions.first()
                        {
                            model.select_session(SessionKey {
                                project: pid,
                                worktree: None,
                                session: session.id,
                            });
                        } else if let Some(project) = model.project(pid)
                            && let Some(worktree) = project.worktrees.first()
                            && let Some(session) = worktree.sessions.first()
                        {
                            model.select_session(SessionKey {
                                project: pid,
                                worktree: Some(worktree.id),
                                session: session.id,
                            });
                        }
                        Transition::Continue
                    }
                    TreeId::Worktree(pid, wid) => {
                        model.tree.toggle_expanded(&selected);
                        if let Some(project) = model.project(pid)
                            && let Some(worktree) = project.worktree(wid)
                            && let Some(session) = worktree.sessions.first()
                        {
                            model.select_session(SessionKey {
                                project: pid,
                                worktree: Some(wid),
                                session: session.id,
                            });
                        }
                        Transition::Continue
                    }
                    TreeId::Session(pid, worktree, sid) => {
                        if let Some(active) = model.active {
                            transitions.push(update(
                                model,
                                Msg::Terminal {
                                    project: active.project,
                                    worktree: active.worktree,
                                    session: active.session,
                                    msg: TerminalMsg::FocusLost,
                                },
                            ));
                        }
                        model.select_session(SessionKey {
                            project: pid,
                            worktree,
                            session: sid,
                        });
                        model.set_focus(Focus::Terminal);
                        transitions.push(update(
                            model,
                            Msg::Terminal {
                                project: pid,
                                worktree,
                                session: sid,
                                msg: TerminalMsg::FocusGained,
                            },
                        ));
                        Transition::Multiple(transitions)
                    }
                }
            } else {
                Transition::Continue
            }
        }
        Action::NewProject => update(model, Msg::OpenNewProject),
        Action::NewWorktree => {
            let Some(selected) = model.tree.selected().cloned() else {
                model.status = Some(StatusMessage::error("Select a project or session first"));
                return Transition::Continue;
            };
            let project_id = match selected {
                TreeId::Project(pid) => pid,
                TreeId::Worktree(pid, _) => pid,
                TreeId::Session(pid, _, _) => pid,
            };
            update(
                model,
                Msg::OpenNewWorktree {
                    project: project_id,
                },
            )
        }
        Action::NewSession => update(model, Msg::NewSession),
        Action::DeleteSelected => {
            delete_selected(model);
            Transition::Continue
        }
        Action::RenameSession => {
            if let Some(TreeId::Session(pid, worktree, sid)) = model.tree.selected().cloned() {
                let session_name = model.project(pid).and_then(|project| {
                    project.session(worktree, sid).map(|session| {
                        session
                            .custom_title
                            .clone()
                            .or_else(|| session.title.clone())
                            .unwrap_or_else(|| format!("session{}", session.number))
                    })
                });
                if let Some(session_name) = session_name {
                    model.modal = Some(ModalState::RenameSession {
                        input: InputState::with_value(session_name),
                        project: pid,
                        worktree,
                        session: sid,
                        scroll: ScrollState::horizontal(),
                    });
                } else {
                    model.status = Some(StatusMessage::error("Session no longer exists"));
                }
            } else {
                model.status = Some(StatusMessage::error("Select a session to rename"));
            }
            Transition::Continue
        }
        Action::SessionByIndex(target_index) => {
            if let Some(active) = model.active {
                let target_session = model
                    .project(active.project)
                    .and_then(|project| match active.worktree {
                        None => project.sessions.get(target_index - 1),
                        Some(wid) => project
                            .worktree(wid)
                            .and_then(|wt| wt.sessions.get(target_index - 1)),
                    });
                let target_session_id = target_session.map(|session| session.id);

                if let Some(target_session_id) = target_session_id {
                    if let Some(active) = model.active {
                        transitions.push(update(
                            model,
                            Msg::Terminal {
                                project: active.project,
                                worktree: active.worktree,
                                session: active.session,
                                msg: TerminalMsg::FocusLost,
                            },
                        ));
                    }
                    model.select_session(SessionKey {
                        project: active.project,
                        worktree: active.worktree,
                        session: target_session_id,
                    });
                    if model.focus == Focus::Terminal {
                        transitions.push(update(
                            model,
                            Msg::Terminal {
                                project: active.project,
                                worktree: active.worktree,
                                session: target_session_id,
                                msg: TerminalMsg::FocusGained,
                            },
                        ));
                    }
                    Transition::Multiple(transitions)
                } else {
                    Transition::Continue
                }
            } else {
                Transition::Continue
            }
        }
        Action::NextSession | Action::PrevSession => {
            let target = match action {
                Action::NextSession => next_session(&model.tree, model.active),
                Action::PrevSession => prev_session(&model.tree, model.active),
                _ => None,
            };

            if let Some(target) = target {
                if let Some(active) = model.active {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: active.project,
                            worktree: active.worktree,
                            session: active.session,
                            msg: TerminalMsg::FocusLost,
                        },
                    ));
                }
                model.select_session(target);
                if model.focus == Focus::Terminal {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: target.project,
                            worktree: target.worktree,
                            session: target.session,
                            msg: TerminalMsg::FocusGained,
                        },
                    ));
                }
                Transition::Multiple(transitions)
            } else {
                Transition::Continue
            }
        }
        Action::ToggleTerminalLock => {
            model.terminal_locked = !model.terminal_locked;
            Transition::Continue
        }
    }
}

enum InsertPosition {
    Top,
    After(SessionId),
}

fn create_session(
    model: &mut Model,
    project_id: ProjectId,
    worktree: Option<WorktreeId>,
    insert_position: InsertPosition,
    transitions: &mut Vec<Transition<Msg>>,
) {
    let Some(project_index) = model.projects.iter().position(|p| p.id == project_id) else {
        return;
    };

    let (path, number) = {
        let project = &model.projects[project_index];
        match worktree {
            None => (project.path.clone(), project.next_session_number),
            Some(wid) => {
                let Some(worktree) = project.worktree(wid) else {
                    return;
                };
                (worktree.path.clone(), worktree.next_session_number)
            }
        }
    };
    let session = match model.spawn_session(&path, number) {
        Ok(session) => session,
        Err(err) => {
            warn!(?err, "failed to create new session");
            model.status = Some(StatusMessage::error(format!(
                "Failed to create session: {err}"
            )));
            return;
        }
    };

    let sid = session.id;
    match insert_position {
        InsertPosition::Top => {
            model.projects[project_index].add_session_at_start(worktree, session);
        }
        InsertPosition::After(target_sid) => {
            model.projects[project_index].add_session_after(worktree, session, Some(target_sid));
        }
    }

    rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
    model.select_session(SessionKey {
        project: project_id,
        worktree,
        session: sid,
    });
    model.set_focus(Focus::Terminal);

    if let Some(receiver) = model.wakeup_for(&TreeId::Session(project_id, worktree, sid)) {
        transitions.push(arm_wakeup(
            TreeId::Session(project_id, worktree, sid),
            receiver,
        ));
    }
}

fn copy_optional_files(
    from_dir: &std::path::Path,
    to_dir: &std::path::Path,
    status: &mut Option<StatusMessage>,
) {
    for name in [".env", "config.yml"] {
        let source = from_dir.join(name);
        if !source.exists() {
            continue;
        }
        let target = to_dir.join(name);
        if let Err(err) = std::fs::copy(&source, &target) {
            *status = Some(StatusMessage::error(format!(
                "Failed to copy {name}: {err}"
            )));
        }
    }
}

fn delete_selected(model: &mut Model) {
    let Some(selected) = model.tree.selected().cloned() else {
        return;
    };

    match selected {
        TreeId::Project(pid) => {
            if let Some(pos) = model.projects.iter().position(|p| p.id == pid) {
                model.projects.remove(pos);
            }
            rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
            model.set_focus(Focus::Sidebar);
            model.save();
        }
        TreeId::Worktree(pid, wid) => {
            let Some(project_idx) = model.projects.iter().position(|p| p.id == pid) else {
                return;
            };
            let worktree_path = model.projects[project_idx]
                .worktree(wid)
                .map(|wt| wt.path.clone());
            let Some(worktree_path) = worktree_path else {
                model.status = Some(StatusMessage::error("Worktree no longer exists"));
                return;
            };
            let repo_path = model.projects[project_idx].path.clone();
            let vcs = vcs::detect(&repo_path);
            if let Err(err) = vcs::remove_worktree(&repo_path, &worktree_path, vcs) {
                model.status = Some(StatusMessage::error(format!(
                    "Failed to remove worktree: {err}"
                )));
                return;
            }
            model.projects[project_idx].remove_worktree(wid);
            rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
            model.set_focus(Focus::Sidebar);
        }
        TreeId::Session(pid, worktree, sid) => {
            if let Some(project_idx) = model.projects.iter().position(|p| p.id == pid) {
                let became_empty = {
                    let project = &mut model.projects[project_idx];
                    project.remove_session(worktree, sid);
                    match worktree {
                        None => project.sessions.is_empty(),
                        Some(wid) => project
                            .worktree(wid)
                            .map(|wt| wt.sessions.is_empty())
                            .unwrap_or(true),
                    }
                };

                if became_empty {
                    if let Ok(Some(TreeId::Session(new_pid, new_worktree, new_sid))) =
                        model.ensure_container_has_session(project_idx, worktree)
                    {
                        model.select_session(SessionKey {
                            project: new_pid,
                            worktree: new_worktree,
                            session: new_sid,
                        });
                    }
                } else {
                    let next_session = match worktree {
                        None => model.projects[project_idx].sessions.first(),
                        Some(wid) => model.projects[project_idx]
                            .worktree(wid)
                            .and_then(|wt| wt.sessions.first()),
                    };
                    if let Some(next_session) = next_session {
                        model.select_session(SessionKey {
                            project: pid,
                            worktree,
                            session: next_session.id,
                        });
                    }
                }
                rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
                model.set_focus(Focus::Sidebar);
            }
        }
    }
}

fn terminal_pane(model: &Model) -> Node<Msg> {
    if let Some((project, session)) = model.active_session() {
        let worktree = model.active.map(|active| active.worktree).unwrap_or(None);
        let pid = project.id;
        let sid = session.id;
        terminal(
            "main-terminal",
            &session.terminal,
            model.focus == Focus::Terminal,
            move |msg| Msg::Terminal {
                project: pid,
                worktree,
                session: sid,
                msg,
            },
        )
        .with_flex_grow(1.0)
        .with_style(section_style(model.focus == Focus::Terminal))
    } else {
        block_with_title(
            "Terminal",
            vec![text::<Msg>("Select or create a session to start.")],
        )
        .with_flex_grow(1.0)
        .with_style(section_style(model.focus == Focus::Terminal))
    }
}

fn view(model: &Model) -> Node<Msg> {
    let sidebar = sidebar_view(
        &model.tree,
        &model.tree_scroll,
        model.auto_hide,
        model.focus == Focus::Sidebar,
        Msg::Tree,
        || Msg::FocusSidebar,
    );

    let content = row(vec![sidebar, terminal_pane(model)]).with_flex_grow(1.0);

    let status = status_bar_view(
        model.focus,
        model.status.as_ref(),
        model.auto_hide,
        model.active_session(),
        model.terminal_locked,
        &model.keymap,
    );

    let mut nodes = vec![content, status.with_max_width(Dimension::percent(1.))];

    if let Some(modal_state) = &model.modal {
        nodes.push(modal_view(modal_state, Msg::Modal));
    }

    column(nodes).with_fill()
}

fn main() {
    color_eyre::install().expect("failed to install color-eyre");

    // Set up tracing to file instead of stdout (which would mess up the TUI)
    use std::fs::File;
    use tracing_subscriber::fmt;
    use tracing_subscriber::prelude::*;
    let log_file_path =
        std::env::var("TERM_LOG_FILE").unwrap_or_else(|_| "./term_debug.log".to_owned());
    let log_file = File::create(log_file_path).expect("failed to create log file");
    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(log_file).with_ansi(false))
        .init();

    let model = Model::new().expect("failed to create terminals");

    let program = Program::new(model, update, view).map_event(|event| match event {
        Event::Key(key) => Some(Msg::Key(key)),
        Event::Paste(text) => Some(Msg::Paste(text)),
        Event::FocusGained => Some(Msg::FocusGained),
        Event::FocusLost => Some(Msg::FocusLost),
        _ => None,
    });

    if let Err(err) = program.run() {
        eprintln!("Program failed: {:?}", err);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::buffer::DoubleBuffer;
    use chatui::dom::rounding::round_layout;
    use chatui::event::{KeyCode, Size};
    use chatui::palette::Palette;
    use chatui::render::Renderer;
    use taffy::compute_root_layout;

    fn test_model() -> Option<Model> {
        match Model::new_without_persistence() {
            Ok(model) => Some(model),
            Err(err) => {
                eprintln!("Skipping term test: {err}");
                None
            }
        }
    }

    #[test]
    fn deleting_last_session_creates_replacement() {
        let Some(mut model) = test_model() else {
            return;
        };
        let TreeId::Session(pid, worktree, sid) = model.tree.selected().cloned().expect("selected")
        else {
            panic!("expected a session selected");
        };

        delete_selected(&mut model);

        let project = model.projects.first().expect("project present");
        assert_eq!(project.sessions.len(), 1);
        let new_session = &project.sessions[0];
        assert_ne!(new_session.id, sid);
        assert_eq!(new_session.number, 2);
        assert_eq!(
            model.active,
            Some(SessionKey {
                project: pid,
                worktree,
                session: new_session.id
            })
        );
    }

    #[test]
    fn activating_session_switches_focus_and_selection() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = model.projects[0].id;

        // Add another session
        update(&mut model, Msg::NewSession);
        let second_session = model.projects[0].sessions[1].id;

        update(
            &mut model,
            Msg::Tree(TreeMsg::Activate(TreeId::Session(
                project_id,
                None,
                second_session,
            ))),
        );

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(
            model.active,
            Some(SessionKey {
                project: project_id,
                worktree: None,
                session: second_session
            })
        );
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, None, second_session))
        );
    }

    #[test]
    fn ctrl_down_switches_to_next_session_when_terminal_focused() {
        let Some(mut model) = test_model() else {
            return;
        };
        update(&mut model, Msg::NewSession);

        let project_id = model.projects[0].id;
        let first_session = model.projects[0].sessions[0].id;
        let second_session = model.projects[0].sessions[1].id;

        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: first_session,
        });
        model.focus = Focus::Terminal;

        let mut key = Key::new(KeyCode::Down);
        key.ctrl = true;
        update(&mut model, Msg::Key(key));

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(
            model.active,
            Some(SessionKey {
                project: project_id,
                worktree: None,
                session: second_session
            })
        );
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, None, second_session))
        );
    }

    #[test]
    fn ctrl_up_switches_to_previous_session_when_terminal_focused() {
        let Some(mut model) = test_model() else {
            return;
        };
        update(&mut model, Msg::NewSession);

        let project_id = model.projects[0].id;
        let first_session = model.projects[0].sessions[0].id;
        let second_session = model.projects[0].sessions[1].id;

        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: second_session,
        });
        model.focus = Focus::Terminal;

        let mut key = Key::new(KeyCode::Up);
        key.ctrl = true;
        update(&mut model, Msg::Key(key));

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(
            model.active,
            Some(SessionKey {
                project: project_id,
                worktree: None,
                session: first_session
            })
        );
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, None, first_session))
        );
    }

    #[test]
    fn cmd_number_switches_to_session_in_active_project() {
        let Some(mut model) = test_model() else {
            return;
        };
        update(&mut model, Msg::NewSession);

        let project_id = model.projects[0].id;
        let first_session = model.projects[0].sessions[0].id;
        let second_session = model.projects[0].sessions[1].id;

        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: second_session,
        });
        model.focus = Focus::Terminal;

        let mut key = Key::new(KeyCode::Char('1'));
        key.super_key = true;
        update(&mut model, Msg::Key(key));

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(
            model.active,
            Some(SessionKey {
                project: project_id,
                worktree: None,
                session: first_session
            })
        );
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, None, first_session))
        );
    }

    #[test]
    fn cmd_number_does_not_change_when_session_missing() {
        let Some(mut model) = test_model() else {
            return;
        };

        let project_id = model.projects[0].id;
        let active_session = model.projects[0].sessions[0].id;

        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: active_session,
        });
        model.focus = Focus::Terminal;

        let mut key = Key::new(KeyCode::Char('3'));
        key.super_key = true;
        update(&mut model, Msg::Key(key));

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(
            model.active,
            Some(SessionKey {
                project: project_id,
                worktree: None,
                session: active_session
            })
        );
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, None, active_session))
        );
    }

    #[test]
    fn ctrl_comma_opens_and_applies_session_rename() {
        let Some(mut model) = test_model() else {
            return;
        };
        let (project_id, worktree_id, session_id, initial_name) = {
            let (project, session) = model.active_session().expect("active session");
            let worktree = model.active.map(|active| active.worktree).unwrap_or(None);
            (
                project.id,
                worktree,
                session.id,
                session
                    .custom_title
                    .clone()
                    .or_else(|| session.title.clone())
                    .unwrap_or_else(|| format!("session{}", session.number)),
            )
        };

        let mut key = Key::new(KeyCode::Char(','));
        key.ctrl = true;
        update(&mut model, Msg::Key(key));

        let (target_project, target_worktree, target_session) = match model.modal.as_ref() {
            Some(ModalState::RenameSession {
                project,
                worktree,
                session,
                input,
                ..
            }) => {
                assert_eq!(*project, project_id);
                assert_eq!(*worktree, worktree_id);
                assert_eq!(*session, session_id);
                assert_eq!(input.value(), initial_name);
                (*project, *worktree, *session)
            }
            _ => panic!("expected rename modal to open"),
        };

        if let Some(ModalState::RenameSession { input, .. }) = model.modal.as_mut() {
            input.set_value("custom session");
        }

        update(&mut model, Msg::Modal(ModalMsg::Submit));

        let session = model
            .project(target_project)
            .and_then(|project| project.session(target_worktree, target_session))
            .expect("session still present after rename");
        assert_eq!(session.display_name(), "custom session");
        assert!(model.modal.is_none());
    }

    #[test]
    fn bell_indicator_shows_for_session() {
        let Some(mut model) = test_model() else {
            return;
        };
        let TreeId::Session(pid, worktree, sid) =
            model.tree.selected().cloned().expect("selected session")
        else {
            panic!("expected session selected");
        };

        if let Some(project) = model.project_mut(pid) {
            if let Some(session) = project.session_mut(worktree, sid) {
                session.bell = true;
            }
        }

        rebuild_tree(&model.projects, &mut model.tree, &mut model.active);

        let visible_session = model
            .tree
            .visible()
            .iter()
            .find(|node| matches!(node.id, TreeId::Session(_, _, _)))
            .expect("session visible");

        let label = visible_session
            .label
            .iter()
            .find(|span| span.content.contains('🔔'))
            .map(|span| span.content.clone());

        assert!(
            label.is_some(),
            "session label should contain bell indicator when bell is set"
        );
    }

    #[test]
    fn active_session_clears_bell_indicator() {
        let Some(mut model) = test_model() else {
            return;
        };
        let TreeId::Session(pid, worktree, sid) =
            model.tree.selected().cloned().expect("selected session")
        else {
            panic!("expected session selected");
        };

        if let Some(project) = model.project_mut(pid) {
            if let Some(session) = project.session_mut(worktree, sid) {
                session.bell = true;
            }
        }

        model.sync_session_state(&TreeId::Session(pid, worktree, sid));
        rebuild_tree(&model.projects, &mut model.tree, &mut model.active);

        let visible_session = model
            .tree
            .visible()
            .iter()
            .find(|node| matches!(node.id, TreeId::Session(_, _, _)))
            .expect("session visible");

        let label = visible_session
            .label
            .iter()
            .find(|span| span.content.contains('🔔'))
            .map(|span| span.content.clone());

        assert!(
            label.is_none(),
            "active session label should not contain bell indicator"
        );
    }

    #[test]
    fn background_activity_marks_unread_indicator() {
        let Some(mut model) = test_model() else {
            return;
        };
        update(&mut model, Msg::NewSession);

        let project_id = model.projects[0].id;
        let first_session = model.projects[0].sessions[0].id;
        let second_session = model.projects[0].sessions[1].id;

        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: first_session,
        });
        model.focus = Focus::Terminal;

        if let Some(session) = model
            .projects
            .get(0)
            .and_then(|project| project.sessions.iter().find(|s| s.id == second_session))
        {
            session.terminal.test_trigger_wakeup();
        }

        update(
            &mut model,
            Msg::SessionWake(TreeId::Session(project_id, None, second_session)),
        );

        let target = model
            .tree
            .visible()
            .iter()
            .find(|node| {
                matches!(
                    node.id,
                    TreeId::Session(pid, worktree, sid)
                        if pid == project_id && sid == second_session && worktree.is_none()
                )
            })
            .expect("second session visible");

        assert!(
            target.label.iter().any(|span| span.content.contains('•')),
            "expected unread indicator dot on inactive session"
        );

        let active_has_unread = model.projects[0]
            .sessions
            .iter()
            .find(|s| s.id == first_session)
            .expect("first session exists")
            .has_unread_output;
        assert!(
            !active_has_unread,
            "active session should not show unread output"
        );
    }

    #[test]
    fn selecting_session_clears_unread_indicator() {
        let Some(mut model) = test_model() else {
            return;
        };
        update(&mut model, Msg::NewSession);

        let project_id = model.projects[0].id;
        let first_session = model.projects[0].sessions[0].id;
        let second_session = model.projects[0].sessions[1].id;

        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: first_session,
        });

        if let Some(session) = model
            .projects
            .get(0)
            .and_then(|project| project.sessions.iter().find(|s| s.id == second_session))
        {
            session.terminal.test_trigger_wakeup();
        }

        update(
            &mut model,
            Msg::SessionWake(TreeId::Session(project_id, None, second_session)),
        );

        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: second_session,
        });

        let target = model
            .tree
            .visible()
            .iter()
            .find(|node| {
                matches!(
                    node.id,
                    TreeId::Session(pid, worktree, sid)
                        if pid == project_id && sid == second_session && worktree.is_none()
                )
            })
            .expect("second session visible");

        assert!(
            !target.label.iter().any(|span| span.content.contains('•')),
            "unread indicator should clear after selecting the session"
        );

        let cleared = model.projects[0]
            .sessions
            .iter()
            .find(|s| s.id == second_session)
            .expect("second session exists")
            .has_unread_output;
        assert!(
            !cleared,
            "has_unread_output flag should reset after selecting the session"
        );
    }

    #[test]
    fn renders_sidebar_tree() {
        let Some(mut model) = test_model() else {
            return;
        };
        update(&mut model, Msg::NewSession);
        model.focus = Focus::Sidebar;

        let project_name = &model.projects[0].name;
        let mut node = sidebar_view(
            &model.tree,
            &model.tree_scroll,
            model.auto_hide,
            true,
            Msg::Tree,
            || Msg::FocusSidebar,
        )
        .with_fill();
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(22.0),
                height: taffy::AvailableSpace::Definite(8.0),
            },
        );
        round_layout(&mut node);

        let mut buffer = DoubleBuffer::new(22, 8);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer
            .render(&node, Size::new(22, 8))
            .expect("render should succeed");

        let lines: Vec<String> = renderer
            .buffer()
            .to_string()
            .lines()
            .map(|s| s.to_owned())
            .collect();

        let project_label: String = project_name.chars().take(18).collect();
        let project_line = format!("│v {:<18}│", project_label);
        let top = "┌Sessions────────────┐".to_string();
        let bottom = "└────────────────────┘".to_string();
        let session_lines: Vec<String> = model.projects[0]
            .sessions
            .iter()
            .map(|session| format!("│  - {:<16}│", session.display_name()))
            .collect();

        let mut expected = vec![top, project_line];
        expected.extend(session_lines);
        while expected.len() + 1 < 8 {
            expected.push("│                    │".to_string());
        }
        expected.push(bottom);
        assert_eq!(lines, expected,);
    }
}
