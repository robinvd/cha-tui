//! Session manager demo with a sidebar tree and a single terminal pane.

mod focus;
mod modal;
mod persistence;
mod project;
mod session;
mod sidebar;
mod status;

use std::path::PathBuf;
use std::io;

#[cfg(test)]
use once_cell::sync::Lazy;
#[cfg(test)]
use std::sync::Mutex;

use chatui::event::{Event, Key, KeyCode};
use chatui::{
    InputMsg, InputState, Node, Program, TerminalMsg, Transition, TreeMsg, TreeState,
    block_with_title, column, default_terminal_keybindings, row, terminal, text,
};
use smol::channel::Receiver;
use tracing::warn;

use focus::Focus;
use modal::{ModalMsg, ModalResult, ModalState, modal_handle_key, modal_update, modal_view};
use persistence::{load_projects, save_projects};
use project::{Project, ProjectId};
use session::{Session, SessionId};
use sidebar::{
    TreeId, move_project_down, move_project_up, next_session, prev_session,
    rebuild_tree, section_style, sidebar_view,
};
use status::{StatusMessage, status_bar_view};

#[cfg(test)]
static TEST_MODEL_GUARD: Lazy<Mutex<()>> = Lazy::new(|| Mutex::new(()));

struct Model {
    projects: Vec<Project>,
    tree: TreeState<TreeId>,
    focus: Focus,
    auto_hide: bool,
    active: Option<(ProjectId, SessionId)>,
    modal: Option<ModalState>,
    initial_update: bool,
    next_project_id: u64,
    next_session_id: u64,
    /// When true, skip writing to the config file (used in tests).
    skip_persist: bool,
    status: Option<StatusMessage>,
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
            focus: Focus::Sidebar,
            auto_hide: false,
            active: None,
            modal: None,
            initial_update: true,
            next_project_id: 1,
            next_session_id: 1,
            skip_persist,
            status: None,
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
        let session = self.spawn_session(&project.path.clone())?;
        project.add_session(session);

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

    fn spawn_session(&mut self, path: &PathBuf) -> std::io::Result<Session> {
        let id = SessionId(self.next_session_id);
        self.next_session_id += 1;
        // Find the project to get the next session number
        let number = self
            .projects
            .iter()
            .find(|p| &p.path == path)
            .map(|p| p.next_session_number)
            .unwrap_or(1);
        Session::spawn(path, number, id)
    }

    fn active_session(&self) -> Option<(&Project, &Session)> {
        let (pid, sid) = self.active?;
        let project = self.projects.iter().find(|p| p.id == pid)?;
        let session = project.session(sid)?;
        Some((project, session))
    }



    fn project(&self, pid: ProjectId) -> Option<&Project> {
        self.projects.iter().find(|p| p.id == pid)
    }

    fn project_mut(&mut self, pid: ProjectId) -> Option<&mut Project> {
        self.projects.iter_mut().find(|p| p.id == pid)
    }

    fn select_session(&mut self, pid: ProjectId, sid: SessionId) {
        let mut cleared_bell = false;
        let mut cleared_unread = false;

        if let Some(project) = self.project_mut(pid)
            && let Some(session) = project.session_mut(sid)
        {
            cleared_bell = session.clear_bell();
            cleared_unread = session.mark_seen();
        }

        self.active = Some((pid, sid));
        self.tree.select(TreeId::Session(pid, sid));

        if cleared_bell || cleared_unread {
            rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
        }
    }

    fn sync_active_from_tree_selection(&mut self) {
        if let Some(TreeId::Session(pid, sid)) = self.tree.selected().cloned() {
            let mut cleared_bell = false;
            let mut cleared_unread = false;

            if let Some(project) = self.project_mut(pid)
                && let Some(session) = project.session_mut(sid)
            {
                cleared_bell = session.clear_bell();
                cleared_unread = session.mark_seen();
            }

            self.active = Some((pid, sid));

            if cleared_bell || cleared_unread {
                rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
            }
        }
    }

    fn wakeup_for(&self, id: &TreeId) -> Option<Receiver<()>> {
        match id {
            TreeId::Session(pid, sid) => {
                let project = self.project(*pid)?;
                let session = project.session(*sid)?;
                Some(session.wakeup.clone())
            }
            TreeId::Project(_) => None,
        }
    }

    fn sync_session_state(&mut self, id: &TreeId) -> bool {
        let TreeId::Session(pid, sid) = id else {
            return false;
        };

        let is_active = self
            .active
            .map(|active| active == (*pid, *sid))
            .unwrap_or(false);

        let Some(project) = self.project_mut(*pid) else {
            return false;
        };
        let Some(session) = project.session_mut(*sid) else {
            return false;
        };

        let mut changed = false;
        changed |= session.sync_title();
        changed |= session.sync_bell();
        changed |= session.sync_exited();
        changed |= session.sync_activity(is_active);
        changed
    }

    fn ensure_project_has_session(
        &mut self,
        project_index: usize,
    ) -> std::io::Result<Option<TreeId>> {
        if self.projects[project_index].sessions.is_empty() {
            let project_id = self.projects[project_index].id;
            let path = self.projects[project_index].path.clone();

            let id = SessionId(self.next_session_id);
            self.next_session_id += 1;
            let number = self.projects[project_index].next_session_number;
            let session = Session::spawn(&path, number, id)?;
            let tree_id = TreeId::Session(project_id, session.id);

            self.projects[project_index].add_session(session);
            rebuild_tree(&self.projects, &mut self.tree, &mut self.active);
            Ok(Some(tree_id))
        } else {
            Ok(None)
        }
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
    Tree(TreeMsg<TreeId>),
    Terminal {
        project: ProjectId,
        session: SessionId,
        msg: TerminalMsg,
    },
    SessionWake(TreeId),
    FocusGained,
    FocusLost,
    OpenNewProject,
    Modal(ModalMsg),
    NewSession,
    DeleteSelected,
    ModalInput(InputMsg),
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
            | Msg::Tree(_)
            | Msg::Modal(_)
            | Msg::ModalInput(_)
            | Msg::FocusSidebar
            | Msg::OpenNewProject
            | Msg::NewSession
            | Msg::DeleteSelected
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
                    TreeId::Session(project.id, session.id),
                    session.wakeup.clone(),
                ));
            }
        }
    }

    match msg {
        Msg::FocusSidebar => {
            if model.focus == Focus::Terminal
                && let Some((pid, sid)) = model.active
            {
                transitions.push(update(
                    model,
                    Msg::Terminal {
                        project: pid,
                        session: sid,
                        msg: TerminalMsg::FocusLost,
                    },
                ));
            }
            model.focus = Focus::Sidebar;
        }
        Msg::Key(key) => {
            if key.ctrl && key.code == KeyCode::Char('b') {
                let old_focus = model.focus;
                model.focus = model.focus.toggle();

                if old_focus != model.focus
                    && let Some((pid, sid)) = model.active
                {
                    let msg = match model.focus {
                        Focus::Terminal => TerminalMsg::FocusGained,
                        Focus::Sidebar => TerminalMsg::FocusLost,
                    };
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: pid,
                            session: sid,
                            msg,
                        },
                    ));
                }

                transitions.push(Transition::Continue);
                return Transition::Multiple(transitions);
            }

            // Modal key handling
            if model.modal.is_some() {
                if let Some(modal_msg) = modal_handle_key(key) {
                    return update(model, Msg::Modal(modal_msg));
                }
                transitions.push(Transition::Continue);
                return Transition::Multiple(transitions);
            }

            // Session navigation with Ctrl+Up/Down
            if key.ctrl
                && key.code == KeyCode::Up
                && let Some((pid, sid)) = prev_session(&model.tree, model.active)
            {
                if let Some((old_pid, old_sid)) = model.active {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: old_pid,
                            session: old_sid,
                            msg: TerminalMsg::FocusLost,
                        },
                    ));
                }
                model.select_session(pid, sid);
                if model.focus == Focus::Terminal {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: pid,
                            session: sid,
                            msg: TerminalMsg::FocusGained,
                        },
                    ));
                }
                return Transition::Multiple(transitions);
            }
            if key.ctrl
                && key.code == KeyCode::Down
                && let Some((pid, sid)) = next_session(&model.tree, model.active)
            {
                if let Some((old_pid, old_sid)) = model.active {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: old_pid,
                            session: old_sid,
                            msg: TerminalMsg::FocusLost,
                        },
                    ));
                }
                model.select_session(pid, sid);
                if model.focus == Focus::Terminal {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: pid,
                            session: sid,
                            msg: TerminalMsg::FocusGained,
                        },
                    ));
                }
                return Transition::Multiple(transitions);
            }

            match model.focus {
                Focus::Sidebar => match key.code {
                    KeyCode::Char('q') if key.ctrl => {
                        return Transition::Quit;
                    }
                    KeyCode::Char('h') => {
                        model.auto_hide = !model.auto_hide;
                        return Transition::Continue;
                    }
                    KeyCode::Right if key.ctrl => {
                        model.focus = Focus::Terminal;
                        if let Some((pid, sid)) = model.active {
                            transitions.push(update(
                                model,
                                Msg::Terminal {
                                    project: pid,
                                    session: sid,
                                    msg: TerminalMsg::FocusGained,
                                },
                            ));
                        }
                        return Transition::Multiple(transitions);
                    }
                    KeyCode::Up if key.shift => {
                        if let Some(selected) = model.tree.selected().cloned() {
                            let moved = match selected {
                                TreeId::Project(pid) => move_project_up(&mut model.projects, pid),
                                TreeId::Session(pid, sid) => {
                                    if let Some(project) = model.project_mut(pid) {
                                        project.move_session_up(sid)
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
                        return Transition::Continue;
                    }
                    KeyCode::Down if key.shift => {
                        if let Some(selected) = model.tree.selected().cloned() {
                            let moved = match selected {
                                TreeId::Project(pid) => move_project_down(&mut model.projects, pid),
                                TreeId::Session(pid, sid) => {
                                    if let Some(project) = model.project_mut(pid) {
                                        project.move_session_down(sid)
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
                        return Transition::Continue;
                    }
                    KeyCode::Up | KeyCode::Char('k') => {
                        model.tree.ensure_selected();
                        model.tree.select_prev();
                        model.sync_active_from_tree_selection();
                        return Transition::Continue;
                    }
                    KeyCode::Down | KeyCode::Char('j') => {
                        model.tree.ensure_selected();
                        model.tree.select_next();
                        model.sync_active_from_tree_selection();
                        return Transition::Continue;
                    }
                    KeyCode::Enter => {
                        if let Some(selected) = model.tree.selected().cloned() {
                            match selected {
                                TreeId::Project(pid) => {
                                    model.tree.toggle_expanded(&selected);
                                    if let Some(project) = model.project(pid)
                                        && let Some(session) = project.sessions.first()
                                    {
                                        model.select_session(pid, session.id);
                                    }
                                }
                                TreeId::Session(pid, sid) => {
                                    model.select_session(pid, sid);
                                    model.focus = Focus::Terminal;
                                }
                            }
                            return Transition::Continue;
                        }
                    }
                    KeyCode::Char('p') => return update(model, Msg::OpenNewProject),
                    KeyCode::Char('n') => return update(model, Msg::NewSession),
                    KeyCode::Char('d') => return update(model, Msg::DeleteSelected),
                    _ => {}
                },
                Focus::Terminal => {
                    if let Some((pid, sid)) = model.active
                        && let Some(msg) =
                            default_terminal_keybindings(key, move |term_msg| Msg::Terminal {
                                project: pid,
                                session: sid,
                                msg: term_msg,
                            })
                    {
                        return update(model, msg);
                    }
                }
            }
        }
        Msg::Tree(tree_msg) => match tree_msg {
            TreeMsg::ToggleExpand(id) => {
                model.tree.toggle_expanded(&id);
                model.focus = Focus::Sidebar;
            }
            TreeMsg::Activate(TreeId::Session(pid, sid))
            | TreeMsg::DoubleClick(TreeId::Session(pid, sid)) => {
                if let Some((old_pid, old_sid)) = model.active {
                    transitions.push(update(
                        model,
                        Msg::Terminal {
                            project: old_pid,
                            session: old_sid,
                            msg: TerminalMsg::FocusLost,
                        },
                    ));
                }
                model.select_session(pid, sid);
                model.focus = Focus::Terminal;
                transitions.push(update(
                    model,
                    Msg::Terminal {
                        project: pid,
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
        },
        Msg::Terminal {
            project,
            session,
            msg,
        } => {
            if let Some(proj) = model.project_mut(project)
                && let Some(sess) = proj.session_mut(session)
            {
                sess.update(msg);
                let wakeup = sess.wakeup.clone();
                transitions.push(Transition::Continue);
                transitions.push(arm_wakeup(TreeId::Session(project, session), wakeup));
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
                && let Some((pid, sid)) = model.active
            {
                return update(
                    model,
                    Msg::Terminal {
                        project: pid,
                        session: sid,
                        msg: TerminalMsg::FocusGained,
                    },
                );
            }
        }
        Msg::FocusLost => {
            if model.focus == Focus::Terminal
                && let Some((pid, sid)) = model.active
            {
                return update(
                    model,
                    Msg::Terminal {
                        project: pid,
                        session: sid,
                        msg: TerminalMsg::FocusLost,
                    },
                );
            }
        }
        Msg::OpenNewProject => {
            model.modal = Some(ModalState::NewProject {
                input: InputState::new(),
            });
            model.focus = Focus::Sidebar;
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
                        } else if let Some(project) = model.projects.last()
                            && let Some(session) = project.sessions.first()
                        {
                            model.select_session(project.id, session.id);
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
            let project_id = match selected {
                TreeId::Project(pid) => pid,
                TreeId::Session(pid, _) => pid,
            };

            let Some(project_index) = model.projects.iter().position(|p| p.id == project_id)
            else {
                return Transition::Continue;
            };

            let path = model.projects[project_index].path.clone();
            let number = model.projects[project_index].next_session_number;

            let id = SessionId(model.next_session_id);
            model.next_session_id += 1;

            let session = match Session::spawn(&path, number, id) {
                Ok(session) => session,
                Err(err) => {
                    warn!(?err, "failed to create new session");
                    model.status = Some(StatusMessage::error(format!(
                        "Failed to create session: {err}"
                    )));
                    return Transition::Continue;
                }
            };

            let sid = session.id;
            model.projects[project_index].add_session(session);

            rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
            model.select_session(project_id, sid);
            model.focus = Focus::Terminal;

            if let Some(receiver) = model.wakeup_for(&TreeId::Session(project_id, sid)) {
                transitions.push(arm_wakeup(TreeId::Session(project_id, sid), receiver));
            }
        }
        Msg::DeleteSelected => {
            let Some(selected) = model.tree.selected().cloned() else {
                return Transition::Continue;
            };

            match selected {
                TreeId::Project(pid) => {
                    if let Some(pos) = model.projects.iter().position(|p| p.id == pid) {
                        model.projects.remove(pos);
                    }
                    rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
                    model.focus = Focus::Sidebar;
                    model.save();
                }
                TreeId::Session(pid, sid) => {
                    if let Some(project_idx) = model.projects.iter().position(|p| p.id == pid) {
                        let became_empty = {
                            let project = &mut model.projects[project_idx];
                            project.remove_session(sid);
                            project.sessions.is_empty()
                        };

                        if became_empty {
                            if let Ok(Some(TreeId::Session(new_pid, new_sid))) =
                                model.ensure_project_has_session(project_idx)
                            {
                                model.select_session(new_pid, new_sid);
                            }
                        } else if let Some(next_session) =
                            model.projects[project_idx].sessions.first()
                        {
                            model.select_session(pid, next_session.id);
                        }
                        rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
                        model.focus = Focus::Sidebar;
                    }
                }
            }
        }
        Msg::ModalInput(input_msg) => {
            if let Some(ModalState::NewProject { input }) = model.modal.as_mut() {
                input.update(input_msg);
            }
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

fn terminal_pane(model: &Model) -> Node<Msg> {
    if let Some((project, session)) = model.active_session() {
        let pid = project.id;
        let sid = session.id;
        terminal(
            "main-terminal",
            &session.terminal,
            model.focus == Focus::Terminal,
            move |msg| Msg::Terminal {
                project: pid,
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
    );

    let mut nodes = vec![content, status];

    if let Some(modal_state) = &model.modal {
        nodes.push(modal_view(modal_state, Msg::ModalInput));
    }

    column(nodes).with_fill()
}

fn main() {
    color_eyre::install().expect("failed to install color-eyre");

    // Set up tracing to file instead of stdout (which would mess up the TUI)
    use std::fs::File;
    use tracing_subscriber::fmt;
    use tracing_subscriber::prelude::*;
    let log_file = File::create("./term_debug.log").expect("failed to create log file");
    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(log_file).with_ansi(false))
        .init();

    let model = Model::new().expect("failed to create terminals");

    let program = Program::new(model, update, view).map_event(|event| match event {
        Event::Key(key) => Some(Msg::Key(key)),
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
    use chatui::event::Size;
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
        let TreeId::Session(pid, sid) = model.tree.selected().cloned().expect("selected") else {
            panic!("expected a session selected");
        };

        update(&mut model, Msg::DeleteSelected);

        let project = model.projects.first().expect("project present");
        assert_eq!(project.sessions.len(), 1);
        let new_session = &project.sessions[0];
        assert_ne!(new_session.id, sid);
        assert_eq!(new_session.number, 2);
        assert_eq!(model.active, Some((pid, new_session.id)));
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
                second_session,
            ))),
        );

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(model.active, Some((project_id, second_session)));
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, second_session))
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

        model.select_session(project_id, first_session);
        model.focus = Focus::Terminal;

        let mut key = Key::new(KeyCode::Down);
        key.ctrl = true;
        update(&mut model, Msg::Key(key));

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(model.active, Some((project_id, second_session)));
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, second_session))
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

        model.select_session(project_id, second_session);
        model.focus = Focus::Terminal;

        let mut key = Key::new(KeyCode::Up);
        key.ctrl = true;
        update(&mut model, Msg::Key(key));

        assert_eq!(model.focus, Focus::Terminal);
        assert_eq!(model.active, Some((project_id, first_session)));
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(project_id, first_session))
        );
    }

    #[test]
    fn bell_indicator_shows_for_session() {
        let Some(mut model) = test_model() else {
            return;
        };
        let TreeId::Session(pid, sid) = model.tree.selected().cloned().expect("selected session")
        else {
            panic!("expected session selected");
        };

        if let Some(project) = model.project_mut(pid) {
            if let Some(session) = project.session_mut(sid) {
                session.bell = true;
            }
        }

        rebuild_tree(&model.projects, &mut model.tree, &mut model.active);

        let visible_session = model
            .tree
            .visible()
            .iter()
            .find(|node| matches!(node.id, TreeId::Session(_, _)))
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
    fn background_activity_marks_unread_indicator() {
        let Some(mut model) = test_model() else {
            return;
        };
        update(&mut model, Msg::NewSession);

        let project_id = model.projects[0].id;
        let first_session = model.projects[0].sessions[0].id;
        let second_session = model.projects[0].sessions[1].id;

        model.select_session(project_id, first_session);
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
            Msg::SessionWake(TreeId::Session(project_id, second_session)),
        );

        let target = model
            .tree
            .visible()
            .iter()
            .find(|node| {
                matches!(
                    node.id,
                    TreeId::Session(pid, sid) if pid == project_id && sid == second_session
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

        model.select_session(project_id, first_session);

        if let Some(session) = model
            .projects
            .get(0)
            .and_then(|project| project.sessions.iter().find(|s| s.id == second_session))
        {
            session.terminal.test_trigger_wakeup();
        }

        update(
            &mut model,
            Msg::SessionWake(TreeId::Session(project_id, second_session)),
        );

        model.select_session(project_id, second_session);

        let target = model
            .tree
            .visible()
            .iter()
            .find(|node| {
                matches!(
                    node.id,
                    TreeId::Session(pid, sid) if pid == project_id && sid == second_session
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

        let project_line = format!("│v {:<18}│", project_name);
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
