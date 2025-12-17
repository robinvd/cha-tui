//! Session manager demo with a sidebar tree and a single terminal pane.

use std::path::PathBuf;
use std::{fs, io};

use chatui::dom::Color;
use chatui::event::{Event, Key, KeyCode};
use chatui::{
    InputMsg, InputState, InputStyle, Node, Program, Style, TerminalMsg, TerminalState, TextSpan,
    Transition, TreeMsg, TreeNode, TreeState, TreeStyle, block_with_title, column,
    default_input_keybindings, default_terminal_keybindings, modal, row, terminal, text, tree_view,
};
use serde::{Deserialize, Serialize};
use smol::channel::Receiver;
use tracing::warn;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
enum Focus {
    #[default]
    Sidebar,
    Terminal,
}

impl Focus {
    fn toggle(self) -> Self {
        match self {
            Focus::Sidebar => Focus::Terminal,
            Focus::Terminal => Focus::Sidebar,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct ProjectId(u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct SessionId(u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum TreeId {
    Project(ProjectId),
    Session(ProjectId, SessionId),
}

#[derive(Serialize, Deserialize)]
struct PersistedProject {
    name: String,
    path: PathBuf,
}

#[derive(Serialize, Deserialize, Default)]
struct PersistedState {
    projects: Vec<PersistedProject>,
}

struct Session {
    id: SessionId,
    number: usize,
    title: Option<String>,
    bell: bool,
    exited: bool,
    terminal: TerminalState,
    wakeup: Receiver<()>,
    /// Cached display name to detect changes from foreground process
    cached_display_name: String,
}

impl Session {
    fn display_name(&self) -> String {
        let base = self
            .title
            .clone()
            .or_else(|| self.terminal.foreground_process_name())
            .unwrap_or_else(|| format!("session{}", self.number));
        if self.exited {
            format!("{base} [exited]")
        } else if self.bell {
            format!("{base} 🔔")
        } else {
            base
        }
    }

    fn sync_display_name(&mut self) -> bool {
        let new_name = self.display_name();
        if new_name != self.cached_display_name {
            self.cached_display_name = new_name;
            true
        } else {
            false
        }
    }
}

struct Project {
    id: ProjectId,
    name: String,
    path: PathBuf,
    sessions: Vec<Session>,
    next_session_number: usize,
}

impl Project {
    fn new(id: ProjectId, name: String, path: PathBuf) -> Self {
        Self {
            id,
            name,
            path,
            sessions: Vec::new(),
            next_session_number: 1,
        }
    }
}

enum ModalState {
    NewProject { input: InputState },
}

struct Model {
    projects: Vec<Project>,
    tree: TreeState<TreeId>,
    focus: Focus,
    active: Option<(ProjectId, SessionId)>,
    modal: Option<ModalState>,
    initial_update: bool,
    next_project_id: u64,
    next_session_id: u64,
}

impl Model {
    fn new() -> std::io::Result<Self> {
        let mut model = Self {
            projects: Vec::new(),
            tree: TreeState::new(),
            focus: Focus::Sidebar,
            active: None,
            modal: None,
            initial_update: true,
            next_project_id: 1,
            next_session_id: 1,
        };

        let persisted = match Self::load_projects() {
            Ok(data) => data,
            Err(err) => {
                warn!(?err, "failed to load saved projects");
                Vec::new()
            }
        };

        if persisted.is_empty() {
            let cwd = std::env::current_dir()?;
            let name = cwd
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("project")
                .to_string();
            model
                .add_project(cwd, name)
                .expect("failed to create initial project");
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
                model
                    .add_project(cwd, name)
                    .expect("failed to create initial project");
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
        let session = self.spawn_session(&project.path, project.next_session_number)?;
        project.sessions.push(session);
        project.next_session_number += 1;

        self.projects.push(project);
        self.rebuild_tree();
        self.focus = Focus::Sidebar;

        if let Err(err) = self.save_projects() {
            warn!(?err, "failed to save projects");
        }
        Ok(())
    }

    fn spawn_session(&mut self, path: &PathBuf, number: usize) -> std::io::Result<Session> {
        let terminal = TerminalState::with_working_dir(path).or_else(|err| {
            warn!(
                ?err,
                "failed to start terminal in project dir, falling back to default"
            );
            TerminalState::new()
        })?;
        let wakeup = terminal.wakeup_receiver();
        let id = SessionId(self.next_session_id);
        self.next_session_id += 1;
        let mut session = Session {
            id,
            number,
            title: None,
            bell: false,
            exited: false,
            terminal,
            wakeup,
            cached_display_name: String::new(),
        };
        session.cached_display_name = session.display_name();
        Ok(session)
    }

    fn active_session(&self) -> Option<(&Project, &Session)> {
        let (pid, sid) = self.active?;
        let project = self.projects.iter().find(|p| p.id == pid)?;
        let session = project.sessions.iter().find(|s| s.id == sid)?;
        Some((project, session))
    }

    fn tree_style() -> TreeStyle {
        TreeStyle {
            indent: 2,
            ..TreeStyle::default()
        }
    }

    fn rebuild_tree(&mut self) {
        self.active = None;
        let prev_selected = self
            .tree
            .selected()
            .cloned()
            .or_else(|| self.active.map(|(pid, sid)| TreeId::Session(pid, sid)));

        let mut items = Vec::new();
        for project in &self.projects {
            let children = project
                .sessions
                .iter()
                .map(|session| {
                    let mut row_style = Style::default();
                    let mut selected_row_style = None;
                    if let Some((active_pid, active_sid)) = self.active
                        && active_pid == project.id
                        && active_sid == session.id
                    {
                        row_style.fg = Some(Color::Green);
                        let mut sel = Style::bold();
                        sel.fg = Some(Color::Green);
                        selected_row_style = Some(sel);
                    }

                    TreeNode::leaf(
                        TreeId::Session(project.id, session.id),
                        vec![TextSpan::new(session.display_name(), Style::default())],
                    )
                    .with_row_style(row_style)
                    .with_selected_row_style(selected_row_style.unwrap_or_else(Style::bold))
                })
                .collect::<Vec<_>>();

            let mut project_label = Style::bold();
            project_label.fg = Some(Color::rgb(180, 180, 180));
            let label = vec![TextSpan::new(project.name.clone(), project_label)];

            items.push(TreeNode::branch(
                TreeId::Project(project.id),
                label,
                children,
            ));
        }

        self.tree.set_items(items);
        self.tree.expand_all();

        if let Some(selected) = prev_selected
            && self.tree.visible().iter().any(|v| v.id == selected)
        {
            self.tree.select(selected);
        }

        if self.tree.selected().is_none()
            && let Some((pid, sid)) = self.first_session_id()
        {
            self.tree.select(TreeId::Session(pid, sid));
        }

        if let Some(TreeId::Session(pid, sid)) = self.tree.selected().cloned() {
            self.active = Some((pid, sid));
        }
    }

    fn first_session_id(&self) -> Option<(ProjectId, SessionId)> {
        self.projects
            .first()
            .and_then(|p| p.sessions.first().map(|s| (p.id, s.id)))
    }

    fn select_session(&mut self, pid: ProjectId, sid: SessionId) {
        let cleared_bell = self.clear_session_bell(pid, sid);
        self.active = Some((pid, sid));
        self.tree.select(TreeId::Session(pid, sid));
        if cleared_bell {
            self.rebuild_tree();
        }
    }

    fn sync_active_from_tree_selection(&mut self) {
        if let Some(TreeId::Session(pid, sid)) = self.tree.selected().cloned() {
            self.active = Some((pid, sid));
        }
    }

    fn next_session(&self) -> Option<(ProjectId, SessionId)> {
        let (active_pid, active_sid) = self.active?;
        let mut seen_active = false;

        for node in self.tree.visible() {
            if let TreeId::Session(pid, sid) = node.id {
                if seen_active {
                    return Some((pid, sid));
                }
                if pid == active_pid && sid == active_sid {
                    seen_active = true;
                }
            }
        }

        None
    }

    fn prev_session(&self) -> Option<(ProjectId, SessionId)> {
        let (active_pid, active_sid) = self.active?;
        let mut previous = None;

        for node in self.tree.visible() {
            if let TreeId::Session(pid, sid) = node.id {
                if pid == active_pid && sid == active_sid {
                    break;
                }
                previous = Some((pid, sid));
            }
        }

        previous
    }

    fn wakeup_for(&self, id: &TreeId) -> Option<Receiver<()>> {
        match id {
            TreeId::Session(pid, sid) => {
                let project = self.projects.iter().find(|p| p.id == *pid)?;
                let session = project.sessions.iter().find(|s| s.id == *sid)?;
                Some(session.wakeup.clone())
            }
            TreeId::Project(_) => None,
        }
    }

    fn sync_session_title(&mut self, id: &TreeId) -> bool {
        let TreeId::Session(pid, sid) = id else {
            return false;
        };

        let Some(project) = self.projects.iter_mut().find(|p| p.id == *pid) else {
            return false;
        };
        let Some(session) = project.sessions.iter_mut().find(|s| s.id == *sid) else {
            return false;
        };

        // Sync the OSC title from the terminal
        let new_title = session.terminal.title();
        if new_title != session.title {
            session.title = new_title;
        }

        // Check if display name changed (includes foreground process)
        session.sync_display_name()
    }

    fn sync_session_bell(&mut self, id: &TreeId) -> bool {
        let TreeId::Session(pid, sid) = id else {
            return false;
        };

        let Some(project) = self.projects.iter_mut().find(|p| p.id == *pid) else {
            return false;
        };
        let Some(session) = project.sessions.iter_mut().find(|s| s.id == *sid) else {
            return false;
        };

        if session.terminal.take_bell() && !session.bell {
            session.bell = true;
            return true;
        }

        false
    }

    fn clear_session_bell(&mut self, pid: ProjectId, sid: SessionId) -> bool {
        let Some(project) = self.projects.iter_mut().find(|p| p.id == pid) else {
            return false;
        };
        let Some(session) = project.sessions.iter_mut().find(|s| s.id == sid) else {
            return false;
        };

        if session.bell {
            session.bell = false;
            return true;
        }

        false
    }

    fn sync_session_exited(&mut self, id: &TreeId) -> bool {
        let TreeId::Session(pid, sid) = id else {
            return false;
        };

        let Some(project) = self.projects.iter_mut().find(|p| p.id == *pid) else {
            return false;
        };
        let Some(session) = project.sessions.iter_mut().find(|s| s.id == *sid) else {
            return false;
        };

        if !session.terminal.is_running() && !session.exited {
            session.exited = true;
            return true;
        }

        false
    }

    fn ensure_project_has_session(
        &mut self,
        project_index: usize,
    ) -> std::io::Result<Option<TreeId>> {
        if self.projects[project_index].sessions.is_empty() {
            let project_id = self.projects[project_index].id;
            let path = self.projects[project_index].path.clone();
            let next_number = self.projects[project_index].next_session_number;

            let session = self.spawn_session(&path, next_number)?;
            let id = TreeId::Session(project_id, session.id);
            {
                let project = &mut self.projects[project_index];
                project.sessions.push(session);
                project.next_session_number += 1;
            }
            self.rebuild_tree();
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }

    fn config_path() -> io::Result<PathBuf> {
        if let Some(dir) = std::env::var_os("CHATUI_CONFIG_DIR") {
            return Ok(PathBuf::from(dir).join("term_projects.json"));
        }

        directories::ProjectDirs::from("", "", "chatui")
            .map(|dirs| dirs.config_dir().join("term_projects.json"))
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "config directory unavailable"))
    }

    fn load_projects() -> io::Result<Vec<PersistedProject>> {
        let path = Self::config_path()?;
        if !path.exists() {
            return Ok(Vec::new());
        }

        let data = fs::read_to_string(&path)?;
        let state: PersistedState =
            serde_json::from_str(&data).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;
        Ok(state.projects)
    }

    fn save_projects(&self) -> io::Result<()> {
        let path = Self::config_path()?;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }

        let projects = self
            .projects
            .iter()
            .map(|project| PersistedProject {
                name: project.name.clone(),
                path: project.path.clone(),
            })
            .collect();
        let state = PersistedState { projects };
        let data = serde_json::to_string_pretty(&state).map_err(io::Error::other)?;
        fs::write(path, data)?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
enum Msg {
    FocusSidebar,
    FocusTerminal,
    Key(Key),
    Tree(TreeMsg<TreeId>),
    Terminal {
        project: ProjectId,
        session: SessionId,
        msg: TerminalMsg,
    },
    SessionWake(TreeId),
    OpenNewProject,
    SubmitProject,
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

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    let mut transitions = Vec::new();

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
            model.focus = Focus::Sidebar;
        }
        Msg::FocusTerminal => {
            model.focus = Focus::Terminal;
        }
        Msg::Key(key) => {
            if key.ctrl && key.code == KeyCode::Char('o') {
                model.focus = model.focus.toggle();
                transitions.push(Transition::Continue);
                return Transition::Multiple(transitions);
            }

            if let Some(ModalState::NewProject { input }) = model.modal.as_mut() {
                if key.code == KeyCode::Esc {
                    model.modal = None;
                    return Transition::Continue;
                }
                if key.code == KeyCode::Enter {
                    return update(model, Msg::SubmitProject);
                }
                if let Some(msg) = default_input_keybindings(input, key, Msg::ModalInput) {
                    return update(model, msg);
                }
                transitions.push(Transition::Continue);
                return Transition::Multiple(transitions);
            }

            match model.focus {
                Focus::Sidebar => match key.code {
                    KeyCode::Char('q') if key.ctrl => {
                        return Transition::Quit;
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
                                    if let Some(project) =
                                        model.projects.iter().find(|p| p.id == pid)
                                        && let Some(session) = project.sessions.first()
                                    {
                                        model.select_session(pid, session.id);
                                        model.focus = Focus::Terminal;
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
                    if key.ctrl
                        && key.code == KeyCode::Up
                        && let Some((pid, sid)) = model.prev_session()
                    {
                        model.select_session(pid, sid);
                        return Transition::Continue;
                    }
                    if key.ctrl
                        && key.code == KeyCode::Down
                        && let Some((pid, sid)) = model.next_session()
                    {
                        model.select_session(pid, sid);
                        return Transition::Continue;
                    }
                    if let Some((project_id, session_id)) = model.active
                        && let Some(msg) =
                            default_terminal_keybindings(key, move |term_msg| Msg::Terminal {
                                project: project_id,
                                session: session_id,
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
                model.select_session(pid, sid);
                model.focus = Focus::Terminal;
            }
            TreeMsg::Activate(TreeId::Project(pid))
            | TreeMsg::DoubleClick(TreeId::Project(pid)) => {
                model.tree.toggle_expanded(&TreeId::Project(pid));
                if let Some(project) = model.projects.iter().find(|p| p.id == pid)
                    && let Some(session) = project.sessions.first()
                {
                    model.select_session(pid, session.id);
                    model.focus = Focus::Terminal;
                }
            }
        },
        Msg::Terminal {
            project,
            session,
            msg,
        } => {
            if let Some(proj) = model.projects.iter_mut().find(|p| p.id == project)
                && let Some(sess) = proj.sessions.iter_mut().find(|s| s.id == session)
            {
                sess.terminal.update(msg);
                transitions.push(Transition::Continue);
                transitions.push(arm_wakeup(
                    TreeId::Session(proj.id, sess.id),
                    sess.wakeup.clone(),
                ));
            }
        }
        Msg::SessionWake(id) => {
            let mut needs_rebuild = false;
            if model.sync_session_title(&id) {
                needs_rebuild = true;
            }
            if model.sync_session_bell(&id) {
                needs_rebuild = true;
            }
            if model.sync_session_exited(&id) {
                needs_rebuild = true;
            }
            if needs_rebuild {
                model.rebuild_tree();
            }
            if let Some(receiver) = model.wakeup_for(&id) {
                transitions.push(arm_wakeup(id, receiver));
            }
        }
        Msg::OpenNewProject => {
            model.modal = Some(ModalState::NewProject {
                input: InputState::new(),
            });
            model.focus = Focus::Sidebar;
        }
        Msg::SubmitProject => {
            let ModalState::NewProject { input } =
                model.modal.take().expect("submit without modal");
            let value = input.value();
            let trimmed = value.trim();
            if trimmed.is_empty() {
                model.modal = None;
                return Transition::Continue;
            }

            let path = PathBuf::from(trimmed);
            let name = path
                .file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| trimmed.to_string());

            if let Err(err) = model.add_project(path, name) {
                warn!(?err, "failed to add project");
            } else if let Some(project) = model.projects.last()
                && let Some(session) = project.sessions.first()
            {
                model.select_session(project.id, session.id);
            }

            model.modal = None;
        }
        Msg::NewSession => {
            let Some(selected) = model.tree.selected().cloned() else {
                return Transition::Continue;
            };
            let project_id = match selected {
                TreeId::Project(pid) => pid,
                TreeId::Session(pid, _) => pid,
            };

            let Some(project_index) = model.projects.iter().position(|p| p.id == project_id) else {
                return Transition::Continue;
            };

            let (pid, path, next_number) = {
                let project = &model.projects[project_index];
                (
                    project.id,
                    project.path.clone(),
                    project.next_session_number,
                )
            };

            let session = match model.spawn_session(&path, next_number) {
                Ok(session) => session,
                Err(err) => {
                    warn!(?err, "failed to create new session");
                    return Transition::Continue;
                }
            };

            let sid = session.id;
            {
                let project = &mut model.projects[project_index];
                project.sessions.push(session);
                project.next_session_number += 1;
            }

            model.rebuild_tree();
            model.select_session(pid, sid);
            model.focus = Focus::Terminal;

            if let Some(receiver) = model.wakeup_for(&TreeId::Session(pid, sid)) {
                transitions.push(arm_wakeup(TreeId::Session(pid, sid), receiver));
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
                    model.rebuild_tree();
                    model.focus = Focus::Sidebar;
                    if let Err(err) = model.save_projects() {
                        warn!(?err, "failed to save projects");
                    }
                }
                TreeId::Session(pid, sid) => {
                    if let Some(project_idx) = model.projects.iter().position(|p| p.id == pid) {
                        let became_empty = {
                            let project = &mut model.projects[project_idx];
                            project.sessions.retain(|s| s.id != sid);
                            project.sessions.is_empty()
                        };

                        if became_empty
                            && let Ok(Some(TreeId::Session(new_pid, new_sid))) =
                                model.ensure_project_has_session(project_idx)
                        {
                            model.select_session(new_pid, new_sid);
                        } else if let Some(next_session) =
                            model.projects[project_idx].sessions.first()
                        {
                            model.select_session(pid, next_session.id);
                        }
                        model.rebuild_tree();
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

fn sidebar(model: &Model) -> Node<Msg> {
    let style = Model::tree_style();
    let tree = if model.tree.visible().is_empty() {
        text::<Msg>("No projects yet. Press p to add one.")
    } else {
        tree_view(
            "session-tree",
            &model.tree,
            &style,
            Msg::Tree,
            model.focus == Focus::Sidebar,
        )
    };

    block_with_title("Sessions", vec![tree.with_fill()])
        .with_flex_grow(0.35)
        .with_flex_shrink(0.0)
        .with_style(section_style(model.focus == Focus::Sidebar))
        .on_click(|| Msg::FocusSidebar)
}

fn terminal_pane(model: &Model) -> Node<Msg> {
    if let Some((project, session)) = model.active_session() {
        let pid = project.id;
        let sid = session.id;
        let title = format!("{} • {}", project.name, session.display_name());
        block_with_title(
            &title,
            vec![
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
                .with_fill(),
            ],
        )
        .with_flex_grow(1.0)
        .with_style(section_style(model.focus == Focus::Terminal))
        .on_click(|| Msg::FocusTerminal)
    } else {
        block_with_title(
            "Terminal",
            vec![text::<Msg>("Select or create a session to start.")],
        )
        .with_flex_grow(1.0)
        .with_style(section_style(model.focus == Focus::Terminal))
    }
}

fn status_bar(model: &Model) -> Node<Msg> {
    let instructions: &[&str] = match model.focus {
        Focus::Sidebar => &[
            "Ctrl-O switch focus",
            "Ctrl-Q quit",
            "p new project",
            "n new session",
            "d delete",
            "j/k navigate",
            "Enter select",
        ],
        Focus::Terminal => &[
            "Ctrl-O switch focus",
            "Ctrl-↑/↓ prev/next session",
        ],
    };
    let label = format!(" Focus: {:?} │ {} ", model.focus, instructions.join(" │ "));

    row(vec![text::<Msg>(label).with_style(
        Style::bg(Color::rgb(40, 40, 40)).merged(&Style::fg(Color::White)),
    )])
}

fn view(model: &Model) -> Node<Msg> {
    let content = row(vec![sidebar(model), terminal_pane(model)]).with_flex_grow(1.0);
    let mut nodes = vec![content, status_bar(model)];

    if let Some(ModalState::NewProject { input }) = &model.modal {
        let mut input_style = InputStyle::default();
        input_style.cursor.bg = Some(Color::rgb(100, 200, 255));
        let prompt = text::<Msg>("Directory: ").with_style(Style::bold());
        let field = chatui::input::<Msg>("project-input", input, &input_style, Msg::ModalInput)
            .with_flex_grow(1.0);
        let inner = row(vec![prompt, field]).with_min_width(taffy::Dimension::length(40.0));

        let modal_node = block_with_title("Add project", vec![inner.with_fill()]).with_fill();
        nodes.push(modal(vec![modal_node]));
    }

    column(nodes).with_fill()
}

fn section_style(active: bool) -> Style {
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

    fn test_model() -> Model {
        Model::new().expect("create model")
    }

    #[test]
    fn deleting_last_session_creates_replacement() {
        let mut model = test_model();
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
        let mut model = test_model();
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
        let mut model = test_model();
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
        let mut model = test_model();
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
        let mut model = test_model();
        let TreeId::Session(pid, sid) = model.tree.selected().cloned().expect("selected session")
        else {
            panic!("expected session selected");
        };

        if let Some(project) = model.projects.iter_mut().find(|p| p.id == pid) {
            if let Some(session) = project.sessions.iter_mut().find(|s| s.id == sid) {
                session.bell = true;
            }
        }

        model.rebuild_tree();

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
    fn renders_sidebar_tree() {
        let mut model = test_model();
        update(&mut model, Msg::NewSession);
        model.focus = Focus::Sidebar;

        let project_name = &model.projects[0].name;
        let mut node = sidebar(&model).with_fill();
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
        assert_eq!(
            lines,
            vec![
                top,
                project_line,
                "│  - session1        │".to_string(),
                "│  - session2        │".to_string(),
                "│                    │".to_string(),
                "│                    │".to_string(),
                "│                    │".to_string(),
                bottom,
            ]
        );
    }
}
