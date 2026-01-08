//! Session manager demo with a sidebar tree and a single terminal pane.

mod divider;
mod filter;
mod focus;
mod git;
mod jj;
mod keymap;
mod modal;
mod persistence;
mod project;
mod remote;
mod session;
mod sidebar;
mod status;
mod term_io;
mod vcs;

use std::collections::HashMap;
use std::io::{self};
use std::path::PathBuf;
use std::sync::Arc;

use taffy::{Dimension, FlexWrap};

const STRIP_TILE_FRACTION: f32 = 0.8;

use chatui::event::{Event, Key, KeyCode};
use chatui::{
    InputMsg, InputState, Node, Program, ScrollAxis, ScrollMsg, ScrollState, ScrollTarget,
    TerminalMsg, TerminalNotification, Transition, TreeMsg, TreeState, block_with_title, column,
    default_terminal_keybindings, row, scrollable_content, terminal, text,
};
use facet::Facet;
use facet_args as args;
use smol::channel::Receiver;
use tracing::warn;

use chatui::dom::Style;
use divider::{horizontal_divider, vertical_divider};
use focus::Focus;
use keymap::{Action, Keymap, Scope};
use modal::{ModalMsg, ModalResult, ModalState, modal_handle_key, modal_update, modal_view};
use project::{Layout, Project, ProjectId, SessionKey, StartupState, Worktree, WorktreeId};
use remote::{
    RemoteClientArgs, RemoteEnvelope, RemoteParams, RemoteProject, RemoteRequest, RemoteResponse,
    RemoteResult, RemoteServer, RemoteSession, RemoteStatus, RemoteWorktree, params_action_name,
    params_input_bytes, params_project_id, params_session_id, params_worktree_id, parse_key_spec,
    parse_vcs_kind, project_selection, remote_container_env_map, remote_env_map,
    resolve_new_session_target, session_selection, worktree_selection,
};
use session::{Session, SessionId};
use sidebar::{
    TreeId, move_project_down, move_project_up, next_session, prev_session, rebuild_tree,
    section_style, sidebar_view,
};
use status::{StatusMessage, status_bar_view};
use term_io::{RealIo, StartupScript, TermIo};
use vcs::VcsKind;

#[derive(Facet)]
struct Args {
    /// Subcommand to run (omit to launch the TUI).
    #[facet(args::subcommand)]
    command: Option<Command>,
}

#[derive(Facet)]
#[repr(u8)]
enum Command {
    /// Send a command to a running term instance over the remote socket.
    Remote {
        /// Force JSON output (default when stdout is not a TTY).
        #[facet(args::named, args::short = 'j', rename = "json")]
        json: bool,
        /// Force text output (default when stdout is a TTY).
        #[facet(args::named, args::short = 't', rename = "text")]
        text: bool,
        /// Socket path (defaults to TERM_REMOTE_SOCKET).
        #[facet(args::named)]
        socket: Option<String>,
        /// Remote action to execute.
        #[facet(args::subcommand)]
        action: RemoteCommand,
    },
}

struct RemoteArgs {
    json: bool,
    text: bool,
    socket: Option<String>,
    action: RemoteCommand,
}

#[derive(Facet)]
#[repr(u8)]
enum RemoteCommand {
    /// App-level commands.
    App {
        #[facet(args::subcommand)]
        command: AppCommand,
    },
    /// Focus-related commands.
    Focus {
        #[facet(args::subcommand)]
        command: FocusCommand,
    },
    /// Selection commands.
    Selection {
        #[facet(args::subcommand)]
        command: SelectionCommand,
    },
    /// Session commands.
    Session {
        #[facet(args::subcommand)]
        command: SessionCommand,
    },
    /// Project commands.
    Project {
        #[facet(args::subcommand)]
        command: ProjectCommand,
    },
    /// Input commands.
    Input {
        #[facet(args::subcommand)]
        command: InputCommand,
    },
    /// Status commands.
    Status {
        #[facet(args::subcommand)]
        command: StatusCommand,
    },
    /// Settings commands.
    Settings {
        #[facet(args::subcommand)]
        command: SettingsCommand,
    },
}

#[derive(Facet)]
#[repr(u8)]
enum AppCommand {
    /// Quit the running term instance.
    Quit,
}

#[derive(Facet)]
#[repr(u8)]
enum FocusCommand {
    /// Toggle focus between sidebar and terminal.
    Toggle,
    /// Focus the sidebar.
    Sidebar,
    /// Focus the terminal.
    Terminal,
    /// Focus the terminal with lock enabled.
    #[facet(rename = "terminal-lock")]
    TerminalLock,
}

#[derive(Facet)]
#[repr(u8)]
enum SelectionCommand {
    /// Move the selection.
    Move {
        #[facet(args::subcommand)]
        direction: Direction,
    },
    /// Reorder the selected item.
    Reorder {
        #[facet(args::subcommand)]
        direction: Direction,
    },
    /// Activate a project, worktree, or session by id.
    Activate {
        /// Project id (overrides TERM_PROJECT_ID).
        #[facet(args::named)]
        project: Option<u64>,
        /// Worktree id (overrides TERM_WORKTREE_ID).
        #[facet(args::named)]
        worktree: Option<u64>,
        /// Session id (overrides TERM_SESSION_ID).
        #[facet(args::named)]
        session: Option<u64>,
    },
    /// Delete the selected item.
    Delete,
    /// Show the current selection.
    Get,
}

#[derive(Facet)]
#[repr(u8)]
enum Direction {
    Up,
    Down,
}

#[derive(Facet)]
#[repr(u8)]
enum SessionCommand {
    /// Switch to the next session.
    Next,
    /// Switch to the previous session.
    Prev,
    /// Select a session by index.
    Index {
        /// Index of the session in the active project.
        #[facet(args::named)]
        index: usize,
    },
    /// Create a new session.
    New {
        /// Project id (overrides TERM_PROJECT_ID).
        #[facet(args::named)]
        project: Option<u64>,
        /// Worktree id (overrides TERM_WORKTREE_ID).
        #[facet(args::named)]
        worktree: Option<u64>,
        /// Session id to insert after (overrides TERM_SESSION_ID).
        #[facet(args::named, rename = "after")]
        session: Option<u64>,
        /// Binary to run for the new session.
        #[facet(args::named)]
        binary: Option<String>,
    },
    /// Add a new session.
    Add {
        /// Project id (overrides TERM_PROJECT_ID).
        #[facet(args::named)]
        project: Option<u64>,
        /// Worktree id (overrides TERM_WORKTREE_ID).
        #[facet(args::named)]
        worktree: Option<u64>,
        /// Session id to insert after (overrides TERM_SESSION_ID).
        #[facet(args::named, rename = "after")]
        session: Option<u64>,
        /// Binary to run for the new session.
        #[facet(args::named)]
        binary: Option<String>,
    },
    /// Rename the selected session.
    Rename {
        /// Project id (overrides TERM_PROJECT_ID).
        #[facet(args::named)]
        project: Option<u64>,
        /// Worktree id (overrides TERM_WORKTREE_ID).
        #[facet(args::named)]
        worktree: Option<u64>,
        /// Session id (overrides TERM_SESSION_ID).
        #[facet(args::named)]
        session: Option<u64>,
        /// New name for the session.
        #[facet(args::named)]
        name: String,
    },
    /// List sessions for a project/worktree.
    List {
        /// Project id (overrides TERM_PROJECT_ID).
        #[facet(args::named)]
        project: Option<u64>,
        /// Worktree id (overrides TERM_WORKTREE_ID).
        #[facet(args::named)]
        worktree: Option<u64>,
    },
    /// Query sessions with key=value filters (e.g. title=api has-updates=true).
    Query {
        #[facet(args::positional)]
        query: Vec<String>,
    },
    /// Show the current session.
    Get,
}

#[derive(Facet)]
#[repr(u8)]
enum ProjectCommand {
    /// Create a new project.
    New {
        /// Path for the new project.
        #[facet(args::named)]
        path: String,
        /// Name for the new project (defaults to the path basename).
        #[facet(args::named)]
        name: Option<String>,
    },
    /// List known projects.
    List,
    /// Worktree commands scoped to a project.
    Worktree {
        #[facet(args::subcommand)]
        command: ProjectWorktreeCommand,
    },
}

#[derive(Facet)]
#[repr(u8)]
enum ProjectWorktreeCommand {
    /// Create a new worktree.
    New {
        /// Project id (overrides TERM_PROJECT_ID).
        #[facet(args::named)]
        project: Option<u64>,
        /// Name for the new worktree.
        #[facet(args::named)]
        name: String,
        /// VCS kind for the new worktree (git or jj).
        #[facet(args::named)]
        vcs: Option<String>,
    },
    /// List worktrees for a project.
    List {
        /// Project id (overrides TERM_PROJECT_ID).
        #[facet(args::named)]
        project: Option<u64>,
    },
}

#[derive(Facet)]
#[repr(u8)]
enum InputCommand {
    /// Send a key to the active session.
    Key {
        /// Key code to send (e.g. "j", "enter", "esc").
        #[facet(args::named)]
        key: String,
        /// Ctrl modifier for input key.
        #[facet(args::named)]
        ctrl: bool,
        /// Alt modifier for input key.
        #[facet(args::named)]
        alt: bool,
        /// Shift modifier for input key.
        #[facet(args::named)]
        shift: bool,
        /// Super modifier for input key.
        #[facet(args::named, rename = "super")]
        super_key: bool,
    },
    /// Paste content into the active session.
    Paste {
        /// Text payload to paste.
        #[facet(args::named)]
        text: String,
    },
    /// Send input bytes to the active session.
    Send {
        /// Input payload (string).
        #[facet(args::named)]
        text: Option<String>,
        /// Input payload (comma-separated bytes, supports 0x..).
        #[facet(args::named)]
        bytes: Option<String>,
    },
}

#[derive(Facet)]
#[repr(u8)]
enum StatusCommand {
    /// Show the current status.
    Get,
}

#[derive(Facet)]
#[repr(u8)]
enum SettingsCommand {
    /// Sidebar-related settings.
    Sidebar {
        #[facet(args::subcommand)]
        command: SidebarSettingsCommand,
    },
}

#[derive(Facet)]
#[repr(u8)]
enum SidebarSettingsCommand {
    /// Configure sidebar auto-hide.
    AutoHide {
        #[facet(args::subcommand)]
        mode: ToggleMode,
    },
}

#[derive(Facet)]
#[repr(u8)]
enum ToggleMode {
    On,
    Off,
    Toggle,
}

#[derive(Clone)]
struct StartupScriptRequest {
    project_id: ProjectId,
    worktree: Option<WorktreeId>,
    workspace_path: PathBuf,
}

struct Model {
    io: Arc<dyn TermIo>,
    projects: Vec<Project>,
    tree: TreeState<TreeId>,
    tree_scroll: ScrollState,
    focus: Focus,
    auto_hide: bool,
    active: Option<SessionKey>,
    modal: Option<ModalState>,
    filter: Option<filter::FilterExpression>,
    filter_text: String,
    initial_update: bool,
    next_project_id: u64,
    next_session_id: u64,
    status: Option<StatusMessage>,
    keymap: Keymap,
    remote_socket: Option<PathBuf>,
    remote_receiver: Option<Receiver<RemoteEnvelope>>,
    strip_scroll: ScrollState,
}

#[derive(Default)]
struct SessionSync {
    changed: bool,
    bell: bool,
    notifications: Vec<TerminalNotification>,
    active_removed: bool,
}

impl Model {
    fn new_with_remote(
        io: Arc<dyn TermIo>,
        remote_socket: Option<PathBuf>,
    ) -> std::io::Result<Self> {
        Self::new_with_io(io, remote_socket)
    }

    fn new_with_io(io: Arc<dyn TermIo>, remote_socket: Option<PathBuf>) -> std::io::Result<Self> {
        let mut model = Self {
            io,
            projects: Vec::new(),
            tree: TreeState::new(),
            tree_scroll: ScrollState::vertical(),
            focus: Focus::Sidebar,
            auto_hide: false,
            active: None,
            modal: None,
            filter: None,
            filter_text: String::new(),
            initial_update: true,
            next_project_id: 1,
            next_session_id: 1,
            status: None,
            keymap: Keymap::default(),
            remote_socket,
            remote_receiver: None,
            strip_scroll: ScrollState::horizontal(),
        };

        let persisted = match model.io.load_projects() {
            Ok(data) => data,
            Err(err) => {
                warn!(?err, "failed to load saved projects");
                Vec::new()
            }
        };

        if persisted.is_empty() {
            let cwd = model.io.current_dir()?;
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
                let cwd = model.io.current_dir()?;
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

    fn set_remote_receiver(&mut self, receiver: Receiver<RemoteEnvelope>) {
        self.remote_receiver = Some(receiver);
    }

    fn rebuild_tree(&mut self) {
        rebuild_tree(
            &self.projects,
            &mut self.tree,
            &mut self.active,
            self.filter.as_ref(),
        );
    }

    fn add_project(&mut self, path: PathBuf, name: String) -> std::io::Result<()> {
        let path = match self.io.canonicalize_dir(&path) {
            Ok(p) => p,
            Err(_) => path,
        };
        if !self.io.is_dir(&path) {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("{} is not a directory", path.display()),
            ));
        }

        let project_id = ProjectId(self.next_project_id);
        self.next_project_id += 1;

        let mut project = Project::new(project_id, name, path);
        project.worktrees_loaded = !self.io.load_worktrees();

        self.projects.push(project);
        self.rebuild_tree();
        self.focus = Focus::Sidebar;

        if let Err(err) = self.io.save_projects(&self.projects) {
            warn!(?err, "failed to save projects");
            self.status = Some(StatusMessage::error(format!(
                "err saving projects: {}",
                err
            )));
        }
        Ok(())
    }

    fn spawn_session(
        &mut self,
        path: &std::path::Path,
        number: usize,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
        binary: Option<&str>,
    ) -> std::io::Result<Session> {
        let id = SessionId(self.next_session_id);
        self.next_session_id += 1;
        let env = self.session_env(project_id, worktree, id);
        Session::spawn(self.io.clone(), path, number, id, env, binary)
    }

    fn session_env(
        &self,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
        session_id: SessionId,
    ) -> HashMap<String, String> {
        let Some(socket) = &self.remote_socket else {
            return HashMap::new();
        };
        remote_env_map(socket, project_id, worktree, session_id)
    }

    fn startup_env(
        &self,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
    ) -> HashMap<String, String> {
        let Some(socket) = &self.remote_socket else {
            return HashMap::new();
        };
        remote_container_env_map(socket, project_id, worktree)
    }

    fn build_startup_script_transition(
        &self,
        request: StartupScriptRequest,
    ) -> Option<Transition<Msg>> {
        let script_path = self.io.resolve_startup_script(&request.workspace_path)?;
        let env = self.startup_env(request.project_id, request.worktree);
        let project_id = request.project_id;
        let worktree = request.worktree;
        Some(startup_script_transition(
            self.io.clone(),
            StartupScript {
                script_path,
                workspace_path: request.workspace_path,
                env,
            },
            project_id,
            worktree,
        ))
    }

    fn container_startup_state_mut(
        &mut self,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
    ) -> Option<&mut StartupState> {
        let project = self.project_mut(project_id)?;
        match worktree {
            None => Some(&mut project.startup_state),
            Some(wid) => project.worktree_mut(wid).map(|wt| &mut wt.startup_state),
        }
    }

    fn container_startup_state(
        &self,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
    ) -> Option<StartupState> {
        let project = self.project(project_id)?;
        match worktree {
            None => Some(project.startup_state),
            Some(wid) => project.worktree(wid).map(|wt| wt.startup_state),
        }
    }

    fn container_path(
        &self,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
    ) -> Option<PathBuf> {
        let project = self.project(project_id)?;
        match worktree {
            None => Some(project.path.clone()),
            Some(wid) => project.worktree(wid).map(|wt| wt.path.clone()),
        }
    }

    fn container_has_sessions(&self, project_id: ProjectId, worktree: Option<WorktreeId>) -> bool {
        let project = match self.project(project_id) {
            Some(project) => project,
            None => return false,
        };
        match worktree {
            None => !project.sessions.is_empty(),
            Some(wid) => project
                .worktree(wid)
                .map(|wt| !wt.sessions.is_empty())
                .unwrap_or(false),
        }
    }

    fn activate_container(
        &mut self,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
        transitions: &mut Vec<Transition<Msg>>,
    ) {
        if self.container_startup_state(project_id, worktree) != Some(StartupState::Inactive) {
            return;
        }
        let Some(workspace_path) = self.container_path(project_id, worktree) else {
            return;
        };

        let request = StartupScriptRequest {
            project_id,
            worktree,
            workspace_path,
        };
        if let Some(transition) = self.build_startup_script_transition(request) {
            if let Some(startup_state) = self.container_startup_state_mut(project_id, worktree) {
                *startup_state = StartupState::Loading;
            }
            transitions.push(transition);
        } else {
            if let Some(startup_state) = self.container_startup_state_mut(project_id, worktree) {
                *startup_state = StartupState::Active;
            }
            if !self.container_has_sessions(project_id, worktree) {
                create_session(
                    self,
                    project_id,
                    worktree,
                    InsertPosition::Top,
                    None,
                    false,
                    transitions,
                );
            }
        }
        self.rebuild_tree();
    }

    fn finish_startup_script(
        &mut self,
        project_id: ProjectId,
        worktree: Option<WorktreeId>,
        transitions: &mut Vec<Transition<Msg>>,
    ) {
        if self.container_startup_state(project_id, worktree) == Some(StartupState::Loading)
            && let Some(startup_state) = self.container_startup_state_mut(project_id, worktree)
        {
            *startup_state = StartupState::Active;
        }
        if !self.container_has_sessions(project_id, worktree) {
            create_session(
                self,
                project_id,
                worktree,
                InsertPosition::Top,
                None,
                false,
                transitions,
            );
        }
        self.rebuild_tree();
    }

    fn start_worktree_load(&mut self, project_id: ProjectId) -> Option<Transition<Msg>> {
        if !self.io.load_worktrees() {
            return None;
        }
        let project = self.project_mut(project_id)?;
        if project.worktrees_loaded || project.worktrees_loading {
            return None;
        }
        project.worktrees_loading = true;
        let path = project.path.clone();
        let io = self.io.clone();
        Some(Transition::Task(Box::pin(async move {
            let vcs = io.detect_vcs(path.clone()).await;
            let result = io.list_worktrees(path, vcs).await;
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
            self.projects[project_index].add_worktree(worktree.name, worktree.path);
        }

        self.rebuild_tree();
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
        let path = match self.io.add_worktree(&repo_path, &name, vcs) {
            Ok(path) => path,
            Err(err) => {
                self.status = Some(StatusMessage::error(format!(
                    "Failed to add worktree: {err}"
                )));
                return None;
            }
        };
        copy_optional_files(self.io.as_ref(), &repo_path, &path, &mut self.status);

        let wid = self.projects[project_index].add_worktree(name, path);
        self.rebuild_tree();
        self.set_focus(Focus::Sidebar);
        Some(TreeId::Worktree(project_id, wid))
    }

    fn active_session(&self) -> Option<(&Project, &Session)> {
        let key = self.active?;
        let project = self.projects.iter().find(|p| p.id == key.project)?;
        let session = project.session(key.worktree, key.session)?;
        Some((project, session))
    }

    fn active_container_sessions(
        &self,
    ) -> Option<(ProjectId, Option<WorktreeId>, SessionId, &[Session])> {
        let key = self.active?;
        let project = self.projects.iter().find(|p| p.id == key.project)?;
        project.session(key.worktree, key.session)?;
        let sessions = match key.worktree {
            Some(wid) => project.worktree(wid)?.sessions.as_slice(),
            None => project.sessions.as_slice(),
        };
        Some((project.id, key.worktree, key.session, sessions))
    }

    fn active_layout(&self) -> Layout {
        let Some(key) = self.active else {
            return Layout::Tall;
        };
        let Some(project) = self.projects.iter().find(|p| p.id == key.project) else {
            return Layout::Tall;
        };
        match key.worktree {
            Some(wid) => project
                .worktree(wid)
                .map(|worktree| worktree.layout)
                .unwrap_or(Layout::Tall),
            None => project.layout,
        }
    }

    fn toggle_active_layout(&mut self) {
        let Some(key) = self.active else {
            return;
        };
        let Some(project) = self.project_mut(key.project) else {
            return;
        };
        match key.worktree {
            Some(wid) => {
                if let Some(worktree) = project.worktree_mut(wid) {
                    worktree.layout = worktree.layout.toggle();
                }
            }
            None => {
                project.layout = project.layout.toggle();
            }
        }
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

        if self.active_layout() == Layout::Strip {
            self.strip_scroll.ensure_visible(
                "terminal-strip",
                ScrollTarget::with_mixin("terminal", key.session.0),
            );
        }

        if cleared_bell || cleared_unread {
            self.rebuild_tree();
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

            if self.active_layout() == Layout::Strip {
                self.strip_scroll.ensure_visible(
                    "terminal-strip",
                    ScrollTarget::with_mixin("terminal", sid.0),
                );
            }

            if cleared_bell || cleared_unread {
                self.rebuild_tree();
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

    fn sync_session_state(&mut self, id: &TreeId) -> SessionSync {
        let TreeId::Session(pid, worktree, sid) = id else {
            return SessionSync::default();
        };

        let is_active = self
            .active
            .map(|active| {
                active.project == *pid && active.session == *sid && active.worktree == *worktree
            })
            .unwrap_or(false);

        let Some(project) = self.project_mut(*pid) else {
            return SessionSync::default();
        };
        let exited = {
            let Some(session) = project.session(*worktree, *sid) else {
                return SessionSync::default();
            };
            !session.terminal.is_running()
        };

        if exited {
            let sync = SessionSync {
                changed: project.remove_session(*worktree, *sid),
                active_removed: self.active.is_some_and(|active| {
                    active.project == *pid && active.worktree == *worktree && active.session == *sid
                }),
                ..Default::default()
            };
            return sync;
        }

        let Some(session) = project.session_mut(*worktree, *sid) else {
            return SessionSync::default();
        };

        let mut sync = SessionSync::default();

        let title_sync = session.sync_title();
        sync.changed |= title_sync.display_changed;

        let bell_sync = session.sync_bell(is_active);
        sync.changed |= bell_sync.changed;
        sync.bell = bell_sync.triggered;

        sync.changed |= session.sync_activity(is_active);

        let notifications = session.take_notifications();
        if !notifications.is_empty() {
            sync.notifications = notifications;
        }

        sync
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

        let project_id = self.projects[project_index].id;
        let session = self.spawn_session(&path, number, project_id, worktree, None)?;
        let tree_id = TreeId::Session(self.projects[project_index].id, worktree, session.id);

        self.projects[project_index].add_session(worktree, session);
        if let Some(state) = self.container_startup_state_mut(project_id, worktree)
            && *state == StartupState::Inactive
        {
            *state = StartupState::Active;
        }
        self.rebuild_tree();
        Ok(Some(tree_id))
    }

    fn save(&self) {
        if let Err(err) = self.io.save_projects(&self.projects) {
            warn!(?err, "failed to save projects");
        }
    }
}

#[derive(Clone, Debug)]
enum Msg {
    FocusSidebar,
    FocusTerminal(SessionKey),
    Key(Key),
    Paste(String),
    RemoteRequest(Box<RemoteEnvelope>),
    RemoteClosed,
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
    StartupScriptFinished {
        project: ProjectId,
        worktree: Option<WorktreeId>,
        ok: bool,
    },
    StripScroll(ScrollMsg),
}

fn arm_wakeup(tree_id: TreeId, receiver: Receiver<()>) -> Transition<Msg> {
    Transition::Task(Box::pin(async move {
        let _ = receiver.recv().await;
        Msg::SessionWake(tree_id)
    }))
}

fn arm_remote(receiver: Receiver<RemoteEnvelope>) -> Transition<Msg> {
    Transition::Task(Box::pin(async move {
        match receiver.recv().await {
            Ok(request) => Msg::RemoteRequest(Box::new(request)),
            Err(_) => Msg::RemoteClosed,
        }
    }))
}

fn startup_script_transition(
    io: Arc<dyn TermIo>,
    script: StartupScript,
    project: ProjectId,
    worktree: Option<WorktreeId>,
) -> Transition<Msg> {
    Transition::Task(Box::pin(async move {
        let ok = smol::unblock(move || io.run_startup_script(script)).await;
        Msg::StartupScriptFinished {
            project,
            worktree,
            ok,
        }
    }))
}

fn clear_status_on_input(model: &mut Model, msg: &Msg) {
    if matches!(
        msg,
        Msg::Key(_)
            | Msg::Paste(_)
            | Msg::RemoteRequest(_)
            | Msg::Tree(_)
            | Msg::Modal(_)
            | Msg::FocusSidebar
            | Msg::FocusTerminal(_)
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
        if let Some(receiver) = model.remote_receiver.clone() {
            transitions.push(arm_remote(receiver));
        }
    }

    match msg {
        Msg::FocusSidebar => {
            if model.focus.is_terminal()
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
        Msg::FocusTerminal(target) => {
            let was_terminal = model.focus.is_terminal();
            let previous = model.active;

            if previous != Some(target) {
                if was_terminal && let Some(active) = previous {
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
            }

            model.set_focus(Focus::Terminal);
            if !was_terminal || previous != Some(target) {
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
        Msg::RemoteRequest(envelope) => {
            let (transition, response) = handle_remote_request(model, envelope.request);
            let _ = envelope.respond_to.send(response);
            if let Some(receiver) = model.remote_receiver.clone() {
                return transition.combine(arm_remote(receiver));
            }
            return transition;
        }
        Msg::RemoteClosed => {
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
            let scopes = match model.focus {
                Focus::TerminalLocked => vec![Scope::TerminalLocked],
                Focus::Terminal => vec![Scope::Terminal, Scope::Global],
                Focus::Sidebar => vec![Scope::Sidebar, Scope::Global],
            };

            if let Some(action) = model.keymap.resolve(&scopes, key) {
                return handle_action(model, action, key, transitions);
            }

            // Fallback: when terminal focused, send to terminal
            if model.focus == Focus::Terminal
                && let Some(active) = model.active
                && let Some((_, session)) = model.active_session()
                && let Some(msg) =
                    default_terminal_keybindings(key, session.terminal.mode(), move |term_msg| {
                        Msg::Terminal {
                            project: active.project,
                            worktree: active.worktree,
                            session: active.session,
                            msg: term_msg,
                        }
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
                model.activate_container(pid, None, &mut transitions);
            }
            TreeMsg::Activate(TreeId::Worktree(pid, wid))
            | TreeMsg::DoubleClick(TreeId::Worktree(pid, wid)) => {
                model.tree.toggle_expanded(&TreeId::Worktree(pid, wid));
                model.activate_container(pid, Some(wid), &mut transitions);
            }
        },
        Msg::TreeScroll(scroll_msg) => model.tree_scroll.update(scroll_msg),
        Msg::StripScroll(scroll_msg) => model.strip_scroll.update(scroll_msg),
        Msg::Terminal {
            project,
            worktree,
            session,
            msg,
        } => {
            if let Some(proj) = model.project_mut(project)
                && let Some(sess) = proj.session_mut(worktree, session)
            {
                let cleared_bell = if matches!(msg, TerminalMsg::Input(_) | TerminalMsg::Paste(_)) {
                    sess.clear_bell()
                } else {
                    false
                };

                sess.update(msg);
                let wakeup = sess.wakeup.clone();
                transitions.push(Transition::Continue);
                transitions.push(arm_wakeup(
                    TreeId::Session(project, worktree, session),
                    wakeup,
                ));

                if cleared_bell {
                    rebuild_tree(&model.projects, &mut model.tree, &mut model.active);
                }
            }
        }
        Msg::SessionWake(id) => {
            let sync = model.sync_session_state(&id);
            if sync.changed {
                model.rebuild_tree();
            }
            if sync.active_removed
                && model.focus.is_terminal()
                && let Some(active) = model.active
            {
                transitions.push(update(
                    model,
                    Msg::Terminal {
                        project: active.project,
                        worktree: active.worktree,
                        session: active.session,
                        msg: TerminalMsg::FocusGained,
                    },
                ));
            }
            if sync.bell {
                transitions.push(Transition::Bell);
            }
            for notification in sync.notifications {
                transitions.push(Transition::Notify {
                    title: notification.title,
                    body: notification.body,
                });
            }
            if let Some(receiver) = model.wakeup_for(&id) {
                transitions.push(arm_wakeup(id, receiver));
            }
        }
        Msg::FocusGained => {
            if model.focus.is_terminal()
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
            if model.focus.is_terminal()
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
            model.apply_worktrees_loaded(project, result);
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
                            model.tree.select(TreeId::Project(project_id));
                        }
                        model.modal = None;
                    }
                    ModalResult::WorktreeSubmitted { project, name, vcs } => {
                        model.add_worktree(project, name, vcs);
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
                            model.rebuild_tree();
                        } else {
                            model.status = Some(StatusMessage::error("Session no longer exists"));
                        }
                        model.modal = None;
                    }
                    ModalResult::FilterSubmitted(filter_text) => {
                        let trimmed = filter_text.trim();
                        if trimmed.is_empty() {
                            // Empty filter shows all
                            model.filter = None;
                            model.filter_text = String::new();
                        } else {
                            match filter::parse_filter(trimmed) {
                                Ok(expr) => {
                                    model.filter = Some(expr);
                                    model.filter_text = trimmed.to_string();
                                }
                                Err(err) => {
                                    model.status = Some(StatusMessage::error(format!(
                                        "Invalid filter: {}",
                                        err
                                    )));
                                }
                            }
                        }
                        model.rebuild_tree();
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
                None,
                true,
                &mut transitions,
            );
        }
        Msg::StartupScriptFinished {
            project,
            worktree,
            ok,
        } => {
            if !ok {
                model.status = Some(StatusMessage::error("Startup script failed"));
            }
            model.finish_startup_script(project, worktree, &mut transitions);
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

fn handle_remote_request(
    model: &mut Model,
    request: RemoteRequest,
) -> (Transition<Msg>, RemoteResponse) {
    if request.version != remote::REMOTE_PROTOCOL_VERSION {
        return (
            Transition::Continue,
            RemoteResponse::err(format!("Unsupported protocol version: {}", request.version)),
        );
    }

    let action = params_action_name(&request.action);
    let params = request.params;

    match action.as_str() {
        "app-quit" => action_response(model, Action::Quit),
        "focus-toggle" => action_response(model, Action::ToggleFocus),
        "focus-sidebar" => handle_remote_set_focus(model, Focus::Sidebar),
        "focus-terminal" => handle_remote_set_focus(model, Focus::Terminal),
        "focus-terminal-lock" => handle_remote_set_focus(model, Focus::TerminalLocked),
        "selection-move-up" => action_response(model, Action::MoveSelectionUp),
        "selection-move-down" => action_response(model, Action::MoveSelectionDown),
        "selection-reorder-up" => action_response(model, Action::MoveItemUp),
        "selection-reorder-down" => action_response(model, Action::MoveItemDown),
        "selection-activate" => handle_remote_activate_target(model, &params),
        "selection-delete" => action_response(model, Action::DeleteSelected),
        "selection-get" => handle_remote_current_selection(model),
        "session-next" => action_response(model, Action::NextSession),
        "session-prev" => action_response(model, Action::PrevSession),
        "session-index" => {
            let Some(index) = params.index else {
                return (
                    Transition::Continue,
                    RemoteResponse::err("session index requires --index"),
                );
            };
            action_response(model, Action::SessionByIndex(index))
        }
        "session-new" => handle_remote_new_session(model, &params),
        "session-rename" => handle_remote_rename_session(model, &params),
        "session-list" => handle_remote_list_sessions(model, &params),
        "session-query" => handle_remote_query_sessions(model, &params),
        "session-get" => handle_remote_current_session(model),
        "project-new" => handle_remote_new_project(model, &params),
        "project-list" => handle_remote_list_projects(model),
        "project-worktree-new" => handle_remote_new_worktree(model, &params),
        "project-worktree-list" => handle_remote_list_worktrees(model, &params),
        "input-key" => handle_remote_send_key(model, &params),
        "input-paste" => handle_remote_paste(model, &params),
        "input-send" => handle_remote_send_input(model, &params),
        "settings-sidebar-auto-hide-toggle" => action_response(model, Action::ToggleAutoHide),
        "settings-sidebar-auto-hide-on" => {
            model.auto_hide = true;
            (
                Transition::Continue,
                RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
            )
        }
        "settings-sidebar-auto-hide-off" => {
            model.auto_hide = false;
            (
                Transition::Continue,
                RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
            )
        }
        "status-get" => handle_remote_status(model),
        _ => (
            Transition::Continue,
            RemoteResponse::err(format!("Unknown action: {}", request.action)),
        ),
    }
}

fn action_response(model: &mut Model, action: Action) -> (Transition<Msg>, RemoteResponse) {
    let transition = handle_action(model, action, Key::new(KeyCode::Char(' ')), Vec::new());
    (
        transition,
        RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
    )
}

fn handle_remote_set_focus(model: &mut Model, focus: Focus) -> (Transition<Msg>, RemoteResponse) {
    let previous = model.focus;
    if previous == focus {
        return (
            Transition::Continue,
            RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
        );
    }

    model.set_focus(focus);
    if previous.is_terminal() != focus.is_terminal()
        && let Some(active) = model.active
    {
        let msg = if focus.is_terminal() {
            TerminalMsg::FocusGained
        } else {
            TerminalMsg::FocusLost
        };
        let transition = update(
            model,
            Msg::Terminal {
                project: active.project,
                worktree: active.worktree,
                session: active.session,
                msg,
            },
        );
        return (
            transition,
            RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
        );
    }

    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
    )
}

fn handle_remote_activate_target(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let selection = selection_from_model(model);
    let has_project_param = params.project_id.is_some();
    let project_id =
        params_project_id(params).or_else(|| selection.map(|sel| ProjectId(sel.project_id)));
    let worktree = if has_project_param {
        params_worktree_id(params)
    } else {
        params_worktree_id(params)
            .or_else(|| selection.and_then(|sel| sel.worktree_id.map(WorktreeId)))
    };
    let session_id = if has_project_param {
        params_session_id(params)
    } else {
        params_session_id(params)
            .or_else(|| selection.and_then(|sel| sel.session_id.map(SessionId)))
    };

    let Some(project_id) = project_id else {
        return (
            Transition::Continue,
            RemoteResponse::err("selection activate requires --project"),
        );
    };

    let Some(project) = model.project(project_id) else {
        return (
            Transition::Continue,
            RemoteResponse::err("Project no longer exists"),
        );
    };

    let target = if let Some(session_id) = session_id {
        if project.session(worktree, session_id).is_none() {
            return (
                Transition::Continue,
                RemoteResponse::err("Session no longer exists"),
            );
        }
        TreeId::Session(project_id, worktree, session_id)
    } else if let Some(worktree) = worktree {
        if project.worktree(worktree).is_none() {
            return (
                Transition::Continue,
                RemoteResponse::err("Worktree no longer exists"),
            );
        }
        TreeId::Worktree(project_id, worktree)
    } else {
        TreeId::Project(project_id)
    };

    model.tree.select(target);
    action_response(model, Action::ActivateSelected)
}

fn handle_remote_new_session(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let selection = selection_from_model(model);
    let Some(target) = resolve_new_session_target(params, selection) else {
        return (
            Transition::Continue,
            RemoteResponse::err("No active selection available for session new"),
        );
    };
    let project_id = ProjectId(target.project_id);
    let worktree = target.worktree_id.map(WorktreeId);
    let insert_position = target
        .insert_after
        .map(SessionId)
        .map(InsertPosition::After)
        .unwrap_or(InsertPosition::Top);

    let mut transitions = Vec::new();
    let session_id = create_session(
        model,
        project_id,
        worktree,
        insert_position,
        params.binary.as_deref(),
        true,
        &mut transitions,
    );

    let response = if let Some(session_id) = session_id {
        let session = model.project(project_id).and_then(|project| {
            let worktree_ref = worktree.and_then(|wid| project.worktree(wid));
            project
                .session(worktree, session_id)
                .map(|session| build_remote_session(project, worktree_ref, session))
        });
        RemoteResponse::ok(Some(RemoteResult::CurrentSession { session }))
    } else {
        RemoteResponse::err("Failed to create session")
    };

    let transition = if transitions.is_empty() {
        Transition::Continue
    } else {
        Transition::Multiple(transitions)
    };
    (transition, response)
}

fn handle_remote_new_project(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let Some(path_str) = params.path.clone() else {
        return (
            Transition::Continue,
            RemoteResponse::err("project new requires --path"),
        );
    };
    let path = PathBuf::from(&path_str);
    let name = params
        .name
        .clone()
        .or_else(|| {
            path.file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
        })
        .unwrap_or_else(|| path_str.clone());

    let mut transitions = Vec::new();
    if let Err(err) = model.add_project(path, name) {
        return (Transition::Continue, RemoteResponse::err(err.to_string()));
    }
    if let Some(project_id) = model.projects.last().map(|project| project.id) {
        if let Some(transition) = model.start_worktree_load(project_id) {
            transitions.push(transition);
        }
        model.tree.select(TreeId::Project(project_id));
    }

    let response = model
        .active_session()
        .map(|(project, session)| {
            let worktree = model
                .active
                .and_then(|active| active.worktree)
                .and_then(|wid| project.worktree(wid));
            RemoteResponse::ok(Some(RemoteResult::CurrentSession {
                session: Some(build_remote_session(project, worktree, session)),
            }))
        })
        .unwrap_or_else(|| RemoteResponse::ok(Some(RemoteResult::Ok { message: None })));

    let transition = if transitions.is_empty() {
        Transition::Continue
    } else {
        Transition::Multiple(transitions)
    };
    (transition, response)
}

fn handle_remote_new_worktree(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let selection = selection_from_model(model);
    let project_id =
        params_project_id(params).or_else(|| selection.map(|sel| ProjectId(sel.project_id)));
    let Some(project_id) = project_id else {
        return (
            Transition::Continue,
            RemoteResponse::err("project worktree new requires --project or active selection"),
        );
    };
    let Some(name) = params.name.clone() else {
        return (
            Transition::Continue,
            RemoteResponse::err("project worktree new requires --name"),
        );
    };
    let vcs = params
        .vcs
        .as_deref()
        .and_then(parse_vcs_kind)
        .or_else(|| {
            model
                .project(project_id)
                .map(|project| vcs::detect(&project.path))
        })
        .unwrap_or(VcsKind::Git);

    let transitions = Vec::new();
    let Some(_tree_id) = model.add_worktree(project_id, name, vcs) else {
        return (
            Transition::Continue,
            RemoteResponse::err("Failed to create worktree"),
        );
    };

    let response = model
        .active_session()
        .map(|(project, session)| {
            let worktree = model
                .active
                .and_then(|active| active.worktree)
                .and_then(|wid| project.worktree(wid));
            RemoteResponse::ok(Some(RemoteResult::CurrentSession {
                session: Some(build_remote_session(project, worktree, session)),
            }))
        })
        .unwrap_or_else(|| RemoteResponse::ok(Some(RemoteResult::Ok { message: None })));

    let transition = if transitions.is_empty() {
        Transition::Continue
    } else {
        Transition::Multiple(transitions)
    };
    (transition, response)
}

fn handle_remote_rename_session(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let selection = selection_from_model(model);
    let target = resolve_session_target(params, selection);
    let Some((project_id, worktree, session_id)) = target else {
        return (
            Transition::Continue,
            RemoteResponse::err("session rename requires a selected session"),
        );
    };
    let Some(name) = params.name.clone() else {
        return (
            Transition::Continue,
            RemoteResponse::err("session rename requires --name"),
        );
    };
    if let Some(project) = model.project_mut(project_id)
        && let Some(session) = project.session_mut(worktree, session_id)
    {
        session.custom_title = Some(name);
        session.sync_display_name();
        model.rebuild_tree();
        return (
            Transition::Continue,
            RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
        );
    }
    (
        Transition::Continue,
        RemoteResponse::err("Session no longer exists"),
    )
}

fn handle_remote_send_key(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let Some(key_spec) = params.key.clone() else {
        return (
            Transition::Continue,
            RemoteResponse::err("input key requires --key"),
        );
    };
    let key = match parse_key_spec(&key_spec) {
        Ok(key) => key,
        Err(err) => return (Transition::Continue, RemoteResponse::err(err)),
    };
    let transition = update(model, Msg::Key(key));
    (
        transition,
        RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
    )
}

fn handle_remote_paste(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let Some(text) = params.text.clone() else {
        return (
            Transition::Continue,
            RemoteResponse::err("input paste requires --text"),
        );
    };
    let transition = update(model, Msg::Paste(text));
    (
        transition,
        RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
    )
}

fn handle_remote_send_input(
    model: &mut Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let Some(bytes) = params_input_bytes(params) else {
        return (
            Transition::Continue,
            RemoteResponse::err("input send requires --text or --bytes"),
        );
    };
    let Some(active) = model.active else {
        return (
            Transition::Continue,
            RemoteResponse::err("No active session for input send"),
        );
    };
    let msg = Msg::Terminal {
        project: active.project,
        worktree: active.worktree,
        session: active.session,
        msg: TerminalMsg::Input(bytes),
    };
    let transition = update(model, msg);
    (
        transition,
        RemoteResponse::ok(Some(RemoteResult::Ok { message: None })),
    )
}

fn handle_remote_list_projects(model: &Model) -> (Transition<Msg>, RemoteResponse) {
    let projects = model
        .projects
        .iter()
        .map(|project| RemoteProject {
            id: project.id.0,
            name: project.name.clone(),
            path: project.path.display().to_string(),
        })
        .collect::<Vec<_>>();
    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::Projects { projects })),
    )
}

fn handle_remote_list_worktrees(
    model: &Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let selection = selection_from_model(model);
    let project_id =
        params_project_id(params).or_else(|| selection.map(|sel| ProjectId(sel.project_id)));
    let Some(project_id) = project_id else {
        return (
            Transition::Continue,
            RemoteResponse::err("project worktree list requires --project or active selection"),
        );
    };
    let worktrees = model
        .project(project_id)
        .map(|project| {
            project
                .worktrees
                .iter()
                .map(|worktree| RemoteWorktree {
                    project_id: project_id.0,
                    id: worktree.id.0,
                    name: worktree.name.clone(),
                    path: worktree.path.display().to_string(),
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::Worktrees { worktrees })),
    )
}

enum SessionQuery {
    Title(String, MatchKind),
    ProjectName(String, MatchKind),
    WorkspaceName(String, MatchKind),
    Cwd(String, MatchKind),
    Id(u64),
    HasUpdates(bool),
    HasBell(bool),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MatchKind {
    Exact,
    Substring,
}

fn handle_remote_query_sessions(
    model: &Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let queries = match parse_session_queries(params) {
        Ok(queries) => queries,
        Err(err) => return (Transition::Continue, RemoteResponse::err(err)),
    };
    let mut sessions = Vec::new();
    for project in &model.projects {
        for session in &project.sessions {
            if matches_session_queries(&queries, project, None, session) {
                sessions.push(build_remote_session(project, None, session));
            }
        }
        for worktree in &project.worktrees {
            for session in &worktree.sessions {
                if matches_session_queries(&queries, project, Some(worktree), session) {
                    sessions.push(build_remote_session(project, Some(worktree), session));
                }
            }
        }
    }

    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::Sessions { sessions })),
    )
}

fn parse_session_queries(params: &RemoteParams) -> Result<Vec<SessionQuery>, String> {
    let Some(queries) = params.query.as_deref() else {
        return Ok(Vec::new());
    };
    queries
        .iter()
        .map(parse_session_query)
        .collect::<Result<Vec<_>, _>>()
}

fn parse_session_query(query: &remote::RemoteSessionQuery) -> Result<SessionQuery, String> {
    let key = normalize_query_key(&query.key);
    let value = query.value.trim();
    let match_kind = parse_match_kind(query)?;
    if value.is_empty() {
        return Err(format!("query '{key}' requires a value"));
    }
    match key.as_str() {
        "title" => Ok(SessionQuery::Title(value.to_string(), match_kind)),
        "project" | "project name" => Ok(SessionQuery::ProjectName(value.to_string(), match_kind)),
        "workspace" | "workspace name" | "worktree name" => {
            Ok(SessionQuery::WorkspaceName(value.to_string(), match_kind))
        }
        "cwd" => Ok(SessionQuery::Cwd(value.to_string(), match_kind)),
        "id" => {
            ensure_exact_operator(&key, match_kind)?;
            value
                .parse::<u64>()
                .map(SessionQuery::Id)
                .map_err(|_| format!("invalid id value '{value}'"))
        }
        "has updates" | "updates" => {
            ensure_exact_operator(&key, match_kind)?;
            parse_query_bool(value)
                .map(SessionQuery::HasUpdates)
                .ok_or_else(|| format!("invalid has updates value '{value}'"))
        }
        "has bell" | "bell" => {
            ensure_exact_operator(&key, match_kind)?;
            parse_query_bool(value)
                .map(SessionQuery::HasBell)
                .ok_or_else(|| format!("invalid has bell value '{value}'"))
        }
        _ => Err(format!("unknown query key '{key}'")),
    }
}

fn parse_match_kind(query: &remote::RemoteSessionQuery) -> Result<MatchKind, String> {
    match query.operator.as_str() {
        "=" => Ok(MatchKind::Exact),
        "~=" => Ok(MatchKind::Substring),
        other => Err(format!("unknown query operator '{other}'")),
    }
}

fn ensure_exact_operator(key: &str, match_kind: MatchKind) -> Result<(), String> {
    if match_kind == MatchKind::Exact {
        Ok(())
    } else {
        Err(format!("query '{key}' requires '=' operator"))
    }
}

fn normalize_query_key(key: &str) -> String {
    let mut normalized = String::new();
    for part in key
        .split(|ch: char| ch.is_ascii_whitespace() || ch == '_' || ch == '-')
        .filter(|part| !part.is_empty())
    {
        if !normalized.is_empty() {
            normalized.push(' ');
        }
        normalized.push_str(&part.to_ascii_lowercase());
    }
    normalized
}

fn parse_query_bool(value: &str) -> Option<bool> {
    match value.trim().to_ascii_lowercase().as_str() {
        "true" | "t" | "1" | "yes" | "y" | "on" => Some(true),
        "false" | "f" | "0" | "no" | "n" | "off" => Some(false),
        _ => None,
    }
}

fn matches_session_queries(
    queries: &[SessionQuery],
    project: &Project,
    worktree: Option<&Worktree>,
    session: &Session,
) -> bool {
    queries
        .iter()
        .all(|query| matches_session_query(query, project, worktree, session))
}

fn matches_session_query(
    query: &SessionQuery,
    project: &Project,
    worktree: Option<&Worktree>,
    session: &Session,
) -> bool {
    match query {
        SessionQuery::Title(needle, match_kind) => {
            matches_query_text(&session_title_for_query(session), needle, *match_kind)
        }
        SessionQuery::ProjectName(needle, match_kind) => {
            matches_query_text(&project.name, needle, *match_kind)
        }
        SessionQuery::WorkspaceName(needle, match_kind) => worktree
            .map(|worktree| matches_query_text(&worktree.name, needle, *match_kind))
            .unwrap_or(false),
        SessionQuery::Cwd(needle, match_kind) => {
            let path = match worktree {
                Some(worktree) => worktree.path.display().to_string(),
                None => project.path.display().to_string(),
            };
            matches_query_text(&path, needle, *match_kind)
        }
        SessionQuery::Id(id) => session.id.0 == *id,
        SessionQuery::HasUpdates(expected) => session.has_unread_output == *expected,
        SessionQuery::HasBell(expected) => session.bell == *expected,
    }
}

fn session_title_for_query(session: &Session) -> String {
    session
        .custom_title
        .clone()
        .or_else(|| session.title.clone())
        .unwrap_or_else(|| session.display_name())
}

fn matches_query_text(value: &str, query: &str, match_kind: MatchKind) -> bool {
    let value = value.to_ascii_lowercase();
    let query = query.trim().to_ascii_lowercase();
    match match_kind {
        MatchKind::Exact => value == query,
        MatchKind::Substring => value.contains(&query),
    }
}

fn handle_remote_list_sessions(
    model: &Model,
    params: &RemoteParams,
) -> (Transition<Msg>, RemoteResponse) {
    let selection = selection_from_model(model);
    let project_id =
        params_project_id(params).or_else(|| selection.map(|sel| ProjectId(sel.project_id)));
    let Some(project_id) = project_id else {
        return (
            Transition::Continue,
            RemoteResponse::err("session list requires --project or active selection"),
        );
    };
    let worktree = params_worktree_id(params)
        .or_else(|| selection.and_then(|sel| sel.worktree_id.map(WorktreeId)));

    let sessions = model
        .project(project_id)
        .map(|project| {
            let worktree_ref = worktree.and_then(|wid| project.worktree(wid));
            let list = match worktree_ref {
                Some(worktree) => worktree.sessions.as_slice(),
                None => project.sessions.as_slice(),
            };
            list.iter()
                .map(|session| build_remote_session(project, worktree_ref, session))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::Sessions { sessions })),
    )
}

fn handle_remote_current_session(model: &Model) -> (Transition<Msg>, RemoteResponse) {
    let session = model.active_session().map(|(project, session)| {
        let worktree = model
            .active
            .and_then(|active| active.worktree)
            .and_then(|wid| project.worktree(wid));
        build_remote_session(project, worktree, session)
    });
    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::CurrentSession { session })),
    )
}

fn handle_remote_current_selection(model: &Model) -> (Transition<Msg>, RemoteResponse) {
    let selection = selection_from_model(model);
    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::CurrentSelection { selection })),
    )
}

fn handle_remote_status(model: &Model) -> (Transition<Msg>, RemoteResponse) {
    let status = RemoteStatus {
        focus: match model.focus {
            Focus::Sidebar => "sidebar".to_string(),
            Focus::Terminal => "terminal".to_string(),
            Focus::TerminalLocked => "terminal-lock".to_string(),
        },
        auto_hide: model.auto_hide,
    };
    (
        Transition::Continue,
        RemoteResponse::ok(Some(RemoteResult::Status { status })),
    )
}

fn selection_from_model(model: &Model) -> Option<remote::RemoteSelection> {
    let selection = model.tree.selected().cloned().or_else(|| {
        model
            .active
            .map(|active| TreeId::Session(active.project, active.worktree, active.session))
    })?;
    match selection {
        TreeId::Project(pid) => Some(project_selection(pid)),
        TreeId::Worktree(pid, wid) => Some(worktree_selection(pid, wid)),
        TreeId::Session(pid, worktree, sid) => Some(session_selection(pid, worktree, sid)),
    }
}

fn resolve_session_target(
    params: &RemoteParams,
    selection: Option<remote::RemoteSelection>,
) -> Option<(ProjectId, Option<WorktreeId>, SessionId)> {
    let session_id = params_session_id(params)
        .or_else(|| selection.and_then(|sel| sel.session_id.map(SessionId)))?;
    let project_id =
        params_project_id(params).or_else(|| selection.map(|sel| ProjectId(sel.project_id)))?;
    let worktree = params_worktree_id(params)
        .or_else(|| selection.and_then(|sel| sel.worktree_id.map(WorktreeId)));
    Some((project_id, worktree, session_id))
}

fn build_remote_session(
    project: &Project,
    worktree: Option<&Worktree>,
    session: &Session,
) -> RemoteSession {
    let cwd = worktree
        .map(|worktree| worktree.path.display().to_string())
        .unwrap_or_else(|| project.path.display().to_string());
    RemoteSession {
        project_id: project.id.0,
        project_name: project.name.clone(),
        worktree_id: worktree.map(|worktree| worktree.id.0),
        worktree_name: worktree.map(|worktree| worktree.name.clone()),
        session_id: session.id.0,
        number: session.number,
        title: session.title.clone(),
        custom_title: session.custom_title.clone(),
        display_name: session.display_name(),
        exited: session.exited,
        bell: session.bell,
        has_unread_output: session.has_unread_output,
        cwd,
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
                let msg = if new_focus.is_terminal() {
                    TerminalMsg::FocusGained
                } else {
                    TerminalMsg::FocusLost
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
        Action::ToggleLayout => {
            model.toggle_active_layout();
            Transition::Continue
        }
        Action::FocusTerminal => {
            let was_terminal = model.focus.is_terminal();
            if !was_terminal && let Some(active) = model.active {
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
                    model.rebuild_tree();
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
                    model.rebuild_tree();
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
                        model.activate_container(pid, None, &mut transitions);
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
                        if transitions.is_empty() {
                            Transition::Continue
                        } else {
                            Transition::Multiple(transitions)
                        }
                    }
                    TreeId::Worktree(pid, wid) => {
                        model.tree.toggle_expanded(&selected);
                        model.activate_container(pid, Some(wid), &mut transitions);
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
                        if transitions.is_empty() {
                            Transition::Continue
                        } else {
                            Transition::Multiple(transitions)
                        }
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
                    if model.focus.is_terminal() {
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
                if model.focus.is_terminal() {
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
            let new_focus = match model.focus {
                Focus::TerminalLocked => Focus::Terminal,
                _ => Focus::TerminalLocked,
            };
            model.set_focus(new_focus);
            Transition::Continue
        }
        Action::StripScrollLeft => {
            if model.active_layout() == Layout::Strip {
                model.strip_scroll.update(ScrollMsg::AxisDeltaPercent {
                    axis: ScrollAxis::Horizontal,
                    ratio: -0.5,
                });
            }
            Transition::Continue
        }
        Action::StripScrollRight => {
            if model.active_layout() == Layout::Strip {
                model.strip_scroll.update(ScrollMsg::AxisDeltaPercent {
                    axis: ScrollAxis::Horizontal,
                    ratio: 0.5,
                });
            }
            Transition::Continue
        }
        Action::ToggleFilter => {
            model.modal = Some(ModalState::SessionFilter {
                input: InputState::with_value(model.filter_text.clone()),
                scroll: ScrollState::horizontal(),
            });
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
    binary: Option<&str>,
    focus_terminal: bool,
    transitions: &mut Vec<Transition<Msg>>,
) -> Option<SessionId> {
    let project_index = model.projects.iter().position(|p| p.id == project_id)?;

    let (path, number) = {
        let project = &model.projects[project_index];
        match worktree {
            None => (project.path.clone(), project.next_session_number),
            Some(wid) => {
                let worktree = project.worktree(wid)?;
                (worktree.path.clone(), worktree.next_session_number)
            }
        }
    };
    let session = match model.spawn_session(&path, number, project_id, worktree, binary) {
        Ok(session) => session,
        Err(err) => {
            warn!(?err, "failed to create new session");
            model.status = Some(StatusMessage::error(format!(
                "Failed to create session: {err}"
            )));
            return None;
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

    if let Some(state) = model.container_startup_state_mut(project_id, worktree)
        && *state == StartupState::Inactive
    {
        *state = StartupState::Active;
    }

    model.rebuild_tree();
    model.select_session(SessionKey {
        project: project_id,
        worktree,
        session: sid,
    });
    if focus_terminal {
        model.set_focus(Focus::Terminal);
    }

    if let Some(receiver) = model.wakeup_for(&TreeId::Session(project_id, worktree, sid)) {
        transitions.push(arm_wakeup(
            TreeId::Session(project_id, worktree, sid),
            receiver,
        ));
    }
    Some(sid)
}

fn copy_optional_files(
    io: &dyn TermIo,
    from_dir: &std::path::Path,
    to_dir: &std::path::Path,
    status: &mut Option<StatusMessage>,
) {
    for name in [".env", "config.yml"] {
        let source = from_dir.join(name);
        if !io.path_exists(&source) {
            continue;
        }
        let target = to_dir.join(name);
        if let Err(err) = io.copy_optional_file(&source, &target) {
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
            model.rebuild_tree();
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
            let vcs = model.io.detect_vcs_sync(&repo_path);
            if let Err(err) = model.io.remove_worktree(&repo_path, &worktree_path, vcs) {
                model.status = Some(StatusMessage::error(format!(
                    "Failed to remove worktree: {err}"
                )));
                return;
            }
            model.projects[project_idx].remove_worktree(wid);
            model.rebuild_tree();
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
                model.rebuild_tree();
                model.set_focus(Focus::Sidebar);
            }
        }
    }
}

fn terminal_pane(model: &Model) -> Node<Msg> {
    match model.active_layout() {
        Layout::Zoom => terminal_pane_zoom(model),
        Layout::Tall => terminal_pane_tall(model),
        Layout::Wide => terminal_pane_wide(model),
        Layout::Strip => terminal_pane_strip(model),
    }
}

fn terminal_pane_placeholder(model: &Model) -> Node<Msg> {
    block_with_title(
        "Terminal",
        vec![text::<Msg>("Select or create a session to start.")],
    )
    .with_flex_grow(1.0)
    .with_style(section_style(model.focus.is_terminal()))
}

fn terminal_pane_zoom(model: &Model) -> Node<Msg> {
    if let Some((project, session)) = model.active_session() {
        let worktree = model.active.map(|active| active.worktree).unwrap_or(None);
        let pid = project.id;
        let sid = session.id;
        let is_focused = model.focus.is_terminal();
        terminal("terminal", &session.terminal, is_focused, move |msg| {
            Msg::Terminal {
                project: pid,
                worktree,
                session: sid,
                msg,
            }
        })
        .with_id_mixin("terminal", sid.0)
        .with_flex_grow(1.0)
        .with_style(section_style(is_focused))
        .on_click(move || {
            Msg::FocusTerminal(SessionKey {
                project: pid,
                worktree,
                session: sid,
            })
        })
    } else {
        terminal_pane_placeholder(model)
    }
}

fn divider_style(_model: &Model) -> Style {
    let mut style = section_style(false);
    style.border = false;
    style
}

fn terminal_pane_tall(model: &Model) -> Node<Msg> {
    let Some((project_id, worktree, active_session, sessions)) = model.active_container_sessions()
    else {
        return terminal_pane_placeholder(model);
    };

    let mut iter = sessions.iter();
    let Some(first) = iter.next() else {
        return terminal_pane_placeholder(model);
    };

    let terminal_node = |session: &Session| {
        let sid = session.id;
        let is_active = sid == active_session;
        let is_focused = model.focus.is_terminal() && is_active;
        terminal("terminal", &session.terminal, is_focused, move |msg| {
            Msg::Terminal {
                project: project_id,
                worktree,
                session: sid,
                msg,
            }
        })
        .with_id_mixin("terminal", sid.0)
        .with_flex_grow(1.0)
        .with_style(section_style(is_focused))
        .on_click(move || {
            Msg::FocusTerminal(SessionKey {
                project: project_id,
                worktree,
                session: sid,
            })
        })
    };

    let left = terminal_node(first);
    let divider_style = divider_style(model);
    let mut right_children = Vec::new();
    for session in iter {
        if !right_children.is_empty() {
            right_children.push(horizontal_divider(divider_style));
        }
        right_children.push(terminal_node(session));
    }
    if right_children.is_empty() {
        return left.with_flex_grow(1.0);
    }

    let right = column(right_children).with_flex_grow(1.0);
    row(vec![
        left.with_flex_grow(1.0),
        vertical_divider(divider_style),
        right,
    ])
    .with_flex_grow(1.0)
}

fn terminal_pane_wide(model: &Model) -> Node<Msg> {
    let Some((project_id, worktree, active_session, sessions)) = model.active_container_sessions()
    else {
        return terminal_pane_placeholder(model);
    };

    let mut iter = sessions.iter();
    let Some(first) = iter.next() else {
        return terminal_pane_placeholder(model);
    };

    let terminal_node = |session: &Session| {
        let sid = session.id;
        let is_active = sid == active_session;
        let is_focused = model.focus.is_terminal() && is_active;
        terminal("terminal", &session.terminal, is_focused, move |msg| {
            Msg::Terminal {
                project: project_id,
                worktree,
                session: sid,
                msg,
            }
        })
        .with_id_mixin("terminal", sid.0)
        .with_flex_grow(1.0)
        .with_style(section_style(is_focused))
        .on_click(move || {
            Msg::FocusTerminal(SessionKey {
                project: project_id,
                worktree,
                session: sid,
            })
        })
    };

    let top = terminal_node(first);
    let divider_style = divider_style(model);
    let mut bottom_children = Vec::new();
    for session in iter {
        if !bottom_children.is_empty() {
            bottom_children.push(vertical_divider(divider_style));
        }
        bottom_children.push(terminal_node(session));
    }
    if bottom_children.is_empty() {
        return top.with_flex_grow(1.0);
    }

    let bottom = row(bottom_children).with_flex_grow(1.0);
    column(vec![
        top.with_flex_grow(1.0),
        horizontal_divider(divider_style),
        bottom,
    ])
    .with_flex_grow(1.0)
}

fn terminal_pane_strip(model: &Model) -> Node<Msg> {
    let Some((project_id, worktree, active_session, sessions)) = model.active_container_sessions()
    else {
        return terminal_pane_placeholder(model);
    };

    let mut iter = sessions.iter();
    let Some(first) = iter.next() else {
        return terminal_pane_placeholder(model);
    };

    let tile_width = Dimension::percent(STRIP_TILE_FRACTION);

    let terminal_node = |session: &Session| {
        let sid = session.id;
        let is_active = sid == active_session;
        let is_focused = model.focus.is_terminal() && is_active;
        terminal("terminal", &session.terminal, is_focused, move |msg| {
            Msg::Terminal {
                project: project_id,
                worktree,
                session: sid,
                msg,
            }
        })
        .with_id_mixin("terminal", sid.0)
        .with_width(tile_width)
        .with_flex_grow(0.0)
        .with_flex_shrink(0.0)
        .with_style(section_style(is_focused))
        .on_click(move || {
            Msg::FocusTerminal(SessionKey {
                project: project_id,
                worktree,
                session: sid,
            })
        })
    };

    let divider_style = divider_style(model);
    let mut children = Vec::new();
    children.push(terminal_node(first));
    for session in iter {
        children.push(
            vertical_divider(divider_style)
                .with_flex_grow(0.0)
                .with_flex_shrink(0.0),
        );
        children.push(terminal_node(session));
    }

    let content = row(children)
        .with_flex_grow(1.0)
        .with_flex_wrap(FlexWrap::NoWrap);

    scrollable_content(
        "terminal-strip",
        &model.strip_scroll,
        3,
        Msg::StripScroll,
        content,
    )
    .with_flex_grow(1.0)
}

fn view(model: &Model) -> Node<Msg> {
    let sidebar = sidebar_view(
        &model.tree,
        &model.tree_scroll,
        model.auto_hide,
        model.focus == Focus::Sidebar,
        model.filter.is_some(),
        Msg::Tree,
        || Msg::FocusSidebar,
    );

    let content = row(vec![sidebar, terminal_pane(model)]).with_flex_grow(1.0);

    let status = status_bar_view(
        model.focus,
        model.status.as_ref(),
        model.auto_hide,
        model.active_session(),
        model.active_layout(),
        &model.keymap,
    );

    let mut nodes = vec![content, status.with_max_width(Dimension::percent(1.))];

    if let Some(modal_state) = &model.modal {
        nodes.push(modal_view(modal_state, Msg::Modal));
    }

    column(nodes).with_fill()
}

fn remote_action_name(action: &RemoteCommand) -> &'static str {
    match action {
        RemoteCommand::App {
            command: AppCommand::Quit,
        } => "app-quit",
        RemoteCommand::Focus { command } => match command {
            FocusCommand::Toggle => "focus-toggle",
            FocusCommand::Sidebar => "focus-sidebar",
            FocusCommand::Terminal => "focus-terminal",
            FocusCommand::TerminalLock => "focus-terminal-lock",
        },
        RemoteCommand::Selection { command } => match command {
            SelectionCommand::Move { direction } => match direction {
                Direction::Up => "selection-move-up",
                Direction::Down => "selection-move-down",
            },
            SelectionCommand::Reorder { direction } => match direction {
                Direction::Up => "selection-reorder-up",
                Direction::Down => "selection-reorder-down",
            },
            SelectionCommand::Activate { .. } => "selection-activate",
            SelectionCommand::Delete => "selection-delete",
            SelectionCommand::Get => "selection-get",
        },
        RemoteCommand::Session { command } => match command {
            SessionCommand::Next => "session-next",
            SessionCommand::Prev => "session-prev",
            SessionCommand::Index { .. } => "session-index",
            SessionCommand::New { .. } => "session-new",
            SessionCommand::Add { .. } => "session-new",
            SessionCommand::Rename { .. } => "session-rename",
            SessionCommand::List { .. } => "session-list",
            SessionCommand::Query { .. } => "session-query",
            SessionCommand::Get => "session-get",
        },
        RemoteCommand::Project { command } => match command {
            ProjectCommand::New { .. } => "project-new",
            ProjectCommand::List => "project-list",
            ProjectCommand::Worktree { command } => match command {
                ProjectWorktreeCommand::New { .. } => "project-worktree-new",
                ProjectWorktreeCommand::List { .. } => "project-worktree-list",
            },
        },
        RemoteCommand::Input { command } => match command {
            InputCommand::Key { .. } => "input-key",
            InputCommand::Paste { .. } => "input-paste",
            InputCommand::Send { .. } => "input-send",
        },
        RemoteCommand::Status {
            command: StatusCommand::Get,
        } => "status-get",
        RemoteCommand::Settings { command } => match command {
            SettingsCommand::Sidebar { command } => match command {
                SidebarSettingsCommand::AutoHide { mode } => match mode {
                    ToggleMode::On => "settings-sidebar-auto-hide-on",
                    ToggleMode::Off => "settings-sidebar-auto-hide-off",
                    ToggleMode::Toggle => "settings-sidebar-auto-hide-toggle",
                },
            },
        },
    }
}

fn build_remote_client_args(remote: RemoteArgs) -> RemoteClientArgs {
    let mut args = RemoteClientArgs {
        action: remote_action_name(&remote.action).to_string(),
        json: remote.json,
        text: remote.text,
        socket: remote.socket,
        project: None,
        worktree: None,
        session: None,
        index: None,
        name: None,
        path: None,
        vcs: None,
        binary: None,
        key: None,
        ctrl: false,
        alt: false,
        shift: false,
        super_key: false,
        content: None,
        input: None,
        bytes: None,
        query: Vec::new(),
    };

    match remote.action {
        RemoteCommand::Selection {
            command:
                SelectionCommand::Activate {
                    project,
                    worktree,
                    session,
                },
        } => {
            args.project = project;
            args.worktree = worktree;
            args.session = session;
        }
        RemoteCommand::Session {
            command: SessionCommand::Index { index },
        } => {
            args.index = Some(index);
        }
        RemoteCommand::Session {
            command:
                SessionCommand::New {
                    project,
                    worktree,
                    session,
                    binary,
                },
        } => {
            args.project = project;
            args.worktree = worktree;
            args.session = session;
            args.binary = binary;
        }
        RemoteCommand::Session {
            command:
                SessionCommand::Add {
                    project,
                    worktree,
                    session,
                    binary,
                },
        } => {
            args.project = project;
            args.worktree = worktree;
            args.session = session;
            args.binary = binary;
        }
        RemoteCommand::Project {
            command: ProjectCommand::New { path, name },
        } => {
            args.path = Some(path);
            args.name = name;
        }
        RemoteCommand::Project {
            command:
                ProjectCommand::Worktree {
                    command: ProjectWorktreeCommand::New { project, name, vcs },
                },
        } => {
            args.project = project;
            args.name = Some(name);
            args.vcs = vcs;
        }
        RemoteCommand::Session {
            command:
                SessionCommand::Rename {
                    project,
                    worktree,
                    session,
                    name,
                },
        } => {
            args.project = project;
            args.worktree = worktree;
            args.session = session;
            args.name = Some(name);
        }
        RemoteCommand::Input {
            command:
                InputCommand::Key {
                    key,
                    ctrl,
                    alt,
                    shift,
                    super_key,
                },
        } => {
            args.key = Some(key);
            args.ctrl = ctrl;
            args.alt = alt;
            args.shift = shift;
            args.super_key = super_key;
        }
        RemoteCommand::Input {
            command: InputCommand::Paste { text },
        } => {
            args.content = Some(text);
        }
        RemoteCommand::Input {
            command: InputCommand::Send { text, bytes },
        } => {
            args.input = text;
            args.bytes = bytes;
        }
        RemoteCommand::Project {
            command:
                ProjectCommand::Worktree {
                    command: ProjectWorktreeCommand::List { project },
                },
        } => {
            args.project = project;
        }
        RemoteCommand::Session {
            command: SessionCommand::List { project, worktree },
        } => {
            args.project = project;
            args.worktree = worktree;
        }
        RemoteCommand::Session {
            command: SessionCommand::Query { query },
        } => {
            args.query = query;
        }
        RemoteCommand::App { .. }
        | RemoteCommand::Focus { .. }
        | RemoteCommand::Selection { .. }
        | RemoteCommand::Session { .. }
        | RemoteCommand::Project { .. }
        | RemoteCommand::Status { .. }
        | RemoteCommand::Settings { .. } => {}
    }

    args
}

fn main() -> Result<(), miette::Report> {
    miette::set_panic_hook();

    // let args: Args = match args::from_std_args() {
    //     Ok(args) => args,
    //     Err(err) => {
    //         if err.is_help_request() {
    //             if let Some(help) = err.help_text() {
    //                 println!("{help}");
    //                 return;
    //             }
    //         }
    //         let handler = miette::GraphicalReportHandler::new();
    //         let stderr = stderr().lock();
    //         handler.render_report(&mut stderr, &err);
    //         return;
    //     }
    // };
    let args: Args = args::from_std_args()?;

    if let Some(Command::Remote {
        json,
        text,
        socket,
        action,
    }) = args.command
    {
        let client_args = build_remote_client_args(RemoteArgs {
            json,
            text,
            socket,
            action,
        });
        if let Err(err) = remote::run_remote(client_args) {
            eprintln!("error from term server: {err}");
            std::process::exit(1);
        }
        return Ok(());
    }

    // Set up tracing to file only if TERM_LOG is set
    if let Ok(log_file_path) = std::env::var("TERM_LOG") {
        use std::fs::File;
        use tracing_subscriber::fmt;
        use tracing_subscriber::prelude::*;
        let log_file = File::create(&log_file_path).expect("failed to create log file");
        tracing_subscriber::registry()
            .with(fmt::layer().with_writer(log_file).with_ansi(false))
            .init();
    }

    let (remote_sender, remote_receiver) = smol::channel::unbounded();
    let remote_server = match RemoteServer::start(remote_sender) {
        Ok(server) => Some(server),
        Err(err) => {
            warn!(?err, "failed to start remote server");
            None
        }
    };

    let remote_socket = remote_server.as_ref().map(|server| server.path.clone());
    let io = Arc::new(RealIo);
    let mut model = Model::new_with_remote(io, remote_socket).expect("failed to create terminals");
    if remote_server.is_some() {
        model.set_remote_receiver(remote_receiver);
    }

    let program = Program::new(&mut model, update, view).map_event(|event| match event {
        Event::Key(key) => Some(Msg::Key(key)),
        Event::Paste(text) => Some(Msg::Paste(text)),
        Event::FocusGained => Some(Msg::FocusGained),
        Event::FocusLost => Some(Msg::FocusLost),
        _ => None,
    });

    if let Err(err) = program.run() {
        eprintln!("Program failed: {:?}", err);
        return Ok(());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::divider::{Divider, DividerOrientation};
    use crate::persistence::{load_projects_from_root, save_projects_to_root};
    use chatui::buffer::DoubleBuffer;
    use chatui::dom::rounding::round_layout;
    use chatui::event::{KeyCode, Size};
    use chatui::palette::Palette;
    use chatui::render::Renderer;
    use chatui::test_utils::render_node_to_lines;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};
    use taffy::compute_root_layout;

    fn test_model() -> Option<Model> {
        let io = Arc::new(term_io::FakeIo::new());
        match Model::new_with_io(io, None) {
            Ok(model) => Some(model),
            Err(err) => {
                eprintln!("Skipping term test: {err}");
                None
            }
        }
    }

    fn activate_first_project(model: &mut Model) -> ProjectId {
        let project_id = model.projects[0].id;
        update(
            model,
            Msg::Tree(TreeMsg::Activate(TreeId::Project(project_id))),
        );
        project_id
    }

    struct TerminalLayout {
        width: f32,
        height: f32,
        x: f32,
        y: f32,
    }

    struct DividerLayout {
        orientation: DividerOrientation,
        width: f32,
        height: f32,
        x: f32,
        y: f32,
    }

    fn collect_terminal_layouts(
        node: &Node<Msg>,
        origin: (f32, f32),
        layouts: &mut Vec<TerminalLayout>,
    ) {
        let layout = node.layout_state().layout;
        let x = origin.0 + layout.location.x;
        let y = origin.1 + layout.location.y;

        if let Some(renderable) = node.as_renderable() {
            if renderable.debug_label() == "terminal" {
                layouts.push(TerminalLayout {
                    width: layout.size.width,
                    height: layout.size.height,
                    x,
                    y,
                });
            }
        }
        if let Some(element) = node.as_element() {
            for child in &element.children {
                collect_terminal_layouts(child, (x, y), layouts);
            }
        }
    }

    fn collect_divider_layouts(
        node: &Node<Msg>,
        origin: (f32, f32),
        layouts: &mut Vec<DividerLayout>,
    ) {
        let layout = node.layout_state().layout;
        let x = origin.0 + layout.location.x;
        let y = origin.1 + layout.location.y;

        if let Some(renderable) = node.as_renderable() {
            if renderable.debug_label() == "divider" {
                let divider = renderable
                    .as_any()
                    .downcast_ref::<Divider>()
                    .expect("divider renderable");
                layouts.push(DividerLayout {
                    orientation: divider.orientation,
                    width: layout.size.width,
                    height: layout.size.height,
                    x,
                    y,
                });
            }
        }

        if let Some(element) = node.as_element() {
            for child in &element.children {
                collect_divider_layouts(child, (x, y), layouts);
            }
        }
    }

    fn char_at(lines: &[String], x: usize, y: usize) -> char {
        lines[y]
            .chars()
            .nth(x)
            .unwrap_or_else(|| panic!("missing char at {x}, {y}"))
    }

    #[test]
    fn saving_projects_after_adding_project_persists_all_projects() {
        let io = Arc::new(term_io::FakeIo::with_test_dir(PathBuf::from(
            "/tmp/project",
        )));
        let Ok(mut model) = Model::new_with_io(io.clone(), None) else {
            eprintln!("Skipping term test: failed to build model");
            return;
        };

        let initial = io.saved_projects_snapshots();
        assert_eq!(initial.len(), 1);
        assert_eq!(initial[0].len(), 1);
        assert_eq!(initial[0][0].0, "project");
        assert_eq!(initial[0][0].1, PathBuf::from("/tmp/project"));

        model
            .add_project(PathBuf::from("/tmp/second"), "second".to_string())
            .expect("add project");

        let snapshots = io.saved_projects_snapshots();
        assert_eq!(snapshots.len(), 2);
        let last = snapshots.last().expect("snapshot present");
        assert_eq!(last.len(), 2);
        assert_eq!(last[0].0, "project");
        assert_eq!(last[1].0, "second");
        assert_eq!(last[1].1, PathBuf::from("/tmp/second"));
    }

    #[test]
    fn load_and_save_projects_use_config_root() {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let base = PathBuf::from(format!("/tmp/term-test-{now}"));
        let config_root = base.join("config");
        let project_path = base.join("workspace");
        fs::create_dir_all(&project_path).expect("workspace dir");

        let project = Project::new(ProjectId(1), "demo".to_string(), project_path.clone());
        save_projects_to_root(&[project], Some(&config_root)).expect("save");

        let persisted = load_projects_from_root(Some(&config_root)).expect("load");
        assert_eq!(persisted.len(), 1);
        assert_eq!(persisted[0].name, "demo");
        assert_eq!(persisted[0].path, project_path);

        fs::remove_dir_all(base).ok();
    }

    #[test]
    fn deleting_last_session_creates_replacement() {
        let Some(mut model) = test_model() else {
            return;
        };
        let _project_id = activate_first_project(&mut model);
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
        let project_id = activate_first_project(&mut model);

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
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
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
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
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
    fn remote_activate_target_ignores_selection_session_for_explicit_project() {
        let Some(mut model) = test_model() else {
            return;
        };
        activate_first_project(&mut model);
        let cwd = std::env::current_dir().expect("cwd available");
        model
            .add_project(cwd, "secondary".to_string())
            .expect("project added");

        let primary_project = model.projects[0].id;
        let secondary_project = model.projects[1].id;
        let primary_session = model.projects[0].sessions[0].id;

        model
            .tree
            .select(TreeId::Session(primary_project, None, primary_session));

        let params = RemoteParams {
            project_id: Some(secondary_project.0),
            ..RemoteParams::default()
        };
        let (_, response) = handle_remote_activate_target(&mut model, &params);

        assert!(response.ok, "expected ok response, got {response:?}");
        let secondary_session = model.projects[1]
            .sessions
            .first()
            .expect("secondary session created")
            .id;
        assert_eq!(
            model.tree.selected(),
            Some(&TreeId::Session(secondary_project, None, secondary_session))
        );
    }

    #[test]
    fn cmd_number_switches_to_session_in_active_project() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
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
    fn remote_session_query_filters_sessions() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        model.projects[0].name = "alpha".to_string();

        let worktree_id = match model.add_worktree(project_id, "dev".to_string(), VcsKind::Git) {
            Some(TreeId::Worktree(_, worktree_id)) => worktree_id,
            _ => panic!("expected worktree to be created"),
        };
        model.tree.select(TreeId::Worktree(project_id, worktree_id));
        update(&mut model, Msg::NewSession);

        let (session_id, worktree_path) = {
            let worktree = model
                .projects
                .get_mut(0)
                .and_then(|project| project.worktree_mut(worktree_id))
                .expect("worktree exists");
            let session = worktree
                .sessions
                .first_mut()
                .expect("worktree session exists");
            session.custom_title = Some("backend".to_string());
            session.has_unread_output = true;
            session.bell = true;
            (session.id.0, worktree.path.display().to_string())
        };

        let params = RemoteParams {
            query: Some(vec![
                remote::RemoteSessionQuery {
                    key: "project name".to_string(),
                    operator: "~=".to_string(),
                    value: "alp".to_string(),
                },
                remote::RemoteSessionQuery {
                    key: "workspace name".to_string(),
                    operator: "=".to_string(),
                    value: "dev".to_string(),
                },
                remote::RemoteSessionQuery {
                    key: "cwd".to_string(),
                    operator: "=".to_string(),
                    value: worktree_path,
                },
                remote::RemoteSessionQuery {
                    key: "has updates".to_string(),
                    operator: "=".to_string(),
                    value: "true".to_string(),
                },
                remote::RemoteSessionQuery {
                    key: "has bell".to_string(),
                    operator: "=".to_string(),
                    value: "true".to_string(),
                },
            ]),
            ..RemoteParams::default()
        };

        let (_, response) = handle_remote_query_sessions(&model, &params);
        assert!(response.ok, "expected ok response, got {response:?}");
        let Some(RemoteResult::Sessions { sessions }) = response.result else {
            panic!("expected session list response");
        };
        assert_eq!(sessions.len(), 1);
        assert_eq!(sessions[0].session_id, session_id);
    }

    #[test]
    fn cmd_number_does_not_change_when_session_missing() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
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
        activate_first_project(&mut model);
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
        activate_first_project(&mut model);
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

        model.rebuild_tree();

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
    fn active_session_keeps_bell_indicator_until_input() {
        let Some(mut model) = test_model() else {
            return;
        };
        activate_first_project(&mut model);
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

        let _ = model.sync_session_state(&TreeId::Session(pid, worktree, sid));
        model.rebuild_tree();

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
            "active session label should contain bell indicator before input"
        );

        // Send input to clear bell
        update(
            &mut model,
            Msg::Terminal {
                project: pid,
                worktree,
                session: sid,
                msg: TerminalMsg::Input(vec![b'a']),
            },
        );

        rebuild_tree(&model.projects, &mut model.tree, &mut model.active);

        let visible_session_after = model
            .tree
            .visible()
            .iter()
            .find(|node| matches!(node.id, TreeId::Session(_, _, _)))
            .expect("session visible");

        let label_after = visible_session_after
            .label
            .iter()
            .find(|span| span.content.contains('🔔'))
            .map(|span| span.content.clone());

        assert!(
            label_after.is_none(),
            "active session label should not contain bell indicator after input"
        );
    }

    #[test]
    fn background_activity_marks_unread_indicator() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
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
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
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
    fn terminal_tall_layout_two_sessions() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
        let first_session = model.projects[0].sessions[0].id;
        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: first_session,
        });

        let mut node = terminal_pane(&model).with_fill();
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(80.0),
                height: taffy::AvailableSpace::Definite(20.0),
            },
        );
        round_layout(&mut node);

        let mut layouts = Vec::new();
        collect_terminal_layouts(&node, (0.0, 0.0), &mut layouts);
        assert_eq!(layouts.len(), 2);

        layouts.sort_by(|a, b| a.x.partial_cmp(&b.x).unwrap());
        let left = &layouts[0];
        let right = &layouts[1];

        assert_eq!(left.x, 0.0);
        assert_eq!(left.width, 40.0);
        assert_eq!(left.height, 20.0);
        assert_eq!(right.x, 41.0);
        assert_eq!(right.width, 39.0);
        assert_eq!(right.height, 20.0);
    }

    #[test]
    fn terminal_wide_layout_two_sessions() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
        let first_session = model.projects[0].sessions[0].id;
        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: first_session,
        });
        model.toggle_active_layout();

        let mut node = terminal_pane(&model).with_fill();
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(80.0),
                height: taffy::AvailableSpace::Definite(20.0),
            },
        );
        round_layout(&mut node);

        let mut layouts = Vec::new();
        collect_terminal_layouts(&node, (0.0, 0.0), &mut layouts);
        assert_eq!(layouts.len(), 2);

        layouts.sort_by(|a, b| a.y.partial_cmp(&b.y).unwrap());
        let top = &layouts[0];
        let bottom = &layouts[1];

        assert_eq!(top.y, 0.0);
        assert_eq!(top.width, 80.0);
        assert_eq!(top.height, 10.0);
        assert_eq!(bottom.y, 11.0);
        assert_eq!(bottom.width, 80.0);
        assert_eq!(bottom.height, 9.0);
    }

    #[test]
    fn terminal_wide_layout_three_sessions() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
        update(&mut model, Msg::NewSession);
        let second_session = model.projects[0].sessions[1].id;
        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: second_session,
        });
        model.toggle_active_layout();

        let mut node = terminal_pane(&model).with_fill();
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(90.0),
                height: taffy::AvailableSpace::Definite(30.0),
            },
        );
        round_layout(&mut node);

        let mut layouts = Vec::new();
        collect_terminal_layouts(&node, (0.0, 0.0), &mut layouts);
        assert_eq!(layouts.len(), 3);

        let mut top = Vec::new();
        let mut bottom = Vec::new();
        for layout in &layouts {
            if layout.y == 0.0 {
                top.push(layout);
            } else {
                bottom.push(layout);
            }
        }

        assert_eq!(top.len(), 1);
        assert_eq!(bottom.len(), 2);

        let top = top[0];
        assert_eq!(top.width, 90.0);
        assert_eq!(top.height, 15.0);

        bottom.sort_by(|a, b| a.x.partial_cmp(&b.x).unwrap());
        assert_eq!(bottom[0].y, 16.0);
        assert_eq!(bottom[1].y, 16.0);
        assert_eq!(bottom[0].width, 45.0);
        assert_eq!(bottom[1].width, 44.0);
        assert_eq!(bottom[0].height, 14.0);
        assert_eq!(bottom[1].height, 14.0);
    }

    #[test]
    fn terminal_tall_layout_three_sessions() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
        update(&mut model, Msg::NewSession);
        let second_session = model.projects[0].sessions[1].id;
        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: second_session,
        });

        let mut node = terminal_pane(&model).with_fill();
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(90.0),
                height: taffy::AvailableSpace::Definite(30.0),
            },
        );
        round_layout(&mut node);

        let mut layouts = Vec::new();
        collect_terminal_layouts(&node, (0.0, 0.0), &mut layouts);
        assert_eq!(layouts.len(), 3);

        let mut left = Vec::new();
        let mut right = Vec::new();
        for layout in &layouts {
            if layout.x == 0.0 {
                left.push(layout);
            } else {
                right.push(layout);
            }
        }

        assert_eq!(left.len(), 1);
        assert_eq!(right.len(), 2);

        let left = left[0];
        assert_eq!(left.width, 45.0);
        assert_eq!(left.height, 30.0);

        right.sort_by(|a, b| a.y.partial_cmp(&b.y).unwrap());
        assert_eq!(right[0].x, 46.0);
        assert_eq!(right[1].x, 46.0);
        assert_eq!(right[0].width, 44.0);
        assert_eq!(right[1].width, 44.0);
        assert_eq!(right[0].height, 15.0);
        assert_eq!(right[1].height, 14.0);
    }

    #[test]
    fn renders_tall_layout_session_borders() {
        let Some(mut model) = test_model() else {
            return;
        };
        activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
        update(&mut model, Msg::NewSession);

        let mut node = terminal_pane(&model).with_fill();
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(40.0),
                height: taffy::AvailableSpace::Definite(10.0),
            },
        );
        round_layout(&mut node);

        let mut divider_layouts = Vec::new();
        collect_divider_layouts(&node, (0.0, 0.0), &mut divider_layouts);
        assert_eq!(divider_layouts.len(), 2);

        let mut buffer = DoubleBuffer::new(40, 10);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer
            .render(&node, Size::new(40, 10))
            .expect("render should succeed");

        let lines: Vec<String> = renderer
            .buffer()
            .to_string()
            .lines()
            .map(|s| s.to_owned())
            .collect();

        let mut vertical_count = 0;
        let mut horizontal_count = 0;
        for divider in &divider_layouts {
            let x = divider.x as usize;
            let y = divider.y as usize;
            let width = divider.width as usize;
            let height = divider.height as usize;
            match divider.orientation {
                DividerOrientation::Vertical => {
                    vertical_count += 1;
                    for row in y..y + height {
                        assert_eq!(char_at(&lines, x, row), '│');
                    }
                }
                DividerOrientation::Horizontal => {
                    horizontal_count += 1;
                    for col in x..x + width {
                        assert_eq!(char_at(&lines, col, y), '─');
                    }
                }
            }
        }

        assert_eq!(vertical_count, 1);
        assert_eq!(horizontal_count, 1);
    }

    #[test]
    fn renders_sidebar_tree() {
        let Some(mut model) = test_model() else {
            return;
        };
        activate_first_project(&mut model);
        update(&mut model, Msg::NewSession);
        model.focus = Focus::Sidebar;

        let project_name = &model.projects[0].name;
        let mut node = sidebar_view(
            &model.tree,
            &model.tree_scroll,
            model.auto_hide,
            true,
            false,
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

    #[test]
    fn strip_layout_renders_correctly() {
        let Some(mut model) = test_model() else {
            return;
        };
        let _project_id = activate_first_project(&mut model);

        // Switch to strip layout
        model.toggle_active_layout(); // Tall -> Wide
        model.toggle_active_layout(); // Wide -> Strip
        assert_eq!(model.active_layout(), Layout::Strip);

        // Add multiple sessions to test strip layout
        update(&mut model, Msg::NewSession);
        update(&mut model, Msg::NewSession);
        update(&mut model, Msg::NewSession);

        // Create a test view
        let mut node = terminal_pane(&model);

        // Layout with a reasonable size
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(100.0),
                height: taffy::AvailableSpace::Definite(24.0),
            },
        );
        round_layout(&mut node);

        // Render to check for visual issues (content may be empty in test environment)
        let _lines = render_node_to_lines(&mut node, 100, 24).expect("render should succeed");

        // Check that the node has reasonable dimensions
        let layout = node.layout_state().layout;
        assert!(layout.size.width > 0.0, "Strip should have positive width");
        assert!(
            layout.size.height > 0.0,
            "Strip should have positive height"
        );

        // Check that terminals are properly sized and positioned
        let terminal_layouts = collect_terminal_layouts_strip(&node);
        assert!(
            !terminal_layouts.is_empty(),
            "Should have terminal layouts in strip layout"
        );

        // All terminals should have positive dimensions
        for layout in &terminal_layouts {
            assert!(layout.width > 0.0, "Terminal should have positive width");
            assert!(layout.height > 0.0, "Terminal should have positive height");
        }

        // In strip layout, terminals should be arranged horizontally
        // Check that they have reasonable positioning
        if terminal_layouts.len() > 1 {
            for i in 1..terminal_layouts.len() {
                let prev = &terminal_layouts[i - 1];
                let curr = &terminal_layouts[i];
                // Terminals should be positioned at different x coordinates
                assert!(
                    curr.x > prev.x,
                    "Terminals should be arranged horizontally in strip layout"
                );
            }
        }
    }

    /// Helper to collect terminal layouts from the strip view
    fn collect_terminal_layouts_strip(node: &Node<Msg>) -> Vec<TerminalLayout> {
        let mut layouts = Vec::new();
        collect_terminal_layouts_recursive_strip(node, (0.0, 0.0), &mut layouts);
        layouts
    }

    fn collect_terminal_layouts_recursive_strip(
        node: &Node<Msg>,
        origin: (f32, f32),
        layouts: &mut Vec<TerminalLayout>,
    ) {
        let layout = node.layout_state().layout;
        let x = origin.0 + layout.location.x;
        let y = origin.1 + layout.location.y;

        if let Some(renderable) = node.as_renderable() {
            if renderable.debug_label() == "terminal" {
                layouts.push(TerminalLayout {
                    width: layout.size.width,
                    height: layout.size.height,
                    x,
                    y,
                });
            }
        }
        if let Some(element) = node.as_element() {
            let new_origin = (origin.0 + layout.location.x, origin.1 + layout.location.y);
            for child in &element.children {
                collect_terminal_layouts_recursive_strip(child, new_origin, layouts);
            }
        }
    }

    #[test]
    fn renders_filter_modal() {
        let modal_state = ModalState::SessionFilter {
            input: InputState::with_value("title~=test".to_string()),
            scroll: ScrollState::horizontal(),
        };

        let mut node = modal_view(&modal_state, |_| panic!("no modal msg expected"));

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(80.0),
                height: taffy::AvailableSpace::Definite(24.0),
            },
        );
        round_layout(&mut node);

        let mut buffer = DoubleBuffer::new(80, 24);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer
            .render(&node, Size::new(80, 24))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();

        // Should show the filter modal title
        assert!(
            screen.contains("Filter sessions"),
            "screen should contain filter modal title"
        );

        // Should show the filter input value
        assert!(
            screen.contains("title~=test"),
            "screen should contain the filter text"
        );
    }

    #[test]
    fn filter_bubbles_unread_indicators() {
        let Some(mut model) = test_model() else {
            return;
        };
        let project_id = activate_first_project(&mut model);
        update(&mut model, Msg::NewSession); // Creates session 2

        let first_session = model.projects[0].sessions[0].id;
        let second_session = model.projects[0].sessions[1].id;

        // Rename sessions to avoid spaces in filter (parser limitation)
        if let Some(project) = model.projects.get_mut(0) {
            if let Some(s1) = project.sessions.iter_mut().find(|s| s.id == first_session) {
                s1.custom_title = Some("unique1".to_string());
                s1.sync_display_name();
            }
            if let Some(s2) = project.sessions.iter_mut().find(|s| s.id == second_session) {
                s2.custom_title = Some("unique2".to_string());
                s2.sync_display_name();
                // Set unread on the second session
                s2.has_unread_output = true;
                s2.bell = false;
            }
        }

        // Apply a filter that matches ONLY the first session (hides the second one)
        model.filter = Some(filter::parse_filter("title=unique1").expect("valid filter"));

        // Select the first session so the second one isn't forced to be visible
        model.select_session(SessionKey {
            project: project_id,
            worktree: None,
            session: first_session,
        });

        model.rebuild_tree();

        // Check the visible nodes
        let visible: Vec<_> = model.tree.visible().iter().collect();

        // Verify session 2 is NOT visible
        let session2_visible = visible.iter().any(|node| match node.id {
            TreeId::Session(_, _, sid) => sid == second_session,
            _ => false,
        });
        assert!(!session2_visible, "Session 2 should be hidden by filter");

        // Verify session 1 IS visible
        let session1_visible = visible.iter().any(|node| match node.id {
            TreeId::Session(_, _, sid) => sid == first_session,
            _ => false,
        });
        assert!(session1_visible, "Session 1 should be visible");

        // Verify Project node has the bubble indicator " •"
        let project_node = visible
            .iter()
            .find(|node| matches!(node.id, TreeId::Project(pid) if pid == project_id))
            .expect("project node visible");

        assert!(
            project_node
                .label
                .iter()
                .any(|span| span.content.contains("•")),
            "Project node should show unread indicator for hidden child"
        );
    }
}
