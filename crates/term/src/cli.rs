use crate::remote::RemoteClientArgs;
use facet::Facet;
use facet_args as args;

#[derive(Facet)]
pub struct Args {
    /// Subcommand to run (omit to launch the TUI).
    #[facet(args::subcommand)]
    pub command: Option<Command>,
}

#[derive(Facet)]
#[repr(u8)]
pub enum Command {
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

#[derive(Facet)]
#[repr(u8)]
pub enum RemoteCommand {
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
pub enum AppCommand {
    /// Quit the running term instance.
    Quit,
}

#[derive(Facet)]
#[repr(u8)]
pub enum FocusCommand {
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
pub enum SelectionCommand {
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
pub enum Direction {
    Up,
    Down,
}

#[derive(Facet)]
#[repr(u8)]
pub enum SessionCommand {
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
pub enum ProjectCommand {
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
pub enum ProjectWorktreeCommand {
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
pub enum InputCommand {
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
pub enum StatusCommand {
    /// Show the current status.
    Get,
}

#[derive(Facet)]
#[repr(u8)]
pub enum SettingsCommand {
    /// Sidebar-related settings.
    Sidebar {
        #[facet(args::subcommand)]
        command: SidebarSettingsCommand,
    },
}

#[derive(Facet)]
#[repr(u8)]
pub enum SidebarSettingsCommand {
    /// Configure sidebar auto-hide.
    AutoHide {
        #[facet(args::subcommand)]
        mode: ToggleMode,
    },
}

#[derive(Facet)]
#[repr(u8)]
pub enum ToggleMode {
    On,
    Off,
    Toggle,
}

pub struct RemoteArgs {
    pub json: bool,
    pub text: bool,
    pub socket: Option<String>,
    pub action: RemoteCommand,
}

pub fn remote_action_name(action: &RemoteCommand) -> &'static str {
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

pub fn build_remote_client_args(remote: RemoteArgs) -> RemoteClientArgs {
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
