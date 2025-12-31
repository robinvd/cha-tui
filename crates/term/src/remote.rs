use std::collections::HashMap;
use std::io::IsTerminal;
use std::io::{self, Read, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

use chatui::event::{Key, KeyCode};
use facet::Facet;
use smol::channel::Sender;

use crate::project::{ProjectId, WorktreeId};
use crate::session::SessionId;
use crate::vcs::VcsKind;

pub const ENV_REMOTE_SOCKET: &str = "TERM_REMOTE_SOCKET";
pub const ENV_PROJECT_ID: &str = "TERM_PROJECT_ID";
pub const ENV_WORKTREE_ID: &str = "TERM_WORKTREE_ID";
pub const ENV_SESSION_ID: &str = "TERM_SESSION_ID";
pub const REMOTE_PROTOCOL_VERSION: u32 = 1;
const REMOTE_ACTIONS: &[&str] = &[
    "app-quit",
    "focus-toggle",
    "focus-sidebar",
    "focus-terminal",
    "focus-terminal-lock",
    "selection-move-up",
    "selection-move-down",
    "selection-reorder-up",
    "selection-reorder-down",
    "selection-activate",
    "selection-delete",
    "selection-get",
    "session-next",
    "session-prev",
    "session-index",
    "session-new",
    "session-rename",
    "session-list",
    "session-get",
    "project-new",
    "project-list",
    "project-worktree-new",
    "project-worktree-list",
    "input-key",
    "input-paste",
    "input-send",
    "settings-sidebar-auto-hide-toggle",
    "settings-sidebar-auto-hide-on",
    "settings-sidebar-auto-hide-off",
    "status-get",
];

#[derive(Clone, Debug, Default, Facet)]
pub struct RemoteParams {
    #[facet(default)]
    pub project_id: Option<u64>,
    #[facet(default)]
    pub worktree_id: Option<u64>,
    #[facet(default)]
    pub session_id: Option<u64>,
    #[facet(default)]
    pub index: Option<usize>,
    #[facet(default)]
    pub name: Option<String>,
    #[facet(default)]
    pub path: Option<String>,
    #[facet(default)]
    pub vcs: Option<String>,
    #[facet(default)]
    pub key: Option<RemoteKey>,
    #[facet(default)]
    pub text: Option<String>,
    #[facet(default)]
    pub input: Option<String>,
    #[facet(default)]
    pub bytes: Option<Vec<u8>>,
}

#[derive(Clone, Debug, Default, Facet)]
pub struct RemoteKey {
    pub code: String,
    #[facet(default)]
    pub ctrl: bool,
    #[facet(default)]
    pub alt: bool,
    #[facet(default)]
    pub shift: bool,
    #[facet(default)]
    pub super_key: bool,
}

#[derive(Clone, Debug, Facet)]
pub struct RemoteRequest {
    pub version: u32,
    pub action: String,
    #[facet(default)]
    pub params: RemoteParams,
}

#[derive(Clone, Debug, Facet)]
pub struct RemoteResponse {
    pub version: u32,
    pub ok: bool,
    #[facet(skip_serializing_if = Option::is_none)]
    pub result: Option<RemoteResult>,
    #[facet(skip_serializing_if = Option::is_none)]
    pub error: Option<String>,
}

#[derive(Clone, Debug, Facet)]
#[repr(u8)]
pub enum RemoteResult {
    Ok {
        #[facet(skip_serializing_if = Option::is_none)]
        message: Option<String>,
    },
    Projects {
        projects: Vec<RemoteProject>,
    },
    Worktrees {
        worktrees: Vec<RemoteWorktree>,
    },
    Sessions {
        sessions: Vec<RemoteSession>,
    },
    CurrentSession {
        session: Option<RemoteSession>,
    },
    CurrentSelection {
        selection: Option<RemoteSelection>,
    },
    Status {
        status: RemoteStatus,
    },
}

#[derive(Clone, Debug, Facet)]
pub struct RemoteProject {
    pub id: u64,
    pub name: String,
    pub path: String,
}

#[derive(Clone, Debug, Facet)]
pub struct RemoteWorktree {
    pub project_id: u64,
    pub id: u64,
    pub name: String,
    pub path: String,
}

#[derive(Clone, Debug, Facet)]
pub struct RemoteSession {
    pub project_id: u64,
    #[facet(skip_serializing_if = Option::is_none)]
    pub worktree_id: Option<u64>,
    pub session_id: u64,
    pub number: usize,
    #[facet(skip_serializing_if = Option::is_none)]
    pub title: Option<String>,
    #[facet(skip_serializing_if = Option::is_none)]
    pub custom_title: Option<String>,
    pub display_name: String,
    pub exited: bool,
    pub bell: bool,
    pub has_unread_output: bool,
}

#[derive(Clone, Copy, Debug, Facet, PartialEq, Eq)]
pub struct RemoteSelection {
    pub project_id: u64,
    #[facet(skip_serializing_if = Option::is_none)]
    pub worktree_id: Option<u64>,
    #[facet(skip_serializing_if = Option::is_none)]
    pub session_id: Option<u64>,
}

#[derive(Clone, Debug, Facet)]
pub struct RemoteStatus {
    pub focus: String,
    pub auto_hide: bool,
}

#[derive(Clone, Debug)]
pub struct RemoteEnvelope {
    pub request: RemoteRequest,
    pub respond_to: mpsc::Sender<RemoteResponse>,
}

pub struct RemoteServer {
    pub path: PathBuf,
}

pub enum OutputFormat {
    Json,
    Text,
}

pub struct RemoteClientArgs {
    pub action: String,
    pub json: bool,
    pub text: bool,
    pub socket: Option<String>,
    pub project: Option<u64>,
    pub worktree: Option<u64>,
    pub session: Option<u64>,
    pub index: Option<usize>,
    pub name: Option<String>,
    pub path: Option<String>,
    pub vcs: Option<String>,
    pub key: Option<String>,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub super_key: bool,
    pub content: Option<String>,
    pub input: Option<String>,
    pub bytes: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct NewSessionTarget {
    pub project_id: u64,
    pub worktree_id: Option<u64>,
    pub insert_after: Option<u64>,
}

impl RemoteResponse {
    pub fn ok(result: Option<RemoteResult>) -> Self {
        Self {
            version: REMOTE_PROTOCOL_VERSION,
            ok: true,
            result,
            error: None,
        }
    }

    pub fn err(message: impl Into<String>) -> Self {
        Self {
            version: REMOTE_PROTOCOL_VERSION,
            ok: false,
            result: None,
            error: Some(message.into()),
        }
    }
}

impl RemoteServer {
    pub fn start(sender: Sender<RemoteEnvelope>) -> io::Result<Self> {
        let path = socket_path()?;
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        if path.exists() {
            let _ = std::fs::remove_file(&path);
        }
        let listener = UnixListener::bind(&path)?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let _ = std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600));
        }

        thread::spawn(move || {
            for connection in listener.incoming() {
                let stream = match connection {
                    Ok(stream) => stream,
                    Err(err) => {
                        eprintln!("term remote listener error: {err}");
                        continue;
                    }
                };
                let sender = sender.clone();
                thread::spawn(move || {
                    handle_connection(stream, sender);
                });
            }
        });

        Ok(Self { path })
    }
}

impl Drop for RemoteServer {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

pub fn run_remote(args: RemoteClientArgs) -> color_eyre::Result<()> {
    let socket = resolve_socket(&args)?;
    let format = select_output_format(io::stdout().is_terminal(), args.json, args.text)
        .map_err(|err| color_eyre::eyre::eyre!(err))?;
    validate_action(&args.action).map_err(|err| color_eyre::eyre::eyre!(err))?;
    let params = params_from_args(&args)?;
    let request = RemoteRequest {
        version: REMOTE_PROTOCOL_VERSION,
        action: args.action,
        params,
    };
    let payload = facet_json::to_string(&request).map_err(|err| color_eyre::eyre::eyre!(err))?;
    let response = send_request(&socket, &payload)?;

    match format {
        OutputFormat::Json => {
            println!("{response}");
        }
        OutputFormat::Text => {
            let parsed: RemoteResponse = match facet_json::from_str(&response) {
                Ok(parsed) => parsed,
                Err(err) => {
                    let snippet = response_snippet(&response);
                    return Err(color_eyre::eyre::eyre!(
                        "Failed to parse response: {err}\nRaw response (truncated): {snippet}"
                    ));
                }
            };
            print_response_text(&parsed)?;
        }
    }

    Ok(())
}

pub fn resolve_new_session_target(
    params: &RemoteParams,
    selection: Option<RemoteSelection>,
) -> Option<NewSessionTarget> {
    let project_id = params.project_id.or(selection.map(|sel| sel.project_id))?;
    let worktree_id = params
        .worktree_id
        .or(selection.and_then(|sel| sel.worktree_id));
    let insert_after = params
        .session_id
        .or(selection.and_then(|sel| sel.session_id));
    Some(NewSessionTarget {
        project_id,
        worktree_id,
        insert_after,
    })
}

pub fn parse_key_spec(spec: &RemoteKey) -> Result<Key, String> {
    let code = parse_key_code(&spec.code)?;
    Ok(Key::with_modifiers(
        code,
        spec.ctrl,
        spec.alt,
        spec.shift,
        spec.super_key,
    ))
}

pub fn parse_vcs_kind(spec: &str) -> Option<VcsKind> {
    match spec.to_ascii_lowercase().as_str() {
        "git" => Some(VcsKind::Git),
        "jj" => Some(VcsKind::Jj),
        _ => None,
    }
}

pub fn parse_key_code(input: &str) -> Result<KeyCode, String> {
    let lower = input.to_ascii_lowercase();
    if lower == "space" {
        return Ok(KeyCode::Char(' '));
    }
    if lower.len() == 1 {
        let ch = lower.chars().next().unwrap();
        return Ok(KeyCode::Char(ch));
    }
    match lower.as_str() {
        "enter" => Ok(KeyCode::Enter),
        "esc" | "escape" => Ok(KeyCode::Esc),
        "backspace" => Ok(KeyCode::Backspace),
        "left" => Ok(KeyCode::Left),
        "right" => Ok(KeyCode::Right),
        "up" => Ok(KeyCode::Up),
        "down" => Ok(KeyCode::Down),
        "page-up" => Ok(KeyCode::PageUp),
        "page-down" => Ok(KeyCode::PageDown),
        "home" => Ok(KeyCode::Home),
        "end" => Ok(KeyCode::End),
        "tab" => Ok(KeyCode::Tab),
        "insert" => Ok(KeyCode::Insert),
        "delete" => Ok(KeyCode::Delete),
        "caps-lock" => Ok(KeyCode::CapsLock),
        "scroll-lock" => Ok(KeyCode::ScrollLock),
        "num-lock" => Ok(KeyCode::NumLock),
        "print-screen" => Ok(KeyCode::PrintScreen),
        "pause" => Ok(KeyCode::Pause),
        "menu" => Ok(KeyCode::Menu),
        _ => {
            if let Some(num) = lower.strip_prefix('f') {
                let value = num
                    .parse::<u8>()
                    .map_err(|_| format!("Invalid function key: {input}"))?;
                if value == 0 {
                    return Err("Function keys start at f1".to_string());
                }
                return Ok(KeyCode::Function(value));
            }
            Err(format!("Unknown key code: {input}"))
        }
    }
}

pub fn parse_bytes_list(input: &str) -> Result<Vec<u8>, String> {
    if input.trim().is_empty() {
        return Ok(Vec::new());
    }
    let mut bytes = Vec::new();
    for part in input.split(',') {
        let trimmed = part.trim();
        let value = if let Some(hex) = trimmed.strip_prefix("0x") {
            u8::from_str_radix(hex, 16)
        } else if let Some(hex) = trimmed.strip_prefix("0X") {
            u8::from_str_radix(hex, 16)
        } else {
            trimmed.parse::<u8>()
        }
        .map_err(|_| format!("Invalid byte value: {trimmed}"))?;
        bytes.push(value);
    }
    Ok(bytes)
}

pub fn select_output_format(
    is_tty: bool,
    force_json: bool,
    force_text: bool,
) -> Result<OutputFormat, String> {
    if force_json && force_text {
        return Err("Use only one of --json or --text".to_string());
    }
    if force_json {
        return Ok(OutputFormat::Json);
    }
    if force_text {
        return Ok(OutputFormat::Text);
    }
    if is_tty {
        Ok(OutputFormat::Text)
    } else {
        Ok(OutputFormat::Json)
    }
}

fn socket_path() -> io::Result<PathBuf> {
    if let Some(dir) = std::env::var_os("XDG_RUNTIME_DIR") {
        return Ok(PathBuf::from(dir)
            .join("chatui")
            .join(format!("term-{}.sock", std::process::id())));
    }
    let base = directories::ProjectDirs::from("", "", "chatui")
        .map(|dirs| dirs.cache_dir().to_path_buf())
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "cache directory unavailable"))?;
    Ok(base
        .join("term")
        .join(format!("term-{}.sock", std::process::id())))
}

fn resolve_socket(args: &RemoteClientArgs) -> color_eyre::Result<PathBuf> {
    if let Some(socket) = &args.socket {
        return Ok(PathBuf::from(socket));
    }
    if let Some(socket) = std::env::var_os(ENV_REMOTE_SOCKET) {
        return Ok(PathBuf::from(socket));
    }
    Err(color_eyre::eyre::eyre!(
        "TERM_REMOTE_SOCKET is not set and no --socket provided"
    ))
}

fn params_from_args(args: &RemoteClientArgs) -> color_eyre::Result<RemoteParams> {
    let mut params = RemoteParams::default();
    let explicit_project = args.project.is_some();
    let explicit_worktree = args.worktree.is_some();
    let explicit_session = args.session.is_some();

    params.project_id = args.project.or_else(|| env_id(ENV_PROJECT_ID));
    params.worktree_id = if explicit_worktree {
        args.worktree
    } else if explicit_project {
        None
    } else {
        env_id(ENV_WORKTREE_ID)
    };
    params.session_id = if explicit_session {
        args.session
    } else if explicit_project || explicit_worktree {
        None
    } else {
        env_id(ENV_SESSION_ID)
    };
    params.index = args.index;
    params.name = args.name.clone();
    params.path = args.path.clone();
    params.vcs = args.vcs.clone();
    params.text = args.content.clone();

    if let Some(key) = &args.key {
        params.key = Some(RemoteKey {
            code: key.clone(),
            ctrl: args.ctrl,
            alt: args.alt,
            shift: args.shift,
            super_key: args.super_key,
        });
    }

    if let Some(input) = &args.input {
        params.input = Some(input.clone());
    }
    if let Some(bytes) = &args.bytes {
        params.bytes = Some(parse_bytes_list(bytes).map_err(|err| color_eyre::eyre::eyre!(err))?);
    }

    Ok(params)
}

fn env_id(var: &str) -> Option<u64> {
    let value = std::env::var(var).ok()?;
    value.parse::<u64>().ok()
}

fn send_request(socket: &PathBuf, payload: &str) -> color_eyre::Result<String> {
    let mut stream = UnixStream::connect(socket)?;
    stream.write_all(payload.as_bytes())?;
    stream.shutdown(std::net::Shutdown::Write)?;
    let mut response = String::new();
    stream.read_to_string(&mut response)?;
    Ok(response)
}

fn print_response_text(response: &RemoteResponse) -> color_eyre::Result<()> {
    if !response.ok {
        let message = response
            .error
            .clone()
            .unwrap_or_else(|| "remote request failed".to_string());
        return Err(color_eyre::eyre::eyre!(message));
    }

    match response.result.as_ref() {
        None => println!("ok"),
        Some(RemoteResult::Ok { message }) => {
            if let Some(message) = message {
                println!("{message}");
            } else {
                println!("ok");
            }
        }
        Some(RemoteResult::Projects { projects }) => {
            for project in projects {
                println!("{} {} {}", project.id, project.name, project.path);
            }
        }
        Some(RemoteResult::Worktrees { worktrees }) => {
            for worktree in worktrees {
                println!(
                    "{} {} {} {}",
                    worktree.project_id, worktree.id, worktree.name, worktree.path
                );
            }
        }
        Some(RemoteResult::Sessions { sessions }) => {
            for session in sessions {
                let worktree = session
                    .worktree_id
                    .map(|id| id.to_string())
                    .unwrap_or_else(|| "-".to_string());
                println!(
                    "{} {} {} {} {}",
                    session.project_id,
                    worktree,
                    session.session_id,
                    session.number,
                    session.display_name
                );
            }
        }
        Some(RemoteResult::CurrentSession { session }) => {
            if let Some(session) = session {
                println!("{}", session.display_name);
            }
        }
        Some(RemoteResult::CurrentSelection { selection }) => {
            if let Some(selection) = selection {
                let worktree = selection
                    .worktree_id
                    .map(|id| id.to_string())
                    .unwrap_or_else(|| "-".to_string());
                let session = selection
                    .session_id
                    .map(|id| id.to_string())
                    .unwrap_or_else(|| "-".to_string());
                println!("{} {} {}", selection.project_id, worktree, session);
            }
        }
        Some(RemoteResult::Status { status }) => {
            println!("focus={} auto_hide={}", status.focus, status.auto_hide);
        }
    }

    Ok(())
}

fn handle_connection(mut stream: UnixStream, sender: Sender<RemoteEnvelope>) {
    let mut input = String::new();
    if let Err(err) = stream.read_to_string(&mut input) {
        let _ = write_response(&mut stream, RemoteResponse::err(err.to_string()));
        return;
    }
    let request: RemoteRequest = match facet_json::from_str(&input) {
        Ok(request) => request,
        Err(err) => {
            let _ = write_response(&mut stream, RemoteResponse::err(err.to_string()));
            return;
        }
    };
    let (respond_to, response_rx) = mpsc::channel();
    let envelope = RemoteEnvelope {
        request,
        respond_to,
    };
    if smol::block_on(sender.send(envelope)).is_err() {
        let _ = write_response(&mut stream, RemoteResponse::err("remote queue closed"));
        return;
    }
    let response = match response_rx.recv_timeout(Duration::from_secs(5)) {
        Ok(response) => response,
        Err(_) => RemoteResponse::err("remote request timed out"),
    };
    let _ = write_response(&mut stream, response);
}

fn write_response(stream: &mut UnixStream, response: RemoteResponse) -> io::Result<()> {
    let payload = facet_json::to_string(&response)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err.to_string()))?;
    stream.write_all(payload.as_bytes())
}

pub fn params_project_id(params: &RemoteParams) -> Option<ProjectId> {
    params.project_id.map(ProjectId)
}

pub fn params_worktree_id(params: &RemoteParams) -> Option<WorktreeId> {
    params.worktree_id.map(WorktreeId)
}

pub fn params_session_id(params: &RemoteParams) -> Option<SessionId> {
    params.session_id.map(SessionId)
}

pub fn params_input_bytes(params: &RemoteParams) -> Option<Vec<u8>> {
    if let Some(bytes) = &params.bytes {
        return Some(bytes.clone());
    }
    params.input.as_ref().map(|input| input.as_bytes().to_vec())
}

pub fn params_action_name(action: &str) -> String {
    action.trim().to_ascii_lowercase()
}

fn validate_action(action: &str) -> Result<(), String> {
    let normalized = params_action_name(action);
    if REMOTE_ACTIONS.contains(&normalized.as_str()) {
        Ok(())
    } else {
        Err(format!(
            "Unknown action: {normalized}\nAvailable actions: {}",
            REMOTE_ACTIONS.join(", ")
        ))
    }
}

fn response_snippet(response: &str) -> String {
    let trimmed = response.trim();
    if trimmed.is_empty() {
        return "<empty response>".to_string();
    }
    let limit = 512usize;
    if trimmed.chars().count() <= limit {
        return trimmed.to_string();
    }
    let snippet: String = trimmed.chars().take(limit).collect();
    format!("{snippet}...")
}

pub fn worktree_selection(project: ProjectId, worktree: WorktreeId) -> RemoteSelection {
    RemoteSelection {
        project_id: project.0,
        worktree_id: Some(worktree.0),
        session_id: None,
    }
}

pub fn project_selection(project: ProjectId) -> RemoteSelection {
    RemoteSelection {
        project_id: project.0,
        worktree_id: None,
        session_id: None,
    }
}

pub fn session_selection(
    project: ProjectId,
    worktree: Option<WorktreeId>,
    session: SessionId,
) -> RemoteSelection {
    RemoteSelection {
        project_id: project.0,
        worktree_id: worktree.map(|id| id.0),
        session_id: Some(session.0),
    }
}

pub fn remote_env_map(
    socket_path: &PathBuf,
    project: ProjectId,
    worktree: Option<WorktreeId>,
    session: SessionId,
) -> HashMap<String, String> {
    let mut env = HashMap::new();
    env.insert(
        ENV_REMOTE_SOCKET.to_string(),
        socket_path.display().to_string(),
    );
    env.insert(ENV_PROJECT_ID.to_string(), project.0.to_string());
    env.insert(ENV_SESSION_ID.to_string(), session.0.to_string());
    if let Some(worktree) = worktree {
        env.insert(ENV_WORKTREE_ID.to_string(), worktree.0.to_string());
    }
    env
}

#[cfg(test)]
mod tests {
    use super::*;
    use once_cell::sync::Lazy;
    use std::sync::Mutex;

    static ENV_GUARD: Lazy<Mutex<()>> = Lazy::new(|| Mutex::new(()));

    fn with_env(vars: &[(&str, Option<&str>)], f: impl FnOnce()) {
        let _guard = ENV_GUARD.lock().unwrap();
        let mut previous = Vec::new();
        for (key, value) in vars {
            let key_string = (*key).to_string();
            let old = std::env::var(key).ok();
            previous.push((key_string.clone(), old));
            match value {
                Some(val) => unsafe {
                    std::env::set_var(key, val);
                },
                None => unsafe {
                    std::env::remove_var(key);
                },
            }
        }
        f();
        for (key, value) in previous {
            match value {
                Some(val) => unsafe {
                    std::env::set_var(&key, val);
                },
                None => unsafe {
                    std::env::remove_var(&key);
                },
            }
        }
    }

    fn base_args() -> RemoteClientArgs {
        RemoteClientArgs {
            action: "status-get".to_string(),
            json: false,
            text: false,
            socket: None,
            project: None,
            worktree: None,
            session: None,
            index: None,
            name: None,
            path: None,
            vcs: None,
            key: None,
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            content: None,
            input: None,
            bytes: None,
        }
    }

    #[test]
    fn output_format_prefers_text_for_tty() {
        assert!(matches!(
            select_output_format(true, false, false).unwrap(),
            OutputFormat::Text
        ));
    }

    #[test]
    fn output_format_prefers_json_for_pipe() {
        assert!(matches!(
            select_output_format(false, false, false).unwrap(),
            OutputFormat::Json
        ));
    }

    #[test]
    fn output_format_flags_override() {
        assert!(matches!(
            select_output_format(true, true, false).unwrap(),
            OutputFormat::Json
        ));
        assert!(matches!(
            select_output_format(false, false, true).unwrap(),
            OutputFormat::Text
        ));
    }

    #[test]
    fn parse_bytes_list_accepts_hex() {
        let bytes = parse_bytes_list("0x1b,0x5b,65").unwrap();
        assert_eq!(bytes, vec![0x1b, 0x5b, 65]);
    }

    #[test]
    fn resolve_new_session_target_prefers_params() {
        let params = RemoteParams {
            project_id: Some(2),
            worktree_id: Some(3),
            session_id: Some(4),
            ..RemoteParams::default()
        };
        let selection = RemoteSelection {
            project_id: 1,
            worktree_id: Some(2),
            session_id: Some(3),
        };
        let target = resolve_new_session_target(&params, Some(selection)).unwrap();
        assert_eq!(
            target,
            NewSessionTarget {
                project_id: 2,
                worktree_id: Some(3),
                insert_after: Some(4),
            }
        );
    }

    #[test]
    fn resolve_new_session_target_uses_selection() {
        let params = RemoteParams::default();
        let selection = RemoteSelection {
            project_id: 7,
            worktree_id: None,
            session_id: Some(9),
        };
        let target = resolve_new_session_target(&params, Some(selection)).unwrap();
        assert_eq!(
            target,
            NewSessionTarget {
                project_id: 7,
                worktree_id: None,
                insert_after: Some(9),
            }
        );
    }

    #[test]
    fn validate_action_accepts_known_action() {
        assert!(validate_action("project-list").is_ok());
    }

    #[test]
    fn validate_action_rejects_unknown_action() {
        let err = validate_action("nope").unwrap_err();
        assert!(err.contains("Unknown action"));
    }

    #[test]
    fn params_from_args_ignore_env_session_for_explicit_project() {
        with_env(
            &[
                (ENV_PROJECT_ID, Some("1")),
                (ENV_WORKTREE_ID, Some("5")),
                (ENV_SESSION_ID, Some("6")),
            ],
            || {
                let mut args = base_args();
                args.project = Some(2);
                let params = params_from_args(&args).unwrap();
                assert_eq!(params.project_id, Some(2));
                assert_eq!(params.worktree_id, None);
                assert_eq!(params.session_id, None);
            },
        );
    }

    #[test]
    fn params_from_args_ignore_env_session_for_explicit_worktree() {
        with_env(
            &[
                (ENV_PROJECT_ID, Some("1")),
                (ENV_WORKTREE_ID, Some("5")),
                (ENV_SESSION_ID, Some("6")),
            ],
            || {
                let mut args = base_args();
                args.worktree = Some(10);
                let params = params_from_args(&args).unwrap();
                assert_eq!(params.project_id, Some(1));
                assert_eq!(params.worktree_id, Some(10));
                assert_eq!(params.session_id, None);
            },
        );
    }
}
