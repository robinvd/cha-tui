use std::cell::Cell;
use std::collections::HashMap;
use std::env;
use std::path::PathBuf;
use std::process::Stdio;
use std::rc::Rc;
use std::sync::Arc;
use std::thread;

use agent_client_protocol::{
    Agent, CancelNotification, Client, ClientCapabilities, ClientSideConnection, Content,
    ContentBlock, Implementation, InitializeRequest, LoadSessionRequest, NewSessionRequest, PermissionOptionKind, Plan,
    PlanEntryPriority, PlanEntryStatus, PromptRequest, ProtocolVersion, RequestPermissionOutcome,
    RequestPermissionRequest, RequestPermissionResponse, SelectedPermissionOutcome,
    SessionNotification, SessionUpdate, StopReason, TextContent, ToolCall, ToolCallContent,
    ToolCallId, ToolCallStatus, ToolCallUpdate,
};
use async_trait::async_trait;
use chatui_markdown::{MarkdownDocument, MarkdownMsg, MarkdownState, markdown_view};
use chatui::components::paragraph;
use chatui::components::scroll::{ScrollAxis, ScrollBehavior, ScrollTarget};
use chatui::dom::{Color, Node};
use chatui::event::{Event, Key, KeyCode};
use chatui::{
    InputMsg, InputState, InputStyle, Program, ScrollMsg, ScrollState, Style, TextSpan, Transition,
    block_with_title, column, default_input_keybindings, input, row, scrollable_content, text,
};
use fuztea::{ColumnConfig, FuzzyFinder, FuzzyFinderEvent, FuzzyFinderMsg};
use miette::{Context, IntoDiagnostic, Result, miette};
use smol::channel;
use smol::process::Command;
use taffy::prelude::TaffyZero;
use taffy::style::Dimension;
use time::OffsetDateTime;

mod session_store;
use session_store::{RealIo, SavedSession, SessionIo};

#[derive(Clone)]
struct AgentArgs {
    server: String,
    server_args: Vec<String>,
    cwd: PathBuf,
}

#[derive(Clone, Debug)]
enum AgentCommand {
    SendPrompt(String),
    CancelPrompt,
    LoadSession(String),
}

#[derive(Clone, Debug)]
enum AgentEvent {
    Status(String),
    Ready,
    Session(Box<SessionUpdate>),
    PromptFinished(StopReason),
    PermissionRequest {
        request: Box<RequestPermissionRequest>,
        respond_to: channel::Sender<RequestPermissionResponse>,
    },
    Error(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Author {
    User,
    Agent,
    System,
}

#[derive(Clone, Debug)]
struct ChatMessage {
    author: Author,
    content: String,
    markdown: Option<MarkdownDocument>,
    markdown_state: MarkdownState,
}

#[derive(Clone, Debug)]
enum ChatEntry {
    Message(ChatMessage),
    Thought(String),
    Plan(Plan),
    ToolCall(ToolCall),
    PermissionPrompt(PermissionPrompt),
}

#[derive(Clone, Debug)]
struct PermissionPrompt {
    request: RequestPermissionRequest,
    responder: channel::Sender<RequestPermissionResponse>,
    selected: usize,
}

struct Model {
    timeline: Vec<ChatEntry>,
    input: InputState,
    input_style: InputStyle,
    chat_scroll: ScrollState,
    status: String,
    agent_in_progress: bool,
    agent_ready: bool,
    agent_listener_active: bool,
    agent_events: channel::Receiver<AgentEvent>,
    agent_commands: channel::Sender<AgentCommand>,
    server_label: String,
    pending_permission_idx: Option<usize>,
    tool_calls: HashMap<ToolCallId, ToolCall>,
    session_finder: Option<FuzzyFinder<SavedSession>>,
    session_io: Arc<dyn SessionIo>,
}

impl Model {
    fn new(
        events: channel::Receiver<AgentEvent>,
        commands: channel::Sender<AgentCommand>,
        server_label: String,
        session_io: Arc<dyn SessionIo>,
    ) -> Self {
        let mut chat_scroll = ScrollState::new(ScrollBehavior::Vertical);
        chat_scroll.set_offset(0.0);
        let mut input_style = InputStyle::default();
        input_style.text.bg = Some(Color::Palette(8));
        Self {
            timeline: Vec::new(),
            input: InputState::new_multiline(),
            input_style,
            chat_scroll,
            status: format!("Connecting to {server_label}"),
            agent_in_progress: false,
            agent_ready: false,
            agent_listener_active: false,
            agent_events: events,
            agent_commands: commands,
            server_label,
            pending_permission_idx: None,
            tool_calls: HashMap::new(),
            session_finder: None,
            session_io,
        }
    }

    fn push_message(&mut self, author: Author, content: impl Into<String>) {
        self.push_entry(ChatEntry::Message(ChatMessage {
            author,
            content: content.into(),
            markdown: None,
            markdown_state: MarkdownState::new(),
        }));
    }

    fn push_entry(&mut self, entry: ChatEntry) {
        self.apply_sticky_scroll(|model| model.timeline.push(entry));
    }

    fn apply_sticky_scroll(&mut self, f: impl FnOnce(&mut Self)) {
        let stick_to_bottom = self.chat_scroll.is_at_end(ScrollAxis::Vertical);
        f(self);
        if stick_to_bottom {
            self.scroll_to_latest_message();
        }
    }

    fn scroll_to_latest_message(&mut self) {
        if self.timeline.is_empty() {
            return;
        }
        self.chat_scroll
            .ensure_visible("chat-log", ScrollTarget::new("chat-last-message"));
    }
}

#[derive(Clone)]
struct UiClient {
    events: channel::Sender<AgentEvent>,
}

#[async_trait(?Send)]
impl Client for UiClient {
    async fn request_permission(
        &self,
        args: RequestPermissionRequest,
    ) -> agent_client_protocol::Result<RequestPermissionResponse> {
        let (respond_to, responses) = channel::bounded(1);
        let _ = self
            .events
            .send(AgentEvent::PermissionRequest {
                request: Box::new(args.clone()),
                respond_to,
            })
            .await;

        let outcome = responses.recv().await.unwrap_or_else(|_| {
            RequestPermissionResponse::new(RequestPermissionOutcome::Cancelled)
        });

        Ok(outcome)
    }

    async fn session_notification(
        &self,
        args: SessionNotification,
    ) -> agent_client_protocol::Result<()> {
        let _ = self
            .events
            .send(AgentEvent::Session(Box::new(args.update)))
            .await;
        Ok(())
    }
}

fn main() -> Result<()> {
    miette::set_panic_hook();

    // Set up tracing to file only if KAERU_LOG is set
    if let Ok(log_file_path) = std::env::var("KAERU_LOG") {
        use std::fs::File;
        use tracing_subscriber::fmt;
        use tracing_subscriber::prelude::*;
        let log_file = File::create(&log_file_path).expect("failed to create log file");
        tracing_subscriber::registry()
            .with(fmt::layer().with_writer(log_file).with_ansi(false))
            .init();
    }

    let args = parse_args()?;
    let server_label = args.server.clone();
    let session_io = Arc::new(RealIo::new());
    let (command_tx, event_rx) = start_agent_thread(args, session_io.clone())?;

    let mut model = Model::new(event_rx, command_tx, server_label.clone(), session_io);
    let mut program = Program::new(&mut model, update, view).map_event(map_event);

    if let Err(err) = smol::block_on(program.run_async()) {
        eprintln!("Program exited with error: {:?}", err);
    }

    Ok(())
}

fn parse_args() -> Result<AgentArgs> {
    let mut args = env::args().skip(1).peekable();
    let mut cwd = None;
    let mut server = None;
    let mut server_args = Vec::new();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--cwd" | "-C" => {
                let path = args
                    .next()
                    .ok_or_else(|| miette!("--cwd requires a path argument"))?;
                cwd = Some(PathBuf::from(path));
            }
            "--" => {
                server_args.extend(args);
                break;
            }
            _ if server.is_none() => server = Some(arg),
            _ => server_args.push(arg),
        }
    }

    let server =
        server.ok_or_else(|| miette!("Usage: kaeru <acp-server> [args...] [--cwd PATH]"))?;
    let cwd = cwd.unwrap_or(env::current_dir().into_diagnostic()?);

    Ok(AgentArgs {
        server,
        server_args,
        cwd,
    })
}

fn start_agent_thread(
    args: AgentArgs,
    session_io: Arc<dyn SessionIo>,
) -> Result<(channel::Sender<AgentCommand>, channel::Receiver<AgentEvent>)> {
    let (command_tx, command_rx) = channel::unbounded();
    let (event_tx, event_rx) = channel::unbounded();

    thread::spawn(move || {
        let executor = Rc::new(smol::LocalExecutor::new());
        let run_executor = executor.clone();
        let events = event_tx.clone();
        let result = smol::block_on(async move {
            run_executor
                .run(agent_worker(executor, args, command_rx, event_tx, session_io))
                .await
        });

        if let Err(err) = result {
            let _ = smol::block_on(events.send(AgentEvent::Error(err.to_string())));
        }
    });

    Ok((command_tx, event_rx))
}

async fn agent_worker(
    executor: Rc<smol::LocalExecutor<'static>>,
    args: AgentArgs,
    commands: channel::Receiver<AgentCommand>,
    events: channel::Sender<AgentEvent>,
    session_io: Arc<dyn SessionIo>,
) -> Result<()> {
    let mut child = Command::new(&args.server);
    if !args.server_args.is_empty() {
        child.args(&args.server_args);
    }
    let mut child = child
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .into_diagnostic()
        .wrap_err("Failed to launch ACP server")?;

    let stdin = child
        .stdin
        .take()
        .ok_or_else(|| miette!("ACP server stdin unavailable"))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| miette!("ACP server stdout unavailable"))?;

    let client = UiClient {
        events: events.clone(),
    };

    let (connection, io_task) = ClientSideConnection::new(client, stdin, stdout, {
        let executor = executor.clone();
        move |task| {
            executor.spawn(task).detach();
        }
    });
    executor.spawn(io_task).detach();
    let connection = Rc::new(connection);

    let _ = events
        .send(AgentEvent::Status(format!("Connected to {}", args.server)))
        .await;

    let init_request = InitializeRequest::new(ProtocolVersion::LATEST)
        .client_capabilities(ClientCapabilities::default())
        .client_info(
            Implementation::new("kaeru", env!("CARGO_PKG_VERSION")).title("Kaeru".to_string()),
        );

    let init = connection.initialize(init_request).await;

    let init_response = match init {
        Ok(resp) => resp,
        Err(err) => {
            let _ = events
                .send(AgentEvent::Error(format!("Initialize failed: {}", err)))
                .await;
            let _ = child.kill();
            return Ok(());
        }
    };

    let session = connection
        .new_session(NewSessionRequest::new(args.cwd.clone()))
        .await;

    let mut session_id = match session {
        Ok(resp) => resp.session_id,
        Err(err) => {
            let _ = events
                .send(AgentEvent::Error(format!("Session start failed: {}", err)))
                .await;
            let _ = child.kill();
            return Ok(());
        }
    };

    if let Err(e) = session_io.save_session(SavedSession {
        id: session_id.to_string(),
        date: OffsetDateTime::now_utc(),
        prompt: "<New Session>".to_string(),
    }) {
         let _ = events.send(AgentEvent::Error(format!("Failed to save session: {}", e))).await;
    }

    let _ = events.send(AgentEvent::Ready).await;
    let _ = events
        .send(AgentEvent::Status(format!(
            "Negotiated protocol {}. Session ready.",
            init_response.protocol_version
        )))
        .await;

    let mut first_prompt = true;
    let prompt_inflight = Rc::new(Cell::new(false));
    while let Ok(command) = commands.recv().await {
        match command {
            AgentCommand::LoadSession(id) => {
                let req = LoadSessionRequest::new(id.clone(), args.cwd.clone());
                match connection.load_session(req).await {
                    Ok(_) => {
                        session_id = id.into();
                        first_prompt = false; // Loaded session presumably has history
                        let _ = events
                            .send(AgentEvent::Status(format!("Session loaded: {}", session_id)))
                            .await;
                    }
                    Err(err) => {
                        let _ = events
                            .send(AgentEvent::Error(format!("Failed to load session: {}", err)))
                            .await;
                    }
                }
            }
            AgentCommand::SendPrompt(text) => {
                if text.trim().is_empty() {
                    continue;
                }

                if first_prompt {
                    let _ = session_io.update_session_prompt(&session_id.to_string(), text.clone());
                    first_prompt = false;
                }

                if prompt_inflight.replace(true) {
                    let _ = events
                        .send(AgentEvent::Status(
                            "Prompt already in progress; ignoring new request".to_string(),
                        ))
                        .await;
                    continue;
                }

                let prompt = PromptRequest::new(session_id.clone(), vec![ContentBlock::from(text)]);
                let prompt_events = events.clone();
                let prompt_connection = connection.clone();
                let prompt_state = prompt_inflight.clone();
                executor
                    .spawn(async move {
                        let result = prompt_connection.prompt(prompt).await;
                        if !prompt_state.replace(false) {
                            return;
                        }
                        match result {
                            Ok(response) => {
                                let _ = prompt_events
                                    .send(AgentEvent::PromptFinished(response.stop_reason))
                                    .await;
                            }
                            Err(err) => {
                                let _ = prompt_events
                                    .send(AgentEvent::Error(format!("Prompt failed: {}", err)))
                                    .await;
                            }
                        }
                    })
                    .detach();
            }
            AgentCommand::CancelPrompt => {
                let cancel = CancelNotification::new(session_id.clone());
                let cancel_events = events.clone();
                let cancel_connection = connection.clone();
                let prompt_state = prompt_inflight.clone();
                executor
                    .spawn(async move {
                        match cancel_connection.cancel(cancel).await {
                            Ok(())
                                => {
                                if prompt_state.replace(false) {
                                    let _ = cancel_events
                                        .send(AgentEvent::PromptFinished(StopReason::Cancelled))
                                        .await;
                                }
                            }
                            Err(err) => {
                                let _ = cancel_events
                                    .send(AgentEvent::Error(format!("Cancel failed: {}", err)))
                                    .await;
                            }
                        }
                    })
                    .detach();
            }
        }
    }

    let _ = child.kill();
    Ok(())
}

fn map_event(event: Event) -> Option<Msg> {
    match event {
        Event::Key(key) => Some(Msg::KeyPressed(key)),
        Event::Resize(_) => Some(Msg::Resize),
        _ => None,
    }
}

enum Msg {
    KeyPressed(Key),
    Resize,
    Input(InputMsg),
    Scroll(ScrollMsg),
    Agent(Box<AgentEvent>),
    AgentClosed,
    Markdown(usize, MarkdownMsg),
    SessionFinder(FuzzyFinderMsg),
    Noop,
}

fn handle_finder_event(
    model: &mut Model,
    event: FuzzyFinderEvent<SavedSession, Msg>,
) -> Transition<Msg> {
    match event {
        FuzzyFinderEvent::Continue => Transition::Continue,
        FuzzyFinderEvent::Cancel => {
            model.session_finder = None;
            Transition::Continue
        }
        FuzzyFinderEvent::Select(selection) => {
            model.session_finder = None;
            if let Some(session) = selection {
                let id = session.id;
                model.status = format!("Loading session {}...", id);
                let commands = model.agent_commands.clone();
                Transition::Task(Box::pin(async move {
                    let _ = commands.send(AgentCommand::LoadSession(id)).await;
                    Msg::Noop
                }))
            } else {
                Transition::Continue
            }
        }
        FuzzyFinderEvent::Activate => {
             let selection = model.session_finder.as_ref().and_then(|f| f.submission()).cloned();
             model.session_finder = None;
             if let Some(session) = selection {
                 let id = session.id;
                 model.status = format!("Loading session {}...", id);
                 let commands = model.agent_commands.clone();
                 Transition::Task(Box::pin(async move {
                     let _ = commands.send(AgentCommand::LoadSession(id)).await;
                     Msg::Noop
                 }))
             } else {
                 Transition::Continue
             }
        }
        FuzzyFinderEvent::Task(task) => Transition::Task(task),
    }
}

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    match msg {
        Msg::KeyPressed(key) => handle_key(model, key),
        Msg::Resize => Transition::Multiple(vec![Transition::Continue, listen_for_agent(model)]),
        Msg::SessionFinder(msg) => {
            let event = if let Some(finder) = &mut model.session_finder {
                fuztea::update(finder, msg, Msg::SessionFinder)
            } else {
                return Transition::Continue;
            };
            handle_finder_event(model, event)
        }
        Msg::Input(input_msg) => {
            model.input.update(input_msg);
            Transition::Continue
        }
        Msg::Scroll(scroll_msg) => {
            model.chat_scroll.update(scroll_msg);
            Transition::Continue
        }
        Msg::Agent(event) => {
            model.agent_listener_active = false;
            let mut transition = handle_agent_event(model, *event);
            transition = transition.combine(listen_for_agent(model));
            transition
        }
        Msg::AgentClosed => {
            model.agent_listener_active = false;
            model.status = "Agent connection closed".to_string();
            model.agent_in_progress = false;

            if let Some(cancel) =
                clear_permission_prompt(model, RequestPermissionOutcome::Cancelled)
            {
                Transition::Multiple(vec![Transition::Continue, cancel])
            } else {
                Transition::Continue
            }
        }
        Msg::Markdown(idx, msg) => {
            if let Some(ChatEntry::Message(msg_entry)) = model.timeline.get_mut(idx) {
                msg_entry.markdown_state.update(msg);
            }
            Transition::Continue
        }
        Msg::Noop => Transition::Continue,
    }
}

fn handle_key(model: &mut Model, key: Key) -> Transition<Msg> {
    if model.session_finder.is_some() {
        let event = {
            let finder = model.session_finder.as_mut().unwrap();
            fuztea::update(finder, FuzzyFinderMsg::KeyPressed(key), Msg::SessionFinder)
        };
        return handle_finder_event(model, event);
    }

    if key.ctrl && matches!(key.code, KeyCode::Char('l')) {
        let sessions = model.session_io.load_sessions().unwrap_or_default();
        let config = ColumnConfig::default()
            .with_column_names(vec!["Date".to_string(), "Prompt".to_string()]);

        let (mut finder, handle) = FuzzyFinder::with_config(
            |s: &SavedSession| {
                let date_str = s
                    .date
                    .format(&time::format_description::well_known::Rfc3339)
                    .unwrap_or_default();
                vec![date_str, s.prompt.clone()]
            },
            config,
        );
        for s in sessions {
            handle.push_item(s);
        }
        // Force a tick to process injected items so they appear immediately
        finder.tick();
        model.session_finder = Some(finder);
        return Transition::Continue;
    }

    if key.ctrl
        && matches!(
            key.code,
            KeyCode::Char('c') | KeyCode::Char('q') | KeyCode::Char('C') | KeyCode::Char('Q')
        )
    {
        return Transition::Quit;
    }

    if let Some(transition) = handle_permission_key(model, key) {
        return transition;
    }

    if key.ctrl && matches!(key.code, KeyCode::Char('d')) {
        model.chat_scroll.update(ScrollMsg::AxisDeltaPercent {
            axis: ScrollAxis::Vertical,
            ratio: 0.5,
        });
        return Transition::Continue;
    }

    if key.ctrl && matches!(key.code, KeyCode::Char('u')) {
        model.chat_scroll.update(ScrollMsg::AxisDeltaPercent {
            axis: ScrollAxis::Vertical,
            ratio: -0.5,
        });
        return Transition::Continue;
    }

    if key.shift && matches!(key.code, KeyCode::Enter) {
        model.input.update(InputMsg::InsertChar('\n'));
        return Transition::Continue;
    }

    match key.code {
        KeyCode::Esc => cancel_or_quit(model),
        KeyCode::Enter => {
            if model.pending_permission_idx.is_some() {
                return submit_selected_permission(model);
            }

            if model.agent_in_progress {
                model.status = "Agent is in progress (Esc to cancel)".to_string();
                return Transition::Continue;
            }

            send_prompt(model)
        }
        KeyCode::PageUp => {
            model.chat_scroll.update(ScrollMsg::AxisDeltaPercent {
                axis: ScrollAxis::Vertical,
                ratio: -0.5,
            });
            Transition::Continue
        }
        KeyCode::PageDown => {
            model.chat_scroll.update(ScrollMsg::AxisDeltaPercent {
                axis: ScrollAxis::Vertical,
                ratio: 0.5,
            });
            Transition::Continue
        }
        _ => {
            if let Some(msg) = default_input_keybindings(&model.input, key, |m| m) {
                model.input.update(msg);
                return Transition::Continue;
            }
            Transition::Continue
        }
    }
}

fn send_prompt(model: &mut Model) -> Transition<Msg> {
    if !model.agent_ready {
        model.push_message(Author::System, "Session not ready yet.");
        return Transition::Continue;
    }

    if model.agent_in_progress {
        model.status = "Agent is in progress (Esc to cancel)".to_string();
        return Transition::Continue;
    }

    let message = model.input.value();
    if message.trim().is_empty() {
        return Transition::Continue;
    }

    model.push_message(Author::User, message.clone());
    model.input = InputState::default();
    model.agent_in_progress = true;
    model.status = "Waiting for agent...".to_string();

    let commands = model.agent_commands.clone();
    Transition::Multiple(vec![
        Transition::Continue,
        Transition::Task(Box::pin(async move {
            let _ = commands.send(AgentCommand::SendPrompt(message)).await;
            Msg::Noop
        })),
        listen_for_agent(model),
    ])
}

fn handle_agent_event(model: &mut Model, event: AgentEvent) -> Transition<Msg> {
    let mut transitions = vec![Transition::Continue];

    match event {
        AgentEvent::Status(message) => model.status = message,
        AgentEvent::Ready => {
            model.agent_ready = true;
            model.status = "Session ready".to_string();
        }
        AgentEvent::Session(update) => handle_session_update(model, *update),
        AgentEvent::PromptFinished(stop_reason) => {
            model.status = format!("Prompt finished: {:?}", stop_reason);
            model.agent_in_progress = false;

            if let Some(ChatEntry::Message(last)) = model
                .timeline
                .iter_mut()
                .rev()
                .find(|e| matches!(e, ChatEntry::Message(m) if m.author == Author::Agent))
            {
                if last.markdown.is_none() {
                    let doc = MarkdownDocument::parse(&last.content);
                    last.markdown_state.sync_with(&doc);
                    last.markdown = Some(doc);
                }
            }

            if let Some(cancel) =
                clear_permission_prompt(model, RequestPermissionOutcome::Cancelled)
            {
                transitions.push(cancel);
            }
        }
        AgentEvent::PermissionRequest {
            request,
            respond_to,
        } => {
            let request = *request;
            model.status = "Permission required".to_string();
            model.agent_in_progress = true;
            let description = describe_tool_call(&request.tool_call);
            model.push_message(
                Author::System,
                format!("Permission requested for {description}"),
            );
            let prompt = PermissionPrompt {
                request,
                responder: respond_to,
                selected: 0,
            };
            let idx = model.timeline.len();
            model.push_entry(ChatEntry::PermissionPrompt(prompt));
            model.pending_permission_idx = Some(idx);
        }
        AgentEvent::Error(err) => {
            model.push_message(Author::System, format!("Error: {err}"));
            model.status = "Error encountered".to_string();
            model.agent_in_progress = false;

            if let Some(cancel) =
                clear_permission_prompt(model, RequestPermissionOutcome::Cancelled)
            {
                transitions.push(cancel);
            }
        }
    }

    if transitions.len() == 1 {
        Transition::Continue
    } else {
        Transition::Multiple(transitions)
    }
}

fn handle_session_update(model: &mut Model, update: SessionUpdate) {
    match update {
        SessionUpdate::AgentMessageChunk(chunk) => {
            let text = content_text(&chunk.content);
            model.apply_sticky_scroll(move |model| {
                if let Some(ChatEntry::Message(last)) = model.timeline.last_mut()
                    && last.author == Author::Agent
                {
                    last.content.push_str(&text);
                } else {
                    model.timeline.push(ChatEntry::Message(ChatMessage {
                        author: Author::Agent,
                        content: text,
                        markdown: None,
                        markdown_state: MarkdownState::new(),
                    }));
                }
            });
        }
        SessionUpdate::UserMessageChunk(chunk) => {
            let text = content_text(&chunk.content);
            model.apply_sticky_scroll(move |model| {
                if let Some(ChatEntry::Message(last)) = model.timeline.last_mut()
                    && last.author == Author::User
                {
                    last.content.push_str(&text);
                } else {
                    model.timeline.push(ChatEntry::Message(ChatMessage {
                        author: Author::User,
                        content: text,
                        markdown: None,
                        markdown_state: MarkdownState::new(),
                    }));
                }
            });
        }
        SessionUpdate::Plan(plan) => {
            model.push_entry(ChatEntry::Plan(plan));
            model.status = "Plan updated".to_string();
        }
        SessionUpdate::AvailableCommandsUpdate(update) => {
            model.push_message(
                Author::System,
                format!(
                    "Available commands updated ({} commands).",
                    update.available_commands.len()
                ),
            );
        }
        SessionUpdate::CurrentModeUpdate(mode) => {
            model.push_message(
                Author::System,
                format!("Agent switched mode to {}", mode.current_mode_id),
            );
        }
        SessionUpdate::ToolCall(call) => {
            let snapshot = upsert_tool_call(&mut model.tool_calls, call);
            upsert_tool_entry(model, snapshot);
            model.status = "Tool call started".to_string();
        }
        SessionUpdate::ToolCallUpdate(update) => {
            let tool_call_id = update.tool_call_id.clone();
            match apply_tool_update(&mut model.tool_calls, update) {
                Some(snapshot) => {
                    model.status = format!("Tool updated: {}", describe_tool(&snapshot));
                    upsert_tool_entry(model, snapshot);
                }
                None => model.push_message(
                    Author::System,
                    format!("Tool update received for {}.", tool_call_id.0),
                ),
            }
        }
        SessionUpdate::AgentThoughtChunk(chunk) => {
            let text = content_text(&chunk.content);
            model.apply_sticky_scroll(move |model| {
                if let Some(ChatEntry::Thought(current)) = model.timeline.last_mut() {
                    current.push_str(&text);
                } else {
                    model.timeline.push(ChatEntry::Thought(text));
                }
            });
        }
        _ => {}
    }
}

fn upsert_tool_call(tool_calls: &mut HashMap<ToolCallId, ToolCall>, call: ToolCall) -> ToolCall {
    let call_id = call.tool_call_id.clone();
    tool_calls.insert(call_id, call.clone());
    call
}

fn upsert_tool_entry(model: &mut Model, call: ToolCall) {
    model.apply_sticky_scroll(move |model| {
        if let Some(existing) = model
            .timeline
            .iter_mut()
            .rev()
            .find_map(|entry| match entry {
                ChatEntry::ToolCall(existing) if existing.tool_call_id == call.tool_call_id => {
                    Some(existing)
                }
                _ => None,
            })
        {
            *existing = call.clone();
            return;
        }

        model.timeline.push(ChatEntry::ToolCall(call));
    });
}

fn apply_tool_update(
    tool_calls: &mut HashMap<ToolCallId, ToolCall>,
    update: ToolCallUpdate,
) -> Option<ToolCall> {
    if let Some(existing) = tool_calls.get_mut(&update.tool_call_id) {
        existing.update(update.fields);
        return Some(existing.clone());
    }

    match ToolCall::try_from(update.clone()) {
        Ok(call) => {
            tool_calls.insert(call.tool_call_id.clone(), call.clone());
            Some(call)
        }
        Err(_) => None,
    }
}

fn describe_tool(call: &ToolCall) -> String {
    let status = format_tool_status(call.status);
    format!("{} ({status})", call.title)
}

fn describe_tool_call(update: &ToolCallUpdate) -> String {
    let title = update
        .fields
        .title
        .clone()
        .unwrap_or_else(|| update.tool_call_id.0.to_string());
    let status = update
        .fields
        .status
        .map(format_tool_status)
        .unwrap_or_else(|| "pending".to_string());

    format!("{title} ({status})")
}

fn format_tool_status(status: ToolCallStatus) -> String {
    match status {
        ToolCallStatus::Pending => "pending".to_string(),
        ToolCallStatus::InProgress => "in progress".to_string(),
        ToolCallStatus::Completed => "completed".to_string(),
        ToolCallStatus::Failed => "failed".to_string(),
        _ => "unknown".to_string(),
    }
}

fn summarize_tool_content(content: &ToolCallContent) -> String {
    match content {
        ToolCallContent::Content(content) => content_text(&content.content),
        ToolCallContent::Diff(diff) => format!("Diff: {}", diff.path.display()),
        ToolCallContent::Terminal(term) => format!("Terminal {}", term.terminal_id.0),
        _ => "[tool output]".to_string(),
    }
}

fn truncate_preview(text: &str, max_len: usize) -> String {
    if text.len() <= max_len {
        text.to_string()
    } else {
        format!(
            "{}...",
            text.chars()
                .take(max_len.saturating_sub(3))
                .collect::<String>()
        )
    }
}

fn handle_permission_key(model: &mut Model, key: Key) -> Option<Transition<Msg>> {
    let prompt = pending_permission_mut(model)?;

    match key.code {
        KeyCode::Tab => {
            let options_len = prompt.request.options.len();
            if options_len == 0 {
                return Some(Transition::Continue);
            }

            if key.shift {
                if prompt.selected == 0 {
                    prompt.selected = options_len - 1;
                } else {
                    prompt.selected -= 1;
                }
            } else {
                prompt.selected = (prompt.selected + 1) % options_len;
            }

            Some(Transition::Continue)
        }
        KeyCode::Enter if !key.alt && !key.ctrl && !key.shift => {
            Some(submit_selected_permission(model))
        }
        _ => None,
    }
}

fn submit_selected_permission(model: &mut Model) -> Transition<Msg> {
    if let Some(mut prompt) = take_pending_permission(model) {
        if prompt.request.options.is_empty() {
            model.status = "Permission cancelled (no options)".to_string();
            return Transition::Multiple(vec![
                Transition::Continue,
                resolve_permission_prompt(prompt, RequestPermissionOutcome::Cancelled),
            ]);
        }

        if prompt.selected >= prompt.request.options.len() {
            prompt.selected = prompt.request.options.len() - 1;
        }

        let option = prompt.request.options[prompt.selected].clone();
        model.status = format!("Permission selected: {}", option.name);
        return Transition::Multiple(vec![
            Transition::Continue,
            resolve_permission_prompt(
                prompt,
                RequestPermissionOutcome::Selected(SelectedPermissionOutcome::new(
                    option.option_id.clone(),
                )),
            ),
        ]);
    }

    Transition::Continue
}

fn clear_permission_prompt(
    model: &mut Model,
    outcome: RequestPermissionOutcome,
) -> Option<Transition<Msg>> {
    take_pending_permission(model).map(|prompt| resolve_permission_prompt(prompt, outcome))
}

fn pending_permission_mut(model: &mut Model) -> Option<&mut PermissionPrompt> {
    let idx = model.pending_permission_idx?;
    match model.timeline.get_mut(idx) {
        Some(ChatEntry::PermissionPrompt(prompt)) => Some(prompt),
        _ => {
            model.pending_permission_idx = None;
            None
        }
    }
}

fn take_pending_permission(model: &mut Model) -> Option<PermissionPrompt> {
    let idx = model.pending_permission_idx?;
    model.pending_permission_idx = None;

    if idx >= model.timeline.len() {
        return None;
    }

    match model.timeline.remove(idx) {
        ChatEntry::PermissionPrompt(prompt) => Some(prompt),
        _ => None,
    }
}

fn resolve_permission_prompt(
    prompt: PermissionPrompt,
    outcome: RequestPermissionOutcome,
) -> Transition<Msg> {
    let responder = prompt.responder.clone();
    Transition::Task(Box::pin(async move {
        let _ = responder
            .send(RequestPermissionResponse::new(outcome))
            .await;
        Msg::Noop
    }))
}

fn cancel_or_quit(model: &mut Model) -> Transition<Msg> {
    if model.agent_in_progress {
        request_cancel(model)
    } else {
        Transition::Quit
    }
}

fn request_cancel(model: &mut Model) -> Transition<Msg> {
    model.status = "Cancel requested...".to_string();
    let commands = model.agent_commands.clone();
    let mut transitions = vec![
        Transition::Continue,
        Transition::Task(Box::pin(async move {
            let _ = commands.send(AgentCommand::CancelPrompt).await;
            Msg::Noop
        })),
    ];

    if let Some(cancel) = clear_permission_prompt(model, RequestPermissionOutcome::Cancelled) {
        transitions.push(cancel);
    }

    Transition::Multiple(transitions)
}

fn content_text(content: &ContentBlock) -> String {
    match content {
        ContentBlock::Text(text) => text.text.clone(),
        ContentBlock::Image(_) => "[image content]".to_string(),
        ContentBlock::Audio(_) => "[audio content]".to_string(),
        ContentBlock::Resource(_) => "[resource]".to_string(),
        ContentBlock::ResourceLink(_) => "[resource link]".to_string(),
        _ => "[unsupported content]".to_string(),
    }
}

fn listen_for_agent(model: &mut Model) -> Transition<Msg> {
    if model.agent_listener_active {
        return Transition::Continue;
    }

    let events = model.agent_events.clone();
    model.agent_listener_active = true;
    Transition::Task(Box::pin(async move {
        match events.recv().await {
            Ok(event) => Msg::Agent(Box::new(event)),
            Err(_) => Msg::AgentClosed,
        }
    }))
}

fn view(model: &Model) -> Node<'_, Msg> {
    let activity_style = if model.agent_in_progress {
        Style::fg(Color::Palette(3)).merged(&Style::bold())
    } else {
        Style::dim()
    };
    let activity_label = if model.agent_in_progress {
        "[busy]"
    } else {
        "[idle]"
    };
    let mut status_text = model.status.clone();
    if model.agent_in_progress {
        status_text.push_str(" | Agent is working (Esc to cancel; Enter disabled)");
    }

    let header = row(vec![
        text::<Msg>(format!("kaeru – {}", model.server_label)).with_style(Style::bold()),
        text::<Msg>(format!("  {}", status_text)).with_style(Style::dim()),
        text::<Msg>(format!("  {activity_label}")).with_style(activity_style),
    ])
    .with_min_height(Dimension::length(1.0));

    let mut message_nodes: Vec<Node<'_, Msg>> = Vec::new();

    if model.timeline.is_empty() {
        message_nodes.push(text::<Msg>("No messages yet. Type to chat.").with_style(Style::dim()));
    } else {
        message_nodes.extend(model.timeline.iter().enumerate().map(|(i, e)| render_entry(i, e)));
    }

    let total_nodes = message_nodes.len();
    let message_nodes: Vec<Node<'_, Msg>> = message_nodes
        .into_iter()
        .enumerate()
        .map(|(idx, mut node)| {
            if idx + 1 == total_nodes {
                node = node.with_id("chat-last-message");
            }
            node
        })
        .collect();

    let top_spacer = column(vec![])
        .with_flex_grow(1.)
        .with_flex_shrink(1.)
        .with_min_height(Dimension::ZERO);
    let bot_spacer = column(vec![]).with_height(Dimension::length(1.));
    let message_column = column(message_nodes)
        .with_gap(0, 1)
        .with_min_height(Dimension::ZERO)
        .with_flex_shrink(0.)
        .with_padding_bottom(1);

    let chat_log = scrollable_content(
        "chat-log",
        &model.chat_scroll,
        3,
        Msg::Scroll,
        column(vec![top_spacer, message_column, bot_spacer]).with_min_height(Dimension::ZERO),
    )
    .with_min_height(Dimension::length(3.0))
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO);

    let prompt_style = if model.agent_in_progress {
        Style::fg(Color::Palette(7)).merged(&Style::dim())
    } else {
        Style::fg(Color::Palette(2))
    };
    let prompt = text::<Msg>("> ").with_style(prompt_style);
    let input_field = input("chat-input", &model.input, &model.input_style, Msg::Input)
        .with_flex_grow(1.)
        .with_min_width(Dimension::ZERO);
    let input_row = row(vec![prompt, input_field]).with_min_height(Dimension::length(1.0));

    let input_area = column(vec![input_row])
        .with_padding(1)
        .with_gap(0, 1)
        .with_style(Style::bg(Color::Palette(8)));

    // column(vec![header, chat_box, input_row]).with_fill()
    let main_view = column(vec![chat_log, input_area, header]).with_fill();

    if let Some(finder) = &model.session_finder {
        return fuztea::view(finder, Msg::SessionFinder);
    }

    main_view
}

fn render_entry(idx: usize, entry: &ChatEntry) -> Node<'_, Msg> {
    match entry {
        ChatEntry::Message(message) => render_message(idx, message),
        ChatEntry::Thought(thought) => render_thought(thought),
        ChatEntry::Plan(plan) => render_plan(plan),
        ChatEntry::ToolCall(call) => render_tool_call(call),
        ChatEntry::PermissionPrompt(prompt) => render_permission_prompt_view(prompt),
    }
}

fn render_plan(plan: &Plan) -> Node<'_, Msg> {
    let entries: Vec<Node<'_, Msg>> = plan
        .entries
        .iter()
        .map(|entry| {
            let (status_label, status_style) = match entry.status {
                PlanEntryStatus::Pending => ("[ ]", Style::dim()),
                PlanEntryStatus::InProgress => ("[>]", Style::fg(Color::Palette(3))),
                PlanEntryStatus::Completed => ("[x]", Style::fg(Color::Palette(2))),
                _ => ("[ ]", Style::dim()),
            };

            let priority_style = match entry.priority {
                PlanEntryPriority::High => Style::fg(Color::Palette(1)).merged(&Style::bold()),
                PlanEntryPriority::Medium => Style::fg(Color::Palette(3)),
                PlanEntryPriority::Low => Style::dim(),
                _ => Style::dim(),
            };
            let priority_label = match entry.priority {
                PlanEntryPriority::High => "H",
                PlanEntryPriority::Medium => "M",
                PlanEntryPriority::Low => "L",
                _ => "?",
            };

            let content = paragraph::rich_paragraph::<Msg>(vec![TextSpan::new(
                &entry.content,
                Style::default(),
            )])
            .with_flex_grow(1.)
            .with_min_width(Dimension::ZERO);

            row(vec![
                text::<Msg>(priority_label).with_style(priority_style),
                text::<Msg>(status_label).with_style(status_style),
                content,
            ])
            .with_gap(1, 0)
        })
        .collect();

    block_with_title("Plan", vec![column(entries).with_gap(0, 1)])
        .with_padding(1)
        .with_style(Style::bg(Color::Palette(8)))
}

fn render_tool_call(call: &ToolCall) -> Node<'_, Msg> {
    let status_label = match call.status {
        ToolCallStatus::Completed => "Completed".to_string(),
        ToolCallStatus::Failed => "Failed".to_string(),
        ToolCallStatus::InProgress => "Working...".to_string(),
        ToolCallStatus::Pending => "Pending".to_string(),
        _ => "Unknown".to_string(),
    };

    let status_style = match call.status {
        ToolCallStatus::Completed => Style::fg(Color::Palette(2)).merged(&Style::bold()),
        ToolCallStatus::Failed => Style::fg(Color::Palette(1)).merged(&Style::bold()),
        ToolCallStatus::InProgress => Style::fg(Color::Palette(3)).merged(&Style::bold()),
        ToolCallStatus::Pending => Style::dim(),
        _ => Style::default(),
    };

    let mut rows = vec![
        row(vec![
            text::<Msg>("⚙").with_style(Style::dim().merged(&Style::bold())),
            text::<Msg>(status_label).with_style(status_style),
            paragraph::rich_paragraph::<Msg>(vec![TextSpan::new(&call.title, Style::bold())])
                .with_flex_grow(1.)
                .with_min_width(Dimension::ZERO),
        ])
        .with_gap(1, 0),
    ];

    if let Some(latest) = call.content.last() {
        rows.push(row(vec![
            text::<Msg>(" ").with_style(Style::dim()),
            paragraph::rich_paragraph::<Msg>(vec![TextSpan::new(
                truncate_preview(&summarize_tool_content(latest), 220),
                Style::dim(),
            )])
            .with_flex_grow(1.)
            .with_min_width(Dimension::ZERO),
        ]));
    }

    column(rows).with_gap(0, 0).with_style(Style::dim())
}

fn render_permission_prompt_view(prompt: &PermissionPrompt) -> Node<'_, Msg> {
    let tool_label = describe_tool_call(&prompt.request.tool_call);
    let options: Vec<Node<'_, Msg>> = if prompt.request.options.is_empty() {
        vec![text::<Msg>("No permission options provided").with_style(Style::dim())]
    } else {
        prompt
            .request
            .options
            .iter()
            .enumerate()
            .map(|(idx, option)| {
                let selected = idx == prompt.selected;
                let highlight = if selected {
                    Style::bg(Color::Palette(10))
                        .merged(&Style::fg(Color::Palette(0)))
                        .merged(&Style::bold())
                } else {
                    Style::default()
                };

                row(vec![
                    text::<Msg>(format!("{}.", idx + 1)).with_style(Style::dim()),
                    text::<Msg>(&option.name).with_style(if selected {
                        Style::bold()
                    } else {
                        Style::default()
                    }),
                    text::<Msg>(permission_kind_label(&option.kind)).with_style(Style::dim()),
                ])
                .with_gap(1, 0)
                .with_style(highlight)
            })
            .collect()
    };

    block_with_title(
        "Permission required",
        vec![
            paragraph::rich_paragraph::<Msg>(vec![TextSpan::new(
                format!("Tool wants to run: {tool_label}"),
                Style::default(),
            )]),
            column(options)
                .with_gap(0, 0)
                .with_min_width(Dimension::ZERO),
            text::<Msg>("Tab/Shift-Tab to pick, Enter to confirm, Esc to cancel")
                .with_style(Style::dim()),
        ],
    )
    .with_padding(1)
    .with_style(Style::bg(Color::Palette(8)))
}

fn permission_kind_label(kind: &PermissionOptionKind) -> &'static str {
    match kind {
        PermissionOptionKind::AllowOnce => "allow once",
        PermissionOptionKind::AllowAlways => "always allow",
        PermissionOptionKind::RejectOnce => "reject once",
        PermissionOptionKind::RejectAlways => "always reject",
        _ => "other",
    }
}

fn render_message(idx: usize, message: &ChatMessage) -> Node<'_, Msg> {
    let (label, mut label_style, msg_style) = match message.author {
        Author::User => (
            ">",
            Style::bg(Color::Palette(8)),
            Style::bg(Color::Palette(8)),
        ),
        Author::Agent => ("•", Style::default(), Style::default()),
        Author::System => ("•", Style::fg(Color::Palette(2)), Style::default()),
    };

    label_style = label_style.merged(&Style::bold());
    if message.author == Author::System {
        label_style = label_style.merged(&Style::dim());
    }

    let label_node = text::<Msg>(label).with_style(label_style);
    let content = if let Some(doc) = &message.markdown {
        markdown_view(
            &format!("msg-{idx}"),
            doc,
            &message.markdown_state,
            move |msg| Msg::Markdown(idx, msg),
        )
        .with_flex_grow(1.)
        .with_min_width(Dimension::ZERO)
    } else {
        paragraph::rich_paragraph::<Msg>(vec![TextSpan::new(&message.content, msg_style)])
            .with_flex_grow(1.)
            .with_min_width(Dimension::ZERO)
    };

    row(vec![label_node, content])
        .with_gap(1, 0)
        .with_style(msg_style)
}

fn render_thought(thought: &str) -> Node<'_, Msg> {
    row(vec![
        text::<Msg>("💭").with_style(Style::dim().merged(&Style::bold())),
        paragraph::rich_paragraph::<Msg>(vec![TextSpan::new(thought, Style::dim())])
            .with_flex_grow(1.)
            .with_min_width(Dimension::ZERO),
    ])
    .with_gap(1, 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::test_utils::render_node_to_string;
    use session_store::FakeIo;

    fn test_model() -> Model {
        let (command_tx, _) = channel::unbounded();
        let (_, event_rx) = channel::unbounded();
        let session_io = Arc::new(FakeIo::new());
        Model::new(event_rx, command_tx, "test-server".to_string(), session_io)
    }

    #[test]
    fn render_initial_state() {
        let model = test_model();
        let node = view(&model);
        let output = render_node_to_string(node, 80, 20).unwrap();
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_markdown_message() {
        let mut model = test_model();
        
        let content = "# Header\n\n- List item 1\n- List item 2\n\n```rust\nfn main() {}\n```";
        model.push_message(Author::Agent, content);
        
        if let Some(ChatEntry::Message(msg)) = model.timeline.last_mut() {
             let doc = MarkdownDocument::parse(&msg.content);
             msg.markdown_state.sync_with(&doc);
             msg.markdown = Some(doc);
        }

        let node = view(&model);
        let output = render_node_to_string(node, 80, 30).unwrap();
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_input_multiline() {
        let mut model = test_model();
        
        model.input.update(InputMsg::InsertText("Line 1".to_string()));
        model.input.update(InputMsg::InsertChar('\n'));
        model.input.update(InputMsg::InsertText("Line 2".to_string()));
        model.input.update(InputMsg::InsertChar('\n'));
        model.input.update(InputMsg::InsertText("Line 3".to_string()));

        let node = view(&model);
        let output = render_node_to_string(node, 80, 20).unwrap();
        insta::assert_snapshot!(output);
    }

    #[test]
    fn render_session_loader() {
        let (command_tx, _) = channel::unbounded();
        let (_, event_rx) = channel::unbounded();
        let session_io = Arc::new(FakeIo::new());
        
        let _ = session_io.save_session(SavedSession {
            id: "session-1".to_string(),
            date: OffsetDateTime::UNIX_EPOCH,
            prompt: "First session prompt".to_string(),
        });
        let _ = session_io.save_session(SavedSession {
            id: "session-2".to_string(),
            date: OffsetDateTime::UNIX_EPOCH + time::Duration::hours(1),
            prompt: "Second session prompt".to_string(),
        });

        let mut model = Model::new(event_rx, command_tx, "test-server".to_string(), session_io);

        let ctrl_l = Key::with_modifiers(KeyCode::Char('l'), true, false, false, false);
        // We use the public update function which calls handle_key internally
        update(&mut model, Msg::KeyPressed(ctrl_l));

        let node = view(&model);
        let output = render_node_to_string(node, 80, 20).unwrap();
        insta::assert_snapshot!(output);
    }
}