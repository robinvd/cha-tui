//! Terminal widget using alacritty_terminal for terminal emulation.
//!
//! This module provides a terminal widget that can be embedded in the TUI.
//! It uses alacritty_terminal for the actual terminal emulation.
//!
//! # Kitty Keyboard Protocol
//!
//! This module supports the kitty keyboard protocol for unambiguous key encoding.
//! Use [`key_to_input`] or [`default_terminal_keybindings`]
//! with the terminal's mode flags to enable this feature.
//!
//! TODOs
//! - Dont use 2 channels for EventListener

// Re-export TermMode for users of the kitty keyboard protocol functions
pub use alacritty_terminal::term::TermMode;

#[cfg(test)]
use once_cell::sync::Lazy;
use std::any::Any;
use std::borrow::Cow;
use std::collections::{HashMap, VecDeque, hash_map::DefaultHasher};
use std::hash::{Hash, Hasher};
use std::io;
#[cfg(unix)]
use std::os::fd::{AsRawFd, OwnedFd, RawFd};
use std::path::{Path, PathBuf};
use std::sync::Arc;
#[cfg(test)]
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, AtomicU16, AtomicU64, Ordering};
use std::thread::JoinHandle;

use super::process;
use alacritty_terminal::event::{Event as TermEvent, EventListener, WindowSize};
use alacritty_terminal::event_loop::{EventLoop, EventLoopSender, Msg, State as EventLoopState};
use alacritty_terminal::grid::Scroll;
use alacritty_terminal::sync::FairMutex;
use alacritty_terminal::term::color::{COUNT as COLOR_COUNT, Colors};
use alacritty_terminal::term::test::TermSize;
use alacritty_terminal::term::{self, Config as TermConfig, Term};
use alacritty_terminal::tty::{self, Options as PtyOptions, Shell};
use alacritty_terminal::vte::ansi::{self, CursorShape as AlacCursorShape, Handler, NamedColor};

use smol::channel::{self, Receiver, Sender};

use crate::buffer::{CellAttributes, CursorShape};
use crate::dom::{Node, Renderable, renderable};
use crate::event::{
    Key, KeyCode, KeyEventKind, LocalMouseEvent, MediaKeyCode, ModifierKeyCode, MouseButton,
    MouseEventKind, MouseScrollAxis, MouseScrollDirection,
};
use crate::palette::Rgba;
use crate::render::RenderContext;

#[cfg(unix)]
fn set_cloexec(fd: RawFd) -> io::Result<()> {
    // Prevent PTY master fds from leaking into newly spawned shells.
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
    if flags == -1 {
        return Err(io::Error::last_os_error());
    }

    if flags & libc::FD_CLOEXEC != 0 {
        return Ok(());
    }

    let result = unsafe { libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) };
    if result == -1 {
        return Err(io::Error::last_os_error());
    }

    Ok(())
}

#[cfg(test)]
struct TerminalTestPermit {
    lock: &'static Mutex<usize>,
    cvar: &'static Condvar,
}

#[cfg(test)]
impl TerminalTestPermit {
    fn acquire() -> Self {
        static PERMITS: Lazy<(Mutex<usize>, Condvar)> =
            Lazy::new(|| (Mutex::new(3), Condvar::new()));

        let (lock, cvar) = &*PERMITS;
        let mut available = lock.lock().expect("terminal test semaphore poisoned");
        while *available == 0 {
            available = cvar
                .wait(available)
                .expect("terminal test semaphore wait poisoned");
        }
        *available -= 1;

        Self { lock, cvar }
    }
}

#[cfg(test)]
impl Drop for TerminalTestPermit {
    fn drop(&mut self) {
        let mut available = self.lock.lock().expect("terminal test semaphore poisoned");
        *available += 1;
        self.cvar.notify_one();
    }
}

/// Messages for terminal state updates.
#[derive(Clone, Debug)]
pub enum TerminalMsg {
    /// Raw input bytes to write to the PTY
    Input(Vec<u8>),
    /// Paste text into the terminal
    Paste(String),
    /// Scroll by delta lines (positive = down, negative = up)
    Scroll(i32),
    /// Resize the terminal to new dimensions
    Resize { cols: u16, rows: u16 },
    /// Focus gained
    FocusGained,
    /// Focus lost
    FocusLost,
}

#[derive(Clone, Debug)]
pub struct TerminalNotification {
    pub title: Option<String>,
    pub body: String,
}

/// Event listener that tracks terminal content changes.
#[derive(Clone)]
struct TerminalEventListener {
    /// Version counter incremented on each wakeup event.
    version: Arc<AtomicU64>,
    /// Channel to notify when terminal content changes.
    wakeup_sender: Sender<()>,
    /// Channel carrying terminal events to the state.
    event_sender: Sender<TermEvent>,
    /// Marks that the terminal content may have changed.
    content_dirty: Arc<AtomicBool>,
}

impl TerminalEventListener {
    fn new(
        version: Arc<AtomicU64>,
        wakeup_sender: Sender<()>,
        event_sender: Sender<TermEvent>,
        content_dirty: Arc<AtomicBool>,
    ) -> Self {
        Self {
            version,
            wakeup_sender,
            event_sender,
            content_dirty,
        }
    }
}

impl EventListener for TerminalEventListener {
    fn send_event(&self, event: TermEvent) {
        if matches!(&event, TermEvent::Wakeup) {
            self.version.fetch_add(1, Ordering::Relaxed);
            self.content_dirty.store(true, Ordering::Relaxed);
            let _ = self.wakeup_sender.try_send(());
            return;
        }

        if let Err(err) = self.event_sender.try_send(event) {
            tracing::warn!(?err, "Failed to forward terminal event");
        } else {
            let _ = self.wakeup_sender.try_send(());
        }
    }
}

type EventLoopHandle = JoinHandle<(EventLoop<tty::Pty, TerminalEventListener>, EventLoopState)>;

/// State for the embedded terminal.
pub struct TerminalState {
    term: Arc<FairMutex<Term<TerminalEventListener>>>,
    sender: EventLoopSender,
    /// Content version counter, incremented on each terminal wakeup.
    version: Arc<AtomicU64>,
    /// Sender for wakeup notifications from the terminal.
    wakeup_sender: Sender<()>,
    /// Receiver for wakeup notifications from the terminal.
    wakeup_receiver: Receiver<()>,
    /// Shared terminal width in columns.
    cols_shared: Arc<AtomicU16>,
    /// Shared terminal height in rows.
    rows_shared: Arc<AtomicU16>,
    /// Shared exited flag.
    exited_shared: Arc<AtomicBool>,
    /// Shared window title reported by the shell/app.
    title: Arc<Mutex<Option<String>>>,
    /// Whether a bell has been emitted since the last check.
    bell: Arc<AtomicBool>,
    /// Pending OSC notification payloads from the terminal.
    notifications: Arc<Mutex<VecDeque<TerminalNotification>>>,
    /// Tracks changes to the visible terminal content (excluding cursor state).
    content_tracker: Mutex<ContentTracker>,
    /// Marks when content should be rehashed.
    content_dirty: Arc<AtomicBool>,
    /// Cloned PTY master fd for querying foreground process.
    #[cfg(unix)]
    pty_fd: OwnedFd,
    cols: u16,
    rows: u16,
    join_handle: Option<EventLoopHandle>,
    #[cfg(test)]
    _test_permit: TerminalTestPermit,
}

#[derive(Default)]
struct ContentTracker {
    hash: u64,
    version: u64,
    text_hash: u64,
    text_version: u64,
}

impl std::fmt::Debug for TerminalState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TerminalState")
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .finish_non_exhaustive()
    }
}

impl TerminalState {
    fn format_color_response(
        term: &Arc<FairMutex<Term<TerminalEventListener>>>,
        index: usize,
        formatter: &dyn Fn(ansi::Rgb) -> String,
    ) -> Option<String> {
        if index >= COLOR_COUNT {
            tracing::warn!(index, "Color request index out of bounds");
            return None;
        }

        let color = {
            let term = term.lock();
            term.colors()[index]
        };

        color.map(formatter)
    }

    #[allow(clippy::too_many_arguments)]
    fn handle_event_now(
        event: TermEvent,
        sender: &EventLoopSender,
        cols_shared: &Arc<AtomicU16>,
        rows_shared: &Arc<AtomicU16>,
        exited_shared: &Arc<AtomicBool>,
        title_shared: &Arc<Mutex<Option<String>>>,
        bell_shared: &Arc<AtomicBool>,
        notifications: &Arc<Mutex<VecDeque<TerminalNotification>>>,
        term: Option<&Arc<FairMutex<Term<TerminalEventListener>>>>,
        version: &Arc<AtomicU64>,
        wakeup_sender: &Sender<()>,
    ) {
        match event {
            TermEvent::TextAreaSizeRequest(formatter) => {
                let size = WindowSize {
                    num_cols: cols_shared.load(Ordering::Relaxed),
                    num_lines: rows_shared.load(Ordering::Relaxed),
                    cell_width: 1,
                    cell_height: 1,
                };
                let payload = formatter(size);
                let _ = sender.send(Msg::Input(Cow::Owned(payload.into_bytes())));
            }
            TermEvent::PtyWrite(text) => {
                let _ = sender.send(Msg::Input(Cow::Owned(text.into_bytes())));
            }
            TermEvent::Exit | TermEvent::ChildExit(_) => {
                exited_shared.store(true, Ordering::Relaxed);
            }
            TermEvent::Title(title) => {
                if let Ok(mut current) = title_shared.lock() {
                    *current = Some(title);
                }
            }
            TermEvent::ResetTitle => {
                if let Ok(mut current) = title_shared.lock() {
                    *current = None;
                }
            }
            TermEvent::Notification { title, body } => {
                if let Ok(mut queue) = notifications.lock() {
                    queue.push_back(TerminalNotification { title, body });
                }
                bell_shared.store(true, Ordering::Relaxed);
            }
            TermEvent::Bell => {
                bell_shared.store(true, Ordering::Relaxed);
            }
            TermEvent::ClipboardStore(_, _) | TermEvent::ClipboardLoad(_, _) => {}
            TermEvent::ColorRequest(index, formatter) => {
                if let Some(term) = term
                    && let Some(payload) =
                        Self::format_color_response(term, index, formatter.as_ref())
                {
                    let _ = sender.send(Msg::Input(Cow::Owned(payload.into_bytes())));
                }
            }
            TermEvent::MouseCursorDirty | TermEvent::CursorBlinkingChange => {
                version.fetch_add(1, Ordering::Relaxed);
                let _ = wakeup_sender.try_send(());
            }
            _ => {}
        }
    }

    /// Create a new terminal state with the default shell.
    pub fn new() -> std::io::Result<Self> {
        Self::with_shell(None)
    }

    /// Create a new terminal with a specific shell.
    pub fn with_shell(shell: Option<&str>) -> std::io::Result<Self> {
        let shell = shell.map(|s| Shell::new(s.to_string(), Vec::new()));
        Self::spawn_internal(shell, None, HashMap::new())
    }

    /// Create a new terminal state starting in a specific working directory.
    pub fn with_working_dir(path: impl AsRef<Path>) -> std::io::Result<Self> {
        Self::spawn_internal(None, Some(path.as_ref().to_path_buf()), HashMap::new())
    }

    /// Create a new terminal state with a working directory and extra environment variables.
    pub fn with_working_dir_and_env(
        path: impl AsRef<Path>,
        env: HashMap<String, String>,
    ) -> std::io::Result<Self> {
        Self::spawn_internal(None, Some(path.as_ref().to_path_buf()), env)
    }

    /// Create a new terminal state with a working directory, extra environment variables,
    /// and an optional shell command.
    pub fn with_working_dir_and_env_and_shell(
        path: impl AsRef<Path>,
        env: HashMap<String, String>,
        shell: Option<&str>,
    ) -> std::io::Result<Self> {
        let shell = shell.map(|command| Shell::new(command.to_string(), Vec::new()));
        Self::spawn_internal(shell, Some(path.as_ref().to_path_buf()), env)
    }

    /// Spawn a command in the terminal.
    pub fn spawn(command: &str, args: &[&str]) -> std::io::Result<Self> {
        let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
        let shell = Shell::new(command.to_string(), args);
        Self::spawn_internal(Some(shell), None, HashMap::new())
    }

    fn spawn_internal(
        shell: Option<Shell>,
        working_directory: Option<PathBuf>,
        env: HashMap<String, String>,
    ) -> std::io::Result<Self> {
        #[cfg(test)]
        let test_permit = TerminalTestPermit::acquire();

        let cols = 80u16;
        let rows = 24u16;

        let pty_config = PtyOptions {
            shell,
            working_directory,
            drain_on_exit: true,
            env,
        };

        let window_size = WindowSize {
            num_cols: cols,
            num_lines: rows,
            cell_width: 1,
            cell_height: 1,
        };

        // TODO? window_id not 0
        let pty = tty::new(&pty_config, window_size, 0)?;

        #[cfg(unix)]
        set_cloexec(pty.file().as_raw_fd())?;

        // Clone the PTY fd before moving it into EventLoop
        #[cfg(unix)]
        let pty_fd = pty.file().try_clone()?;

        let version = Arc::new(AtomicU64::new(0));
        let cols_shared = Arc::new(AtomicU16::new(cols));
        let rows_shared = Arc::new(AtomicU16::new(rows));
        let exited_shared = Arc::new(AtomicBool::new(false));
        let title_shared = Arc::new(Mutex::new(None));
        let notifications = Arc::new(Mutex::new(VecDeque::new()));
        let bell_shared = Arc::new(AtomicBool::new(false));
        let content_dirty = Arc::new(AtomicBool::new(true));
        let (wakeup_sender, wakeup_receiver) = channel::unbounded();
        let (event_sender, event_receiver) = channel::unbounded();
        let event_listener = TerminalEventListener::new(
            version.clone(),
            wakeup_sender.clone(),
            event_sender.clone(),
            content_dirty.clone(),
        );
        let config = TermConfig {
            kitty_keyboard: true,
            ..Default::default()
        };
        let term_size = TermSize::new(cols as usize, rows as usize);
        let term = Term::new(config, &term_size, event_listener.clone());
        let term = Arc::new(FairMutex::new(term));

        let event_loop = EventLoop::new(term.clone(), event_listener, pty, false, false)
            .map_err(|e| std::io::Error::other(format!("EventLoop error: {e:?}")))?;

        let sender = event_loop.channel();
        let join_handle = event_loop.spawn();

        let state = Self {
            term: term.clone(),
            sender: sender.clone(),
            version: version.clone(),
            wakeup_sender: wakeup_sender.clone(),
            wakeup_receiver,
            cols_shared: cols_shared.clone(),
            rows_shared: rows_shared.clone(),
            exited_shared: exited_shared.clone(),
            title: title_shared.clone(),
            bell: bell_shared.clone(),
            notifications: notifications.clone(),
            content_tracker: Mutex::new(ContentTracker::default()),
            content_dirty: content_dirty.clone(),
            #[cfg(unix)]
            pty_fd: pty_fd.into(),
            cols,
            rows,
            join_handle: Some(join_handle),
            #[cfg(test)]
            _test_permit: test_permit,
        };

        {
            let cols_shared = state.cols_shared.clone();
            let rows_shared = state.rows_shared.clone();
            let exited_shared = state.exited_shared.clone();
            let title_shared = state.title.clone();
            let bell_shared = state.bell.clone();
            let notifications = state.notifications.clone();
            // Use a weak reference to the terminal state to avoid a reference cycle
            // that would prevent the terminal from being dropped.
            let term_weak = Arc::downgrade(&state.term);
            let version = state.version.clone();
            let wakeup_sender = state.wakeup_sender.clone();
            let sender = state.sender.clone();
            smol::spawn(async move {
                while let Ok(event) = event_receiver.recv().await {
                    let term = term_weak.upgrade();
                    Self::handle_event_now(
                        event,
                        &sender,
                        &cols_shared,
                        &rows_shared,
                        &exited_shared,
                        &title_shared,
                        &bell_shared,
                        &notifications,
                        term.as_ref(),
                        &version,
                        &wakeup_sender,
                    );
                }
            })
            .detach();
        }

        Ok(state)
    }

    /// Update the terminal state based on a message.
    /// Returns true if the terminal needs to be re-rendered.
    pub fn update(&mut self, msg: TerminalMsg) -> bool {
        match msg {
            TerminalMsg::Input(data) => {
                self.write(&data);
                // Don't force a re-render here - the PTY hasn't processed the input yet.
                // The terminal's wakeup mechanism will trigger a re-render when output arrives.
                false
            }
            TerminalMsg::Paste(text) => {
                let payload = Self::format_paste_payload(&text, self.mode());
                self.write(&payload);
                false
            }
            TerminalMsg::Scroll(delta) => {
                let mut term = self.term.lock();
                let old_offset = term.grid().display_offset();
                // Negate delta: TerminalMsg convention is positive=down, negative=up
                // but alacritty's scroll_display uses positive=up (into history)
                term.scroll_display(Scroll::Delta(-delta));
                let new_offset = term.grid().display_offset();
                drop(term);
                old_offset != new_offset
            }
            TerminalMsg::Resize { cols, rows } => {
                self.resize(cols, rows);
                true
            }
            TerminalMsg::FocusGained => {
                if self.mode().contains(TermMode::FOCUS_IN_OUT) {
                    self.write(b"\x1b[I");
                }
                false
            }
            TerminalMsg::FocusLost => {
                if self.mode().contains(TermMode::FOCUS_IN_OUT) {
                    self.write(b"\x1b[O");
                }
                false
            }
        }
    }

    /// Write raw bytes to the terminal PTY.
    pub fn write(&mut self, data: &[u8]) {
        let _ = self.sender.send(Msg::Input(Cow::Owned(data.to_vec())));
    }

    fn format_paste_payload(text: &str, mode: TermMode) -> Vec<u8> {
        let mut payload = Vec::with_capacity(text.len() + 12);
        if mode.contains(TermMode::BRACKETED_PASTE) {
            payload.extend_from_slice(b"\x1b[200~");
            payload.extend_from_slice(text.as_bytes());
            payload.extend_from_slice(b"\x1b[201~");
        } else {
            payload.extend_from_slice(text.as_bytes());
        }
        payload
    }

    /// Resize the terminal to new dimensions.
    fn resize(&mut self, cols: u16, rows: u16) {
        if cols == self.cols && rows == self.rows {
            return;
        }
        if cols == 0 || rows == 0 {
            return;
        }

        self.cols = cols;
        self.rows = rows;
        self.cols_shared.store(cols, Ordering::Relaxed);
        self.rows_shared.store(rows, Ordering::Relaxed);

        let size = WindowSize {
            num_cols: cols,
            num_lines: rows,
            cell_width: 1,
            cell_height: 1,
        };

        let _ = self.sender.send(Msg::Resize(size));

        let mut term = self.term.lock();
        term.resize(TermSize::new(cols as usize, rows as usize));
    }

    /// Check if the terminal process has exited.
    pub fn is_running(&self) -> bool {
        !self.exited_shared.load(Ordering::Relaxed)
    }

    #[cfg(test)]
    fn handle_event_direct(&mut self, event: TermEvent) {
        Self::handle_event_now(
            event,
            &self.sender,
            &self.cols_shared,
            &self.rows_shared,
            &self.exited_shared,
            &self.title,
            &self.bell,
            &self.notifications,
            Some(&self.term),
            &self.version,
            &self.wakeup_sender,
        );
    }

    /// Get the current terminal dimensions.
    pub fn size(&self) -> (u16, u16) {
        (self.cols, self.rows)
    }

    /// Get the current content version.
    ///
    /// This value is incremented each time the terminal receives new content.
    pub fn version(&self) -> u64 {
        self.version.load(Ordering::Relaxed)
    }

    /// Get the version for visible content changes (excluding cursor updates).
    pub fn content_version(&self) -> u64 {
        self.content_versions().0
    }

    /// Get the version for visible text content changes (excluding colors and cursor updates).
    pub fn text_content_version(&self) -> u64 {
        self.content_versions().1
    }

    /// Get the versions for visible content changes (full, text-only).
    pub fn content_versions(&self) -> (u64, u64) {
        let mut tracker = self
            .content_tracker
            .lock()
            .expect("terminal content tracker poisoned");
        if self.content_dirty.swap(false, Ordering::Relaxed) {
            let hash = self.compute_content_hash();
            if tracker.hash != hash {
                tracker.hash = hash;
                tracker.version = tracker.version.wrapping_add(1);
            }

            let text_hash = self.compute_text_hash();
            if tracker.text_hash != text_hash {
                tracker.text_hash = text_hash;
                tracker.text_version = tracker.text_version.wrapping_add(1);
            }
        }
        (tracker.version, tracker.text_version)
    }

    fn compute_content_hash(&self) -> u64 {
        let term = self.term.lock();
        let content = term.renderable_content();
        let mut hasher = DefaultHasher::new();
        for cell in content.display_iter {
            cell.c.hash(&mut hasher);
            hash_ansi_color(cell.fg, &mut hasher);
            hash_ansi_color(cell.bg, &mut hasher);
            cell.flags.bits().hash(&mut hasher);
        }
        hasher.finish()
    }

    fn compute_text_hash(&self) -> u64 {
        let term = self.term.lock();
        let content = term.renderable_content();
        let mut hasher = DefaultHasher::new();
        for cell in content.display_iter {
            cell.c.hash(&mut hasher);
        }
        hasher.finish()
    }

    /// Returns a receiver that signals when the terminal content has changed.
    ///
    /// Use this to trigger re-renders when the terminal updates asynchronously.
    pub fn wakeup_receiver(&self) -> Receiver<()> {
        self.wakeup_receiver.clone()
    }

    #[doc(hidden)]
    pub fn test_trigger_wakeup(&self) {
        self.version.fetch_add(1, Ordering::Relaxed);
        let _ = self.wakeup_sender.try_send(());
        self.content_dirty.store(true, Ordering::Relaxed);
        if let Ok(mut tracker) = self.content_tracker.lock() {
            tracker.hash = tracker.hash.wrapping_add(1);
            tracker.text_hash = tracker.text_hash.wrapping_add(1);
        }
    }

    /// Get the current title reported by the terminal, if any.
    pub fn title(&self) -> Option<String> {
        self.title.lock().ok().and_then(|current| current.clone())
    }

    /// Get the name of the foreground process running in this terminal.
    ///
    /// This queries the PTY to determine which process is currently in the
    /// foreground (e.g., "vim", "cargo", "python").
    #[cfg(unix)]
    pub fn foreground_process_name(&self) -> Option<String> {
        process::foreground_process_name(self.pty_fd.as_raw_fd())
    }

    /// Get the name of the foreground process running in this terminal.
    #[cfg(not(unix))]
    pub fn foreground_process_name(&self) -> Option<String> {
        None
    }

    /// Returns true if the terminal has emitted a bell since the last check.
    pub fn take_bell(&self) -> bool {
        self.bell.swap(false, Ordering::Relaxed)
    }

    pub fn take_notifications(&self) -> Vec<TerminalNotification> {
        if let Ok(mut queue) = self.notifications.lock() {
            queue.drain(..).collect()
        } else {
            Vec::new()
        }
    }

    /// Get the current terminal mode flags.
    pub fn mode(&self) -> TermMode {
        let term = self.term.lock();
        *term.mode()
    }
}

fn hash_ansi_color<H: Hasher>(color: ansi::Color, hasher: &mut H) {
    match color {
        ansi::Color::Named(named) => {
            0u8.hash(hasher);
            (named as u8).hash(hasher);
        }
        ansi::Color::Spec(rgb) => {
            1u8.hash(hasher);
            rgb.r.hash(hasher);
            rgb.g.hash(hasher);
            rgb.b.hash(hasher);
        }
        ansi::Color::Indexed(idx) => {
            2u8.hash(hasher);
            idx.hash(hasher);
        }
    }
}

impl Drop for TerminalState {
    fn drop(&mut self) {
        let _ = self.sender.send(Msg::Shutdown);
        if let Some(handle) = self.join_handle.take() {
            let _ = handle.join();
        }
    }
}

impl Default for TerminalState {
    fn default() -> Self {
        Self::new().expect("Failed to create terminal")
    }
}

/// Create a terminal widget node.
pub fn terminal<Msg>(
    id: &'static str,
    state: &TerminalState,
    focused: bool,
    on_event: impl Fn(TerminalMsg) -> Msg + 'static,
) -> Node<Msg>
where
    Msg: 'static,
{
    use std::rc::Rc;

    let renderable_widget = TerminalRenderable::new(state, focused);
    let mut node = renderable::<Msg>(renderable_widget).with_id(id);

    let handler = Rc::new(on_event);
    let mouse_handler = handler.clone();
    let resize_handler = handler.clone();
    let current_size = state.size();
    let term = state.term.clone();
    node = node.on_mouse(move |event: LocalMouseEvent| {
        // Get current mode dynamically - applications can enable/disable mouse mode
        let current_mode = *term.lock().mode();

        match event.event.kind {
            MouseEventKind::Scroll(scroll) => {
                if current_mode.intersects(TermMode::MOUSE_MODE) {
                    let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                    if let Some(bytes) = encode_mouse_event(&event, true, sgr, false) {
                        return Some(mouse_handler(TerminalMsg::Input(bytes)));
                    }
                }

                if scroll.axis == MouseScrollAxis::Vertical {
                    let delta = if scroll.direction == MouseScrollDirection::Positive {
                        -3
                    } else {
                        3
                    };
                    return Some(mouse_handler(TerminalMsg::Scroll(delta)));
                }

                None
            }
            MouseEventKind::Down(_) | MouseEventKind::Drag(_) | MouseEventKind::Up(_)
                if current_mode.intersects(TermMode::MOUSE_MODE) =>
            {
                let pressed = !matches!(event.event.kind, MouseEventKind::Up(_));
                let is_drag = matches!(event.event.kind, MouseEventKind::Drag(_));
                let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                if let Some(bytes) = encode_mouse_event(&event, pressed, sgr, is_drag) {
                    return Some(mouse_handler(TerminalMsg::Input(bytes)));
                }
                None
            }
            MouseEventKind::Move
                if current_mode.intersects(TermMode::MOUSE_MODE)
                    && current_mode.contains(TermMode::MOUSE_MOTION) =>
            {
                let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                if let Some(bytes) = encode_mouse_event(&event, true, sgr, true) {
                    return Some(mouse_handler(TerminalMsg::Input(bytes)));
                }
                None
            }
            _ => None,
        }
    });

    node = node.on_resize(move |layout| {
        let cols = layout.size.width.floor() as u16;
        let rows = layout.size.height.floor() as u16;
        if cols != current_size.0 || rows != current_size.1 {
            Some(resize_handler(TerminalMsg::Resize { cols, rows }))
        } else {
            None
        }
    });

    node
}

/// Legacy key to input encoding (internal use only).
/// Used as fallback when kitty protocol is not enabled.
fn legacy_key_to_input(key: Key, app_cursor: bool) -> Option<Vec<u8>> {
    let modifier_code = |key: &Key| {
        let mut code = 1;
        if key.shift {
            code += 1;
        }
        if key.alt {
            code += 2;
        }
        if key.ctrl {
            code += 4;
        }
        (code != 1).then_some(code)
    };

    let bytes = match key.code {
        KeyCode::Char(c) => {
            if key.ctrl {
                // Ctrl+letter produces control codes
                if c.is_ascii_alphabetic() {
                    let ctrl_code = (c.to_ascii_lowercase() as u8) - b'a' + 1;
                    vec![ctrl_code]
                } else {
                    return None;
                }
            } else if key.alt {
                // Alt+letter produces ESC followed by the letter
                let mut bytes = vec![0x1b];
                let mut buf = [0u8; 4];
                let s = c.encode_utf8(&mut buf);
                bytes.extend_from_slice(s.as_bytes());
                bytes
            } else {
                let mut buf = [0u8; 4];
                let s = c.encode_utf8(&mut buf);
                s.as_bytes().to_vec()
            }
        }
        KeyCode::Enter => vec![b'\r'],
        KeyCode::Backspace => vec![0x7f],
        KeyCode::Tab => {
            if key.shift {
                vec![0x1b, b'[', b'Z'] // Shift+Tab
            } else {
                vec![b'\t']
            }
        }
        KeyCode::Esc => vec![0x1b],
        KeyCode::Up => {
            if key.ctrl {
                vec![0x1b, b'[', b'1', b';', b'5', b'A']
            } else if key.alt {
                vec![0x1b, b'[', b'1', b';', b'3', b'A']
            } else if key.shift {
                vec![0x1b, b'[', b'1', b';', b'2', b'A']
            } else if app_cursor {
                vec![0x1b, b'O', b'A']
            } else {
                vec![0x1b, b'[', b'A']
            }
        }
        KeyCode::Down => {
            if key.ctrl {
                vec![0x1b, b'[', b'1', b';', b'5', b'B']
            } else if key.alt {
                vec![0x1b, b'[', b'1', b';', b'3', b'B']
            } else if key.shift {
                vec![0x1b, b'[', b'1', b';', b'2', b'B']
            } else if app_cursor {
                vec![0x1b, b'O', b'B']
            } else {
                vec![0x1b, b'[', b'B']
            }
        }
        KeyCode::Right => {
            if key.ctrl {
                vec![0x1b, b'[', b'1', b';', b'5', b'C']
            } else if key.alt {
                vec![0x1b, b'[', b'1', b';', b'3', b'C']
            } else if key.shift {
                vec![0x1b, b'[', b'1', b';', b'2', b'C']
            } else if app_cursor {
                vec![0x1b, b'O', b'C']
            } else {
                vec![0x1b, b'[', b'C']
            }
        }
        KeyCode::Left => {
            if key.ctrl {
                vec![0x1b, b'[', b'1', b';', b'5', b'D']
            } else if key.alt {
                vec![0x1b, b'[', b'1', b';', b'3', b'D']
            } else if key.shift {
                vec![0x1b, b'[', b'1', b';', b'2', b'D']
            } else if app_cursor {
                vec![0x1b, b'O', b'D']
            } else {
                vec![0x1b, b'[', b'D']
            }
        }
        KeyCode::Home => {
            if key.ctrl {
                vec![0x1b, b'[', b'1', b';', b'5', b'H']
            } else {
                vec![0x1b, b'[', b'H']
            }
        }
        KeyCode::End => {
            if key.ctrl {
                vec![0x1b, b'[', b'1', b';', b'5', b'F']
            } else {
                vec![0x1b, b'[', b'F']
            }
        }
        KeyCode::PageUp => vec![0x1b, b'[', b'5', b'~'],
        KeyCode::PageDown => vec![0x1b, b'[', b'6', b'~'],
        KeyCode::Function(n) => {
            let modifier = modifier_code(&key);
            match n {
                1 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b';', modifier + b'0', b'P'],
                    None => vec![0x1b, b'O', b'P'],
                },
                2 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b';', modifier + b'0', b'Q'],
                    None => vec![0x1b, b'O', b'Q'],
                },
                3 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b';', modifier + b'0', b'R'],
                    None => vec![0x1b, b'O', b'R'],
                },
                4 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b';', modifier + b'0', b'S'],
                    None => vec![0x1b, b'O', b'S'],
                },
                5 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b'5', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'1', b'5', b'~'],
                },
                6 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b'7', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'1', b'7', b'~'],
                },
                7 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b'8', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'1', b'8', b'~'],
                },
                8 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'1', b'9', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'1', b'9', b'~'],
                },
                9 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'2', b'0', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'2', b'0', b'~'],
                },
                10 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'2', b'1', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'2', b'1', b'~'],
                },
                11 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'2', b'3', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'2', b'3', b'~'],
                },
                12 => match modifier {
                    Some(modifier) => vec![0x1b, b'[', b'2', b'4', b';', modifier + b'0', b'~'],
                    None => vec![0x1b, b'[', b'2', b'4', b'~'],
                },
                _ => return None,
            }
        }
        KeyCode::Insert => {
            let modifier = modifier_code(&key);
            match modifier {
                Some(m) => vec![0x1b, b'[', b'2', b';', m + b'0', b'~'],
                None => vec![0x1b, b'[', b'2', b'~'],
            }
        }
        KeyCode::Delete => {
            let modifier = modifier_code(&key);
            match modifier {
                Some(m) => vec![0x1b, b'[', b'3', b';', m + b'0', b'~'],
                None => vec![0x1b, b'[', b'3', b'~'],
            }
        }
        // These keys don't produce output in legacy mode
        KeyCode::CapsLock
        | KeyCode::ScrollLock
        | KeyCode::NumLock
        | KeyCode::PrintScreen
        | KeyCode::Pause
        | KeyCode::Menu
        | KeyCode::Modifier(_)
        | KeyCode::Media(_) => return None,
    };

    Some(bytes)
}

// ============================================================================
// Kitty Keyboard Protocol Encoding
// ============================================================================
//
// The Kitty Keyboard Protocol provides unambiguous key encoding. See:
// https://sw.kovidgoyal.net/kitty/keyboard-protocol/
//
// Key codes for special keys in the protocol:
// - Control characters: Tab=9, Enter=13, Escape=27, Space=32, Backspace=127
// - Numpad keys: 57399-57426
// - Function keys F13-F35: 57376-57398
// - Modifier keys: 57441-57452
// - Media keys: 57428-57440
// - Misc: ScrollLock=57359, PrintScreen=57361, Pause=57362, Menu=57363

/// Kitty keyboard protocol key codes for numpad keys.
mod kitty_codes {
    // Numpad keys
    pub const NUMPAD_0: u32 = 57399;
    pub const NUMPAD_1: u32 = 57400;
    pub const NUMPAD_2: u32 = 57401;
    pub const NUMPAD_3: u32 = 57402;
    pub const NUMPAD_4: u32 = 57403;
    pub const NUMPAD_5: u32 = 57404;
    pub const NUMPAD_6: u32 = 57405;
    pub const NUMPAD_7: u32 = 57406;
    pub const NUMPAD_8: u32 = 57407;
    pub const NUMPAD_9: u32 = 57408;
    pub const NUMPAD_DECIMAL: u32 = 57409;
    pub const NUMPAD_DIVIDE: u32 = 57410;
    pub const NUMPAD_MULTIPLY: u32 = 57411;
    pub const NUMPAD_SUBTRACT: u32 = 57412;
    pub const NUMPAD_ADD: u32 = 57413;
    pub const NUMPAD_ENTER: u32 = 57414;
    pub const NUMPAD_EQUAL: u32 = 57415;
    // Numpad navigation (57417-57426) - mapped when keypad flag is set
    pub const NUMPAD_LEFT: u32 = 57417;
    pub const NUMPAD_RIGHT: u32 = 57418;
    pub const NUMPAD_UP: u32 = 57419;
    pub const NUMPAD_DOWN: u32 = 57420;
    pub const NUMPAD_PAGE_UP: u32 = 57421;
    pub const NUMPAD_PAGE_DOWN: u32 = 57422;
    pub const NUMPAD_HOME: u32 = 57423;
    pub const NUMPAD_END: u32 = 57424;
    pub const NUMPAD_INSERT: u32 = 57425;
    pub const NUMPAD_DELETE: u32 = 57426;

    // Function keys F13-F35
    pub const F13: u32 = 57376;
    pub const F14: u32 = 57377;
    pub const F15: u32 = 57378;
    pub const F16: u32 = 57379;
    pub const F17: u32 = 57380;
    pub const F18: u32 = 57381;
    pub const F19: u32 = 57382;
    pub const F20: u32 = 57383;
    pub const F21: u32 = 57384;
    pub const F22: u32 = 57385;
    pub const F23: u32 = 57386;
    pub const F24: u32 = 57387;
    pub const F25: u32 = 57388;
    pub const F26: u32 = 57389;
    pub const F27: u32 = 57390;
    pub const F28: u32 = 57391;
    pub const F29: u32 = 57392;
    pub const F30: u32 = 57393;
    pub const F31: u32 = 57394;
    pub const F32: u32 = 57395;
    pub const F33: u32 = 57396;
    pub const F34: u32 = 57397;
    pub const F35: u32 = 57398;

    // Modifier keys
    pub const LEFT_SHIFT: u32 = 57441;
    pub const LEFT_CONTROL: u32 = 57442;
    pub const LEFT_ALT: u32 = 57443;
    pub const LEFT_SUPER: u32 = 57444;
    pub const LEFT_HYPER: u32 = 57445;
    pub const LEFT_META: u32 = 57446;
    pub const RIGHT_SHIFT: u32 = 57447;
    pub const RIGHT_CONTROL: u32 = 57448;
    pub const RIGHT_ALT: u32 = 57449;
    pub const RIGHT_SUPER: u32 = 57450;
    pub const RIGHT_HYPER: u32 = 57451;
    pub const RIGHT_META: u32 = 57452;

    // Media keys
    pub const MEDIA_PLAY: u32 = 57428;
    pub const MEDIA_PAUSE: u32 = 57429;
    pub const MEDIA_PLAY_PAUSE: u32 = 57430;
    pub const MEDIA_STOP: u32 = 57432;
    pub const MEDIA_FAST_FORWARD: u32 = 57433;
    pub const MEDIA_REWIND: u32 = 57434;
    pub const MEDIA_TRACK_NEXT: u32 = 57435;
    pub const MEDIA_TRACK_PREVIOUS: u32 = 57436;
    pub const MEDIA_RECORD: u32 = 57437;
    pub const VOLUME_DOWN: u32 = 57438;
    pub const VOLUME_UP: u32 = 57439;
    pub const VOLUME_MUTE: u32 = 57440;

    // Misc keys
    pub const SCROLL_LOCK: u32 = 57359;
    pub const PRINT_SCREEN: u32 = 57361;
    pub const PAUSE: u32 = 57362;
    pub const MENU: u32 = 57363;
    pub const CAPS_LOCK: u32 = 57358;
    pub const NUM_LOCK: u32 = 57360;

    // Control characters (these are just unicode codepoints)
    pub const TAB: u32 = 9;
    pub const ENTER: u32 = 13;
    pub const ESCAPE: u32 = 27;
    pub const BACKSPACE: u32 = 127;
}

/// Encode modifiers for kitty keyboard protocol.
/// Returns modifiers+1 as per the protocol spec.
fn encode_kitty_modifiers(key: &Key) -> u8 {
    let mut mods: u8 = 0;
    if key.shift {
        mods |= 1;
    }
    if key.alt {
        mods |= 2;
    }
    if key.ctrl {
        mods |= 4;
    }
    if key.super_key {
        mods |= 8;
    }
    if key.hyper {
        mods |= 16;
    }
    if key.meta {
        mods |= 32;
    }
    mods + 1 // Protocol requires modifiers+1
}

/// Translate a key event to terminal input bytes, respecting the terminal's mode flags.
///
/// This function implements the kitty keyboard protocol when the terminal has
/// enabled the appropriate mode flags. It falls back to legacy encoding when
/// kitty protocol is not enabled.
///
/// # Arguments
/// * `key` - The key event to encode
/// * `mode` - The terminal mode flags indicating which protocol features are enabled
///
/// # Returns
/// * `Some(Vec<u8>)` - The encoded escape sequence
/// * `None` - If the key should not produce output
pub fn key_to_input(key: Key, mode: TermMode) -> Option<Vec<u8>> {
    let kitty_seq = mode.intersects(
        TermMode::REPORT_ALL_KEYS_AS_ESC
            | TermMode::DISAMBIGUATE_ESC_CODES
            | TermMode::REPORT_EVENT_TYPES,
    );
    let app_cursor = mode.contains(TermMode::APP_CURSOR);

    // If kitty protocol is not enabled, use legacy encoding
    // But only for key press events - releases should be ignored in legacy mode
    if !kitty_seq {
        if key.kind != KeyEventKind::Press {
            return None;
        }
        return legacy_key_to_input(key, app_cursor);
    }

    let kitty_encode_all = mode.contains(TermMode::REPORT_ALL_KEYS_AS_ESC);
    let kitty_event_type = mode.contains(TermMode::REPORT_EVENT_TYPES)
        && (key.kind == KeyEventKind::Repeat || key.kind == KeyEventKind::Release);

    // Determine if we should use kitty encoding for this specific key
    let should_use_kitty = should_build_kitty_sequence(&key, mode);

    if !should_use_kitty && !kitty_encode_all {
        // Use legacy encoding for simple key presses
        if key.kind != KeyEventKind::Press {
            return None;
        }
        return legacy_key_to_input(key, app_cursor);
    }

    // Build the kitty sequence
    build_kitty_sequence(&key, kitty_encode_all, kitty_event_type)
}

/// Determine if we should build a kitty escape sequence for this key.
fn should_build_kitty_sequence(key: &Key, mode: TermMode) -> bool {
    if mode.contains(TermMode::REPORT_ALL_KEYS_AS_ESC) {
        return true;
    }

    let disambiguate = mode.contains(TermMode::DISAMBIGUATE_ESC_CODES);
    if !disambiguate {
        return false;
    }

    // Keys that need disambiguation
    match key.code {
        KeyCode::Esc => true,
        // Numpad keys need disambiguation
        _ if key.keypad => true,
        // Modified keys (except plain shift on text keys)
        KeyCode::Char(_) => key.ctrl || key.alt || key.super_key || key.hyper || key.meta,
        // Special keys that might be ambiguous
        KeyCode::Tab | KeyCode::Enter | KeyCode::Backspace if key.shift || key.ctrl || key.alt => {
            true
        }
        // Function keys F13+ always need kitty encoding, F1-F12 only with modifiers
        KeyCode::Function(n) if n > 12 => true,
        KeyCode::Function(_)
        | KeyCode::Up
        | KeyCode::Down
        | KeyCode::Left
        | KeyCode::Right
        | KeyCode::Home
        | KeyCode::End
        | KeyCode::PageUp
        | KeyCode::PageDown
        | KeyCode::Insert
        | KeyCode::Delete => key.shift || key.ctrl || key.alt || key.super_key,
        // Modifier keys themselves
        KeyCode::Modifier(_) => true,
        // Media keys
        KeyCode::Media(_) => true,
        // Lock keys
        KeyCode::CapsLock | KeyCode::ScrollLock | KeyCode::NumLock => true,
        // Other special keys
        KeyCode::PrintScreen | KeyCode::Pause | KeyCode::Menu => true,
        _ => false,
    }
}

/// Build a kitty keyboard protocol escape sequence.
fn build_kitty_sequence(key: &Key, encode_all: bool, include_event_type: bool) -> Option<Vec<u8>> {
    // Get the key code (unicode codepoint or kitty functional key code)
    let (key_code, terminator) = get_kitty_key_code(key)?;

    let modifiers = encode_kitty_modifiers(key);
    let has_modifiers = modifiers > 1;

    // For simple cases without modifiers or event types, we can use shorter sequences
    if !has_modifiers && !include_event_type && !encode_all {
        // For functional keys, still use CSI sequences
        if terminator == 'u' || matches!(key.code, KeyCode::Function(_)) {
            // Simple CSI sequence without modifiers
            return Some(format!("\x1b[{key_code}{terminator}").into_bytes());
        }
    }

    let mut payload = format!("\x1b[{key_code}");

    // Add modifiers if present or if we need to add event type
    if has_modifiers || include_event_type {
        payload.push_str(&format!(";{modifiers}"));
    }

    // Add event type if reporting is enabled
    if include_event_type {
        payload.push(':');
        let event_type = match key.kind {
            KeyEventKind::Repeat => '2',
            KeyEventKind::Press => '1',
            KeyEventKind::Release => '3',
        };
        payload.push(event_type);
    }

    payload.push(terminator);

    Some(payload.into_bytes())
}

/// Get the kitty key code and terminator for a key.
/// Returns (code, terminator) where terminator is 'u' for kitty or a legacy terminator.
fn get_kitty_key_code(key: &Key) -> Option<(u32, char)> {
    use kitty_codes::*;

    // Handle numpad keys when keypad flag is set
    if key.keypad {
        let code = match key.code {
            KeyCode::Char('0') => NUMPAD_0,
            KeyCode::Char('1') => NUMPAD_1,
            KeyCode::Char('2') => NUMPAD_2,
            KeyCode::Char('3') => NUMPAD_3,
            KeyCode::Char('4') => NUMPAD_4,
            KeyCode::Char('5') => NUMPAD_5,
            KeyCode::Char('6') => NUMPAD_6,
            KeyCode::Char('7') => NUMPAD_7,
            KeyCode::Char('8') => NUMPAD_8,
            KeyCode::Char('9') => NUMPAD_9,
            KeyCode::Char('.') => NUMPAD_DECIMAL,
            KeyCode::Char('/') => NUMPAD_DIVIDE,
            KeyCode::Char('*') => NUMPAD_MULTIPLY,
            KeyCode::Char('-') => NUMPAD_SUBTRACT,
            KeyCode::Char('+') => NUMPAD_ADD,
            KeyCode::Char('=') => NUMPAD_EQUAL,
            KeyCode::Enter => NUMPAD_ENTER,
            KeyCode::Left => NUMPAD_LEFT,
            KeyCode::Right => NUMPAD_RIGHT,
            KeyCode::Up => NUMPAD_UP,
            KeyCode::Down => NUMPAD_DOWN,
            KeyCode::PageUp => NUMPAD_PAGE_UP,
            KeyCode::PageDown => NUMPAD_PAGE_DOWN,
            KeyCode::Home => NUMPAD_HOME,
            KeyCode::End => NUMPAD_END,
            KeyCode::Insert => NUMPAD_INSERT,
            KeyCode::Delete => NUMPAD_DELETE,
            _ => return get_kitty_key_code_non_numpad(key),
        };
        return Some((code, 'u'));
    }

    get_kitty_key_code_non_numpad(key)
}

/// Get kitty key code for non-numpad keys.
fn get_kitty_key_code_non_numpad(key: &Key) -> Option<(u32, char)> {
    use kitty_codes::*;

    match key.code {
        // Character keys use their unicode codepoint
        KeyCode::Char(c) => {
            let code = if key.shift {
                // For shifted characters, use the lowercase version as the base
                u32::from(c.to_lowercase().next().unwrap_or(c))
            } else {
                u32::from(c)
            };
            Some((code, 'u'))
        }

        // Control characters
        KeyCode::Tab => Some((TAB, 'u')),
        KeyCode::Enter => Some((ENTER, 'u')),
        KeyCode::Esc => Some((ESCAPE, 'u')),
        KeyCode::Backspace => Some((BACKSPACE, 'u')),

        // Navigation keys - use legacy terminators for compatibility
        KeyCode::Up => Some((1, 'A')),
        KeyCode::Down => Some((1, 'B')),
        KeyCode::Right => Some((1, 'C')),
        KeyCode::Left => Some((1, 'D')),
        KeyCode::Home => Some((1, 'H')),
        KeyCode::End => Some((1, 'F')),
        KeyCode::PageUp => Some((5, '~')),
        KeyCode::PageDown => Some((6, '~')),
        KeyCode::Insert => Some((2, '~')),
        KeyCode::Delete => Some((3, '~')),

        // Function keys F1-F12 use legacy encoding
        KeyCode::Function(n) => match n {
            1 => Some((1, 'P')),
            2 => Some((1, 'Q')),
            3 => Some((13, '~')), // F3 in kitty differs from terminfo
            4 => Some((1, 'S')),
            5 => Some((15, '~')),
            6 => Some((17, '~')),
            7 => Some((18, '~')),
            8 => Some((19, '~')),
            9 => Some((20, '~')),
            10 => Some((21, '~')),
            11 => Some((23, '~')),
            12 => Some((24, '~')),
            // F13-F35 use kitty encoding
            13 => Some((F13, 'u')),
            14 => Some((F14, 'u')),
            15 => Some((F15, 'u')),
            16 => Some((F16, 'u')),
            17 => Some((F17, 'u')),
            18 => Some((F18, 'u')),
            19 => Some((F19, 'u')),
            20 => Some((F20, 'u')),
            21 => Some((F21, 'u')),
            22 => Some((F22, 'u')),
            23 => Some((F23, 'u')),
            24 => Some((F24, 'u')),
            25 => Some((F25, 'u')),
            26 => Some((F26, 'u')),
            27 => Some((F27, 'u')),
            28 => Some((F28, 'u')),
            29 => Some((F29, 'u')),
            30 => Some((F30, 'u')),
            31 => Some((F31, 'u')),
            32 => Some((F32, 'u')),
            33 => Some((F33, 'u')),
            34 => Some((F34, 'u')),
            35 => Some((F35, 'u')),
            _ => None,
        },

        // Modifier keys
        KeyCode::Modifier(m) => {
            let code = match m {
                ModifierKeyCode::LeftShift => LEFT_SHIFT,
                ModifierKeyCode::LeftControl => LEFT_CONTROL,
                ModifierKeyCode::LeftAlt => LEFT_ALT,
                ModifierKeyCode::LeftSuper => LEFT_SUPER,
                ModifierKeyCode::LeftHyper => LEFT_HYPER,
                ModifierKeyCode::LeftMeta => LEFT_META,
                ModifierKeyCode::RightShift => RIGHT_SHIFT,
                ModifierKeyCode::RightControl => RIGHT_CONTROL,
                ModifierKeyCode::RightAlt => RIGHT_ALT,
                ModifierKeyCode::RightSuper => RIGHT_SUPER,
                ModifierKeyCode::RightHyper => RIGHT_HYPER,
                ModifierKeyCode::RightMeta => RIGHT_META,
            };
            Some((code, 'u'))
        }

        // Media keys
        KeyCode::Media(m) => {
            let code = match m {
                MediaKeyCode::Play => MEDIA_PLAY,
                MediaKeyCode::Pause => MEDIA_PAUSE,
                MediaKeyCode::PlayPause => MEDIA_PLAY_PAUSE,
                MediaKeyCode::Stop => MEDIA_STOP,
                MediaKeyCode::FastForward => MEDIA_FAST_FORWARD,
                MediaKeyCode::Rewind => MEDIA_REWIND,
                MediaKeyCode::TrackNext => MEDIA_TRACK_NEXT,
                MediaKeyCode::TrackPrevious => MEDIA_TRACK_PREVIOUS,
                MediaKeyCode::Record => MEDIA_RECORD,
                MediaKeyCode::LowerVolume => VOLUME_DOWN,
                MediaKeyCode::RaiseVolume => VOLUME_UP,
                MediaKeyCode::MuteVolume => VOLUME_MUTE,
            };
            Some((code, 'u'))
        }

        // Lock keys
        KeyCode::CapsLock => Some((CAPS_LOCK, 'u')),
        KeyCode::ScrollLock => Some((SCROLL_LOCK, 'u')),
        KeyCode::NumLock => Some((NUM_LOCK, 'u')),

        // Other special keys
        KeyCode::PrintScreen => Some((PRINT_SCREEN, 'u')),
        KeyCode::Pause => Some((PAUSE, 'u')),
        KeyCode::Menu => Some((MENU, 'u')),
    }
}

/// Encode a mouse event as terminal escape sequences.
///
/// Uses SGR mouse encoding format: `\x1b[<Cb;Cx;CyM` (press) or `\x1b[<Cb;Cx;Cym` (release)
/// where Cb is the button code with modifiers, and Cx/Cy are 1-based coordinates.
pub fn encode_mouse_event(
    event: &LocalMouseEvent,
    pressed: bool,
    sgr_mode: bool,
    motion: bool,
) -> Option<Vec<u8>> {
    // Determine button code
    let button = match event.event.kind {
        MouseEventKind::Down(button)
        | MouseEventKind::Up(button)
        | MouseEventKind::Drag(button) => match button {
            MouseButton::Left => 0,
            MouseButton::Middle => 1,
            MouseButton::Right => 2,
        },
        MouseEventKind::Scroll(scroll) => match (scroll.axis, scroll.direction) {
            (MouseScrollAxis::Vertical, MouseScrollDirection::Positive) => 64, // scroll up
            (MouseScrollAxis::Vertical, MouseScrollDirection::Negative) => 65, // scroll down
            (MouseScrollAxis::Horizontal, MouseScrollDirection::Negative) => 66, // scroll left
            (MouseScrollAxis::Horizontal, MouseScrollDirection::Positive) => 67, // scroll right
        },
        MouseEventKind::Move if motion => 3,
        _ => return None,
    };

    // Add modifier flags
    let mut cb = button;
    if event.event.modifiers.shift {
        cb |= 4;
    }
    if event.event.modifiers.alt {
        cb |= 8;
    }
    if event.event.modifiers.ctrl {
        cb |= 16;
    }
    if motion {
        cb |= 32;
    }

    // Convert to 1-based coordinates
    let cx = event.local_position.x.saturating_add(1);
    let cy = event.local_position.y.saturating_add(1);

    if sgr_mode {
        // SGR format: \x1b[<Cb;Cx;CyM or \x1b[<Cb;Cx;Cym
        let suffix = if pressed { 'M' } else { 'm' };
        Some(format!("\x1b[<{};{};{}{}", cb, cx, cy, suffix).into_bytes())
    } else {
        // Legacy X10/normal format: \x1b[M CbCxCy (encoded as raw bytes + 32)
        // Only supports press events and coordinates up to 223
        if !pressed || cx > 223 || cy > 223 {
            return None;
        }
        Some(vec![
            0x1b,
            b'[',
            b'M',
            (cb + 32) as u8,
            (cx + 32) as u8,
            (cy + 32) as u8,
        ])
    }
}

/// Default key bindings for terminal input with kitty keyboard protocol support.
///
/// This function respects the terminal's mode flags to determine whether to use
/// kitty keyboard protocol encoding or legacy encoding.
///
/// # Arguments
/// * `key` - The key event to handle
/// * `mode` - The terminal mode flags (from `TerminalState::mode()`)
/// * `on_input` - Function to convert terminal messages to application messages
pub fn default_terminal_keybindings<Msg>(
    key: Key,
    mode: TermMode,
    on_input: impl Fn(TerminalMsg) -> Msg,
) -> Option<Msg> {
    key_to_input(key, mode).map(|bytes| on_input(TerminalMsg::Input(bytes)))
}

/// Renderable widget for the terminal.
struct TerminalRenderable {
    term: Arc<FairMutex<Term<TerminalEventListener>>>,
    cols: u16,
    rows: u16,
    /// Content version at the time this renderable was created.
    version: u64,
    /// Whether the terminal is currently focused.
    focused: bool,
}

impl std::fmt::Debug for TerminalRenderable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TerminalRenderable")
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .field("version", &self.version)
            .field("focused", &self.focused)
            .finish_non_exhaustive()
    }
}

impl TerminalRenderable {
    fn new(state: &TerminalState, focused: bool) -> Self {
        Self {
            term: state.term.clone(),
            cols: state.cols,
            rows: state.rows,
            version: state.version(),
            focused,
        }
    }

    fn convert_color(color: ansi::Color, colors: &Colors) -> Option<Rgba> {
        match color {
            ansi::Color::Named(named) => {
                let rgb = colors[named]?;
                Some(Rgba::opaque(rgb.r, rgb.g, rgb.b))
            }
            ansi::Color::Spec(rgb) => Some(Rgba::opaque(rgb.r, rgb.g, rgb.b)),
            ansi::Color::Indexed(idx) => {
                let rgb = colors[idx as usize]?;
                Some(Rgba::opaque(rgb.r, rgb.g, rgb.b))
            }
        }
    }

    fn convert_cursor_shape(shape: AlacCursorShape) -> CursorShape {
        match shape {
            AlacCursorShape::Block => CursorShape::SteadyBlock,
            AlacCursorShape::Underline => CursorShape::SteadyUnderline,
            AlacCursorShape::Beam => CursorShape::SteadyBar,
            AlacCursorShape::Hidden => CursorShape::SteadyBlock,
            _ => CursorShape::SteadyBlock,
        }
    }
}

impl Renderable for TerminalRenderable {
    fn eq(&self, other: &dyn Renderable) -> bool {
        let Some(o) = other.as_any().downcast_ref::<Self>() else {
            return false;
        };
        Arc::ptr_eq(&self.term, &o.term)
            && self.cols == o.cols
            && self.rows == o.rows
            && self.version == o.version
            && self.focused == o.focused
    }

    fn measure(
        &self,
        _style: &taffy::Style,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
    ) -> taffy::Size<f32> {
        let width = known_dimensions
            .width
            .unwrap_or(match available_space.width {
                taffy::AvailableSpace::Definite(_) => self.cols as f32,
                taffy::AvailableSpace::MinContent => 1.,
                taffy::AvailableSpace::MaxContent => 100.,
            });

        let height = known_dimensions
            .height
            .unwrap_or(match available_space.height {
                taffy::AvailableSpace::Definite(_) => self.rows as f32,
                taffy::AvailableSpace::MinContent => 1.,
                taffy::AvailableSpace::MaxContent => 100.,
            });

        taffy::Size { width, height }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        let area = ctx.area();
        if area.width == 0 || area.height == 0 {
            return;
        }

        let mut term = self.term.lock();
        for i in 0..16 {
            term.set_color(
                i,
                vte::ansi::Rgb {
                    r: ctx.palette().colors[i].r,
                    g: ctx.palette().colors[i].g,
                    b: ctx.palette().colors[i].b,
                },
            );
        }
        term.set_color(
            NamedColor::Foreground as usize,
            vte::ansi::Rgb {
                r: ctx.palette().foreground.r,
                g: ctx.palette().foreground.g,
                b: ctx.palette().foreground.b,
            },
        );
        term.set_color(
            NamedColor::Background as usize,
            vte::ansi::Rgb {
                r: ctx.palette().background.r,
                g: ctx.palette().background.g,
                b: ctx.palette().background.b,
            },
        );
        let content = term.renderable_content();
        let colors = content.colors;
        let cursor = content.cursor;
        let display_offset = content.display_offset as i32;

        let default_fg = colors[NamedColor::Foreground]
            .map(|c| Rgba::opaque(c.r, c.g, c.b))
            .unwrap_or(Rgba::opaque(229, 229, 229));
        let default_bg = colors[NamedColor::Background]
            .map(|c| Rgba::opaque(c.r, c.g, c.b))
            .unwrap_or(Rgba::opaque(0, 0, 0));

        let mut default_attrs = CellAttributes::default();
        default_attrs.set_foreground(default_fg);
        default_attrs.set_background(default_bg);

        // Get horizontal scroll offset (already adjusted for node position by render context)
        let scroll_x = ctx.scroll_x().max(0.0).round() as usize;
        // Clear the area with default background
        for row in 0..area.height {
            for col in 0..area.width {
                ctx.write_char(area.x + col, area.y + row, ' ', &default_attrs);
            }
        }

        // Render each cell from the terminal
        // Grid line numbers are relative to the visible viewport (can be negative when scrolled)
        // We need to convert to screen coordinates by adding display_offset
        for cell in content.display_iter {
            let point = cell.point;
            let x = point.column.0;
            // Convert grid line to screen coordinate
            let screen_y = point.line.0 + display_offset;

            if screen_y < 0 || screen_y >= area.height as i32 {
                continue;
            }

            // Skip cells before horizontal scroll offset
            if x < scroll_x {
                continue;
            }

            // Calculate rendered x position (account for horizontal scroll)
            let rendered_x = x - scroll_x;
            if rendered_x >= area.width {
                continue;
            }

            let flags = cell.flags;
            if flags.intersects(
                term::cell::Flags::WIDE_CHAR_SPACER | term::cell::Flags::LEADING_WIDE_CHAR_SPACER,
            ) {
                if let Some(cell) = ctx
                    .buffer()
                    .get_cell_mut(area.x + rendered_x, area.y + screen_y as usize)
                {
                    cell.zero_width = true;
                }
                continue;
            }

            let mut attrs = CellAttributes::default();

            // Set foreground color
            if let Some(fg) = Self::convert_color(cell.fg, colors) {
                attrs.set_foreground(fg);
            } else {
                attrs.set_foreground(default_fg);
            }

            // Set background color
            if let Some(bg) = Self::convert_color(cell.bg, colors) {
                attrs.set_background(bg);
            } else {
                attrs.set_background(default_bg);
            }

            // Set text attributes
            if flags.contains(term::cell::Flags::BOLD) {
                attrs.set_bold(true);
            }
            if flags.contains(term::cell::Flags::DIM) {
                attrs.set_dim(true);
            }
            if flags.contains(term::cell::Flags::INVERSE) {
                attrs.set_reverse(true);
            }

            let ch = cell.c;
            ctx.write_char(area.x + rendered_x, area.y + screen_y as usize, ch, &attrs);
        }

        // Render cursor only if focused
        if self.focused {
            let cursor_x = cursor.point.column.0;
            // Convert cursor line to screen coordinate
            let cursor_screen_y = cursor.point.line.0 + display_offset;

            // Check if cursor is within horizontal scroll viewport
            let cursor_rendered_x = cursor_x.saturating_sub(scroll_x);

            if cursor_screen_y >= 0
                && cursor_screen_y < area.height as i32
                && cursor_x >= scroll_x
                && cursor_rendered_x < area.width
                && cursor.shape != AlacCursorShape::Hidden
            {
                tracing::info!(
                    "cursor {}:{}",
                    area.x + cursor_rendered_x,
                    area.y + cursor_screen_y as usize
                );
                ctx.set_cursor(
                    area.x + cursor_rendered_x,
                    area.y + cursor_screen_y as usize,
                    Self::convert_cursor_shape(cursor.shape),
                );
            }
        }
    }

    fn debug_label(&self) -> &'static str {
        "terminal"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event::{
        LocalMouseEvent, MouseButton, MouseEvent, MouseEventKind, MouseScroll, MouseScrollAxis,
        MouseScrollDirection,
    };

    // Helper: test key_to_input in legacy mode (empty TermMode)
    fn legacy_key_input(key: Key) -> Option<Vec<u8>> {
        key_to_input(key, TermMode::empty())
    }

    #[test]
    fn key_to_input_handles_regular_chars() {
        let key = Key::new(KeyCode::Char('a'));
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![b'a']));
    }

    #[test]
    fn key_to_input_handles_utf8() {
        let key = Key::new(KeyCode::Char('漢'));
        let input = legacy_key_input(key);
        assert_eq!(input, Some("漢".as_bytes().to_vec()));
    }

    #[test]
    fn key_to_input_handles_ctrl_c() {
        let key = Key::with_modifiers(KeyCode::Char('c'), true, false, false, false);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![3])); // ETX (End of Text)
    }

    #[test]
    fn key_to_input_handles_enter() {
        let key = Key::new(KeyCode::Enter);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![b'\r']));
    }

    #[test]
    fn key_to_input_handles_backspace() {
        let key = Key::new(KeyCode::Backspace);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x7f]));
    }

    #[test]
    fn key_to_input_handles_arrow_keys() {
        let key = Key::new(KeyCode::Up);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'A']));

        let key = Key::new(KeyCode::Down);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'B']));

        let key = Key::new(KeyCode::Right);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'C']));

        let key = Key::new(KeyCode::Left);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'D']));
    }

    #[test]
    fn key_to_input_handles_alt_keys() {
        let key = Key::with_modifiers(KeyCode::Char('f'), false, true, false, false);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'f']));
    }

    #[test]
    fn key_to_input_handles_ctrl_arrow() {
        let key = Key::with_modifiers(KeyCode::Right, true, false, false, false);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'1', b';', b'5', b'C']));
    }

    #[test]
    fn key_to_input_handles_function_keys() {
        let key = Key::new(KeyCode::Function(1));
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'O', b'P']));

        let key = Key::new(KeyCode::Function(5));
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'1', b'5', b'~']));
    }

    #[test]
    fn key_to_input_handles_function_key_modifiers() {
        let key = Key::with_modifiers(KeyCode::Function(3), true, false, false, false);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'1', b';', b'5', b'R']));
    }

    #[test]
    fn encode_mouse_left_click_sgr() {
        let event = MouseEvent::new(5, 10, MouseEventKind::Down(MouseButton::Left));
        let event = LocalMouseEvent::new(event, 5, 10);
        let bytes = encode_mouse_event(&event, true, true, false);
        // SGR format: \x1b[<0;6;11M (button 0, 1-based coords)
        assert_eq!(bytes, Some(b"\x1b[<0;6;11M".to_vec()));
    }

    #[test]
    fn encode_mouse_right_click_sgr() {
        let event = MouseEvent::new(0, 0, MouseEventKind::Down(MouseButton::Right));
        let event = LocalMouseEvent::new(event, 0, 0);
        let bytes = encode_mouse_event(&event, true, true, false);
        // SGR format: \x1b[<2;1;1M (button 2 = right, 1-based coords)
        assert_eq!(bytes, Some(b"\x1b[<2;1;1M".to_vec()));
    }

    #[test]
    fn encode_mouse_release_sgr() {
        let event = MouseEvent::new(10, 20, MouseEventKind::Up(MouseButton::Left));
        let event = LocalMouseEvent::new(event, 10, 20);
        let bytes = encode_mouse_event(&event, false, true, false);
        // SGR release format: \x1b[<0;11;21m (lowercase 'm' for release)
        assert_eq!(bytes, Some(b"\x1b[<0;11;21m".to_vec()));
    }

    #[test]
    fn encode_mouse_with_modifiers_sgr() {
        let event = MouseEvent::with_modifiers(
            0,
            0,
            MouseEventKind::Down(MouseButton::Left),
            true,
            true,
            true,
            false,
        );
        let event = LocalMouseEvent::new(event, 0, 0);
        let bytes = encode_mouse_event(&event, true, true, false);
        // Button 0 + shift(4) + alt(8) + ctrl(16) = 28
        assert_eq!(bytes, Some(b"\x1b[<28;1;1M".to_vec()));
    }

    #[test]
    fn encode_mouse_scroll_sgr() {
        let event = MouseEvent::new(
            5,
            5,
            MouseEventKind::Scroll(MouseScroll {
                axis: MouseScrollAxis::Vertical,
                direction: MouseScrollDirection::Positive,
            }),
        );
        let event = LocalMouseEvent::new(event, 5, 5);
        let bytes = encode_mouse_event(&event, true, true, false);
        // Scroll up = button 64
        assert_eq!(bytes, Some(b"\x1b[<64;6;6M".to_vec()));
    }

    #[test]
    fn encode_mouse_horizontal_scroll_sgr() {
        let event = MouseEvent::new(
            5,
            5,
            MouseEventKind::Scroll(MouseScroll {
                axis: MouseScrollAxis::Horizontal,
                direction: MouseScrollDirection::Positive,
            }),
        );
        let event = LocalMouseEvent::new(event, 5, 5);
        let bytes = encode_mouse_event(&event, true, true, false);
        // Scroll right = button 67
        assert_eq!(bytes, Some(b"\x1b[<67;6;6M".to_vec()));

        let event = MouseEvent::new(
            5,
            5,
            MouseEventKind::Scroll(MouseScroll {
                axis: MouseScrollAxis::Horizontal,
                direction: MouseScrollDirection::Negative,
            }),
        );
        let event = LocalMouseEvent::new(event, 5, 5);
        let bytes = encode_mouse_event(&event, true, true, false);
        // Scroll left = button 66
        assert_eq!(bytes, Some(b"\x1b[<66;6;6M".to_vec()));
    }

    #[test]
    fn encode_mouse_legacy_format() {
        let event = MouseEvent::new(5, 10, MouseEventKind::Down(MouseButton::Left));
        let event = LocalMouseEvent::new(event, 5, 10);
        let bytes = encode_mouse_event(&event, true, false, false);
        // Legacy format: \x1b[M followed by (button+32), (x+32+1), (y+32+1)
        // button=0, x=5+1=6, y=10+1=11
        assert_eq!(bytes, Some(vec![0x1b, b'[', b'M', 32, 38, 43]));
    }

    #[test]
    fn encode_mouse_no_button_returns_none() {
        let event = MouseEvent::new(0, 0, MouseEventKind::Move);
        let event = LocalMouseEvent::new(event, 0, 0);
        let bytes = encode_mouse_event(&event, true, true, false);
        assert_eq!(bytes, None);
    }

    #[test]
    fn version_increments_on_terminal_output() {
        use std::time::Duration;

        // Spawn a command that produces output
        let state = TerminalState::spawn("echo", &["hello"]).expect("failed to spawn terminal");
        let receiver = state.wakeup_receiver();
        let initial_version = state.version();

        // Wait for wakeup notification (with timeout)
        let result = smol::block_on(async {
            smol::future::or(
                async {
                    receiver.recv().await.ok();
                    true
                },
                async {
                    smol::Timer::after(Duration::from_secs(2)).await;
                    false
                },
            )
            .await
        });

        assert!(result, "should receive wakeup notification");
        assert!(
            state.version() > initial_version,
            "version should increment after terminal output"
        );
    }

    #[test]
    fn bell_events_are_captured() {
        let mut state = TerminalState::new().expect("failed to create terminal");
        state.handle_event_direct(TermEvent::Bell);

        assert!(state.take_bell(), "bell flag should be set");
        assert!(
            !state.take_bell(),
            "bell flag should be cleared after retrieval"
        );
    }

    #[test]
    fn notification_events_set_bell() {
        let mut state = TerminalState::new().expect("failed to create terminal");
        state.handle_event_direct(TermEvent::Notification {
            title: Some("Title".into()),
            body: "Update".into(),
        });

        assert!(state.take_bell(), "bell flag should be set by notification");
    }

    #[test]
    fn renderable_eq_detects_version_change() {
        use std::time::Duration;

        let state = TerminalState::spawn("echo", &["test"]).expect("failed to spawn terminal");
        let receiver = state.wakeup_receiver();

        // Create renderable before any output
        let renderable1 = TerminalRenderable::new(&state, true);

        // Wait for output
        smol::block_on(async {
            smol::future::or(
                async {
                    receiver.recv().await.ok();
                    true
                },
                async {
                    smol::Timer::after(Duration::from_secs(2)).await;
                    false
                },
            )
            .await
        });

        // Create renderable after output
        let renderable2 = TerminalRenderable::new(&state, true);

        // The renderables should not be equal due to version difference
        assert!(
            !renderable1.eq(&renderable2),
            "renderables with different versions should not be equal"
        );
    }

    #[test]
    fn update_returns_false_for_input() {
        let mut state = TerminalState::spawn("cat", &[]).expect("failed to spawn terminal");

        // Get initial version
        let initial_version = state.version();

        // Send some input
        let input_data = b"test";
        let result = state.update(TerminalMsg::Input(input_data.to_vec()));

        // Should return false - re-render will happen via wakeup when PTY echoes the input
        assert!(!result, "update should return false for input");

        // Version should not change immediately (PTY hasn't processed input yet)
        let version_after_update = state.version();
        assert_eq!(
            version_after_update, initial_version,
            "version should not change immediately after update"
        );
    }

    #[test]
    fn paste_payload_wraps_when_bracketed_paste_enabled() {
        let payload = TerminalState::format_paste_payload("paste", TermMode::BRACKETED_PASTE);
        assert_eq!(payload, b"\x1b[200~paste\x1b[201~".to_vec());

        let payload = TerminalState::format_paste_payload("paste", TermMode::empty());
        assert_eq!(payload, b"paste".to_vec());
    }

    #[test]
    fn renderable_focused_field_affects_equality() {
        let state = TerminalState::spawn("echo", &["test"]).expect("failed to spawn terminal");

        // Create two renderables with same state but different focus
        let focused_renderable = TerminalRenderable::new(&state, true);
        let unfocused_renderable = TerminalRenderable::new(&state, false);

        // They should not be equal due to different focus state
        assert!(
            !focused_renderable.eq(&unfocused_renderable),
            "renderables with different focus should not be equal"
        );

        // Two focused renderables should be equal (same state and focus)
        let another_focused_renderable = TerminalRenderable::new(&state, true);
        assert!(
            focused_renderable.eq(&another_focused_renderable),
            "renderables with same focus should be equal"
        );
    }

    #[test]
    fn child_exit_event_marks_state_not_running() {
        let mut state = TerminalState::new().expect("failed to create terminal");

        state.handle_event_direct(TermEvent::ChildExit(0));

        assert!(!state.is_running(), "state should be marked exited");
    }

    #[cfg(unix)]
    #[test]
    fn pty_fd_is_cloexec() {
        let state = TerminalState::new().expect("failed to create terminal");

        let fd = state.pty_fd.as_raw_fd();
        let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
        assert_ne!(
            flags,
            -1,
            "fcntl F_GETFD failed: {}",
            std::io::Error::last_os_error()
        );
        assert!(
            flags & libc::FD_CLOEXEC != 0,
            "PTY fd should have FD_CLOEXEC set"
        );
    }

    #[test]
    fn size_request_event_is_processed() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicBool, Ordering};

        let mut state = TerminalState::new().expect("failed to create terminal");
        let called = Arc::new(AtomicBool::new(false));
        let called_flag = called.clone();

        state.handle_event_direct(TermEvent::TextAreaSizeRequest(Arc::new(move |size| {
            called_flag.store(true, Ordering::Relaxed);
            format!("{}x{}", size.num_cols, size.num_lines)
        })));

        assert!(
            called.load(Ordering::Relaxed),
            "formatter should be invoked when handling size request"
        );
    }

    #[test]
    fn color_request_uses_current_colors() {
        let state = TerminalState::new().expect("failed to create terminal");
        let target = ansi::Rgb { r: 1, g: 2, b: 3 };

        {
            let mut term = state.term.lock();
            term.set_color(NamedColor::Foreground as usize, target);
        }

        let response = TerminalState::format_color_response(
            &state.term,
            NamedColor::Foreground as usize,
            &|color| format!("{:02x}-{:02x}-{:02x}", color.r, color.g, color.b),
        );

        assert_eq!(response, Some("01-02-03".to_string()));
    }

    #[test]
    fn cursor_blinking_change_triggers_wakeup() {
        use std::time::Duration;

        let mut state = TerminalState::new().expect("failed to create terminal");
        let initial_version = state.version();
        let receiver = state.wakeup_receiver();

        state.handle_event_direct(TermEvent::CursorBlinkingChange);

        let woke = smol::block_on(async {
            smol::future::or(async { receiver.recv().await.is_ok() }, async {
                smol::Timer::after(Duration::from_millis(100)).await;
                false
            })
            .await
        });

        assert!(woke, "wakeup should be delivered for cursor state changes");
        assert!(
            state.version() > initial_version,
            "version should increment on cursor state change"
        );
    }

    // ========================================================================
    // Kitty Keyboard Protocol Tests
    // ========================================================================

    #[test]
    fn kitty_legacy_mode_uses_legacy_encoding() {
        // With no kitty flags, should use legacy encoding
        let key = Key::new(KeyCode::Char('a'));
        let mode = TermMode::empty();
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"a".to_vec()));
    }

    #[test]
    fn kitty_legacy_mode_ignores_key_release() {
        let mut key = Key::new(KeyCode::Char('a'));
        key.kind = KeyEventKind::Release;
        let mode = TermMode::empty();
        let input = key_to_input(key, mode);
        assert_eq!(input, None);
    }

    #[test]
    fn kitty_disambiguate_mode_encodes_escape_key() {
        let key = Key::new(KeyCode::Esc);
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;
        let input = key_to_input(key, mode);
        // Should produce CSI 27 u
        assert_eq!(input, Some(b"\x1b[27u".to_vec()));
    }

    #[test]
    fn kitty_disambiguate_mode_encodes_ctrl_c() {
        let key = Key::with_modifiers(KeyCode::Char('c'), true, false, false, false);
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;
        let input = key_to_input(key, mode);
        // Should produce CSI 99;5 u (99='c', 5=ctrl+1)
        assert_eq!(input, Some(b"\x1b[99;5u".to_vec()));
    }

    #[test]
    fn kitty_encode_all_encodes_simple_chars() {
        let key = Key::new(KeyCode::Char('a'));
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 97 u (97='a')
        assert_eq!(input, Some(b"\x1b[97u".to_vec()));
    }

    #[test]
    fn kitty_encode_all_with_modifiers() {
        let key = Key::with_modifiers(KeyCode::Char('a'), true, true, false, false);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 97;7 u (97='a', 7=ctrl(4)+alt(2)+1)
        assert_eq!(input, Some(b"\x1b[97;7u".to_vec()));
    }

    #[test]
    fn kitty_event_types_encodes_release() {
        let mut key = Key::new(KeyCode::Char('a'));
        key.kind = KeyEventKind::Release;
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC | TermMode::REPORT_EVENT_TYPES;
        let input = key_to_input(key, mode);
        // Should produce CSI 97;1:3 u (97='a', 1=no mods, 3=release)
        assert_eq!(input, Some(b"\x1b[97;1:3u".to_vec()));
    }

    #[test]
    fn kitty_event_types_encodes_repeat() {
        let mut key = Key::new(KeyCode::Char('a'));
        key.kind = KeyEventKind::Repeat;
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC | TermMode::REPORT_EVENT_TYPES;
        let input = key_to_input(key, mode);
        // Should produce CSI 97;1:2 u (97='a', 1=no mods, 2=repeat)
        assert_eq!(input, Some(b"\x1b[97;1:2u".to_vec()));
    }

    #[test]
    fn kitty_numpad_keys() {
        let mut key = Key::new(KeyCode::Char('5'));
        key.keypad = true;
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;
        let input = key_to_input(key, mode);
        // Should produce CSI 57404 u (numpad 5)
        assert_eq!(input, Some(b"\x1b[57404u".to_vec()));
    }

    #[test]
    fn kitty_numpad_enter() {
        let mut key = Key::new(KeyCode::Enter);
        key.keypad = true;
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;
        let input = key_to_input(key, mode);
        // Should produce CSI 57414 u (numpad enter)
        assert_eq!(input, Some(b"\x1b[57414u".to_vec()));
    }

    #[test]
    fn kitty_modifier_keys() {
        let key = Key::new(KeyCode::Modifier(ModifierKeyCode::LeftShift));
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 57441 u
        assert_eq!(input, Some(b"\x1b[57441u".to_vec()));
    }

    #[test]
    fn kitty_function_key_f13() {
        let key = Key::new(KeyCode::Function(13));
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;
        let input = key_to_input(key, mode);
        // Should produce CSI 57376 u
        assert_eq!(input, Some(b"\x1b[57376u".to_vec()));
    }

    #[test]
    fn kitty_media_keys() {
        let key = Key::new(KeyCode::Media(MediaKeyCode::PlayPause));
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 57430 u
        assert_eq!(input, Some(b"\x1b[57430u".to_vec()));
    }

    #[test]
    fn kitty_tab_key() {
        let key = Key::new(KeyCode::Tab);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 9 u
        assert_eq!(input, Some(b"\x1b[9u".to_vec()));
    }

    #[test]
    fn kitty_shift_enter() {
        let key = Key::with_modifiers(KeyCode::Enter, false, false, true, false);
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;
        let input = key_to_input(key, mode);
        // Should produce CSI 13;2u (13=enter, 2=shift+1)
        assert_eq!(input, Some(b"\x1b[13;2u".to_vec()));
    }

    #[test]
    fn kitty_enter_key() {
        let key = Key::new(KeyCode::Enter);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 13 u
        assert_eq!(input, Some(b"\x1b[13u".to_vec()));
    }

    #[test]
    fn kitty_backspace_key() {
        let key = Key::new(KeyCode::Backspace);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 127 u
        assert_eq!(input, Some(b"\x1b[127u".to_vec()));
    }

    #[test]
    fn kitty_shift_tab() {
        let key = Key::with_modifiers(KeyCode::Tab, false, false, true, false);
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;
        let input = key_to_input(key, mode);
        // Should produce CSI 9;2 u (9=tab, 2=shift+1)
        assert_eq!(input, Some(b"\x1b[9;2u".to_vec()));
    }

    #[test]
    fn kitty_arrow_keys_with_mode() {
        let key = Key::new(KeyCode::Up);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Arrow keys use legacy terminator for compatibility
        assert_eq!(input, Some(b"\x1b[1A".to_vec()));
    }

    #[test]
    fn app_cursor_mode_uses_ss3_for_arrow_keys() {
        let key = Key::new(KeyCode::Up);
        let mode = TermMode::APP_CURSOR;
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1bOA".to_vec()));
    }

    #[test]
    fn kitty_arrow_keys_with_modifiers() {
        let key = Key::with_modifiers(KeyCode::Up, true, false, false, false);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // CSI 1;5 A (1=base, 5=ctrl+1, A=up)
        assert_eq!(input, Some(b"\x1b[1;5A".to_vec()));
    }

    #[test]
    fn kitty_caps_lock() {
        let key = Key::new(KeyCode::CapsLock);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Should produce CSI 57358 u
        assert_eq!(input, Some(b"\x1b[57358u".to_vec()));
    }

    #[test]
    fn kitty_insert_key() {
        let key = Key::new(KeyCode::Insert);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Insert uses legacy encoding: CSI 2 ~
        assert_eq!(input, Some(b"\x1b[2~".to_vec()));
    }

    #[test]
    fn kitty_delete_key() {
        let key = Key::new(KeyCode::Delete);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // Delete uses legacy encoding: CSI 3 ~
        assert_eq!(input, Some(b"\x1b[3~".to_vec()));
    }

    #[test]
    fn kitty_all_modifiers_combined() {
        let key = Key::with_kitty_extras(
            KeyCode::Char('a'),
            true, // ctrl
            true, // alt
            true, // shift
            true, // super
            true, // hyper
            true, // meta
            KeyEventKind::Press,
            false,
        );
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        // 1+2+4+8+16+32 = 63, so modifiers = 64 (63+1)
        // But we lowercase 'a' for shift, so key code is 97
        assert_eq!(input, Some(b"\x1b[97;64u".to_vec()));
    }

    #[test]
    fn kitty_page_up_down() {
        let key = Key::new(KeyCode::PageUp);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[5~".to_vec()));

        let key = Key::new(KeyCode::PageDown);
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[6~".to_vec()));
    }

    #[test]
    fn kitty_home_end() {
        let key = Key::new(KeyCode::Home);
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[1H".to_vec()));

        let key = Key::new(KeyCode::End);
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[1F".to_vec()));
    }

    #[test]
    fn kitty_function_keys_f1_to_f12() {
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;

        // F1-F4 use SS3 in legacy but CSI in kitty
        let key = Key::new(KeyCode::Function(1));
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[1P".to_vec()));

        let key = Key::new(KeyCode::Function(4));
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[1S".to_vec()));

        // F5-F12 use CSI number ~
        let key = Key::new(KeyCode::Function(5));
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[15~".to_vec()));

        let key = Key::new(KeyCode::Function(12));
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[24~".to_vec()));
    }

    #[test]
    fn kitty_numpad_operators() {
        let mode = TermMode::DISAMBIGUATE_ESC_CODES;

        let mut key = Key::new(KeyCode::Char('+'));
        key.keypad = true;
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57413u".to_vec())); // NUMPAD_ADD

        let mut key = Key::new(KeyCode::Char('-'));
        key.keypad = true;
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57412u".to_vec())); // NUMPAD_SUBTRACT

        let mut key = Key::new(KeyCode::Char('*'));
        key.keypad = true;
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57411u".to_vec())); // NUMPAD_MULTIPLY

        let mut key = Key::new(KeyCode::Char('/'));
        key.keypad = true;
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57410u".to_vec())); // NUMPAD_DIVIDE
    }

    #[test]
    fn kitty_right_modifier_keys() {
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;

        let key = Key::new(KeyCode::Modifier(ModifierKeyCode::RightShift));
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57447u".to_vec()));

        let key = Key::new(KeyCode::Modifier(ModifierKeyCode::RightControl));
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57448u".to_vec()));

        let key = Key::new(KeyCode::Modifier(ModifierKeyCode::RightAlt));
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57449u".to_vec()));
    }

    #[test]
    fn kitty_special_lock_keys() {
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;

        let key = Key::new(KeyCode::ScrollLock);
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57359u".to_vec()));

        let key = Key::new(KeyCode::NumLock);
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57360u".to_vec()));
    }

    #[test]
    fn kitty_misc_keys() {
        let mode = TermMode::REPORT_ALL_KEYS_AS_ESC;

        let key = Key::new(KeyCode::PrintScreen);
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57361u".to_vec()));

        let key = Key::new(KeyCode::Pause);
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57362u".to_vec()));

        let key = Key::new(KeyCode::Menu);
        let input = key_to_input(key, mode);
        assert_eq!(input, Some(b"\x1b[57363u".to_vec()));
    }

    #[test]
    fn legacy_insert_delete() {
        // Verify legacy mode still works for insert/delete
        let key = Key::new(KeyCode::Insert);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(b"\x1b[2~".to_vec()));

        let key = Key::new(KeyCode::Delete);
        let input = legacy_key_input(key);
        assert_eq!(input, Some(b"\x1b[3~".to_vec()));
    }

    #[test]
    fn legacy_mode_ignores_modifier_keys() {
        // Modifier keys don't produce output in legacy mode
        let key = Key::new(KeyCode::Modifier(ModifierKeyCode::LeftShift));
        let input = legacy_key_input(key);
        assert_eq!(input, None);

        let key = Key::new(KeyCode::CapsLock);
        let input = legacy_key_input(key);
        assert_eq!(input, None);
    }

    #[test]
    fn terminal_in_horizontal_scroll() {
        use crate::components::scroll;
        use crate::dom::rounding::round_layout;
        use crate::test_utils::render_node_to_string;
        use crate::{ScrollMsg, ScrollState, Size};
        use std::time::Duration;
        use taffy::{AvailableSpace, compute_root_layout};

        // Create a terminal with wide content (numbers 1-9 repeated to reach 120 chars)
        let state = TerminalState::spawn(
            "sh",
            &[
                "-c",
                "python3 -c \"print(''.join(str((i % 9) + 1) for i in range(120)))\"",
            ],
        )
        .expect("failed to spawn terminal");

        // Wait for the output to be processed
        let receiver = state.wakeup_receiver();
        let _ = smol::block_on(async {
            smol::future::or(
                async {
                    receiver.recv().await.ok();
                    true
                },
                async {
                    smol::Timer::after(Duration::from_secs(2)).await;
                    false
                },
            )
            .await
        });

        // Wrap terminal in a horizontal scrollable
        let mut scroll_state = ScrollState::horizontal();
        scroll_state.update(ScrollMsg::Resize {
            viewport: Size {
                width: 20, // Narrow viewport
                height: 10,
            },
            content: Size {
                width: 100, // Wide content
                height: 10,
            },
        });

        // Test with no horizontal scroll offset - should show "12345678912345678912"
        {
            scroll_state.update(ScrollMsg::AxisJumpTo {
                axis: scroll::ScrollAxis::Horizontal,
                offset: 0.0,
            });

            let term_node = terminal::<()>("term", &state, false, |_| ());
            let mut node =
                scroll::scrollable_content("scroll-container", &scroll_state, 3, |_| (), term_node);

            compute_root_layout(
                &mut node,
                u64::MAX.into(),
                taffy::Size {
                    width: AvailableSpace::Definite(20.0),
                    height: AvailableSpace::Definite(10.0),
                },
            );
            round_layout(&mut node);

            let rendered = render_node_to_string(&mut node, 20, 10).expect("render should succeed");

            // First line should show first 20 characters
            let first_line = rendered.lines().next().unwrap_or("");
            let expected = "12345678912345678912";
            assert_eq!(
                first_line, expected,
                "At scroll offset 0, should show first 20 chars. Got:\n{rendered}"
            );
        }

        // Test with horizontal scroll offset 50 - should show characters 50-69
        {
            scroll_state.update(ScrollMsg::AxisJumpTo {
                axis: scroll::ScrollAxis::Horizontal,
                offset: 50.0,
            });

            let term_node = terminal::<()>("term", &state, false, |_| ());
            let mut node =
                scroll::scrollable_content("scroll-container", &scroll_state, 3, |_| (), term_node);

            compute_root_layout(
                &mut node,
                u64::MAX.into(),
                taffy::Size {
                    width: AvailableSpace::Definite(20.0),
                    height: AvailableSpace::Definite(10.0),
                },
            );
            round_layout(&mut node);

            let rendered = render_node_to_string(&mut node, 20, 10).expect("render should succeed");

            // At offset 50, position 50 is '7' (50 % 9 = 5, 5+1 = 6... wait, let me recalculate)
            // Position 50: (50 % 9) + 1 = (5) + 1 = 6
            // Positions 50-69: 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7
            let first_line = rendered.lines().next().unwrap_or("");
            let expected = "67891234567891234567";
            assert_eq!(
                first_line, expected,
                "At scroll offset 50, should show characters 50-69. Got:\n{rendered}"
            );
        }
    }

    #[test]
    fn multiple_terminals_in_horizontal_scroll() {
        use crate::components::scroll;
        use crate::dom::rounding::round_layout;
        use crate::dom::row;
        use crate::test_utils::render_node_to_string;
        use crate::{ScrollMsg, ScrollState, Size};
        use std::time::Duration;
        use taffy::{AvailableSpace, Dimension, compute_root_layout};

        // Create two terminals with distinct content
        let state1 = TerminalState::spawn("sh", &["-c", "python3 -c \"print('A' * 40)\""])
            .expect("failed to spawn terminal 1");

        let state2 = TerminalState::spawn("sh", &["-c", "python3 -c \"print('B' * 40)\""])
            .expect("failed to spawn terminal 2");

        // Wait for output
        for state in [&state1, &state2] {
            let receiver = state.wakeup_receiver();
            let _ = smol::block_on(async {
                smol::future::or(
                    async {
                        receiver.recv().await.ok();
                        true
                    },
                    async {
                        smol::Timer::after(Duration::from_secs(2)).await;
                        false
                    },
                )
                .await
            });
        }

        // Create a horizontal scroll container with two terminals side-by-side
        // Each terminal is 30 cols wide, but viewport is only 20 cols
        let mut scroll_state = ScrollState::horizontal();
        scroll_state.update(ScrollMsg::Resize {
            viewport: Size {
                width: 20,
                height: 5,
            },
            content: Size {
                width: 60, // Two terminals of 30 each
                height: 5,
            },
        });

        // At scroll offset 0, we should see the first terminal (A's)
        {
            scroll_state.update(ScrollMsg::AxisJumpTo {
                axis: scroll::ScrollAxis::Horizontal,
                offset: 0.0,
            });

            let term1 = terminal::<()>("term1", &state1, false, |_| ())
                .with_width(Dimension::length(30.0))
                .with_flex_shrink(0.0)
                .with_flex_grow(0.0);
            let term2 = terminal::<()>("term2", &state2, false, |_| ())
                .with_width(Dimension::length(30.0))
                .with_flex_shrink(0.0)
                .with_flex_grow(0.0);

            let content = row(vec![term1, term2]);
            let mut node =
                scroll::scrollable_content("scroll-container", &scroll_state, 3, |_| (), content);

            compute_root_layout(
                &mut node,
                u64::MAX.into(),
                taffy::Size {
                    width: AvailableSpace::Definite(20.0),
                    height: AvailableSpace::Definite(5.0),
                },
            );
            round_layout(&mut node);

            let rendered = render_node_to_string(&mut node, 20, 5).expect("render should succeed");

            // First line should contain only A's
            let first_line = rendered.lines().next().unwrap_or("");
            assert!(
                first_line.chars().all(|c| c == 'A' || c == ' '),
                "At scroll offset 0, should only see first terminal (A's). Got:\n{rendered}"
            );
            assert!(
                first_line.contains('A'),
                "Should see at least one 'A' at offset 0. Got:\n{rendered}"
            );
        }

        // At scroll offset 30, we should see the second terminal (B's)
        {
            scroll_state.update(ScrollMsg::AxisJumpTo {
                axis: scroll::ScrollAxis::Horizontal,
                offset: 30.0,
            });

            let term1 = terminal::<()>("term1", &state1, false, |_| ())
                .with_width(Dimension::length(30.0))
                .with_flex_shrink(0.0)
                .with_flex_grow(0.0);
            let term2 = terminal::<()>("term2", &state2, false, |_| ())
                .with_width(Dimension::length(30.0))
                .with_flex_shrink(0.0)
                .with_flex_grow(0.0);

            let content = row(vec![term1, term2]);
            let mut node =
                scroll::scrollable_content("scroll-container", &scroll_state, 3, |_| (), content);

            compute_root_layout(
                &mut node,
                u64::MAX.into(),
                taffy::Size {
                    width: AvailableSpace::Definite(20.0),
                    height: AvailableSpace::Definite(5.0),
                },
            );
            round_layout(&mut node);

            let rendered = render_node_to_string(&mut node, 20, 5).expect("render should succeed");

            // First line should contain only B's (we've scrolled past terminal 1)
            let first_line = rendered.lines().next().unwrap_or("");
            assert!(
                first_line.chars().all(|c| c == 'B' || c == ' '),
                "At scroll offset 30, should only see second terminal (B's). Got:\n{rendered}"
            );
            assert!(
                first_line.contains('B'),
                "Should see at least one 'B' at offset 30. Got:\n{rendered}"
            );
        }

        // At scroll offset 10, we should see partial first terminal (A's) and partial second terminal (B's)
        {
            scroll_state.update(ScrollMsg::AxisJumpTo {
                axis: scroll::ScrollAxis::Horizontal,
                offset: 10.0,
            });

            let term1 = terminal::<()>("term1", &state1, false, |_| ())
                .with_width(Dimension::length(30.0))
                .with_flex_shrink(0.0)
                .with_flex_grow(0.0);
            let term2 = terminal::<()>("term2", &state2, false, |_| ())
                .with_width(Dimension::length(30.0))
                .with_flex_shrink(0.0)
                .with_flex_grow(0.0);

            let content = row(vec![term1, term2]);
            let mut node =
                scroll::scrollable_content("scroll-container", &scroll_state, 3, |_| (), content);

            compute_root_layout(
                &mut node,
                u64::MAX.into(),
                taffy::Size {
                    width: AvailableSpace::Definite(20.0),
                    height: AvailableSpace::Definite(5.0),
                },
            );
            round_layout(&mut node);

            let rendered = render_node_to_string(&mut node, 20, 5).expect("render should succeed");

            // First line should start with A's (from remaining of terminal 1)
            // Viewport is 20 cols, scroll offset is 10
            // Terminal 1 starts at 0, so we see cols 10-29 (20 cols of A's)
            let first_line = rendered.lines().next().unwrap_or("");
            assert!(
                first_line.contains('A'),
                "At scroll offset 10, should still see A's from first terminal. Got:\n{rendered}"
            );
        }
    }
}
