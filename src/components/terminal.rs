//! Terminal widget using alacritty_terminal for terminal emulation.
//!
//! This module provides a terminal widget that can be embedded in the TUI.
//! It uses alacritty_terminal for the actual terminal emulation.
//!
//! TODOs
//! - Dont use 2 channels for EventListener
//! - Add kitty keyboard support

#[cfg(test)]
use once_cell::sync::Lazy;
use std::any::Any;
use std::borrow::Cow;
#[cfg(unix)]
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
use alacritty_terminal::term::color::Colors;
use alacritty_terminal::term::test::TermSize;
use alacritty_terminal::term::{self, Config as TermConfig, Term, TermMode};
use alacritty_terminal::tty::{self, Options as PtyOptions, Shell};
use alacritty_terminal::vte::ansi::{self, CursorShape as AlacCursorShape, Handler, NamedColor};

use smol::channel::{self, Receiver, Sender};

use crate::buffer::{CellAttributes, CursorShape};
use crate::dom::{Node, Renderable, renderable};
use crate::event::{Key, KeyCode, MouseButtons, MouseEvent};
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
    /// Scroll by delta lines (positive = down, negative = up)
    Scroll(i32),
    /// Resize the terminal to new dimensions
    Resize { cols: u16, rows: u16 },
    /// Focus gained
    FocusGained,
    /// Focus lost
    FocusLost,
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
}

impl TerminalEventListener {
    fn new(
        version: Arc<AtomicU64>,
        wakeup_sender: Sender<()>,
        event_sender: Sender<TermEvent>,
    ) -> Self {
        Self {
            version,
            wakeup_sender,
            event_sender,
        }
    }
}

impl EventListener for TerminalEventListener {
    fn send_event(&self, event: TermEvent) {
        if matches!(&event, TermEvent::Wakeup) {
            self.version.fetch_add(1, Ordering::Relaxed);
            let result = self.wakeup_sender.try_send(());
            tracing::info!("Wakeup event sent: {:?}", result);
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
    #[allow(dead_code)]
    /// Sender for detailed terminal events (primarily for tests).
    event_sender: Sender<TermEvent>,
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
    /// Cloned PTY master fd for querying foreground process.
    #[cfg(unix)]
    pty_fd: OwnedFd,
    cols: u16,
    rows: u16,
    join_handle: Option<EventLoopHandle>,
    #[cfg(test)]
    _test_permit: TerminalTestPermit,
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
    fn handle_event_now(
        event: TermEvent,
        sender: &EventLoopSender,
        cols_shared: &Arc<AtomicU16>,
        rows_shared: &Arc<AtomicU16>,
        exited_shared: &Arc<AtomicBool>,
        title_shared: &Arc<Mutex<Option<String>>>,
        bell_shared: &Arc<AtomicBool>,
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
            TermEvent::Bell => {
                bell_shared.store(true, Ordering::Relaxed);
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
        Self::spawn_internal(shell, None)
    }

    /// Create a new terminal state starting in a specific working directory.
    pub fn with_working_dir(path: impl AsRef<Path>) -> std::io::Result<Self> {
        Self::spawn_internal(None, Some(path.as_ref().to_path_buf()))
    }

    /// Spawn a command in the terminal.
    pub fn spawn(command: &str, args: &[&str]) -> std::io::Result<Self> {
        let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
        let shell = Shell::new(command.to_string(), args);
        Self::spawn_internal(Some(shell), None)
    }

    fn spawn_internal(
        shell: Option<Shell>,
        working_directory: Option<PathBuf>,
    ) -> std::io::Result<Self> {
        #[cfg(test)]
        let test_permit = TerminalTestPermit::acquire();

        let cols = 80u16;
        let rows = 24u16;

        let pty_config = PtyOptions {
            shell,
            working_directory,
            drain_on_exit: true,
            env: Default::default(),
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
        let bell_shared = Arc::new(AtomicBool::new(false));
        let (wakeup_sender, wakeup_receiver) = channel::unbounded();
        let (event_sender, event_receiver) = channel::unbounded();
        let event_listener =
            TerminalEventListener::new(version.clone(), wakeup_sender.clone(), event_sender.clone());
        let config = TermConfig {
            ..Default::default()
        };
        let term_size = TermSize::new(cols as usize, rows as usize);
        let term = Term::new(config, &term_size, event_listener.clone());
        let term = Arc::new(FairMutex::new(term));

        let event_loop = EventLoop::new(term.clone(), event_listener, pty, false, false)
            .map_err(|e| std::io::Error::other(format!("EventLoop error: {e:?}")))?;

        let sender = event_loop.channel();
        let join_handle = event_loop.spawn();

        {
            let cols_shared = cols_shared.clone();
            let rows_shared = rows_shared.clone();
            let exited_shared = exited_shared.clone();
            let title_shared = title_shared.clone();
            let bell_shared = bell_shared.clone();
            let sender = sender.clone();
            smol::spawn(async move {
                while let Ok(event) = event_receiver.recv().await {
                    Self::handle_event_now(
                        event,
                        &sender,
                        &cols_shared,
                        &rows_shared,
                        &exited_shared,
                        &title_shared,
                        &bell_shared,
                    );
                }
            })
            .detach();
        }

        Ok(Self {
            term,
            sender,
            version,
            wakeup_sender,
            wakeup_receiver,
            event_sender,
            cols_shared,
            rows_shared,
            exited_shared,
            title: title_shared,
            bell: bell_shared,
            #[cfg(unix)]
            pty_fd: pty_fd.into(),
            cols,
            rows,
            join_handle: Some(join_handle),
            #[cfg(test)]
            _test_permit: test_permit,
        })
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

    /// Get the current terminal mode flags.
    pub fn mode(&self) -> TermMode {
        let term = self.term.lock();
        *term.mode()
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
    use std::cell::RefCell;
    use std::rc::Rc;

    let renderable_widget = TerminalRenderable::new(state, focused);
    let mut node = renderable::<Msg>(renderable_widget).with_id(id);

    let handler = Rc::new(on_event);
    let mouse_handler = handler.clone();
    let resize_handler = handler.clone();
    let current_size = state.size();
    let term = state.term.clone();
    let last_buttons = Rc::new(RefCell::new(MouseButtons::default()));

    node = node.on_mouse(move |event: MouseEvent| {
        let prev_buttons = *last_buttons.borrow();
        *last_buttons.borrow_mut() = event.buttons;
        // Get current mode dynamically - applications can enable/disable mouse mode
        let current_mode = *term.lock().mode();

        // Handle scroll wheel - always process for alternate scroll mode
        if event.buttons.vert_wheel {
            // If terminal is in mouse mode, forward as mouse event
            if current_mode.intersects(TermMode::MOUSE_MODE) {
                let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                if let Some(bytes) = encode_mouse_event(&event, true, sgr, false) {
                    return Some(mouse_handler(TerminalMsg::Input(bytes)));
                }
            }
            // Otherwise use scroll behavior
            let delta = if event.buttons.wheel_positive { -3 } else { 3 };
            return Some(mouse_handler(TerminalMsg::Scroll(delta)));
        }

        // Handle horizontal scroll
        if event.buttons.horz_wheel {
            if current_mode.intersects(TermMode::MOUSE_MODE) {
                let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                if let Some(bytes) = encode_mouse_event(&event, true, sgr, false) {
                    return Some(mouse_handler(TerminalMsg::Input(bytes)));
                }
            }
        }

        // Handle click events when terminal is in mouse mode
        if current_mode.intersects(TermMode::MOUSE_MODE) {
            let is_press = event.buttons.left || event.buttons.middle || event.buttons.right;
            let was_pressed = prev_buttons.left || prev_buttons.middle || prev_buttons.right;
            let motion_event = current_mode.contains(TermMode::MOUSE_MOTION);
            if is_press {
                let is_drag = was_pressed;
                let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                if let Some(bytes) = encode_mouse_event(&event, true, sgr, is_drag) {
                    return Some(mouse_handler(TerminalMsg::Input(bytes)));
                }
            } else if was_pressed {
                let mut release_event = event;
                release_event.buttons = MouseButtons {
                    left: prev_buttons.left,
                    right: prev_buttons.right,
                    middle: prev_buttons.middle,
                    horz_wheel: false,
                    vert_wheel: false,
                    wheel_positive: true,
                };
                let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                if let Some(bytes) = encode_mouse_event(&release_event, false, sgr, false) {
                    return Some(mouse_handler(TerminalMsg::Input(bytes)));
                }
            } else if motion_event {
                let sgr = current_mode.contains(TermMode::SGR_MOUSE);
                if let Some(bytes) = encode_mouse_event(&event, true, sgr, true) {
                    return Some(mouse_handler(TerminalMsg::Input(bytes)));
                }
            }
        }

        None
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

/// Translate a key event to terminal input bytes.
pub fn key_to_input(key: Key) -> Option<Vec<u8>> {
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
    };

    Some(bytes)
}

/// Encode a mouse event as terminal escape sequences.
///
/// Uses SGR mouse encoding format: `\x1b[<Cb;Cx;CyM` (press) or `\x1b[<Cb;Cx;Cym` (release)
/// where Cb is the button code with modifiers, and Cx/Cy are 1-based coordinates.
pub fn encode_mouse_event(
    event: &MouseEvent,
    pressed: bool,
    sgr_mode: bool,
    motion: bool,
) -> Option<Vec<u8>> {
    // Determine button code
    let button = if event.buttons.left {
        0
    } else if event.buttons.middle {
        1
    } else if event.buttons.right {
        2
    } else if event.buttons.vert_wheel {
        if event.buttons.wheel_positive {
            64 // scroll up
        } else {
            65 // scroll down
        }
    } else if event.buttons.horz_wheel {
        if event.buttons.wheel_positive {
            67 // scroll right
        } else {
            66 // scroll left
        }
    } else if motion {
        3
    } else {
        return None;
    };

    // Add modifier flags
    let mut cb = button;
    if event.shift {
        cb |= 4;
    }
    if event.alt {
        cb |= 8;
    }
    if event.ctrl {
        cb |= 16;
    }
    if motion {
        cb |= 32;
    }

    // Convert to 1-based coordinates
    let cx = event.local_x.saturating_add(1);
    let cy = event.local_y.saturating_add(1);

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

/// Default key bindings for terminal input.
pub fn default_terminal_keybindings<Msg>(
    key: Key,
    on_input: impl Fn(TerminalMsg) -> Msg,
) -> Option<Msg> {
    key_to_input(key).map(|bytes| on_input(TerminalMsg::Input(bytes)))
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
                taffy::AvailableSpace::Definite(w) => w,
                taffy::AvailableSpace::MinContent => 1.,
                taffy::AvailableSpace::MaxContent => 100.,
            });

        let height = known_dimensions
            .height
            .unwrap_or(match available_space.height {
                taffy::AvailableSpace::Definite(h) => h,
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
            if x >= area.width {
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
            let flags = cell.flags;
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
            ctx.write_char(area.x + x, area.y + screen_y as usize, ch, &attrs);
        }

        // Render cursor only if focused
        if self.focused {
            let cursor_x = cursor.point.column.0;
            // Convert cursor line to screen coordinate
            let cursor_screen_y = cursor.point.line.0 + display_offset;

            if cursor_screen_y >= 0
                && cursor_screen_y < area.height as i32
                && cursor_x < area.width
                && cursor.shape != AlacCursorShape::Hidden
            {
                tracing::info!(
                    "cursor {}:{}",
                    area.x + cursor_x,
                    area.y + cursor_screen_y as usize
                );
                ctx.set_cursor(
                    area.x + cursor_x,
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

    #[test]
    fn key_to_input_handles_regular_chars() {
        let key = Key::new(KeyCode::Char('a'));
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![b'a']));
    }

    #[test]
    fn key_to_input_handles_utf8() {
        let key = Key::new(KeyCode::Char('漢'));
        let input = key_to_input(key);
        assert_eq!(input, Some("漢".as_bytes().to_vec()));
    }

    #[test]
    fn key_to_input_handles_ctrl_c() {
        let key = Key::with_modifiers(KeyCode::Char('c'), true, false, false, false);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![3])); // ETX (End of Text)
    }

    #[test]
    fn key_to_input_handles_enter() {
        let key = Key::new(KeyCode::Enter);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![b'\r']));
    }

    #[test]
    fn key_to_input_handles_backspace() {
        let key = Key::new(KeyCode::Backspace);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x7f]));
    }

    #[test]
    fn key_to_input_handles_arrow_keys() {
        let key = Key::new(KeyCode::Up);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'A']));

        let key = Key::new(KeyCode::Down);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'B']));

        let key = Key::new(KeyCode::Right);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'C']));

        let key = Key::new(KeyCode::Left);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'D']));
    }

    #[test]
    fn key_to_input_handles_alt_keys() {
        let key = Key::with_modifiers(KeyCode::Char('f'), false, true, false, false);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'f']));
    }

    #[test]
    fn key_to_input_handles_ctrl_arrow() {
        let key = Key::with_modifiers(KeyCode::Right, true, false, false, false);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'1', b';', b'5', b'C']));
    }

    #[test]
    fn key_to_input_handles_function_keys() {
        let key = Key::new(KeyCode::Function(1));
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'O', b'P']));

        let key = Key::new(KeyCode::Function(5));
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'1', b'5', b'~']));
    }

    #[test]
    fn key_to_input_handles_function_key_modifiers() {
        let key = Key::with_modifiers(KeyCode::Function(3), true, false, false, false);
        let input = key_to_input(key);
        assert_eq!(input, Some(vec![0x1b, b'[', b'1', b';', b'5', b'R']));
    }

    #[test]
    fn encode_mouse_left_click_sgr() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 5,
            y: 10,
            local_x: 5,
            local_y: 10,
            buttons: MouseButtons {
                left: true,
                right: false,
                middle: false,
                horz_wheel: false,
                vert_wheel: false,
                wheel_positive: false,
            },
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 1,
        };
        let bytes = encode_mouse_event(&event, true, true, false);
        // SGR format: \x1b[<0;6;11M (button 0, 1-based coords)
        assert_eq!(bytes, Some(b"\x1b[<0;6;11M".to_vec()));
    }

    #[test]
    fn encode_mouse_right_click_sgr() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 0,
            y: 0,
            local_x: 0,
            local_y: 0,
            buttons: MouseButtons {
                left: false,
                right: true,
                middle: false,
                horz_wheel: false,
                vert_wheel: false,
                wheel_positive: false,
            },
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 1,
        };
        let bytes = encode_mouse_event(&event, true, true, false);
        // SGR format: \x1b[<2;1;1M (button 2 = right, 1-based coords)
        assert_eq!(bytes, Some(b"\x1b[<2;1;1M".to_vec()));
    }

    #[test]
    fn encode_mouse_release_sgr() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 10,
            y: 20,
            local_x: 10,
            local_y: 20,
            buttons: MouseButtons {
                left: true,
                right: false,
                middle: false,
                horz_wheel: false,
                vert_wheel: false,
                wheel_positive: false,
            },
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 0,
        };
        let bytes = encode_mouse_event(&event, false, true, false);
        // SGR release format: \x1b[<0;11;21m (lowercase 'm' for release)
        assert_eq!(bytes, Some(b"\x1b[<0;11;21m".to_vec()));
    }

    #[test]
    fn encode_mouse_with_modifiers_sgr() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 0,
            y: 0,
            local_x: 0,
            local_y: 0,
            buttons: MouseButtons {
                left: true,
                right: false,
                middle: false,
                horz_wheel: false,
                vert_wheel: false,
                wheel_positive: false,
            },
            ctrl: true,
            alt: true,
            shift: true,
            super_key: false,
            click_count: 1,
        };
        let bytes = encode_mouse_event(&event, true, true, false);
        // Button 0 + shift(4) + alt(8) + ctrl(16) = 28
        assert_eq!(bytes, Some(b"\x1b[<28;1;1M".to_vec()));
    }

    #[test]
    fn encode_mouse_scroll_sgr() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 5,
            y: 5,
            local_x: 5,
            local_y: 5,
            buttons: MouseButtons {
                left: false,
                right: false,
                middle: false,
                horz_wheel: false,
                vert_wheel: true,
                wheel_positive: true,
            },
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 0,
        };
        let bytes = encode_mouse_event(&event, true, true, false);
        // Scroll up = button 64
        assert_eq!(bytes, Some(b"\x1b[<64;6;6M".to_vec()));
    }

    #[test]
    fn encode_mouse_horizontal_scroll_sgr() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 5,
            y: 5,
            local_x: 5,
            local_y: 5,
            buttons: MouseButtons {
                left: false,
                right: false,
                middle: false,
                horz_wheel: true,
                vert_wheel: false,
                wheel_positive: true,
            },
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 0,
        };
        let bytes = encode_mouse_event(&event, true, true, false);
        // Scroll right = button 67
        assert_eq!(bytes, Some(b"\x1b[<67;6;6M".to_vec()));

        let event = MouseEvent {
            x: 5,
            y: 5,
            local_x: 5,
            local_y: 5,
            buttons: MouseButtons {
                left: false,
                right: false,
                middle: false,
                horz_wheel: true,
                vert_wheel: false,
                wheel_positive: false,
            },
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 0,
        };
        let bytes = encode_mouse_event(&event, true, true, false);
        // Scroll left = button 66
        assert_eq!(bytes, Some(b"\x1b[<66;6;6M".to_vec()));
    }

    #[test]
    fn encode_mouse_legacy_format() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 5,
            y: 10,
            local_x: 5,
            local_y: 10,
            buttons: MouseButtons {
                left: true,
                right: false,
                middle: false,
                horz_wheel: false,
                vert_wheel: false,
                wheel_positive: false,
            },
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 1,
        };
        let bytes = encode_mouse_event(&event, true, false, false);
        // Legacy format: \x1b[M followed by (button+32), (x+32+1), (y+32+1)
        // button=0, x=5+1=6, y=10+1=11
        assert_eq!(bytes, Some(vec![0x1b, b'[', b'M', 32, 38, 43]));
    }

    #[test]
    fn encode_mouse_no_button_returns_none() {
        use crate::event::MouseButtons;
        let event = MouseEvent {
            x: 0,
            y: 0,
            local_x: 0,
            local_y: 0,
            buttons: MouseButtons::default(),
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 0,
        };
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
}
