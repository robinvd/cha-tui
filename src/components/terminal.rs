//! Terminal widget using wezterm-term for terminal emulation.
//! This module provides a terminal widget that can be embedded in the TUI.

#[cfg(test)]
use once_cell::sync::Lazy;
use portable_pty::{CommandBuilder, MasterPty, PtySize, native_pty_system};
use smol::channel::{self, Receiver, Sender};
use std::any::Any;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::io;
use std::io::Read;
use std::os::fd::RawFd;
use std::path::{Path, PathBuf};
use std::sync::Arc;
#[cfg(test)]
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::thread::JoinHandle;

use wezterm_surface::{CursorShape as WezCursorShape, CursorVisibility};
use wezterm_term::color::{ColorPalette, RgbColor, SrgbaTuple};
use wezterm_term::input::{
    KeyCode as WezKeyCode, KeyModifiers, MouseButton as WezMouseButton,
    MouseEvent as WezMouseEvent, MouseEventKind as WezMouseEventKind,
};
use wezterm_term::{Alert, AlertHandler, Terminal, TerminalConfiguration, TerminalSize};

use super::process;
use crate::buffer::{CellAttributes, CursorShape};
use crate::dom::{Node, Renderable, renderable};
use crate::event::{
    Key, KeyCode, KeyEventKind, MediaKeyCode, ModifierKeyCode, MouseButtons, MouseEvent,
};
use crate::palette::{Palette, Rgba};
use crate::render::RenderContext;

fn set_cloexec(fd: RawFd) -> io::Result<()> {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
    if flags == -1 {
        return Err(io::Error::last_os_error());
    }

    let flags = flags | libc::FD_CLOEXEC;
    if unsafe { libc::fcntl(fd, libc::F_SETFD, flags) } == -1 {
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
            Lazy::new(|| (Mutex::new(0), Condvar::new()));
        let (lock, cvar) = &*PERMITS;
        let mut available = lock.lock().expect("terminal test semaphore poisoned");
        while *available >= 2 {
            available = cvar
                .wait(available)
                .expect("terminal test semaphore wait poisoned");
        }
        *available += 1;
        Self { lock, cvar }
    }
}

#[cfg(test)]
impl Drop for TerminalTestPermit {
    fn drop(&mut self) {
        let mut available = self.lock.lock().expect("terminal test semaphore poisoned");
        if *available > 0 {
            *available -= 1;
        }
        self.cvar.notify_one();
    }
}

/// Messages for terminal state updates.
#[derive(Clone, Debug)]
pub enum TerminalMsg {
    /// Key input from the user.
    Key(Key),
    /// Mouse input from the user.
    Mouse(TerminalMouseEvent),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TerminalMouseEventKind {
    Press,
    Release,
    Move,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TerminalMouseButton {
    None,
    Left,
    Middle,
    Right,
    WheelUp,
    WheelDown,
    WheelLeft,
    WheelRight,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TerminalMouseEvent {
    pub kind: TerminalMouseEventKind,
    pub button: TerminalMouseButton,
    pub x: u16,
    pub y: u16,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub super_key: bool,
}

struct TerminalConfig {
    palette: Mutex<ColorPalette>,
    generation: AtomicUsize,
}

impl TerminalConfig {
    fn new(palette: ColorPalette) -> Self {
        Self {
            palette: Mutex::new(palette),
            generation: AtomicUsize::new(0),
        }
    }

    fn set_palette(&self, palette: ColorPalette) {
        if let Ok(mut current) = self.palette.lock()
            && *current != palette
        {
            *current = palette;
            self.generation.fetch_add(1, Ordering::Relaxed);
        }
    }
}

impl std::fmt::Debug for TerminalConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TerminalConfig")
            .field("generation", &self.generation.load(Ordering::Relaxed))
            .finish_non_exhaustive()
    }
}

impl TerminalConfiguration for TerminalConfig {
    fn generation(&self) -> usize {
        self.generation.load(Ordering::Relaxed)
    }

    fn color_palette(&self) -> ColorPalette {
        self.palette
            .lock()
            .map(|palette| palette.clone())
            .unwrap_or_else(|_| ColorPalette::default())
    }

    fn enable_kitty_keyboard(&self) -> bool {
        true
    }

    fn enable_csi_u_key_encoding(&self) -> bool {
        true
    }

    fn enable_title_reporting(&self) -> bool {
        true
    }
}

struct TerminalAlertListener {
    title: Arc<Mutex<Option<String>>>,
    bell: Arc<AtomicBool>,
}

impl AlertHandler for TerminalAlertListener {
    fn alert(&mut self, alert: Alert) {
        match alert {
            Alert::Bell => {
                self.bell.store(true, Ordering::Relaxed);
            }
            Alert::WindowTitleChanged(title) => {
                if let Ok(mut current) = self.title.lock() {
                    *current = Some(title);
                }
            }
            Alert::IconTitleChanged(title) => {
                if let Ok(mut current) = self.title.lock() {
                    *current = title;
                }
            }
            _ => {}
        }
    }
}

#[derive(Default)]
struct ContentTracker {
    hash: u64,
    version: u64,
}

/// State for the embedded terminal.
pub struct TerminalState {
    terminal: Arc<Mutex<Terminal>>,
    config: Arc<TerminalConfig>,
    version: Arc<AtomicU64>,
    wakeup_sender: Sender<()>,
    wakeup_receiver: Receiver<()>,
    exited_shared: Arc<AtomicBool>,
    title: Arc<Mutex<Option<String>>>,
    bell: Arc<AtomicBool>,
    content_tracker: Mutex<ContentTracker>,
    content_dirty: Arc<AtomicBool>,
    scroll_offset: usize,
    pty: Mutex<Box<dyn MasterPty + Send>>,
    child: Mutex<Box<dyn portable_pty::Child + Send + Sync>>,
    pty_fd: Option<RawFd>,
    cols: u16,
    rows: u16,
    reader_handle: Option<JoinHandle<()>>,
    #[cfg(test)]
    _test_permit: TerminalTestPermit,
}

impl std::fmt::Debug for TerminalState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TerminalState")
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .field("scroll_offset", &self.scroll_offset)
            .finish_non_exhaustive()
    }
}

impl TerminalState {
    /// Create a new terminal state with the default shell.
    pub fn new() -> std::io::Result<Self> {
        Self::with_shell(None)
    }

    /// Create a new terminal with a specific shell.
    pub fn with_shell(shell: Option<&str>) -> std::io::Result<Self> {
        let shell = shell.map(|s| (s.to_string(), Vec::new()));
        Self::spawn_internal(shell, None)
    }

    /// Create a new terminal state starting in a specific working directory.
    pub fn with_working_dir(path: impl AsRef<Path>) -> std::io::Result<Self> {
        Self::spawn_internal(None, Some(path.as_ref().to_path_buf()))
    }

    /// Spawn a command in the terminal.
    pub fn spawn(command: &str, args: &[&str]) -> std::io::Result<Self> {
        let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
        let shell = (command.to_string(), args);
        Self::spawn_internal(Some(shell), None)
    }

    fn spawn_internal(
        shell: Option<(String, Vec<String>)>,
        working_directory: Option<PathBuf>,
    ) -> std::io::Result<Self> {
        #[cfg(test)]
        let test_permit = TerminalTestPermit::acquire();

        let cols = 80u16;
        let rows = 24u16;

        let pty_system = native_pty_system();
        let pty_pair = pty_system
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .map_err(|err| std::io::Error::other(format!("openpty failed: {err:#}")))?;

        #[cfg(unix)]
        if let Some(fd) = pty_pair.master.as_raw_fd() {
            set_cloexec(fd)?;
        }

        let mut command = match shell {
            Some((command, args)) => {
                let mut cmd = CommandBuilder::new(command);
                for arg in args {
                    cmd.arg(arg);
                }
                cmd
            }
            None => CommandBuilder::new_default_prog(),
        };

        if let Some(path) = working_directory {
            command.cwd(path);
        }

        let child = pty_pair
            .slave
            .spawn_command(command)
            .map_err(|err| std::io::Error::other(format!("spawn failed: {err:#}")))?;

        let writer = pty_pair
            .master
            .take_writer()
            .map_err(|err| std::io::Error::other(format!("writer failed: {err:#}")))?;

        let palette = ColorPalette::default();
        let config = Arc::new(TerminalConfig::new(palette));
        let terminal = Terminal::new(
            TerminalSize {
                rows: rows as usize,
                cols: cols as usize,
                pixel_width: 0,
                pixel_height: 0,
                dpi: 0,
            },
            config.clone(),
            "chatui",
            env!("CARGO_PKG_VERSION"),
            Box::new(writer),
        );

        let version = Arc::new(AtomicU64::new(0));
        let exited_shared = Arc::new(AtomicBool::new(false));
        let title_shared = Arc::new(Mutex::new(None));
        let bell_shared = Arc::new(AtomicBool::new(false));
        let content_dirty = Arc::new(AtomicBool::new(true));
        let (wakeup_sender, wakeup_receiver) = channel::unbounded();

        let terminal = Arc::new(Mutex::new(terminal));
        if let Ok(mut term) = terminal.lock() {
            term.set_notification_handler(Box::new(TerminalAlertListener {
                title: title_shared.clone(),
                bell: bell_shared.clone(),
            }));
        }

        let mut reader = pty_pair
            .master
            .try_clone_reader()
            .map_err(|err| std::io::Error::other(format!("reader failed: {err:#}")))?;
        let pty_fd = pty_pair.master.as_raw_fd();
        let term_for_reader = terminal.clone();
        let wakeup_sender_reader = wakeup_sender.clone();
        let version_reader = version.clone();
        let content_dirty_reader = content_dirty.clone();
        let exited_reader = exited_shared.clone();

        let reader_handle = std::thread::spawn(move || {
            let mut buf = [0u8; 8192];
            loop {
                match reader.read(&mut buf) {
                    Ok(0) => break,
                    Ok(n) => {
                        if let Ok(mut term) = term_for_reader.lock() {
                            term.advance_bytes(&buf[..n]);
                        }
                        version_reader.fetch_add(1, Ordering::Relaxed);
                        content_dirty_reader.store(true, Ordering::Relaxed);
                        let _ = wakeup_sender_reader.try_send(());
                    }
                    Err(err) => {
                        if err.kind() == io::ErrorKind::Interrupted {
                            continue;
                        }
                        break;
                    }
                }
            }
            exited_reader.store(true, Ordering::Relaxed);
            let _ = wakeup_sender_reader.try_send(());
        });

        Ok(Self {
            terminal,
            config,
            version,
            wakeup_sender,
            wakeup_receiver,
            exited_shared,
            title: title_shared,
            bell: bell_shared,
            content_tracker: Mutex::new(ContentTracker::default()),
            content_dirty,
            scroll_offset: 0,
            pty: Mutex::new(pty_pair.master),
            child: Mutex::new(child),
            pty_fd,
            cols,
            rows,
            reader_handle: Some(reader_handle),
            #[cfg(test)]
            _test_permit: test_permit,
        })
    }

    /// Update the terminal state based on a message.
    /// Returns true if the terminal needs to be re-rendered.
    pub fn update(&mut self, msg: TerminalMsg) -> bool {
        match msg {
            TerminalMsg::Key(key) => {
                self.handle_key(key);
                false
            }
            TerminalMsg::Mouse(event) => {
                self.handle_mouse(event);
                false
            }
            TerminalMsg::Paste(text) => {
                if let Ok(mut term) = self.terminal.lock()
                    && let Err(err) = term.send_paste(&text)
                {
                    tracing::warn!(?err, "Failed to send paste");
                }
                false
            }
            TerminalMsg::Scroll(delta) => self.scroll(delta),
            TerminalMsg::Resize { cols, rows } => {
                self.resize(cols, rows);
                true
            }
            TerminalMsg::FocusGained => {
                if let Ok(mut term) = self.terminal.lock() {
                    term.focus_changed(true);
                }
                false
            }
            TerminalMsg::FocusLost => {
                if let Ok(mut term) = self.terminal.lock() {
                    term.focus_changed(false);
                }
                false
            }
        }
    }

    fn handle_key(&self, key: Key) {
        let Some((code, mods)) = map_key(&key) else {
            return;
        };
        let mut term = match self.terminal.lock() {
            Ok(term) => term,
            Err(_) => return,
        };
        let result = match key.kind {
            KeyEventKind::Release => term.key_up(code, mods),
            KeyEventKind::Press | KeyEventKind::Repeat => term.key_down(code, mods),
        };
        if let Err(err) = result {
            tracing::warn!(?err, "Failed to send key input");
        }
    }

    fn handle_mouse(&self, event: TerminalMouseEvent) {
        let Some(mouse_event) = map_mouse(event) else {
            return;
        };
        if let Ok(mut term) = self.terminal.lock()
            && let Err(err) = term.mouse_event(mouse_event)
        {
            tracing::warn!(?err, "Failed to send mouse input");
        }
    }

    fn scroll(&mut self, delta: i32) -> bool {
        let scrollback_available = self.scrollback_rows();
        if scrollback_available == 0 {
            return false;
        }
        let current = self.scroll_offset as i32;
        let max = scrollback_available as i32;
        let next = (current - delta).clamp(0, max);
        if next as usize != self.scroll_offset {
            self.scroll_offset = next as usize;
            self.content_dirty.store(true, Ordering::Relaxed);
            true
        } else {
            false
        }
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
        self.scroll_offset = self.scroll_offset.min(self.scrollback_rows());
        self.content_dirty.store(true, Ordering::Relaxed);

        let size = PtySize {
            rows,
            cols,
            pixel_width: 0,
            pixel_height: 0,
        };

        if let Ok(pty) = self.pty.lock() {
            let _ = pty.resize(size);
        }

        if let Ok(mut term) = self.terminal.lock() {
            term.resize(TerminalSize {
                rows: rows as usize,
                cols: cols as usize,
                pixel_width: 0,
                pixel_height: 0,
                dpi: 0,
            });
        }
    }

    fn scrollback_rows(&self) -> usize {
        match self.terminal.lock() {
            Ok(term) => {
                let screen = term.screen();
                let total_rows = screen.scrollback_rows();
                total_rows.saturating_sub(screen.physical_rows)
            }
            Err(_) => 0,
        }
    }

    /// Check if the terminal process has exited.
    pub fn is_running(&self) -> bool {
        if self.exited_shared.load(Ordering::Relaxed) {
            return false;
        }
        if let Ok(mut child) = self.child.lock()
            && let Ok(Some(_)) = child.try_wait()
        {
            self.exited_shared.store(true, Ordering::Relaxed);
            return false;
        }
        true
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
        }
        tracker.version
    }

    fn compute_content_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        let (lines, _palette) = self.visible_lines_and_palette();
        for (idx, line) in lines.iter().enumerate() {
            idx.hash(&mut hasher);
            line.compute_shape_hash().hash(&mut hasher);
            line.len().hash(&mut hasher);
        }
        hasher.finish()
    }

    fn visible_lines_and_palette(&self) -> (Vec<wezterm_term::Line>, ColorPalette) {
        let term = self.terminal.lock().expect("terminal lock poisoned");
        let screen = term.screen();
        let total_rows = screen.scrollback_rows();
        let physical_rows = screen.physical_rows;
        let scrollback_available = total_rows.saturating_sub(physical_rows);
        let offset = self.scroll_offset.min(scrollback_available);
        let start = total_rows.saturating_sub(physical_rows + offset);
        let end = total_rows.saturating_sub(offset);
        (screen.lines_in_phys_range(start..end), term.palette())
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
        self.pty_fd.and_then(process::foreground_process_name)
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

    pub fn is_mouse_grabbed(&self) -> bool {
        self.terminal
            .lock()
            .map(|term| term.is_mouse_grabbed())
            .unwrap_or(false)
    }

    pub fn is_alt_screen_active(&self) -> bool {
        self.terminal
            .lock()
            .map(|term| term.is_alt_screen_active())
            .unwrap_or(false)
    }
}

impl Drop for TerminalState {
    fn drop(&mut self) {
        if let Ok(mut child) = self.child.lock() {
            let _ = child.kill();
        }
        if let Some(handle) = self.reader_handle.take() {
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
    let last_buttons = Rc::new(RefCell::new(MouseButtons::default()));
    let term = state.terminal.clone();

    node = node.on_mouse(move |event: MouseEvent| {
        let prev_buttons = *last_buttons.borrow();
        *last_buttons.borrow_mut() = event.buttons;

        let (is_mouse_grabbed, is_alt_screen_active) = term
            .lock()
            .map(|term| (term.is_mouse_grabbed(), term.is_alt_screen_active()))
            .unwrap_or((false, false));

        if event.buttons.vert_wheel {
            if is_mouse_grabbed || is_alt_screen_active {
                let button = if event.buttons.wheel_positive {
                    TerminalMouseButton::WheelUp
                } else {
                    TerminalMouseButton::WheelDown
                };
                return Some(mouse_handler(TerminalMsg::Mouse(TerminalMouseEvent {
                    kind: TerminalMouseEventKind::Press,
                    button,
                    x: event.local_x,
                    y: event.local_y,
                    ctrl: event.ctrl,
                    alt: event.alt,
                    shift: event.shift,
                    super_key: event.super_key,
                })));
            }
            let delta = if event.buttons.wheel_positive { -3 } else { 3 };
            return Some(mouse_handler(TerminalMsg::Scroll(delta)));
        }

        if event.buttons.horz_wheel {
            if is_mouse_grabbed || is_alt_screen_active {
                let button = if event.buttons.wheel_positive {
                    TerminalMouseButton::WheelRight
                } else {
                    TerminalMouseButton::WheelLeft
                };
                return Some(mouse_handler(TerminalMsg::Mouse(TerminalMouseEvent {
                    kind: TerminalMouseEventKind::Press,
                    button,
                    x: event.local_x,
                    y: event.local_y,
                    ctrl: event.ctrl,
                    alt: event.alt,
                    shift: event.shift,
                    super_key: event.super_key,
                })));
            }
            return None;
        }

        if is_mouse_grabbed {
            let is_press = event.buttons.left || event.buttons.middle || event.buttons.right;
            let was_pressed = prev_buttons.left || prev_buttons.middle || prev_buttons.right;
            if is_press {
                if let Some(button) = map_primary_button(event.buttons) {
                    return Some(mouse_handler(TerminalMsg::Mouse(TerminalMouseEvent {
                        kind: TerminalMouseEventKind::Press,
                        button,
                        x: event.local_x,
                        y: event.local_y,
                        ctrl: event.ctrl,
                        alt: event.alt,
                        shift: event.shift,
                        super_key: event.super_key,
                    })));
                }
            } else if was_pressed {
                if let Some(button) = map_primary_button(prev_buttons) {
                    return Some(mouse_handler(TerminalMsg::Mouse(TerminalMouseEvent {
                        kind: TerminalMouseEventKind::Release,
                        button,
                        x: event.local_x,
                        y: event.local_y,
                        ctrl: event.ctrl,
                        alt: event.alt,
                        shift: event.shift,
                        super_key: event.super_key,
                    })));
                }
            } else {
                return Some(mouse_handler(TerminalMsg::Mouse(TerminalMouseEvent {
                    kind: TerminalMouseEventKind::Move,
                    button: TerminalMouseButton::None,
                    x: event.local_x,
                    y: event.local_y,
                    ctrl: event.ctrl,
                    alt: event.alt,
                    shift: event.shift,
                    super_key: event.super_key,
                })));
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

fn map_primary_button(buttons: MouseButtons) -> Option<TerminalMouseButton> {
    if buttons.left {
        Some(TerminalMouseButton::Left)
    } else if buttons.middle {
        Some(TerminalMouseButton::Middle)
    } else if buttons.right {
        Some(TerminalMouseButton::Right)
    } else {
        None
    }
}

fn map_key(key: &Key) -> Option<(WezKeyCode, KeyModifiers)> {
    let code = match key.code {
        KeyCode::Char(c) => {
            if key.keypad {
                map_keypad_char(c).unwrap_or(WezKeyCode::Char(c))
            } else {
                WezKeyCode::Char(c)
            }
        }
        KeyCode::Enter => WezKeyCode::Enter,
        KeyCode::Esc => WezKeyCode::Escape,
        KeyCode::Backspace => WezKeyCode::Backspace,
        KeyCode::Left => WezKeyCode::LeftArrow,
        KeyCode::Right => WezKeyCode::RightArrow,
        KeyCode::Up => WezKeyCode::UpArrow,
        KeyCode::Down => WezKeyCode::DownArrow,
        KeyCode::PageUp => WezKeyCode::PageUp,
        KeyCode::PageDown => WezKeyCode::PageDown,
        KeyCode::Home => WezKeyCode::Home,
        KeyCode::End => WezKeyCode::End,
        KeyCode::Tab => WezKeyCode::Tab,
        KeyCode::Function(n) => WezKeyCode::Function(n),
        KeyCode::Insert => WezKeyCode::Insert,
        KeyCode::Delete => WezKeyCode::Delete,
        KeyCode::CapsLock => WezKeyCode::CapsLock,
        KeyCode::ScrollLock => WezKeyCode::ScrollLock,
        KeyCode::NumLock => WezKeyCode::NumLock,
        KeyCode::PrintScreen => WezKeyCode::PrintScreen,
        KeyCode::Pause => WezKeyCode::Pause,
        KeyCode::Menu => WezKeyCode::Menu,
        KeyCode::Modifier(modifier) => match modifier {
            ModifierKeyCode::LeftShift => WezKeyCode::LeftShift,
            ModifierKeyCode::LeftControl => WezKeyCode::LeftControl,
            ModifierKeyCode::LeftAlt => WezKeyCode::LeftAlt,
            ModifierKeyCode::LeftSuper => WezKeyCode::LeftWindows,
            ModifierKeyCode::LeftHyper => WezKeyCode::Hyper,
            ModifierKeyCode::LeftMeta => WezKeyCode::Meta,
            ModifierKeyCode::RightShift => WezKeyCode::RightShift,
            ModifierKeyCode::RightControl => WezKeyCode::RightControl,
            ModifierKeyCode::RightAlt => WezKeyCode::RightAlt,
            ModifierKeyCode::RightSuper => WezKeyCode::RightWindows,
            ModifierKeyCode::RightHyper => WezKeyCode::Hyper,
            ModifierKeyCode::RightMeta => WezKeyCode::Meta,
        },
        KeyCode::Media(media) => match media {
            MediaKeyCode::Play | MediaKeyCode::PlayPause | MediaKeyCode::Pause => {
                WezKeyCode::MediaPlayPause
            }
            MediaKeyCode::Stop => WezKeyCode::MediaStop,
            MediaKeyCode::FastForward => return None,
            MediaKeyCode::Rewind => return None,
            MediaKeyCode::TrackNext => WezKeyCode::MediaNextTrack,
            MediaKeyCode::TrackPrevious => WezKeyCode::MediaPrevTrack,
            MediaKeyCode::Record => return None,
            MediaKeyCode::LowerVolume => WezKeyCode::VolumeDown,
            MediaKeyCode::RaiseVolume => WezKeyCode::VolumeUp,
            MediaKeyCode::MuteVolume => WezKeyCode::VolumeMute,
        },
    };

    let mut modifiers = KeyModifiers::NONE;
    if key.shift {
        modifiers |= KeyModifiers::SHIFT;
    }
    if key.alt {
        modifiers |= KeyModifiers::ALT;
    }
    if key.ctrl {
        modifiers |= KeyModifiers::CTRL;
    }
    if key.super_key {
        modifiers |= KeyModifiers::SUPER;
    }
    Some((code, modifiers))
}

fn map_keypad_char(c: char) -> Option<WezKeyCode> {
    match c {
        '0' => Some(WezKeyCode::Numpad0),
        '1' => Some(WezKeyCode::Numpad1),
        '2' => Some(WezKeyCode::Numpad2),
        '3' => Some(WezKeyCode::Numpad3),
        '4' => Some(WezKeyCode::Numpad4),
        '5' => Some(WezKeyCode::Numpad5),
        '6' => Some(WezKeyCode::Numpad6),
        '7' => Some(WezKeyCode::Numpad7),
        '8' => Some(WezKeyCode::Numpad8),
        '9' => Some(WezKeyCode::Numpad9),
        '.' => Some(WezKeyCode::Decimal),
        '/' => Some(WezKeyCode::Divide),
        '*' => Some(WezKeyCode::Multiply),
        '-' => Some(WezKeyCode::Subtract),
        '+' => Some(WezKeyCode::Add),
        _ => None,
    }
}

fn map_mouse(event: TerminalMouseEvent) -> Option<WezMouseEvent> {
    let button = match event.button {
        TerminalMouseButton::None => WezMouseButton::None,
        TerminalMouseButton::Left => WezMouseButton::Left,
        TerminalMouseButton::Middle => WezMouseButton::Middle,
        TerminalMouseButton::Right => WezMouseButton::Right,
        TerminalMouseButton::WheelUp => WezMouseButton::WheelUp(1),
        TerminalMouseButton::WheelDown => WezMouseButton::WheelDown(1),
        TerminalMouseButton::WheelLeft => WezMouseButton::WheelLeft(1),
        TerminalMouseButton::WheelRight => WezMouseButton::WheelRight(1),
    };

    let kind = match event.kind {
        TerminalMouseEventKind::Press => WezMouseEventKind::Press,
        TerminalMouseEventKind::Release => WezMouseEventKind::Release,
        TerminalMouseEventKind::Move => WezMouseEventKind::Move,
    };

    let mut modifiers = KeyModifiers::NONE;
    if event.shift {
        modifiers |= KeyModifiers::SHIFT;
    }
    if event.alt {
        modifiers |= KeyModifiers::ALT;
    }
    if event.ctrl {
        modifiers |= KeyModifiers::CTRL;
    }
    if event.super_key {
        modifiers |= KeyModifiers::SUPER;
    }

    Some(WezMouseEvent {
        kind,
        x: event.x as usize,
        y: event.y as i64,
        x_pixel_offset: 0,
        y_pixel_offset: 0,
        button,
        modifiers,
    })
}

fn palette_to_srgba(color: Rgba) -> SrgbaTuple {
    RgbColor::new_8bpc(color.r, color.g, color.b).into()
}

fn rgba_from_srgba(color: SrgbaTuple) -> Rgba {
    let to_u8 = |value: f32| (value * 255.0).round().clamp(0.0, 255.0) as u8;
    Rgba::new(
        to_u8(color.0),
        to_u8(color.1),
        to_u8(color.2),
        to_u8(color.3),
    )
}

fn wezterm_palette_from(palette: &Palette) -> ColorPalette {
    let mut wez_palette = ColorPalette::default();
    for (idx, color) in palette.colors.iter().enumerate() {
        wez_palette.colors.0[idx] = palette_to_srgba(*color);
    }
    wez_palette.foreground = palette_to_srgba(palette.foreground);
    wez_palette.background = palette_to_srgba(palette.background);
    wez_palette
}

/// Default key bindings for terminal input with wezterm-term encoding.
///
/// # Arguments
/// * `key` - The key event to handle
/// * `on_input` - Function to convert terminal messages to application messages
pub fn default_terminal_keybindings<Msg>(
    key: Key,
    on_input: impl Fn(TerminalMsg) -> Msg,
) -> Option<Msg> {
    map_key(&key).map(|_| on_input(TerminalMsg::Key(key)))
}

/// Renderable widget for the terminal.
struct TerminalRenderable {
    term: Arc<Mutex<Terminal>>,
    config: Arc<TerminalConfig>,
    cols: u16,
    rows: u16,
    scroll_offset: usize,
    version: u64,
    focused: bool,
}

impl std::fmt::Debug for TerminalRenderable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TerminalRenderable")
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .field("scroll_offset", &self.scroll_offset)
            .field("version", &self.version)
            .field("focused", &self.focused)
            .finish_non_exhaustive()
    }
}

impl TerminalRenderable {
    fn new(state: &TerminalState, focused: bool) -> Self {
        Self {
            term: state.terminal.clone(),
            config: state.config.clone(),
            cols: state.cols,
            rows: state.rows,
            scroll_offset: state.scroll_offset,
            version: state.version(),
            focused,
        }
    }

    fn convert_cursor_shape(shape: WezCursorShape) -> CursorShape {
        match shape {
            WezCursorShape::BlinkingBlock | WezCursorShape::Default => CursorShape::BlinkingBlock,
            WezCursorShape::SteadyBlock => CursorShape::SteadyBlock,
            WezCursorShape::BlinkingUnderline => CursorShape::BlinkingUnderline,
            WezCursorShape::SteadyUnderline => CursorShape::SteadyUnderline,
            WezCursorShape::BlinkingBar => CursorShape::BlinkingBar,
            WezCursorShape::SteadyBar => CursorShape::SteadyBar,
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
            && self.scroll_offset == o.scroll_offset
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

        self.config.set_palette(wezterm_palette_from(ctx.palette()));

        let (lines, palette, cursor) = {
            let term = self.term.lock().expect("terminal lock poisoned");
            let screen = term.screen();
            let total_rows = screen.scrollback_rows();
            let physical_rows = screen.physical_rows;
            let scrollback_available = total_rows.saturating_sub(physical_rows);
            let offset = self.scroll_offset.min(scrollback_available);
            let start = total_rows.saturating_sub(physical_rows + offset);
            let end = total_rows.saturating_sub(offset);
            let lines = screen.lines_in_phys_range(start..end);
            (lines, term.palette(), term.cursor_pos())
        };

        let default_fg = rgba_from_srgba(palette.foreground);
        let default_bg = rgba_from_srgba(palette.background);

        let mut default_attrs = CellAttributes::default();
        default_attrs.set_foreground(default_fg);
        default_attrs.set_background(default_bg);

        for row in 0..area.height {
            for col in 0..area.width {
                ctx.write_char(area.x + col, area.y + row, ' ', &default_attrs);
            }
        }

        for (row_idx, line) in lines.iter().enumerate() {
            if row_idx >= area.height {
                break;
            }
            for cell in line.visible_cells() {
                let x = cell.cell_index();
                if x >= area.width {
                    continue;
                }
                let mut attrs = CellAttributes::default();
                let cell_attrs = cell.attrs();

                let fg = palette.resolve_fg(cell_attrs.foreground());
                let bg = palette.resolve_bg(cell_attrs.background());

                attrs.set_foreground(rgba_from_srgba(fg));
                attrs.set_background(rgba_from_srgba(bg));
                match cell_attrs.intensity() {
                    wezterm_term::Intensity::Bold => attrs.set_bold(true),
                    wezterm_term::Intensity::Half => attrs.set_dim(true),
                    _ => {}
                }
                if cell_attrs.reverse() {
                    attrs.set_reverse(true);
                }

                ctx.write_text(area.x + x, area.y + row_idx, cell.str(), &attrs);
            }
        }

        if self.focused && self.scroll_offset == 0 && cursor.visibility == CursorVisibility::Visible
        {
            let cursor_x = cursor.x;
            let cursor_y = cursor.y;
            if cursor_y >= 0 && cursor_y < area.height as i64 && cursor_x < area.width {
                ctx.set_cursor(
                    area.x + cursor_x,
                    area.y + cursor_y as usize,
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
    fn default_keybindings_maps_key_event() {
        let key = Key::new(KeyCode::Char('a'));
        let msg = default_terminal_keybindings(key, |term_msg| term_msg);
        assert!(matches!(msg, Some(TerminalMsg::Key(_))));
    }

    #[test]
    fn version_increments_on_terminal_output() {
        use std::time::Duration;

        let state = TerminalState::spawn("echo", &["hello"]).expect("failed to spawn terminal");
        let version1 = state.version();
        let rx = state.wakeup_receiver();

        for _ in 0..20 {
            if rx.try_recv().is_ok() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        let version2 = state.version();

        assert!(
            version2 > version1,
            "version should increment after terminal output"
        );
    }

    #[test]
    fn renderable_eq_detects_version_change() {
        use std::time::Duration;

        let state = TerminalState::spawn("echo", &["test"]).expect("failed to spawn terminal");
        let render1 = TerminalRenderable::new(&state, true);
        let rx = state.wakeup_receiver();
        for _ in 0..20 {
            if rx.try_recv().is_ok() {
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        std::thread::sleep(Duration::from_millis(50));
        let render2 = TerminalRenderable::new(&state, true);

        assert!(
            !render1.eq(&render2),
            "renderable should differ after output"
        );
    }

    #[test]
    fn update_returns_false_for_key_input() {
        let mut state = TerminalState::new().expect("failed to create terminal");
        let key = Key::new(KeyCode::Char('a'));
        let result = state.update(TerminalMsg::Key(key));
        assert!(!result);
    }
}
