//! Terminal widget using alacritty_terminal for terminal emulation.
//!
//! This module provides a terminal widget that can be embedded in the TUI.
//! It uses alacritty_terminal for the actual terminal emulation.

use std::any::Any;
use std::borrow::Cow;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use alacritty_terminal::event::{Event as TermEvent, EventListener, WindowSize};
use alacritty_terminal::event_loop::{EventLoop, EventLoopSender, Msg};
use alacritty_terminal::grid::Dimensions;
use alacritty_terminal::sync::FairMutex;
use alacritty_terminal::term::color::Colors;
use alacritty_terminal::term::test::TermSize;
use alacritty_terminal::term::{self, Config as TermConfig, Term};
use alacritty_terminal::tty::{self, Options as PtyOptions, Shell};
use alacritty_terminal::vte::ansi::{self, CursorShape as AlacCursorShape, Handler, NamedColor};
use smol::channel::{self, Receiver, Sender};

use crate::buffer::{CellAttributes, CursorShape};
use crate::dom::{Node, Renderable, renderable};
use crate::event::{Key, KeyCode, MouseEvent};
use crate::palette::Rgba;
use crate::render::RenderContext;

/// Messages for terminal state updates.
#[derive(Clone, Debug)]
pub enum TerminalMsg {
    /// Raw input bytes to write to the PTY
    Input(Vec<u8>),
    /// Scroll by delta lines (positive = down, negative = up)
    Scroll(i32),
    /// Resize the terminal to new dimensions
    Resize { cols: u16, rows: u16 },
}

/// Event listener that tracks terminal content changes.
#[derive(Clone)]
struct TerminalEventListener {
    /// Version counter incremented on each wakeup event.
    version: Arc<AtomicU64>,
    /// Channel to notify when terminal content changes.
    wakeup_sender: Sender<()>,
}

impl TerminalEventListener {
    fn new(version: Arc<AtomicU64>, wakeup_sender: Sender<()>) -> Self {
        Self {
            version,
            wakeup_sender,
        }
    }
}

impl EventListener for TerminalEventListener {
    fn send_event(&self, event: TermEvent) {
        if matches!(event, TermEvent::Wakeup) {
            self.version.fetch_add(1, Ordering::Relaxed);
            let result = self.wakeup_sender.try_send(());
            tracing::info!("Wakeup event sent: {:?}", result);
        }
    }
}

/// State for the embedded terminal.
pub struct TerminalState {
    term: Arc<FairMutex<Term<TerminalEventListener>>>,
    sender: EventLoopSender,
    /// Content version counter, incremented on each terminal wakeup.
    version: Arc<AtomicU64>,
    /// Receiver for wakeup notifications from the terminal.
    wakeup_receiver: Receiver<()>,
    cols: u16,
    rows: u16,
    scroll_offset: i32,
    exited: bool,
}

impl std::fmt::Debug for TerminalState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TerminalState")
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .field("scroll_offset", &self.scroll_offset)
            .field("exited", &self.exited)
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
        let shell = shell.map(|s| Shell::new(s.to_string(), Vec::new()));
        Self::spawn_internal(shell)
    }

    /// Spawn a command in the terminal.
    pub fn spawn(command: &str, args: &[&str]) -> std::io::Result<Self> {
        let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
        let shell = Shell::new(command.to_string(), args);
        Self::spawn_internal(Some(shell))
    }

    fn spawn_internal(shell: Option<Shell>) -> std::io::Result<Self> {
        let cols = 80u16;
        let rows = 24u16;

        let pty_config = PtyOptions {
            shell,
            working_directory: None,
            drain_on_exit: false,
            env: Default::default(),
        };

        let window_size = WindowSize {
            num_cols: cols,
            num_lines: rows,
            cell_width: 1,
            cell_height: 1,
        };

        let pty = tty::new(&pty_config, window_size, 0)?;

        let version = Arc::new(AtomicU64::new(0));
        let (wakeup_sender, wakeup_receiver) = channel::unbounded();
        let event_listener = TerminalEventListener::new(version.clone(), wakeup_sender);
        let term_size = TermSize::new(cols as usize, rows as usize);
        let term = Term::new(TermConfig::default(), &term_size, event_listener.clone());
        let term = Arc::new(FairMutex::new(term));

        let event_loop = EventLoop::new(term.clone(), event_listener, pty, false, false)
            .map_err(|e| std::io::Error::other(format!("EventLoop error: {e:?}")))?;

        let sender = event_loop.channel();
        let _join_handle = event_loop.spawn();

        Ok(Self {
            term,
            sender,
            version,
            wakeup_receiver,
            cols,
            rows,
            scroll_offset: 0,
            exited: false,
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
                let term = self.term.lock();
                let history_size = term.history_size() as i32;
                drop(term);

                let new_offset = (self.scroll_offset + delta).clamp(-history_size, 0);
                if new_offset != self.scroll_offset {
                    self.scroll_offset = new_offset;
                    true
                } else {
                    false
                }
            }
            TerminalMsg::Resize { cols, rows } => {
                self.resize(cols, rows);
                true
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
        !self.exited
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

    node = node.on_mouse(move |event: MouseEvent| {
        if event.buttons.vert_wheel {
            let delta = if event.buttons.wheel_positive { -3 } else { 3 };
            Some(mouse_handler(TerminalMsg::Scroll(delta)))
        } else {
            None
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

/// Translate a key event to terminal input bytes.
pub fn key_to_input(key: Key) -> Option<Vec<u8>> {
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
    };

    Some(bytes)
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
    scroll_offset: i32,
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
            .field("scroll_offset", &self.scroll_offset)
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
            scroll_offset: state.scroll_offset,
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
                taffy::AvailableSpace::MinContent | taffy::AvailableSpace::MaxContent => {
                    self.cols as f32
                }
            });

        let height = known_dimensions
            .height
            .unwrap_or(match available_space.height {
                taffy::AvailableSpace::Definite(h) => h,
                taffy::AvailableSpace::MinContent | taffy::AvailableSpace::MaxContent => {
                    self.rows as f32
                }
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
        for cell in content.display_iter {
            let point = cell.point;
            let x = point.column.0;
            let y = point.line.0 - self.scroll_offset;

            if y < 0 || y >= area.height as i32 {
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
            ctx.write_char(area.x + x, area.y + y as usize, ch, &attrs);
        }

        // Render cursor only if focused
        if self.focused {
            let cursor_x = cursor.point.column.0;
            let cursor_y = cursor.point.line.0 - self.scroll_offset;

            if cursor_y >= 0
                && cursor_y < area.height as i32
                && cursor_x < area.width
                && cursor.shape != AlacCursorShape::Hidden
            {
                tracing::info!(
                    "cursor {}:{}",
                    area.x + cursor_x,
                    area.y + cursor_y as usize
                );
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
}
