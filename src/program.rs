use std::boxed::Box;
use std::io::Write;
use std::time::{Duration, Instant};

use crate::buffer::DoubleBuffer;
use crate::dom::Node;
use crate::error::ProgramError;
use crate::event::{Event, Key, KeyCode, MouseButtons, MouseEvent, Size};
use crate::palette::Palette;
use crate::render::Renderer;

use taffy::compute_root_layout;
use termina::{PlatformTerminal, Terminal};
use tracing::info;

pub type UpdateFn<Model, Msg> = Box<dyn FnMut(&mut Model, Msg) -> Transition>;
pub type ViewFn<Model, Msg> = Box<dyn Fn(&Model) -> Node<Msg>>;
pub type EventFn<Msg> = Box<dyn Fn(Event) -> Option<Msg>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Transition {
    Continue,
    Quit,
}

pub struct Program<Model, Msg> {
    model: Model,
    update: UpdateFn<Model, Msg>,
    view: ViewFn<Model, Msg>,
    event_mapper: EventFn<Msg>,
    current_size: Size,
    current_view: Option<Node<Msg>>,
    last_mouse_buttons: MouseButtons,
    last_click: Option<LastClick>,
    palette: Palette,
}

impl<Model, Msg> Program<Model, Msg> {
    pub fn new(
        model: Model,
        update: impl FnMut(&mut Model, Msg) -> Transition + 'static,
        view: impl Fn(&Model) -> Node<Msg> + 'static,
    ) -> Self {
        Self {
            model,
            update: Box::new(update),
            view: Box::new(view),
            event_mapper: Box::new(|_| None),
            current_size: Size::new(DEFAULT_WIDTH, DEFAULT_HEIGHT),
            current_view: None,
            last_mouse_buttons: MouseButtons::default(),
            last_click: None,
            palette: Palette::default(),
        }
    }

    pub fn map_event(mut self, event_mapper: impl Fn(Event) -> Option<Msg> + 'static) -> Self {
        self.event_mapper = Box::new(event_mapper);
        self
    }

    pub fn run(mut self) -> Result<(), ProgramError> {
        let mut terminal = PlatformTerminal::new()
            .map_err(|e| ProgramError::terminal(format!("Failed to create terminal: {}", e)))?;

        // Enter raw mode for the duration of the loop.
        terminal
            .enter_raw_mode()
            .map_err(|e| ProgramError::terminal(format!("Failed to enter raw mode: {}", e)))?;

        // Enter alternate screen and enable mouse tracking
        write!(
            terminal,
            "\x1b[?1049h\x1b[?1000h\x1b[?1002h\x1b[?1003h\x1b[?1006h", // Enable SGR mouse mode
        )
        .map_err(|e| ProgramError::terminal(format!("Failed to setup terminal: {}", e)))?;
        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush terminal: {}", e)))?;

        let result = self.event_loop(&mut terminal);

        // Always attempt to restore terminal state.
        let _ = write!(
            terminal,
            "\x1b[?1006l\x1b[?1003l\x1b[?1002l\x1b[?1000l\x1b[?1049l", // Exit alternate screen
        );
        let _ = terminal.flush();
        let _ = terminal.enter_cooked_mode();

        result
    }

    pub fn send(&mut self, event: Event) -> Result<Transition, ProgramError> {
        self.process_event(event, None)
    }

    fn event_loop(&mut self, terminal: &mut PlatformTerminal) -> Result<(), ProgramError> {
        let dimensions = terminal
            .get_dimensions()
            .map_err(|e| ProgramError::terminal(format!("Failed to get dimensions: {}", e)))?;
        let mut buffer = DoubleBuffer::new(dimensions.cols as usize, dimensions.rows as usize);

        self.render_view(&mut buffer)?;
        buffer
            .flush(terminal)
            .map_err(|e| ProgramError::terminal(format!("Failed to flush buffer: {:?}", e)))?;

        // Hide cursor
        write!(terminal, "\x1b[?25l")
            .map_err(|e| ProgramError::terminal(format!("Failed to hide cursor: {}", e)))?;
        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush terminal: {}", e)))?;

        loop {
            // Check for terminal resize
            let new_dimensions = terminal
                .get_dimensions()
                .map_err(|e| ProgramError::terminal(format!("Failed to get dimensions: {}", e)))?;
            let (old_cols, old_rows) = buffer.dimensions();
            if new_dimensions.cols as usize != old_cols || new_dimensions.rows as usize != old_rows
            {
                let (transition, needs_render) = self.handle_event(Event::Resize(Size {
                    width: new_dimensions.cols,
                    height: new_dimensions.rows,
                }));
                if needs_render {
                    self.render_view(&mut buffer)?;
                    buffer.flush(terminal)?;
                }
                if matches!(transition, Transition::Quit) {
                    break;
                }
            }

            // Poll for events with no timeout (blocking read)
            if terminal
                .poll(|_| true, None)
                .map_err(|e| ProgramError::event(format!("Failed to poll events: {}", e)))?
            {
                let event = terminal
                    .read(|_| true)
                    .map_err(|e| ProgramError::event(format!("Failed to read event: {}", e)))?;

                let mut needs_render = false;
                let mut should_quit = false;

                if let Some(converted_event) = convert_input_event(event) {
                    let (transition, render_flag) = self.handle_event(converted_event);
                    needs_render |= render_flag;
                    if matches!(transition, Transition::Quit) {
                        should_quit = true;
                    }
                }

                // Read any additional pending events with zero timeout
                while terminal
                    .poll(|_| true, Some(Duration::from_millis(0)))
                    .map_err(|e| ProgramError::event(format!("Failed to poll events: {}", e)))?
                {
                    let event = terminal
                        .read(|_| true)
                        .map_err(|e| ProgramError::event(format!("Failed to read event: {}", e)))?;

                    if let Some(converted_event) = convert_input_event(event) {
                        let (transition, render_flag) = self.handle_event(converted_event);
                        needs_render |= render_flag;
                        if matches!(transition, Transition::Quit) {
                            should_quit = true;
                            break;
                        }
                    }
                }

                if needs_render {
                    self.render_view(&mut buffer)?;
                    // raw escape codes for `Synchronized Output` start
                    write!(terminal, "\x1b[?2026h").map_err(|e| {
                        ProgramError::terminal(format!("Failed to write sync start: {}", e))
                    })?;
                    buffer.flush(terminal)?;
                    // raw escape codes for `Synchronized Output` end
                    write!(terminal, "\x1b[?2026l").map_err(|e| {
                        ProgramError::terminal(format!("Failed to write sync end: {}", e))
                    })?;
                    terminal.flush().map_err(|e| {
                        ProgramError::terminal(format!("Failed to flush terminal: {}", e))
                    })?;
                }

                if should_quit {
                    break;
                }
            }
        }

        Ok(())
    }

    fn process_event(
        &mut self,
        event: Event,
        buffer: Option<&mut DoubleBuffer>,
    ) -> Result<Transition, ProgramError> {
        let (transition, needs_render) = self.handle_event(event);
        if needs_render {
            if let Some(buf) = buffer {
                self.render_view(buf)?;
            } else {
                self.rebuild_view();
            }
        }
        Ok(transition)
    }

    fn handle_event(&mut self, event: Event) -> (Transition, bool) {
        let mut needs_render = matches!(event, Event::Resize(_));

        if let Event::Resize(size) = &event {
            self.current_size = *size;
        }

        let mut msg = (self.event_mapper)(event.clone());

        if let Event::Mouse(mouse_event) = &event {
            let click_msg = self.handle_mouse_event(mouse_event);
            if msg.is_none() {
                msg = click_msg;
            }
        }

        let transition = if let Some(message) = msg {
            let transition = (self.update)(&mut self.model, message);
            if matches!(transition, Transition::Continue) {
                needs_render = true;
            }
            transition
        } else {
            Transition::Continue
        };

        (transition, needs_render)
    }

    fn render_view(&mut self, buffer: &mut DoubleBuffer) -> Result<(), ProgramError> {
        self.rebuild_view();
        if let Some(current_view) = self.current_view.as_ref() {
            Renderer::new(buffer, &self.palette).render(current_view, self.current_size)
        } else {
            Ok(())
        }
    }

    fn rebuild_view(&mut self) {
        let new_view = (self.view)(&self.model);

        if let Some(current_view) = self.current_view.as_mut() {
            if let crate::dom::patch::PatchResult::Replaced(replacement) =
                crate::dom::patch::patch(current_view, new_view)
            {
                *current_view = *replacement;
            }
        } else {
            self.current_view = Some(new_view);
        }

        if let Some(view) = self.current_view.as_mut() {
            info!("computing layout with: {:?}", self.current_size);
            compute_root_layout(
                view,
                u64::MAX.into(),
                taffy::Size {
                    width: taffy::AvailableSpace::Definite(self.current_size.width as f32),
                    height: taffy::AvailableSpace::Definite(self.current_size.height as f32),
                },
            );
            crate::dom::rounding::round_layout(view);
            crate::dom::print::print_tree(view);

            // TODO early exit if Transition::Quit
            let mut quit = false;
            view.report_changed(&mut |node: &Node<Msg>| {
                if let Some(resize_callback) = &node.on_resize
                    && let Some(msg) = resize_callback(&node.layout_state.layout)
                {
                    match (self.update)(&mut self.model, msg) {
                        Transition::Quit => quit = true,
                        Transition::Continue => {}
                    }
                }
            });
        }
    }

    fn handle_mouse_event(&mut self, event: &MouseEvent) -> Option<Msg> {
        let previous = self.last_mouse_buttons;
        self.last_mouse_buttons = event.buttons;

        let mut enriched = *event;
        enriched.click_count = 0;

        if event.buttons.left && !previous.left {
            let timestamp = Instant::now();
            let is_double = self
                .last_click
                .as_ref()
                .map(|last| {
                    timestamp.duration_since(last.timestamp) <= DOUBLE_CLICK_INTERVAL
                        && last.x == event.x
                        && last.y == event.y
                })
                .unwrap_or(false);

            enriched.click_count = if is_double { 2 } else { 1 };
            self.last_click = Some(LastClick {
                timestamp,
                x: event.x,
                y: event.y,
            });
        }

        if let Some(view) = self.current_view.as_ref()
            && let Some(msg) = view.hit_test(event.x, event.y, &mut |target| {
                target.mouse_message(enriched)
            })
        {
            return Some(msg);
        }

        None
    }
}

const DEFAULT_WIDTH: u16 = 80;
const DEFAULT_HEIGHT: u16 = 24;
const DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(400);

struct LastClick {
    timestamp: Instant,
    x: u16,
    y: u16,
}

fn convert_input_event(input: termina::Event) -> Option<Event> {
    tracing::debug!("termina input event: {:?}", input);
    match input {
        termina::Event::Key(key) => map_key_event(key),
        termina::Event::Mouse(mouse) => map_mouse_event(mouse),
        termina::Event::WindowResized(size) => Some(Event::Resize(Size::new(size.cols, size.rows))),
        _ => None,
    }
}

fn map_mouse_event(mouse: termina::event::MouseEvent) -> Option<Event> {
    use termina::event::{MouseButton, MouseEventKind};

    let mut buttons = MouseButtons::default();

    match mouse.kind {
        MouseEventKind::Down(btn) => {
            // Button press - set the button as pressed
            match btn {
                MouseButton::Left => buttons.left = true,
                MouseButton::Right => buttons.right = true,
                MouseButton::Middle => buttons.middle = true,
            }
        }
        MouseEventKind::Up(btn) => {
            // Button release - all buttons are now released
            // We still need to report which button was released for the event
            // but we'll keep buttons at default (all false)
            match btn {
                MouseButton::Left => {
                    // Report this as a mouse event with left button having just been released
                    // The program's handle_mouse_event will detect the state change
                }
                MouseButton::Right => {}
                MouseButton::Middle => {}
            }
        }
        MouseEventKind::Drag(btn) => {
            // Dragging - the button is pressed
            match btn {
                MouseButton::Left => buttons.left = true,
                MouseButton::Right => buttons.right = true,
                MouseButton::Middle => buttons.middle = true,
            }
        }
        MouseEventKind::Moved => {
            // Just movement, no buttons
        }
        MouseEventKind::ScrollDown => {
            buttons.vert_wheel = true;
            buttons.wheel_positive = false;
        }
        MouseEventKind::ScrollUp => {
            buttons.vert_wheel = true;
            buttons.wheel_positive = true;
        }
        MouseEventKind::ScrollLeft => {
            buttons.horz_wheel = true;
            buttons.wheel_positive = false;
        }
        MouseEventKind::ScrollRight => {
            buttons.horz_wheel = true;
            buttons.wheel_positive = true;
        }
    }

    Some(Event::Mouse(MouseEvent::with_modifiers(
        mouse.column,
        mouse.row,
        buttons,
        mouse.modifiers.contains(termina::event::Modifiers::CONTROL),
        mouse.modifiers.contains(termina::event::Modifiers::ALT),
        mouse.modifiers.contains(termina::event::Modifiers::SHIFT),
    )))
}

fn map_key_event(key: termina::event::KeyEvent) -> Option<Event> {
    let code = map_key_code(key.code)?;
    let event_key = Key {
        code,
        ctrl: key.modifiers.contains(termina::event::Modifiers::CONTROL),
        alt: key.modifiers.contains(termina::event::Modifiers::ALT),
        shift: key.modifiers.contains(termina::event::Modifiers::SHIFT),
    };
    Some(Event::Key(event_key))
}

fn map_key_code(code: termina::event::KeyCode) -> Option<KeyCode> {
    use termina::event::KeyCode as TnKeyCode;

    match code {
        TnKeyCode::Char(c) => Some(KeyCode::Char(c)),
        TnKeyCode::Enter => Some(KeyCode::Enter),
        TnKeyCode::Escape => Some(KeyCode::Esc),
        TnKeyCode::Backspace => Some(KeyCode::Backspace),
        TnKeyCode::Left => Some(KeyCode::Left),
        TnKeyCode::Right => Some(KeyCode::Right),
        TnKeyCode::Up => Some(KeyCode::Up),
        TnKeyCode::Down => Some(KeyCode::Down),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dom::{column, text};

    #[derive(Default)]
    struct CounterModel {
        count: i32,
    }

    enum Msg {
        Increment,
        Quit,
    }

    fn update(model: &mut CounterModel, msg: Msg) -> Transition {
        match msg {
            Msg::Increment => {
                model.count += 1;
                Transition::Continue
            }
            Msg::Quit => Transition::Quit,
        }
    }

    fn view(model: &CounterModel) -> Node<Msg> {
        column(vec![text(format!("count: {}", model.count))])
    }

    #[test]
    fn send_updates_model_and_rerenders() {
        let mut program =
            Program::new(CounterModel::default(), update, view).map_event(|event| match event {
                Event::Key(key) if matches!(key.code, KeyCode::Char('+')) => Some(Msg::Increment),
                _ => None,
            });
        let mut buffer = DoubleBuffer::new(10, 2);

        let transition = program
            .process_event(Event::key(KeyCode::Char('+')), Some(&mut buffer))
            .expect("send should succeed");
        assert_eq!(transition, Transition::Continue);

        let screen = buffer.to_string();
        assert!(screen.contains("count: 1"));
    }

    #[test]
    fn send_produces_quit_transition() {
        let mut program =
            Program::new(CounterModel::default(), update, view).map_event(|event| match event {
                Event::Key(key) if matches!(key.code, KeyCode::Char('q')) => Some(Msg::Quit),
                _ => None,
            });

        let transition = program
            .send(Event::key(KeyCode::Char('q')))
            .expect("send should succeed");
        assert_eq!(transition, Transition::Quit);
    }

    #[derive(Default)]
    struct ClickModel {
        clicks: usize,
    }

    enum ClickMsg {
        Click,
    }

    fn click_update(model: &mut ClickModel, msg: ClickMsg) -> Transition {
        match msg {
            ClickMsg::Click => {
                model.clicks += 1;
                Transition::Continue
            }
        }
    }

    fn click_view(_model: &ClickModel) -> Node<ClickMsg> {
        text("button").on_click(|| ClickMsg::Click)
    }

    #[test]
    fn mouse_click_triggers_on_click_handler() {
        let mut program = Program::new(ClickModel::default(), click_update, click_view);
        let mut buffer = DoubleBuffer::new(4, 2);

        program
            .process_event(Event::resize(4, 2), Some(&mut buffer))
            .expect("resize should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::new(true, false, false)),
                Some(&mut buffer),
            )
            .expect("mouse down should succeed");
        assert_eq!(program.model.clicks, 1);

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::default()),
                Some(&mut buffer),
            )
            .expect("mouse move up should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::new(true, false, false)),
                Some(&mut buffer),
            )
            .expect("second click should succeed");
        assert_eq!(program.model.clicks, 1);

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::default()),
                Some(&mut buffer),
            )
            .expect("mouse up should succeed");

        program
            .process_event(
                Event::mouse(1, 0, MouseButtons::new(true, false, false)),
                Some(&mut buffer),
            )
            .expect("third click should succeed");
        assert_eq!(program.model.clicks, 2);
    }

    #[derive(Default)]
    struct DoubleClickModel {
        single: usize,
        double: usize,
    }

    enum DoubleClickMsg {
        Single,
        Double,
    }

    fn double_click_update(model: &mut DoubleClickModel, msg: DoubleClickMsg) -> Transition {
        match msg {
            DoubleClickMsg::Single => {
                model.single += 1;
                Transition::Continue
            }
            DoubleClickMsg::Double => {
                model.double += 1;
                Transition::Continue
            }
        }
    }

    fn double_click_view(_model: &DoubleClickModel) -> Node<DoubleClickMsg> {
        text("button").on_mouse(|event| {
            if event.is_double_click() {
                Some(DoubleClickMsg::Double)
            } else if event.is_single_click() {
                Some(DoubleClickMsg::Single)
            } else {
                None
            }
        })
    }

    #[test]
    fn double_click_triggers_handler() {
        let mut program = Program::new(
            DoubleClickModel::default(),
            double_click_update,
            double_click_view,
        );
        let mut buffer = DoubleBuffer::new(4, 2);

        program
            .process_event(Event::resize(4, 2), Some(&mut buffer))
            .expect("resize should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::new(true, false, false)),
                Some(&mut buffer),
            )
            .expect("first click should succeed");
        assert_eq!(program.model.single, 1);
        assert_eq!(program.model.double, 0);

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::default()),
                Some(&mut buffer),
            )
            .expect("mouse up should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::new(true, false, false)),
                Some(&mut buffer),
            )
            .expect("second click should succeed");
        assert_eq!(program.model.single, 1);
        assert_eq!(program.model.double, 1);
    }

    #[test]
    fn double_click_requires_same_position() {
        let mut program = Program::new(
            DoubleClickModel::default(),
            double_click_update,
            double_click_view,
        );
        let mut buffer = DoubleBuffer::new(4, 2);

        program
            .process_event(Event::resize(4, 2), Some(&mut buffer))
            .expect("resize should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::new(true, false, false)),
                Some(&mut buffer),
            )
            .expect("first click should succeed");
        assert_eq!(program.model.single, 1);
        assert_eq!(program.model.double, 0);

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::default()),
                Some(&mut buffer),
            )
            .expect("mouse up should succeed");

        program
            .process_event(
                Event::mouse(1, 0, MouseButtons::new(true, false, false)),
                Some(&mut buffer),
            )
            .expect("second click should succeed");
        assert_eq!(program.model.single, 2);
        assert_eq!(program.model.double, 0);
    }
}
