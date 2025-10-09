use std::boxed::Box;
use std::io::Write;
use std::rc::Rc;
use std::time::{Duration, Instant};

use crate::buffer::DoubleBuffer;
use crate::dom::Node;
use crate::error::ProgramError;
use crate::event::{Event, Key, KeyCode, MouseButtons, MouseEvent, Size};
use crate::palette::Palette;
use crate::render::Renderer;
use crate::scroll::ScrollAlignment;

use smol::stream::StreamExt;
use taffy::compute_root_layout;
use termina::EventStream;
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

struct ScrollEffect<Msg> {
    callback: Rc<dyn Fn(f32) -> Msg>,
    offset: f32,
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
    queued_quit: bool,
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
            queued_quit: false,
        }
    }

    pub fn map_event(mut self, event_mapper: impl Fn(Event) -> Option<Msg> + 'static) -> Self {
        self.event_mapper = Box::new(event_mapper);
        self
    }

    pub fn run(mut self) -> Result<(), ProgramError> {
        // Keep a sync entrypoint for consumers; run the async loop on the smol executor.
        smol::block_on(self.run_async())
    }

    /// Async runtime: drives terminal I/O and event handling using termina's EventStream.
    pub async fn run_async(&mut self) -> Result<(), ProgramError> {
        let mut terminal = PlatformTerminal::new()
            .map_err(|e| ProgramError::terminal(format!("Failed to create terminal: {}", e)))?;

        // Enter raw mode for the duration of the loop.
        terminal
            .enter_raw_mode()
            .map_err(|e| ProgramError::terminal(format!("Failed to enter raw mode: {}", e)))?;

        // Enter alternate screen and enable mouse tracking
        write!(
            terminal,
            "\x1b[?1049h\x1b[?1000h\x1b[?1002h\x1b[?1003h\x1b[?1004h\x1b[?1006h", // Enable SGR mouse mode and focus tracking
        )
        .map_err(|e| ProgramError::terminal(format!("Failed to setup terminal: {}", e)))?;
        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush terminal: {}", e)))?;

        let result = self.event_loop_async(&mut terminal).await;

        // Always attempt to restore terminal state.
        // Show cursor again and disable mouse tracking + exit alternate screen.
        let _ = write!(
            terminal,
            "\x1b[?25h\x1b[?1006l\x1b[?1004l\x1b[?1003l\x1b[?1002l\x1b[?1000l\x1b[?1049l",
        );
        let _ = terminal.flush();
        let _ = terminal.enter_cooked_mode();

        result
    }

    pub fn send(&mut self, event: Event) -> Result<Transition, ProgramError> {
        self.process_event(event, None)
    }

    async fn event_loop_async(
        &mut self,
        terminal: &mut PlatformTerminal,
    ) -> Result<(), ProgramError> {
        let dimensions = terminal
            .get_dimensions()
            .map_err(|e| ProgramError::terminal(format!("Failed to get dimensions: {}", e)))?;
        let mut buffer = DoubleBuffer::new(dimensions.cols as usize, dimensions.rows as usize);

        // Ensure initial size is propagated to the model/view before first render
        let _ = self.handle_event(Event::Resize(Size::new(dimensions.cols, dimensions.rows)));
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

        // Consume events from termina asynchronously using its reader.
        let mut events = EventStream::new(terminal.event_reader(), |_| true);

        while let Some(event_res) = events.next().await {
            let mut needs_render = false;
            let mut should_quit = false;

            let event = event_res
                .map_err(|e| ProgramError::event(format!("Failed to read event: {}", e)))?;

            if let Some(converted_event) = convert_input_event(event) {
                let (transition, render_flag) = self.handle_event(converted_event);
                needs_render |= render_flag;
                if matches!(transition, Transition::Quit) {
                    should_quit = true;
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

        if matches!(transition, Transition::Quit) {
            return (Transition::Quit, needs_render);
        }

        if self.queued_quit {
            self.queued_quit = false;
            return (Transition::Quit, needs_render);
        }

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
            if quit {
                self.queued_quit = true;
            }
        }

        let mut scroll_effects: Vec<ScrollEffect<Msg>> = Vec::new();
        if let Some(view) = self.current_view.as_mut() {
            Self::collect_pending_scrolls(view, &mut scroll_effects);
        }

        for effect in scroll_effects {
            let message = (effect.callback)(effect.offset);
            if matches!((self.update)(&mut self.model, message), Transition::Quit) {
                self.queued_quit = true;
            }
        }
    }

    fn collect_pending_scrolls(node: &mut Node<Msg>, effects: &mut Vec<ScrollEffect<Msg>>) {
        if let Some(pending) = node.take_pending_scroll()
            && let Some((target_top, target_height)) =
                Self::target_geometry(node, pending.target_hash)
        {
            let layout = node.layout_state.layout;
            let viewport_height = layout.size.height.max(0.0);
            let content_height = layout.content_size.height.max(viewport_height);
            let new_scroll = Self::resolve_scroll_offset(
                node.scroll_y,
                viewport_height,
                content_height,
                target_top,
                target_height,
                pending.alignment,
            );

            if (new_scroll - node.scroll_y).abs() > f32::EPSILON {
                node.scroll_y = new_scroll;
                effects.push(ScrollEffect {
                    callback: pending.callback,
                    offset: new_scroll,
                });
            }
        }

        if let crate::dom::NodeContent::Element(element) = &mut node.content {
            for child in &mut element.children {
                Self::collect_pending_scrolls(child, effects);
            }
        }
    }

    fn target_geometry(node: &Node<Msg>, target_hash: u64) -> Option<(f32, f32)> {
        fn helper<Msg>(node: &Node<Msg>, target_hash: u64, offset: f32) -> Option<(f32, f32)> {
            if node.hashed_id() == target_hash {
                let layout = node.layout_state.layout;
                return Some((offset, layout.size.height.max(0.0)));
            }

            match &node.content {
                crate::dom::NodeContent::Element(element) => {
                    for child in &element.children {
                        let child_layout = child.layout_state.layout;
                        let child_offset = offset + child_layout.location.y;
                        if let Some(result) = helper(child, target_hash, child_offset) {
                            return Some(result);
                        }
                    }
                    None
                }
                crate::dom::NodeContent::Text(_) => None,
                crate::dom::NodeContent::Leaf(_) => None,
            }
        }

        helper(node, target_hash, 0.0)
    }

    fn resolve_scroll_offset(
        current_scroll: f32,
        viewport_height: f32,
        content_height: f32,
        target_top: f32,
        target_height: f32,
        alignment: ScrollAlignment,
    ) -> f32 {
        if viewport_height <= f32::EPSILON {
            return 0.0;
        }

        let max_scroll = (content_height - viewport_height).max(0.0);
        let target_bottom = target_top + target_height;
        let viewport_bottom = current_scroll + viewport_height;

        let desired = match alignment {
            ScrollAlignment::Start => target_top,
            ScrollAlignment::End => target_bottom - viewport_height,
            ScrollAlignment::Nearest => {
                if target_top < current_scroll {
                    target_top
                } else if target_bottom > viewport_bottom {
                    target_bottom - viewport_height
                } else {
                    current_scroll
                }
            }
        };

        desired.clamp(0.0, max_scroll)
    }

    fn handle_mouse_event(&mut self, event: &MouseEvent) -> Option<Msg> {
        let previous = self.last_mouse_buttons;
        self.last_mouse_buttons = event.buttons;

        let mut enriched = *event;
        enriched.click_count = 0;

        if event.buttons.left && !previous.left {
            let timestamp = Instant::now();
            let click_count = if let Some(last) = &self.last_click
                && timestamp.duration_since(last.timestamp) <= DOUBLE_CLICK_INTERVAL
                && last.x == event.x
                && last.y == event.y
            {
                if last.count >= 3 { 1 } else { last.count + 1 }
            } else {
                1
            };

            enriched.click_count = click_count;
            self.last_click = Some(LastClick {
                timestamp,
                x: event.x,
                y: event.y,
                count: click_count,
            });
        }

        if let Some(view) = self.current_view.as_ref()
            && let Some(msg) = view.hit_test(event.x, event.y, &mut |target, origin_x, origin_y| {
                let mut local_event = enriched;
                let origin_x_rounded = origin_x.round() as i32;
                let origin_y_rounded = origin_y.round() as i32;

                let rel_x = i32::from(local_event.x).saturating_sub(origin_x_rounded);
                let rel_y = i32::from(local_event.y).saturating_sub(origin_y_rounded);
                local_event.local_x = rel_x.max(0) as u16;
                local_event.local_y = rel_y.max(0) as u16;

                target.mouse_message(local_event)
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
    count: u8,
}

fn convert_input_event(input: termina::Event) -> Option<Event> {
    tracing::debug!("termina input event: {:?}", input);
    match input {
        termina::Event::Key(key) => map_key_event(key),
        termina::Event::Mouse(mouse) => map_mouse_event(mouse),
        termina::Event::WindowResized(size) => Some(Event::Resize(Size::new(size.cols, size.rows))),
        termina::Event::FocusIn => Some(Event::FocusGained),
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
        TnKeyCode::Tab => Some(KeyCode::Tab),
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

    #[test]
    fn focus_in_event_is_exposed() {
        let event = super::convert_input_event(termina::Event::FocusIn);
        assert_eq!(event, Some(Event::FocusGained));
    }
}
