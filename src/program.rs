use std::boxed::Box;

use crate::dom::Node;
use crate::error::ProgramError;
use crate::event::{Event, Key, KeyCode, MouseButtons, MouseEvent, Size};
use crate::render::Renderer;

use taffy::compute_root_layout;
use termwiz::caps::Capabilities;
use termwiz::input::{
    InputEvent, KeyEvent, Modifiers as TwModifiers, MouseButtons as TwMouseButtons,
    MouseEvent as TwMouseEvent,
};
use termwiz::surface::{Change, Surface};
use termwiz::terminal::buffered::BufferedTerminal;
use termwiz::terminal::{Terminal, new_terminal};
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
        }
    }

    pub fn map_event(mut self, event_mapper: impl Fn(Event) -> Option<Msg> + 'static) -> Self {
        self.event_mapper = Box::new(event_mapper);
        self
    }

    pub fn run(mut self) -> Result<(), ProgramError> {
        let caps = Capabilities::new_from_env().map_err(ProgramError::from)?;
        let terminal = new_terminal(caps).map_err(ProgramError::from)?;
        let mut terminal = BufferedTerminal::new(terminal).map_err(ProgramError::from)?;

        // Enter alternate screen and raw mode for the duration of the loop.
        {
            let term = terminal.terminal();
            term.enter_alternate_screen().map_err(ProgramError::from)?;
            term.set_raw_mode().map_err(ProgramError::from)?;
        }

        let result = self.event_loop(&mut terminal);

        // Always attempt to restore terminal state.
        {
            let term = terminal.terminal();
            let _ = term.set_cooked_mode();
            let _ = term.exit_alternate_screen();
        }

        result
    }

    pub fn send(&mut self, event: Event) -> Result<Transition, ProgramError> {
        self.process_event(event, None)
    }

    fn event_loop<T: Terminal>(
        &mut self,
        terminal: &mut BufferedTerminal<T>,
    ) -> Result<(), ProgramError> {
        // self.sync_size_from_terminal(terminal)?;
        self.render_view(terminal)?;
        terminal.flush()?;

        // WORKAROUND: somehow the cursor visablity is not synced by the normal render correctly on first render.
        terminal
            .terminal()
            .render(&[Change::CursorVisibility(
                termwiz::surface::CursorVisibility::Hidden,
            )])
            .unwrap();

        loop {
            if terminal.check_for_resize()? {
                let size = terminal.dimensions();
                let transition = self.process_event(
                    Event::Resize(Size {
                        width: size.0 as u16,
                        height: size.1 as u16,
                    }),
                    Some(terminal),
                )?;
                if matches!(transition, Transition::Quit) {
                    break;
                }
                terminal.flush()?;
            }
            match terminal
                .terminal()
                .poll_input(None)
                .map_err(ProgramError::from)?
            {
                Some(input) => {
                    if let Some(event) = convert_input_event(input) {
                        let transition = self.process_event(event, Some(terminal))?;
                        if matches!(transition, Transition::Quit) {
                            break;
                        }
                        terminal.flush()?;
                    }
                }
                None => continue,
            }
        }

        Ok(())
    }

    fn process_event(
        &mut self,
        event: Event,
        terminal: Option<&mut Surface>,
    ) -> Result<Transition, ProgramError> {
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

        if needs_render {
            if let Some(term) = terminal {
                self.render_view(term)?;
            } else {
                self.rebuild_view();
            }
        }

        Ok(transition)
    }

    fn render_view(&mut self, s: &mut Surface) -> Result<(), ProgramError> {
        self.rebuild_view();
        if let Some(current_view) = self.current_view.as_ref() {
            Renderer::new(s).render(current_view, self.current_size)
        } else {
            Ok(())
        }
    }

    fn rebuild_view(&mut self) {
        self.current_view = Some((self.view)(&self.model));
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
        }
    }

    fn handle_mouse_event(&mut self, event: &MouseEvent) -> Option<Msg> {
        let previous = self.last_mouse_buttons;
        self.last_mouse_buttons = event.buttons;

        if let Some(view) = self.current_view.as_ref()
            && let Some(target) = view.hit_test(event.x, event.y)
            && let Some(msg) = target.mouse_message(event)
        // && event.buttons.left
        // && !previous.left
        {
            return Some(msg);
        }

        None
    }
}

const DEFAULT_WIDTH: u16 = 80;
const DEFAULT_HEIGHT: u16 = 24;

fn convert_input_event(input: InputEvent) -> Option<Event> {
    tracing::debug!("termwiz input event: {:?}", input);
    match input {
        InputEvent::Key(key) => map_key_event(key),
        InputEvent::Mouse(mouse) => map_mouse_event(mouse),
        InputEvent::Resized { cols, rows } => Some(Event::Resize(Size::new(
            clamp_to_u16(cols),
            clamp_to_u16(rows),
        ))),
        _ => None,
    }
}

fn map_mouse_event(mouse: TwMouseEvent) -> Option<Event> {
    let buttons = MouseButtons {
        left: mouse.mouse_buttons.contains(TwMouseButtons::LEFT),
        right: mouse.mouse_buttons.contains(TwMouseButtons::RIGHT),
        middle: mouse.mouse_buttons.contains(TwMouseButtons::MIDDLE),
        horz_wheel: mouse.mouse_buttons.contains(TwMouseButtons::HORZ_WHEEL),
        vert_wheel: mouse.mouse_buttons.contains(TwMouseButtons::VERT_WHEEL),
        wheel_positive: mouse.mouse_buttons.contains(TwMouseButtons::WHEEL_POSITIVE),
    };

    Some(Event::Mouse(MouseEvent::with_modifiers(
        // termwiz mouse x/y are 1 indexed
        mouse.x - 1,
        mouse.y - 1,
        buttons,
        mouse.modifiers.contains(TwModifiers::CTRL),
        mouse.modifiers.contains(TwModifiers::ALT),
        mouse.modifiers.contains(TwModifiers::SHIFT),
    )))
}

fn map_key_event(key: KeyEvent) -> Option<Event> {
    let code = map_key_code(key.key)?;
    let modifiers = key.modifiers;
    let event_key = Key {
        code,
        ctrl: modifiers.contains(TwModifiers::CTRL),
        alt: modifiers.contains(TwModifiers::ALT),
        shift: modifiers.contains(TwModifiers::SHIFT),
    };
    Some(Event::Key(event_key))
}

fn map_key_code(code: termwiz::input::KeyCode) -> Option<KeyCode> {
    use termwiz::input::KeyCode as TwKeyCode;

    match code {
        TwKeyCode::Char(c) => Some(KeyCode::Char(c)),
        TwKeyCode::Enter => Some(KeyCode::Enter),
        TwKeyCode::Escape => Some(KeyCode::Esc),
        TwKeyCode::Backspace => Some(KeyCode::Backspace),
        TwKeyCode::LeftArrow => Some(KeyCode::Left),
        TwKeyCode::RightArrow => Some(KeyCode::Right),
        TwKeyCode::UpArrow => Some(KeyCode::Up),
        TwKeyCode::DownArrow => Some(KeyCode::Down),
        _ => None,
    }
}

fn clamp_to_u16(value: usize) -> u16 {
    u16::try_from(value).unwrap_or(u16::MAX)
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
        let mut surface = Surface::new(10, 2);

        let transition = program
            .process_event(Event::key(KeyCode::Char('+')), Some(&mut surface))
            .expect("send should succeed");
        assert_eq!(transition, Transition::Continue);

        let screen = surface.screen_chars_to_string();
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
        Mouse(u16, u16),
    }

    fn click_update(model: &mut ClickModel, msg: ClickMsg) -> Transition {
        match msg {
            ClickMsg::Click => {
                model.clicks += 1;
                Transition::Continue
            }
            ClickMsg::Mouse(_, _) => Transition::Continue,
        }
    }

    fn click_view(_model: &ClickModel) -> Node<ClickMsg> {
        text("button").on_click(|| ClickMsg::Click)
    }

    #[test]
    fn mouse_click_triggers_on_click_handler() {
        let mut program = Program::new(ClickModel::default(), click_update, click_view);
        let mut surface = Surface::new(4, 2);

        program
            .process_event(Event::resize(4, 2), Some(&mut surface))
            .expect("resize should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::new(true, false, false)),
                Some(&mut surface),
            )
            .expect("mouse down should succeed");
        assert_eq!(program.model.clicks, 1);

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::default()),
                Some(&mut surface),
            )
            .expect("mouse move up should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseButtons::new(true, false, false)),
                Some(&mut surface),
            )
            .expect("second click should succeed");
        assert_eq!(program.model.clicks, 2);
    }
}
