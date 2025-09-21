use std::boxed::Box;

use crate::dom::Node;
use crate::error::ProgramError;
use crate::event::{Event, Key, KeyCode, Size};
use crate::render::Renderer;

use termwiz::caps::Capabilities;
use termwiz::input::{InputEvent, KeyEvent, Modifiers as TwModifiers};
use termwiz::terminal::buffered::BufferedTerminal;
use termwiz::terminal::{Terminal, new_terminal};

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
    renderer: Renderer,
    current_size: Size,
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
            renderer: Renderer::new(),
            current_size: Size::new(DEFAULT_WIDTH, DEFAULT_HEIGHT),
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
        self.process_event(event, Option::<&mut BufferedTerminal<InnerTerminal>>::None)
    }

    fn event_loop<T: Terminal>(
        &mut self,
        terminal: &mut BufferedTerminal<T>,
    ) -> Result<(), ProgramError> {
        self.sync_size_from_terminal(terminal)?;
        self.render_to_terminal(terminal)?;

        loop {
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
                    }
                }
                None => continue,
            }
        }

        Ok(())
    }

    fn process_event<T: Terminal>(
        &mut self,
        event: Event,
        terminal: Option<&mut BufferedTerminal<T>>,
    ) -> Result<Transition, ProgramError> {
        let mut needs_render = matches!(event, Event::Resize(_));

        if let Event::Resize(size) = event {
            self.current_size = size;
        }

        let msg = (self.event_mapper)(event);
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
            self.render_view()?;
            if let Some(term) = terminal {
                self.copy_to_terminal(term)?;
            }
        }

        Ok(transition)
    }

    fn render_view(&mut self) -> Result<(), ProgramError> {
        let node = (self.view)(&self.model);
        self.renderer.render(&node, self.current_size)
    }

    fn copy_to_terminal<T: Terminal>(
        &mut self,
        terminal: &mut BufferedTerminal<T>,
    ) -> Result<(), ProgramError> {
        terminal.draw_from_screen(self.renderer.surface(), 0, 0);
        terminal.flush().map_err(ProgramError::from)
    }

    fn render_to_terminal<T: Terminal>(
        &mut self,
        terminal: &mut BufferedTerminal<T>,
    ) -> Result<(), ProgramError> {
        self.render_view()?;
        self.copy_to_terminal(terminal)
    }

    fn sync_size_from_terminal<T: Terminal>(
        &mut self,
        terminal: &mut BufferedTerminal<T>,
    ) -> Result<(), ProgramError> {
        let size = terminal
            .terminal()
            .get_screen_size()
            .map_err(ProgramError::from)?;
        self.current_size = Size::new(clamp_to_u16(size.cols), clamp_to_u16(size.rows));
        Ok(())
    }
}

const DEFAULT_WIDTH: u16 = 80;
const DEFAULT_HEIGHT: u16 = 24;

type InnerTerminal = termwiz::terminal::SystemTerminal;

fn convert_input_event(input: InputEvent) -> Option<Event> {
    match input {
        InputEvent::Key(key) => map_key_event(key),
        InputEvent::Resized { cols, rows } => Some(Event::Resize(Size::new(
            clamp_to_u16(cols),
            clamp_to_u16(rows),
        ))),
        _ => None,
    }
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

        let transition = program
            .send(Event::key(KeyCode::Char('+')))
            .expect("send should succeed");
        assert_eq!(transition, Transition::Continue);

        let screen = program.renderer.surface().screen_chars_to_string();
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
}
