use std::panic;

use chatui::dom::{Color, Node};
use chatui::event::{Event, Key, KeyCode};
use chatui::{Program, Style, Transition, block, column, row, text};
use termwiz::escape::CSI;
use termwiz::escape::csi::{DecPrivateMode, DecPrivateModeCode, Mode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
enum Focus {
    #[default]
    List,
    Input,
}

impl Focus {
    fn toggle(self) -> Self {
        match self {
            Focus::List => Focus::Input,
            Focus::Input => Focus::List,
        }
    }
}

#[derive(Default)]
struct Model {
    items: Vec<Item>,
    selected: usize,
    input: String,
    focus: Focus,
}

#[derive(Clone)]
struct Item {
    title: String,
    completed: bool,
}

enum Msg {
    KeyPressed(Key),
}

fn update(model: &mut Model, msg: Msg) -> Transition {
    match msg {
        Msg::KeyPressed(key) => handle_key(model, key),
    }
}

fn view(model: &Model) -> Node<Msg> {
    let header = text::<Msg>(
        "TODOs (Esc/q quit from list, ↑/↓ move, space toggles, Tab focuses input, Enter adds)",
    )
    .with_style(Style::bold());

    let items = model
        .items
        .iter()
        .enumerate()
        .map(|(index, item)| render_item(item, index == model.selected))
        .collect();

    block(vec![column(vec![
        header,
        render_input(model),
        column(items),
    ])])
}

fn render_input(model: &Model) -> Node<Msg> {
    let cursor = if model.focus == Focus::Input {
        "_"
    } else {
        " "
    };
    let prompt = if model.input.is_empty() {
        format!("> {}", cursor)
    } else {
        format!("> {}{}", model.input, cursor)
    };

    let value_style = match model.focus {
        Focus::Input => Style::fg(Color::Green),
        Focus::List => Style::default(),
    };

    block(vec![column(vec![
        text::<Msg>("New TODO (Tab to focus, Enter adds):"),
        text::<Msg>(prompt).with_style(value_style),
    ])])
}

fn handle_key(model: &mut Model, key: Key) -> Transition {
    if key.ctrl
        && matches!(
            key.code,
            KeyCode::Char('c') | KeyCode::Char('C') | KeyCode::Char('q') | KeyCode::Char('Q')
        )
    {
        return Transition::Quit;
    }

    match key.code {
        KeyCode::Esc => Transition::Quit,
        KeyCode::Up => {
            if model.selected > 0 {
                model.selected -= 1;
            }
            model.focus = Focus::List;
            Transition::Continue
        }
        KeyCode::Down => {
            if model.selected + 1 < model.items.len() {
                model.selected += 1;
            }
            model.focus = Focus::List;
            Transition::Continue
        }
        KeyCode::Char('\t') => {
            model.focus = model.focus.toggle();
            Transition::Continue
        }
        KeyCode::Enter => {
            if model.focus == Focus::Input {
                if model.input.trim().is_empty() {
                    return Transition::Continue;
                }

                let title = model.input.clone();
                model.items.push(Item {
                    title,
                    completed: false,
                });
                if !model.items.is_empty() {
                    model.selected = model.items.len() - 1;
                }
                model.input.clear();
                model.focus = Focus::List;
            } else if let Some(item) = model.items.get_mut(model.selected) {
                item.completed = !item.completed;
            }

            Transition::Continue
        }
        KeyCode::Backspace => {
            if model.focus == Focus::Input {
                model.input.pop();
            }
            Transition::Continue
        }
        KeyCode::Char(' ') => {
            if model.focus == Focus::List {
                if let Some(item) = model.items.get_mut(model.selected) {
                    item.completed = !item.completed;
                }
                return Transition::Continue;
            }

            if !key.ctrl {
                model.input.push(' ');
            }
            Transition::Continue
        }
        KeyCode::Char(ch) => handle_char(model, key, ch),
        _ => Transition::Continue,
    }
}

fn handle_char(model: &mut Model, key: Key, ch: char) -> Transition {
    if key.alt {
        return Transition::Continue;
    }

    if model.focus == Focus::List {
        if matches!(ch, 'q' | 'Q') && !key.ctrl {
            return Transition::Quit;
        }

        model.focus = Focus::Input;
    }

    if !key.ctrl {
        model.input.push(ch);
    }

    Transition::Continue
}

fn render_item(item: &Item, selected: bool) -> Node<Msg> {
    let marker = if item.completed { "[x]" } else { "[ ]" };
    let text_node = text::<Msg>(format!("{} {}", marker, item.title));
    let styled_text = if selected {
        text_node.with_style(Style::fg(Color::Cyan))
    } else {
        text_node
    };

    row(vec![styled_text])
}

fn map_event(event: Event) -> Option<Msg> {
    match event {
        Event::Key(key) => Some(Msg::KeyPressed(key)),
        _ => None,
    }
}

fn seed_model() -> Model {
    Model {
        items: vec![
            Item {
                title: "Ship Elm TUI scaffold".into(),
                completed: true,
            },
            Item {
                title: "Wire rendering into runtime".into(),
                completed: true,
            },
            Item {
                title: "Polish TODO example".into(),
                completed: false,
            },
            Item {
                title: "Add focus styles".into(),
                completed: false,
            },
        ],
        selected: 0,
        input: String::new(),
        focus: Focus::List,
    }
}

#[cfg(unix)]
fn install_panic_hook() -> color_eyre::Result<()> {
    use std::io::{self, Write};
    use std::os::fd::AsRawFd;
    use termios::{TCSANOW, Termios, tcsetattr};

    let stdout = std::io::stdout();
    let fd = stdout.as_raw_fd();
    let base_termios = Termios::from_fd(fd)?;

    panic::set_hook(Box::new(move |panic| {
        let _ = tcsetattr(fd, TCSANOW, &base_termios);
        let mut stderr = io::stderr();

        let restore_sequences = [
            CSI::Mode(Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::ClearAndEnableAlternateScreen,
            ))),
            CSI::Mode(Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::BracketedPaste,
            ))),
            CSI::Mode(Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::AnyEventMouse,
            ))),
            CSI::Mode(Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::SGRMouse,
            ))),
            CSI::Mode(Mode::SetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::ShowCursor,
            ))),
        ];

        for sequence in restore_sequences {
            let _ = write!(stderr, "{}", sequence);
        }
        let _ = stderr.flush();

        let (hook, _) = color_eyre::config::HookBuilder::default()
            .try_into_hooks()
            .unwrap();
        eprintln!("{}", hook.panic_report(panic));
    }));

    Ok(())
}

#[cfg(not(unix))]
fn install_panic_hook() -> color_eyre::Result<()> {
    panic::set_hook(Box::new(|panic| {
        let (hook, _) = color_eyre::config::HookBuilder::default()
            .try_into_hooks()
            .unwrap();
        eprintln!("{}", hook.panic_report(panic));
    }));

    Ok(())
}

fn main() -> color_eyre::Result<()> {
    install_panic_hook()?;

    let program = Program::new(seed_model(), update, view).map_event(map_event);

    if let Err(error) = program.run() {
        eprintln!("Program exited with error: {:?}", error);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::dom::Color;

    #[test]
    fn selected_item_sets_text_color() {
        let item = Item {
            title: "Example".into(),
            completed: false,
        };

        let node = render_item(&item, true);

        let row = node.into_element().expect("expected row element");
        match row.children.first() {
            Some(child) => {
                let text = child.as_text().expect("expected text child");
                assert_eq!(text.style.fg, Some(Color::Cyan));
            }
            None => panic!("expected text child"),
        }
    }

    #[test]
    fn unselected_item_leaves_text_color_default() {
        let item = Item {
            title: "Example".into(),
            completed: false,
        };

        let node = render_item(&item, false);

        let row = node.into_element().expect("expected row element");
        match row.children.first() {
            Some(child) => {
                let text = child.as_text().expect("expected text child");
                assert_eq!(text.style.fg, None);
            }
            None => panic!("expected text child"),
        }
    }

    #[test]
    fn submitting_input_creates_new_item() {
        let mut model = Model {
            items: Vec::new(),
            selected: 0,
            input: "Buy milk".into(),
            focus: Focus::Input,
        };

        let transition = handle_key(&mut model, Key::new(KeyCode::Enter));

        match transition {
            Transition::Continue => {}
            _ => panic!("expected transition to continue"),
        }

        assert_eq!(model.items.len(), 1);
        assert_eq!(model.items[0].title, "Buy milk");
        assert!(model.input.is_empty());
        assert_eq!(model.focus, Focus::List);
        assert_eq!(model.selected, 0);
    }
}
