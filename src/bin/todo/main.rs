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
    let header = text::<Msg>("TODOs (Esc/q quit from list, ↑/↓ move, space toggles, Enter adds)")
        .with_style(Style::bold());

    let items = model
        .items
        .iter()
        .enumerate()
        .map(|(index, item)| {
            let highlighted = model.focus == Focus::List && index == model.selected;
            render_item(item, highlighted)
        })
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
        text::<Msg>("New TODO (Enter adds):"),
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
            match model.focus {
                Focus::List => {
                    if model.selected > 0 {
                        model.selected -= 1;
                    } else {
                        model.focus = Focus::Input;
                    }
                }
                Focus::Input => {
                    if !model.items.is_empty() {
                        model.focus = Focus::List;
                        model.selected = model.items.len() - 1;
                    }
                }
            }
            Transition::Continue
        }
        KeyCode::Down => {
            match model.focus {
                Focus::List => {
                    if model.selected + 1 < model.items.len() {
                        model.selected += 1;
                    } else {
                        model.focus = Focus::Input;
                    }
                }
                Focus::Input => {
                    if !model.items.is_empty() {
                        model.focus = Focus::List;
                        if model.selected >= model.items.len() {
                            model.selected = model.items.len() - 1;
                        }
                    }
                }
            }
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

    match model.focus {
        Focus::List => {
            if matches!(ch, 'q' | 'Q') && !key.ctrl {
                Transition::Quit
            } else {
                Transition::Continue
            }
        }
        Focus::Input => {
            if !key.ctrl {
                model.input.push(ch);
            }
            Transition::Continue
        }
    }
}

fn render_item(item: &Item, highlighted: bool) -> Node<Msg> {
    let marker = if item.completed { "[x]" } else { "[ ]" };
    let text_node = text::<Msg>(format!("{} {}", marker, item.title));
    let styled_text = if highlighted {
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

fn init_tracing() -> color_eyre::Result<()> {
    use std::fs::File;
    use std::path::PathBuf;
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

    let log_path = PathBuf::from("todo.log");

    File::options().create(true).append(true).open(&log_path)?;

    let writer = tracing_subscriber::fmt::writer::BoxMakeWriter::new({
        let log_path = log_path.clone();
        move || {
            File::options()
                .create(true)
                .append(true)
                .open(&log_path)
                .expect("log file should remain writable")
        }
    });

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_ansi(false)
                .with_writer(writer),
        )
        .try_init()
        .map_err(|error| color_eyre::eyre::eyre!(error))?;

    Ok(())
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
    init_tracing()?;

    let program = Program::new(seed_model(), update, view).map_event(map_event);

    tracing::info!("Starting TODO program");

    if let Err(error) = program.run() {
        tracing::error!(?error, "Program exited with error");
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

    #[test]
    fn pressing_up_on_first_item_moves_focus_to_input() {
        let mut model = Model {
            items: vec![Item {
                title: "Example".into(),
                completed: false,
            }],
            selected: 0,
            input: String::new(),
            focus: Focus::List,
        };

        let transition = handle_key(&mut model, Key::new(KeyCode::Up));

        match transition {
            Transition::Continue => {}
            _ => panic!("expected transition to continue"),
        }

        assert_eq!(model.focus, Focus::Input);
        assert_eq!(model.selected, 0);
    }

    #[test]
    fn pressing_down_from_input_moves_focus_to_list() {
        let mut model = Model {
            items: vec![Item {
                title: "Example".into(),
                completed: false,
            }],
            selected: 0,
            input: String::new(),
            focus: Focus::Input,
        };

        let transition = handle_key(&mut model, Key::new(KeyCode::Down));

        match transition {
            Transition::Continue => {}
            _ => panic!("expected transition to continue"),
        }

        assert_eq!(model.focus, Focus::List);
        assert_eq!(model.selected, 0);
    }

    #[test]
    fn pressing_down_on_last_item_moves_focus_to_input() {
        let mut model = Model {
            items: vec![
                Item {
                    title: "One".into(),
                    completed: false,
                },
                Item {
                    title: "Two".into(),
                    completed: false,
                },
            ],
            selected: 1,
            input: String::new(),
            focus: Focus::List,
        };

        let transition = handle_key(&mut model, Key::new(KeyCode::Down));

        match transition {
            Transition::Continue => {}
            _ => panic!("expected transition to continue"),
        }

        assert_eq!(model.focus, Focus::Input);
        assert_eq!(model.selected, 1);
    }

    #[test]
    fn pressing_up_from_input_wraps_to_last_item() {
        let mut model = Model {
            items: vec![
                Item {
                    title: "One".into(),
                    completed: false,
                },
                Item {
                    title: "Two".into(),
                    completed: false,
                },
            ],
            selected: 0,
            input: String::new(),
            focus: Focus::Input,
        };

        let transition = handle_key(&mut model, Key::new(KeyCode::Up));

        match transition {
            Transition::Continue => {}
            _ => panic!("expected transition to continue"),
        }

        assert_eq!(model.focus, Focus::List);
        assert_eq!(model.selected, 1);
    }

    #[test]
    fn typing_while_list_focused_does_not_modify_input() {
        let mut model = Model {
            items: vec![Item {
                title: "Example".into(),
                completed: false,
            }],
            selected: 0,
            input: String::new(),
            focus: Focus::List,
        };

        let key = Key::new(KeyCode::Char('a'));
        let transition = handle_char(&mut model, key, 'a');

        match transition {
            Transition::Continue => {}
            _ => panic!("expected transition to continue"),
        }

        assert!(model.input.is_empty());
        assert_eq!(model.focus, Focus::List);
    }

    #[test]
    fn list_focus_highlights_selected_item() {
        let model = Model {
            items: vec![Item {
                title: "Example".into(),
                completed: false,
            }],
            selected: 0,
            input: String::new(),
            focus: Focus::List,
        };

        let root = view(&model);
        let block = root.into_element().expect("expected block element");
        let content_column = block
            .children
            .into_iter()
            .next()
            .expect("expected outer column")
            .into_element()
            .expect("expected column element");
        let mut content_children = content_column.children.into_iter();
        let _ = content_children.next();
        let _ = content_children.next();
        let items_column = content_children
            .next()
            .expect("expected items column")
            .into_element()
            .expect("expected column element");
        let mut item_rows = items_column.children.into_iter();
        let row = item_rows
            .next()
            .expect("expected row")
            .into_element()
            .expect("expected row element");
        let mut row_children = row.children.into_iter();
        let text = row_children
            .next()
            .expect("expected text child")
            .into_text()
            .expect("expected text node");

        assert_eq!(text.style.fg, Some(Color::Cyan));
    }

    #[test]
    fn input_focus_does_not_highlight_selected_item() {
        let model = Model {
            items: vec![Item {
                title: "Example".into(),
                completed: false,
            }],
            selected: 0,
            input: String::new(),
            focus: Focus::Input,
        };

        let root = view(&model);
        let block = root.into_element().expect("expected block element");
        let content_column = block
            .children
            .into_iter()
            .next()
            .expect("expected outer column")
            .into_element()
            .expect("expected column element");
        let mut content_children = content_column.children.into_iter();
        let _ = content_children.next();
        let _ = content_children.next();
        let items_column = content_children
            .next()
            .expect("expected items column")
            .into_element()
            .expect("expected column element");
        let mut item_rows = items_column.children.into_iter();
        let row = item_rows
            .next()
            .expect("expected row")
            .into_element()
            .expect("expected row element");
        let mut row_children = row.children.into_iter();
        let text = row_children
            .next()
            .expect("expected text child")
            .into_text()
            .expect("expected text node");

        assert_eq!(text.style.fg, None);
    }
}
