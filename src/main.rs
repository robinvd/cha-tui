use chatui::dom::Node;
use chatui::event::{Event, KeyCode};
use chatui::{Program, Style, Transition, block, column, row, text};

#[derive(Default)]
struct Model {
    items: Vec<Item>,
    selected: usize,
}

#[derive(Clone)]
struct Item {
    title: String,
    completed: bool,
}

enum Msg {
    MoveUp,
    MoveDown,
    Toggle,
    Quit,
}

fn update(model: &mut Model, msg: Msg) -> Transition {
    match msg {
        Msg::MoveUp => {
            if model.selected > 0 {
                model.selected -= 1;
            }
            Transition::Continue
        }
        Msg::MoveDown => {
            if model.selected + 1 < model.items.len() {
                model.selected += 1;
            }
            Transition::Continue
        }
        Msg::Toggle => {
            if let Some(item) = model.items.get_mut(model.selected) {
                item.completed = !item.completed;
            }
            Transition::Continue
        }
        Msg::Quit => Transition::Quit,
    }
}

fn view(model: &Model) -> Node<Msg> {
    let header =
        text::<Msg>("TODOs (q to quit, ↑/↓ move, space toggles)").with_style(Style::bold());

    let items = model
        .items
        .iter()
        .enumerate()
        .map(|(index, item)| render_item(item, index == model.selected))
        .collect();

    block(vec![column(vec![header, column(items)])])
}

fn render_item(item: &Item, selected: bool) -> Node<Msg> {
    let marker = if item.completed { "[x]" } else { "[ ]" };
    let text_node = text::<Msg>(format!("{} {}", marker, item.title));
    let styled_text = if selected {
        text_node.with_style(Style::fg(chatui::dom::Color::Cyan))
    } else {
        text_node
    };

    row(vec![styled_text])
}

fn map_event(event: Event) -> Option<Msg> {
    match event {
        Event::Key(key) => match key.code {
            KeyCode::Char('q') => Some(Msg::Quit),
            KeyCode::Up => Some(Msg::MoveUp),
            KeyCode::Down => Some(Msg::MoveDown),
            KeyCode::Char(' ') => Some(Msg::Toggle),
            _ => None,
        },
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
    }
}

fn main() {
    let program = Program::new(seed_model(), update, view).map_event(map_event);

    if let Err(error) = program.run() {
        eprintln!("Program exited with error: {:?}", error);
    }
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

        match node {
            Node::Element(row) => match row.children.first() {
                Some(Node::Text(text)) => assert_eq!(text.style.fg, Some(Color::Cyan)),
                _ => panic!("expected text child"),
            },
            _ => panic!("expected row element"),
        }
    }

    #[test]
    fn unselected_item_leaves_text_color_default() {
        let item = Item {
            title: "Example".into(),
            completed: false,
        };

        let node = render_item(&item, false);

        match node {
            Node::Element(row) => match row.children.first() {
                Some(Node::Text(text)) => assert_eq!(text.style.fg, None),
                _ => panic!("expected text child"),
            },
            _ => panic!("expected row element"),
        }
    }
}
