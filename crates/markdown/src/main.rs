use anyhow::Result;
use chatui::{
    components::scroll::ScrollMsg,
    event::{Event, KeyCode},
    Program, Transition,
};
use chatui_markdown::{markdown_view, MarkdownDocument, MarkdownMsg, MarkdownState};
use clap::Parser;
use std::fs;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: String,
}

struct Model {
    doc: MarkdownDocument,
    state: MarkdownState,
}

#[derive(Clone, Debug)]
enum Msg {
    Markdown(MarkdownMsg),
    Quit,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let content = fs::read_to_string(&args.file)?;
    let doc = MarkdownDocument::parse(&content);
    let mut state = MarkdownState::new();
    state.sync_with(&doc);

    let mut model = Model { doc, state };

    let mut program = Program::new(&mut model, update, view).map_event(map_event);
    smol::block_on(program.run_async()).map_err(|e| anyhow::anyhow!("{:?}", e))?;
    Ok(())
}

fn map_event(event: Event) -> Option<Msg> {
    match event {
        Event::Key(key) if key.ctrl && key.code == KeyCode::Char('c') => Some(Msg::Quit),
        Event::Key(key) => match key.code {
            KeyCode::Char('q') | KeyCode::Esc => Some(Msg::Quit),
            KeyCode::Down | KeyCode::Char('j') => Some(Msg::Markdown(MarkdownMsg::DocScroll(
                ScrollMsg::AxisDelta {
                    axis: chatui::components::scroll::ScrollAxis::Vertical,
                    amount: 1,
                },
            ))),
            KeyCode::Up | KeyCode::Char('k') => Some(Msg::Markdown(MarkdownMsg::DocScroll(
                ScrollMsg::AxisDelta {
                    axis: chatui::components::scroll::ScrollAxis::Vertical,
                    amount: -1,
                },
            ))),
            KeyCode::PageDown => Some(Msg::Markdown(MarkdownMsg::DocScroll(
                ScrollMsg::AxisDeltaPercent {
                    axis: chatui::components::scroll::ScrollAxis::Vertical,
                    ratio: 1.0,
                },
            ))),
            KeyCode::PageUp => Some(Msg::Markdown(MarkdownMsg::DocScroll(
                ScrollMsg::AxisDeltaPercent {
                    axis: chatui::components::scroll::ScrollAxis::Vertical,
                    ratio: -1.0,
                },
            ))),
            _ => None,
        },
        _ => None,
    }
}

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    match msg {
        Msg::Quit => Transition::Quit,
        Msg::Markdown(m) => {
            model.state.update(m);
            Transition::Continue
        }
    }
}

fn view(model: &Model) -> chatui::dom::Node<'_, Msg> {
    markdown_view("md-view", &model.doc, &model.state, Msg::Markdown).with_fill()
}
