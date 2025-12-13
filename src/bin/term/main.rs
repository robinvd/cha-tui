//! Terminal multiplexer demo with two side-by-side terminals.

use chatui::dom::{Color, Node};
use chatui::event::{Event, Key, KeyCode};
use chatui::{
    Program, Style, TerminalMsg, TerminalState, Transition, block_with_title, column,
    default_terminal_keybindings, row, terminal, text,
};
use smol::channel::Receiver;
use tracing::info;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
enum Focus {
    #[default]
    Left,
    Right,
}

impl Focus {
    fn toggle(self) -> Self {
        match self {
            Focus::Left => Focus::Right,
            Focus::Right => Focus::Left,
        }
    }

    fn label(self) -> &'static str {
        match self {
            Focus::Left => "Left",
            Focus::Right => "Right",
        }
    }
}

struct Model {
    initial_update: bool,
    left_term: TerminalState,
    right_term: TerminalState,
    left_wakeup: Receiver<()>,
    right_wakeup: Receiver<()>,
    focus: Focus,
}

impl Model {
    fn new() -> std::io::Result<Self> {
        let left_term = TerminalState::new()?;
        let right_term = TerminalState::new()?;
        let left_wakeup = left_term.wakeup_receiver();
        let right_wakeup = right_term.wakeup_receiver();
        Ok(Self {
            left_term,
            right_term,
            left_wakeup,
            right_wakeup,
            focus: Focus::Left,
            initial_update: true,
        })
    }
}

#[derive(Clone, Debug)]
enum Msg {
    LeftTerminal(TerminalMsg),
    RightTerminal(TerminalMsg),
    Key(Key),
    FocusLeft,
    FocusRight,
    Refresh,
}

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    if model.initial_update {
        model.initial_update = false;

        info!("init refresh cycle");
        return update(model, msg).combine(update(model, Msg::Refresh));
    }
    match msg {
        Msg::LeftTerminal(term_msg) => {
            info!("LeftTerminal message: {:?}", term_msg);
            model.left_term.update(term_msg);
        }
        Msg::RightTerminal(term_msg) => {
            info!("RightTerminal message: {:?}", term_msg);
            model.right_term.update(term_msg);
        }
        Msg::Key(key) => {
            info!("Key message: {:?}", key);
            if key.ctrl && key.code == KeyCode::Char('o') {
                model.focus = model.focus.toggle();
                return Transition::Continue;
            }

            if key.ctrl && key.code == KeyCode::Char('q') {
                return Transition::Quit;
            }

            // Send terminal input
            let wakeup = match model.focus {
                Focus::Left => {
                    if let Some(Msg::LeftTerminal(term_msg)) =
                        default_terminal_keybindings(key, Msg::LeftTerminal)
                    {
                        model.left_term.update(term_msg);
                    }
                    model.left_wakeup.clone()
                }
                Focus::Right => {
                    if let Some(Msg::RightTerminal(term_msg)) =
                        default_terminal_keybindings(key, Msg::RightTerminal)
                    {
                        model.right_term.update(term_msg);
                    }
                    model.right_wakeup.clone()
                }
            };

            // Drain stale wakeups then wait for fresh one from our input
            while wakeup.try_recv().is_ok() {}
            // let deadline = std::time::Instant::now() + std::time::Duration::from_millis(50);
            // while std::time::Instant::now() < deadline {
            //     if wakeup.try_recv().is_ok() {
            //         break;
            //     }
            //     std::thread::sleep(std::time::Duration::from_micros(500));
            // }
        }
        Msg::FocusLeft => {
            model.focus = Focus::Left;
        }
        Msg::FocusRight => {
            model.focus = Focus::Right;
        }
        Msg::Refresh => {
            info!("Refresh message received");
            // Drain any pending wakeups first - this ensures we don't miss updates
            // that occurred between the last render and now
            let mut had_pending = false;
            while model.left_wakeup.try_recv().is_ok() {
                had_pending = true;
            }
            while model.right_wakeup.try_recv().is_ok() {
                had_pending = true;
            }
            info!("Refresh: had_pending={}", had_pending);

            // Spawn a task to await the next wakeup from either terminal
            let left = model.left_wakeup.clone();
            let right = model.right_wakeup.clone();
            let task = Transition::Task(Box::pin(async move {
                tracing::info!("start listen for term update");
                smol::future::or(
                    async {
                        let _ = left.recv().await;
                    },
                    async {
                        let _ = right.recv().await;
                    },
                )
                .await;
                tracing::info!("found term update");
                Msg::Refresh
            }));

            // If we had pending wakeups, return Continue to trigger a render,
            // but we also need to set up the task. We'll return the task and
            // rely on Transition::Task triggering a render.
            return task;
        }
    }
    Transition::Continue
}

fn view(model: &Model) -> Node<Msg> {
    let left_style = if model.focus == Focus::Left {
        Style::fg(Color::Green)
    } else {
        Style::dim()
    };

    let right_style = if model.focus == Focus::Right {
        Style::fg(Color::Green)
    } else {
        Style::dim()
    };

    let left_pane = block_with_title::<Msg>(
        "Terminal 1",
        vec![terminal("left-term", &model.left_term, Msg::LeftTerminal).with_fill()],
    )
    .with_style(left_style)
    .with_fill()
    .on_click(|| Msg::FocusLeft);

    let right_pane = block_with_title::<Msg>(
        "Terminal 2",
        vec![terminal("right-term", &model.right_term, Msg::RightTerminal).with_fill()],
    )
    .with_style(right_style)
    .with_fill()
    .on_click(|| Msg::FocusRight);

    let terminals = row(vec![left_pane, right_pane])
        .with_fill()
        .with_flex_grow(1.0);

    let status_bar = row(vec![
        text::<Msg>(format!(
            " Focus: {} │ Ctrl-O: switch │ Ctrl-Q: quit ",
            model.focus.label()
        ))
        .with_style(Style::bg(Color::rgb(40, 40, 40)).merged(&Style::fg(Color::White))),
    ]);

    column(vec![terminals, status_bar]).with_fill()
}

fn main() {
    color_eyre::install().expect("failed to install color-eyre");

    // Set up tracing to file instead of stdout (which would mess up the TUI)
    use std::fs::File;
    use tracing_subscriber::fmt;
    use tracing_subscriber::prelude::*;
    let log_file = File::create("./term_debug.log").expect("failed to create log file");
    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(log_file).with_ansi(false))
        .init();

    let model = Model::new().expect("failed to create terminals");
    let left_wakeup = model.left_wakeup.clone();
    let right_wakeup = model.right_wakeup.clone();

    let program = Program::new(model, update, view).map_event(move |event| match event {
        Event::Key(key) => Some(Msg::Key(key)),
        Event::Resize(_) => {
            // On resize (including initial), start the refresh loop
            let left = left_wakeup.clone();
            let right = right_wakeup.clone();
            // Drain any pending wakeups before starting
            while left.try_recv().is_ok() {}
            while right.try_recv().is_ok() {}
            Some(Msg::Refresh)
        }
        _ => None,
    });

    if let Err(err) = program.run() {
        eprintln!("Program failed: {:?}", err);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn focus_toggles_between_terminals() {
        let mut model = Model::new().expect("failed to create model");
        assert_eq!(model.focus, Focus::Left);

        update(
            &mut model,
            Msg::Key(Key::with_modifiers(
                KeyCode::Char('o'),
                true,
                false,
                false,
                false,
            )),
        );
        assert_eq!(model.focus, Focus::Right);

        update(
            &mut model,
            Msg::Key(Key::with_modifiers(
                KeyCode::Char('o'),
                true,
                false,
                false,
                false,
            )),
        );
        assert_eq!(model.focus, Focus::Left);
    }

    #[test]
    fn click_focuses_terminal() {
        let mut model = Model::new().expect("failed to create model");
        assert_eq!(model.focus, Focus::Left);

        update(&mut model, Msg::FocusRight);
        assert_eq!(model.focus, Focus::Right);

        update(&mut model, Msg::FocusLeft);
        assert_eq!(model.focus, Focus::Left);
    }

    #[test]
    fn keystroke_waits_for_pty_wakeup() {
        use std::time::{Duration, Instant};

        let mut model = Model::new().expect("failed to create model");
        let wakeup_receiver = model.left_wakeup.clone();

        // Drain any initial wakeups from shell startup
        std::thread::sleep(Duration::from_millis(100));
        while wakeup_receiver.try_recv().is_ok() {}

        // Get initial version
        let initial_version = model.left_term.version();

        // Send a keystroke - this should wait for PTY to process
        let start = Instant::now();
        update(&mut model, Msg::Key(Key::new(KeyCode::Char('x'))));
        let elapsed = start.elapsed();

        // Should have waited some time for PTY (but not too long)
        // The wait can be very short if PTY responds quickly
        assert!(
            elapsed.as_millis() < 100,
            "keystroke took too long: {:?}",
            elapsed
        );

        // Version should have incremented after PTY processed input
        let new_version = model.left_term.version();
        assert!(
            new_version > initial_version,
            "version should increment after keystroke: {} vs {}",
            initial_version,
            new_version
        );
    }
}
