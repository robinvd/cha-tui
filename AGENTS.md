# Agent Specification

## Spec
- Provide a minimal Elm-style terminal UI framework backed by termwiz without exposing termwiz types to applications.
- Core runtime lives in `src/program.rs`: `Program::new(model, update, view)` wires the loop, `Program::map_event` installs a translator from `event::Event` to application messages, `Program::run` drives a buffered termwiz terminal (alternate screen + raw mode) using the renderer, and `Program::send` replays events for tests.
- State transitions use `Transition::{Continue, Quit}`; updates mutate the model directly. Rendering is triggered for resize events or `Transition::Continue` results.
- DOM primitives reside in `src/dom/mod.rs`: `Node` carries shared state and wraps `NodeContent::{Element, Text}`, alongside `ElementKind::{Column, Row, Block}`, `Style`, and `Attributes`. Helpers include `text`, `column`, `row`, `block`, with builder-like APIs (`Node::with_style`, `Attributes::with_style`, etc.). Layout is minimal—columns stack children, rows share width evenly, blocks add a border and render inner content.
- Events in `src/event.rs`: `Event::{Key, Resize}`, `Key` (code + ctrl/alt/shift), `KeyCode` (basic navigation + char), `Size`. Convenience constructors ensure default modifiers.
- Renderer in `src/render.rs`: owns an in-memory termwiz `Surface`, clears/resizes each frame, walks the DOM to paint text, columns, rows, and bordered blocks, and exposes accessors for copying into a buffered terminal. Styling maps to termwiz `CellAttributes`, supporting named ANSI colors, palette indexes via `Color::indexed`, RGB values via `Color::rgb`, and bold intensity. Unit tests validate layout, borders, and color application via the surface buffer.
- Program errors (`src/error.rs`) wrap termwiz failures and string-based render/event issues via `ProgramError::{Terminal, Render, Event}` with helper constructors and `From<termwiz::Error>`.
- Example application in `src/main.rs`: simple TODO list demonstrating update/view/event wiring, hotkeys for navigation/toggling, and block-based layout.
- Always run `cargo test` and `cargo clippy` after modifications.


## Running

### TUI

Running any TUI app (`cargo run`) could start an interactive test app, and if so not suitable for AI development as the program will run forever.
In this case use unit tests as a means of verifying new functionality. Or use the commands in the justfile

To test the interactive app use a terminal session.
```bash
just session-restart # Restart a session or start a new one if none exists

# Send a string to the interactive session, Use ANSI escape sequences for complex text
# for example `session-send-key '\x11'` for Ctrl-q, '\e[B' for Down, '\ef' for Alt-f, '\r' for enter
#
# Will also print the screen after sending the key data
just session-send-key

just session-get-screen # print the current session screen to stdout
```

ALWAYS use the just commands, and dont use manual version of the commands for interactive terminal testing.

Before running a new test in the interactive terminal, always do a `session-restart` to made sure there is nothing running.

### LOGS

Run the app with `RUST_LOG=debug cargo run ...` and it will put logs in todo.log or gs.log depending on the app.
