# Agent Specification

## Spec
- written in rust
- Provide a minimal Elm-style terminal UI framework without exposing terminal backend types to applications.
- Core runtime lives in `src/program.rs`: `Program::new(&mut model, update, view)` wires the loop, `Program::map_event` installs a translator from `event::Event` to application messages, `Program::run` drives a buffered terminal (alternate screen + raw mode) using the renderer, and `Program::send` replays events for tests.
- State transitions use `Transition::{Continue, Quit}`; updates mutate the model directly. Rendering is triggered for resize events or `Transition::Continue` results.
- DOM primitives reside in `src/dom/mod.rs`: `Node` carries shared state and wraps `NodeContent::{Element, Text}`, alongside `ElementKind::{Column, Row, Block}`, `Style`, and `Attributes`. Helpers include `text`, `column`, `row`, `block`, with builder-like APIs (`Node::with_style`, `Attributes::with_style`, etc.). Layout is minimal—columns stack children, rows share width evenly, blocks add a border and render inner content.
- Events in `src/event.rs`: `Event::{Key, Resize}`, `Key` (code + ctrl/alt/shift), `KeyCode` (basic navigation + char), `Size`. Convenience constructors ensure default modifiers.
- Color is RGB/RGBA ONLY, no palette colors are used. The palette is queried on startup and then those RGB values are used when required.
- Always run `cargo test` and `cargo clippy` after modifications.
- Geometry utils (`Rect`/`Point`) in `src/geometry`
- Only unix (macos/linux) has to be supported
- Unless required dont use Color::Rgb, use the palette colors


## Code style

try to keep files to max 1000 lines (without tests).
Prefer modules that own a full scope, in ELM this means putting state/update/view function in one module based on domain. not a seperate `state`/`update`/`view` modules. instead a `sidebar` module for example that fully owns the sidebar logic.

Dont eagerly refactor existing files to use less lines. only new functionality.

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

### Testing

Use `just test` for running the tests, this makes sure all envs etc are set correctly

### LOGS

Run the app with `RUST_LOG=debug cargo run ...` (again! using the `just session-*` or tmux) and it will put logs in todo.log or gs.log depending on the app.

### Final checks

when doing an editing operation: before presenting the results to the user, always run `just quality-check` and fix any errors. this run fmt/clippy etc.

## crates

Notes on a subselection of crates that need extra info:

### term
a terminal multiplexer, with a sidebar for different projects/sessions

by default logs tracing::info! etc to ./term_debug.log

Any IO should be done using the TermIO trait so that tests dont actually change anything.

### vte + alacritty_terminal

vendored version of the official crates with various additions required for this project.
As these are vendored there is no need to think about backwards compatibility as all users are in this cargo workspace.
