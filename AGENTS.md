# Agent Specification

## Spec
- Provide a minimal Elm-style terminal UI framework backed by termwiz without exposing termwiz types to applications.
- Core runtime lives in `src/program.rs`: `Program::new(model, update, view)` wires the loop, `Program::map_event` installs a translator from `event::Event` to application messages, `Program::run` drives a buffered termwiz terminal (alternate screen + raw mode) using the renderer, and `Program::send` replays events for tests.
- State transitions use `Transition::{Continue, Quit}`; updates mutate the model directly. Rendering is triggered for resize events or `Transition::Continue` results.
- DOM primitives reside in `src/dom/mod.rs`: `Node` carries shared state and wraps `NodeContent::{Element, Text}`, alongside `ElementKind::{Column, Row, Block}`, `Style`, and `Attributes`. Helpers include `text`, `column`, `row`, `block`, with builder-like APIs (`Node::with_style`, `Attributes::with_style`, etc.). Layout is minimal—columns stack children, rows share width evenly, blocks add a border and render inner content.
- Events in `src/event.rs`: `Event::{Key, Resize}`, `Key` (code + ctrl/alt/shift), `KeyCode` (basic navigation + char), `Size`. Convenience constructors ensure default modifiers.
- Renderer in `src/render.rs`: owns an in-memory termwiz `Surface`, clears/resizes each frame, walks the DOM to paint text, columns, rows, and bordered blocks, and exposes accessors for copying into a buffered terminal. Styling maps to termwiz `CellAttributes` with simple ANSI colors and bold intensity support. Unit tests validate layout, borders, and color application via the surface buffer.
- Program errors (`src/error.rs`) wrap termwiz failures and string-based render/event issues via `ProgramError::{Terminal, Render, Event}` with helper constructors and `From<termwiz::Error>`.
- Example application in `src/main.rs`: simple TODO list demonstrating update/view/event wiring, hotkeys for navigation/toggling, and block-based layout.
- Always run `cargo test` and `cargo clippy` after modifications.
