//! Modal dialog components.

use chatui::dom::Color;
use chatui::event::{Key, KeyCode};
use chatui::{
    InputMsg, InputState, InputStyle, Node, Style, block_with_title, modal, row, text,
};

/// State for modal dialogs.
#[derive(Clone)]
pub enum ModalState {
    NewProject { input: InputState },
}

/// Messages for modal interactions.
#[derive(Clone, Debug)]
pub enum ModalMsg {
    Input(InputMsg),
    Submit,
    Cancel,
}

/// Result of modal update.
pub enum ModalResult {
    /// Modal is still open, continue.
    Continue,
    /// Modal was cancelled.
    Cancelled,
    /// New project path was submitted.
    ProjectSubmitted(String),
}

/// Handle a key event for the modal, returning a message if applicable.
pub fn modal_handle_key(key: Key) -> Option<ModalMsg> {
    match key.code {
        KeyCode::Esc => Some(ModalMsg::Cancel),
        KeyCode::Enter => Some(ModalMsg::Submit),
        _ => chatui::default_input_keybindings(
            // We need a dummy state for keybinding check - this is a bit awkward
            // but the keybindings don't actually use the state
            &InputState::new(),
            key,
            ModalMsg::Input,
        ),
    }
}

/// Update modal state with a message.
pub fn modal_update(state: &mut ModalState, msg: ModalMsg) -> ModalResult {
    match msg {
        ModalMsg::Cancel => ModalResult::Cancelled,
        ModalMsg::Submit => {
            let ModalState::NewProject { input } = state;
            let value = input.value();
            let trimmed = value.trim();
            if trimmed.is_empty() {
                ModalResult::Cancelled
            } else {
                ModalResult::ProjectSubmitted(trimmed.to_string())
            }
        }
        ModalMsg::Input(input_msg) => {
            let ModalState::NewProject { input } = state;
            input.update(input_msg);
            ModalResult::Continue
        }
    }
}

/// Render the modal dialog.
pub fn modal_view<Msg: Clone + 'static>(
    state: &ModalState,
    wrap_input: impl Fn(InputMsg) -> Msg + 'static,
) -> Node<Msg> {
    match state {
        ModalState::NewProject { input } => {
            let mut input_style = InputStyle::default();
            input_style.cursor.bg = Some(Color::rgb(100, 200, 255));
            let prompt = text::<Msg>("Directory: ").with_style(Style::bold());
            let field = chatui::input::<Msg>("project-input", input, &input_style, wrap_input)
                .with_flex_grow(1.0);
            let inner = row(vec![prompt, field]).with_min_width(taffy::Dimension::length(40.0));

            let modal_node = block_with_title("Add project", vec![inner.with_fill()]).with_fill();
            modal(vec![modal_node])
        }
    }
}
