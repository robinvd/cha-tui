//! Modal dialog components.

use std::rc::Rc;

use super::project::ProjectId;
use super::session::SessionId;
use chatui::ScrollMsg;
use chatui::event::{Key, KeyCode};
use chatui::{InputMsg, InputState, InputStyle, Node, block_with_title, components::scroll, modal};
use taffy::Dimension;

/// State for modal dialogs.
#[derive(Clone)]
pub enum ModalState {
    NewProject {
        input: InputState,
        scroll: scroll::ScrollState,
    },
    RenameSession {
        input: InputState,
        scroll: scroll::ScrollState,
        project: ProjectId,
        session: SessionId,
    },
}

/// Messages for modal interactions.
#[derive(Clone, Debug)]
pub enum ModalMsg {
    Input(InputMsg),
    Scroll(ScrollMsg),
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
    /// Session rename was submitted.
    SessionRenamed {
        project: ProjectId,
        session: SessionId,
        name: String,
    },
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
        ModalMsg::Submit => match state {
            ModalState::NewProject { input, .. } => {
                let value = input.value();
                let trimmed = value.trim();
                if trimmed.is_empty() {
                    ModalResult::Cancelled
                } else {
                    ModalResult::ProjectSubmitted(trimmed.to_string())
                }
            }
            ModalState::RenameSession {
                input,
                project,
                session,
                ..
            } => {
                let value = input.value();
                let trimmed = value.trim();
                if trimmed.is_empty() {
                    ModalResult::Cancelled
                } else {
                    ModalResult::SessionRenamed {
                        project: *project,
                        session: *session,
                        name: trimmed.to_string(),
                    }
                }
            }
        },
        ModalMsg::Input(input_msg) => match state {
            ModalState::NewProject { input, .. } | ModalState::RenameSession { input, .. } => {
                input.update(input_msg);
                ModalResult::Continue
            }
        },
        ModalMsg::Scroll(input_msg) => match state {
            ModalState::NewProject { scroll, .. } | ModalState::RenameSession { scroll, .. } => {
                scroll.update(input_msg);
                ModalResult::Continue
            }
        },
    }
}

/// Render the modal dialog.
pub fn modal_view<Msg: Clone + 'static>(
    state: &ModalState,
    wrap_input: impl Fn(ModalMsg) -> Msg + 'static,
) -> Node<Msg> {
    let (title, input_state, scroll) = match state {
        ModalState::NewProject { input, scroll } => ("Add project", input, scroll),
        ModalState::RenameSession { input, scroll, .. } => ("Rename session", input, scroll),
    };
    let wrap_input = Rc::new(wrap_input);

    let input_style = InputStyle::default();
    let field = chatui::input::<Msg>(title, input_state, &input_style, {
        let wrap_input = Rc::clone(&wrap_input);
        move |e| wrap_input(ModalMsg::Input(e))
    })
    .with_height(Dimension::length(1.));
    let item = scroll::scrollable_content(
        "modal_scoll",
        scroll,
        3,
        {
            let wrap_input = Rc::clone(&wrap_input);
            move |e| wrap_input(ModalMsg::Scroll(e))
        },
        field,
    );

    let modal_node = block_with_title(title, vec![item])
        .with_min_width(Dimension::length(32.))
        .with_max_width(Dimension::length(64.));
    modal(vec![modal_node])
}
