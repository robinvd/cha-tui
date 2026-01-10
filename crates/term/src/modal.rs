//! Modal dialog components.

use std::rc::Rc;

use super::project::{ProjectId, WorktreeId};
use super::session::SessionId;
use super::vcs::VcsKind;
use chatui::ScrollMsg;
use chatui::dom::Color;
use chatui::event::{Key, KeyCode};
use chatui::{
    InputMsg, InputState, InputStyle, Node, block_with_title, column, components::scroll, modal,
    text,
};
use taffy::Dimension;

/// State for modal dialogs.
#[derive(Clone)]
pub enum ModalState {
    NewProject {
        input: InputState,
        scroll: scroll::ScrollState,
    },
    NewWorktree {
        input: InputState,
        scroll: scroll::ScrollState,
        project: ProjectId,
        vcs: VcsKind,
    },
    RenameSession {
        input: InputState,
        scroll: scroll::ScrollState,
        project: ProjectId,
        worktree: Option<WorktreeId>,
        session: SessionId,
    },
    SessionFilter {
        input: InputState,
        scroll: scroll::ScrollState,
    },
}

/// Messages for modal interactions.
#[derive(Clone, Debug)]
pub enum ModalMsg {
    Input(InputMsg),
    Scroll(ScrollMsg),
    Submit,
    Cancel,
    ToggleVcs,
}

/// Result of modal update.
pub enum ModalResult {
    /// Modal is still open, continue.
    Continue,
    /// Modal was cancelled.
    Cancelled,
    /// New project path was submitted.
    ProjectSubmitted(String),
    /// New worktree name was submitted.
    WorktreeSubmitted {
        project: ProjectId,
        name: String,
        vcs: VcsKind,
    },
    /// Session rename was submitted.
    SessionRenamed {
        project: ProjectId,
        worktree: Option<WorktreeId>,
        session: SessionId,
        name: String,
    },
    /// Session filter was submitted.
    FilterSubmitted(String),
}

/// Handle a key event for the modal, returning a message if applicable.
pub fn modal_handle_key(key: Key) -> Option<ModalMsg> {
    if key.ctrl && matches!(key.code, KeyCode::Char('t')) {
        return Some(ModalMsg::ToggleVcs);
    }
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
            ModalState::NewWorktree {
                input,
                project,
                vcs,
                ..
            } => {
                let value = input.value();
                let trimmed = value.trim();
                if trimmed.is_empty() {
                    ModalResult::Cancelled
                } else {
                    ModalResult::WorktreeSubmitted {
                        project: *project,
                        name: trimmed.to_string(),
                        vcs: *vcs,
                    }
                }
            }
            ModalState::RenameSession {
                input,
                project,
                worktree,
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
                        worktree: *worktree,
                        session: *session,
                        name: trimmed.to_string(),
                    }
                }
            }
            ModalState::SessionFilter { input, .. } => {
                let value = input.value();
                ModalResult::FilterSubmitted(value)
            }
        },
        ModalMsg::Input(input_msg) => match state {
            ModalState::NewProject { input, .. }
            | ModalState::NewWorktree { input, .. }
            | ModalState::RenameSession { input, .. }
            | ModalState::SessionFilter { input, .. } => {
                input.update(input_msg);
                ModalResult::Continue
            }
        },
        ModalMsg::Scroll(input_msg) => match state {
            ModalState::NewProject { scroll, .. }
            | ModalState::NewWorktree { scroll, .. }
            | ModalState::RenameSession { scroll, .. }
            | ModalState::SessionFilter { scroll, .. } => {
                scroll.update(input_msg);
                ModalResult::Continue
            }
        },
        ModalMsg::ToggleVcs => match state {
            ModalState::NewWorktree { vcs, .. } => {
                *vcs = vcs.toggle();
                ModalResult::Continue
            }
            _ => ModalResult::Continue,
        },
    }
}

/// Render the modal dialog.
pub fn modal_view<Msg: Clone + 'static>(
    state: &ModalState,
    wrap_input: impl Fn(ModalMsg) -> Msg + 'static,
) -> Node<'_, Msg> {
    let (title, input_state, scroll, vcs_label) = match state {
        ModalState::NewProject { input, scroll } => ("Add project", input, scroll, None),
        ModalState::NewWorktree {
            input, scroll, vcs, ..
        } => ("Add worktree", input, scroll, Some(*vcs)),
        ModalState::RenameSession { input, scroll, .. } => ("Rename session", input, scroll, None),
        ModalState::SessionFilter { input, scroll } => ("Filter sessions", input, scroll, None),
    };
    let wrap_input = Rc::new(wrap_input);

    let mut input_style = InputStyle::default();
    input_style.text.bg = Some(Color::BrightBlack);
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

    let content = if let Some(vcs) = vcs_label {
        let label = text::<Msg>(format!("using {} (Ctrl+T to toggle)", vcs.label()))
            .with_height(Dimension::length(2.));
        column(vec![label, item])
    } else {
        item
    };

    let modal_node = block_with_title(title, vec![content])
        .with_min_width(Dimension::length(32.))
        .with_max_width(Dimension::length(64.));
    modal(vec![modal_node])
}
