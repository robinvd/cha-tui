
use super::{
    CommitMsg, DeleteMsg, DiffMsg, GlobalMsg, Key, KeyCode, Model, Msg, NavigationMsg,
};

#[derive(Clone)]
struct Shortcut {
    label: &'static str,
    description: &'static str,
    show_in_bar: bool,
    binding: Option<Binding>,
    msg: Option<Msg>,
}

#[derive(Clone, Copy)]
struct Binding {
    code: KeyCode,
    ctrl: ModifierRequirement,
    alt: ModifierRequirement,
    shift: ModifierRequirement,
}

#[derive(Clone, Copy)]
enum ModifierRequirement {
    Any,
    Enabled,
    Disabled,
}

impl ModifierRequirement {
    const fn matches(self, value: bool) -> bool {
        match self {
            Self::Any => true,
            Self::Enabled => value,
            Self::Disabled => !value,
        }
    }
}

impl Binding {
    const fn new(
        code: KeyCode,
        ctrl: ModifierRequirement,
        alt: ModifierRequirement,
        shift: ModifierRequirement,
    ) -> Self {
        Self {
            code,
            ctrl,
            alt,
            shift,
        }
    }

    fn matches_key(self, key: Key) -> bool {
        let code_matches = match (self.code, key.code) {
            (KeyCode::Char(expected), KeyCode::Char(actual)) => {
                if key.ctrl {
                    actual.eq_ignore_ascii_case(&expected)
                } else {
                    actual == expected
                }
            }
            _ => self.code == key.code,
        };

        code_matches
            && self.ctrl.matches(key.ctrl)
            && self.alt.matches(key.alt)
            && self.shift.matches(key.shift)
    }
}

pub(super) struct ShortcutDisplay {
    pub label: &'static str,
    pub description: &'static str,
    pub msg: Option<Msg>,
}

pub(super) fn message_for_key(model: &Model, key: Key) -> Option<Msg> {
    context_shortcuts(model)
        .iter()
        .chain(GLOBAL_SHORTCUTS.iter())
        .filter_map(|shortcut| shortcut.binding.map(|binding| (shortcut, binding)))
        .find(|(_, binding)| binding.matches_key(key))
        .and_then(|(shortcut, _)| shortcut.msg.as_ref().cloned())
}

pub(super) fn display_shortcuts(model: &Model) -> Vec<ShortcutDisplay> {
    context_shortcuts(model)
        .iter()
        .chain(GLOBAL_SHORTCUTS.iter())
        .filter(|shortcut| shortcut.show_in_bar)
        .map(|shortcut| ShortcutDisplay {
            label: shortcut.label,
            description: shortcut.description,
            msg: shortcut.msg.as_ref().cloned(),
        })
        .collect()
}

fn context_shortcuts(model: &Model) -> &'static [Shortcut] {
    if model.commit_modal.is_some() {
        COMMIT_MODAL_SHORTCUTS
    } else {
        NORMAL_SHORTCUTS
    }
}

const GLOBAL_SHORTCUTS: &[Shortcut] = &[
    Shortcut {
        label: "Ctrl-C",
        description: "Quit immediately",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('c'),
            ModifierRequirement::Enabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Global(GlobalMsg::Quit)),
    },
    Shortcut {
        label: "?",
        description: "Toggle shortcuts",
        show_in_bar: false,
        binding: Some(Binding::new(
            KeyCode::Char('?'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Global(GlobalMsg::ToggleShortcutsHelp)),
    },
];

const NORMAL_SHORTCUTS: &[Shortcut] = &[
    Shortcut {
        label: "Esc",
        description: "Quit",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Esc,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Global(GlobalMsg::Quit)),
    },
    Shortcut {
        label: "q",
        description: "Quit",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('q'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Global(GlobalMsg::Quit)),
    },
    Shortcut {
        label: "Enter",
        description: "Stage / Unstage",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Enter,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::ToggleStage)),
    },
    Shortcut {
        label: "Double-click",
        description: "Stage / Unstage",
        show_in_bar: true,
        binding: None,
        msg: None,
    },
    Shortcut {
        label: "Tab",
        description: "Toggle focus",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Tab,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::ToggleFocus)),
    },
    Shortcut {
        label: "Up",
        description: "Move selection up",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Up,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::MoveSelectionUp)),
    },
    Shortcut {
        label: "Down",
        description: "Move selection down",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Down,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::MoveSelectionDown)),
    },
    Shortcut {
        label: "⇠",
        description: "Collapse directory",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Left,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::CollapseNode)),
    },
    Shortcut {
        label: "",
        description: "Expand directory",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Right,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::ExpandNode)),
    },
    Shortcut {
        label: "j",
        description: "Scroll files",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('j'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::ScrollFiles(1))),
    },
    Shortcut {
        label: "k",
        description: "Scroll files",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('k'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Navigation(NavigationMsg::ScrollFiles(-1))),
    },
    Shortcut {
        label: "J",
        description: "Scroll diff",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('J'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Diff(DiffMsg::ScrollVertical(1))),
    },
    Shortcut {
        label: "K",
        description: "Scroll diff",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('K'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Diff(DiffMsg::ScrollVertical(-1))),
    },
    Shortcut {
        label: "l",
        description: "Scroll diff right",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('l'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Diff(DiffMsg::ScrollHorizontal(4))),
    },
    Shortcut {
        label: "h",
        description: "Scroll diff left",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('h'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Diff(DiffMsg::ScrollHorizontal(-4))),
    },
    Shortcut {
        label: "n",
        description: "Toggle line numbers",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('n'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Diff(DiffMsg::ToggleLineNumbers)),
    },
    Shortcut {
        label: "r",
        description: "Refresh status",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('r'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Global(GlobalMsg::RefreshStatus)),
    },
    Shortcut {
        label: "c",
        description: "Open commit",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('c'),
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Commit(CommitMsg::Open)),
    },
    Shortcut {
        label: "Ctrl-D",
        description: "Delete changes",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('d'),
            ModifierRequirement::Enabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Delete(DeleteMsg::Open)),
    },
];

const COMMIT_MODAL_SHORTCUTS: &[Shortcut] = &[
    Shortcut {
        label: "Ctrl-D",
        description: "Commit",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Char('d'),
            ModifierRequirement::Enabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Commit(CommitMsg::Submit)),
    },
    Shortcut {
        label: "Esc",
        description: "Cancel",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Esc,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: Some(Msg::Commit(CommitMsg::Cancel)),
    },
    Shortcut {
        label: "Backspace",
        description: "Delete character",
        show_in_bar: true,
        binding: Some(Binding::new(
            KeyCode::Backspace,
            ModifierRequirement::Disabled,
            ModifierRequirement::Disabled,
            ModifierRequirement::Any,
        )),
        msg: None,
    },
];
