//! Keybinding definitions and resolution for the term application.

use std::collections::HashMap;

use chatui::event::{Key, KeyCode};
/// Logical scope in which a keybinding is active.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Scope {
    Global,
    Sidebar,
    Terminal,
    TerminalLocked,
}

/// Actions that can be triggered by keybindings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    ToggleFocus,
    Quit,
    ToggleAutoHide,
    FocusTerminal,
    MoveSelectionUp,
    MoveSelectionDown,
    MoveItemUp,
    MoveItemDown,
    ActivateSelected,
    NewProject,
    NewWorktree,
    NewSession,
    DeleteSelected,
    RenameSession,
    SessionByIndex(usize),
    NextSession,
    PrevSession,
    ToggleTerminalLock,
}

/// Normalized representation of a key chord.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KeyChord {
    pub key: KeyCode,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub super_key: bool,
}

impl KeyChord {
    pub const fn new(key: KeyCode) -> Self {
        Self {
            key,
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
        }
    }

    pub const fn ctrl(key: KeyCode) -> Self {
        Self {
            key,
            ctrl: true,
            alt: false,
            shift: false,
            super_key: false,
        }
    }

    pub const fn super_key(key: KeyCode) -> Self {
        Self {
            key,
            ctrl: false,
            alt: false,
            shift: false,
            super_key: true,
        }
    }

    pub const fn shift(key: KeyCode) -> Self {
        Self {
            key,
            ctrl: false,
            alt: false,
            shift: true,
            super_key: false,
        }
    }

    pub fn from_key(key: Key) -> Self {
        Self {
            key: key.code,
            ctrl: key.ctrl,
            alt: key.alt,
            shift: key.shift,
            super_key: key.super_key,
        }
    }

    pub fn to_key(self) -> Key {
        let mut key = Key::new(self.key);
        key.ctrl = self.ctrl;
        key.alt = self.alt;
        key.shift = self.shift;
        key.super_key = self.super_key;
        key
    }

    pub fn label(&self) -> String {
        let mut parts = Vec::new();
        if self.ctrl {
            parts.push("C+".to_string());
        }
        if self.super_key {
            parts.push("⌘+".to_string());
        }
        if self.alt {
            parts.push("A+".to_string());
        }
        if self.shift {
            parts.push("S+".to_string());
        }

        let key_str = match self.key {
            KeyCode::Char(' ') => "Space".to_string(),
            KeyCode::Char(c) => c.to_string().to_uppercase(),
            KeyCode::Enter => "⏎".to_string(),
            KeyCode::Esc => "Esc".to_string(),
            KeyCode::Backspace => "Backspace".to_string(),
            KeyCode::Left => "←".to_string(),
            KeyCode::Right => "→".to_string(),
            KeyCode::Up => "↑".to_string(),
            KeyCode::Down => "↓".to_string(),
            KeyCode::PageUp => "PageUp".to_string(),
            KeyCode::PageDown => "PageDown".to_string(),
            KeyCode::Home => "Home".to_string(),
            KeyCode::End => "End".to_string(),
            KeyCode::Tab => "Tab".to_string(),
            KeyCode::Function(n) => format!("F{n}"),
            _ => format!("{:?}", self.key),
        };

        parts.push(key_str);
        parts.join("")
    }
}

/// A single keybinding entry.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Binding {
    pub scope: Scope,
    pub chord: KeyChord,
    pub action: Action,
    pub description: &'static str,
    pub show_in_status: bool,
}

/// Default keymap used by the application.
pub static DEFAULT_BINDINGS: &[Binding] = &[
    // Global bindings
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('b')),
        action: Action::ToggleFocus,
        description: "switch focus",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char(',')),
        action: Action::RenameSession,
        description: "rename session",
        show_in_status: true,
    },
    // Session selection by number (Ctrl)
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('1')),
        action: Action::SessionByIndex(1),
        description: "session 1",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('2')),
        action: Action::SessionByIndex(2),
        description: "session 2",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('3')),
        action: Action::SessionByIndex(3),
        description: "session 3",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('4')),
        action: Action::SessionByIndex(4),
        description: "session 4",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('5')),
        action: Action::SessionByIndex(5),
        description: "session 5",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('6')),
        action: Action::SessionByIndex(6),
        description: "session 6",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('7')),
        action: Action::SessionByIndex(7),
        description: "session 7",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('8')),
        action: Action::SessionByIndex(8),
        description: "session 8",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('9')),
        action: Action::SessionByIndex(9),
        description: "session 9",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Char('0')),
        action: Action::SessionByIndex(10),
        description: "session 10",
        show_in_status: false,
    },
    // Session selection by number (Super)
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('1')),
        action: Action::SessionByIndex(1),
        description: "session 1",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('2')),
        action: Action::SessionByIndex(2),
        description: "session 2",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('3')),
        action: Action::SessionByIndex(3),
        description: "session 3",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('4')),
        action: Action::SessionByIndex(4),
        description: "session 4",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('5')),
        action: Action::SessionByIndex(5),
        description: "session 5",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('6')),
        action: Action::SessionByIndex(6),
        description: "session 6",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('7')),
        action: Action::SessionByIndex(7),
        description: "session 7",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('8')),
        action: Action::SessionByIndex(8),
        description: "session 8",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('9')),
        action: Action::SessionByIndex(9),
        description: "session 9",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Char('0')),
        action: Action::SessionByIndex(10),
        description: "session 10",
        show_in_status: false,
    },
    // Next/previous session (Ctrl or Super)
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Up),
        action: Action::PrevSession,
        description: "previous session",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::ctrl(KeyCode::Down),
        action: Action::NextSession,
        description: "next session",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Up),
        action: Action::PrevSession,
        description: "previous session",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Global,
        chord: KeyChord::super_key(KeyCode::Down),
        action: Action::NextSession,
        description: "next session",
        show_in_status: false,
    },
    // Sidebar scope
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::ctrl(KeyCode::Char('q')),
        action: Action::Quit,
        description: "quit",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Char('h')),
        action: Action::ToggleAutoHide,
        description: "toggle auto-hide",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::ctrl(KeyCode::Right),
        action: Action::FocusTerminal,
        description: "focus terminal",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::shift(KeyCode::Up),
        action: Action::MoveItemUp,
        description: "move up",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::shift(KeyCode::Down),
        action: Action::MoveItemDown,
        description: "move down",
        show_in_status: false,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Up),
        action: Action::MoveSelectionUp,
        description: "previous",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Char('k')),
        action: Action::MoveSelectionUp,
        description: "previous",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Down),
        action: Action::MoveSelectionDown,
        description: "next",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Char('j')),
        action: Action::MoveSelectionDown,
        description: "next",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Enter),
        action: Action::ActivateSelected,
        description: "select",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Char('p')),
        action: Action::NewProject,
        description: "new project",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Char('w')),
        action: Action::NewWorktree,
        description: "new worktree",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Char('n')),
        action: Action::NewSession,
        description: "new session",
        show_in_status: true,
    },
    Binding {
        scope: Scope::Sidebar,
        chord: KeyChord::new(KeyCode::Char('d')),
        action: Action::DeleteSelected,
        description: "delete",
        show_in_status: true,
    },
    // Terminal scope
    Binding {
        scope: Scope::Terminal,
        chord: KeyChord::ctrl(KeyCode::Char('g')),
        action: Action::ToggleTerminalLock,
        description: "lock terminal",
        show_in_status: true,
    },
    // Terminal locked scope
    Binding {
        scope: Scope::TerminalLocked,
        chord: KeyChord::ctrl(KeyCode::Char('g')),
        action: Action::ToggleTerminalLock,
        description: "unlock terminal",
        show_in_status: true,
    },
];

/// Shortcut entry for UI display.
#[derive(Clone, Debug)]
pub struct Shortcut {
    pub chord: KeyChord,
    pub description: &'static str,
}

pub struct Keymap {
    lookup: HashMap<Scope, HashMap<KeyChord, Action>>,
    sidebar_shortcuts: Vec<Shortcut>,
    terminal_shortcuts: Vec<Shortcut>,
    terminal_locked_shortcuts: Vec<Shortcut>,
}

impl Keymap {
    pub fn new(bindings: Vec<Binding>) -> Self {
        let mut lookup = HashMap::new();
        for binding in &bindings {
            lookup
                .entry(binding.scope)
                .or_insert_with(HashMap::new)
                .entry(binding.chord)
                .or_insert(binding.action);
        }
        let sidebar_shortcuts = build_shortcuts(
            &bindings,
            &SIDEBAR_ACTIONS,
            &[Scope::Sidebar, Scope::Global],
        );
        let terminal_shortcuts = build_shortcuts(
            &bindings,
            &TERMINAL_ACTIONS,
            &[Scope::Terminal, Scope::Global],
        );
        let terminal_locked_shortcuts = build_shortcuts(
            &bindings,
            &TERMINAL_LOCKED_ACTIONS,
            &[Scope::TerminalLocked],
        );

        Self {
            lookup,
            sidebar_shortcuts,
            terminal_shortcuts,
            terminal_locked_shortcuts,
        }
    }

    /// Resolve a key to an action, searching scopes in order.
    pub fn resolve(&self, scopes: &[Scope], key: Key) -> Option<Action> {
        let chord = KeyChord::from_key(key);
        for scope in scopes {
            if let Some(action) = self
                .lookup
                .get(scope)
                .and_then(|bindings| bindings.get(&chord))
                .copied()
            {
                return Some(action);
            }
        }
        None
    }

    /// Shortcuts to display in the status bar for the given focus/lock state.
    pub fn status_shortcuts(&self, focus_terminal: bool, terminal_locked: bool) -> &[Shortcut] {
        if focus_terminal && terminal_locked {
            return &self.terminal_locked_shortcuts;
        }

        if focus_terminal {
            return &self.terminal_shortcuts;
        }

        &self.sidebar_shortcuts
    }
}

impl Default for Keymap {
    fn default() -> Self {
        Self::new(DEFAULT_BINDINGS.to_vec())
    }
}

const SIDEBAR_ACTIONS: [Action; 10] = [
    Action::ToggleFocus,
    Action::Quit,
    Action::NewProject,
    Action::NewWorktree,
    Action::NewSession,
    Action::RenameSession,
    Action::DeleteSelected,
    Action::MoveSelectionDown,
    Action::MoveSelectionUp,
    Action::ActivateSelected,
];

const TERMINAL_ACTIONS: [Action; 5] = [
    Action::ToggleFocus,
    Action::PrevSession,
    Action::NextSession,
    Action::RenameSession,
    Action::ToggleTerminalLock,
];

const TERMINAL_LOCKED_ACTIONS: [Action; 1] = [Action::ToggleTerminalLock];

fn find_first(bindings: &[Binding], scope: Scope, action: Action) -> Option<Shortcut> {
    bindings
        .iter()
        .find(|b| b.scope == scope && b.action == action && b.show_in_status)
        .map(|b| Shortcut {
            chord: b.chord,
            description: b.description,
        })
}

fn build_shortcuts(bindings: &[Binding], actions: &[Action], scopes: &[Scope]) -> Vec<Shortcut> {
    actions
        .iter()
        .filter_map(|action| {
            scopes
                .iter()
                .find_map(|scope| find_first(bindings, *scope, *action))
        })
        .collect()
}
