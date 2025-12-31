//! Focus management for the terminal application.

/// Which pane currently has keyboard focus.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Focus {
    #[default]
    Sidebar,
    Terminal,
    TerminalLocked,
}

impl Focus {
    /// Toggle between Sidebar and Terminal focus.
    pub fn toggle(self) -> Self {
        match self {
            Focus::Sidebar => Focus::Terminal,
            Focus::Terminal | Focus::TerminalLocked => Focus::Sidebar,
        }
    }

    pub fn is_terminal(self) -> bool {
        matches!(self, Focus::Terminal | Focus::TerminalLocked)
    }

    pub fn status_label(self) -> &'static str {
        match self {
            Focus::Sidebar => "sidebar",
            Focus::Terminal => "terminal",
            Focus::TerminalLocked => "locked",
        }
    }
}
