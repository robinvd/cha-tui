//! Focus management for the terminal application.

/// Which pane currently has keyboard focus.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Focus {
    #[default]
    Sidebar,
    Terminal,
}

impl Focus {
    /// Toggle between Sidebar and Terminal focus.
    pub fn toggle(self) -> Self {
        match self {
            Focus::Sidebar => Focus::Terminal,
            Focus::Terminal => Focus::Sidebar,
        }
    }
}
