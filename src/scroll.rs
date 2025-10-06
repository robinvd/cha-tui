use std::fmt;

/// Alignment strategy when scrolling a target into view.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScrollAlignment {
    /// Place the target as close as possible without moving if it's already fully visible.
    Nearest,
    /// Align the top of the target with the top of the viewport.
    Start,
    /// Align the bottom of the target with the bottom of the viewport.
    End,
}

impl fmt::Display for ScrollAlignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = match self {
            Self::Nearest => "nearest",
            Self::Start => "start",
            Self::End => "end",
        };
        write!(f, "{}", label)
    }
}
