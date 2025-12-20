//! Status bar component.

use chatui::dom::Color;
use chatui::{Node, Style, TextSpan, rich_text, row};

use super::focus::Focus;
use super::project::Project;
use super::session::Session;

/// Kind of status message.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StatusKind {
    Error,
}

/// A status message to display in the status bar.
#[derive(Clone, Debug)]
pub struct StatusMessage {
    pub text: String,
    pub kind: StatusKind,
}

impl StatusMessage {
    /// Create an error status message.
    pub fn error(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            kind: StatusKind::Error,
        }
    }
}

/// Render the status bar.
pub fn status_bar_view<Msg: 'static>(
    focus: Focus,
    status: Option<&StatusMessage>,
    auto_hide: bool,
    active_session: Option<(&Project, &Session)>,
) -> Node<Msg> {
    let instructions: &[&str] = match focus {
        Focus::Sidebar => &[
            "C+B switch focus",
            "C+Q quit",
            "p new project",
            "n new session",
            "d delete",
            "j/k navigate",
            "⏎ select",
        ],
        Focus::Terminal => &["C+B switch focus", "C+↑/↓ prev/next session"],
    };
    let label = format!(" Focus: {:?} │ {} ", focus, instructions.join("  "));

    let base_style = Style::default();
    let mut left_spans = Vec::new();
    let mut right_spans = Vec::new();

    left_spans.push(TextSpan::new(label, base_style));

    if auto_hide
        && let Some((project, session)) = active_session
    {
        right_spans.push(TextSpan::new(&project.name, base_style));
        right_spans.push(TextSpan::new(" - ", base_style));
        right_spans.push(TextSpan::new(session.display_name(), base_style));
    }

    if let Some(status) = status {
        let mut style = base_style;
        match status.kind {
            StatusKind::Error => {
                style.fg = Some(Color::Red);
            }
        }
        left_spans.push(TextSpan::new(format!(" {} ", status.text), style));
        left_spans.push(TextSpan::new("│", base_style));
    }

    row(vec![
        rich_text(left_spans).with_flex_grow(1.),
        rich_text(right_spans),
    ])
}
