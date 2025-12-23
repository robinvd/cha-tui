//! Status bar component.

use chatui::dom::Color;
use chatui::{Node, Style, TextSpan, rich_text, row};

use super::Msg;
use super::focus::Focus;
use super::keymap::{Keymap, Shortcut};
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
pub fn status_bar_view(
    focus: Focus,
    status: Option<&StatusMessage>,
    auto_hide: bool,
    active_session: Option<(&Project, &Session)>,
    terminal_locked: bool,
    keymap: &Keymap,
) -> Node<Msg> {
    let base_style = Style::default();
    let key_style = Style::bold();
    let mut right_spans = Vec::new();

    let mut left_items: Vec<Node<Msg>> = Vec::new();
    left_items.push(
        rich_text(vec![TextSpan::new(
            format!(" Focus: {:?} │", focus),
            base_style,
        )])
        .with_id("status-focus"),
    );

    left_items.extend(shortcut_buttons(
        keymap.status_shortcuts(focus == Focus::Terminal, terminal_locked),
        &key_style,
        &base_style,
    ));

    if auto_hide && let Some((project, session)) = active_session {
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
        left_items.push(
            rich_text(vec![
                TextSpan::new(format!(" {} ", status.text), style),
                TextSpan::new("│", base_style),
            ])
            .with_id("status-message"),
        );
    }

    row(vec![
        row(left_items)
            .with_gap(1, 0)
            .with_flex_grow(1.)
            .with_id("status-left"),
        rich_text(right_spans),
    ])
}

fn shortcut_buttons(shortcuts: &[Shortcut], key_style: &Style, text_style: &Style) -> Vec<Node<Msg>> {
    let mut items = Vec::new();

    for (idx, shortcut) in shortcuts.iter().enumerate() {
        let chord = shortcut.chord;
        let label = chord.label();
        let description = shortcut.description;
        items.push(shortcut_button(
            label,
            description,
            idx as u64,
            key_style,
            text_style,
            move || Msg::Key(chord.to_key()),
        ));
    }

    items
}

fn shortcut_button(
    label: impl Into<String>,
    description: &'static str,
    idx: u64,
    key_style: &Style,
    text_style: &Style,
    on_click: impl Fn() -> Msg + 'static,
) -> Node<Msg> {
    let spans = vec![
        TextSpan::new(label, *key_style),
        TextSpan::new(" ", *text_style),
        TextSpan::new(description, *text_style),
    ];

    rich_text(spans)
        .with_padding_2d(1, 0)
        .with_id_mixin("status-shortcut", idx)
        .on_click(on_click)
}
