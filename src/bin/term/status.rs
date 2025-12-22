//! Status bar component.

use chatui::dom::Color;
use chatui::event::{Key, KeyCode};
use chatui::{Node, Style, TextSpan, rich_text, row};

use super::Msg;
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
pub fn status_bar_view(
    focus: Focus,
    status: Option<&StatusMessage>,
    auto_hide: bool,
    active_session: Option<(&Project, &Session)>,
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

    left_items.extend(shortcut_buttons(focus, &key_style, &base_style));

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

fn shortcut_buttons(focus: Focus, key_style: &Style, text_style: &Style) -> Vec<Node<Msg>> {
    let mut items = Vec::new();
    let mut idx = 0;

    match focus {
        Focus::Sidebar => {
            items.push(shortcut_button(
                "C+B",
                "switch focus",
                idx,
                key_style,
                text_style,
                || Msg::Key(ctrl_key(KeyCode::Char('b'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "C+Q",
                "quit",
                idx,
                key_style,
                text_style,
                || Msg::Key(ctrl_key(KeyCode::Char('q'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "p",
                "new project",
                idx,
                key_style,
                text_style,
                || Msg::Key(Key::new(KeyCode::Char('p'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "n",
                "new session",
                idx,
                key_style,
                text_style,
                || Msg::Key(Key::new(KeyCode::Char('n'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "C+,",
                "rename session",
                idx,
                key_style,
                text_style,
                || Msg::Key(ctrl_key(KeyCode::Char(','))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "d",
                "delete",
                idx,
                key_style,
                text_style,
                || Msg::Key(Key::new(KeyCode::Char('d'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "j",
                "next",
                idx,
                key_style,
                text_style,
                || Msg::Key(Key::new(KeyCode::Char('j'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "k",
                "previous",
                idx,
                key_style,
                text_style,
                || Msg::Key(Key::new(KeyCode::Char('k'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "⏎",
                "select",
                idx,
                key_style,
                text_style,
                || Msg::Key(Key::new(KeyCode::Enter)),
            ));
        }
        Focus::Terminal => {
            items.push(shortcut_button(
                "C+B",
                "switch focus",
                idx,
                key_style,
                text_style,
                || Msg::Key(ctrl_key(KeyCode::Char('b'))),
            ));
            idx += 1;
            items.push(shortcut_button(
                "C+↑",
                "previous session",
                idx,
                key_style,
                text_style,
                || Msg::Key(ctrl_key(KeyCode::Up)),
            ));
            idx += 1;
            items.push(shortcut_button(
                "C+↓",
                "next session",
                idx,
                key_style,
                text_style,
                || Msg::Key(ctrl_key(KeyCode::Down)),
            ));
            idx += 1;
            items.push(shortcut_button(
                "C+,",
                "rename session",
                idx,
                key_style,
                text_style,
                || Msg::Key(ctrl_key(KeyCode::Char(','))),
            ));
        }
    }

    items
}

fn shortcut_button(
    label: &'static str,
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

fn ctrl_key(code: KeyCode) -> Key {
    let mut key = Key::new(code);
    key.ctrl = true;
    key
}
