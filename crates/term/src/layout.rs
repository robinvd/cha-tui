//! Session layout helpers.

use chatui::dom::Style;
use chatui::{
    Node, ScrollMsg, ScrollState, TerminalMsg, block_with_title, column, row, scrollable_content,
    terminal, text,
};
use std::rc::Rc;
use taffy::{Dimension, FlexWrap};

use crate::divider::{horizontal_divider, vertical_divider};
use crate::session::{Session, SessionId};
use crate::sidebar::section_style;

const STRIP_TILE_FRACTION: f32 = 0.8;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LayoutKind {
    Zoom,
    Tall,
    Tall3,
    Focus,
    Wide,
    Strip,
}

impl LayoutKind {
    pub fn status_label(self) -> &'static str {
        match self {
            LayoutKind::Zoom => "zoom",
            LayoutKind::Tall => "tall",
            LayoutKind::Tall3 => "tall3",
            LayoutKind::Focus => "focus",
            LayoutKind::Wide => "wide",
            LayoutKind::Strip => "strip",
        }
    }
}

pub struct Layout<'a, Msg> {
    pub kind: LayoutKind,
    pub active_session: Option<SessionId>,
    pub focus: bool,
    pub on_terminal_msg: Rc<dyn Fn(SessionId, TerminalMsg) -> Msg>,
    pub on_focus: Rc<dyn Fn(SessionId) -> Msg>,
    pub strip_scroll: &'a ScrollState,
    pub on_strip_scroll: Rc<dyn Fn(ScrollMsg) -> Msg>,
}

impl<'a, Msg> Layout<'a, Msg> {
    pub fn new(
        kind: LayoutKind,
        active_session: Option<SessionId>,
        focus: bool,
        on_terminal_msg: Rc<dyn Fn(SessionId, TerminalMsg) -> Msg>,
        on_focus: Rc<dyn Fn(SessionId) -> Msg>,
        strip_scroll: &'a ScrollState,
        on_strip_scroll: Rc<dyn Fn(ScrollMsg) -> Msg>,
    ) -> Self {
        Self {
            kind,
            active_session,
            focus,
            on_terminal_msg,
            on_focus,
            strip_scroll,
            on_strip_scroll,
        }
    }
}

pub fn view_sessions<'a, Msg: 'static>(
    sessions: &'a [Session],
    layout: Layout<'a, Msg>,
) -> Node<'a, Msg> {
    match layout.kind {
        LayoutKind::Zoom => view_zoom(sessions, &layout),
        LayoutKind::Tall => view_tall(sessions, &layout),
        LayoutKind::Tall3 => view_tall3(sessions, &layout),
        LayoutKind::Focus => view_focus(sessions, &layout),
        LayoutKind::Wide => view_wide(sessions, &layout),
        LayoutKind::Strip => view_strip(sessions, &layout),
    }
}

fn terminal_placeholder<Msg: 'static>(focus: bool) -> Node<'static, Msg> {
    block_with_title(
        "Terminal",
        vec![text::<Msg>("Select or create a session to start.")],
    )
    .with_flex_grow(1.0)
    .with_style(section_style(focus))
}

fn divider_style() -> Style {
    let mut style = section_style(false);
    style.border = false;
    style
}

fn active_session_id(sessions: &[Session], active: Option<SessionId>) -> Option<SessionId> {
    active.or_else(|| sessions.first().map(|session| session.id))
}

fn terminal_node<'a, Msg: 'static>(
    session: &'a Session,
    layout: &Layout<'a, Msg>,
    is_active: bool,
) -> Node<'a, Msg> {
    let sid = session.id;
    let is_focused = layout.focus && is_active;
    let on_terminal_msg = Rc::clone(&layout.on_terminal_msg);
    let on_focus = Rc::clone(&layout.on_focus);
    terminal("terminal", &session.terminal, is_focused, move |msg| {
        on_terminal_msg(sid, msg)
    })
    .with_id_mixin("terminal", sid.0)
    .with_flex_grow(1.0)
    .with_style(section_style(is_focused))
    .on_click(move || on_focus(sid))
}

fn view_zoom<'a, Msg: 'static>(sessions: &'a [Session], layout: &Layout<'a, Msg>) -> Node<'a, Msg> {
    let Some(active_id) = active_session_id(sessions, layout.active_session) else {
        return terminal_placeholder(layout.focus);
    };
    let Some(active) = sessions.iter().find(|session| session.id == active_id) else {
        return terminal_placeholder(layout.focus);
    };
    terminal_node(active, layout, true)
}

fn view_tall<'a, Msg: 'static>(sessions: &'a [Session], layout: &Layout<'a, Msg>) -> Node<'a, Msg> {
    let Some(active_id) = active_session_id(sessions, layout.active_session) else {
        return terminal_placeholder(layout.focus);
    };
    let mut iter = sessions.iter();
    let Some(first) = iter.next() else {
        return terminal_placeholder(layout.focus);
    };

    let left = terminal_node(first, layout, first.id == active_id);
    let divider_style = divider_style();
    let mut right_children = Vec::new();
    for session in iter {
        if !right_children.is_empty() {
            right_children.push(horizontal_divider(divider_style));
        }
        right_children.push(terminal_node(session, layout, session.id == active_id));
    }
    if right_children.is_empty() {
        return left.with_flex_grow(1.0);
    }

    let right = column(right_children).with_flex_grow(1.0);
    row(vec![
        left.with_flex_grow(1.0),
        vertical_divider(divider_style),
        right,
    ])
    .with_flex_grow(1.0)
}

fn view_tall3<'a, Msg: 'static>(
    sessions: &'a [Session],
    layout: &Layout<'a, Msg>,
) -> Node<'a, Msg> {
    let Some(active_id) = active_session_id(sessions, layout.active_session) else {
        return terminal_placeholder(layout.focus);
    };
    let mut iter = sessions.iter();
    let Some(first) = iter.next() else {
        return terminal_placeholder(layout.focus);
    };

    let col1 = terminal_node(first, layout, first.id == active_id);
    let divider_style = divider_style();

    let Some(second) = iter.next() else {
        return col1.with_flex_grow(1.0);
    };
    let col2 = terminal_node(second, layout, second.id == active_id);

    let mut col3_children = Vec::new();
    for session in iter {
        if !col3_children.is_empty() {
            col3_children.push(horizontal_divider(divider_style));
        }
        col3_children.push(terminal_node(session, layout, session.id == active_id));
    }

    if col3_children.is_empty() {
        return row(vec![
            col1.with_flex_grow(1.0),
            vertical_divider(divider_style),
            col2.with_flex_grow(1.0),
        ])
        .with_flex_grow(1.0);
    }

    let col3 = column(col3_children).with_flex_grow(1.0);

    row(vec![
        col1.with_flex_grow(1.0),
        vertical_divider(divider_style),
        col2.with_flex_grow(1.0),
        vertical_divider(divider_style),
        col3,
    ])
    .with_flex_grow(1.0)
}

fn view_focus<'a, Msg: 'static>(
    sessions: &'a [Session],
    layout: &Layout<'a, Msg>,
) -> Node<'a, Msg> {
    let Some(active_id) = active_session_id(sessions, layout.active_session) else {
        return terminal_placeholder(layout.focus);
    };
    if sessions.is_empty() {
        return terminal_placeholder(layout.focus);
    }

    let Some(active_session) = sessions.iter().find(|session| session.id == active_id) else {
        return view_tall(sessions, layout);
    };

    let left = terminal_node(active_session, layout, true);
    let divider_style = divider_style();

    let mut right_children = Vec::new();
    for session in sessions {
        if session.id == active_id {
            continue;
        }
        if !right_children.is_empty() {
            right_children.push(horizontal_divider(divider_style));
        }
        right_children.push(terminal_node(session, layout, false));
    }

    if right_children.is_empty() {
        return left.with_flex_grow(1.0);
    }

    let right = column(right_children).with_flex_grow(1.0);
    row(vec![
        left.with_flex_grow(1.0),
        vertical_divider(divider_style),
        right,
    ])
    .with_flex_grow(1.0)
}

fn view_wide<'a, Msg: 'static>(sessions: &'a [Session], layout: &Layout<'a, Msg>) -> Node<'a, Msg> {
    let Some(active_id) = active_session_id(sessions, layout.active_session) else {
        return terminal_placeholder(layout.focus);
    };
    let mut iter = sessions.iter();
    let Some(first) = iter.next() else {
        return terminal_placeholder(layout.focus);
    };

    let top = terminal_node(first, layout, first.id == active_id);
    let divider_style = divider_style();
    let mut bottom_children = Vec::new();
    for session in iter {
        if !bottom_children.is_empty() {
            bottom_children.push(vertical_divider(divider_style));
        }
        bottom_children.push(terminal_node(session, layout, session.id == active_id));
    }
    if bottom_children.is_empty() {
        return top.with_flex_grow(1.0);
    }

    let bottom = row(bottom_children).with_flex_grow(1.0);
    column(vec![
        top.with_flex_grow(1.0),
        horizontal_divider(divider_style),
        bottom,
    ])
    .with_flex_grow(1.0)
}

fn view_strip<'a, Msg: 'static>(
    sessions: &'a [Session],
    layout: &Layout<'a, Msg>,
) -> Node<'a, Msg> {
    let Some(active_id) = active_session_id(sessions, layout.active_session) else {
        return terminal_placeholder(layout.focus);
    };
    let mut iter = sessions.iter();
    let Some(first) = iter.next() else {
        return terminal_placeholder(layout.focus);
    };

    let tile_width = Dimension::percent(STRIP_TILE_FRACTION);
    let divider_style = divider_style();

    let mut children = Vec::new();
    children.push(
        terminal_node(first, layout, first.id == active_id)
            .with_width(tile_width)
            .with_flex_grow(0.0)
            .with_flex_shrink(0.0),
    );
    for session in iter {
        children.push(
            vertical_divider(divider_style)
                .with_flex_grow(0.0)
                .with_flex_shrink(0.0),
        );
        children.push(
            terminal_node(session, layout, session.id == active_id)
                .with_width(tile_width)
                .with_flex_grow(0.0)
                .with_flex_shrink(0.0),
        );
    }

    let content = row(children)
        .with_flex_grow(1.0)
        .with_flex_wrap(FlexWrap::NoWrap);

    let on_strip_scroll = Rc::clone(&layout.on_strip_scroll);
    scrollable_content(
        "terminal-strip",
        layout.strip_scroll,
        3,
        move |msg| on_strip_scroll(msg),
        content,
    )
    .with_flex_grow(1.0)
}
