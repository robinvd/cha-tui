use ropey::Rope;
use termwiz::cell::unicode_column_width;

use crate::Style;
use crate::dom::{Color, Node, TextSpan, rich_text};
use crate::event::{Key, KeyCode, MouseEvent};

#[derive(Clone, Debug)]
pub struct InputState {
    rope: Rope,
    cursor: usize,
    anchor: usize,
}

impl Default for InputState {
    fn default() -> Self {
        Self {
            rope: Rope::new(),
            cursor: 0,
            anchor: 0,
        }
    }
}

impl InputState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_value(value: impl Into<String>) -> Self {
        let mut state = Self::default();
        state.set_value(value);
        state
    }

    pub fn value(&self) -> String {
        self.rope.to_string()
    }

    pub fn len_chars(&self) -> usize {
        self.rope.len_chars()
    }

    pub fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn selection(&self) -> Option<(usize, usize)> {
        if self.cursor == self.anchor {
            None
        } else if self.cursor < self.anchor {
            Some((self.cursor, self.anchor))
        } else {
            Some((self.anchor, self.cursor))
        }
    }

    pub fn clear(&mut self) {
        self.rope = Rope::new();
        self.cursor = 0;
        self.anchor = 0;
    }

    pub fn set_value(&mut self, value: impl Into<String>) {
        let value = value.into();
        self.rope = Rope::from_str(&value);
        let len = self.len_chars();
        self.cursor = len;
        self.anchor = len;
    }

    pub fn update(&mut self, msg: InputMsg) -> bool {
        match msg {
            InputMsg::InsertChar(ch) => self.insert_text(&ch.to_string()),
            InputMsg::InsertText(text) => self.insert_text(&text),
            InputMsg::DeleteBackward => self.delete_backward(),
            InputMsg::DeleteWordBackward => self.delete_word_backward(),
            InputMsg::DeleteToStart => self.delete_to_start(),
            InputMsg::DeleteToEnd => self.delete_to_end(),
            InputMsg::MoveLeft { extend } => self.move_left(extend),
            InputMsg::MoveRight { extend } => self.move_right(extend),
            InputMsg::MoveToStart { extend } => self.move_to_index(0, extend),
            InputMsg::MoveToEnd { extend } => {
                let len = self.len_chars();
                self.move_to_index(len, extend)
            }
            InputMsg::MoveWordLeft { extend } => {
                let target = self.word_start_before(self.cursor);
                self.move_to_index(target, extend)
            }
            InputMsg::MoveWordRight { extend } => {
                let target = self.next_word_boundary(self.cursor);
                self.move_to_index(target, extend)
            }
            InputMsg::Pointer {
                column,
                click_count,
            } => self.handle_pointer(column as usize, click_count.max(1)),
            InputMsg::Replace(text) => {
                self.set_value(text);
                true
            }
            InputMsg::SelectRange { start, end } => {
                self.set_selection(start, end);
                true
            }
            InputMsg::ClearSelection => {
                if self.cursor != self.anchor {
                    self.anchor = self.cursor;
                    true
                } else {
                    false
                }
            }
        }
    }

    fn insert_text(&mut self, text: &str) -> bool {
        if text.is_empty() {
            return false;
        }

        let text = text.replace(['\n', '\r'], " ");
        let mut changed = self.delete_selection();
        self.rope.insert(self.cursor, &text);
        let added = text.chars().count();
        self.cursor += added;
        self.anchor = self.cursor;
        changed |= added > 0;
        changed
    }

    fn delete_backward(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        if self.cursor == 0 {
            return false;
        }

        let prev = self.cursor - 1;
        self.rope.remove(prev..self.cursor);
        self.cursor = prev;
        self.anchor = prev;
        true
    }

    fn delete_word_backward(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        if self.cursor == 0 {
            return false;
        }

        let boundary = self.word_start_before(self.cursor);
        if boundary == self.cursor {
            return false;
        }
        self.rope.remove(boundary..self.cursor);
        self.cursor = boundary;
        self.anchor = boundary;
        true
    }

    fn delete_to_start(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        if self.cursor == 0 {
            return false;
        }

        self.rope.remove(0..self.cursor);
        self.cursor = 0;
        self.anchor = 0;
        true
    }

    fn delete_to_end(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        let len = self.len_chars();
        if self.cursor == len {
            return false;
        }
        self.rope.remove(self.cursor..len);
        self.anchor = self.cursor;
        true
    }

    fn move_left(&mut self, extend: bool) -> bool {
        if !extend && let Some((start, _)) = self.selection() {
            self.cursor = start;
            self.anchor = start;
            return true;
        }

        if self.cursor == 0 {
            if !extend {
                self.anchor = self.cursor;
            }
            return false;
        }

        self.cursor -= 1;
        if !extend {
            self.anchor = self.cursor;
        }
        true
    }

    fn move_right(&mut self, extend: bool) -> bool {
        if !extend && let Some((_, end)) = self.selection() {
            self.cursor = end;
            self.anchor = end;
            return true;
        }

        let len = self.len_chars();
        if self.cursor >= len {
            if !extend {
                self.anchor = self.cursor;
            }
            return false;
        }

        self.cursor += 1;
        if !extend {
            self.anchor = self.cursor;
        }
        true
    }

    fn move_to_index(&mut self, index: usize, extend: bool) -> bool {
        let clamped = index.min(self.len_chars());
        if !extend {
            self.anchor = clamped;
        }
        if self.cursor == clamped && self.anchor == clamped {
            return false;
        }
        self.cursor = clamped;
        if !extend {
            self.anchor = clamped;
        }
        true
    }

    fn set_selection(&mut self, start: usize, end: usize) {
        let len = self.len_chars();
        let start = start.min(len);
        let end = end.min(len);
        self.anchor = start;
        self.cursor = end;
    }

    fn delete_selection(&mut self) -> bool {
        if let Some((start, end)) = self.selection() {
            self.rope.remove(start..end);
            self.cursor = start;
            self.anchor = start;
            true
        } else {
            false
        }
    }

    fn handle_pointer(&mut self, column: usize, click_count: u8) -> bool {
        let index = self.column_to_index(column);

        match click_count {
            1 => self.move_to_index(index, false),
            2 => {
                let (start, end) = self.word_range_at(index);
                self.set_selection(start, end);
                true
            }
            3 => {
                let len = self.len_chars();
                self.set_selection(0, len);
                true
            }
            _ => self.move_to_index(index, false),
        }
    }

    fn column_to_index(&self, column: usize) -> usize {
        let mut consumed = 0usize;
        let mut index = 0usize;
        for ch in self.rope.chars() {
            let width = Self::char_width(ch);
            if column < consumed + width {
                return index;
            }
            consumed += width;
            index += 1;
        }
        index
    }

    fn prev_word_boundary(&self, mut index: usize) -> usize {
        if index == 0 {
            return 0;
        }

        if index > self.len_chars() {
            index = self.len_chars();
        }

        index = index.saturating_sub(1);

        let mut kind = self.word_kind_at(index).unwrap_or(WordKind::Whitespace);
        while index > 0 {
            let prev_kind = self.word_kind_at(index - 1).unwrap_or(WordKind::Whitespace);
            if prev_kind != kind {
                break;
            }
            index -= 1;
            kind = prev_kind;
        }
        index
    }

    fn next_word_boundary(&self, mut index: usize) -> usize {
        let len = self.len_chars();
        if index >= len {
            return len;
        }

        let kind = self.word_kind_at(index).unwrap_or(WordKind::Whitespace);
        while index < len {
            let next_kind = self.word_kind_at(index).unwrap_or(WordKind::Whitespace);
            if next_kind != kind {
                break;
            }
            index += 1;
        }
        index
    }

    fn word_start_before(&self, mut index: usize) -> usize {
        let len = self.len_chars();
        if index > len {
            index = len;
        }

        while index > 0 {
            match self.word_kind_at(index - 1) {
                Some(WordKind::Whitespace) => index -= 1,
                _ => break,
            }
        }

        if index == 0 {
            0
        } else {
            self.prev_word_boundary(index)
        }
    }

    fn word_range_at(&self, index: usize) -> (usize, usize) {
        let len = self.len_chars();
        if len == 0 {
            return (0, 0);
        }

        let idx = index.min(len.saturating_sub(1));
        let kind = self.word_kind_at(idx).unwrap_or(WordKind::Whitespace);
        let mut start = idx;
        while start > 0 {
            let prev_kind = self.word_kind_at(start - 1).unwrap_or(WordKind::Whitespace);
            if prev_kind != kind {
                break;
            }
            start -= 1;
        }

        let mut end = idx + 1;
        while end < len {
            let next_kind = self.word_kind_at(end).unwrap_or(WordKind::Whitespace);
            if next_kind != kind {
                break;
            }
            end += 1;
        }

        (start, end)
    }

    fn word_kind_at(&self, index: usize) -> Option<WordKind> {
        if index >= self.len_chars() {
            return None;
        }

        let ch = self.rope.char(index);
        Some(WordKind::classify(ch))
    }

    fn char_width(ch: char) -> usize {
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        unicode_column_width(s, None).max(1)
    }
}

#[derive(Clone, Debug)]
pub struct InputStyle {
    pub text: Style,
    pub selection: Style,
    pub cursor: Style,
    pub cursor_symbol: &'static str,
}

impl Default for InputStyle {
    fn default() -> Self {
        let selection = Style {
            bg: Some(Color::BrightBlue),
            fg: Some(Color::White),
            ..Style::default()
        };

        let cursor = Style {
            bg: Some(Color::White),
            fg: Some(Color::Black),
            bold: true,
            ..Style::default()
        };

        Self {
            text: Style::default(),
            selection,
            cursor,
            cursor_symbol: " ",
        }
    }
}

impl InputState {
    pub fn spans(&self, style: &InputStyle) -> Vec<TextSpan> {
        let selection = self.selection();
        let len = self.len_chars();

        if selection.is_none() && len == 0 {
            return vec![TextSpan::new(style.cursor_symbol, style.cursor.clone())];
        }

        let mut spans = Vec::new();

        match selection {
            Some((start, end)) => {
                if start > 0 {
                    spans.push(TextSpan::new(
                        self.rope.slice(..start).to_string(),
                        style.text.clone(),
                    ));
                }

                if end > start {
                    spans.push(TextSpan::new(
                        self.rope.slice(start..end).to_string(),
                        style.selection.clone(),
                    ));
                }

                if end < len {
                    spans.push(TextSpan::new(
                        self.rope.slice(end..).to_string(),
                        style.text.clone(),
                    ));
                }
            }
            None => {
                if self.cursor > 0 {
                    spans.push(TextSpan::new(
                        self.rope.slice(..self.cursor).to_string(),
                        style.text.clone(),
                    ));
                }

                if self.cursor < len {
                    let cursor_char = self.rope.char(self.cursor).to_string();
                    spans.push(TextSpan::new(cursor_char, style.cursor.clone()));

                    let trailing = self.rope.slice((self.cursor + 1)..).to_string();
                    if !trailing.is_empty() {
                        spans.push(TextSpan::new(trailing, style.text.clone()));
                    }
                } else {
                    spans.push(TextSpan::new(
                        style.cursor_symbol.to_string(),
                        style.cursor.clone(),
                    ));
                }
            }
        }

        spans
    }
}

#[derive(Clone, Debug)]
pub enum InputMsg {
    InsertChar(char),
    InsertText(String),
    DeleteBackward,
    DeleteWordBackward,
    DeleteToStart,
    DeleteToEnd,
    MoveLeft { extend: bool },
    MoveRight { extend: bool },
    MoveToStart { extend: bool },
    MoveToEnd { extend: bool },
    MoveWordLeft { extend: bool },
    MoveWordRight { extend: bool },
    Pointer { column: u16, click_count: u8 },
    Replace(String),
    SelectRange { start: usize, end: usize },
    ClearSelection,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WordKind {
    Whitespace,
    Alphanumeric,
    Other,
}

impl WordKind {
    fn classify(ch: char) -> Self {
        if ch.is_whitespace() {
            Self::Whitespace
        } else if ch.is_alphanumeric() || ch == '_' {
            Self::Alphanumeric
        } else {
            Self::Other
        }
    }
}

pub fn input<Msg>(
    id: &'static str,
    state: &InputState,
    style: &InputStyle,
    map_msg: impl Fn(InputMsg) -> Msg + 'static,
) -> Node<Msg>
where
    Msg: 'static,
{
    use std::rc::Rc;

    let spans = state.spans(style);
    let mut node = rich_text::<Msg>(spans).with_id(id);

    let handler = Rc::new(map_msg);
    let mouse_handler = handler.clone();

    node = node.on_mouse(move |event: MouseEvent| {
        if event.buttons.left && event.click_count > 0 {
            let msg = InputMsg::Pointer {
                column: event.local_x,
                click_count: event.click_count,
            };
            Some(mouse_handler(msg))
        } else {
            None
        }
    });

    node
}

pub fn default_keybindings<UpdateMsg>(
    key: Key,
    map: impl Fn(InputMsg) -> UpdateMsg,
) -> Option<UpdateMsg> {
    let msg = match key.code {
        KeyCode::Char(ch) if !key.ctrl && !key.alt => Some(InputMsg::InsertChar(ch)),
        KeyCode::Char('h') if key.ctrl => Some(InputMsg::DeleteBackward),
        KeyCode::Char('w') if key.ctrl => Some(InputMsg::DeleteWordBackward),
        KeyCode::Char('u') if key.ctrl => Some(InputMsg::DeleteToStart),
        KeyCode::Char('k') if key.ctrl => Some(InputMsg::DeleteToEnd),
        KeyCode::Char('a') if key.ctrl => Some(InputMsg::MoveToStart { extend: key.shift }),
        KeyCode::Char('e') if key.ctrl => Some(InputMsg::MoveToEnd { extend: key.shift }),
        KeyCode::Backspace if key.ctrl || key.alt => Some(InputMsg::DeleteWordBackward),
        KeyCode::Backspace => Some(InputMsg::DeleteBackward),
        KeyCode::Left if key.ctrl || key.alt => Some(InputMsg::MoveWordLeft { extend: key.shift }),
        KeyCode::Right if key.ctrl || key.alt => {
            Some(InputMsg::MoveWordRight { extend: key.shift })
        }
        KeyCode::Left => Some(InputMsg::MoveLeft { extend: key.shift }),
        KeyCode::Right => Some(InputMsg::MoveRight { extend: key.shift }),
        KeyCode::Char(_) => None,
        KeyCode::Enter | KeyCode::Esc | KeyCode::Tab | KeyCode::Up | KeyCode::Down => None,
    }?;

    Some(map(msg))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_and_delete_text() {
        let mut state = InputState::default();
        state.update(InputMsg::InsertText("abc".into()));
        assert_eq!(state.value(), "abc");

        state.update(InputMsg::MoveLeft { extend: false });
        state.update(InputMsg::DeleteBackward);
        assert_eq!(state.value(), "ac");
        assert_eq!(state.cursor(), 1);
    }

    #[test]
    fn ctrl_w_deletes_previous_word() {
        let mut state = InputState::with_value("hello world");
        state.update(InputMsg::DeleteWordBackward);
        assert_eq!(state.value(), "hello ");
    }

    #[test]
    fn double_click_selects_word() {
        let mut state = InputState::with_value("foo bar");
        state.update(InputMsg::Pointer {
            column: 2,
            click_count: 2,
        });
        assert_eq!(state.selection(), Some((0, 3)));
    }

    #[test]
    fn spans_highlight_character_under_cursor() {
        let mut state = InputState::with_value("abc");
        state.update(InputMsg::MoveToStart { extend: false });
        let style = InputStyle::default();
        let spans = state.spans(&style);

        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].content, "a");
        assert_eq!(spans[0].style, style.cursor);
        assert_eq!(spans[1].content, "bc");
        assert_eq!(spans[1].style, style.text);
    }

    #[test]
    fn spans_render_placeholder_when_cursor_at_end() {
        let state = InputState::with_value("hi");
        let style = InputStyle::default();
        let spans = state.spans(&style);

        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].content, "hi");
        assert_eq!(spans[0].style, style.text);
        assert_eq!(spans[1].content, style.cursor_symbol);
        assert_eq!(spans[1].style, style.cursor);
    }
}
