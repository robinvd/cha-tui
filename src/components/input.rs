use ropey::Rope;
use termwiz::cell::unicode_column_width;

use crate::Style;
use crate::dom::{Color, Node, Renderable, TextSpan, leaf};
use crate::event::{Key, KeyCode, MouseEvent};
use crate::render::LeafRenderContext;
use taffy::AvailableSpace;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputMode {
    SingleLine,
    Multiline,
}

#[derive(Clone, Debug)]
pub struct InputState {
    rope: Rope,
    cursor: usize,
    anchor: usize,
    viewport_cols: usize,
    viewport_rows: usize,
    scroll_x: usize,
    scroll_y: usize,
    mode: InputMode,
    preferred_column: Option<usize>,
}

impl Default for InputState {
    fn default() -> Self {
        Self {
            rope: Rope::new(),
            cursor: 0,
            anchor: 0,
            viewport_cols: 0,
            scroll_x: 0,
            viewport_rows: 0,
            scroll_y: 0,
            mode: InputMode::SingleLine,
            preferred_column: None,
        }
    }
}

impl InputState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_mode(mode: InputMode) -> Self {
        Self {
            mode,
            ..Self::default()
        }
    }

    pub fn new_multiline() -> Self {
        Self::with_mode(InputMode::Multiline)
    }

    pub fn with_value_and_mode(value: impl Into<String>, mode: InputMode) -> Self {
        let mut state = Self::with_mode(mode);
        state.set_value(value);
        state
    }

    pub fn with_value(value: impl Into<String>) -> Self {
        Self::with_value_and_mode(value, InputMode::SingleLine)
    }

    pub fn with_value_multiline(value: impl Into<String>) -> Self {
        Self::with_value_and_mode(value, InputMode::Multiline)
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

    pub fn scroll_x(&self) -> usize {
        self.scroll_x
    }

    pub fn scroll_y(&self) -> usize {
        self.scroll_y
    }

    pub fn mode(&self) -> InputMode {
        self.mode
    }

    pub fn is_multiline(&self) -> bool {
        matches!(self.mode, InputMode::Multiline)
    }

    pub fn set_mode(&mut self, mode: InputMode) {
        if self.mode != mode {
            self.mode = mode;
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        }
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
        self.scroll_x = 0;
        self.scroll_y = 0;
        self.reset_preferred_column();
    }

    pub fn set_value(&mut self, value: impl Into<String>) {
        let value = value.into();
        self.rope = Rope::from_str(&value);
        let len = self.len_chars();
        self.cursor = len;
        self.anchor = len;
        self.scroll_x = 0;
        self.scroll_y = 0;
        self.reset_preferred_column();
    }

    fn reset_preferred_column(&mut self) {
        self.preferred_column = None;
    }

    fn cursor_line(&self) -> usize {
        let idx = self.cursor.min(self.len_chars());
        self.rope.char_to_line(idx)
    }

    fn line_count(&self) -> usize {
        self.rope.len_lines().max(1)
    }

    fn column_in_line(&self, line_index: usize, char_index: usize) -> usize {
        let start = self.rope.line_to_char(line_index).min(char_index);
        self.display_width_between(start, char_index)
    }

    fn display_width_between(&self, start: usize, end: usize) -> usize {
        self.rope.slice(start..end).chars().fold(0usize, |acc, ch| {
            if ch == '\n' {
                0
            } else {
                acc + Self::char_width(ch)
            }
        })
    }

    fn index_at_line_column(&self, line_index: usize, column: usize) -> usize {
        let line_start = self.rope.line_to_char(line_index).min(self.len_chars());
        let line_end = self
            .rope
            .line_to_char((line_index + 1).min(self.rope.len_lines()))
            .min(self.len_chars());

        let mut idx = line_start;
        let mut col = 0usize;
        while idx < line_end {
            let ch = self.rope.char(idx);
            if ch == '\n' {
                break;
            }
            let width = Self::char_width(ch);
            if col + width > column {
                break;
            }
            col += width;
            idx += 1;
        }

        idx
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
            InputMsg::MoveUp { extend } => self.move_up(extend),
            InputMsg::MoveDown { extend } => self.move_down(extend),
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
                row,
                click_count,
            } => self.handle_pointer(column as usize, row as usize, click_count.max(1)),
            InputMsg::Replace(text) => {
                self.set_value(text);
                self.ensure_cursor_visible();
                true
            }
            InputMsg::SelectRange { start, end } => {
                self.set_selection(start, end);
                self.ensure_cursor_visible();
                true
            }
            InputMsg::ClearSelection => {
                if self.cursor != self.anchor {
                    self.anchor = self.cursor;
                    self.reset_preferred_column();
                    self.ensure_cursor_visible();
                    true
                } else {
                    false
                }
            }
            InputMsg::SetViewportSize { cols, rows } => {
                let cols = cols.max(1);
                let rows = rows.max(1);
                let changed = self.viewport_cols != cols || self.viewport_rows != rows;
                self.viewport_cols = cols;
                self.viewport_rows = rows;
                self.ensure_cursor_visible();
                changed
            }
        }
    }

    fn insert_text(&mut self, text: &str) -> bool {
        if text.is_empty() {
            return false;
        }

        let text = if self.is_multiline() {
            let sanitized = text.replace('\r', "");
            if sanitized.is_empty() {
                return false;
            }
            sanitized
        } else {
            text.replace(['\n', '\r'], " ")
        };
        if text.is_empty() {
            return false;
        }

        let mut changed = self.delete_selection();
        self.rope.insert(self.cursor, &text);
        let added = text.chars().count();
        self.cursor += added;
        self.anchor = self.cursor;
        self.reset_preferred_column();
        changed |= added > 0;
        self.ensure_cursor_visible();
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
        self.reset_preferred_column();
        self.ensure_cursor_visible();
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
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn delete_to_start(&mut self) -> bool {
        if self.delete_selection() {
            self.ensure_cursor_visible();
            return true;
        }

        if self.cursor == 0 {
            self.ensure_cursor_visible();
            return false;
        }

        self.rope.remove(0..self.cursor);
        self.cursor = 0;
        self.anchor = 0;
        self.reset_preferred_column();
        self.ensure_cursor_visible();
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
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn move_left(&mut self, extend: bool) -> bool {
        if !extend && let Some((start, _)) = self.selection() {
            self.cursor = start;
            self.anchor = start;
            self.reset_preferred_column();
            self.ensure_cursor_visible();
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
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn ensure_cursor_visible(&mut self) {
        let viewport = self.viewport_cols.max(1);
        let col = self.display_width_between(0, self.cursor);
        let min_scroll = col.saturating_sub(viewport.saturating_sub(1));
        if self.scroll_x > min_scroll {
            self.scroll_x = min_scroll;
        }
        if col < self.scroll_x {
            self.scroll_x = col;
        } else if col >= self.scroll_x + viewport {
            self.scroll_x = col + 1 - viewport;
        }

        if self.is_multiline() {
            let viewport_rows = self.viewport_rows.max(1);
            let cursor_line = self.cursor_line();
            let max_scroll_y = self.line_count().saturating_sub(viewport_rows);

            if cursor_line < self.scroll_y {
                self.scroll_y = cursor_line;
            } else if cursor_line >= self.scroll_y + viewport_rows {
                self.scroll_y = cursor_line + 1 - viewport_rows;
            }

            if self.scroll_y > max_scroll_y {
                self.scroll_y = max_scroll_y;
            }
        } else {
            self.scroll_y = 0;
        }
    }

    fn move_right(&mut self, extend: bool) -> bool {
        if !extend && let Some((_, end)) = self.selection() {
            self.cursor = end;
            self.anchor = end;
            self.reset_preferred_column();
            self.ensure_cursor_visible();
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
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn move_up(&mut self, extend: bool) -> bool {
        self.move_vertical(-1, extend)
    }

    fn move_down(&mut self, extend: bool) -> bool {
        self.move_vertical(1, extend)
    }

    fn move_vertical(&mut self, line_delta: isize, extend: bool) -> bool {
        if !self.is_multiline() {
            if !extend {
                self.anchor = self.cursor;
            }
            return false;
        }

        let line_count = self.line_count();
        if line_count <= 1 {
            if !extend {
                self.anchor = self.cursor;
            }
            return false;
        }

        let current_line = self.cursor_line();
        let target_line = match line_delta {
            d if d < 0 => {
                if current_line == 0 {
                    if !extend {
                        self.anchor = self.cursor;
                    }
                    return false;
                }
                current_line - 1
            }
            d if d > 0 => {
                if current_line + 1 >= line_count {
                    if !extend {
                        self.anchor = self.cursor;
                    }
                    return false;
                }
                current_line + 1
            }
            _ => current_line,
        };

        let desired_col = match self.preferred_column {
            Some(col) => col,
            None => {
                let col = self.column_in_line(current_line, self.cursor);
                self.preferred_column = Some(col);
                col
            }
        };

        let target_index = self.index_at_line_column(target_line, desired_col);

        if target_index == self.cursor {
            if !extend {
                self.anchor = target_index;
            }
            self.ensure_cursor_visible();
            return false;
        }

        self.cursor = target_index;
        if !extend {
            self.anchor = target_index;
        }
        self.preferred_column = Some(desired_col);
        self.ensure_cursor_visible();
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
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn set_selection(&mut self, start: usize, end: usize) {
        let len = self.len_chars();
        let start = start.min(len);
        let end = end.min(len);
        self.anchor = start;
        self.cursor = end;
        self.reset_preferred_column();
        self.ensure_cursor_visible();
    }

    fn delete_selection(&mut self) -> bool {
        if let Some((start, end)) = self.selection() {
            self.rope.remove(start..end);
            self.cursor = start;
            self.anchor = start;
            self.reset_preferred_column();
            self.ensure_cursor_visible();
            true
        } else {
            false
        }
    }

    fn handle_pointer(&mut self, column: usize, row: usize, click_count: u8) -> bool {
        let index = self.column_to_index(column, row);

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

    fn column_to_index(&self, column: usize, row: usize) -> usize {
        if self.is_multiline() {
            let line_count = self.line_count();
            let max_index = line_count - 1;
            let absolute_row = self.scroll_y.saturating_add(row).min(max_index);
            self.index_at_line_column(absolute_row, column)
        } else {
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
        if ch == '\n' {
            return 0;
        }
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
    MoveLeft {
        extend: bool,
    },
    MoveRight {
        extend: bool,
    },
    MoveUp {
        extend: bool,
    },
    MoveDown {
        extend: bool,
    },
    MoveToStart {
        extend: bool,
    },
    MoveToEnd {
        extend: bool,
    },
    MoveWordLeft {
        extend: bool,
    },
    MoveWordRight {
        extend: bool,
    },
    Pointer {
        column: u16,
        row: u16,
        click_count: u8,
    },
    Replace(String),
    SelectRange {
        start: usize,
        end: usize,
    },
    ClearSelection,
    SetViewportSize {
        cols: usize,
        rows: usize,
    },
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
    let input_leaf = InputLeaf::new(spans, style);
    let mut node = leaf::<Msg>(input_leaf)
        .with_id(id)
        .with_scroll_x(state.scroll_x as f32)
        .with_scroll(state.scroll_y as f32);

    let handler = Rc::new(map_msg);
    let mouse_handler = handler.clone();

    node = node.on_mouse(move |event: MouseEvent| {
        if event.buttons.left && event.click_count > 0 {
            let msg = InputMsg::Pointer {
                column: event.local_x,
                row: event.local_y,
                click_count: event.click_count,
            };
            Some(mouse_handler(msg))
        } else {
            None
        }
    });

    // Track viewport width via resize and update state
    let resize_handler = handler.clone();
    node = node.on_resize(move |layout| {
        let cols = layout.size.width.max(0.0).round() as usize;
        let rows = layout.size.height.max(0.0).round() as usize;
        Some(resize_handler(InputMsg::SetViewportSize { cols, rows }))
    });

    node
}

#[derive(Clone, Debug)]
struct InputLeaf {
    lines: Vec<InputLine>,
    base_style: Style,
    max_width: usize,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct InputLine {
    segments: Vec<LineSegment>,
    width: usize,
}

#[derive(Clone, Debug, PartialEq)]
struct LineSegment {
    content: String,
    style: Style,
    width: usize,
}

impl InputLeaf {
    fn new(spans: Vec<TextSpan>, style: &InputStyle) -> Self {
        let cursor_style = style.cursor.clone();
        let cursor_symbol = style.cursor_symbol.to_string();
        let mut lines = vec![InputLine::default()];

        for span in spans {
            let mut buffer = String::new();
            for ch in span.content.chars() {
                if ch == '\n' {
                    if !buffer.is_empty() {
                        let content = std::mem::take(&mut buffer);
                        lines
                            .last_mut()
                            .expect("lines always has at least one entry")
                            .push_segment(content, span.style.clone());
                    }
                    if span.style == cursor_style {
                        lines
                            .last_mut()
                            .expect("lines always has at least one entry")
                            .push_segment(cursor_symbol.clone(), cursor_style.clone());
                    }
                    lines.push(InputLine::default());
                } else {
                    buffer.push(ch);
                }
            }

            if !buffer.is_empty() {
                lines
                    .last_mut()
                    .expect("lines always has at least one entry")
                    .push_segment(buffer, span.style.clone());
            }
        }

        if lines.is_empty() {
            lines.push(InputLine::default());
        }

        let max_width = lines
            .iter()
            .map(|line| line.width)
            .max()
            .unwrap_or(0)
            .max(1);

        Self {
            lines,
            base_style: style.text.clone(),
            max_width,
        }
    }
}

impl InputLine {
    fn push_segment(&mut self, content: String, style: Style) {
        if content.is_empty() {
            return;
        }
        let width = content.chars().map(InputState::char_width).sum();
        if width == 0 {
            return;
        }
        self.width += width;
        self.segments.push(LineSegment {
            content,
            style,
            width,
        });
    }
}

impl Renderable for InputLeaf {
    fn eq_leaf(&self, other: &dyn Renderable) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|o| o.lines == self.lines && o.base_style == self.base_style)
            .unwrap_or(false)
    }

    fn measure(
        &self,
        _style: &taffy::Style,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
    ) -> taffy::Size<f32> {
        let width = known_dimensions
            .width
            .unwrap_or(match available_space.width {
                AvailableSpace::Definite(w) => w,
                AvailableSpace::MinContent | AvailableSpace::MaxContent => self.max_width as f32,
            });

        let content_height = self.lines.len().max(1) as f32;
        let height = known_dimensions.height.unwrap_or(content_height);

        taffy::Size { width, height }
    }

    fn render(&self, ctx: &mut LeafRenderContext<'_>) {
        let area = ctx.area();
        if area.width == 0 || area.height == 0 {
            return;
        }

        let scroll_y = ctx.scroll_y().max(0.0).floor() as usize;
        let skip_cols = ctx.scroll_x().max(0.0).round() as usize;

        let base_attrs = ctx.style_to_attributes(&self.base_style);
        let blank_line = " ".repeat(area.width);

        for row in 0..area.height {
            let y = area.y + row;
            if !blank_line.is_empty() {
                ctx.write_text(area.x, y, &blank_line, &base_attrs);
            }

            let line_idx = scroll_y + row;
            let Some(line) = self.lines.get(line_idx) else {
                continue;
            };

            let mut remaining = area.width;
            let mut cursor_x = area.x;
            let mut line_skip = skip_cols;

            for segment in &line.segments {
                if remaining == 0 {
                    break;
                }
                if line_skip >= segment.width {
                    line_skip -= segment.width;
                    continue;
                }

                let mut collected = String::new();
                let mut taken = 0;

                for ch in segment.content.chars() {
                    let width = InputState::char_width(ch);
                    if width == 0 {
                        continue;
                    }
                    if line_skip >= width {
                        line_skip -= width;
                        continue;
                    } else if line_skip > 0 {
                        line_skip = 0;
                    }

                    if taken + width > remaining {
                        break;
                    }

                    collected.push(ch);
                    taken += width;
                }

                if collected.is_empty() {
                    continue;
                }

                let attrs = ctx.style_to_attributes(&segment.style);
                ctx.write_text(cursor_x, y, &collected, &attrs);

                cursor_x += taken;
                remaining = remaining.saturating_sub(taken);
            }
        }
    }

    fn debug_label(&self) -> &'static str {
        "input"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub fn default_keybindings<UpdateMsg>(
    state: &InputState,
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
        KeyCode::Up if !key.ctrl && !key.alt && state.is_multiline() => {
            Some(InputMsg::MoveUp { extend: key.shift })
        }
        KeyCode::Down if !key.ctrl && !key.alt && state.is_multiline() => {
            Some(InputMsg::MoveDown { extend: key.shift })
        }
        KeyCode::Enter if !key.ctrl && !key.alt && state.is_multiline() => {
            Some(InputMsg::InsertChar('\n'))
        }
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
            row: 0,
            click_count: 2,
        });
        assert_eq!(state.selection(), Some((0, 3)));
    }

    #[test]
    fn single_line_filters_newlines() {
        let mut state = InputState::default();
        state.update(InputMsg::InsertText("hello\nworld".into()));
        assert_eq!(state.value(), "hello world");
    }

    #[test]
    fn multiline_preserves_newlines() {
        let mut state = InputState::new_multiline();
        state.update(InputMsg::InsertText("hello\nworld".into()));
        assert_eq!(state.value(), "hello\nworld");
    }

    #[test]
    fn multiline_move_vertical_preserves_column() {
        let mut state = InputState::with_value_multiline("abcd\nef");
        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        assert_eq!(state.cursor(), 2);

        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.cursor(), 7);

        state.update(InputMsg::MoveUp { extend: false });
        assert_eq!(state.cursor(), 2);
    }

    #[test]
    fn pointer_uses_row_for_multiline_navigation() {
        let mut state = InputState::with_value_multiline("hello\nworld");
        state.update(InputMsg::Pointer {
            column: 0,
            row: 1,
            click_count: 1,
        });
        assert_eq!(state.cursor(), 6);
    }

    #[test]
    fn multiline_render_breaks_lines() {
        let mut state = InputState::with_value_multiline("foo\nbar");
        state.update(InputMsg::SetViewportSize { cols: 4, rows: 2 });
        assert_eq!(state.scroll_x(), 0);
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(4, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(4.0),
                height: AvailableSpace::Definite(2.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(4, 2))
            .expect("render should succeed");
        let rendered = renderer.buffer().to_string();
        let mut lines = rendered.lines();
        let first = lines.next().unwrap_or("");
        let second = lines.next().unwrap_or("");

        assert!(first.starts_with("foo"));
        assert!(second.starts_with("bar"));
    }

    #[test]
    fn cursor_before_newline_stays_on_previous_line() {
        let mut state = InputState::with_value_multiline("123\n4");
        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        assert_eq!(state.cursor(), 3);

        state.update(InputMsg::SetViewportSize { cols: 4, rows: 2 });

        let mut style = InputStyle::default();
        style.cursor_symbol = "|";

        let spans = state.spans(&style);
        let leaf = InputLeaf::new(spans.clone(), &style);
        assert_eq!(state.scroll_x(), 0);
        assert_eq!(leaf.lines.len(), 2);

        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(4, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(4.0),
                height: AvailableSpace::Definite(2.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(4, 2))
            .expect("render should succeed");
        let rendered = renderer.buffer().to_string();
        let mut lines = rendered.lines();

        let first = lines.next().unwrap_or("");
        let second = lines.next().unwrap_or("");

        assert!(first.starts_with("123|"), "first line: {:?}", first);
        assert!(second.starts_with("4"), "second line: {:?}", second);
    }

    #[test]
    fn multiline_vertical_scroll_keeps_cursor_visible() {
        let mut state = InputState::with_value_multiline("a\nb\nc");
        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::SetViewportSize { cols: 1, rows: 1 });
        assert_eq!(state.scroll_y(), 0);

        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.scroll_y(), 1);

        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.scroll_y(), 2);
    }

    #[test]
    fn default_keybindings_enter_inserts_newline_in_multiline() {
        let state = InputState::new_multiline();
        let key = Key::new(KeyCode::Enter);
        let msg = default_keybindings(&state, key, |m| m);
        assert!(matches!(msg, Some(InputMsg::InsertChar('\n'))));
    }

    #[test]
    fn default_keybindings_enter_ignored_in_single_line() {
        let state = InputState::default();
        let key = Key::new(KeyCode::Enter);
        assert!(default_keybindings(&state, key, |m| m).is_none());
    }

    #[test]
    fn default_keybindings_maps_up_down_in_multiline() {
        let state = InputState::new_multiline();
        let up_key = Key::new(KeyCode::Up);
        let down_key = Key::new(KeyCode::Down);
        assert!(matches!(
            default_keybindings(&state, up_key, |m| m),
            Some(InputMsg::MoveUp { .. })
        ));
        assert!(matches!(
            default_keybindings(&state, down_key, |m| m),
            Some(InputMsg::MoveDown { .. })
        ));
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

    #[test]
    fn input_keeps_cursor_visible_with_horizontal_scroll() {
        // Set long value and narrow viewport; ensure state scrolls to keep cursor visible
        let mut state = InputState::with_value("hello world this is long");
        // Simulate resize to 8 cols
        state.update(InputMsg::SetViewportSize { cols: 8, rows: 1 });
        // Move cursor to end (already there by set_value)
        // Ensure scroll advanced
        assert!(state.scroll_x() > 0);

        let style = InputStyle::default();
        // Build node and ensure it asks for x-scroll
        let mut node = crate::input::<()>("input", &state, &style, |_| ());
        // Layout and render to 8x1
        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(8, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(8.0),
                height: AvailableSpace::Definite(1.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(8, 1))
            .expect("render should succeed");
        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        // Expect to see the last characters including the cursor placeholder/char region at end
        assert!(first_line.ends_with(' '));
    }

    #[test]
    fn input_windowing_handles_fullwidth_chars() {
        // Include fullwidth char and verify it shows within window after horizontal scroll
        let mut state = InputState::with_value("ab漢cdef");
        state.update(InputMsg::SetViewportSize { cols: 6, rows: 1 });
        // Move cursor to end to trigger scroll
        state.update(InputMsg::MoveToEnd { extend: false });
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(6, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(6.0),
                height: AvailableSpace::Definite(1.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(6, 1))
            .expect("render should succeed");
        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        assert!(first_line.contains('漢'));
    }

    #[test]
    fn cursor_placeholder_cell_has_cursor_style() {
        // Only the placeholder cell at the cursor position should adopt the cursor style.
        let state = InputState::with_value("hi");
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::{Palette, Rgba};
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(6, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(6.0),
                height: AvailableSpace::Definite(1.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(6, 1))
            .expect("render should succeed");

        let back = renderer.buffer().back_buffer();

        // First two cells are the text with default styling
        assert_eq!(back[0][0].ch, 'h');
        assert_eq!(back[0][0].attrs.background(), None);
        assert!(!back[0][0].attrs.is_bold());
        assert_eq!(back[0][1].ch, 'i');
        assert_eq!(back[0][1].attrs.background(), None);
        assert!(!back[0][1].attrs.is_bold());

        // The cursor placeholder space uses cursor style
        let expected_bg = Rgba::opaque(229, 229, 229); // White
        let expected_fg = Rgba::opaque(0, 0, 0); // Black
        let cursor_cell = &back[0][2];
        assert_eq!(cursor_cell.ch, ' ');
        assert_eq!(cursor_cell.attrs.background(), Some(expected_bg));
        assert_eq!(cursor_cell.attrs.foreground(), Some(expected_fg));
        assert!(cursor_cell.attrs.is_bold());

        for cell in &back[0][3..] {
            assert_eq!(cell.attrs.background(), None);
            assert!(!cell.attrs.is_bold());
        }
    }

    #[test]
    fn commit_style_cursor_tints_trailing_whitespace_with_text_style() {
        // Regression: cursor background should not bleed into the rest of the row.
        // Trailing whitespace should adopt the text background, matching the commit modal styling.
        let state = InputState::with_value("hi");

        let mut style = InputStyle::default();
        style.text.bg = Some(crate::dom::Color::rgb(0x44, 0x55, 0x66));
        style.text.fg = Some(crate::dom::Color::rgb(0x22, 0x33, 0x44));
        style.cursor.bg = Some(crate::dom::Color::rgb(0x11, 0x22, 0x33));
        style.cursor.fg = Some(crate::dom::Color::rgb(0xee, 0xdd, 0xcc));

        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::event::Size;
        use crate::palette::{Palette, Rgba};
        use crate::render::Renderer;

        let mut buffer = DoubleBuffer::new(8, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        node.layout_state.layout.size.width = 8.0;
        node.layout_state.layout.size.height = 1.0;
        node.layout_state.layout.location.x = 0.0;
        node.layout_state.layout.location.y = 0.0;
        node.layout_state.unrounded_layout.size.width = 8.0;
        node.layout_state.unrounded_layout.size.height = 1.0;
        node.layout_state.unrounded_layout.location.x = 0.0;
        node.layout_state.unrounded_layout.location.y = 0.0;

        renderer
            .render(&node, Size::new(8, 1))
            .expect("render should succeed");

        let back = renderer.buffer().back_buffer();
        let row = &back[0];

        let expected_text_bg = Rgba::opaque(0x44, 0x55, 0x66);
        let expected_cursor_bg = Rgba::opaque(0x11, 0x22, 0x33);
        let expected_cursor_fg = Rgba::opaque(0xee, 0xdd, 0xcc);

        let text_len = state.len_chars();
        for cell in &row[..text_len] {
            assert_eq!(cell.attrs.background(), Some(expected_text_bg));
        }

        let cursor_cell = &row[text_len];
        assert_eq!(cursor_cell.ch, ' ');
        assert_eq!(cursor_cell.attrs.background(), Some(expected_cursor_bg));
        assert_eq!(cursor_cell.attrs.foreground(), Some(expected_cursor_fg));

        for cell in &row[(text_len + 1)..] {
            assert_eq!(cell.ch, ' ');
            assert_eq!(cell.attrs.background(), Some(expected_text_bg));
            assert_ne!(cell.attrs.background(), Some(expected_cursor_bg));
        }
    }
}
