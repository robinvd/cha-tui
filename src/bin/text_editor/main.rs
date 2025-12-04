mod highlight;
mod runtime;

use std::fs::{self, File};
use std::io;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::process::Command;

use chatui::components::input::{
    InlineHint, InlineHintId, InlineHintKind, InlineHintPlacement, TextChangeSet,
};
use chatui::components::scroll::{self};
use chatui::dom::Color;
use chatui::event::{Event, Key, KeyCode};
use chatui::{
    HighlightLayerId, InputMsg, InputState, InputStyle, Program, ScrollMsg, ScrollState, Style,
    TextSpan, Transition, block_with_title, column, default_input_keybindings, input, modal,
    rich_text, text,
};
use color_eyre::eyre::WrapErr;
use highlight::DocumentHighlighter;
use ropey::Rope;
use taffy::Dimension;
use time::OffsetDateTime;
use tree_house::tree_sitter::{InputEdit, Point};

const STATUS_PATH_MAX_CHARS: usize = 40;
const SYNTAX_LAYER_ID: HighlightLayerId = HighlightLayerId::new(1);
const SYNTAX_LAYER_PRIORITY: u8 = 10;
const GIT_BLAME_HINT_ID: InlineHintId = InlineHintId(1);
const GIT_BLAME_SUMMARY_LIMIT: usize = 60;

struct Model {
    path: PathBuf,
    input: InputState,
    input_scroll: ScrollState,
    style: InputStyle,
    dirty: bool,
    status: Option<Status>,
    confirm_quit: bool,
    highlighter: Option<DocumentHighlighter>,
    last_blame_target: Option<(usize, u64)>,
}

#[derive(Clone, Debug)]
struct Status {
    message: String,
    kind: StatusKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StatusKind {
    Info,
    Success,
    Error,
}

impl Status {
    fn new(message: impl Into<String>, kind: StatusKind) -> Self {
        Self {
            message: message.into(),
            kind,
        }
    }

    fn info(message: impl Into<String>) -> Self {
        Self::new(message, StatusKind::Info)
    }

    fn success(message: impl Into<String>) -> Self {
        Self::new(message, StatusKind::Success)
    }

    fn error(message: impl Into<String>) -> Self {
        Self::new(message, StatusKind::Error)
    }

    fn style(&self) -> Style {
        match self.kind {
            StatusKind::Info => Style::fg(Color::BrightBlue),
            StatusKind::Success => Style::fg(Color::Green),
            StatusKind::Error => Style::fg(Color::Red),
        }
    }
}

impl Model {
    fn load(path: PathBuf) -> color_eyre::Result<Self> {
        if let Some(parent) = path.parent().filter(|p| !p.as_os_str().is_empty()) {
            fs::create_dir_all(parent)
                .wrap_err_with(|| format!("create parent directories for {}", path.display()))?;
        }

        let mut status = None;
        let contents = match fs::read_to_string(&path) {
            Ok(contents) => contents,
            Err(err) if err.kind() == ErrorKind::NotFound => {
                File::create(&path).wrap_err("create new file")?;
                status = Some(Status::info("Created new file"));
                String::new()
            }
            Err(err) => return Err(err.into()),
        };

        let mut model = Self {
            path,
            input: InputState::with_value_multiline(contents),
            style: InputStyle::default(),
            dirty: false,
            status,
            confirm_quit: false,
            highlighter: None,
            input_scroll: ScrollState::both(),
            last_blame_target: None,
        };
        model.initialize_highlighter();
        model.input.set_line_number_gutter(true);
        model.apply_input(InputMsg::MoveToStart { extend: false });
        Ok(model)
    }

    fn initialize_highlighter(&mut self) {
        match DocumentHighlighter::new(&self.path, self.input.rope()) {
            Ok(Some(highlighter)) => match highlighter.highlight_all(self.input.rope()) {
                Ok(spans) => {
                    self.input
                        .set_highlight_layer(SYNTAX_LAYER_ID, SYNTAX_LAYER_PRIORITY, spans);
                    self.highlighter = Some(highlighter);
                }
                Err(err) => {
                    if self.status.is_none() {
                        self.status =
                            Some(Status::error(format!("Syntax highlighting failed: {err}")));
                    }
                    self.input.clear_highlight_layer(SYNTAX_LAYER_ID);
                    self.highlighter = None;
                }
            },
            Ok(None) => {
                self.input.clear_highlight_layer(SYNTAX_LAYER_ID);
                self.highlighter = None;
            }
            Err(err) => {
                if self.status.is_none() {
                    self.status = Some(Status::error(format!(
                        "Syntax highlighting unavailable: {err}"
                    )));
                }
                self.input.clear_highlight_layer(SYNTAX_LAYER_ID);
                self.highlighter = None;
            }
        }
    }

    fn ensure_cursor_visible(&mut self) {
        use chatui::components::scroll::ScrollTarget;

        // Use cursor row/column to create a scroll target
        let row = self.input.cursor_row();
        let col = self.input.cursor_column();

        // Create a unique target ID based on cursor position
        // The scroll system will ensure this target is visible
        let target = ScrollTarget::with_mixin("cursor", ((row as u64) << 32) | (col as u64));
        self.input_scroll.ensure_visible("main-editor", target);
    }

    fn refresh_git_blame_hint(&mut self) {
        let len_chars = self.input.rope().len_chars();
        if len_chars == 0 {
            self.input.remove_inline_hint(GIT_BLAME_HINT_ID);
            self.last_blame_target = None;
            return;
        }

        let cursor = self.input.cursor().min(len_chars);
        let line_idx = self.input.rope().char_to_line(cursor);
        let revision = self.input.revision();

        if self
            .last_blame_target
            .is_some_and(|(line, rev)| line == line_idx && rev == revision)
        {
            return;
        }

        self.input.remove_inline_hint(GIT_BLAME_HINT_ID);

        if let Some(hint) = self.git_blame_hint_for_line(line_idx) {
            self.input.set_inline_hint(hint);
        }

        self.last_blame_target = Some((line_idx, revision));
    }

    fn git_blame_hint_for_line(&self, line_idx: usize) -> Option<InlineHint> {
        let info = git_blame_for_line(&self.path, line_idx + 1)?;
        let rope = self.input.rope();
        let len_chars = rope.len_chars();
        if len_chars == 0 || line_idx >= rope.len_lines() {
            return None;
        }

        let line_start = rope.line_to_char(line_idx).min(len_chars);
        let mut line_end = rope
            .line_to_char((line_idx + 1).min(rope.len_lines()))
            .min(len_chars);
        if line_end > line_start && line_end > 0 && rope.char(line_end.saturating_sub(1)) == '\n' {
            line_end -= 1;
        }

        let style = Style {
            fg: Some(Color::BrightBlack),
            dim: true,
            ..Style::default()
        };

        let text = Rope::from_str(&format!("  // {}", info.format()));

        Some(InlineHint {
            id: GIT_BLAME_HINT_ID,
            range: line_end..line_end,
            text,
            style,
            kind: InlineHintKind::GitBlame,
            placement: InlineHintPlacement::Inline,
        })
    }

    fn apply_input(&mut self, msg: InputMsg) {
        let generation = self.input.revision();
        let rope_before = self.input.rope().clone();
        let _ = self.input.update(msg);
        self.ensure_cursor_visible();
        self.refresh_git_blame_hint();
        if generation != self.input.revision() {
            self.dirty = true;
            let changes = self.input.take_text_changes();
            self.update_highlighting(Some(rope_before), changes);
        }
    }

    fn save(&mut self) -> io::Result<()> {
        let contents = self.input.value();
        if let Some(parent) = self.path.parent().filter(|p| !p.as_os_str().is_empty()) {
            fs::create_dir_all(parent)?;
        }
        fs::write(&self.path, contents)?;
        self.dirty = false;
        self.status = Some(Status::success("Saved"));
        self.confirm_quit = false;
        Ok(())
    }

    fn update_highlighting(&mut self, rope_before: Option<Rope>, changes: Option<TextChangeSet>) {
        let Some(highlighter) = self.highlighter.as_mut() else {
            return;
        };

        let rope_after = self.input.rope().clone();
        let highlight_result = match (rope_before.as_ref(), changes) {
            (Some(before), Some(changes)) if !changes.is_empty() => {
                let edits = text_changes_to_input_edits(before, &changes);
                highlighter.apply_edits(&rope_after, &edits)
            }
            _ => highlighter.rebuild(&rope_after),
        };

        match highlight_result {
            Ok(spans) => {
                self.input
                    .set_highlight_layer(SYNTAX_LAYER_ID, SYNTAX_LAYER_PRIORITY, spans);
            }
            Err(err) => {
                self.status = Some(Status::error(format!("Syntax highlighting failed: {err}")));
                self.input.clear_highlight_layer(SYNTAX_LAYER_ID);
            }
        }
    }

    fn request_quit(&mut self) -> Transition<Msg> {
        if self.dirty {
            self.confirm_quit = true;
            self.status = Some(Status::info(
                "Unsaved changes — confirm quit or press Ctrl-S to save.",
            ));
            Transition::Continue
        } else {
            Transition::Quit
        }
    }

    fn cancel_quit(&mut self) {
        self.confirm_quit = false;
    }
}

#[derive(Debug)]
struct GitBlameInfo {
    short_hash: String,
    author: String,
    summary: String,
    date: Option<String>,
}

impl GitBlameInfo {
    fn format(&self) -> String {
        let summary = Self::truncate_summary(&self.summary, GIT_BLAME_SUMMARY_LIMIT);
        match &self.date {
            Some(date) => format!(
                "{} {} ({}) - {}",
                self.short_hash, self.author, date, summary
            ),
            None => format!("{} {} - {}", self.short_hash, self.author, summary),
        }
    }

    fn truncate_summary(summary: &str, max_chars: usize) -> String {
        if summary.chars().count() <= max_chars {
            return summary.to_string();
        }

        let safe_limit = max_chars.saturating_sub(3).max(1);
        let truncated: String = summary.chars().take(safe_limit).collect();
        format!("{truncated}...")
    }
}

fn git_blame_for_line(path: &Path, line_number: usize) -> Option<GitBlameInfo> {
    let file_name = path.file_name()?;
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let output = Command::new("git")
        .current_dir(parent)
        .arg("--no-pager")
        .arg("blame")
        .arg("--line-porcelain")
        .arg("-L")
        .arg(format!("{line_number},{line_number}"))
        .arg(file_name)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let stdout = String::from_utf8(output.stdout).ok()?;
    parse_git_blame_porcelain(&stdout)
}

fn parse_git_blame_porcelain(output: &str) -> Option<GitBlameInfo> {
    let mut lines = output.lines();
    let header = lines.next()?;
    let mut header_parts = header.split_whitespace();
    let commit = header_parts.next()?.to_string();

    let mut author = None;
    let mut summary = None;
    let mut author_time = None;

    for line in lines {
        if let Some(value) = line.strip_prefix("author ") {
            author = Some(value.trim().to_string());
        } else if let Some(value) = line.strip_prefix("author-time ") {
            if let Ok(secs) = value.trim().parse::<i64>() {
                author_time = Some(secs);
            }
        } else if let Some(value) = line.strip_prefix("summary ") {
            summary = Some(value.trim().to_string());
        } else if line.starts_with('\t') {
            break;
        }
    }

    Some(GitBlameInfo {
        short_hash: short_hash_from_commit(&commit),
        author: author.unwrap_or_else(|| "Unknown".to_string()),
        summary: sanitize_summary(summary.unwrap_or_else(|| "Not Committed Yet".to_string())),
        date: author_time.and_then(format_git_date),
    })
}

fn short_hash_from_commit(commit: &str) -> String {
    if commit.chars().all(|ch| ch == '0') {
        "WORKTREE".to_string()
    } else {
        commit.chars().take(8).collect()
    }
}

fn sanitize_summary(summary: String) -> String {
    summary
        .replace(['\n', '\t'], " ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn format_git_date(secs: i64) -> Option<String> {
    let dt = OffsetDateTime::from_unix_timestamp(secs).ok()?;
    let date = dt.date();
    Some(format!(
        "{:04}-{:02}-{:02}",
        date.year(),
        u8::from(date.month()),
        date.day()
    ))
}

fn text_changes_to_input_edits(rope: &Rope, changes: &TextChangeSet) -> Vec<InputEdit> {
    changes
        .changes
        .iter()
        .map(|change| {
            let start_char = change.range.start;
            let end_char = change.range.end;

            let start_byte = rope.char_to_byte(start_char);
            let old_end_byte = rope.char_to_byte(end_char);
            let inserted_bytes = change.inserted.len();

            let start_point = point_for_char(rope, start_char);
            let old_end_point = point_for_char(rope, end_char);
            let new_end_point = advance_point(start_point, &change.inserted);
            let new_end_byte = start_byte + inserted_bytes;

            InputEdit {
                start_byte: usize_to_u32(start_byte),
                old_end_byte: usize_to_u32(old_end_byte),
                new_end_byte: usize_to_u32(new_end_byte),
                start_point,
                old_end_point,
                new_end_point,
            }
        })
        .collect()
}

fn point_for_char(rope: &Rope, char_idx: usize) -> Point {
    let line = rope.char_to_line(char_idx);
    let line_start_char = rope.line_to_char(line);
    let line_start_byte = rope.char_to_byte(line_start_char);
    let byte = rope.char_to_byte(char_idx);

    Point {
        row: usize_to_u32(line),
        col: usize_to_u32(byte.saturating_sub(line_start_byte)),
    }
}

fn advance_point(mut point: Point, text: &str) -> Point {
    for ch in text.chars() {
        if ch == '\n' {
            point.row += 1;
            point.col = 0;
        } else {
            point.col += ch.len_utf8() as u32;
        }
    }
    point
}

fn usize_to_u32(value: usize) -> u32 {
    value.try_into().unwrap_or(u32::MAX)
}

enum Msg {
    Input(InputMsg),
    InputScroll(ScrollMsg),
    KeyPressed(Key),
}

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    match msg {
        Msg::Input(input_msg) => {
            model.apply_input(input_msg);
            Transition::Continue
        }
        Msg::KeyPressed(key) => handle_key(model, key),
        Msg::InputScroll(scroll_msg) => {
            model.input_scroll.update(scroll_msg);
            Transition::Continue
        }
    }
}

fn handle_key(model: &mut Model, key: Key) -> Transition<Msg> {
    if key.ctrl && key_matches_char(&key, 's') {
        if let Err(err) = model.save() {
            model.status = Some(Status::error(format!("Failed to save: {}", err)));
        }
        return Transition::Continue;
    }

    if key.super_key && key.alt {
        let added = match key.code {
            KeyCode::Up => model.input.add_caret_above(),
            KeyCode::Down => model.input.add_caret_below(),
            _ => false,
        };
        if added {
            model.ensure_cursor_visible();
            model.refresh_git_blame_hint();
        }
        if matches!(key.code, KeyCode::Up | KeyCode::Down) {
            return Transition::Continue;
        }
    }

    // Fallback multi-caret addition using a widely supported keybinding: Ctrl-n adds a caret below.
    if key.ctrl && key_matches_char(&key, 'n') {
        if model.input.add_caret_below() {
            model.ensure_cursor_visible();
            model.refresh_git_blame_hint();
        }
        return Transition::Continue;
    }

    if model.confirm_quit {
        return handle_quit_confirmation(model, key);
    }

    if key.ctrl && key_matches_char(&key, 'q') || matches!(key.code, KeyCode::Esc) {
        return model.request_quit();
    }

    match key.code {
        KeyCode::PageUp => {
            model.input_scroll.update(ScrollMsg::AxisDeltaPercent {
                axis: scroll::ScrollAxis::Vertical,
                ratio: -0.5,
            });
        }
        KeyCode::PageDown => {
            model.input_scroll.update(ScrollMsg::AxisDeltaPercent {
                axis: scroll::ScrollAxis::Vertical,
                ratio: 0.5,
            });
        }
        _ => {}
    }
    if key.ctrl && key_matches_char(&key, 'q') || matches!(key.code, KeyCode::Esc) {
        return model.request_quit();
    }

    if !key.ctrl && !key.alt && matches!(key.code, KeyCode::Tab) {
        model.apply_input(InputMsg::InsertChar('\t'));
        return Transition::Continue;
    }

    if let Some(input_msg) = default_input_keybindings(&model.input, key, |msg| msg) {
        model.apply_input(input_msg);
    }

    Transition::Continue
}

fn handle_quit_confirmation(model: &mut Model, key: Key) -> Transition<Msg> {
    match key.code {
        KeyCode::Enter => Transition::Quit,
        KeyCode::Char(ch) if matches_ignore_case(ch, 'y') => Transition::Quit,
        KeyCode::Esc => {
            model.cancel_quit();
            Transition::Continue
        }
        KeyCode::Char(ch) if matches_ignore_case(ch, 'n') => {
            model.cancel_quit();
            Transition::Continue
        }
        _ => Transition::Continue,
    }
}

fn matches_ignore_case(ch: char, target: char) -> bool {
    ch.eq_ignore_ascii_case(&target)
}

fn key_matches_char(key: &Key, target: char) -> bool {
    matches!(key.code, KeyCode::Char(ch) if matches_ignore_case(ch, target))
}

fn view(model: &Model) -> chatui::Node<Msg> {
    let raw_editor = input::<Msg>("text-editor-input", &model.input, &model.style, Msg::Input)
        .with_width(Dimension::percent(1.0))
        .with_height(Dimension::percent(1.0))
        .with_flex_grow(1.0);

    let editor = scroll::scrollable_content(
        "main-editor",
        &model.input_scroll,
        1,
        Msg::InputScroll,
        raw_editor,
    );

    let status_bar = render_status_bar(model);

    let root = column(vec![editor, status_bar]).with_fill();

    let mut items = vec![root];
    if model.confirm_quit {
        items.push(render_confirm_quit_modal())
    }

    column(items).with_fill()
}

fn render_status_bar(model: &Model) -> chatui::Node<Msg> {
    let dirty_symbol = if model.dirty { '+' } else { ' ' };

    let shortcuts = "Ctrl-S save  Ctrl-Q/Esc quit";

    let mut spans = vec![TextSpan::new(
        format!("[{}] {}", dirty_symbol, status_path_segment(&model.path)),
        status_bar_style().merged(&Style::bold()),
    )];

    if let Some(status) = &model.status {
        spans.extend([
            TextSpan::new("  |  ", status_bar_style()),
            TextSpan::new(
                status.message.to_owned(),
                status_bar_style().merged(&status.style()),
            ),
        ]);
    }

    spans.extend([
        TextSpan::new("  |  ", status_bar_style()),
        TextSpan::new(shortcuts, status_bar_style()),
    ]);

    rich_text(spans)
        .with_width(Dimension::percent(1.0))
        .with_style(status_bar_style())
}

fn status_bar_style() -> Style {
    let mut style = Style::bg(Color::rgba(30, 30, 45, 255));
    style.fg = Some(Color::White);
    style
}

fn status_path_segment(path: &Path) -> String {
    ellipsize_left(&path.display().to_string(), STATUS_PATH_MAX_CHARS)
}

fn ellipsize_left(text: &str, max_chars: usize) -> String {
    if max_chars == 0 {
        return String::new();
    }

    let chars: Vec<char> = text.chars().collect();
    if chars.len() <= max_chars {
        text.to_string()
    } else {
        let start = chars.len() - max_chars;
        let tail: String = chars[start..].iter().collect();
        format!("…{tail}")
    }
}

fn render_confirm_quit_modal() -> chatui::Node<Msg> {
    let title = text::<Msg>("Quit without saving?").with_style(Style::bold());
    let body = text::<Msg>("y/Enter: quit, n/Esc: keep editing").with_style(Style::dim());
    let reminder = text::<Msg>("Ctrl-S saves before quitting.").with_style(Style::dim());

    let content = column(vec![title, body, reminder])
        .with_min_width(Dimension::length(36.0))
        .with_min_height(Dimension::length(5.0));

    modal(vec![block_with_title("Confirm Quit", vec![content])])
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let path = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .ok_or_else(|| color_eyre::eyre::eyre!("usage: text_editor <path>"))?;

    let model = Model::load(path)?;
    let program = Program::new(model, update, view).map_event(|event| match event {
        Event::Key(key) => Some(Msg::KeyPressed(key)),
        _ => None,
    });

    program
        .run()
        .map_err(|err| color_eyre::eyre::eyre!(format!("{err:?}")))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::event::Size;
    use chatui::test_utils::render_node_to_string;
    use tempfile::tempdir;

    #[test]
    fn creates_missing_file_and_sets_status() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("new_file.txt");

        let model = Model::load(path.clone()).expect("load model");
        assert!(path.exists(), "file should be created");
        assert!(
            matches!(
                model.status,
                Some(Status {
                    kind: StatusKind::Info,
                    ..
                })
            ),
            "status should note new file"
        );
    }

    #[test]
    fn save_writes_file_and_clears_dirty() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("file.txt");
        fs::write(&path, "hello").unwrap();

        let mut model = Model::load(path.clone()).expect("load model");
        model.apply_input(InputMsg::MoveToEnd { extend: false });
        model.apply_input(InputMsg::InsertChar('!'));
        assert!(model.dirty, "dirty after edit");

        if let Err(err) = model.save() {
            panic!("save failed: {err}");
        }
        assert!(!model.dirty, "dirty flag cleared after save");
        let contents = fs::read_to_string(path).unwrap();
        assert_eq!(contents, "hello!");
    }

    #[test]
    fn view_renders_dirty_indicator() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("file.txt");
        fs::write(&path, "hi").unwrap();

        let mut model = Model::load(path).expect("load model");
        model.dirty = true;
        let mut node = view(&model);
        let rendered = render_node_to_string(&mut node, 40, 10).expect("render view");
        assert!(
            rendered.contains("[+]"),
            "status bar should show plus indicator for dirty buffers: {rendered}"
        );
    }

    #[test]
    fn view_renders_status_message() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("file.txt");
        fs::write(&path, "hi").unwrap();

        let mut model = Model::load(path).expect("load model");
        model.status = Some(Status::success("Saved OK"));
        let mut node = view(&model);
        let rendered = render_node_to_string(&mut node, 80, 12).expect("render view");
        assert!(
            rendered.contains("Saved OK"),
            "status message should appear in view: {rendered}"
        );
    }

    #[test]
    fn confirm_modal_is_rendered_when_pending() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("file.txt");
        fs::write(&path, "hi").unwrap();

        let mut model = Model::load(path).expect("load model");
        model.confirm_quit = true;
        let mut node = view(&model);
        let rendered = render_node_to_string(&mut node, 60, 15).expect("render view");
        assert!(
            rendered.contains("Quit without saving?"),
            "confirm modal should render when requested: {rendered}"
        );
    }

    #[test]
    fn cursor_starts_at_beginning_of_file() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("file.txt");
        fs::write(&path, "line 1\nline 2\nline 3").unwrap();

        let model = Model::load(path).expect("load model");
        assert_eq!(
            model.input.cursor(),
            0,
            "cursor should start at the beginning of the file"
        );
    }

    #[test]
    #[ignore = "Scrolling is now handled externally, this test needs refactoring"]
    fn cursor_movement_updates_scroll_state() {
        // TODO: Rewrite this test to verify cursor visibility is requested via ensure_visible
        let temp = tempdir().unwrap();
        let path = temp.path().join("file.txt");
        let content: String = (0..20).map(|i| format!("line {i}\n")).collect();
        fs::write(&path, content).unwrap();

        let mut model = Model::load(path).expect("load model");
        model.input_scroll.update(ScrollMsg::Resize {
            viewport: Size {
                width: 10,
                height: 3,
            },
            content: Size {
                width: 10,
                height: 40,
            },
        });
        // model.apply_input(InputMsg::SetViewportSize { cols: 10, rows: 3 });

        for _ in 0..10 {
            model.apply_input(InputMsg::MoveDown { extend: false });
        }

        // assert!(
        //     model.input.scroll_y() > 0,
        //     "input viewport should scroll when cursor moves past the visible area"
        // );
        assert!(
            model.input_scroll.offset_y() > 0.0,
            "scroll state should track the cursor position after movement"
        );
    }
}
