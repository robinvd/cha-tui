use std::ffi::OsStr;
use std::process::Command;

use chatui::dom::{Color, Node};
use chatui::event::{Event, Key, KeyCode};
use chatui::{Program, Style, Transition, block_with_title, column, row, text};
use color_eyre::eyre::{Context, Result, eyre};
use taffy::Dimension;
use taffy::prelude::{FromLength, TaffyZero};
use tracing_subscriber::EnvFilter;

fn main() -> Result<()> {
    color_eyre::install()?;
    init_tracing()?;

    let program = Program::new(Model::new(), update, view).map_event(map_event);

    if let Err(error) = program.run() {
        eprintln!("Program exited with error: {:?}", error);
    }

    Ok(())
}

fn map_event(event: Event) -> Option<Msg> {
    if let Event::Key(key) = event {
        Some(Msg::KeyPressed(key))
    } else {
        None
    }
}

fn update(model: &mut Model, msg: Msg) -> Transition {
    match msg {
        Msg::KeyPressed(key) => handle_key(model, key),
        Msg::ActivateFile(staged, index) => {
            if staged {
                model.selected_staged = index;
                model.focus = Focus::Staged
            } else {
                model.selected_unstaged = index;
                model.focus = Focus::Unstaged
            };
            model.update_diff();
            Transition::Continue
        }
        Msg::ScrollPreview(diff) => {
            model.scroll_diff(diff);
            Transition::Continue
        }
        Msg::ScrollFiles(staged, diff) => {
            if staged {
                model.scroll_files(Focus::Staged, diff);
            } else {
                model.scroll_files(Focus::Unstaged, diff);
            }
            Transition::Continue
        }
    }
}

fn handle_key(model: &mut Model, key: Key) -> Transition {
    if key.ctrl && matches!(key.code, KeyCode::Char('c') | KeyCode::Char('C')) {
        return Transition::Quit;
    }

    match key.code {
        KeyCode::Esc => Transition::Quit,
        KeyCode::Char('q') | KeyCode::Char('Q') => Transition::Quit,
        KeyCode::Up => {
            model.move_selection_up();
            model.update_diff();
            Transition::Continue
        }
        KeyCode::Down => {
            model.move_selection_down();
            model.update_diff();
            Transition::Continue
        }
        KeyCode::Left => {
            model.focus_unstaged();
            model.update_diff();
            Transition::Continue
        }
        KeyCode::Right => {
            model.focus_staged();
            model.update_diff();
            Transition::Continue
        }
        KeyCode::Enter => {
            model.toggle_stage_selected();
            Transition::Continue
        }
        KeyCode::Char('r') | KeyCode::Char('R') => {
            if let Err(err) = model.refresh_status() {
                model.set_error(err);
            }
            Transition::Continue
        }
        KeyCode::Char('J') => {
            model.scroll_diff(1);
            Transition::Continue
        }
        KeyCode::Char('K') => {
            model.scroll_diff(-1);
            Transition::Continue
        }
        KeyCode::Char('j') => {
            model.scroll_files(model.focus, 1);
            Transition::Continue
        }
        KeyCode::Char('k') => {
            model.scroll_files(model.focus, -1);
            Transition::Continue
        }
        _ => Transition::Continue,
    }
}

fn view(model: &Model) -> Node<Msg> {
    let layout = row(vec![
        render_left_pane(model)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO)
            .with_min_width(Dimension::from_length(20.))
            .with_min_height(Dimension::ZERO),
        render_diff_pane(model)
            .with_flex_grow(3.)
            .with_flex_basis(Dimension::ZERO)
            .with_min_height(Dimension::ZERO)
            .with_min_width(Dimension::ZERO)
            .with_overflow_y(taffy::Overflow::Scroll)
            .with_overflow_x(taffy::Overflow::Clip),
    ])
    .with_width(Dimension::percent(1.))
    .with_flex_basis(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_shrink(1.)
    .with_min_height(Dimension::ZERO)
    .with_id("main-row");

    column(vec![
        text::<Msg>("Git Status").with_style(title_style()),
        text::<Msg>(model.error.as_deref().unwrap_or_default()).with_style(error_style()),
        layout,
    ])
    .with_fill()
    .with_id("root")
}

fn render_left_pane(model: &Model) -> Node<Msg> {
    let unstaged = render_section(
        "Unstaged Changes",
        &model.unstaged,
        model.focus == Focus::Unstaged,
        model.selected_unstaged,
        false,
    )
    .with_id("unstaged-section")
    .with_overflow_y(taffy::Overflow::Scroll)
    .with_scroll(model.unstaged_scroll)
    .on_mouse(|e| {
        if e.buttons.vert_wheel {
            Some(Msg::ScrollFiles(
                false,
                if e.buttons.wheel_positive { 1 } else { -1 },
            ))
        } else {
            None
        }
    });

    let staged = render_section(
        "Staged Changes",
        &model.staged,
        model.focus == Focus::Staged,
        model.selected_staged,
        true,
    )
    .with_id("staged-section")
    .with_overflow_y(taffy::Overflow::Scroll)
    .with_scroll(model.staged_scroll)
    .on_mouse(|e| {
        if e.buttons.vert_wheel {
            Some(Msg::ScrollFiles(
                true,
                if e.buttons.wheel_positive { 1 } else { -1 },
            ))
        } else {
            None
        }
    });

    column(vec![
        unstaged.with_flex_grow(1.).with_flex_basis(Dimension::ZERO),
        staged.with_flex_grow(1.).with_flex_basis(Dimension::ZERO),
    ])
    .with_id("left-pane")
}

fn render_section(
    title: &str,
    entries: &[FileEntry],
    is_active: bool,
    selected: usize,
    is_staged: bool,
) -> Node<Msg> {
    block_with_title(
        title,
        vec![
            render_file_list(entries, is_active, selected, is_staged)
                .with_min_height(Dimension::ZERO)
                .with_flex_grow(1.)
                .with_flex_basis(Dimension::ZERO),
        ],
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO)
}

fn render_file_list(
    entries: &[FileEntry],
    is_active: bool,
    selected: usize,
    is_staged: bool,
) -> Node<Msg> {
    let mut items = Vec::new();

    if entries.is_empty() {
        items.push(text::<Msg>("(no files)").with_style(inactive_style()));
    } else {
        for (idx, entry) in entries.iter().enumerate() {
            let mut node =
                text::<Msg>(&entry.display).on_click(move || Msg::ActivateFile(is_staged, idx));

            if idx == selected {
                let mut style = if is_active {
                    active_selection_style()
                } else {
                    inactive_selection_style()
                };
                // Keep the status letter readable.
                if entry.code == '?' {
                    style.bold = true;
                }
                node = node.with_style(style);
            }

            items.push(node);
        }
    }

    column(items)
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO)
}

fn render_diff_pane(model: &Model) -> Node<Msg> {
    const DIFF_TITLE: &str = "Diff Preview";
    let content = render_diff_lines(&model.diff_lines)
        .with_scroll(model.diff_scroll)
        .on_mouse(|e| {
            if e.buttons.vert_wheel {
                Some(Msg::ScrollPreview(if e.buttons.wheel_positive {
                    1
                } else {
                    -1
                }))
            } else {
                None
            }
        });

    block_with_title(
        DIFF_TITLE,
        vec![
            content
                .with_min_height(Dimension::ZERO)
                .with_flex_grow(1.)
                .with_flex_basis(Dimension::ZERO),
        ],
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO)
    .with_id("diff-pane")
}

fn render_diff_lines(lines: &[DiffLine]) -> Node<Msg> {
    if lines.is_empty() {
        return column(vec![
            text::<Msg>("No diff available").with_style(inactive_style()),
        ])
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO);
    }

    let mut rendered = Vec::with_capacity(lines.len());
    for line in lines {
        let mut node = text::<Msg>(&line.content).with_overflow_x(taffy::Overflow::Clip);
        let mut style = Style::default();

        if let Some(color) = line.color {
            style.fg = Some(color);
        }

        if line.bold {
            style.bold = true;
        }

        if style.fg.is_some() || style.bold {
            node = node.with_style(style);
        }

        rendered.push(node);
    }

    column(rendered)
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO)
}

fn title_style() -> Style {
    let mut style = Style::bold();
    style.fg = Some(Color::Cyan);
    style
}

fn error_style() -> Style {
    Style::fg(Color::Red)
}

fn inactive_style() -> Style {
    Style::fg(Color::Blue)
}

fn active_selection_style() -> Style {
    let mut style = Style::bold();
    style.fg = Some(Color::Green);
    style
}

fn inactive_selection_style() -> Style {
    Style {
        fg: Some(Color::Magenta),
        ..Style::default()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
enum Focus {
    #[default]
    Unstaged,
    Staged,
}

#[derive(Clone, Debug)]
struct FileEntry {
    path: String,
    display: String,
    code: char,
}

impl FileEntry {
    fn new(path: String, code: char, display: String) -> Self {
        Self {
            path,
            display,
            code,
        }
    }
}

#[derive(Default)]
struct Model {
    unstaged: Vec<FileEntry>,
    staged: Vec<FileEntry>,
    focus: Focus,
    selected_unstaged: usize,
    selected_staged: usize,
    diff_lines: Vec<DiffLine>,
    error: Option<String>,
    diff_scroll: f32,
    unstaged_scroll: f32,
    staged_scroll: f32,
}

impl Model {
    fn new() -> Self {
        let mut model = Self::default();

        if let Err(err) = model.refresh_status() {
            model.set_error(err);
            model.diff_lines = vec![DiffLine::plain("Unable to load git status")];
        }

        model
    }

    fn set_error(&mut self, err: color_eyre::eyre::Report) {
        self.error = Some(err.to_string());
    }

    fn clear_error(&mut self) {
        self.error = None;
    }

    fn refresh_status(&mut self) -> Result<()> {
        let status = load_git_status()?;
        self.unstaged = status.unstaged;
        self.staged = status.staged;
        self.ensure_focus_valid();
        self.clear_error();
        self.update_diff();
        Ok(())
    }

    fn ensure_focus_valid(&mut self) {
        clamp_index(&mut self.selected_unstaged, self.unstaged.len());
        clamp_index(&mut self.selected_staged, self.staged.len());

        if self.focus == Focus::Unstaged && self.unstaged.is_empty() && !self.staged.is_empty() {
            self.focus = Focus::Staged;
        } else if self.focus == Focus::Staged && self.staged.is_empty() && !self.unstaged.is_empty()
        {
            self.focus = Focus::Unstaged;
        }
        self.clamp_file_scrolls();
    }

    fn update_diff(&mut self) {
        let diff_result = match self.current_entry() {
            Some(entry) => diff_for(entry, self.focus),
            None => {
                self.diff_lines = vec![DiffLine::plain("No file selected")];
                self.diff_scroll = 0.0;
                return;
            }
        };

        match diff_result {
            Ok(lines) => {
                if lines.is_empty() {
                    self.diff_lines = vec![DiffLine::plain("No diff available")];
                } else {
                    self.diff_lines = lines;
                }
                self.diff_scroll = 0.0;

                if matches!(self.error.as_deref(), Some(msg) if msg.starts_with("git diff")) {
                    self.clear_error();
                }
            }
            Err(err) => {
                self.diff_lines = vec![DiffLine::plain("Failed to load diff")];
                self.set_error(err);
                self.diff_scroll = 0.0;
            }
        }
    }

    fn current_entry(&self) -> Option<&FileEntry> {
        match self.focus {
            Focus::Unstaged => self.unstaged.get(self.selected_unstaged),
            Focus::Staged => self.staged.get(self.selected_staged),
        }
    }

    fn move_selection_up(&mut self) {
        match self.focus {
            Focus::Unstaged => {
                if self.selected_unstaged > 0 {
                    self.selected_unstaged -= 1;
                    if (self.selected_unstaged as f32) < self.unstaged_scroll {
                        self.unstaged_scroll = self.selected_unstaged as f32;
                    }
                }
            }
            Focus::Staged => {
                if self.selected_staged > 0 {
                    self.selected_staged -= 1;
                    if (self.selected_staged as f32) < self.staged_scroll {
                        self.staged_scroll = self.selected_staged as f32;
                    }
                } else if !self.unstaged.is_empty() {
                    self.focus = Focus::Unstaged;
                    self.selected_unstaged = self.unstaged.len().saturating_sub(1);
                    self.sync_scroll_to_selected();
                }
            }
        }
    }

    fn move_selection_down(&mut self) {
        match self.focus {
            Focus::Unstaged => {
                if self.selected_unstaged + 1 < self.unstaged.len() {
                    self.selected_unstaged += 1;
                    self.ensure_selected_visible();
                } else if !self.staged.is_empty() {
                    self.focus = Focus::Staged;
                    self.selected_staged = 0;
                    self.sync_scroll_to_selected();
                }
            }
            Focus::Staged => {
                if self.selected_staged + 1 < self.staged.len() {
                    self.selected_staged += 1;
                    self.ensure_selected_visible();
                }
            }
        }
    }

    fn focus_unstaged(&mut self) {
        if !self.unstaged.is_empty() {
            self.focus = Focus::Unstaged;
        }
    }

    fn focus_staged(&mut self) {
        if !self.staged.is_empty() {
            self.focus = Focus::Staged;
        }
    }

    fn toggle_stage_selected(&mut self) {
        let entry = match self.current_entry().cloned() {
            Some(entry) => entry,
            None => return,
        };

        let result = match self.focus {
            Focus::Unstaged => stage_path(&entry.path),
            Focus::Staged => unstage_path(&entry.path),
        };

        if let Err(err) = result {
            self.set_error(err);
            return;
        }

        if let Err(err) = self.refresh_status() {
            self.set_error(err);
        }
    }

    fn scroll_diff(&mut self, delta: i32) {
        if self.diff_lines.is_empty() {
            return;
        }
        let current = self.diff_scroll as i32;
        let mut next = current + delta;
        if next < 0 {
            next = 0;
        }
        let max_offset = (self.diff_lines.len() as i32).saturating_sub(1);
        if next > max_offset {
            next = max_offset;
        }
        self.diff_scroll = next as f32;
    }

    fn scroll_files(&mut self, focus: Focus, delta: i32) {
        let (len, scroll, _selected) = match focus {
            Focus::Unstaged => (
                self.unstaged.len(),
                &mut self.unstaged_scroll,
                self.selected_unstaged,
            ),
            Focus::Staged => (
                self.staged.len(),
                &mut self.staged_scroll,
                self.selected_staged,
            ),
        };
        if len == 0 {
            return;
        }
        let mut next = (*scroll as i32) + delta;
        if next < 0 {
            next = 0;
        }
        let max_offset = (len as i32).saturating_sub(1);
        if next > max_offset {
            next = max_offset;
        }
        *scroll = next as f32;
        // if (selected as f32) < *scroll {
        //     *scroll = selected as f32;
        // }
    }

    fn ensure_selected_visible(&mut self) {
        match self.focus {
            Focus::Unstaged => {
                if (self.selected_unstaged as f32) < self.unstaged_scroll {
                    self.unstaged_scroll = self.selected_unstaged as f32;
                }
            }
            Focus::Staged => {
                if (self.selected_staged as f32) < self.staged_scroll {
                    self.staged_scroll = self.selected_staged as f32;
                }
            }
        }
    }

    fn sync_scroll_to_selected(&mut self) {
        self.ensure_selected_visible();
    }

    fn clamp_file_scrolls(&mut self) {
        if self.unstaged.is_empty() {
            self.unstaged_scroll = 0.0;
        }
        if self.staged.is_empty() {
            self.staged_scroll = 0.0;
        }
        if self.unstaged_scroll as usize >= self.unstaged.len() {
            self.unstaged_scroll = self.unstaged.len().saturating_sub(1) as f32;
        }
        if self.staged_scroll as usize >= self.staged.len() {
            self.staged_scroll = self.staged.len().saturating_sub(1) as f32;
        }
    }
}

enum Msg {
    KeyPressed(Key),
    // true if its the staged list
    ActivateFile(bool, usize),
    ScrollPreview(i32),
    // true if its the staged list
    ScrollFiles(bool, i32),
}

#[derive(Clone, Debug)]
struct DiffLine {
    content: String,
    color: Option<Color>,
    bold: bool,
}

impl DiffLine {
    fn plain(content: impl Into<String>) -> Self {
        Self {
            content: content.into(),
            color: None,
            bold: false,
        }
    }

    fn from_diff_line(line: &str) -> Self {
        let mut diff_line = Self::plain(line);

        if line.starts_with("+++") || line.starts_with("---") {
            diff_line.color = Some(Color::Yellow);
        } else if line.starts_with("diff") {
            diff_line.color = Some(Color::Cyan);
            diff_line.bold = true;
        } else if line.starts_with("@@") {
            diff_line.color = Some(Color::Yellow);
            diff_line.bold = true;
        } else if line.starts_with('+') {
            diff_line.color = Some(Color::Green);
        } else if line.starts_with('-') {
            diff_line.color = Some(Color::Red);
        }

        diff_line
    }
}

struct GitStatus {
    unstaged: Vec<FileEntry>,
    staged: Vec<FileEntry>,
}

fn load_git_status() -> Result<GitStatus> {
    let output = run_git(["status", "--porcelain"])?;

    let mut unstaged = Vec::new();
    let mut staged = Vec::new();

    for line in output.lines() {
        if line.trim().is_empty() {
            continue;
        }

        if let Some(parsed) = ParsedStatus::parse(line) {
            if parsed.include_unstaged() {
                unstaged.push(FileEntry::new(
                    parsed.path.clone(),
                    parsed.unstaged_code,
                    parsed.unstaged_display(),
                ));
            }

            if parsed.include_staged() {
                staged.push(FileEntry::new(
                    parsed.path.clone(),
                    parsed.staged_code,
                    parsed.staged_display(),
                ));
            }
        }
    }

    Ok(GitStatus { unstaged, staged })
}

struct ParsedStatus {
    staged_code: char,
    unstaged_code: char,
    path: String,
    display_path: String,
}

impl ParsedStatus {
    fn parse(line: &str) -> Option<Self> {
        if line.len() < 4 {
            return None;
        }

        let mut chars = line.chars();
        let staged_code = chars.next()?;
        let unstaged_code = chars.next()?;

        // Skip the space after status codes.
        let remainder = line[3..].to_string();
        let path = remainder
            .split(" -> ")
            .last()
            .map(str::to_string)
            .unwrap_or_else(|| remainder.clone());

        Some(Self {
            staged_code,
            unstaged_code,
            path,
            display_path: remainder,
        })
    }

    fn include_unstaged(&self) -> bool {
        self.unstaged_code != ' ' || self.staged_code == '?'
    }

    fn include_staged(&self) -> bool {
        self.staged_code != ' ' && self.staged_code != '?'
    }

    fn unstaged_display(&self) -> String {
        format!("{} {}", self.unstaged_code, self.display_path)
    }

    fn staged_display(&self) -> String {
        format!("{} {}", self.staged_code, self.display_path)
    }
}

fn diff_for(entry: &FileEntry, focus: Focus) -> Result<Vec<DiffLine>> {
    let diff_output = match focus {
        Focus::Unstaged => diff_unstaged(entry),
        Focus::Staged => diff_staged(entry),
    }?;

    Ok(format_diff(&diff_output))
}

fn diff_unstaged(entry: &FileEntry) -> Result<String> {
    if entry.code == '?' {
        return run_git_allow_diff([
            "diff",
            "--color=never",
            "--no-index",
            "--",
            "/dev/null",
            &entry.path,
        ]);
    }

    run_git_allow_diff(["diff", "--color=never", "--", &entry.path])
}

fn diff_staged(entry: &FileEntry) -> Result<String> {
    run_git_allow_diff(["diff", "--cached", "--color=never", "--", &entry.path])
}

fn format_diff(diff: &str) -> Vec<DiffLine> {
    if diff.trim().is_empty() {
        return Vec::new();
    }

    diff.lines().map(DiffLine::from_diff_line).collect()
}

fn stage_path(path: &str) -> Result<()> {
    run_git(["add", "--", path]).map(|_| ())
}

fn unstage_path(path: &str) -> Result<()> {
    run_git(["reset", "HEAD", "--", path]).map(|_| ())
}

fn run_git<I, S>(args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    run_git_internal(args, false)
}

fn run_git_allow_diff<I, S>(args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    run_git_internal(args, true)
}

fn run_git_internal<I, S>(args: I, allow_diff_exit: bool) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args_vec: Vec<String> = args
        .into_iter()
        .map(|arg| arg.as_ref().to_string_lossy().into_owned())
        .collect();

    let output = Command::new("git")
        .args(&args_vec)
        .env("GIT_PAGER", "cat")
        .output()
        .wrap_err_with(|| format!("failed to execute git {}", args_vec.join(" ")))?;

    let status = output.status;
    let acceptable = status.success() || (allow_diff_exit && matches!(status.code(), Some(1)));

    if !acceptable {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(eyre!(
            "git {} failed: {}",
            args_vec.join(" "),
            stderr.trim()
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn clamp_index(index: &mut usize, len: usize) {
    if len == 0 {
        *index = 0;
    } else if *index >= len {
        *index = len - 1;
    }
}

fn init_tracing() -> color_eyre::Result<()> {
    use std::fs::File;
    use std::path::PathBuf;
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

    let log_path = PathBuf::from("gs.log");

    File::options().create(true).append(true).open(&log_path)?;

    let writer = tracing_subscriber::fmt::writer::BoxMakeWriter::new({
        let log_path = log_path.clone();
        move || {
            File::options()
                .create(true)
                .append(true)
                .open(&log_path)
                .expect("log file should remain writable")
        }
    });

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_ansi(false)
                .with_writer(writer),
        )
        .with(EnvFilter::from_default_env())
        .try_init()
        .map_err(|error| color_eyre::eyre::eyre!(error))?;

    Ok(())
}
