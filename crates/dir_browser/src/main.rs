use std::cmp::Ordering;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use chatui::components::data_table::{
    ColumnDef, DataTableMsg, DataTableState, DataTableStyle, RowData, SelectionMode,
    data_table_view,
};
use chatui::dom::{Color, Style, TextSpan, column, text};
use chatui::{Event, Key, KeyCode, Program, TableColumnWidth, Transition};
use taffy::Dimension;
use time::OffsetDateTime;
use time::format_description::well_known::Rfc3339;

#[derive(Clone, Debug)]
struct FileEntry {
    name: String,
    path: PathBuf,
    is_dir: bool,
    size: Option<u64>,
    modified: Option<SystemTime>,
}

impl FileEntry {
    fn from_path(path: PathBuf) -> Self {
        let name = path
            .file_name()
            .map(|name| name.to_string_lossy().to_string())
            .unwrap_or_else(|| path.display().to_string());

        let metadata = fs::symlink_metadata(&path).ok();
        let is_dir = metadata.as_ref().map(|m| m.is_dir()).unwrap_or(false);
        let size = metadata
            .as_ref()
            .and_then(|m| if m.is_file() { Some(m.len()) } else { None });
        let modified = metadata.and_then(|m| m.modified().ok());

        Self {
            name,
            path,
            is_dir,
            size,
            modified,
        }
    }

    fn display_name(&self) -> String {
        if self.is_dir {
            format!("{}/", self.name)
        } else {
            self.name.clone()
        }
    }

    fn size_display(&self) -> String {
        match (self.is_dir, self.size) {
            (true, _) => "-".to_string(),
            (_, Some(size)) => size.to_string(),
            _ => "-".to_string(),
        }
    }

    fn modified_display(&self) -> String {
        self.modified
            .map(|mtime| {
                let datetime: OffsetDateTime = mtime.into();
                datetime
                    .format(&Rfc3339)
                    .unwrap_or_else(|_| "-".to_string())
            })
            .unwrap_or_else(|| "-".to_string())
    }

    fn to_row(&self, row_id: usize) -> RowData<usize> {
        let name_style = if self.is_dir {
            Style::fg(Color::Cyan)
        } else {
            Style::default()
        };

        RowData::new(
            row_id,
            vec![
                vec![TextSpan::new(self.display_name(), name_style)],
                vec![TextSpan::new(self.size_display(), Style::default())],
                vec![TextSpan::new(self.modified_display(), Style::default())],
            ],
        )
    }
}

struct Model {
    current_dir: PathBuf,
    entries: Vec<FileEntry>,
    table_state: DataTableState<usize>,
    status: Option<String>,
}

impl Model {
    fn new(start_dir: PathBuf) -> Self {
        let mut table_state = DataTableState::new(SelectionMode::Row);
        table_state.set_columns(vec![
            ColumnDef::new(TableColumnWidth::Flexible(5.0)).with_header("Name"),
            ColumnDef::new(TableColumnWidth::Fixed(12.0)).with_header("Size"),
            ColumnDef::new(TableColumnWidth::Flexible(1.0)).with_header("Modified"),
        ]);

        let mut model = Self {
            current_dir: start_dir,
            entries: Vec::new(),
            table_state,
            status: None,
        };

        model.reload_entries();
        model
    }

    fn reload_entries(&mut self) {
        match load_directory(&self.current_dir) {
            Ok(mut entries) => {
                entries.sort_by(compare_entries);
                self.entries = entries;
                self.status = None;
                let rows = self
                    .entries
                    .iter()
                    .enumerate()
                    .map(|(idx, entry)| entry.to_row(idx))
                    .collect();
                self.table_state.set_rows(rows);
                if !self.entries.is_empty() {
                    self.table_state.select_first_row();
                }
            }
            Err(err) => {
                self.entries.clear();
                self.table_state.set_rows(Vec::new());
                self.status = Some(err.to_string());
            }
        }
    }

    fn set_directory(&mut self, path: PathBuf) {
        self.current_dir = path;
        self.reload_entries();
    }

    fn selected_entry(&self) -> Option<&FileEntry> {
        self.table_state
            .selected_row()
            .and_then(|idx| self.entries.get(idx))
    }

    fn status_line(&self) -> String {
        let mut base = String::from("Enter: open  Backspace: up  r: refresh  q: quit");
        if let Some(err) = &self.status {
            base.push_str("  |  Error: ");
            base.push_str(err);
        }
        base
    }
}

fn load_directory(path: &Path) -> std::io::Result<Vec<FileEntry>> {
    let entries = fs::read_dir(path)?
        .filter_map(|entry| entry.ok())
        .map(|entry| FileEntry::from_path(entry.path()))
        .collect();
    Ok(entries)
}

fn compare_entries(a: &FileEntry, b: &FileEntry) -> Ordering {
    match (a.is_dir, b.is_dir) {
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        _ => a.name.to_lowercase().cmp(&b.name.to_lowercase()),
    }
}

#[derive(Clone, Debug)]
enum Msg {
    MoveDown,
    MoveUp,
    OpenSelected,
    GoParent,
    Refresh,
    Quit,
    TableEvent(DataTableMsg<usize>),
}

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    match msg {
        Msg::MoveDown => model.table_state.select_next_row(),
        Msg::MoveUp => model.table_state.select_prev_row(),
        Msg::OpenSelected => {
            if let Some(entry) = model.selected_entry()
                && entry.is_dir
            {
                model.set_directory(entry.path.clone());
            }
        }
        Msg::GoParent => {
            if let Some(parent) = model.current_dir.parent() {
                model.set_directory(parent.to_path_buf());
            }
        }
        Msg::Refresh => model.reload_entries(),
        Msg::Quit => return Transition::Quit,
        Msg::TableEvent(event) => {
            if let DataTableMsg::RowActivated(id) = event
                && let Some(entry) = model.entries.get(id)
                && entry.is_dir
            {
                model.set_directory(entry.path.clone());
            }
        }
    }

    Transition::Continue
}

fn view(model: &Model) -> chatui::Node<Msg> {
    let style = DataTableStyle {
        alternate_row_style: Some(Style {
            bg: Some(Color::rgba(30, 30, 40, 255)),
            ..Default::default()
        }),
        selected_row_style: Style {
            reverse: true,
            ..Default::default()
        },
        ..DataTableStyle::default()
    };

    let table = data_table_view(&model.table_state, &style, Msg::TableEvent)
        .with_flex_grow(1.0)
        .with_width(Dimension::percent(1.));

    column(vec![
        text(format!("Directory: {}", model.current_dir.display())),
        table,
        text(model.status_line()),
    ])
    .with_height(Dimension::percent(1.))
    .with_width(Dimension::percent(1.))
}

fn main() {
    color_eyre::install().expect("failed to install color-eyre");

    let start_dir = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| std::env::current_dir().expect("failed to determine current directory"));

    let initial_dir = if start_dir.is_dir() {
        start_dir
    } else {
        start_dir
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."))
    };

    let mut model = Model::new(initial_dir);
    let program = Program::new(&mut model, update, view).map_event(|event| match event {
        Event::Key(Key {
            code: KeyCode::Char('q'),
            ..
        }) => Some(Msg::Quit),
        Event::Key(Key {
            code: KeyCode::Char('j'),
            ..
        })
        | Event::Key(Key {
            code: KeyCode::Down,
            ..
        }) => Some(Msg::MoveDown),
        Event::Key(Key {
            code: KeyCode::Char('k'),
            ..
        })
        | Event::Key(Key {
            code: KeyCode::Up, ..
        }) => Some(Msg::MoveUp),
        Event::Key(Key {
            code: KeyCode::Enter,
            ..
        }) => Some(Msg::OpenSelected),
        Event::Key(Key {
            code: KeyCode::Backspace,
            ..
        }) => Some(Msg::GoParent),
        Event::Key(Key {
            code: KeyCode::Char('r'),
            ..
        }) => Some(Msg::Refresh),
        _ => None,
    });

    if let Err(err) = program.run() {
        eprintln!("Program failed: {:?}", err);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::test_utils::render_node_to_string;
    use tempfile::tempdir;

    fn setup_directory() -> (tempfile::TempDir, PathBuf) {
        let temp = tempdir().expect("create temp dir");
        let dir_path = temp.path().to_path_buf();

        let nested = dir_path.join("nested");
        fs::create_dir_all(&nested).expect("create nested dir");
        fs::write(dir_path.join("file.txt"), "hello world").expect("write file");
        fs::write(nested.join("inner.txt"), "nested").expect("write nested file");

        (temp, dir_path)
    }

    #[test]
    fn renders_directory_contents_and_navigation() {
        let (_temp, root_path) = setup_directory();

        let mut model = Model::new(root_path.clone());

        let mut root_view = view(&model);
        let rendered_root =
            render_node_to_string(&mut root_view, 80, 12).expect("render root directory");
        assert!(
            rendered_root.contains("nested/"),
            "root view should list nested directory"
        );
        assert!(
            rendered_root.contains("file.txt"),
            "root view should list file.txt"
        );

        assert!(matches!(
            update(&mut model, Msg::OpenSelected),
            Transition::Continue
        ));
        assert!(
            model.current_dir.ends_with("nested"),
            "model should navigate into nested directory"
        );

        let mut nested_view = view(&model);
        let rendered_nested =
            render_node_to_string(&mut nested_view, 80, 12).expect("render nested directory");
        assert!(
            rendered_nested.contains("inner.txt"),
            "nested view should show inner.txt"
        );
        assert!(
            !rendered_nested.contains("file.txt"),
            "nested view should not show root files"
        );

        assert!(matches!(
            update(&mut model, Msg::GoParent),
            Transition::Continue
        ));
        assert_eq!(model.current_dir, root_path);

        let mut final_view = view(&model);
        let rendered_final =
            render_node_to_string(&mut final_view, 80, 12).expect("render after returning");
        assert!(
            rendered_final.contains("nested/"),
            "final view should list nested directory again"
        );
    }

    #[test]
    fn selection_moves_with_update_messages() {
        let (_temp, root_path) = setup_directory();
        let mut model = Model::new(root_path);

        assert_eq!(model.table_state.selected_row(), Some(0));

        assert!(matches!(
            update(&mut model, Msg::MoveDown),
            Transition::Continue
        ));
        assert_eq!(model.table_state.selected_row(), Some(1));

        assert!(matches!(
            update(&mut model, Msg::MoveUp),
            Transition::Continue
        ));
        assert_eq!(model.table_state.selected_row(), Some(0));
    }
}
