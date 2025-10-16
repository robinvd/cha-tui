use chatui::components::data_table::{
    CellPosition, ColumnDef, DataTableMsg, DataTableState, DataTableStyle, RowData, SelectionMode,
    data_table_view,
};
use chatui::dom::Color;
use chatui::{
    Event, Key, KeyCode, Program, Style, TableColumnWidth, TextSpan, Transition, column, text,
};
use taffy::Dimension;
use tracing_subscriber::EnvFilter;

#[derive(Clone, Debug)]
struct Employee {
    id: u32,
    name: String,
    role: String,
    department: String,
    salary: u32,
}

impl Employee {
    fn new(id: u32, name: &str, role: &str, department: &str, salary: u32) -> Self {
        Self {
            id,
            name: name.to_string(),
            role: role.to_string(),
            department: department.to_string(),
            salary,
        }
    }

    fn to_row_data(&self) -> RowData<u32> {
        RowData::new(
            self.id,
            vec![
                vec![TextSpan::new(self.id.to_string(), Style::default())],
                vec![TextSpan::new(&self.name, Style::default())],
                vec![TextSpan::new(&self.role, Style::default())],
                vec![TextSpan::new(&self.department, Style::default())],
                vec![TextSpan::new(
                    format!("${}", self.salary),
                    Style::fg(Color::Green),
                )],
            ],
        )
    }
}

struct Model {
    table_state_row: DataTableState<u32>,
    table_state_cell: DataTableState<u32>,
    current_mode: ModeSelection,
    status_message: String,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ModeSelection {
    RowMode,
    CellMode,
}

enum Msg {
    TableRow(DataTableMsg<u32>),
    TableCell(DataTableMsg<u32>),
    NextRow,
    PrevRow,
    FirstRow,
    LastRow,
    NextCell,
    PrevCell,
    FirstCellInRow,
    LastCellInRow,
    SwitchMode,
    Quit,
}

fn init_model() -> Model {
    let employees = vec![
        Employee::new(
            1,
            "Alice Johnson",
            "Software Engineer",
            "Engineering",
            95000,
        ),
        Employee::new(2, "Bob Smith", "Product Manager", "Product", 110000),
        Employee::new(3, "Carol Williams", "Designer", "Design", 85000),
        Employee::new(4, "David Brown", "DevOps Engineer", "Engineering", 100000),
        Employee::new(5, "Eve Davis", "QA Engineer", "Engineering", 80000),
        Employee::new(6, "Frank Miller", "Sales Manager", "Sales", 105000),
        Employee::new(7, "Grace Wilson", "Marketing Manager", "Marketing", 95000),
        Employee::new(8, "Henry Moore", "Data Scientist", "Data", 120000),
    ];

    let columns = vec![
        ColumnDef::new(TableColumnWidth::Fixed(5.0)).with_header("ID"),
        ColumnDef::new(TableColumnWidth::Auto).with_header("Name"),
        ColumnDef::new(TableColumnWidth::Flexible(1.0)).with_header("Role"),
        ColumnDef::new(TableColumnWidth::Auto).with_header("Department"),
        ColumnDef::new(TableColumnWidth::Fixed(12.0)).with_header("Salary"),
    ];

    let rows: Vec<RowData<u32>> = employees.iter().map(|e| e.to_row_data()).collect();

    let mut table_state_row = DataTableState::new(SelectionMode::Row);
    table_state_row.set_columns(columns.clone());
    table_state_row.set_rows(rows.clone());
    table_state_row.select_first_row();

    let mut table_state_cell = DataTableState::new(SelectionMode::Cell);
    table_state_cell.set_columns(columns);
    table_state_cell.set_rows(rows);
    table_state_cell.select_cell(CellPosition::new(0, 0));

    Model {
        table_state_row,
        table_state_cell,
        current_mode: ModeSelection::RowMode,
        status_message: "Row Mode - Use ↑/↓ or j/k to navigate, Enter to activate, Tab to switch modes, q to quit".to_string(),
    }
}

fn update(model: &mut Model, msg: Msg) -> Transition {
    match msg {
        Msg::TableRow(table_msg) => {
            model.status_message = format!("Row Mode - Event: {:?}", table_msg);
        }
        Msg::TableCell(table_msg) => {
            model.status_message = format!("Cell Mode - Event: {:?}", table_msg);
        }
        Msg::NextRow => match model.current_mode {
            ModeSelection::RowMode => {
                model.table_state_row.select_next_row();
                if let Some(row_idx) = model.table_state_row.selected_row() {
                    model.status_message = format!("Row Mode - Selected row: {}", row_idx);
                }
            }
            ModeSelection::CellMode => {
                model.table_state_cell.select_next_row();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - Selected cell: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        },
        Msg::PrevRow => match model.current_mode {
            ModeSelection::RowMode => {
                model.table_state_row.select_prev_row();
                if let Some(row_idx) = model.table_state_row.selected_row() {
                    model.status_message = format!("Row Mode - Selected row: {}", row_idx);
                }
            }
            ModeSelection::CellMode => {
                model.table_state_cell.select_prev_row();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - Selected cell: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        },
        Msg::FirstRow => match model.current_mode {
            ModeSelection::RowMode => {
                model.table_state_row.select_first_row();
                model.status_message = "Row Mode - Selected first row".to_string();
            }
            ModeSelection::CellMode => {
                model.table_state_cell.select_first_row();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - Selected cell: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        },
        Msg::LastRow => match model.current_mode {
            ModeSelection::RowMode => {
                model.table_state_row.select_last_row();
                model.status_message = "Row Mode - Selected last row".to_string();
            }
            ModeSelection::CellMode => {
                model.table_state_cell.select_last_row();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - Selected cell: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        },
        Msg::NextCell => {
            if model.current_mode == ModeSelection::CellMode {
                model.table_state_cell.select_next_cell();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - Selected cell: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        }
        Msg::PrevCell => {
            if model.current_mode == ModeSelection::CellMode {
                model.table_state_cell.select_prev_cell();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - Selected cell: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        }
        Msg::FirstCellInRow => {
            if model.current_mode == ModeSelection::CellMode {
                model.table_state_cell.select_first_cell_in_row();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - First cell in row: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        }
        Msg::LastCellInRow => {
            if model.current_mode == ModeSelection::CellMode {
                model.table_state_cell.select_last_cell_in_row();
                if let Some(pos) = model.table_state_cell.selected_cell() {
                    model.status_message = format!(
                        "Cell Mode - Last cell in row: row {}, col {}",
                        pos.row, pos.column
                    );
                }
            }
        }
        Msg::SwitchMode => {
            model.current_mode = match model.current_mode {
                ModeSelection::RowMode => {
                    // Sync selection from row to cell mode
                    if let Some(row_idx) = model.table_state_row.selected_row() {
                        model
                            .table_state_cell
                            .select_cell(CellPosition::new(row_idx, 0));
                    }
                    model.status_message = "Cell Mode - Use arrow keys or h/j/k/l to navigate, Enter to activate, Tab to switch modes".to_string();
                    ModeSelection::CellMode
                }
                ModeSelection::CellMode => {
                    // Sync selection from cell to row mode
                    if let Some(pos) = model.table_state_cell.selected_cell() {
                        model.table_state_row.select_row(pos.row);
                    }
                    model.status_message = "Row Mode - Use ↑/↓ or j/k to navigate, Enter to activate, Tab to switch modes".to_string();
                    ModeSelection::RowMode
                }
            };
        }
        Msg::Quit => return Transition::Quit,
    }
    Transition::Continue
}

fn view(model: &Model) -> chatui::Node<Msg> {
    let style = DataTableStyle {
        alternate_row_style: Some(Style::bg(Color::rgba(20, 20, 30, 255))),
        column_gap: 2,
        ..Default::default()
    };

    let table = match model.current_mode {
        ModeSelection::RowMode => data_table_view(&model.table_state_row, &style, Msg::TableRow),
        ModeSelection::CellMode => data_table_view(&model.table_state_cell, &style, Msg::TableCell),
    }
    .with_height(Dimension::length(5.));

    let title_text = match model.current_mode {
        ModeSelection::RowMode => "Employee Directory (Row Selection Mode)",
        ModeSelection::CellMode => "Employee Directory (Cell Selection Mode)",
    };

    column(vec![text(title_text), table, text(&model.status_message)])
}

fn main() {
    color_eyre::install().unwrap();
    init_tracing().unwrap();

    let model = init_model();
    let program = Program::new(model, update, view);

    // Map keyboard events
    let program = program.map_event(|event| match event {
        Event::Key(Key {
            code: KeyCode::Char('q'),
            ..
        }) => Some(Msg::Quit),
        Event::Key(Key {
            code: KeyCode::Down,
            ..
        })
        | Event::Key(Key {
            code: KeyCode::Char('j'),
            ..
        }) => Some(Msg::NextRow),
        Event::Key(Key {
            code: KeyCode::Up, ..
        })
        | Event::Key(Key {
            code: KeyCode::Char('k'),
            ..
        }) => Some(Msg::PrevRow),
        Event::Key(Key {
            code: KeyCode::Right,
            ..
        })
        | Event::Key(Key {
            code: KeyCode::Char('l'),
            ..
        }) => Some(Msg::NextCell),
        Event::Key(Key {
            code: KeyCode::Left,
            ..
        })
        | Event::Key(Key {
            code: KeyCode::Char('h'),
            ..
        }) => Some(Msg::PrevCell),
        Event::Key(Key {
            code: KeyCode::Char('g'),
            ..
        }) => Some(Msg::FirstRow),
        Event::Key(Key {
            code: KeyCode::Char('G'),
            ..
        }) => Some(Msg::LastRow),
        Event::Key(Key {
            code: KeyCode::Char('0'),
            ..
        }) => Some(Msg::FirstCellInRow),
        Event::Key(Key {
            code: KeyCode::Char('$'),
            ..
        }) => Some(Msg::LastCellInRow),
        Event::Key(Key {
            code: KeyCode::Tab, ..
        }) => Some(Msg::SwitchMode),
        _ => None,
    });

    program.run().expect("Program failed");
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::buffer::DoubleBuffer;
    use chatui::dom::rounding::round_layout;
    use chatui::event::Size;
    use chatui::palette::Palette;
    use chatui::render::Renderer;
    use taffy::{AvailableSpace, compute_root_layout};

    fn render_to_lines(node: &mut chatui::Node<Msg>, width: usize, height: usize) -> Vec<String> {
        let size = Size::new(
            width.try_into().expect("width fits in u16"),
            height.try_into().expect("height fits in u16"),
        );

        // Prepare layout
        compute_root_layout(
            node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(size.width as f32),
                height: AvailableSpace::Definite(size.height as f32),
            },
        );
        round_layout(node);

        let mut buffer = DoubleBuffer::new(width, height);
        let palette = Palette::default();
        {
            let mut renderer = Renderer::new(&mut buffer, &palette);
            renderer.render(node, size).expect("render should succeed");
        }

        // Extract text from buffer manually
        let mut lines = Vec::new();
        for y in 0..height {
            let mut line = String::new();
            for x in 0..width {
                if let Some(cell) = buffer.get_cell(x, y) {
                    line.push(cell.ch);
                } else {
                    line.push(' ');
                }
            }
            lines.push(line);
        }
        lines
    }

    #[test]
    fn simple_table_in_column_renders() {
        use chatui::dom::{TableColumn, TableColumnWidth, TableRow, column, table, text};

        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<Msg>("A")),
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<Msg>("B")),
        ];
        let rows = vec![
            TableRow::new(vec![text::<Msg>("1"), text::<Msg>("2")]),
            TableRow::new(vec![text::<Msg>("3"), text::<Msg>("4")]),
        ];
        let table_node = table(columns, rows);

        let mut view = column(vec![
            text::<Msg>("Title"),
            table_node,
            text::<Msg>("Footer"),
        ]);

        let lines = render_to_lines(&mut view, 30, 10);

        println!("\n=== Simple Table in Column ===");
        for (i, line) in lines.iter().enumerate() {
            println!("{:2}: '{}'", i, line);
        }
        println!("=== End ===\n");

        let all_content = lines.join("\n");
        assert!(all_content.contains("A"), "Should contain header A");
        assert!(all_content.contains("B"), "Should contain header B");
        assert!(all_content.contains("1"), "Should contain data 1");
        assert!(all_content.contains("2"), "Should contain data 2");
    }

    #[test]
    fn table_renders_directly_without_column() {
        let model = init_model();
        let style = DataTableStyle {
            alternate_row_style: Some(Style::bg(Color::rgba(20, 20, 30, 255))),
            column_gap: 2,
            ..Default::default()
        };

        let mut table_node = data_table_view(&model.table_state_row, &style, |_| Msg::Quit);

        // Render the table directly without wrapping in column
        let lines = render_to_lines(&mut table_node, 100, 30);

        println!("\n=== Direct Table Render ({} lines) ===", lines.len());
        for (i, line) in lines.iter().take(15).enumerate() {
            println!("{:3}: '{}'", i, line);
        }
        println!("=== End ===\n");

        let all_content = lines.join("\n");

        // Check for headers
        assert!(all_content.contains("ID"), "Should contain ID header");
        assert!(all_content.contains("Name"), "Should contain Name header");

        // Check for data
        assert!(all_content.contains("Alice"), "Should contain Alice");
    }

    #[test]
    fn initial_model_renders_table_with_employee_data() {
        let model = init_model();
        let mut view_node = view(&model);

        // Render with a large terminal size
        let lines = render_to_lines(&mut view_node, 100, 50);

        // Print the actual output for debugging
        println!("\n=== Rendered Output ({} lines) ===", lines.len());
        for (i, line) in lines.iter().take(20).enumerate() {
            println!("{:3}: '{}'", i, line);
        }
        println!("=== End Output ===\n");

        // Check that we have multiple lines (not just 2)
        assert!(
            lines.len() > 10,
            "Should have more than 10 lines, got {}",
            lines.len()
        );

        // Join all lines to make assertions easier
        let all_content = lines.join("\n");

        // Check for title
        assert!(
            all_content.contains("Employee Directory"),
            "Should contain title"
        );

        // Check for column headers
        assert!(all_content.contains("ID"), "Should contain ID header");
        assert!(all_content.contains("Name"), "Should contain Name header");
        assert!(all_content.contains("Role"), "Should contain Role header");
        assert!(
            all_content.contains("Department"),
            "Should contain Department header"
        );
        assert!(
            all_content.contains("Salary"),
            "Should contain Salary header"
        );

        // Check for employee data
        assert!(all_content.contains("Alice"), "Should contain Alice");
        assert!(all_content.contains("Bob"), "Should contain Bob");
        assert!(all_content.contains("Carol"), "Should contain Carol");
        assert!(
            all_content.contains("Software Engineer"),
            "Should contain Software Engineer"
        );
        assert!(
            all_content.contains("Product Manager"),
            "Should contain Product Manager"
        );
        assert!(all_content.contains("Designer"), "Should contain Designer");

        // Check for salary data
        assert!(
            all_content.contains("95000") || all_content.contains("$95000"),
            "Should contain salary 95000"
        );
        assert!(
            all_content.contains("110000") || all_content.contains("$110000"),
            "Should contain salary 110000"
        );
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
