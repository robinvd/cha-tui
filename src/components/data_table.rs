use std::hash::Hash;

use crate::dom::{
    Node, Style, TableColumn, TableColumnWidth, TableRow, TextSpan, rich_text, table,
};

/// Selection mode for the data table.
///
/// Determines how the user can navigate and select within the table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SelectionMode {
    /// Select entire rows at a time with up/down navigation.
    Row,
    /// Select individual cells with 2D navigation (up/down/left/right).
    Cell,
}

/// Position of a cell in the table grid.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CellPosition {
    pub row: usize,
    pub column: usize,
}

impl CellPosition {
    pub fn new(row: usize, column: usize) -> Self {
        Self { row, column }
    }
}

/// Definition of a table column with optional header.
///
/// # Example
/// ```
/// use chatui::components::data_table::ColumnDef;
/// use chatui::TableColumnWidth;
///
/// let column = ColumnDef::<u32>::new(TableColumnWidth::Auto)
///     .with_header("Name");
/// ```
#[derive(Clone, Debug)]
pub struct ColumnDef<Id> {
    header: Option<String>,
    width: TableColumnWidth,
    _phantom: std::marker::PhantomData<Id>,
}

impl<Id> ColumnDef<Id> {
    /// Creates a new column definition with the specified width.
    pub fn new(width: TableColumnWidth) -> Self {
        Self {
            header: None,
            width,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Adds a header label to this column.
    pub fn with_header(mut self, header: impl Into<String>) -> Self {
        self.header = Some(header.into());
        self
    }

    /// Returns the header text if present.
    pub fn header(&self) -> Option<&str> {
        self.header.as_deref()
    }

    /// Returns the column width constraint.
    pub fn width(&self) -> TableColumnWidth {
        self.width
    }
}

/// Data for a single row in the table.
///
/// Each row has a unique identifier and cell content for each column.
///
/// # Example
/// ```
/// use chatui::components::data_table::RowData;
/// use chatui::{Style, TextSpan};
///
/// let row = RowData::new(
///     1,
///     vec![
///         vec![TextSpan::new("Cell 1", Style::default())],
///         vec![TextSpan::new("Cell 2", Style::default())],
///     ],
/// );
/// ```
#[derive(Clone, Debug)]
pub struct RowData<Id> {
    pub id: Id,
    pub cells: Vec<Vec<TextSpan>>,
    pub row_style: Option<Style>,
}

impl<Id> RowData<Id> {
    /// Creates a new row with the given ID and cell content.
    ///
    /// The number of cells must match the number of columns in the table.
    pub fn new(id: Id, cells: Vec<Vec<TextSpan>>) -> Self {
        Self {
            id,
            cells,
            row_style: None,
        }
    }

    /// Sets a custom style for this row.
    pub fn with_style(mut self, style: Style) -> Self {
        self.row_style = Some(style);
        self
    }
}

/// Visual styling configuration for the data table.
#[derive(Clone, Debug)]
pub struct DataTableStyle {
    /// Style for normal rows.
    pub row_style: Style,
    /// Style for the selected row (Row selection mode).
    pub selected_row_style: Style,
    /// Style for the selected cell (Cell selection mode).
    pub selected_cell_style: Style,
    /// Style for header row.
    pub header_style: Style,
    /// Optional style for alternating rows.
    pub alternate_row_style: Option<Style>,
    /// Gap between columns in terminal cells.
    pub column_gap: u16,
    /// Gap between rows in terminal cells.
    pub row_gap: u16,
}

impl Default for DataTableStyle {
    fn default() -> Self {
        Self {
            row_style: Style::default(),
            selected_row_style: Style::bg(crate::dom::Color::rgba(60, 60, 80, 255)),
            selected_cell_style: Style::bg(crate::dom::Color::rgba(80, 80, 100, 255)),
            header_style: Style {
                bold: true,
                ..Style::default()
            },
            alternate_row_style: None,
            column_gap: 1,
            row_gap: 0,
        }
    }
}

/// State management for the data table widget.
///
/// Tracks columns, rows, and the current selection based on the mode.
///
/// # Example
/// ```
/// use chatui::components::data_table::{DataTableState, SelectionMode, ColumnDef, RowData};
/// use chatui::{TableColumnWidth, TextSpan, Style};
///
/// let mut state = DataTableState::new(SelectionMode::Row);
///
/// let columns = vec![
///     ColumnDef::new(TableColumnWidth::Auto).with_header("Name"),
///     ColumnDef::new(TableColumnWidth::Auto).with_header("Role"),
/// ];
///
/// let rows = vec![
///     RowData::new(1, vec![
///         vec![TextSpan::new("Alice", Style::default())],
///         vec![TextSpan::new("Engineer", Style::default())],
///     ]),
/// ];
///
/// state.set_columns(columns);
/// state.set_rows(rows);
/// ```
pub struct DataTableState<Id> {
    columns: Vec<ColumnDef<Id>>,
    rows: Vec<RowData<Id>>,
    selection_mode: SelectionMode,
    selected_row: Option<usize>,
    selected_cell: Option<CellPosition>,
}

impl<Id> DataTableState<Id> {
    /// Creates a new data table state with the specified selection mode.
    pub fn new(selection_mode: SelectionMode) -> Self {
        Self {
            columns: Vec::new(),
            rows: Vec::new(),
            selection_mode,
            selected_row: None,
            selected_cell: None,
        }
    }

    /// Sets the columns for the table.
    pub fn set_columns(&mut self, columns: Vec<ColumnDef<Id>>) {
        self.columns = columns;
        self.validate_selection();
    }

    /// Sets the rows for the table.
    pub fn set_rows(&mut self, rows: Vec<RowData<Id>>) {
        self.rows = rows;
        self.validate_selection();
    }

    /// Returns the current selection mode.
    pub fn selection_mode(&self) -> SelectionMode {
        self.selection_mode
    }

    /// Returns the selected row index if in Row mode.
    pub fn selected_row(&self) -> Option<usize> {
        self.selected_row
    }

    /// Returns the selected cell position if in Cell mode.
    pub fn selected_cell(&self) -> Option<CellPosition> {
        self.selected_cell
    }

    /// Returns the number of rows in the table.
    pub fn row_count(&self) -> usize {
        self.rows.len()
    }

    /// Returns the number of columns in the table.
    pub fn column_count(&self) -> usize {
        self.columns.len()
    }

    /// Returns the row data at the given index.
    pub fn row_at(&self, index: usize) -> Option<&RowData<Id>> {
        self.rows.get(index)
    }

    /// Returns all columns.
    pub fn columns(&self) -> &[ColumnDef<Id>] {
        &self.columns
    }

    /// Returns all rows.
    pub fn rows(&self) -> &[RowData<Id>] {
        &self.rows
    }

    // Selection management

    /// Selects the next row (wraps to first).
    pub fn select_next_row(&mut self) {
        if self.rows.is_empty() {
            return;
        }

        match self.selection_mode {
            SelectionMode::Row => {
                self.selected_row = Some(match self.selected_row {
                    Some(idx) => (idx + 1) % self.rows.len(),
                    None => 0,
                });
            }
            SelectionMode::Cell => {
                if let Some(pos) = self.selected_cell {
                    self.selected_cell = Some(CellPosition {
                        row: (pos.row + 1) % self.rows.len(),
                        column: pos.column,
                    });
                } else {
                    self.selected_cell = Some(CellPosition::new(0, 0));
                }
            }
        }
    }

    /// Selects the previous row (wraps to last).
    pub fn select_prev_row(&mut self) {
        if self.rows.is_empty() {
            return;
        }

        match self.selection_mode {
            SelectionMode::Row => {
                self.selected_row = Some(match self.selected_row {
                    Some(idx) if idx > 0 => idx - 1,
                    _ => self.rows.len() - 1,
                });
            }
            SelectionMode::Cell => {
                if let Some(pos) = self.selected_cell {
                    self.selected_cell = Some(CellPosition {
                        row: if pos.row > 0 {
                            pos.row - 1
                        } else {
                            self.rows.len() - 1
                        },
                        column: pos.column,
                    });
                } else {
                    self.selected_cell = Some(CellPosition::new(0, 0));
                }
            }
        }
    }

    /// Selects the first row.
    pub fn select_first_row(&mut self) {
        if self.rows.is_empty() {
            return;
        }

        match self.selection_mode {
            SelectionMode::Row => {
                self.selected_row = Some(0);
            }
            SelectionMode::Cell => {
                if let Some(pos) = self.selected_cell {
                    self.selected_cell = Some(CellPosition {
                        row: 0,
                        column: pos.column,
                    });
                } else {
                    self.selected_cell = Some(CellPosition::new(0, 0));
                }
            }
        }
    }

    /// Selects the last row.
    pub fn select_last_row(&mut self) {
        if self.rows.is_empty() {
            return;
        }

        let last_row = self.rows.len() - 1;
        match self.selection_mode {
            SelectionMode::Row => {
                self.selected_row = Some(last_row);
            }
            SelectionMode::Cell => {
                if let Some(pos) = self.selected_cell {
                    self.selected_cell = Some(CellPosition {
                        row: last_row,
                        column: pos.column,
                    });
                } else {
                    self.selected_cell = Some(CellPosition::new(last_row, 0));
                }
            }
        }
    }

    /// Selects the next cell (moves right, wraps to next row).
    /// Only applicable in Cell mode.
    pub fn select_next_cell(&mut self) {
        if self.selection_mode != SelectionMode::Cell
            || self.rows.is_empty()
            || self.columns.is_empty()
        {
            return;
        }

        let pos = self.selected_cell.unwrap_or(CellPosition::new(0, 0));
        let new_col = pos.column + 1;

        if new_col >= self.columns.len() {
            // Wrap to next row
            let new_row = (pos.row + 1) % self.rows.len();
            self.selected_cell = Some(CellPosition::new(new_row, 0));
        } else {
            self.selected_cell = Some(CellPosition::new(pos.row, new_col));
        }
    }

    /// Selects the previous cell (moves left, wraps to previous row).
    /// Only applicable in Cell mode.
    pub fn select_prev_cell(&mut self) {
        if self.selection_mode != SelectionMode::Cell
            || self.rows.is_empty()
            || self.columns.is_empty()
        {
            return;
        }

        let pos = self.selected_cell.unwrap_or(CellPosition::new(0, 0));

        if pos.column == 0 {
            // Wrap to previous row
            let new_row = if pos.row == 0 {
                self.rows.len() - 1
            } else {
                pos.row - 1
            };
            self.selected_cell = Some(CellPosition::new(new_row, self.columns.len() - 1));
        } else {
            self.selected_cell = Some(CellPosition::new(pos.row, pos.column - 1));
        }
    }

    /// Moves selection to the first cell in the current row.
    /// Only applicable in Cell mode.
    pub fn select_first_cell_in_row(&mut self) {
        if self.selection_mode != SelectionMode::Cell || self.rows.is_empty() {
            return;
        }

        let pos = self.selected_cell.unwrap_or(CellPosition::new(0, 0));
        self.selected_cell = Some(CellPosition::new(pos.row, 0));
    }

    /// Moves selection to the last cell in the current row.
    /// Only applicable in Cell mode.
    pub fn select_last_cell_in_row(&mut self) {
        if self.selection_mode != SelectionMode::Cell
            || self.rows.is_empty()
            || self.columns.is_empty()
        {
            return;
        }

        let pos = self.selected_cell.unwrap_or(CellPosition::new(0, 0));
        self.selected_cell = Some(CellPosition::new(pos.row, self.columns.len() - 1));
    }

    /// Selects a specific row by index.
    pub fn select_row(&mut self, index: usize) {
        if index >= self.rows.len() {
            return;
        }

        match self.selection_mode {
            SelectionMode::Row => {
                self.selected_row = Some(index);
            }
            SelectionMode::Cell => {
                if let Some(pos) = self.selected_cell {
                    self.selected_cell = Some(CellPosition::new(index, pos.column));
                } else {
                    self.selected_cell = Some(CellPosition::new(index, 0));
                }
            }
        }
    }

    /// Selects a specific cell by position.
    /// Only applicable in Cell mode.
    pub fn select_cell(&mut self, position: CellPosition) {
        if self.selection_mode != SelectionMode::Cell {
            return;
        }

        if position.row >= self.rows.len() || position.column >= self.columns.len() {
            return;
        }

        self.selected_cell = Some(position);
    }

    /// Validates and adjusts selection after data changes.
    fn validate_selection(&mut self) {
        match self.selection_mode {
            SelectionMode::Row => {
                if let Some(idx) = self.selected_row
                    && idx >= self.rows.len()
                {
                    self.selected_row = if self.rows.is_empty() {
                        None
                    } else {
                        Some(self.rows.len() - 1)
                    };
                }
            }
            SelectionMode::Cell => {
                if let Some(pos) = self.selected_cell {
                    let valid_row = pos.row < self.rows.len();
                    let valid_col = pos.column < self.columns.len();

                    if !valid_row || !valid_col {
                        self.selected_cell = if self.rows.is_empty() || self.columns.is_empty() {
                            None
                        } else {
                            Some(CellPosition::new(
                                pos.row.min(self.rows.len() - 1),
                                pos.column.min(self.columns.len() - 1),
                            ))
                        };
                    }
                }
            }
        }
    }
}

impl<Id> Default for DataTableState<Id> {
    fn default() -> Self {
        Self::new(SelectionMode::Row)
    }
}

/// Messages emitted by the data table widget.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataTableMsg<Id> {
    /// A row was selected (Row mode).
    RowSelected(Id),
    /// A cell was selected (Cell mode).
    CellSelected { row_id: Id, column: usize },
    /// A row was activated (Enter key or double-click).
    RowActivated(Id),
    /// A cell was activated (Enter key in Cell mode).
    CellActivated { row_id: Id, column: usize },
    /// A header was clicked.
    HeaderClicked(usize),
}

/// Renders a data table with interactive selection and navigation.
///
/// Creates a table view from the given state, applying styles and attaching
/// event handlers for keyboard and mouse interaction.
///
/// # Arguments
/// * `state` - The table state containing columns, rows, and selection
/// * `style` - Visual styling configuration
/// * `_map_msg` - Function to map table messages to application messages (currently unused as mouse events are not yet supported)
///
/// # Example
/// ```
/// use chatui::components::data_table::{
///     data_table_view, DataTableState, DataTableStyle, DataTableMsg, SelectionMode
/// };
///
/// # enum AppMsg { TableMsg(DataTableMsg<u32>) }
/// let state = DataTableState::new(SelectionMode::Row);
/// let style = DataTableStyle::default();
/// let view = data_table_view(&state, &style, |msg| AppMsg::TableMsg(msg));
/// ```
pub fn data_table_view<Msg, Id>(
    state: &DataTableState<Id>,
    style: &DataTableStyle,
    _map_msg: impl Fn(DataTableMsg<Id>) -> Msg + 'static,
) -> Node<Msg>
where
    Id: Clone + PartialEq + 'static,
    Msg: 'static,
{
    if state.columns.is_empty() {
        return crate::dom::column(vec![]);
    }

    // Build columns for the low-level table
    let table_columns: Vec<TableColumn<Msg>> = state
        .columns
        .iter()
        .map(|col_def| {
            let mut table_col = TableColumn::new(col_def.width());

            if let Some(header_text) = col_def.header() {
                let header_spans = vec![TextSpan::new(header_text, style.header_style)];
                let header_node = rich_text::<Msg>(header_spans);

                // Note: Cannot add click handlers to text nodes directly
                // Header clicks would need to be handled at a higher level
                table_col = table_col.with_header(header_node);
            }

            table_col
        })
        .collect();

    // Build rows for the low-level table
    let table_rows: Vec<TableRow<Msg>> = state
        .rows
        .iter()
        .enumerate()
        .map(|(row_idx, row_data)| {
            let cells: Vec<Node<Msg>> = row_data
                .cells
                .iter()
                .enumerate()
                .map(|(col_idx, cell_spans)| {
                    // Determine cell style based on selection
                    let cell_style = get_cell_style(CellStyleContext {
                        mode: state.selection_mode,
                        row_idx,
                        col_idx,
                        selected_row: state.selected_row,
                        selected_cell: state.selected_cell,
                        style,
                        row_style_override: row_data.row_style.as_ref(),
                        is_alternate: row_idx % 2 == 1,
                    });

                    // Create cell with styled text
                    // Note: Cannot add mouse handlers to text nodes directly
                    // Mouse interaction would need to be handled at a higher level or via keyboard
                    rich_text::<Msg>(
                        cell_spans
                            .iter()
                            .map(|span| {
                                let mut span = span.clone();
                                span.style.apply_overlay(&cell_style);
                                span
                            })
                            .collect::<Vec<_>>(),
                    )
                    .with_style(cell_style)
                })
                .collect();

            TableRow::new(cells)
        })
        .collect();

    let mut table_node = table(table_columns, table_rows);

    if style.column_gap > 0 || style.row_gap > 0 {
        table_node = table_node.with_gap(style.column_gap, style.row_gap);
    }

    table_node
}

/// Helper struct to pass cell styling context.
struct CellStyleContext<'a> {
    mode: SelectionMode,
    row_idx: usize,
    col_idx: usize,
    selected_row: Option<usize>,
    selected_cell: Option<CellPosition>,
    style: &'a DataTableStyle,
    row_style_override: Option<&'a Style>,
    is_alternate: bool,
}

/// Determines the appropriate style for a cell based on selection state.
fn get_cell_style(ctx: CellStyleContext) -> Style {
    let mut base_style = match ctx.mode {
        SelectionMode::Row => {
            if Some(ctx.row_idx) == ctx.selected_row {
                ctx.style.selected_row_style
            } else if ctx.is_alternate {
                ctx.style
                    .alternate_row_style
                    .unwrap_or(ctx.style.row_style)
            } else {
                ctx.style.row_style
            }
        }
        SelectionMode::Cell => {
            if Some(CellPosition::new(ctx.row_idx, ctx.col_idx)) == ctx.selected_cell {
                ctx.style.selected_cell_style
            } else if ctx.is_alternate {
                ctx.style
                    .alternate_row_style
                    .unwrap_or(ctx.style.row_style)
            } else {
                ctx.style.row_style
            }
        }
    };

    // Apply row-specific style override if present
    if let Some(override_style) = ctx.row_style_override {
        base_style = base_style.merged(override_style);
    }

    base_style
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_columns() -> Vec<ColumnDef<u32>> {
        vec![
            ColumnDef::new(TableColumnWidth::Auto).with_header("Col1"),
            ColumnDef::new(TableColumnWidth::Auto).with_header("Col2"),
        ]
    }

    fn make_test_rows() -> Vec<RowData<u32>> {
        vec![
            RowData::new(
                1,
                vec![
                    vec![TextSpan::new("A", Style::default())],
                    vec![TextSpan::new("B", Style::default())],
                ],
            ),
            RowData::new(
                2,
                vec![
                    vec![TextSpan::new("C", Style::default())],
                    vec![TextSpan::new("D", Style::default())],
                ],
            ),
            RowData::new(
                3,
                vec![
                    vec![TextSpan::new("E", Style::default())],
                    vec![TextSpan::new("F", Style::default())],
                ],
            ),
        ]
    }

    #[test]
    fn new_state_has_no_selection() {
        let state = DataTableState::<u32>::new(SelectionMode::Row);
        assert_eq!(state.selection_mode(), SelectionMode::Row);
        assert_eq!(state.selected_row(), None);
        assert_eq!(state.selected_cell(), None);
        assert_eq!(state.row_count(), 0);
        assert_eq!(state.column_count(), 0);
    }

    #[test]
    fn set_columns_and_rows() {
        let mut state = DataTableState::new(SelectionMode::Row);
        let columns = make_test_columns();
        let rows = make_test_rows();

        state.set_columns(columns);
        state.set_rows(rows);

        assert_eq!(state.column_count(), 2);
        assert_eq!(state.row_count(), 3);
    }

    #[test]
    fn row_mode_select_next() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_rows(make_test_rows());

        assert_eq!(state.selected_row(), None);

        state.select_next_row();
        assert_eq!(state.selected_row(), Some(0));

        state.select_next_row();
        assert_eq!(state.selected_row(), Some(1));

        state.select_next_row();
        assert_eq!(state.selected_row(), Some(2));

        // Wraps to first
        state.select_next_row();
        assert_eq!(state.selected_row(), Some(0));
    }

    #[test]
    fn row_mode_select_prev() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_rows(make_test_rows());

        state.select_prev_row();
        assert_eq!(state.selected_row(), Some(2)); // Wraps to last

        state.select_prev_row();
        assert_eq!(state.selected_row(), Some(1));

        state.select_prev_row();
        assert_eq!(state.selected_row(), Some(0));

        state.select_prev_row();
        assert_eq!(state.selected_row(), Some(2)); // Wraps again
    }

    #[test]
    fn row_mode_select_first_last() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_rows(make_test_rows());

        state.select_last_row();
        assert_eq!(state.selected_row(), Some(2));

        state.select_first_row();
        assert_eq!(state.selected_row(), Some(0));
    }

    #[test]
    fn cell_mode_select_next_row() {
        let mut state = DataTableState::new(SelectionMode::Cell);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        state.select_cell(CellPosition::new(0, 1));
        assert_eq!(state.selected_cell(), Some(CellPosition::new(0, 1)));

        state.select_next_row();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(1, 1)));

        state.select_next_row();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(2, 1)));

        state.select_next_row();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(0, 1))); // Wraps
    }

    #[test]
    fn cell_mode_select_prev_row() {
        let mut state = DataTableState::new(SelectionMode::Cell);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        state.select_cell(CellPosition::new(1, 1));

        state.select_prev_row();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(0, 1)));

        state.select_prev_row();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(2, 1))); // Wraps to last
    }

    #[test]
    fn cell_mode_navigate_cells() {
        let mut state = DataTableState::new(SelectionMode::Cell);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        state.select_cell(CellPosition::new(0, 0));

        state.select_next_cell();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(0, 1)));

        state.select_next_cell();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(1, 0))); // Wraps to next row

        state.select_prev_cell();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(0, 1)));

        state.select_prev_cell();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(0, 0)));

        state.select_prev_cell();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(2, 1))); // Wraps to last cell
    }

    #[test]
    fn cell_mode_first_last_in_row() {
        let mut state = DataTableState::new(SelectionMode::Cell);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        state.select_cell(CellPosition::new(1, 0));

        state.select_last_cell_in_row();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(1, 1)));

        state.select_first_cell_in_row();
        assert_eq!(state.selected_cell(), Some(CellPosition::new(1, 0)));
    }

    #[test]
    fn validate_selection_after_removing_rows() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_rows(make_test_rows());

        state.select_last_row();
        assert_eq!(state.selected_row(), Some(2));

        // Remove last row
        state.set_rows(vec![RowData::new(
            1,
            vec![
                vec![TextSpan::new("A", Style::default())],
                vec![TextSpan::new("B", Style::default())],
            ],
        )]);

        assert_eq!(state.selected_row(), Some(0));
    }

    #[test]
    fn validate_selection_after_clearing_rows() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_rows(make_test_rows());
        state.select_first_row();

        state.set_rows(vec![]);
        assert_eq!(state.selected_row(), None);
    }

    #[test]
    fn cell_mode_validate_after_removing_columns() {
        let mut state = DataTableState::new(SelectionMode::Cell);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        state.select_cell(CellPosition::new(1, 1));

        // Remove last column
        state.set_columns(vec![
            ColumnDef::new(TableColumnWidth::Auto).with_header("Col1"),
        ]);

        assert_eq!(state.selected_cell(), Some(CellPosition::new(1, 0)));
    }

    #[test]
    fn row_at_returns_correct_data() {
        let mut state = DataTableState::new(SelectionMode::Row);
        let rows = make_test_rows();
        state.set_rows(rows);

        let row = state.row_at(1).unwrap();
        assert_eq!(row.id, 2);
        assert_eq!(row.cells[0][0].content, "C");
    }

    #[test]
    fn empty_table_navigation_does_nothing() {
        let mut state = DataTableState::<u32>::new(SelectionMode::Row);

        state.select_next_row();
        assert_eq!(state.selected_row(), None);

        state.select_prev_row();
        assert_eq!(state.selected_row(), None);
    }

    // Integration tests for rendering

    #[test]
    fn renders_empty_table() {
        let state = DataTableState::<u32>::new(SelectionMode::Row);
        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
        // If this compiles and runs without panic, the test passes
    }

    #[test]
    fn renders_table_with_headers() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_table_without_headers() {
        let mut state = DataTableState::new(SelectionMode::Row);
        let columns = vec![
            ColumnDef::new(TableColumnWidth::Auto),
            ColumnDef::new(TableColumnWidth::Auto),
        ];
        state.set_columns(columns);
        state.set_rows(make_test_rows());

        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_single_row_table() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_columns(make_test_columns());
        state.set_rows(vec![RowData::new(
            1,
            vec![
                vec![TextSpan::new("A", Style::default())],
                vec![TextSpan::new("B", Style::default())],
            ],
        )]);

        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_single_column_table() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_columns(vec![
            ColumnDef::new(TableColumnWidth::Auto).with_header("Col"),
        ]);
        state.set_rows(vec![
            RowData::new(1, vec![vec![TextSpan::new("A", Style::default())]]),
            RowData::new(2, vec![vec![TextSpan::new("B", Style::default())]]),
        ]);

        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_with_row_selection() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());
        state.select_row(1);

        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_with_cell_selection() {
        let mut state = DataTableState::new(SelectionMode::Cell);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());
        state.select_cell(CellPosition::new(1, 1));

        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_with_alternate_row_style() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        let mut style = DataTableStyle::default();
        style.alternate_row_style = Some(Style::bg(crate::dom::Color::rgba(30, 30, 40, 255)));
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_with_custom_row_style() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_columns(make_test_columns());

        let custom_style = Style::fg(crate::dom::Color::Red);
        let rows = vec![
            RowData::new(
                1,
                vec![
                    vec![TextSpan::new("A", Style::default())],
                    vec![TextSpan::new("B", Style::default())],
                ],
            )
            .with_style(custom_style),
        ];
        state.set_rows(rows);

        let style = DataTableStyle::default();
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn renders_with_gap_spacing() {
        let mut state = DataTableState::new(SelectionMode::Row);
        state.set_columns(make_test_columns());
        state.set_rows(make_test_rows());

        let mut style = DataTableStyle::default();
        style.column_gap = 2;
        style.row_gap = 1;
        let _view = data_table_view(&state, &style, |_msg| ());
    }

    #[test]
    fn column_def_accessors() {
        let col = ColumnDef::<u32>::new(TableColumnWidth::Fixed(10.0)).with_header("Test Header");

        assert_eq!(col.header(), Some("Test Header"));
        assert_eq!(col.width(), TableColumnWidth::Fixed(10.0));
    }

    #[test]
    fn cell_position_creation() {
        let pos = CellPosition::new(5, 3);
        assert_eq!(pos.row, 5);
        assert_eq!(pos.column, 3);
    }
}
