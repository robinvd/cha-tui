use super::{
    ElementKind, RetainedElementNode as ElementNode, RetainedNode,
    RetainedNodeContent as NodeContent, text_retained as text,
};
use taffy::style::{
    AlignContent, CoreStyle, Dimension, GridTemplateComponent, Style as TaffyStyle,
    TrackSizingFunction,
};

// Type alias for convenience - in this file we're working with owned (retained) nodes
type Node<Msg> = RetainedNode<Msg>;
use taffy::style_helpers::{flex, length, line, percent, span};

type GridIdent = <TaffyStyle as CoreStyle>::CustomIdent;
type GridComponent = GridTemplateComponent<GridIdent>;

/// A column definition for a table.
///
/// Specifies the width constraint and optional header content for a single column.
/// Use [`TableColumn::new`] to create a column with a width, then optionally add a
/// header with [`TableColumn::with_header`].
///
/// # Example
/// ```
/// use chatui::{TableColumn, TableColumnWidth, text};
///
/// let column = TableColumn::new(TableColumnWidth::Auto)
///     .with_header(text::<()>("Name"));
/// ```
#[derive(Debug)]
pub struct TableColumn<Msg> {
    header: Option<Node<Msg>>,
    width: TableColumnWidth,
}

impl<Msg> TableColumn<Msg> {
    /// Creates a new column with the specified width constraint.
    ///
    /// # Example
    /// ```
    /// use chatui::{TableColumn, TableColumnWidth};
    ///
    /// let auto_column: TableColumn<()> = TableColumn::new(TableColumnWidth::Auto);
    /// let fixed_column: TableColumn<()> = TableColumn::new(TableColumnWidth::Fixed(100.0));
    /// ```
    pub fn new(width: TableColumnWidth) -> Self {
        Self {
            header: None,
            width,
        }
    }

    /// Adds a header node to this column.
    ///
    /// The header will be rendered in the first row of the table if any column
    /// has a header. If no columns have headers, the header row is omitted.
    ///
    /// # Example
    /// ```
    /// use chatui::{TableColumn, TableColumnWidth, text};
    ///
    /// let column = TableColumn::new(TableColumnWidth::Auto)
    ///     .with_header(text::<()>("Column Name"));
    /// ```
    pub fn with_header(mut self, header: Node<Msg>) -> Self {
        self.header = Some(header);
        self
    }

    fn into_parts(self) -> (Option<Node<Msg>>, GridComponent) {
        let track = self.width.to_track();
        (self.header, track)
    }
}

/// A row of cells in a table.
///
/// Contains the cell nodes for a single row. The number of cells must match
/// the number of columns defined in the table, or [`table`] will panic.
///
/// # Example
/// ```
/// use chatui::{TableRow, text};
///
/// let row = TableRow::new(vec![
///     text::<()>("Cell 1"),
///     text::<()>("Cell 2"),
/// ]);
/// ```
#[derive(Debug)]
pub struct TableRow<Msg> {
    cells: Vec<Node<Msg>>,
}

impl<Msg> TableRow<Msg> {
    /// Creates a new row with the specified cells.
    ///
    /// The number of cells must match the number of columns in the table.
    ///
    /// # Example
    /// ```
    /// use chatui::{TableRow, text};
    ///
    /// let row = TableRow::new(vec![
    ///     text::<()>("Alice"),
    ///     text::<()>("Engineer"),
    /// ]);
    /// ```
    pub fn new(cells: Vec<Node<Msg>>) -> Self {
        Self { cells }
    }

    fn len(&self) -> usize {
        self.cells.len()
    }

    fn into_cells(self) -> Vec<Node<Msg>> {
        self.cells
    }
}

/// Width constraint for a table column.
///
/// Defines how a column should be sized within the table layout.
/// Uses CSS Grid track sizing semantics.
///
/// # Variants
/// - [`Auto`](TableColumnWidth::Auto): Column sizes to fit its content
/// - [`Fixed`](TableColumnWidth::Fixed): Column has a fixed width in terminal cells
/// - [`Percent`](TableColumnWidth::Percent): Column takes a percentage of available space
/// - [`Flexible`](TableColumnWidth::Flexible): Column grows proportionally (fr units in CSS Grid)
///
/// # Example
/// ```
/// use chatui::{TableColumn, TableColumnWidth};
///
/// let auto: TableColumn<()> = TableColumn::new(TableColumnWidth::Auto);
/// let fixed: TableColumn<()> = TableColumn::new(TableColumnWidth::Fixed(20.0));
/// let percent: TableColumn<()> = TableColumn::new(TableColumnWidth::Percent(50.0));
/// let flex: TableColumn<()> = TableColumn::new(TableColumnWidth::Flexible(2.0));
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TableColumnWidth {
    /// Column sizes automatically to fit its content.
    Auto,
    /// Column has a fixed width in terminal cells.
    Fixed(f32),
    /// Column takes a percentage of the available space.
    Percent(f32),
    /// Column grows proportionally with the specified weight (CSS Grid fr units).
    Flexible(f32),
}

impl TableColumnWidth {
    fn to_track(self) -> GridComponent {
        match self {
            TableColumnWidth::Auto => {
                GridComponent::Single(TrackSizingFunction::from(Dimension::auto()))
            }
            TableColumnWidth::Fixed(width) => length::<_, GridComponent>(width),
            TableColumnWidth::Percent(fraction) => percent::<_, GridComponent>(fraction),
            TableColumnWidth::Flexible(weight) => flex::<_, GridComponent>(weight),
        }
    }
}

/// Creates a grid-based table layout with columns and rows.
///
/// Constructs a table using CSS Grid layout, with optional headers and data rows.
/// Each cell is positioned in the grid according to its row and column index.
///
/// # Arguments
/// * `columns` - Column definitions specifying width constraints and optional headers
/// * `rows` - Data rows, each containing cells that must match the column count
///
/// # Panics
/// * If `columns` is empty
/// * If any row has a different number of cells than the number of columns
///
/// # Example
/// ```
/// use chatui::{table, TableColumn, TableColumnWidth, TableRow, text};
///
/// let columns = vec![
///     TableColumn::new(TableColumnWidth::Auto)
///         .with_header(text::<()>("Name")),
///     TableColumn::new(TableColumnWidth::Flexible(1.0))
///         .with_header(text::<()>("Role")),
/// ];
///
/// let rows = vec![
///     TableRow::new(vec![
///         text::<()>("Alice"),
///         text::<()>("Engineer"),
///     ]),
///     TableRow::new(vec![
///         text::<()>("Bob"),
///         text::<()>("Designer"),
///     ]),
/// ];
///
/// let table_node = table(columns, rows);
/// ```
///
/// # Header Behavior
/// If any column has a header, all columns are rendered in a header row.
/// Columns without headers render an empty cell in the header row.
/// If no columns have headers, the header row is omitted entirely.
///
/// # Layout
/// The table uses CSS Grid with each cell explicitly positioned using grid
/// line numbers. Cells can be styled individually by wrapping content in
/// styled nodes before passing to [`TableRow::new`].
pub fn table<Msg>(columns: Vec<TableColumn<Msg>>, rows: Vec<TableRow<Msg>>) -> Node<Msg> {
    assert!(
        !columns.is_empty(),
        "table requires at least one column definition"
    );

    let row_count = rows.len();

    let mut column_components = Vec::with_capacity(columns.len());
    let mut header_cells = Vec::with_capacity(columns.len());
    for column in columns {
        let (header, track) = column.into_parts();
        header_cells.push(header);
        column_components.push(track);
    }

    let column_count = column_components.len();
    let has_header = header_cells.iter().any(|cell| cell.is_some());
    let header_rows = if has_header { 1usize } else { 0 };
    let mut children = Vec::with_capacity(column_count * (row_count.saturating_add(header_rows)));

    let mut current_row_index: u16 = 1;
    if has_header {
        for (idx, cell) in header_cells.into_iter().enumerate() {
            let node = cell.unwrap_or_else(|| text::<Msg>(""));
            children.push(place_cell(node, current_row_index, (idx + 1) as u16));
        }
        current_row_index += 1;
    }

    for row in rows {
        if row.len() != column_count {
            panic!(
                "table row has {} cells but table defines {} columns",
                row.len(),
                column_count
            );
        }

        for (idx, cell) in row.into_cells().into_iter().enumerate() {
            children.push(place_cell(cell, current_row_index, (idx + 1) as u16));
        }
        current_row_index += 1;
    }

    let element = ElementNode::new(ElementKind::Table, children);
    let mut node = Node::new(NodeContent::Element(element));
    node.layout_state.style.display = taffy::style::Display::Grid;
    node.layout_state.style.grid_template_columns = column_components;
    node.layout_state.style.align_content = Some(AlignContent::Start);
    node
}

fn place_cell<Msg>(mut node: Node<Msg>, row: u16, column: u16) -> Node<Msg> {
    node.layout_state.style.grid_row.start = line(row as i16);
    node.layout_state.style.grid_row.end = span(1);
    node.layout_state.style.grid_column.start = line(column as i16);
    node.layout_state.style.grid_column.end = span(1);
    node
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dom::{self, ElementKind};
    use taffy::style::Display;
    use taffy::style_helpers::line;

    #[test]
    fn builds_basic_table() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto).with_header(dom::text::<()>("Name").into()),
            TableColumn::new(TableColumnWidth::Flexible(1.0))
                .with_header(dom::text::<()>("Role").into()),
        ];
        let rows = vec![
            TableRow::new(vec![
                dom::text::<()>("Alice").into(),
                dom::text::<()>("Engineer").into(),
            ]),
            TableRow::new(vec![
                dom::text::<()>("Bob").into(),
                dom::text::<()>("Designer").into(),
            ]),
        ];

        let table_node: RetainedNode<()> = table(columns, rows).into();

        let element = table_node.as_element().expect("element");
        assert_eq!(element.kind, ElementKind::Table);
        assert_eq!(element.children.len(), 6);
        assert_eq!(table_node.layout_state().style.display, Display::Grid);
        assert_eq!(
            table_node.layout_state().style.grid_template_columns.len(),
            2
        );

        let first_child = &element.children[0];
        let style = &first_child.layout_state().style;
        assert_eq!(style.grid_row.start, line(1));
        assert_eq!(style.grid_column.start, line(1));
    }

    #[test]
    #[should_panic(expected = "table row has 1 cells but table defines 2 columns")]
    fn mismatched_row_panics() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto),
            TableColumn::new(TableColumnWidth::Auto),
        ];
        let rows = vec![TableRow::new(vec![dom::text::<()>("Only one cell").into()])];

        let _ = table(columns, rows);
    }

    #[test]
    fn omits_header_when_empty() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto),
            TableColumn::new(TableColumnWidth::Flexible(1.0)),
        ];
        let rows = vec![TableRow::new(vec![
            dom::text::<()>("Left").into(),
            dom::text::<()>("Right").into(),
        ])];

        let table_node = table(columns, rows);
        let element = table_node.as_element().expect("element");
        assert_eq!(element.children.len(), 2);
        assert_eq!(
            table_node.layout_state().style.grid_template_columns.len(),
            2
        );
    }

    #[test]
    fn mixed_column_widths() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Fixed(10.0))
                .with_header(dom::text::<()>("ID").into()),
            TableColumn::new(TableColumnWidth::Auto).with_header(dom::text::<()>("Name").into()),
            TableColumn::new(TableColumnWidth::Flexible(2.0))
                .with_header(dom::text::<()>("Description").into()),
            TableColumn::new(TableColumnWidth::Percent(15.0))
                .with_header(dom::text::<()>("Status").into()),
        ];
        let rows = vec![TableRow::new(vec![
            dom::text::<()>("1").into(),
            dom::text::<()>("Item").into(),
            dom::text::<()>("A long description").into(),
            dom::text::<()>("OK").into(),
        ])];

        let table_node = table(columns, rows);
        let element = table_node.as_element().expect("element");
        assert_eq!(element.children.len(), 8); // 4 header + 4 data cells
        assert_eq!(
            table_node.layout_state().style.grid_template_columns.len(),
            4
        );

        // Verify first data cell is positioned correctly
        let first_data_cell = &element.children[4];
        assert_eq!(first_data_cell.layout_state().style.grid_row.start, line(2));
        assert_eq!(
            first_data_cell.layout_state().style.grid_column.start,
            line(1)
        );
    }

    #[test]
    fn empty_table_with_only_headers() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto)
                .with_header(dom::text::<()>("Column 1").into()),
            TableColumn::new(TableColumnWidth::Auto)
                .with_header(dom::text::<()>("Column 2").into()),
        ];
        let rows = vec![];

        let table_node = table(columns, rows);
        let element = table_node.as_element().expect("element");
        assert_eq!(element.children.len(), 2); // Only header cells
        assert_eq!(
            table_node.layout_state().style.grid_template_columns.len(),
            2
        );

        // Verify headers are in first row
        assert_eq!(
            element.children[0].layout_state().style.grid_row.start,
            line(1)
        );
        assert_eq!(
            element.children[1].layout_state().style.grid_row.start,
            line(1)
        );
    }

    #[test]
    fn single_cell_table() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto).with_header(dom::text::<()>("Header").into()),
        ];
        let rows = vec![TableRow::new(vec![dom::text::<()>("Cell").into()])];

        let table_node = table(columns, rows);
        let element = table_node.as_element().expect("element");
        assert_eq!(element.children.len(), 2); // 1 header + 1 data cell
        assert_eq!(
            table_node.layout_state().style.grid_template_columns.len(),
            1
        );

        let data_cell = &element.children[1];
        assert_eq!(data_cell.layout_state().style.grid_row.start, line(2));
        assert_eq!(data_cell.layout_state().style.grid_column.start, line(1));
    }

    #[test]
    fn multiple_rows_with_different_content() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto),
            TableColumn::new(TableColumnWidth::Auto),
        ];
        let rows = vec![
            TableRow::new(vec![
                dom::text::<()>("R1C1").into(),
                dom::text::<()>("R1C2").into(),
            ]),
            TableRow::new(vec![
                dom::text::<()>("R2C1").into(),
                dom::text::<()>("R2C2").into(),
            ]),
            TableRow::new(vec![
                dom::text::<()>("R3C1").into(),
                dom::text::<()>("R3C2").into(),
            ]),
        ];

        let table_node = table(columns, rows);
        let element = table_node.as_element().expect("element");
        assert_eq!(element.children.len(), 6); // No headers, 3 rows × 2 cols

        // Verify third row positioning
        let third_row_first_cell = &element.children[4];
        assert_eq!(
            third_row_first_cell.layout_state().style.grid_row.start,
            line(3)
        );
        assert_eq!(
            third_row_first_cell.layout_state().style.grid_column.start,
            line(1)
        );
    }

    #[test]
    #[should_panic(expected = "table requires at least one column definition")]
    fn empty_columns_panics() {
        let columns = vec![];
        let rows = vec![];
        let _ = table::<()>(columns, rows);
    }
}
