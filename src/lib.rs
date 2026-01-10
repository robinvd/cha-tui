pub mod buffer;
pub mod components;
pub mod dom;
pub mod error;
pub mod event;
pub mod geometry;
mod hash;
pub mod oklab;
pub mod palette;
pub mod program;
pub mod render;
pub mod scroll;
pub mod test_utils;

// Re-export termina's OneBased type
pub use termina::OneBased;

pub use crate::dom::{
    Attributes,
    ElementKind,
    ElementNode,
    Node,
    RetainedNode,
    SizePolicy,
    Style,
    TableColumn,
    TableColumnWidth,
    TableRow,
    TextNode,
    TextNodeRef,
    TextSpan,
    TextSpanRef,
    // Primary borrowed node helpers (these return Node with lifetimes)
    block,
    // Retained node helpers (for when you need owned nodes without lifetimes)
    block_retained,
    block_with_title,
    block_with_title_retained,
    column,
    column_retained,
    modal,
    modal_retained,
    renderable,
    rich_text,
    rich_text_owned,
    rich_text_retained,
    row,
    row_retained,
    table,
    text,
    text_owned,
    text_retained,
};
pub use crate::error::ProgramError;
pub use crate::event::{
    Event, Key, KeyCode, LocalMouseEvent, MouseButton, MouseEvent, MouseEventKind, MouseModifiers,
    MousePosition, MouseScroll, MouseScrollAxis, MouseScrollDirection, Size,
};
pub use crate::geometry::{Point, Rect};
pub use crate::program::{
    EventFn, Program, RetainedNode as DomRetainedNode, Transition, UpdateFn, ViewFn,
};
pub use components::input::{
    HighlightLayerId, HighlightSpan, InputMode, InputMsg, InputState, InputStyle,
    default_keybindings as default_input_keybindings, input,
};
pub use components::paragraph::{Paragraph, paragraph, rich_paragraph, rich_paragraph_ref};
pub use components::scroll::{
    ScrollAxis, ScrollMsg, ScrollState, ScrollTarget, scrollable_content,
};
pub use components::terminal::{
    TermMode, TerminalMsg, TerminalNotification, TerminalState, default_terminal_keybindings,
    encode_mouse_event, key_to_input, terminal,
};
pub use components::tree::{TreeMsg, TreeNode, TreeNodeKind, TreeState, TreeStyle, tree_view};
pub use components::virtual_list::{
    VirtualListAction, VirtualListEvent, VirtualListState,
    default_keybindings as default_virtual_list_keybindings, virtual_list,
};
pub use components::virtualized_column::{VirtualizedColumn, virtualized_column};
pub use scroll::ScrollAlignment;
