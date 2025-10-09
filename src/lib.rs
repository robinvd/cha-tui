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

// Re-export termina's OneBased type
pub use termina::OneBased;

pub use crate::dom::{
    Attributes, ElementKind, ElementNode, Node, SizePolicy, Style, TextNode, TextSpan, block,
    block_with_title, column, modal, rich_text, row, text,
};
pub use crate::error::ProgramError;
pub use crate::event::{Event, Key, KeyCode, MouseButtons, MouseEvent, Size};
pub use crate::geometry::{Point, Rect};
pub use crate::program::{EventFn, Program, Transition, UpdateFn, ViewFn};
pub use components::input::{
    InputMode, InputMsg, InputState, InputStyle, default_keybindings as default_input_keybindings,
    input,
};
pub use components::scroll::{ScrollMsg, ScrollState, scrollable_content};
pub use components::tree::{TreeMsg, TreeNode, TreeNodeKind, TreeState, TreeStyle, tree_view};
pub use scroll::ScrollAlignment;
