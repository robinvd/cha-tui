//! Syntax highlighting using tree-sitter.
//!
//! This crate provides syntax highlighting functionality for code
//! using the `tree-house` library. It is shared between `text_editor`
//! and `chatui-markdown` crates.

pub mod highlight;
pub mod markdown;
pub mod runtime;

// Re-export commonly used items
pub use highlight::{DocumentHighlighter, language_for_path};
pub use markdown::highlight_code_block;
