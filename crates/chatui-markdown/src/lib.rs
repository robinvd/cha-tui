//! Markdown rendering for `chatui`.
//!
//! This crate parses Markdown using [`pulldown_cmark`] and compiles it into a `chatui::Node`
//! tree.

mod code_block;
mod parse;
mod view;

pub use parse::{MarkdownDocument, MarkdownParseOptions};
pub use view::markdown_view;

use chatui::components::scroll::{ScrollMsg, ScrollState};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CodeBlockId(pub usize);

#[non_exhaustive]
#[derive(Clone, Debug, PartialEq)]
pub enum MarkdownMsg {
    DocScroll(ScrollMsg),
    CodeBlockScroll { id: CodeBlockId, msg: ScrollMsg },
}

#[derive(Debug, Clone)]
pub struct MarkdownState {
    doc_scroll: ScrollState,
    code_scroll: Vec<ScrollState>,
}

impl Default for MarkdownState {
    fn default() -> Self {
        Self::new()
    }
}

impl MarkdownState {
    pub fn new() -> Self {
        Self {
            doc_scroll: ScrollState::vertical(),
            code_scroll: Vec::new(),
        }
    }

    pub fn update(&mut self, msg: MarkdownMsg) {
        match msg {
            MarkdownMsg::DocScroll(msg) => self.doc_scroll.update(msg),
            MarkdownMsg::CodeBlockScroll { id, msg } => {
                if let Some(state) = self.code_scroll.get_mut(id.0) {
                    state.update(msg);
                }
            }
        }
    }

    pub fn sync_with(&mut self, doc: &MarkdownDocument) {
        let count = doc.code_block_count();
        if self.code_scroll.len() != count {
            self.code_scroll.resize_with(count, ScrollState::horizontal);
        }
    }

    pub fn doc_scroll(&self) -> &ScrollState {
        &self.doc_scroll
    }

    pub fn code_scroll(&self, id: CodeBlockId) -> Option<&ScrollState> {
        self.code_scroll.get(id.0)
    }
}

#[cfg(test)]
mod tests;
