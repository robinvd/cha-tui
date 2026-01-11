//! Syntax highlighting for markdown code blocks.
//!
//! Provides a simple API for highlighting fenced code blocks
//! using tree-sitter grammars.

use std::time::Duration;

use color_eyre::eyre::{Result, eyre, WrapErr};
use once_cell::sync::Lazy;
use ropey::Rope;
use tree_house::Syntax;
use tree_house::highlighter::{HighlightEvent, Highlighter as TreeHouseHighlighter};

use crate::highlight::{TreeHouseLoader, SYNTAX_TIMEOUT_MILLIS};
use chatui::dom::{Style, TextSpan};

/// Highlight a code block given a language tag and content.
///
/// Returns `None` if:
/// - The language tag is empty
/// - The language is not supported
/// - The HELIX_RUNTIME environment variable is not set
/// - Grammar or query loading fails
///
/// Returns `Some(Vec<Vec<TextSpan>>)` where the outer vec is lines
/// and the inner vec contains styled spans for each line.
pub fn highlight_code_block(lang: &str, content: &str) -> Option<Vec<Vec<TextSpan>>> {
    let lang = lang.trim();
    if lang.is_empty() {
        return None;
    }

    let Some(language_name) = crate::highlight::canonical_language_from_token(lang) else {
        return None;
    };

    static REGISTRY: Lazy<HighlightRegistry> = Lazy::new(|| {
        HighlightRegistry::new().expect("failed to initialize highlight registry")
    });

    REGISTRY
        .highlight(language_name, content)
        .ok()
}

struct HighlightRegistry {
    loader: TreeHouseLoader,
}

impl HighlightRegistry {
    fn new() -> Result<Self> {
        Ok(Self {
            loader: TreeHouseLoader::new()?,
        })
    }

    fn highlight(&self, language_name: &str, source: &str) -> Result<Vec<Vec<TextSpan>>> {
        let language = self
            .loader
            .language_for_name(language_name)
            .wrap_err_with(|| format!("failed to resolve language '{language_name}'"))?;
        let palette = self.loader.palette();

        let rope = Rope::from_str(source);
        let slice = rope.slice(..);
        let timeout = Duration::from_millis(SYNTAX_TIMEOUT_MILLIS);
        let syntax = Syntax::new(slice, language, timeout, &self.loader)
            .map_err(|err| eyre!("failed to parse syntax for '{language_name}': {err}"))?;

        let byte_len = u32::try_from(source.len()).unwrap_or(u32::MAX);
        let mut highlighter = TreeHouseHighlighter::new(&syntax, slice, &self.loader, 0..byte_len);

        let mut lines: Vec<Vec<TextSpan>> = Vec::new();
        let mut current_line: Vec<TextSpan> = Vec::new();
        let mut style_stack: Vec<Style> = Vec::new();
        let mut position = 0usize;

        loop {
            let next_offset = highlighter.next_event_offset();
            if next_offset == u32::MAX {
                break;
            }

            let next = next_offset as usize;
            if next > position && next <= source.len() {
                let fragment = &source[position..next];
                let style = style_stack.last().cloned().unwrap_or_else(Style::default);
                append_fragment(&mut lines, &mut current_line, fragment, &style);
                position = next;
            }

            let (event, highlights) = highlighter.advance();
            match event {
                HighlightEvent::Refresh => {
                    style_stack.clear();
                    for highlight in highlights {
                        style_stack.push(palette.style_for(highlight));
                    }
                }
                HighlightEvent::Push => {
                    for highlight in highlights {
                        style_stack.push(palette.style_for(highlight));
                    }
                }
            }
        }

        if position < source.len() {
            let fragment = &source[position..];
            let style = style_stack.last().cloned().unwrap_or_else(Style::default);
            append_fragment(&mut lines, &mut current_line, fragment, &style);
        }

        if !current_line.is_empty() {
            lines.push(current_line);
        }

        Ok(lines)
    }
}

fn append_fragment(
    lines: &mut Vec<Vec<TextSpan>>,
    current_line: &mut Vec<TextSpan>,
    fragment: &str,
    style: &Style,
) {
    if fragment.is_empty() {
        return;
    }

    let mut start = 0;
    for (idx, ch) in fragment.char_indices() {
        if ch == '\n' {
            let segment = &fragment[start..idx];
            push_segment(current_line, segment, style);
            lines.push(std::mem::take(current_line));
            start = idx + ch.len_utf8();
        }
    }

    let remaining = &fragment[start..];
    push_segment(current_line, remaining, style);
}

fn push_segment(line: &mut Vec<TextSpan>, segment: &str, style: &Style) {
    if segment.is_empty() {
        return;
    }
    if let Some(last) = line.last_mut()
        && last.style == *style
    {
        last.content.push_str(segment);
        return;
    }
    line.push(TextSpan::new(segment, style.clone()));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unknown_language_returns_none() {
        let result = highlight_code_block("not_a_real_language", "some code");
        assert!(result.is_none());
    }

    #[test]
    fn test_empty_language_returns_none() {
        let result = highlight_code_block("", "some code");
        assert!(result.is_none());
    }

    #[test]
    fn test_empty_content_returns_empty_lines() {
        let result = highlight_code_block("rust", "");
        // Empty content should either return None or an empty vec
        if let Some(lines) = result {
            assert!(lines.is_empty() || (lines.len() == 1 && lines[0].is_empty()));
        }
    }
}
