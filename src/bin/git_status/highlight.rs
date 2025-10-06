use std::path::Path;

use once_cell::sync::Lazy;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter};

use chatui::dom::{Color, Style, TextSpan};

use color_eyre::eyre::Result;

#[derive(Clone, Copy)]
pub enum LanguageKind {
    Rust,
    Markdown,
    Python,
    Go,
}

const HIGHLIGHT_NAMES: &[&str] = &[
    "attribute",
    "boolean",
    "comment",
    "comment.documentation",
    "constant",
    "constant.builtin",
    "constructor",
    "constructor.builtin",
    "embedded",
    "escape",
    "function",
    "function.builtin",
    "function.macro",
    "keyword",
    "keyword.control",
    "keyword.function",
    "keyword.operator",
    "keyword.storage",
    "label",
    "markup",
    "markup.bold",
    "markup.heading",
    "markup.italic",
    "markup.link",
    "markup.link.url",
    "markup.list",
    "markup.list.checked",
    "markup.list.numbered",
    "markup.list.unchecked",
    "markup.quote",
    "markup.raw",
    "markup.raw.block",
    "markup.raw.inline",
    "markup.strikethrough",
    "module",
    "number",
    "namespace",
    "operator",
    "property",
    "property.builtin",
    "punctuation",
    "punctuation.bracket",
    "punctuation.delimiter",
    "punctuation.special",
    "string",
    "string.escape",
    "string.regexp",
    "string.special",
    "string.special.symbol",
    "special",
    "tag",
    "type",
    "type.builtin",
    "variable",
    "variable.builtin",
    "variable.member",
    "variable.parameter",
];

static REGISTRY: Lazy<HighlightRegistry> =
    Lazy::new(|| HighlightRegistry::new().expect("failed to initialize highlight registry"));

const RUST_HIGHLIGHTS_QUERY: &str = include_str!("queries/rust_highlights.scm");
const RUST_INJECTIONS_QUERY: &str = include_str!("queries/rust_injections.scm");
const RUST_LOCALS_QUERY: &str = include_str!("queries/rust_locals.scm");
const MARKDOWN_HIGHLIGHTS_QUERY: &str = include_str!("queries/markdown_highlights.scm");
const MARKDOWN_INJECTIONS_QUERY: &str = include_str!("queries/markdown_injections.scm");
const MARKDOWN_INLINE_HIGHLIGHTS_QUERY: &str =
    include_str!("queries/markdown_inline_highlights.scm");
const MARKDOWN_INLINE_INJECTIONS_QUERY: &str =
    include_str!("queries/markdown_inline_injections.scm");

// External language queries sourced from Helix runtime
const PYTHON_HIGHLIGHTS_QUERY: &str = include_str!(
    "/nix/store/f12s6mabm2g8b2fw7k3b5zl9id4qcpsw-helix-runtime/queries/python/highlights.scm"
);
const PYTHON_INJECTIONS_QUERY: &str = include_str!(
    "/nix/store/f12s6mabm2g8b2fw7k3b5zl9id4qcpsw-helix-runtime/queries/python/injections.scm"
);
const PYTHON_LOCALS_QUERY: &str = include_str!(
    "/nix/store/f12s6mabm2g8b2fw7k3b5zl9id4qcpsw-helix-runtime/queries/python/locals.scm"
);

const GO_HIGHLIGHTS_QUERY: &str = include_str!(
    "/nix/store/f12s6mabm2g8b2fw7k3b5zl9id4qcpsw-helix-runtime/queries/go/highlights.scm"
);
const GO_INJECTIONS_QUERY: &str = include_str!(
    "/nix/store/f12s6mabm2g8b2fw7k3b5zl9id4qcpsw-helix-runtime/queries/go/injections.scm"
);
const GO_LOCALS_QUERY: &str =
    include_str!("/nix/store/f12s6mabm2g8b2fw7k3b5zl9id4qcpsw-helix-runtime/queries/go/locals.scm");

pub(crate) const EVERFOREST_FG: Color = Color::rgb(0xd3, 0xc6, 0xaa);
pub(crate) const EVERFOREST_RED: Color = Color::rgb(0xe6, 0x7e, 0x80);
pub(crate) const EVERFOREST_ORANGE: Color = Color::rgb(0xe6, 0x98, 0x75);
pub(crate) const EVERFOREST_YELLOW: Color = Color::rgb(0xdb, 0xbc, 0x7f);
pub(crate) const EVERFOREST_GREEN: Color = Color::rgb(0xa7, 0xc0, 0x80);
pub(crate) const EVERFOREST_AQUA: Color = Color::rgb(0x83, 0xc0, 0x92);
pub(crate) const EVERFOREST_BLUE: Color = Color::rgb(0x7f, 0xbb, 0xb3);
pub(crate) const EVERFOREST_PURPLE: Color = Color::rgb(0xd6, 0x99, 0xb6);
pub(crate) const EVERFOREST_GREY1: Color = Color::rgb(0x85, 0x92, 0x89);
pub(crate) const EVERFOREST_GREY2: Color = Color::rgb(0x9d, 0xa9, 0xa0);
pub(crate) const EVERFOREST_BG_GREEN: Color = Color::rgb(0x42, 0x50, 0x47);
pub(crate) const EVERFOREST_BG_RED: Color = Color::rgb(0x51, 0x40, 0x45);

pub fn language_for_path(path: &Path) -> Option<LanguageKind> {
    let ext = path.extension()?.to_str()?.to_ascii_lowercase();
    match ext.as_str() {
        "rs" => Some(LanguageKind::Rust),
        "py" => Some(LanguageKind::Python),
        "go" => Some(LanguageKind::Go),
        "md" | "markdown" | "mdown" | "mkd" => Some(LanguageKind::Markdown),
        _ => None,
    }
}

pub fn highlight_lines(path: &Path, source: &str) -> Option<Vec<Vec<TextSpan>>> {
    let language = language_for_path(path)?;
    REGISTRY.highlight(language, source).ok()
}

struct HighlightRegistry {
    rust: HighlightConfiguration,
    markdown: HighlightConfiguration,
    markdown_inline: HighlightConfiguration,
    python: HighlightConfiguration,
    go: HighlightConfiguration,
    styles: Vec<Style>,
}

impl HighlightRegistry {
    fn new() -> Result<Self> {
        let mut rust = HighlightConfiguration::new(
            tree_sitter_rust::LANGUAGE.into(),
            "rust",
            RUST_HIGHLIGHTS_QUERY,
            RUST_INJECTIONS_QUERY,
            RUST_LOCALS_QUERY,
        )?;
        rust.configure(HIGHLIGHT_NAMES);

        let mut markdown = HighlightConfiguration::new(
            tree_sitter_md::LANGUAGE.into(),
            "markdown",
            MARKDOWN_HIGHLIGHTS_QUERY,
            MARKDOWN_INJECTIONS_QUERY,
            "",
        )?;
        markdown.configure(HIGHLIGHT_NAMES);

        let mut markdown_inline = HighlightConfiguration::new(
            tree_sitter_md::INLINE_LANGUAGE.into(),
            "markdown_inline",
            MARKDOWN_INLINE_HIGHLIGHTS_QUERY,
            MARKDOWN_INLINE_INJECTIONS_QUERY,
            "",
        )?;
        markdown_inline.configure(HIGHLIGHT_NAMES);

        let mut python = HighlightConfiguration::new(
            tree_sitter_python::LANGUAGE.into(),
            "python",
            PYTHON_HIGHLIGHTS_QUERY,
            PYTHON_INJECTIONS_QUERY,
            PYTHON_LOCALS_QUERY,
        )?;
        python.configure(HIGHLIGHT_NAMES);

        let mut go = HighlightConfiguration::new(
            tree_sitter_go::LANGUAGE.into(),
            "go",
            GO_HIGHLIGHTS_QUERY,
            GO_INJECTIONS_QUERY,
            GO_LOCALS_QUERY,
        )?;
        go.configure(HIGHLIGHT_NAMES);

        let styles = HIGHLIGHT_NAMES.iter().map(|name| style_for(name)).collect();

        Ok(Self {
            rust,
            markdown,
            markdown_inline,
            python,
            go,
            styles,
        })
    }

    fn highlight(&self, language: LanguageKind, source: &str) -> Result<Vec<Vec<TextSpan>>> {
        let mut highlighter = Highlighter::new();
        match language {
            LanguageKind::Rust => {
                let iterator =
                    highlighter.highlight(&self.rust, source.as_bytes(), None, |_| None)?;
                events_to_lines(iterator, source, &self.styles)
            }
            LanguageKind::Python => {
                let iterator =
                    highlighter.highlight(&self.python, source.as_bytes(), None, |_| None)?;
                events_to_lines(iterator, source, &self.styles)
            }
            LanguageKind::Go => {
                let iterator =
                    highlighter.highlight(&self.go, source.as_bytes(), None, |_| None)?;
                events_to_lines(iterator, source, &self.styles)
            }
            LanguageKind::Markdown => {
                let iterator =
                    highlighter.highlight(&self.markdown, source.as_bytes(), None, |name| {
                        if name == "markdown_inline" {
                            Some(&self.markdown_inline)
                        } else {
                            None
                        }
                    })?;
                events_to_lines(iterator, source, &self.styles)
            }
        }
    }
}

fn events_to_lines<I>(iter: I, source: &str, styles: &[Style]) -> Result<Vec<Vec<TextSpan>>>
where
    I: Iterator<Item = Result<HighlightEvent, tree_sitter_highlight::Error>>,
{
    let mut lines: Vec<Vec<TextSpan>> = Vec::new();
    let mut current_line: Vec<TextSpan> = Vec::new();
    let mut style_stack: Vec<Style> = Vec::new();

    for event in iter {
        match event? {
            HighlightEvent::HighlightStart(highlight) => {
                let style = styles.get(highlight.0).cloned().unwrap_or_default();
                style_stack.push(style);
            }
            HighlightEvent::HighlightEnd => {
                style_stack.pop();
            }
            HighlightEvent::Source { start, end } => {
                if start == end {
                    continue;
                }
                let fragment = &source[start..end];
                let style = style_stack.last().cloned().unwrap_or_else(Style::default);

                let mut fragment_start = 0;
                for (idx, ch) in fragment.char_indices() {
                    if ch == '\n' {
                        let segment = &fragment[fragment_start..idx];
                        push_segment(&mut current_line, segment, &style);
                        lines.push(std::mem::take(&mut current_line));
                        fragment_start = idx + ch.len_utf8();
                    }
                }

                let remaining = &fragment[fragment_start..];
                push_segment(&mut current_line, remaining, &style);
            }
        }
    }

    if !current_line.is_empty() {
        lines.push(current_line);
    }

    Ok(lines)
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

fn style_for(name: &str) -> Style {
    fn color_style(color: Color) -> Style {
        Style {
            fg: Some(color),
            ..Style::default()
        }
    }

    fn bold_color_style(color: Color) -> Style {
        Style {
            fg: Some(color),
            bold: true,
            ..Style::default()
        }
    }

    if name.starts_with("comment") {
        return color_style(EVERFOREST_GREY1);
    }

    if name.starts_with("constant.character.escape") || name == "escape" {
        return color_style(EVERFOREST_GREEN);
    }

    if name.starts_with("constant.builtin") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("constant.numeric") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("string.regexp") {
        return color_style(EVERFOREST_GREEN);
    }

    if name.starts_with("string.escape") {
        return color_style(EVERFOREST_GREEN);
    }

    if name.starts_with("string.special") {
        return color_style(EVERFOREST_YELLOW);
    }

    if name.starts_with("string") {
        return color_style(EVERFOREST_AQUA);
    }

    if name.starts_with("variable.builtin") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("variable.other.member") || name.starts_with("variable.member") {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "label" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name == "namespace" || name == "module" {
        return color_style(EVERFOREST_YELLOW);
    }

    if name.starts_with("keyword.operator") || name == "operator" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name.starts_with("keyword.directive") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("keyword") {
        return color_style(EVERFOREST_RED);
    }

    if name.starts_with("function") || name.starts_with("constructor") {
        return color_style(EVERFOREST_GREEN);
    }

    if name == "attribute" {
        return color_style(EVERFOREST_PURPLE);
    }

    if name == "tag" || name == "special" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name.starts_with("type") {
        return color_style(EVERFOREST_YELLOW);
    }

    if name.starts_with("boolean") || name.starts_with("number") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("property") {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "punctuation.special" {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "punctuation.delimiter" {
        return color_style(EVERFOREST_GREY1);
    }

    if name == "punctuation.bracket" {
        return color_style(EVERFOREST_FG);
    }

    if name.starts_with("punctuation") {
        return color_style(EVERFOREST_GREY2);
    }

    if name.starts_with("markup.heading.") {
        let level = name.rsplit('.').next().unwrap_or("");
        return match level {
            "1" => bold_color_style(EVERFOREST_RED),
            "2" => bold_color_style(EVERFOREST_ORANGE),
            "3" => bold_color_style(EVERFOREST_YELLOW),
            "4" => bold_color_style(EVERFOREST_GREEN),
            "5" => bold_color_style(EVERFOREST_BLUE),
            "6" => bold_color_style(EVERFOREST_PURPLE),
            _ => Style {
                bold: true,
                ..Style::default()
            },
        };
    }

    if name == "markup.heading" {
        return bold_color_style(EVERFOREST_YELLOW);
    }

    if name == "markup.bold" {
        return Style {
            bold: true,
            ..Style::default()
        };
    }

    if name == "markup.list" || name.starts_with("markup.list.") {
        return color_style(EVERFOREST_RED);
    }

    if name == "markup.link.url" {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "markup.link.label" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name == "markup.link.text" {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("markup.link") {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "markup.quote" {
        return color_style(EVERFOREST_GREY1);
    }

    if name == "markup.raw.inline" {
        return color_style(EVERFOREST_GREEN);
    }

    if name == "markup.raw.block" {
        return color_style(EVERFOREST_AQUA);
    }

    if name.starts_with("markup") {
        return Style::default();
    }

    if name == "embedded" {
        return color_style(EVERFOREST_FG);
    }

    Style::default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rust_highlight_preserves_content() {
        let source = "fn main() { println!(\"hello\"); }";
        let lines = highlight_lines(Path::new("main.rs"), source).expect("expected highlight");
        assert_eq!(lines.len(), 1);
        let rendered: String = lines[0].iter().map(|span| span.content.as_str()).collect();
        assert_eq!(rendered, source);
    }

    #[test]
    fn markdown_highlight_preserves_content() {
        let source = "# Heading\n\nSome **bold** text.";
        let lines = highlight_lines(Path::new("README.md"), source).expect("expected highlight");
        assert_eq!(lines.len(), 3);
        let reconstructed: Vec<String> = lines
            .iter()
            .map(|line| {
                line.iter()
                    .map(|span| span.content.as_str())
                    .collect::<String>()
            })
            .collect();
        assert_eq!(reconstructed.join("\n"), source);
    }
}
