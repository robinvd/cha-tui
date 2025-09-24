use std::path::Path;

use once_cell::sync::Lazy;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter};

use chatui::dom::{Color, Style, TextSpan};

use color_eyre::eyre::Result;

#[derive(Clone, Copy)]
pub enum LanguageKind {
    Rust,
    Markdown,
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

pub fn language_for_path(path: &Path) -> Option<LanguageKind> {
    let ext = path.extension()?.to_str()?.to_ascii_lowercase();
    match ext.as_str() {
        "rs" => Some(LanguageKind::Rust),
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
    styles: Vec<Style>,
}

impl HighlightRegistry {
    fn new() -> Result<Self> {
        let mut rust = HighlightConfiguration::new(
            tree_sitter_rust::LANGUAGE.into(),
            "rust",
            tree_sitter_rust::HIGHLIGHTS_QUERY,
            tree_sitter_rust::INJECTIONS_QUERY,
            tree_sitter_rust::TAGS_QUERY,
        )?;
        rust.configure(HIGHLIGHT_NAMES);

        let mut markdown = HighlightConfiguration::new(
            tree_sitter_md::LANGUAGE.into(),
            "markdown",
            tree_sitter_md::HIGHLIGHT_QUERY_BLOCK,
            tree_sitter_md::INJECTION_QUERY_BLOCK,
            "",
        )?;
        markdown.configure(HIGHLIGHT_NAMES);

        let mut markdown_inline = HighlightConfiguration::new(
            tree_sitter_md::INLINE_LANGUAGE.into(),
            "markdown_inline",
            tree_sitter_md::HIGHLIGHT_QUERY_INLINE,
            tree_sitter_md::INJECTION_QUERY_INLINE,
            "",
        )?;
        markdown_inline.configure(HIGHLIGHT_NAMES);

        let styles = HIGHLIGHT_NAMES.iter().map(|name| style_for(name)).collect();

        Ok(Self {
            rust,
            markdown,
            markdown_inline,
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

    match name {
        "comment" | "comment.documentation" => color_style(Color::Blue),
        "constant.builtin" | "constant.builtin.boolean" | "constant.numeric" => {
            color_style(Color::Magenta)
        }
        "constant.character.escape" => color_style(Color::Green),
        "string" => color_style(Color::Cyan),
        "string.regexp" => color_style(Color::Green),
        "string.special" => color_style(Color::Yellow),
        "variable.builtin" => color_style(Color::Magenta),
        "variable.other.member" => color_style(Color::Blue),
        "label" => color_style(Color::Yellow),
        "punctuation.special" => color_style(Color::Blue),
        "keyword" | "keyword.storage" => color_style(Color::Red),
        "keyword.operator" | "operator" => color_style(Color::Yellow),
        "keyword.directive" => color_style(Color::Magenta),
        "function" | "function.builtin" | "function.macro" | "constructor" => {
            color_style(Color::Green)
        }
        "tag" => color_style(Color::Yellow),
        "namespace" | "module" => color_style(Color::Yellow),
        "attribute" => color_style(Color::Magenta),
        "special" => color_style(Color::Blue),
        name if name.starts_with("markup.heading.") => {
            let last = name.rsplit('.').next().unwrap_or("");
            match last {
                "1" => bold_color_style(Color::Red),
                "2" | "3" => bold_color_style(Color::Yellow),
                "4" => bold_color_style(Color::Green),
                "5" => bold_color_style(Color::Blue),
                "6" => bold_color_style(Color::Magenta),
                _ => Style {
                    bold: true,
                    ..Style::default()
                },
            }
        }
        "markup.bold" => Style {
            bold: true,
            ..Style::default()
        },
        "markup.list" => color_style(Color::Red),
        "markup.link.url" => color_style(Color::Blue),
        "markup.link.label" => color_style(Color::Yellow),
        "markup.link.text" => color_style(Color::Magenta),
        "markup.quote" => color_style(Color::Blue),
        "markup.raw.inline" => color_style(Color::Green),
        "markup.raw.block" => color_style(Color::Cyan),
        _ => {
            let base = name.split('.').next().unwrap_or(name);
            match base {
                "type" => color_style(Color::Yellow),
                "number" => color_style(Color::Magenta),
                "boolean" => color_style(Color::Magenta),
                "keyword" => color_style(Color::Red),
                "operator" => color_style(Color::Yellow),
                "string" => color_style(Color::Cyan),
                "function" => color_style(Color::Green),
                "comment" => color_style(Color::Blue),
                "attribute" => color_style(Color::Magenta),
                "tag" => color_style(Color::Yellow),
                _ => Style::default(),
            }
        }
    }
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
