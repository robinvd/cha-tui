use crate::parse::{Block, Span};
use crate::{markdown_view, MarkdownDocument, MarkdownState};

#[test]
fn parse_simple_paragraph() {
    let doc = MarkdownDocument::parse("Hello world");
    assert_eq!(doc.blocks.len(), 1);
    match &doc.blocks[0] {
        Block::Paragraph(spans) => {
            assert_eq!(spans.len(), 1);
            assert!(matches!(&spans[0], Span::Text(t) if t == "Hello world"));
        }
        _ => panic!("expected paragraph"),
    }
}

#[test]
fn parse_tight_list_items_create_implicit_paragraphs() {
    let doc = MarkdownDocument::parse("- Item 1\n- Item 2");
    assert_eq!(doc.blocks.len(), 1);

    match &doc.blocks[0] {
        Block::List { items, ordered } => {
            assert!(!ordered);
            assert_eq!(items.len(), 2);

            match items[0].as_slice() {
                [Block::Paragraph(spans)] => {
                    assert!(matches!(spans.first(), Some(Span::Text(t)) if t == "Item 1"));
                }
                _ => panic!("expected paragraph in list item"),
            }
        }
        _ => panic!("expected list"),
    }
}

#[test]
fn fenced_code_blocks_are_numbered_but_indented_are_not() {
    let md = "```rust\nfn main() {}\n```\n\n    indented\n";
    let doc = MarkdownDocument::parse(md);

    assert_eq!(doc.code_block_count(), 1);

    let mut fenced = 0;
    let mut indented = 0;
    for block in &doc.blocks {
        if let Block::Code { id, .. } = block {
            if id.is_some() {
                fenced += 1;
            } else {
                indented += 1;
            }
        }
    }

    assert_eq!(fenced, 1);
    assert_eq!(indented, 1);
}

#[test]
fn parses_tables_with_headers() {
    let md = "| a | b |\n| --- | --- |\n| 1 | 2 |\n";
    let doc = MarkdownDocument::parse(md);

    assert_eq!(doc.blocks.len(), 1);
    match &doc.blocks[0] {
        Block::Table { header, rows } => {
            let header = header.as_ref().expect("expected header");
            assert_eq!(header.len(), 2);
            assert_eq!(rows.len(), 1);

            assert!(matches!(
                header[0].first(),
                Some(Span::Text(t)) if t.trim() == "a"
            ));
            assert!(matches!(
                header[1].first(),
                Some(Span::Text(t)) if t.trim() == "b"
            ));
        }
        _ => panic!("expected table"),
    }
}

// Rendering tests

#[test]
fn render_paragraph() {
    let doc = MarkdownDocument::parse("Hello **bold** world");
    let state = MarkdownState::default();
    let view = markdown_view("test", &doc, &state, |msg| msg);

    let rendered = chatui::test_utils::render_node_to_string(view, 80, 10).unwrap();

    insta::assert_snapshot!(rendered);
}

#[test]
fn render_heading() {
    let doc = MarkdownDocument::parse("# Title H1\n## Title H2");
    let state = MarkdownState::default();
    let view = markdown_view("test", &doc, &state, |msg| msg);

    let rendered = chatui::test_utils::render_node_to_string(view, 80, 10).unwrap();

    insta::assert_snapshot!(rendered);
}

#[test]
fn render_list() {
    let doc = MarkdownDocument::parse("- Item 1\n- Item 2\n\n1. Ordered\n2. Items");
    let state = MarkdownState::default();
    let view = markdown_view("test", &doc, &state, |msg| msg);

    let rendered = chatui::test_utils::render_node_to_string(view, 80, 10).unwrap();

    insta::assert_snapshot!(rendered);
}

#[test]
fn render_code_block() {
    let doc = MarkdownDocument::parse("```rust\nfn main() {}\n```");
    let mut state = MarkdownState::default();
    state.sync_with(&doc);
    let view = markdown_view("test", &doc, &state, |msg| msg);

    let rendered = chatui::test_utils::render_node_to_string(view, 80, 10).unwrap();

    insta::assert_snapshot!(rendered);
}

#[test]
fn render_quote() {
    let doc = MarkdownDocument::parse("> Nested\n> content");
    let state = MarkdownState::default();
    let view = markdown_view("test", &doc, &state, |msg| msg);

    let rendered = chatui::test_utils::render_node_to_string(view, 80, 10).unwrap();

    insta::assert_snapshot!(rendered);
}

#[test]
fn render_rule() {
    let doc = MarkdownDocument::parse("---");
    let state = MarkdownState::default();
    let view = markdown_view("test", &doc, &state, |msg| msg);

    let rendered = chatui::test_utils::render_node_to_string(view, 80, 10).unwrap();

    insta::assert_snapshot!(rendered);
}

#[test]
fn render_table() {
    let doc = MarkdownDocument::parse("| A | B |\n|---|---|\n| 1 | 2 |");
    let state = MarkdownState::default();
    let view = markdown_view("test", &doc, &state, |msg| msg);

    let rendered = chatui::test_utils::render_node_to_string(view, 80, 10).unwrap();

    insta::assert_snapshot!(rendered);
}
