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

#[test]
fn render_code_block_with_horizontal_scroll() {
    use chatui::components::scroll::{ScrollAxis, ScrollMsg};
    use chatui::event::Size;

    // A long line that requires horizontal scrolling in a narrow viewport
    let doc = MarkdownDocument::parse("```rust\nfn very_long_function_name_that_exceeds_width(arg1: Type1, arg2: Type2) -> ReturnType {\n    // content\n}\n```");
    let mut state = MarkdownState::default();
    state.sync_with(&doc);

    // Use a narrow viewport to force horizontal scrolling
    let viewport_width = 30;
    let viewport_height = 10;

    for offset in 0..=8 {
        // First, do a "warm-up" render to establish viewport/content sizes.
        // The on_resize callback would normally send a Resize message during
        // rendering, but we need to manually process it here.
        let _view = markdown_view("scroll_test", &doc, &state, |msg| msg);
        let _ = chatui::test_utils::render_node_to_string(_view, viewport_width, viewport_height);

        // Now manually set the viewport and content sizes based on what we know.
        // The content width is roughly the length of the longest line (~85 chars).
        state.update(crate::MarkdownMsg::CodeBlockScroll {
            id: crate::CodeBlockId(0),
            msg: ScrollMsg::Resize {
                viewport: Size::new(viewport_width as u16, viewport_height as u16),
                content: Size::new(85, 4),
            },
        });

        // Set the horizontal scroll offset (will now be properly clamped)
        state.update(crate::MarkdownMsg::CodeBlockScroll {
            id: crate::CodeBlockId(0),
            msg: ScrollMsg::AxisJumpTo {
                axis: ScrollAxis::Horizontal,
                offset: offset as f32,
            },
        });

        // Render again with the correct offset
        let view = markdown_view("scroll_test", &doc, &state, |msg| msg);
        let rendered =
            chatui::test_utils::render_node_to_string(view, viewport_width, viewport_height)
                .unwrap();

        insta::assert_snapshot!(format!("offset_{offset}"), rendered);
    }
}
