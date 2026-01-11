use crate::code_block::CodeBlockRef;
use crate::parse::{Block, Span};
use crate::{CodeBlockId, MarkdownDocument, MarkdownMsg, MarkdownState};
use chatui::components::paragraph::rich_paragraph;
use chatui::components::scroll::scrollable_content;
use chatui::dom::{
    block, block_with_title, column, renderable_ref, row, table, text, text_owned, Color, Node,
    Style, TableColumn, TableColumnWidth, TableRow, TextSpan,
};
use std::rc::Rc;
use taffy::Dimension;

const DOC_SCROLL_ID: &str = "chatui-markdown-doc";
const CODE_SCROLL_ID: &str = "chatui-markdown-code";

struct RenderContext<'a, Msg> {
    id_prefix_hash: u64,
    map_msg: Rc<dyn Fn(MarkdownMsg) -> Msg>,
    state: &'a MarkdownState,
}

pub fn markdown_view<'a, Msg: 'static>(
    id_prefix: &'static str,
    doc: &'a MarkdownDocument,
    state: &'a MarkdownState,
    map_msg: impl Fn(MarkdownMsg) -> Msg + 'static,
) -> Node<'a, Msg> {
    let map_msg: Rc<dyn Fn(MarkdownMsg) -> Msg> = Rc::new(map_msg);
    let ctx = RenderContext {
        id_prefix_hash: fnv1a_64(id_prefix.as_bytes()),
        map_msg: Rc::clone(&map_msg),
        state,
    };

    let children: Vec<Node<'a, Msg>> = doc
        .blocks
        .iter()
        .map(|b| render_block(b, &ctx, 0))
        .collect();

    let doc_scroll_msg = Rc::clone(&map_msg);
    scrollable_content(
        DOC_SCROLL_ID,
        state.doc_scroll(),
        1,
        move |msg| doc_scroll_msg(MarkdownMsg::DocScroll(msg)),
        column(children).with_fill().with_gap(0, 1),
    )
    .with_id_mixin(DOC_SCROLL_ID, ctx.id_prefix_hash)
}

fn render_block<'a, Msg: 'static>(
    block: &'a Block,
    ctx: &RenderContext<'a, Msg>,
    list_depth: usize,
) -> Node<'a, Msg> {
    match block {
        Block::Paragraph(spans) => rich_paragraph(render_spans(spans)),
        Block::Heading(level, spans) => {
            let style = match level {
                1 => Style {
                    bold: true,
                    fg: Some(Color::Cyan),
                    ..Default::default()
                },
                2 => Style {
                    bold: true,
                    fg: Some(Color::Blue),
                    ..Default::default()
                },
                _ => Style {
                    bold: true,
                    ..Default::default()
                },
            };

            rich_paragraph(render_spans_with_style(spans, style))
        }
        Block::List { items, ordered } => render_list(items, *ordered, ctx, list_depth),
        Block::Code { id, lang, content } => render_code_block(id, lang, content, ctx),
        Block::Quote(blocks) => {
            let children: Vec<Node<Msg>> = blocks
                .iter()
                .map(|b| render_block(b, ctx, list_depth))
                .collect();

            column(children)
                .with_padding_2d(2, 0)
                .with_style(Style::dim())
        }
        Block::Rule => {
            let line = "─".repeat(256);
            text_owned(line).with_style(Style::dim())
        }
        Block::Table { header, rows } => render_table(header.as_deref(), rows),
    }
}

fn render_list<'a, Msg: 'static>(
    items: &'a [Vec<Block>],
    ordered: bool,
    ctx: &RenderContext<'a, Msg>,
    list_depth: usize,
) -> Node<'a, Msg> {
    let marker_width = if ordered {
        format!("{}.", items.len()).len() + 1
    } else {
        2
    };

    let mut children = Vec::with_capacity(items.len());
    for (i, item_blocks) in items.iter().enumerate() {
        let marker = if ordered {
            format!("{}. ", i + 1)
        } else {
            "- ".to_string()
        };

        let content_nodes: Vec<Node<Msg>> = item_blocks
            .iter()
            .map(|b| {
                let nested_depth = if matches!(b, Block::List { .. }) {
                    list_depth + 1
                } else {
                    list_depth
                };
                render_block(b, ctx, nested_depth)
            })
            .collect();

        children.push(row(vec![
            text(marker)
                .with_width(Dimension::length(marker_width as f32))
                .with_flex_shrink(0.0),
            column(content_nodes).with_flex_grow(1.0),
        ]));
    }

    let padding = (list_depth as u32) * 2;
    column(children).with_padding_2d(padding, 0)
}

fn render_code_block<'a, Msg: 'static>(
    id: &'a Option<CodeBlockId>,
    lang: &'a str,
    content: &'a str,
    ctx: &RenderContext<'a, Msg>,
) -> Node<'a, Msg> {
    let code_style = Style {
        fg: Some(Color::PaletteFg),
        bg: Some(Color::BrightBlack),
        ..Default::default()
    };

    let container_style = Style {
        border: true,
        bg: Some(Color::BrightBlack),
        ..Default::default()
    };

    let inner = renderable_ref(CodeBlockRef::new(content, code_style));
    let container = if lang.is_empty() {
        block(vec![inner])
    } else {
        block_with_title(lang.to_string(), vec![inner])
    }
    .with_style(container_style)
    .with_padding_2d(1, 0)
    .with_width(Dimension::percent(1.0))
    .with_align_items(taffy::AlignItems::FlexStart);

    let Some(id) = id else {
        return container;
    };

    let Some(scroll_state) = ctx.state.code_scroll(*id) else {
        return container;
    };

    let map_msg = Rc::clone(&ctx.map_msg);
    let id_val = *id;
    let mixin = ctx.id_prefix_hash ^ (id.0 as u64);

    scrollable_content(
        CODE_SCROLL_ID,
        scroll_state,
        1,
        move |msg| map_msg(MarkdownMsg::CodeBlockScroll { id: id_val, msg }),
        container,
    )
    .with_id_mixin(CODE_SCROLL_ID, mixin)
}

fn render_table<'a, Msg: 'static>(
    header: Option<&'a [Vec<Span>]>,
    rows: &'a [Vec<Vec<Span>>],
) -> Node<'a, Msg> {
    let col_count = header
        .map(|h| h.len())
        .or_else(|| rows.first().map(|r| r.len()))
        .unwrap_or(0);

    if col_count == 0 {
        return text("");
    }

    let columns: Vec<TableColumn<Msg>> = (0..col_count)
        .map(|col_idx| {
            let mut col = TableColumn::new(TableColumnWidth::Flexible(1.0));
            if let Some(header) = header {
                if let Some(cell) = header.get(col_idx) {
                    col = col.with_header(rich_paragraph(render_spans(cell)).into());
                }
            }
            col
        })
        .collect();

    let table_rows: Vec<TableRow<Msg>> = rows
        .iter()
        .map(|r| {
            let cells = (0..col_count)
                .map(|col_idx| {
                    let spans = r.get(col_idx).map(Vec::as_slice).unwrap_or(&[]);
                    rich_paragraph(render_spans(spans)).into()
                })
                .collect();

            TableRow::new(cells)
        })
        .collect();

    table(columns, table_rows).into_node()
}

fn render_spans(spans: &[Span]) -> Vec<TextSpan> {
    render_spans_with_style(spans, Style::default())
}

fn render_spans_with_style(spans: &[Span], base_style: Style) -> Vec<TextSpan> {
    let mut out = Vec::new();
    for span in spans {
        render_span(span, &mut out, base_style);
    }
    out
}

fn render_span(span: &Span, out: &mut Vec<TextSpan>, style: Style) {
    match span {
        Span::Text(t) => out.push(TextSpan::new(t.clone(), style)),
        Span::Strong(s) => {
            let mut new = style;
            new.bold = true;
            for child in s {
                render_span(child, out, new);
            }
        }
        Span::Emphasis(s) => {
            let mut new = style;
            new.dim = true;
            for child in s {
                render_span(child, out, new);
            }
        }
        Span::Strikethrough(s) => {
            let new = Style {
                dim: true,
                fg: Some(Color::BrightBlack),
                ..style
            };
            for child in s {
                render_span(child, out, new);
            }
        }
        Span::Code(c) => {
            let mut new = style;
            new.reverse = true;
            out.push(TextSpan::new(c.clone(), new));
        }
        Span::Link(url, s) => {
            let mut link_style = style;
            link_style.fg = Some(Color::Blue);
            for child in s {
                render_span(child, out, link_style);
            }

            let mut url_style = style;
            url_style.dim = true;
            out.push(TextSpan::new(format!(" ({url})"), url_style));
        }
        Span::SoftBreak => out.push(TextSpan::new(" ", style)),
        Span::HardBreak => out.push(TextSpan::new("\n", style)),
    }
}

fn fnv1a_64(bytes: &[u8]) -> u64 {
    const OFFSET_BASIS: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x100000001b3;

    let mut hash = OFFSET_BASIS;
    for &b in bytes {
        hash ^= b as u64;
        hash = hash.wrapping_mul(PRIME);
    }
    hash
}
