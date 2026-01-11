use crate::CodeBlockId;
use pulldown_cmark::{CodeBlockKind, Event, HeadingLevel, Options, Parser, Tag, TagEnd};

#[derive(Debug, Clone)]
pub struct MarkdownDocument {
    pub(crate) blocks: Vec<Block>,
    code_block_count: usize,
}

#[derive(Debug, Clone)]
pub struct MarkdownParseOptions {
    pub pulldown: Options,
}

impl Default for MarkdownParseOptions {
    fn default() -> Self {
        Self {
            pulldown: Options::all(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Block {
    Paragraph(Vec<Span>),
    Heading(u32, Vec<Span>),
    List {
        items: Vec<Vec<Block>>,
        ordered: bool,
    },
    Code {
        id: Option<CodeBlockId>,
        lang: String,
        content: String,
    },
    Quote(Vec<Block>),
    Rule,
    Table {
        header: Option<Vec<Vec<Span>>>,
        rows: Vec<Vec<Vec<Span>>>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum Span {
    Text(String),
    Strong(Vec<Span>),
    Emphasis(Vec<Span>),
    Strikethrough(Vec<Span>),
    Code(String),
    Link(String, Vec<Span>),
    SoftBreak,
    HardBreak,
}

fn heading_level_to_u32(level: HeadingLevel) -> u32 {
    match level {
        HeadingLevel::H1 => 1,
        HeadingLevel::H2 => 2,
        HeadingLevel::H3 => 3,
        HeadingLevel::H4 => 4,
        HeadingLevel::H5 => 5,
        HeadingLevel::H6 => 6,
    }
}

impl MarkdownDocument {
    pub fn parse(markdown: &str) -> Self {
        Self::parse_with(markdown, &MarkdownParseOptions::default())
    }

    pub fn parse_with(markdown: &str, options: &MarkdownParseOptions) -> Self {
        let parser = Parser::new_ext(markdown, options.pulldown);

        let mut stack: Vec<Container> = vec![Container::Root(vec![])];
        let mut fenced_code_block_count = 0;

        for event in parser {
            match event {
                Event::Start(tag) => {
                    match tag {
                        Tag::Paragraph
                        | Tag::Heading { .. }
                        | Tag::List(_)
                        | Tag::Item
                        | Tag::BlockQuote(_)
                        | Tag::CodeBlock(_)
                        | Tag::Table(_)
                        | Tag::TableRow
                        | Tag::TableCell => {
                            close_implicit_paragraph(&mut stack, &mut fenced_code_block_count);
                        }
                        _ => {}
                    }

                    match tag {
                        Tag::Paragraph => {
                            stack.push(Container::Block(BlockBuilder::Paragraph(vec![])))
                        }
                        Tag::Heading { level, .. } => stack.push(Container::Block(
                            BlockBuilder::Heading(heading_level_to_u32(level), vec![]),
                        )),
                        Tag::List(start) => stack.push(Container::List {
                            items: vec![],
                            ordered: start.is_some(),
                        }),
                        Tag::Item => stack.push(Container::Root(vec![])),
                        Tag::BlockQuote(_) => stack.push(Container::Root(vec![])),
                        Tag::CodeBlock(kind) => {
                            let (lang, fenced) = match kind {
                                CodeBlockKind::Fenced(l) => (l.to_string(), true),
                                CodeBlockKind::Indented => (String::new(), false),
                            };
                            stack.push(Container::Block(BlockBuilder::Code {
                                lang,
                                content: String::new(),
                                fenced,
                            }));
                        }
                        Tag::Table(_) => stack.push(Container::Table {
                            header_rows: vec![],
                            rows: vec![],
                        }),
                        Tag::TableHead => stack.push(Container::TableHeadRow(vec![])),
                        Tag::TableRow => stack.push(Container::TableRow(vec![])),
                        Tag::TableCell => {
                            stack.push(Container::Block(BlockBuilder::TableCell(vec![])))
                        }
                        Tag::Emphasis => stack.push(Container::Span(SpanBuilder::Emphasis(vec![]))),
                        Tag::Strong => stack.push(Container::Span(SpanBuilder::Strong(vec![]))),
                        Tag::Strikethrough => {
                            stack.push(Container::Span(SpanBuilder::Strikethrough(vec![])))
                        }
                        Tag::Link { dest_url, .. } => stack.push(Container::Span(
                            SpanBuilder::Link(dest_url.to_string(), vec![]),
                        )),
                        _ => {}
                    }
                }
                Event::End(tag) => {
                    match tag {
                        TagEnd::Item | TagEnd::BlockQuote(_) | TagEnd::List(_) => {
                            close_implicit_paragraph(&mut stack, &mut fenced_code_block_count);
                        }
                        _ => {}
                    }

                    match tag {
                        TagEnd::TableCell => {
                            if let Some(Container::Block(BlockBuilder::TableCell(spans))) =
                                stack.pop()
                            {
                                match stack.last_mut() {
                                    Some(Container::TableRow(cells))
                                    | Some(Container::TableHeadRow(cells)) => {
                                        cells.push(spans);
                                    }
                                    _ => {}
                                }
                            }
                        }
                        TagEnd::Paragraph | TagEnd::Heading(..) | TagEnd::CodeBlock => {
                            if let Some(Container::Block(builder)) = stack.pop() {
                                let block = builder.build(&mut fenced_code_block_count);
                                add_block(&mut stack, block);
                            }
                        }
                        TagEnd::List(_) => {
                            if let Some(Container::List { items, ordered }) = stack.pop() {
                                add_block(&mut stack, Block::List { items, ordered });
                            }
                        }
                        TagEnd::Item => {
                            if let Some(Container::Root(blocks)) = stack.pop() {
                                if let Some(Container::List { items, .. }) = stack.last_mut() {
                                    items.push(blocks);
                                }
                            }
                        }
                        TagEnd::BlockQuote(_) => {
                            if let Some(Container::Root(blocks)) = stack.pop() {
                                add_block(&mut stack, Block::Quote(blocks));
                            }
                        }
                        TagEnd::Table => {
                            if let Some(Container::Table {
                                header_rows,
                                mut rows,
                                ..
                            }) = stack.pop()
                            {
                                let header = header_rows.first().cloned();
                                if header_rows.len() > 1 {
                                    rows.splice(0..0, header_rows.into_iter().skip(1));
                                }
                                add_block(&mut stack, Block::Table { header, rows });
                            }
                        }
                        TagEnd::TableHead => {
                            if let Some(Container::TableHeadRow(cells)) = stack.pop() {
                                if let Some(header_rows) =
                                    stack.iter_mut().rev().find_map(|c| match c {
                                        Container::Table { header_rows, .. } => Some(header_rows),
                                        _ => None,
                                    })
                                {
                                    header_rows.push(cells);
                                }
                            }
                        }
                        TagEnd::TableRow => {
                            if let Some(Container::TableRow(cells)) = stack.pop() {
                                if let Some(rows) = stack.iter_mut().rev().find_map(|c| match c {
                                    Container::Table { rows, .. } => Some(rows),
                                    _ => None,
                                }) {
                                    rows.push(cells);
                                }
                            }
                        }
                        TagEnd::Emphasis
                        | TagEnd::Strong
                        | TagEnd::Strikethrough
                        | TagEnd::Link => {
                            if let Some(Container::Span(builder)) = stack.pop() {
                                add_span(&mut stack, builder.build());
                            }
                        }
                        _ => {}
                    }
                }
                Event::Text(text) => add_span(&mut stack, Span::Text(text.to_string())),
                Event::Code(text) => add_span(&mut stack, Span::Code(text.to_string())),
                Event::SoftBreak => add_span(&mut stack, Span::SoftBreak),
                Event::HardBreak => add_span(&mut stack, Span::HardBreak),
                Event::Rule => {
                    close_implicit_paragraph(&mut stack, &mut fenced_code_block_count);
                    if let Some(Container::Root(blocks)) = stack.last_mut() {
                        blocks.push(Block::Rule);
                    }
                }
                _ => {}
            }
        }

        close_implicit_paragraph(&mut stack, &mut fenced_code_block_count);

        let blocks = match stack.pop() {
            Some(Container::Root(blocks)) => blocks,
            _ => vec![],
        };

        Self {
            blocks,
            code_block_count: fenced_code_block_count,
        }
    }

    pub fn code_block_count(&self) -> usize {
        self.code_block_count
    }
}

enum Container {
    Root(Vec<Block>),
    List {
        items: Vec<Vec<Block>>,
        ordered: bool,
    },
    Block(BlockBuilder),
    Span(SpanBuilder),
    Table {
        header_rows: Vec<Vec<Vec<Span>>>,
        rows: Vec<Vec<Vec<Span>>>,
    },
    TableHeadRow(Vec<Vec<Span>>),
    TableRow(Vec<Vec<Span>>),
}

enum BlockBuilder {
    Paragraph(Vec<Span>),
    Heading(u32, Vec<Span>),
    Code {
        lang: String,
        content: String,
        fenced: bool,
    },
    TableCell(Vec<Span>),
}

impl BlockBuilder {
    fn build(self, fenced_code_block_count: &mut usize) -> Block {
        match self {
            BlockBuilder::Paragraph(spans) => Block::Paragraph(spans),
            BlockBuilder::Heading(level, spans) => Block::Heading(level, spans),
            BlockBuilder::Code {
                lang,
                content,
                fenced,
            } => {
                let id = if fenced {
                    let id = CodeBlockId(*fenced_code_block_count);
                    *fenced_code_block_count += 1;
                    Some(id)
                } else {
                    None
                };

                Block::Code { id, lang, content }
            }
            BlockBuilder::TableCell(_) => panic!("TableCell should be handled separately"),
        }
    }
}

enum SpanBuilder {
    Strong(Vec<Span>),
    Emphasis(Vec<Span>),
    Strikethrough(Vec<Span>),
    Link(String, Vec<Span>),
}

impl SpanBuilder {
    fn build(self) -> Span {
        match self {
            SpanBuilder::Strong(s) => Span::Strong(s),
            SpanBuilder::Emphasis(s) => Span::Emphasis(s),
            SpanBuilder::Strikethrough(s) => Span::Strikethrough(s),
            SpanBuilder::Link(u, s) => Span::Link(u, s),
        }
    }
}

fn close_implicit_paragraph(stack: &mut Vec<Container>, fenced_code_block_count: &mut usize) {
    if !matches!(
        stack.last(),
        Some(Container::Block(BlockBuilder::Paragraph(_)))
    ) {
        return;
    }

    if let Some(Container::Block(builder)) = stack.pop() {
        add_block(stack, builder.build(fenced_code_block_count));
    }
}

fn add_block(stack: &mut [Container], block: Block) {
    if let Some(Container::Root(blocks)) = stack.last_mut() {
        blocks.push(block);
    }
}

fn add_span(stack: &mut Vec<Container>, span: Span) {
    if let Some(Container::Root(_)) = stack.last() {
        stack.push(Container::Block(BlockBuilder::Paragraph(vec![])));
    }

    if let Some(Container::Block(BlockBuilder::Code { content, .. })) = stack.last_mut() {
        match span {
            Span::Text(t) => content.push_str(&t),
            Span::SoftBreak | Span::HardBreak => content.push('\n'),
            _ => {}
        }
        return;
    }

    match stack.last_mut() {
        Some(Container::Block(builder)) => match builder {
            BlockBuilder::Paragraph(spans)
            | BlockBuilder::Heading(_, spans)
            | BlockBuilder::TableCell(spans) => spans.push(span),
            BlockBuilder::Code { .. } => {}
        },
        Some(Container::Span(builder)) => match builder {
            SpanBuilder::Strong(spans)
            | SpanBuilder::Emphasis(spans)
            | SpanBuilder::Strikethrough(spans)
            | SpanBuilder::Link(_, spans) => spans.push(span),
        },
        _ => {}
    }
}
