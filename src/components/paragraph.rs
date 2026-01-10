use crate::dom::{
    Node, Renderable, RenderablePatch, RenderableRef, RetainedNode, Style, TextSpan, renderable,
    renderable_ref,
};
use crate::render::RenderContext;
use taffy::style::AvailableSpace;
use unicode_width::UnicodeWidthChar;

const TAB_WIDTH: usize = 8;

#[derive(Clone, Debug, PartialEq)]
pub struct Paragraph {
    spans: Vec<TextSpan>,
    base_style: Style,
}

#[derive(Debug)]
pub struct ParagraphRef<'a> {
    pub spans: &'a [TextSpan],
    pub base_style: Style,
}

#[derive(Default)]
struct WrappedLine {
    spans: Vec<TextSpan>,
    width: usize,
}

impl Paragraph {
    pub fn new(content: impl Into<String>) -> Self {
        Self::from_spans(vec![TextSpan::new(content, Style::default())])
    }

    pub fn from_spans(spans: Vec<TextSpan>) -> Self {
        Self {
            spans,
            base_style: Style::default(),
        }
    }

    pub fn with_style(mut self, style: Style) -> Self {
        let prev = self.base_style;
        self.base_style = style;
        for span in &mut self.spans {
            if span.style == prev {
                span.style = style;
            }
        }
        self
    }

    pub fn into_node<Msg: 'static>(self) -> RetainedNode<Msg> {
        renderable(self).into()
    }

    /// Creates a borrowed Node from this Paragraph.
    /// This is useful for view functions that need to return Node instead of RetainedNode.
    pub fn to_node<'a, Msg: 'static>(self) -> Node<'a, Msg> {
        crate::dom::renderable(self)
    }

    /// Creates a borrowed Node that references the spans in this Paragraph.
    pub fn to_node_ref<'a, Msg: 'static>(&'a self) -> Node<'a, Msg> {
        renderable_ref(ParagraphRef {
            spans: &self.spans,
            base_style: self.base_style,
        })
    }

    fn char_width_at_column(ch: char, column: usize) -> usize {
        if ch == '\t' {
            let next_tab_stop = ((column / TAB_WIDTH) + 1) * TAB_WIDTH;
            (next_tab_stop - column).max(1)
        } else {
            UnicodeWidthChar::width(ch).unwrap_or(0).max(1)
        }
    }

    fn wrap_to_width(&self, width: usize) -> Vec<WrappedLine> {
        let max_width = width.max(1);
        let mut lines = vec![WrappedLine::default()];
        let mut line_width = 0usize;
        let mut buffer = String::new();
        let mut buffer_width = 0usize;
        let mut buffer_style: Option<Style> = None;

        let flush_buffer = |lines: &mut Vec<WrappedLine>,
                            buffer: &mut String,
                            buffer_width: &mut usize,
                            line_width: &mut usize,
                            buffer_style: &mut Option<Style>| {
            if buffer.is_empty() {
                return;
            }
            let style = buffer_style
                .as_ref()
                .copied()
                .unwrap_or_else(Style::default);
            lines
                .last_mut()
                .expect("expected at least one line")
                .spans
                .push(TextSpan::new(std::mem::take(buffer), style));
            *line_width += *buffer_width;
            *buffer_width = 0;
        };

        for span in &self.spans {
            if buffer_style.as_ref() != Some(&span.style) && !buffer.is_empty() {
                flush_buffer(
                    &mut lines,
                    &mut buffer,
                    &mut buffer_width,
                    &mut line_width,
                    &mut buffer_style,
                );
            }
            buffer_style = Some(span.style);

            for ch in span.content.chars() {
                if ch == '\n' {
                    flush_buffer(
                        &mut lines,
                        &mut buffer,
                        &mut buffer_width,
                        &mut line_width,
                        &mut buffer_style,
                    );
                    lines.last_mut().expect("expected at least one line").width = line_width;
                    lines.push(WrappedLine::default());
                    line_width = 0;
                    continue;
                }

                let w = Self::char_width_at_column(ch, line_width + buffer_width);
                if line_width > 0 && line_width + buffer_width + w > max_width {
                    flush_buffer(
                        &mut lines,
                        &mut buffer,
                        &mut buffer_width,
                        &mut line_width,
                        &mut buffer_style,
                    );
                    lines.last_mut().expect("expected at least one line").width = line_width;
                    lines.push(WrappedLine::default());
                    line_width = 0;
                }

                if ch == '\t' {
                    buffer.push_str(&" ".repeat(w));
                } else {
                    buffer.push(ch);
                }
                buffer_width += w;

                if line_width + buffer_width >= max_width {
                    flush_buffer(
                        &mut lines,
                        &mut buffer,
                        &mut buffer_width,
                        &mut line_width,
                        &mut buffer_style,
                    );
                }
            }
        }

        flush_buffer(
            &mut lines,
            &mut buffer,
            &mut buffer_width,
            &mut line_width,
            &mut buffer_style,
        );

        if let Some(line) = lines.last_mut() {
            line.width = line_width;
        }

        lines
    }

    fn content_width(&self) -> usize {
        let mut width = 0;
        for span in &self.spans {
            for ch in span.content.chars() {
                width += Self::char_width_at_column(ch, width);
            }
        }
        width
    }

    fn render_line(
        &self,
        ctx: &mut RenderContext<'_>,
        spans: &[TextSpan],
        origin_x: usize,
        y: usize,
        max_width: usize,
        mut skip_cols: usize,
    ) {
        let mut remaining = max_width;
        if remaining == 0 {
            return;
        }

        let mut cursor_x = origin_x;

        for span in spans {
            if remaining == 0 {
                break;
            }

            let mut collected = String::new();
            let mut taken = 0;

            for ch in span.content.chars() {
                if taken == remaining {
                    break;
                }
                let current_col = cursor_x + taken;
                let w = if ch == '\t' {
                    let next_tab_stop = ((current_col / TAB_WIDTH) + 1) * TAB_WIDTH;
                    (next_tab_stop - current_col).max(1)
                } else {
                    UnicodeWidthChar::width(ch).unwrap_or(0).max(1)
                };

                if skip_cols >= w {
                    skip_cols -= w;
                    continue;
                } else if skip_cols > 0 {
                    skip_cols = 0;
                }
                if taken + w > remaining {
                    break;
                }
                if ch == '\t' {
                    for _ in 0..w {
                        collected.push(' ');
                    }
                } else {
                    collected.push(ch);
                }
                taken += w;
            }

            if collected.is_empty() {
                continue;
            }

            let attrs = ctx.style_to_attributes(&span.style);
            ctx.write_text(cursor_x, y, &collected, &attrs);

            cursor_x += taken;
            remaining = remaining.saturating_sub(taken);
        }

        if remaining > 0 {
            let attrs = ctx.style_to_attributes(&self.base_style);
            let padding = " ".repeat(remaining);
            ctx.write_text(cursor_x, y, &padding, &attrs);
        }
    }
}

impl Renderable for Paragraph {
    fn patch_retained(&self, other: &mut dyn Renderable) -> RenderablePatch {
        if let Some(other) = other.as_any_mut().downcast_mut::<Self>() {
            if self.spans == other.spans && self.base_style == other.base_style {
                RenderablePatch::NoChange
            } else {
                let layout_changed = self.spans != other.spans;
                other.spans = self.spans.clone();
                other.base_style = self.base_style;
                if layout_changed {
                    RenderablePatch::ChangedLayout
                } else {
                    RenderablePatch::ChangedNoLayout
                }
            }
        } else {
            RenderablePatch::Replace
        }
    }

    fn measure(
        &self,
        _style: &taffy::Style,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
    ) -> taffy::Size<f32> {
        let wrap_width = known_dimensions.width.or(match available_space.width {
            AvailableSpace::Definite(w) => Some(w),
            _ => None,
        });
        let target_width = wrap_width.unwrap_or_else(|| self.content_width() as f32);
        let wrapped = self.wrap_to_width(target_width.max(1.0) as usize);
        let height = wrapped.len().max(1) as f32;
        let width = wrap_width
            .unwrap_or_else(|| wrapped.iter().map(|line| line.width).max().unwrap_or(0) as f32);

        taffy::Size { width, height }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        let area = ctx.area();
        if area.width == 0 || area.height == 0 {
            return;
        }

        let scroll_y = ctx.scroll_y().max(0.0).floor() as usize;
        let skip_cols = ctx.scroll_x().max(0.0).round() as usize;
        let wrap_width = area.width.saturating_add(skip_cols).max(1);
        let lines = self.wrap_to_width(wrap_width);

        let mut y = area.y;
        let mut remaining_rows = area.height;
        for line in lines.into_iter().skip(scroll_y).take(area.height) {
            self.render_line(ctx, &line.spans, area.x, y, area.width, skip_cols);
            y += 1;
            remaining_rows = remaining_rows.saturating_sub(1);
            if remaining_rows == 0 {
                return;
            }
        }

        while remaining_rows > 0 {
            self.render_line(ctx, &[], area.x, y, area.width, 0);
            y += 1;
            remaining_rows = remaining_rows.saturating_sub(1);
        }
    }

    fn debug_label(&self) -> &'static str {
        "paragraph"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl<'a> RenderableRef for ParagraphRef<'a> {
    fn debug_label(&self) -> &'static str {
        "paragraph"
    }

    fn patch_retained(&self, retained: &mut dyn Renderable) -> RenderablePatch {
        if let Some(p) = retained.as_any_mut().downcast_mut::<Paragraph>() {
            if p.spans == self.spans && p.base_style == self.base_style {
                RenderablePatch::NoChange
            } else {
                let layout_changed = p.spans != self.spans;
                p.spans = self.spans.to_vec();
                p.base_style = self.base_style;
                if layout_changed {
                    RenderablePatch::ChangedLayout
                } else {
                    RenderablePatch::ChangedNoLayout
                }
            }
        } else {
            RenderablePatch::Replace
        }
    }

    fn into_retained(self: Box<Self>) -> Box<dyn Renderable> {
        Box::new(Paragraph {
            spans: self.spans.to_vec(),
            base_style: self.base_style,
        })
    }
}

pub fn paragraph<Msg: 'static>(content: impl Into<String>) -> Node<'static, Msg> {
    Paragraph::new(content).to_node()
}

pub fn rich_paragraph<Msg: 'static>(spans: impl Into<Vec<TextSpan>>) -> Node<'static, Msg> {
    Paragraph::from_spans(spans.into()).to_node()
}

pub fn rich_paragraph_ref<'a, Msg: 'static>(spans: &'a [TextSpan]) -> Node<'a, Msg> {
    renderable_ref(ParagraphRef {
        spans,
        base_style: Style::default(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ScrollState, Size, buffer::DoubleBuffer, components::scroll, dom::rounding::round_layout,
        palette::Palette, render::Renderer,
    };
    use taffy::{AvailableSpace, compute_root_layout};

    #[test]
    fn paragraph_wraps_to_multiple_lines() {
        let node = paragraph("abcd ef");
        let mut node: RetainedNode<()> = node.into();

        taffy::compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(4.0),
                height: taffy::AvailableSpace::MaxContent,
            },
        );
        round_layout(&mut node);

        assert_eq!(node.layout_state().layout.size.height, 2.0);
    }

    #[test]
    fn rich_paragraph_keeps_spans() {
        let para = Paragraph::from_spans(vec![
            TextSpan::new("hello", Style::fg(crate::dom::Color::Blue)),
            TextSpan::new("world", Style::fg(crate::dom::Color::Red)),
        ]);

        assert_eq!(para.spans.len(), 2);
    }

    #[test]
    fn test_in_scroll() {
        let view = |offset: i32| {
            let p1 = paragraph::<()>("p1: 1\n2\n3\n4\n5");
            let p2 = paragraph::<()>("p2: 1\n2\n3\n4\n5");

            let mut state = ScrollState::vertical();
            state.update(crate::ScrollMsg::Resize {
                viewport: Size {
                    width: 6,
                    height: 6,
                },
                content: Size {
                    width: 5,
                    height: 10,
                },
            });
            state.update(crate::ScrollMsg::AxisDelta {
                axis: scroll::ScrollAxis::Vertical,
                amount: offset,
            });
            let mut node = scroll::scrollable_content_retained(
                "1",
                &state,
                3,
                |_| (),
                crate::dom::column_retained(vec![p1.into(), p2.into()]),
            )
            .with_fill();

            compute_root_layout(
                &mut node,
                u64::MAX.into(),
                taffy::Size {
                    width: AvailableSpace::Definite(6.0),
                    height: AvailableSpace::Definite(6.0),
                },
            );
            round_layout(&mut node);
            println!(
                "layout size {:?} content {:?} scrollbar {:?}",
                node.layout_state().layout.size,
                node.layout_state().layout.content_size,
                node.layout_state().layout.scrollbar_size
            );
            if let Some(el) = node.as_element() {
                for (idx, child) in el.children.iter().enumerate() {
                    println!(
                        "child {idx} size {:?} loc {:?}",
                        child.layout_state().layout.size,
                        child.layout_state().layout.location
                    );
                }
            }

            let mut buffer = DoubleBuffer::new(6, 6);
            let palette = Palette::default();
            let mut renderer = Renderer::new(&mut buffer, &palette);

            renderer
                .render(&node, Size::new(6, 6))
                .expect("render should succeed");

            renderer
                .buffer()
                .to_string()
                .lines()
                .map(|s| s.to_owned())
                .collect::<Vec<String>>()
        };

        let text = view(0);
        let expected = vec!["p1: 1█", "2    █", "3    █", "4     ", "5     ", "p2: 1 "];
        assert_eq!(text, expected);

        let text = view(1);
        let expected = vec!["2     ", "3    █", "4    █", "5    █", "p2: 1 ", "2     "];

        // should fail as scrolling doesnt fully work yet
        assert_eq!(text, expected);
    }

    #[test]
    fn test_in_scroll2() {
        let view = |offset: i32| {
            let p1 = crate::dom::row_retained(vec![
                crate::dom::text_retained("> "),
                paragraph::<()>("p1: 1\n2\n3\n4\n5").into(),
            ]);
            let p2 = crate::dom::row_retained(vec![
                crate::dom::text_retained("> "),
                paragraph::<()>("p2: 1\n2\n3\n4\n5").into(),
            ]);

            let mut state = ScrollState::vertical();
            state.update(crate::ScrollMsg::Resize {
                viewport: Size {
                    width: 8,
                    height: 6,
                },
                content: Size {
                    width: 7,
                    height: 10,
                },
            });
            state.update(crate::ScrollMsg::AxisDelta {
                axis: scroll::ScrollAxis::Vertical,
                amount: offset,
            });
            let mut node = scroll::scrollable_content_retained(
                "1",
                &state,
                3,
                |_| (),
                crate::dom::column_retained(vec![p1.into(), p2.into()]),
            )
            .with_fill();

            compute_root_layout(
                &mut node,
                u64::MAX.into(),
                taffy::Size {
                    width: AvailableSpace::Definite(8.0),
                    height: AvailableSpace::Definite(6.0),
                },
            );
            round_layout(&mut node);
            println!(
                "layout size {:?} content {:?} scrollbar {:?}",
                node.layout_state().layout.size,
                node.layout_state().layout.content_size,
                node.layout_state().layout.scrollbar_size
            );
            if let Some(el) = node.as_element() {
                for (idx, child) in el.children.iter().enumerate() {
                    println!(
                        "child {idx} size {:?} loc {:?}",
                        child.layout_state().layout.size,
                        child.layout_state().layout.location
                    );
                }
            }

            let mut buffer = DoubleBuffer::new(8, 6);
            let palette = Palette::default();
            let mut renderer = Renderer::new(&mut buffer, &palette);

            renderer
                .render(&node, Size::new(8, 6))
                .expect("render should succeed");

            renderer
                .buffer()
                .to_string()
                .lines()
                .map(|s| s.to_owned())
                .collect::<Vec<String>>()
        };

        let text = view(0);
        let expected = vec![
            "> p1: 1█",
            "  2    █",
            "  3    █",
            "  4     ",
            "  5     ",
            "> p2: 1 ",
        ];
        assert_eq!(text, expected);

        let text = view(1);
        let expected = vec![
            "  2     ",
            "  3    █",
            "  4    █",
            "  5    █",
            "> p2: 1 ",
            "  2     ",
        ];

        // should fail as scrolling doesnt fully work yet
        assert_eq!(text, expected);
    }
}
