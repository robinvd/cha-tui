use chatui::dom::{Renderable, RenderablePatch, RenderableRef, Style, TextSpan};
use chatui::render::RenderContext;
use taffy::{AvailableSpace, Size, Style as TaffyStyle};
use unicode_width::UnicodeWidthChar;

const TAB_WIDTH: usize = 8;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CodeBlock {
    content: String,
    style: Style,
    highlighted_lines: Option<Vec<Vec<TextSpan>>>,
}

impl CodeBlock {
    fn new(content: impl Into<String>) -> Self {
        Self {
            content: content.into(),
            style: Style::default(),
            highlighted_lines: None,
        }
    }

    fn line_count(&self) -> usize {
        self.content.split_terminator('\n').count().max(1)
    }

    fn content_width(&self) -> usize {
        self.content
            .split_terminator('\n')
            .map(display_width)
            .max()
            .unwrap_or(0)
            .max(1)
    }

    fn with_highlighting(mut self, highlighted_lines: Vec<Vec<TextSpan>>) -> Self {
        self.highlighted_lines = Some(highlighted_lines);
        self
    }
}

impl Renderable for CodeBlock {
    fn apply_style(&mut self, style: &Style) -> bool {
        self.style = *style;
        true
    }

    fn patch_retained(&self, other: &mut dyn Renderable) -> RenderablePatch {
        if let Some(other) = other.as_any_mut().downcast_mut::<Self>() {
            if self.content == other.content
                && self.style == other.style
                && self.highlighted_lines == other.highlighted_lines
            {
                RenderablePatch::NoChange
            } else {
                let layout_changed = self.content != other.content;
                other.content.clone_from(&self.content);
                other.style = self.style;
                other.highlighted_lines = self.highlighted_lines.clone();
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
        _style: &TaffyStyle,
        _known_dimensions: taffy::Size<Option<f32>>,
        _available_space: taffy::Size<AvailableSpace>,
    ) -> Size<f32> {
        Size {
            width: self.content_width() as f32,
            height: self.line_count() as f32,
        }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        let area = ctx.area();
        if area.width == 0 || area.height == 0 {
            return;
        }

        let attrs = ctx.style_to_attributes(&self.style);

        let scroll_y = ctx.scroll_y().max(0.0).floor() as usize;
        let skip_cols = ctx.scroll_x().max(0.0).round() as usize;

        let mut y = area.y;
        let mut remaining_rows = area.height;

        // If we have highlighted lines, render them with syntax highlighting
        if let Some(ref lines) = self.highlighted_lines {
            let mut rendered_any = false;
            for line in lines.iter().skip(scroll_y).take(area.height) {
                if remaining_rows == 0 {
                    break;
                }

                rendered_any = true;
                clear_line(ctx, area.x, y, area.width, &attrs);

                // Render each span in the line
                let mut x = area.x;
                let mut content_x = 0; // Position within the line's content
                let max_x = area.x + area.width;

                for span in line {
                    if x >= max_x {
                        break;
                    }

                    let span_text = &span.content;
                    let span_width = display_width(span_text);

                    // Skip columns if we're scrolled horizontally
                    if content_x + span_width <= skip_cols {
                        // Skip this span - advance content position only
                        // (skipped content is off the left edge, doesn't affect screen position)
                        content_x += span_width;
                        continue;
                    }

                    // Calculate how much of this span we can render
                    let start_skip = if skip_cols > content_x {
                        display_width_to_char_offset(span_text, skip_cols - content_x)
                    } else {
                        0
                    };

                    let remaining_width = max_x - x;
                    let end_take = if start_skip + remaining_width < span_width {
                        display_width_to_char_offset(span_text, start_skip + remaining_width)
                    } else {
                        span_text.len()
                    };

                    if start_skip < end_take {
                        let to_render = &span_text[start_skip..end_take];
                        let span_attrs = ctx.style_to_attributes(&span.style);
                        let merged_attrs = merge_attributes(&span_attrs, &attrs);
                        ctx.write_text_length(x, y, to_render, &merged_attrs, remaining_width);
                        x += display_width(to_render);
                    }
                    content_x += span_width;
                }

                y += 1;
                remaining_rows = remaining_rows.saturating_sub(1);
            }

            if !rendered_any && scroll_y == 0 && remaining_rows > 0 {
                clear_line(ctx, area.x, y, area.width, &attrs);
                y += 1;
                remaining_rows = remaining_rows.saturating_sub(1);
            }
        } else {
            // Original plain text rendering
            let mut rendered_any = false;
            for line in self
                .content
                .split_terminator('\n')
                .skip(scroll_y)
                .take(area.height)
            {
                if remaining_rows == 0 {
                    break;
                }

                rendered_any = true;
                clear_line(ctx, area.x, y, area.width, &attrs);

                let rendered = window_line(line, skip_cols, area.width);
                ctx.write_text_length(area.x, y, &rendered, &attrs, area.width);

                y += 1;
                remaining_rows = remaining_rows.saturating_sub(1);
            }

            if !rendered_any && scroll_y == 0 && remaining_rows > 0 {
                clear_line(ctx, area.x, y, area.width, &attrs);
                y += 1;
                remaining_rows = remaining_rows.saturating_sub(1);
            }
        }

        while remaining_rows > 0 {
            clear_line(ctx, area.x, y, area.width, &attrs);
            y += 1;
            remaining_rows = remaining_rows.saturating_sub(1);
        }
    }

    fn debug_label(&self) -> &'static str {
        "markdown-code-block"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

#[derive(Clone, Debug)]
pub(crate) struct CodeBlockRef<'a> {
    pub(crate) content: &'a str,
    pub(crate) style: Style,
    pub(crate) highlighted_lines: Option<Vec<Vec<TextSpan>>>,
}

impl<'a> CodeBlockRef<'a> {
    pub(crate) fn new(content: &'a str, style: Style) -> Self {
        Self {
            content,
            style,
            highlighted_lines: None,
        }
    }

    pub(crate) fn with_highlighting(mut self, highlighted_lines: Vec<Vec<TextSpan>>) -> Self {
        self.highlighted_lines = Some(highlighted_lines);
        self
    }
}

impl<'a> RenderableRef for CodeBlockRef<'a> {
    fn debug_label(&self) -> &'static str {
        "markdown-code-block"
    }

    fn patch_retained(&self, retained: &mut dyn Renderable) -> RenderablePatch {
        if let Some(block) = retained.as_any_mut().downcast_mut::<CodeBlock>() {
            if block.content == self.content
                && block.style == self.style
                && block.highlighted_lines.as_ref() == self.highlighted_lines.as_ref()
            {
                RenderablePatch::NoChange
            } else {
                let layout_changed = block.content != self.content;
                block.content = self.content.to_string();
                block.style = self.style;
                block.highlighted_lines = self.highlighted_lines.clone();
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
        let mut block = CodeBlock::new(self.content).with_style(self.style);
        if let Some(highlighted) = self.highlighted_lines {
            block = block.with_highlighting(highlighted);
        }
        Box::new(block)
    }
}

impl CodeBlock {
    fn with_style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
}

fn clear_line(
    ctx: &mut RenderContext<'_>,
    x: usize,
    y: usize,
    width: usize,
    attrs: &chatui::buffer::CellAttributes,
) {
    if width == 0 {
        return;
    }
    ctx.write_text(x, y, &" ".repeat(width), attrs);
}

fn display_width(text: &str) -> usize {
    let mut col = 0usize;
    for ch in text.chars() {
        if ch == '\t' {
            let next_tab_stop = ((col / TAB_WIDTH) + 1) * TAB_WIDTH;
            col = next_tab_stop;
            continue;
        }

        col += UnicodeWidthChar::width(ch).unwrap_or(0).max(1);
    }
    col
}

fn window_line(line: &str, skip_cols: usize, max_cols: usize) -> String {
    if max_cols == 0 {
        return String::new();
    }

    let mut out = String::new();
    let mut content_col = 0usize;
    let mut taken = 0usize;

    for ch in line.chars() {
        if taken >= max_cols {
            break;
        }

        if ch == '\t' {
            let tab_width = ((content_col / TAB_WIDTH) + 1) * TAB_WIDTH - content_col;
            for _ in 0..tab_width {
                if content_col >= skip_cols {
                    if taken >= max_cols {
                        return out;
                    }
                    out.push(' ');
                    taken += 1;
                }
                content_col += 1;
            }
            continue;
        }

        let w = UnicodeWidthChar::width(ch).unwrap_or(0).max(1);

        if content_col + w <= skip_cols {
            content_col += w;
            continue;
        }

        if taken + w > max_cols {
            break;
        }

        out.push(ch);
        taken += w;
        content_col += w;
    }

    out
}

fn merge_attributes(
    primary: &chatui::buffer::CellAttributes,
    base: &chatui::buffer::CellAttributes,
) -> chatui::buffer::CellAttributes {
    let mut merged = base.clone();
    // Primary style takes precedence for colors
    if primary.foreground.is_some() {
        merged.foreground = primary.foreground;
    }
    if primary.background.is_some() {
        merged.background = primary.background;
    }
    // For boolean attributes, use OR logic
    merged.bold = merged.bold || primary.bold;
    merged.dim = merged.dim || primary.dim;
    merged.reverse = merged.reverse || primary.reverse;
    merged
}

fn display_width_to_char_offset(text: &str, target_width: usize) -> usize {
    let mut col = 0usize;
    for (idx, ch) in text.char_indices() {
        if col >= target_width {
            return idx;
        }
        col += UnicodeWidthChar::width(ch).unwrap_or(0).max(1);
    }
    text.len()
}
