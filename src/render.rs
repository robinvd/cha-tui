use crate::buffer::{CellAttributes, CursorShape, DoubleBuffer};
use crate::dom::{
    Color, ElementKind, RetainedElementNode, RetainedNode, RetainedNodeContent, Style,
};
use crate::error::ProgramError;
use crate::event::Size;
use crate::geometry::{Point, Rect};
use crate::palette::{Palette, Rgba};

use unicode_width::UnicodeWidthChar;

use taffy::Layout as TaffyLayout;

/// Scroll state passed through the render tree
#[derive(Clone, Copy, Default)]
struct ScrollState {
    /// Vertical scroll offset for content
    scroll_y: f32,
    /// Horizontal scroll offset for content
    scroll_x: f32,
    /// Horizontal scroll offset for position adjustment (0 if not in scroll context)
    scroll_x_for_position: f32,
}

fn color_to_rgba(palette: &Palette, color: Color) -> Rgba {
    match color {
        Color::Reset => palette.foreground,
        Color::Rgba { r, g, b, a } => Rgba::new(r, g, b, a),
        Color::Palette(n) => palette.colors[n.min(15) as usize],
        Color::PaletteFg => palette.foreground,
        Color::PaletteBg => palette.background,
    }
}

fn style_to_attributes(palette: &Palette, style: &Style) -> CellAttributes {
    let mut attrs = CellAttributes::default();
    if let Some(fg) = style.fg {
        attrs.set_foreground(color_to_rgba(palette, fg));
    }
    if let Some(bg) = style.bg {
        attrs.set_background(color_to_rgba(palette, bg));
    }
    if style.bold {
        attrs.set_bold(true);
    } else if style.dim {
        attrs.set_dim(true);
    }
    if style.reverse {
        attrs.set_reverse(true);
    }
    attrs
}

pub struct RenderContext<'a> {
    buffer: &'a mut DoubleBuffer,
    palette: &'a Palette,
    layout: &'a TaffyLayout,
    origin: (usize, usize),
    area: Rect,
    inherited_scroll_y: f32,
    inherited_scroll_x: f32,
}

impl<'a> RenderContext<'a> {
    pub fn new(
        buffer: &'a mut DoubleBuffer,
        palette: &'a Palette,
        layout: &'a TaffyLayout,
        origin: (usize, usize),
        area: Rect,
        inherited_scroll_y: f32,
    ) -> Self {
        Self {
            buffer,
            palette,
            layout,
            origin,
            area,
            inherited_scroll_y,
            inherited_scroll_x: 0.0,
        }
    }

    pub fn layout(&self) -> &TaffyLayout {
        self.layout
    }

    pub fn origin(&self) -> (usize, usize) {
        self.origin
    }

    pub fn area(&self) -> Rect {
        self.area
    }

    pub fn buffer(&mut self) -> &mut DoubleBuffer {
        self.buffer
    }

    pub fn palette(&self) -> &Palette {
        self.palette
    }

    pub fn style_to_attributes(&self, style: &Style) -> CellAttributes {
        style_to_attributes(self.palette, style)
    }

    pub fn write_text(&mut self, x: usize, y: usize, text: &str, attrs: &CellAttributes) {
        self.buffer.write_text(x, y, text, attrs);
    }

    pub fn write_text_length(
        &mut self,
        x: usize,
        y: usize,
        text: &str,
        attrs: &CellAttributes,
        max_cells: usize,
    ) {
        self.buffer.write_text_length(x, y, text, attrs, max_cells);
    }

    pub fn write_char(&mut self, x: usize, y: usize, ch: char, attrs: &CellAttributes) {
        self.buffer.write_char(x, y, ch, attrs);
    }

    pub fn clear_area(&mut self, rect: Rect) {
        self.buffer.clear_area(rect);
    }

    pub fn set_cursor(&mut self, x: usize, y: usize, shape: CursorShape) {
        self.buffer.set_cursor(x, y, shape);
    }

    pub fn clear_cursor(&mut self) {
        self.buffer.clear_cursor();
    }

    pub fn scroll_y(&self) -> f32 {
        self.inherited_scroll_y
    }

    pub fn scroll_x(&self) -> f32 {
        self.inherited_scroll_x
    }

    pub fn sub_context<'b>(
        &'b mut self,
        origin: (usize, usize),
        area: Rect,
        inherited_scroll_y: f32,
        inherited_scroll_x: f32,
    ) -> RenderContext<'b> {
        RenderContext {
            buffer: self.buffer,
            palette: self.palette,
            layout: self.layout,
            origin,
            area,
            inherited_scroll_y,
            inherited_scroll_x,
        }
    }
}

pub struct Renderer<'a> {
    buffer: &'a mut DoubleBuffer,
    palette: &'a Palette,
}

const SCROLLBAR_TRACK_CHAR: char = ' ';
const SCROLLBAR_THUMB_CHAR: char = '█';
const SCROLLBAR_HORZ_THUMB_CHAR: char = '▀';

impl<'a> Renderer<'a> {
    pub fn new(buffer: &'a mut DoubleBuffer, palette: &'a Palette) -> Self {
        Self { buffer, palette }
    }

    pub fn render<Msg>(
        &mut self,
        root: &RetainedNode<Msg>,
        size: Size,
    ) -> Result<(), ProgramError> {
        let width = size.width as usize;
        let height = size.height as usize;

        self.buffer.resize(width, height);
        self.buffer.clear();
        self.buffer.clear_cursor();

        let clip = Rect {
            x: 0,
            y: 0,
            width,
            height,
        };
        self.render_node(
            root,
            Point { x: 0, y: 0 },
            clip,
            ScrollState::default(),
            false,
        );

        Ok(())
    }

    fn render_node<Msg>(
        &mut self,
        node: &RetainedNode<Msg>,
        parent_origin: Point,
        clip: Rect,
        scroll: ScrollState,
        clear: bool,
    ) {
        let layout = node.layout_state.layout;
        let is_scroll_y = node.layout_state.style.overflow.y == taffy::Overflow::Scroll;
        let is_scroll_x = node.layout_state.style.overflow.x == taffy::Overflow::Scroll;
        let child_scroll_y = (scroll.scroll_y - layout.location.y).max(0.0);
        let child_scroll_x = scroll.scroll_x.max(0.0);
        // Use scroll_x_for_position to adjust screen position
        let adjusted_x = (layout.location.x - scroll.scroll_x_for_position).max(0.0);
        let node_origin = Point {
            x: parent_origin.x + adjusted_x as usize,
            y: parent_origin.y + (layout.location.y - scroll.scroll_y).max(0.0) as usize,
        };

        let Some(mut area) = rect_from_layout(&layout, node_origin) else {
            return;
        };
        if !area.has_area() {
            return;
        }

        area = area.intersection(clip);
        if !area.has_area() {
            return;
        }

        if clear {
            self.buffer.clear_area(area);
        }

        match &node.content {
            RetainedNodeContent::Element(element) => {
                let border_style =
                    if matches!(element.kind, ElementKind::Block) && element.attrs.style.border {
                        Some(&element.attrs.style)
                    } else {
                        None
                    };
                self.apply_overlay_style(area, &element.attrs.style);
                let next_scroll_y = if is_scroll_y {
                    child_scroll_y + node.scroll_y
                } else {
                    child_scroll_y
                };
                let next_scroll_x = if is_scroll_x {
                    child_scroll_x + node.scroll_x
                } else {
                    child_scroll_x
                };
                let child_scroll = ScrollState {
                    scroll_y: next_scroll_y,
                    scroll_x: next_scroll_x,
                    scroll_x_for_position: 0.0, // Will be set by render_children if needed
                };
                self.render_element(element, node_origin, area, child_scroll, is_scroll_x);
                if is_scroll_y {
                    self.render_scrollbar(node_origin, area, &layout, node.scroll_y, border_style);
                }
                if is_scroll_x {
                    self.render_hscrollbar(node_origin, area, &layout, node.scroll_x, border_style);
                }
            }
            RetainedNodeContent::Renderable(leaf) => {
                let leaf_scroll_y = child_scroll_y + node.scroll_y;
                let mut ctx = RenderContext::new(
                    self.buffer,
                    self.palette,
                    &layout,
                    (node_origin.x, node_origin.y),
                    area,
                    leaf_scroll_y,
                );
                // Override horizontal scroll into the leaf context if this node scrolls horizontally
                ctx.inherited_scroll_x = if is_scroll_x {
                    child_scroll_x + node.scroll_x
                } else {
                    child_scroll_x
                };
                leaf.render(&mut ctx);
            }
        }
    }

    fn render_element<Msg>(
        &mut self,
        element: &RetainedElementNode<Msg>,
        parent_origin: Point,
        clip: Rect,
        scroll: ScrollState,
        adjust_child_scroll_by_position: bool,
    ) {
        match element.kind {
            ElementKind::Block => self.render_block(element, parent_origin, clip, scroll),
            ElementKind::Modal => self.render_modal(element, parent_origin, clip, scroll.scroll_y),
            ElementKind::Column | ElementKind::Row | ElementKind::Table => self.render_children(
                &element.children,
                parent_origin,
                clip,
                scroll,
                false,
                adjust_child_scroll_by_position,
            ),
        }
    }

    fn render_children<Msg>(
        &mut self,
        children: &[RetainedNode<Msg>],
        parent_origin: Point,
        clip: Rect,
        scroll: ScrollState,
        clear: bool,
        adjust_scroll_by_position: bool,
    ) {
        for child in children {
            let child_scroll = if adjust_scroll_by_position {
                let child_x = child.layout_state.layout.location.x;
                ScrollState {
                    scroll_y: scroll.scroll_y,
                    // Content scroll: reduced by child's position
                    scroll_x: (scroll.scroll_x - child_x).max(0.0),
                    // Position scroll: full scroll, used to shift child's screen position
                    scroll_x_for_position: scroll.scroll_x,
                }
            } else {
                // No adjustment: pass scroll as-is, no position adjustment
                ScrollState {
                    scroll_y: scroll.scroll_y,
                    scroll_x: scroll.scroll_x,
                    scroll_x_for_position: 0.0,
                }
            };
            self.render_node(child, parent_origin, clip, child_scroll, clear);
        }
    }

    fn render_scrollbar(
        &mut self,
        parent_origin: Point,
        clip: Rect,
        layout: &TaffyLayout,
        scroll_offset: f32,
        block_border_style: Option<&Style>,
    ) {
        let _ = parent_origin;

        let viewport_height = layout.size.height.max(0.0);
        if viewport_height <= 0.0 {
            return;
        }

        let content_height = layout.content_size.height.max(viewport_height);
        if content_height <= viewport_height + f32::EPSILON {
            return;
        }

        let raw_scrollbar_width = layout.scrollbar_size.width.max(0.0).round() as usize;
        let share_border_column = block_border_style.is_some() && raw_scrollbar_width <= 1;
        let scrollbar_width = if share_border_column {
            raw_scrollbar_width.max(1)
        } else {
            raw_scrollbar_width
        };

        if scrollbar_width == 0 || clip.width == 0 || clip.height == 0 {
            return;
        }
        if clip.width < scrollbar_width {
            return;
        }

        let track_top = if share_border_column {
            clip.y.saturating_add(1)
        } else {
            clip.y
        };
        let track_height = if share_border_column {
            clip.height.saturating_sub(2)
        } else {
            clip.height
        };
        if track_height == 0 {
            return;
        }

        let scrollbar_x = clip.x + clip.width - scrollbar_width;
        if !share_border_column {
            let track_attrs = CellAttributes::default();
            for y in clip.y..(clip.y + track_height) {
                for x in scrollbar_x..(scrollbar_x + scrollbar_width) {
                    self.write_char(x, y, SCROLLBAR_TRACK_CHAR, &track_attrs);
                }
            }
        }

        let max_scroll = (content_height - viewport_height).max(0.0);
        let clamped_scroll = if max_scroll <= f32::EPSILON {
            0.0
        } else {
            scroll_offset.clamp(0.0, max_scroll)
        };

        let mut thumb_height =
            ((track_height as f32 * (viewport_height / content_height)).floor() as usize).max(1);
        if thumb_height > track_height {
            thumb_height = track_height;
        }

        let travel = track_height.saturating_sub(thumb_height);
        let thumb_top_offset = if travel == 0 || max_scroll <= f32::EPSILON {
            0
        } else {
            ((clamped_scroll / max_scroll) * travel as f32).round() as usize
        };
        let thumb_top = track_top + thumb_top_offset;
        let thumb_bottom = thumb_top
            .saturating_add(thumb_height)
            .min(track_top + track_height);

        let mut thumb_attrs = match block_border_style {
            Some(style) => style_to_attributes(self.palette, style),
            None => CellAttributes::default(),
        };
        thumb_attrs.set_bold(true);

        for y in thumb_top..thumb_bottom {
            for x in scrollbar_x..(scrollbar_x + scrollbar_width) {
                self.write_char(x, y, SCROLLBAR_THUMB_CHAR, &thumb_attrs);
            }
        }
    }

    fn render_hscrollbar(
        &mut self,
        parent_origin: Point,
        clip: Rect,
        layout: &TaffyLayout,
        scroll_offset: f32,
        block_border_style: Option<&Style>,
    ) {
        let _ = parent_origin;

        let viewport_width = layout.size.width.max(0.0);
        if viewport_width <= 0.0 {
            return;
        }

        let content_width = layout.content_size.width.max(viewport_width);
        if content_width <= viewport_width + f32::EPSILON {
            return;
        }

        let raw_scrollbar_height = layout.scrollbar_size.height.max(0.0).round() as usize;
        let share_border_row = block_border_style.is_some() && raw_scrollbar_height <= 1;
        let scrollbar_height = if share_border_row {
            raw_scrollbar_height.max(1)
        } else {
            raw_scrollbar_height
        };

        if scrollbar_height == 0 || clip.width == 0 || clip.height == 0 {
            return;
        }
        if clip.height < scrollbar_height {
            return;
        }

        let track_left = if share_border_row { clip.x + 1 } else { clip.x };
        let track_width = if share_border_row {
            clip.width.saturating_sub(2)
        } else {
            clip.width
        };
        if track_width == 0 {
            return;
        }

        let scrollbar_y = clip.y + clip.height - scrollbar_height;
        if !share_border_row {
            let track_attrs = CellAttributes::default();
            for y in scrollbar_y..(scrollbar_y + scrollbar_height) {
                for x in track_left..(track_left + track_width) {
                    self.write_char(x, y, SCROLLBAR_TRACK_CHAR, &track_attrs);
                }
            }
        }

        let max_scroll = (content_width - viewport_width).max(0.0);
        let clamped_scroll = if max_scroll <= f32::EPSILON {
            0.0
        } else {
            scroll_offset.clamp(0.0, max_scroll)
        };

        let mut thumb_width =
            ((track_width as f32 * (viewport_width / content_width)).floor() as usize).max(1);
        if thumb_width > track_width {
            thumb_width = track_width;
        }

        let travel = track_width.saturating_sub(thumb_width);
        let thumb_left_offset = if travel == 0 || max_scroll <= f32::EPSILON {
            0
        } else {
            ((clamped_scroll / max_scroll) * travel as f32).round() as usize
        };
        let thumb_left = track_left + thumb_left_offset;
        let thumb_right = thumb_left
            .saturating_add(thumb_width)
            .min(track_left + track_width);

        let mut thumb_attrs = match block_border_style {
            Some(style) => style_to_attributes(self.palette, style),
            None => CellAttributes::default(),
        };
        thumb_attrs.set_bold(true);

        for y in scrollbar_y..(scrollbar_y + scrollbar_height) {
            for x in thumb_left..thumb_right {
                self.write_char(x, y, SCROLLBAR_HORZ_THUMB_CHAR, &thumb_attrs);
            }
        }
    }

    fn render_modal<Msg>(
        &mut self,
        element: &RetainedElementNode<Msg>,
        parent_origin: Point,
        clip: Rect,
        scroll_y: f32,
    ) {
        self.apply_overlay_style(clip, &element.attrs.style);
        let scroll = ScrollState {
            scroll_y,
            scroll_x: 0.0,
            scroll_x_for_position: 0.0,
        };
        self.render_children(&element.children, parent_origin, clip, scroll, true, false);
    }

    fn render_block<Msg>(
        &mut self,
        element: &RetainedElementNode<Msg>,
        parent_origin: Point,
        clip: Rect,
        scroll: ScrollState,
    ) {
        self.draw_border(clip, &element.attrs.style);
        if let Some(title) = &element.title {
            self.render_block_title(title, parent_origin, clip, &element.attrs.style);
        }
        if clip.width <= 1 || clip.height <= 2 {
            return;
        }
        let child_origin = Point {
            x: parent_origin.x,
            y: parent_origin.y,
        };
        let child_area = Rect {
            x: clip.x,
            y: clip.y + 1,
            width: clip.width.saturating_sub(1),
            height: clip.height.saturating_sub(2),
        };
        self.render_children(
            &element.children,
            child_origin,
            child_area,
            scroll,
            false,
            false, // Don't adjust scroll by position for block children
        );
    }

    fn draw_border(&mut self, area: Rect, style: &Style) {
        if area.width < 1 || area.height < 1 {
            return;
        }

        let attrs = style_to_attributes(self.palette, style);

        // Top border
        self.write_char(area.x, area.y, '┌', &attrs);
        for x in (area.x + 1)..(area.x + area.width - 1) {
            self.write_char(x, area.y, '─', &attrs);
        }
        if area.width > 1 {
            self.write_char(area.x + area.width - 1, area.y, '┐', &attrs);
        }

        // Bottom border
        if area.height > 1 {
            let bottom = area.y + area.height - 1;
            self.write_char(area.x, bottom, '└', &attrs);
            for x in (area.x + 1)..(area.x + area.width - 1) {
                self.write_char(x, bottom, '─', &attrs);
            }
            if area.width > 1 {
                self.write_char(area.x + area.width - 1, bottom, '┘', &attrs);
            }

            // Side borders
            for y in (area.y + 1)..bottom {
                self.write_char(area.x, y, '│', &attrs);
                if area.width > 1 {
                    self.write_char(area.x + area.width - 1, y, '│', &attrs);
                }
            }
        }
    }

    fn render_block_title(
        &mut self,
        title: &str,
        _parent_origin: Point,
        clip: Rect,
        style: &Style,
    ) {
        if clip.width <= 2 {
            return;
        }

        let mut rendered = String::new();
        let mut available_columns = clip.width.saturating_sub(2);

        for ch in title.chars() {
            let width = UnicodeWidthChar::width(ch).unwrap_or(0);

            if width > available_columns && width > 0 {
                break;
            }

            rendered.push(ch);
            if width > 0 {
                available_columns = available_columns.saturating_sub(width);
            }
        }

        if rendered.is_empty() {
            return;
        }

        let attrs = style_to_attributes(self.palette, style);
        self.buffer
            .write_text(clip.x + 1, clip.y, &rendered, &attrs);
    }

    fn write_char(&mut self, x: usize, y: usize, ch: char, attrs: &CellAttributes) {
        self.buffer.write_char(x, y, ch, attrs);
    }

    fn apply_overlay_style(&mut self, area: Rect, style: &Style) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        // Convert colors before the loop to avoid borrow checker issues
        let fg_color = style.fg.map(|c| color_to_rgba(self.palette, c));
        let bg_color = style.bg.map(|c| color_to_rgba(self.palette, c));

        for y in area.y..(area.y + area.height).min(self.buffer.dimensions().1) {
            for x in area.x..(area.x + area.width).min(self.buffer.dimensions().0) {
                // Get the existing cell character to preserve it
                let ch = self.buffer.get_cell(x, y).map(|c| c.ch).unwrap_or(' ');

                // Build new attributes based on style and existing cell
                let mut attrs = self
                    .buffer
                    .get_cell(x, y)
                    .map(|c| c.attrs.clone())
                    .unwrap_or_default();

                if let Some(fg) = fg_color {
                    attrs.set_foreground(fg);
                }
                if let Some(bg) = bg_color {
                    attrs.set_background(bg);
                }
                if style.bold {
                    attrs.set_bold(true);
                } else if style.dim {
                    attrs.set_dim(true);
                }
                if style.reverse {
                    attrs.set_reverse(true);
                }

                // Use write_char which will handle color blending if needed
                self.buffer.write_char(x, y, ch, &attrs);
            }
        }
    }

    pub fn buffer(&self) -> &DoubleBuffer {
        self.buffer
    }

    pub fn buffer_mut(&mut self) -> &mut DoubleBuffer {
        self.buffer
    }
}

fn rect_from_layout(layout: &TaffyLayout, origin: Point) -> Option<Rect> {
    let width = layout.size.width.max(0.0).round() as usize;
    let height = layout.size.height.max(0.0).round() as usize;

    if width == 0 || height == 0 {
        return None;
    }

    let x = origin.x;
    let y = origin.y;

    Some(Rect {
        x,
        y,
        width,
        height,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::scroll::{ScrollBehavior, ScrollMsg, ScrollState, scrollable_content};
    use crate::dom::rounding::round_layout;
    use crate::dom::{
        Color, Node, RetainedNode, Style, TableColumn, TableColumnWidth, TableRow, TextSpanRef,
        block, block_with_title, column, modal, rich_text, row, table, text,
    };
    use crate::palette::Palette;
    use crate::paragraph;
    use taffy::prelude::TaffyZero;
    use taffy::{AvailableSpace, Dimension, compute_root_layout};

    fn prepare_layout<Msg>(node: &mut RetainedNode<Msg>, size: Size) {
        compute_root_layout(
            node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(size.width as f32),
                height: AvailableSpace::Definite(size.height as f32),
            },
        );
        round_layout(node);
    }

    fn render_to_lines<Msg>(
        node: impl Into<RetainedNode<Msg>>,
        width: usize,
        height: usize,
    ) -> Vec<String> {
        let mut node: RetainedNode<Msg> = node.into();
        let size = Size::new(
            width.try_into().expect("width fits in u16"),
            height.try_into().expect("height fits in u16"),
        );
        prepare_layout(&mut node, size);

        let mut buffer = DoubleBuffer::new(width, height);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer.render(&node, size).expect("render should succeed");

        renderer
            .buffer()
            .to_string()
            .lines()
            .map(|line| line.to_string())
            .collect()
    }

    fn grapheme_width(text: &str) -> usize {
        text.chars()
            .map(|ch| UnicodeWidthChar::width(ch).unwrap_or(0).max(1))
            .sum()
    }

    fn render_scrollable_lines(scroll_offset: f32) -> Vec<String> {
        let mut buffer = DoubleBuffer::new(6, 5);
        let palette = Palette::default();
        let lines: Vec<Node<()>> = (0..20)
            .map(|idx| text::<()>(format!("Line {idx}")))
            .collect();
        let scrollable = column(lines)
            .with_scroll(scroll_offset)
            .with_height(Dimension::length(5.0))
            .with_width(Dimension::percent(1.0));
        let mut root: RetainedNode<()> = column(vec![scrollable])
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0))
            .into();
        prepare_layout(&mut root, Size::new(6, 5));

        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer
            .render(&root, Size::new(6, 5))
            .expect("render should succeed");

        renderer
            .buffer()
            .to_string()
            .lines()
            .map(|line| line.to_string())
            .collect()
    }

    fn render_scrollable_block_lines(scroll_offset: f32) -> Vec<String> {
        let mut buffer = DoubleBuffer::new(8, 7);
        let palette = Palette::default();
        let lines: Vec<Node<()>> = (0..20)
            .map(|idx| text::<()>(format!("Item {idx}")))
            .collect();
        let scrollable = block::<()>(vec![column(lines)])
            .with_scroll(scroll_offset)
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));
        let mut root: RetainedNode<()> = scrollable.into();
        prepare_layout(&mut root, Size::new(8, 7));

        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer
            .render(&root, Size::new(8, 7))
            .expect("render should succeed");

        renderer
            .buffer()
            .to_string()
            .lines()
            .map(|line| line.to_string())
            .collect()
    }

    fn render_paragraph_with_scroll(scroll_offset: f32) -> Vec<String> {
        let node = paragraph::<()>("L0\nL1\nL2\nL3\nL4\nL5")
            .with_scroll(scroll_offset)
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));

        render_to_lines(node, 8, 3)
    }

    fn render_scrollable_paragraph(scroll_offset: f32) -> Vec<String> {
        let mut scroll_state = ScrollState::new(ScrollBehavior::Vertical);
        let content_lines: Vec<String> = (0..8).map(|idx| format!("Line {idx}")).collect();
        let content = content_lines.join("\n");
        let line_count = content_lines.len() as u16;
        scroll_state.update(ScrollMsg::Resize {
            viewport: Size {
                width: 32,
                height: 4,
            },
            content: Size {
                width: 32,
                height: line_count,
            },
        });
        scroll_state.set_offset(scroll_offset);

        let node = scrollable_content(
            "chat",
            &scroll_state,
            3,
            |_: ScrollMsg| (),
            block(vec![paragraph::<()>(content)])
                .with_width(Dimension::percent(1.0))
                .with_height(Dimension::percent(1.0)),
        )
        .with_width(Dimension::percent(1.0))
        .with_height(Dimension::percent(1.0));

        render_to_lines(node, 32, 6)
    }

    fn render_scrollable_multi_paragraph(scroll_offset: f32) -> Vec<String> {
        let mut scroll_state = ScrollState::new(ScrollBehavior::Vertical);
        let paragraphs = vec![paragraph::<()>("A0\nA1\nA2"), paragraph::<()>("B0\nB1\nB2")];
        let total_lines: u16 = 6;
        scroll_state.update(ScrollMsg::Resize {
            viewport: Size {
                width: 16,
                height: 4,
            },
            content: Size {
                width: 16,
                height: total_lines,
            },
        });
        scroll_state.set_offset(scroll_offset);

        let node = scrollable_content(
            "multi",
            &scroll_state,
            3,
            |_: ScrollMsg| (),
            column(paragraphs).with_min_height(Dimension::ZERO),
        )
        .with_width(Dimension::percent(1.0))
        .with_height(Dimension::percent(1.0));

        render_to_lines(node, 16, 4)
    }

    fn thumb_rows(lines: &[String]) -> Vec<usize> {
        lines
            .iter()
            .enumerate()
            .filter_map(|(idx, line)| {
                if line.ends_with(SCROLLBAR_THUMB_CHAR) {
                    Some(idx)
                } else {
                    None
                }
            })
            .collect()
    }

    fn average(rows: &[usize]) -> f32 {
        rows.iter().copied().sum::<usize>() as f32 / rows.len() as f32
    }

    #[test]
    fn renders_text_node() {
        let mut buffer = DoubleBuffer::new(10, 4);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let node = text::<()>("hello");
        let node: RetainedNode<()> = node.into();
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(10, 4));

        renderer
            .render(&node, Size::new(10, 4))
            .expect("render should succeed");

        let contents = renderer.buffer.to_string();
        assert!(contents.starts_with("hello"));
    }

    #[test]
    fn renders_rich_text_spans_with_styles() {
        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let spans = vec![
            TextSpanRef::new("+", Style::fg(Color::Green)),
            TextSpanRef::new("fn", Style::fg(Color::Cyan)),
        ];
        let node = rich_text::<()>(spans);
        let node: RetainedNode<()> = node.into();
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(10, 2));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let back_buffer = renderer.buffer().back_buffer();
        let prefix = &back_buffer[0][0];
        assert_eq!(prefix.ch, '+');
        // Green color (0, 205, 0)
        assert_eq!(prefix.attrs.foreground(), Some(Rgba::opaque(0, 205, 0)));
        let keyword = &back_buffer[0][1];
        assert_eq!(keyword.ch, 'f');
        // Cyan color (0, 205, 205)
        assert_eq!(keyword.attrs.foreground(), Some(Rgba::opaque(0, 205, 205)));
    }

    #[test]
    fn rich_text_respects_horizontal_scroll() {
        let mut buffer = DoubleBuffer::new(6, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let spans = vec![TextSpanRef::new("abcdef", Style::fg(Color::Red))];
        let node = rich_text::<()>(spans).with_scroll_x(2.0);
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(6, 1));

        renderer
            .render(&node, Size::new(6, 1))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        assert_eq!(first_line, "cdef  ");
    }

    #[test]
    fn text_node_expands_tabs_to_spaces() {
        let node = text::<()>("a\tb").with_width(Dimension::length(16.0));

        let lines = render_to_lines(node, 16, 1);
        let line = &lines[0];

        assert!(line.starts_with("a       b"), "unexpected line: {line:?}");
        assert!(
            !line.contains('\t'),
            "rendered output should not contain raw tab characters: {line:?}"
        );
    }

    #[test]
    fn tab_advances_to_next_tab_stop() {
        let node = text::<()>("abc\tdef").with_width(Dimension::length(16.0));

        let lines = render_to_lines(node, 16, 1);
        let line = &lines[0];
        let def_index = line.find("def").expect("expected def in rendered line");

        assert_eq!(
            def_index, 8,
            "tab should advance content to the next tab stop (8-column alignment)"
        );
    }

    fn render_dual_scroll_block_lines(x_scroll: f32, y_scroll: f32) -> Vec<String> {
        let mut buffer = DoubleBuffer::new(12, 8);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        // Many long lines to overflow both width and height
        let lines: Vec<Node<()>> = (0..30)
            .map(|idx| text::<()>(format!("Item {idx} 1234567890abcdef")))
            .collect();
        let col = column(lines).with_flex_shrink(0.);
        let root = block::<()>(vec![col])
            .with_scroll(y_scroll)
            .with_scroll_x(x_scroll)
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));

        let root: RetainedNode<()> = root.into();
        let mut root: RetainedNode<()> = root.into();
        prepare_layout(&mut root, Size::new(12, 8));

        renderer
            .render(&root, Size::new(12, 8))
            .expect("render should succeed");

        renderer
            .buffer()
            .to_string()
            .lines()
            .map(|line| line.to_string())
            .collect()
    }

    #[test]
    fn block_renders_both_scrollbars() {
        let lines = render_dual_scroll_block_lines(4.0, 5.0);
        assert!(!lines.is_empty(), "expected rendered lines");
        // Rightmost column contains vertical scrollbar thumb in interior rows
        let right_column: Vec<char> = lines
            .iter()
            .filter_map(|line| line.chars().last())
            .collect();
        let interior = &right_column[1..right_column.len().saturating_sub(1)];
        assert!(
            interior.contains(&SCROLLBAR_THUMB_CHAR),
            "expected vertical thumb present in right border column"
        );
        // Bottom row contains horizontal scrollbar thumb somewhere in interior columns
        let bottom = lines.last().expect("expected bottom line");
        assert!(
            bottom.contains(SCROLLBAR_HORZ_THUMB_CHAR),
            "expected horizontal thumb present in bottom row"
        );
    }

    #[test]
    fn table_renders_header_and_rows_aligned() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("Name").into()),
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("Role").into()),
        ];
        let rows = vec![
            TableRow::new(vec![
                text::<()>("Alice").into(),
                text::<()>("Engineer").into(),
            ]),
            TableRow::new(vec![
                text::<()>("Bob").into(),
                text::<()>("Designer").into(),
            ]),
        ];
        let table = table(columns, rows);

        let lines = render_to_lines(table, 18, 4);
        assert!(lines[0].contains("Name"));
        assert!(lines[0].contains("Role"));
        assert!(lines[1].contains("Alice"));
        assert!(lines[1].contains("Engineer"));
        assert!(lines[2].contains("Bob"));
        assert!(lines[2].contains("Designer"));

        let name_col = lines[0].find("Name").expect("header Name present");
        let role_col = lines[0].find("Role").expect("header Role present");
        let alice_col = lines[1].find("Alice").expect("row Alice present");
        let engineer_col = lines[1].find("Engineer").expect("row Engineer present");
        let bob_col = lines[2].find("Bob").expect("row Bob present");
        let designer_col = lines[2].find("Designer").expect("row Designer present");

        assert_eq!(name_col, alice_col, "first column aligned");
        assert_eq!(name_col, bob_col, "first column aligned across rows");
        assert_eq!(role_col, engineer_col, "second column aligned");
        assert_eq!(role_col, designer_col, "second column aligned across rows");
    }

    #[test]
    fn table_respects_fixed_column_width() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Fixed(6.0)).with_header(text::<()>("Key").into()),
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("Value").into()),
        ];
        let rows = vec![
            TableRow::new(vec![text::<()>("A").into(), text::<()>("One").into()]),
            TableRow::new(vec![text::<()>("B").into(), text::<()>("Two").into()]),
        ];
        let table = table(columns, rows);

        let lines = render_to_lines(table, 12, 4);
        let value_col = lines[0].find("Value").expect("header Value present");
        let one_col = lines[1].find("One").expect("row One present");
        let two_col = lines[2].find("Two").expect("row Two present");

        assert_eq!(value_col, one_col, "fixed width column aligns cells");
        assert_eq!(value_col, two_col, "fixed width column aligns cells");
        assert!(
            value_col >= 6,
            "Value column should start after fixed-width first column"
        );

        let first_row = &lines[1];
        assert_eq!(
            first_row.chars().nth(0),
            Some('A'),
            "first column retains its content"
        );
        assert_eq!(
            first_row.chars().nth(1),
            Some(' '),
            "fixed width column pads remaining space"
        );
    }

    #[test]
    fn table_with_gap_spacing() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("A").into()),
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("B").into()),
        ];
        let rows = vec![
            TableRow::new(vec![text::<()>("X").into(), text::<()>("Y").into()]),
            TableRow::new(vec![text::<()>("Z").into(), text::<()>("W").into()]),
        ];
        let table = table(columns, rows).with_gap(2, 1);

        let lines = render_to_lines(table, 10, 5);

        // With a gap of 2 columns, the second column should be further right
        let a_pos = lines[0].find('A').expect("A present");
        let b_pos = lines[0].find('B').expect("B present");

        // Gap should create separation between columns
        assert!(b_pos > a_pos + 1, "Column gap should separate columns");
    }

    #[test]
    fn table_without_header_renders_only_data() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto),
            TableColumn::new(TableColumnWidth::Auto),
        ];
        let rows = vec![
            TableRow::new(vec![text::<()>("Data1").into(), text::<()>("Data2").into()]),
            TableRow::new(vec![text::<()>("Data3").into(), text::<()>("Data4").into()]),
        ];
        let table = table(columns, rows);

        let lines = render_to_lines(table, 15, 3);

        // First line should contain first row data, not headers
        assert!(
            lines[0].contains("Data1"),
            "First line should have first row data"
        );
        assert!(
            lines[0].contains("Data2"),
            "First line should have first row data"
        );
        assert!(
            lines[1].contains("Data3"),
            "Second line should have second row data"
        );
        assert!(
            lines[1].contains("Data4"),
            "Second line should have second row data"
        );
    }

    #[test]
    fn table_single_column() {
        let columns =
            vec![TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("Column").into())];
        let rows = vec![
            TableRow::new(vec![text::<()>("Row1").into()]),
            TableRow::new(vec![text::<()>("Row2").into()]),
            TableRow::new(vec![text::<()>("Row3").into()]),
        ];
        let table = table(columns, rows);

        let lines = render_to_lines(table, 10, 5);

        assert!(lines[0].contains("Column"), "Header should render");
        assert!(lines[1].contains("Row1"), "First row should render");
        assert!(lines[2].contains("Row2"), "Second row should render");
        assert!(lines[3].contains("Row3"), "Third row should render");
    }

    #[test]
    fn table_with_mixed_column_widths() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Fixed(5.0)).with_header(text::<()>("ID").into()),
            TableColumn::new(TableColumnWidth::Flexible(1.0))
                .with_header(text::<()>("Name").into()),
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("Val").into()),
        ];
        let rows = vec![
            TableRow::new(vec![
                text::<()>("1").into(),
                text::<()>("Alice").into(),
                text::<()>("X").into(),
            ]),
            TableRow::new(vec![
                text::<()>("2").into(),
                text::<()>("Bob").into(),
                text::<()>("Y").into(),
            ]),
        ];
        let table = table(columns, rows);

        let lines = render_to_lines(table, 20, 4);

        // Verify all content is present
        assert!(lines[0].contains("ID"), "ID header present");
        assert!(lines[0].contains("Name"), "Name header present");
        assert!(lines[0].contains("Val"), "Val header present");
        assert!(lines[1].contains("Alice"), "First row data present");
        assert!(lines[2].contains("Bob"), "Second row data present");
    }

    #[test]
    fn table_empty_with_headers_only() {
        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("Col1").into()),
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("Col2").into()),
        ];
        let rows = vec![];
        let table = table(columns, rows);

        let lines = render_to_lines(table, 15, 2);

        // Only headers should render
        assert!(lines[0].contains("Col1"), "First header present");
        assert!(lines[0].contains("Col2"), "Second header present");
    }

    #[test]
    fn table_in_flexbox_container() {
        // Regression test for bug where grid layout inside flexbox would panic with
        // "text has no children" when trying to measure text nodes as grid cells.
        use crate::dom::{column, text};

        let columns = vec![
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("A").into()),
            TableColumn::new(TableColumnWidth::Auto).with_header(text::<()>("B").into()),
        ];
        let rows = vec![
            TableRow::new(vec![text::<()>("1").into(), text::<()>("2").into()]),
            TableRow::new(vec![text::<()>("3").into(), text::<()>("4").into()]),
        ];
        let table_node = table(columns, rows);

        // Wrap table in a column (flexbox container)
        let view = column(vec![text::<()>("Title"), table_node.into_node()]);

        // This should not panic - before the fix in child_count(), this would panic
        let lines = render_to_lines(view, 20, 6);
        assert!(lines.len() > 0, "Should render without panicking");

        // ACTUALLY CHECK THE CONTENT
        let all_content = lines.join("\n");
        eprintln!("Table in flexbox content:\n{}", all_content);
        assert!(all_content.contains("A"), "Should contain header A");
        assert!(all_content.contains("1"), "Should contain data 1");
    }

    #[test]
    fn fullwidth_char_windowing_contains_wide_char() {
        let mut buffer = DoubleBuffer::new(6, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let spans = vec![TextSpanRef::new("ab漢cdef", Style::fg(Color::Yellow))];
        let node = rich_text::<()>(spans).with_scroll_x(2.0);
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(6, 1));

        renderer
            .render(&node, Size::new(6, 1))
            .expect("render should succeed");
        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        assert!(first_line.contains('漢'));
    }

    #[test]
    fn rich_text_respects_width_clipping() {
        let mut buffer = DoubleBuffer::new(4, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let spans = vec![
            TextSpanRef::new("abc", Style::fg(Color::Red)),
            TextSpanRef::new("def", Style::fg(Color::Green)),
        ];
        let node = rich_text::<()>(spans);
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(4, 2));

        renderer
            .render(&node, Size::new(4, 2))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        assert_eq!(first_line, "abcd");
    }

    #[test]
    fn rect_intersection_clips_to_overlap() {
        let parent = Rect {
            x: 0,
            y: 0,
            width: 6,
            height: 4,
        };
        let child = Rect {
            x: 4,
            y: 1,
            width: 5,
            height: 3,
        };

        let intersection = child.intersection(parent);

        assert_eq!(intersection.x, 4);
        assert_eq!(intersection.width, 2);
        assert_eq!(intersection.y, 1);
        assert_eq!(intersection.height, 3);
    }

    #[test]
    fn rect_intersection_handles_non_overlapping_rects() {
        let a = Rect {
            x: 0,
            y: 0,
            width: 3,
            height: 3,
        };
        let b = Rect {
            x: 5,
            y: 5,
            width: 2,
            height: 2,
        };

        let intersection = a.intersection(b);

        assert_eq!(intersection.x, 5);
        assert_eq!(intersection.y, 5);
        assert_eq!(intersection.width, 0);
        assert_eq!(intersection.height, 0);
    }

    #[test]
    fn column_stacks_children_vertically() {
        let mut buffer = DoubleBuffer::new(6, 4);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let node = column(vec![text::<()>("top"), text::<()>("bottom")]);
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(6, 4));

        renderer
            .render(&node, Size::new(6, 4))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let lines: Vec<&str> = screen.lines().collect();
        assert_eq!(lines[0].trim_end(), "top");
        assert_eq!(lines[1].trim_end(), "bottom");
    }

    #[test]
    fn test_width_discrepancies() {
        let text_content = "chatui on  main [$!?⇡] is 📦 v0.1.0 via 🦀 v1.89.0 took 5m34s";

        // 1. Measure as TextNode::measure does
        let measure_width: usize = grapheme_width(text_content);
        println!("Measure width (entire string): {}", measure_width);

        // 2. Measure as render_text does (char by char)
        let mut render_width = 0;
        for ch in text_content.chars() {
            render_width += UnicodeWidthChar::width(ch).unwrap_or(0).max(1);
        }
        println!("Render width (char by char .max(1)): {}", render_width);

        // 3. Measure as write_text does
        let mut write_width = 0;
        for ch in text_content.chars() {
            write_width += UnicodeWidthChar::width(ch).unwrap_or(0);
        }
        println!("Write width (unicode_width): {}", write_width);

        assert_eq!(
            measure_width, render_width,
            "Measure and Render widths should match"
        );
        assert_eq!(
            render_width, write_width,
            "Render and Write widths should match"
        );
    }

    #[test]
    fn render_output_keeps_single_spaces_after_emoji() {
        let text_content = "chatui on  main [$!?⇡] is 📦 v0.1.0 via 🦀 v1.89.0 took 2m23s";
        let width = grapheme_width(text_content) + 2;
        let node = text::<()>(text_content);

        let lines = render_to_lines(node, width, 1);
        assert_eq!(lines[0].trim_end(), text_content);
    }

    #[test]
    fn row_places_children_horizontally() {
        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let node = row(vec![text::<()>("left"), text::<()>("right")]);
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(10, 2));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let first_line = renderer.buffer().to_string();
        let first = first_line.lines().next().unwrap();
        assert_eq!(first.trim_end(), "leftright");
    }

    #[test]
    fn block_draws_border() {
        let mut buffer = DoubleBuffer::new(4, 3);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let node = block::<()>(Vec::new());
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(4, 3));

        renderer
            .render(&node, Size::new(4, 3))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let lines: Vec<&str> = screen.lines().collect();
        assert_eq!(lines[0].chars().take(1).collect::<String>(), "┌");
        let last = lines
            .iter()
            .rev()
            .find(|line| !line.trim_end().is_empty())
            .unwrap();
        assert_eq!(last.chars().next().unwrap(), '└');
    }

    #[test]
    fn block_renders_title() {
        const TITLE: &str = "Title";
        let mut buffer = DoubleBuffer::new(10, 4);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let node = block_with_title::<()>(TITLE, Vec::new())
            .with_min_width(Dimension::length(10.))
            .with_min_height(Dimension::length(3.));
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(10, 4));

        renderer
            .render(&node, Size::new(10, 4))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let top_line = screen.lines().next().unwrap();
        let chars: Vec<char> = top_line.chars().collect();
        assert!(chars.len() >= TITLE.len() + 2);
        assert_eq!(chars.first(), Some(&'┌'));
        assert_eq!(chars.last(), Some(&'┐'));
        let rendered_title: String = chars[1..1 + TITLE.len()].iter().collect();
        assert_eq!(rendered_title, TITLE);
    }

    #[test]
    fn modal_renders_dim_overlay() {
        let mut buffer = DoubleBuffer::new(12, 6);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        let base = column(vec![
            text::<()>("Base content"),
            text::<()>("stays underneath"),
        ])
        .with_fill();

        let modal_content = block::<()>(vec![column(vec![text::<()>("Confirm commit")])])
            .with_min_width(Dimension::length(8.0))
            .with_min_height(Dimension::length(4.0));

        let overlay = modal(vec![modal_content]);

        let root = column(vec![base, overlay]).with_fill();
        let mut root: RetainedNode<()> = root.into();
        prepare_layout(&mut root, Size::new(12, 6));

        renderer
            .render(&root, Size::new(12, 6))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let lines: Vec<&str> = screen.lines().collect();
        assert!(lines.len() >= 2);
        assert!(lines[0].contains("Base content"));

        let block_row = lines
            .iter()
            .position(|line| line.contains('┌'))
            .expect("expected modal border");
        assert!(block_row > 0);
        let block_col = lines[block_row].find('┌').expect("expected border char");
        assert!(block_col < lines[block_row].len());

        // Check that the background cells have been darkened (blended with semi-transparent black)
        let back_buffer = renderer.buffer().back_buffer();
        let first_cell = &back_buffer[0][0];
        // The cell should have a background color that's darker than the default
        // Since we're blending with 50% transparent black, the background should be darkened
        assert!(
            first_cell.attrs.background().is_some(),
            "Background should be set after blending"
        );
    }

    #[test]
    fn style_applies_foreground_color() {
        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        let node = rich_text::<()>(vec![TextSpanRef::new("color", Style::fg(Color::Blue))]);
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(10, 2));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let back_buffer = renderer.buffer().back_buffer();
        let cell = &back_buffer[0][0];
        // Blue color (0, 0, 238)
        assert_eq!(cell.attrs.foreground(), Some(Rgba::opaque(0, 0, 238)));
    }

    #[test]
    fn dim_style_sets_dim() {
        let palette = Palette::default();
        let attrs = style_to_attributes(&palette, &Style::dim());
        assert!(attrs.is_dim());
    }

    #[test]
    fn reverse_style_sets_reverse() {
        let palette = Palette::default();
        let mut style = Style::default();
        style.reverse = true;
        let attrs = style_to_attributes(&palette, &style);
        assert!(attrs.is_reverse());
    }

    #[test]
    fn translates_rgb_color() {
        let palette = Palette::default();
        let rgba = color_to_rgba(&palette, Color::rgb(10, 20, 30));
        let expected = Rgba::opaque(10, 20, 30);
        assert_eq!(rgba, expected);
    }

    #[test]
    fn nested_blocks_render_with_borders() {
        let header = text::<()>(
            "TODOs (Esc/q quit from list, ↑/↓ move, space toggles, Tab focuses input, Enter adds)",
        )
        .with_style(Style::bold());

        let input_block = block::<()>(vec![column(vec![
            text::<()>("New TODO (Tab to focus, Enter adds):"),
            text::<()>("> _"),
        ])]);

        let items = column(vec![
            text::<()>("[x] Ship Elm TUI scaffold"),
            text::<()>("[x] Wire rendering into runtime"),
            text::<()>("[ ] Polish TODO example"),
            text::<()>("[ ] Add focus styles"),
        ]);

        let root = column(vec![header, input_block, items]);

        let mut buffer = DoubleBuffer::new(90, 12);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let mut root: RetainedNode<()> = root.into();
        prepare_layout(&mut root, Size::new(90, 12));

        renderer
            .render(&root, Size::new(90, 12))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let lines: Vec<&str> = screen.lines().collect();

        assert!(lines[0].starts_with("TODOs"));
        assert!(lines[1].starts_with("┌"));
        assert!(lines[2].starts_with("│N"));
        assert!(
            lines
                .iter()
                .any(|line| line.contains("[ ] Polish TODO example"))
        );
    }

    #[test]
    fn scroll_offset_shifts_children_up() {
        let mut buffer = DoubleBuffer::new(10, 6);
        // Create a column with many lines and apply scroll
        let lines: Vec<Node<()>> = (0..10).map(|i| text::<()>(format!("L{}", i))).collect();
        let root = column(vec![
            column(lines)
                .with_scroll(3.0)
                .with_height(taffy::Dimension::length(5.0)),
        ]);
        let mut root: RetainedNode<()> = root.into();
        compute_root_layout(
            &mut root,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(10.0),
                height: AvailableSpace::Definite(6.0),
            },
        );
        round_layout(&mut root);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer
            .render(&root, Size::new(10, 6))
            .expect("render should succeed");
        let screen = renderer.buffer().to_string();
        // After scrolling by 3, first visible should be L3
        assert!(
            screen.lines().next().unwrap().contains("L3"),
            "screen=\n{}",
            screen
        );
    }

    #[test]
    fn paragraph_respects_vertical_scroll() {
        let top_lines = render_paragraph_with_scroll(0.0);
        let scrolled_lines = render_paragraph_with_scroll(2.0);

        assert!(
            matches!(top_lines.first(), Some(line) if line.contains("L0")),
            "expected top render to start at L0: {:?}",
            top_lines
        );
        assert!(
            matches!(scrolled_lines.first(), Some(line) if line.contains("L2")),
            "expected scrolled render to start at L2: {:?}",
            scrolled_lines
        );
        assert!(
            scrolled_lines.iter().any(|line| line.contains("L4")),
            "expected scrolled render to include L4: {:?}",
            scrolled_lines
        );
        assert!(
            !scrolled_lines.iter().any(|line| line.contains("L0")),
            "scrolled render still shows initial line: {:?}",
            scrolled_lines
        );
    }

    #[test]
    fn scrollable_content_scrolls_paragraph_lines() {
        let top_lines = render_scrollable_paragraph(0.0);
        let scrolled_lines = render_scrollable_paragraph(2.0);

        assert!(top_lines.len() > 2 && scrolled_lines.len() > 2);

        let top_content = &top_lines[1..top_lines.len().saturating_sub(1)];
        let scrolled_content = &scrolled_lines[1..scrolled_lines.len().saturating_sub(1)];

        assert!(
            matches!(top_content.first(), Some(line) if line.contains("Line 0")),
            "expected unscrolled render to start at Line 0: {:?}",
            top_lines
        );
        assert!(
            matches!(scrolled_content.first(), Some(line) if line.contains("Line 1")),
            "expected scrolled render to start at Line 1: {:?}",
            scrolled_lines
        );
        assert!(
            scrolled_content.iter().any(|line| line.contains("Line 3")),
            "expected scrolled render to include later lines: {:?}",
            scrolled_lines
        );
        assert!(
            !scrolled_content.iter().any(|line| line.contains("Line 0")),
            "expected earlier lines to scroll out of view: {:?}",
            scrolled_lines
        );
    }

    #[test]
    fn scrollable_content_with_multiple_paragraphs_respects_scroll() {
        let top_lines = render_scrollable_multi_paragraph(0.0);
        let scrolled_lines = render_scrollable_multi_paragraph(2.0);

        assert!(
            matches!(top_lines.first(), Some(line) if line.contains("A0")),
            "expected first paragraph to start content: {:?}",
            top_lines
        );
        assert!(
            matches!(scrolled_lines.first(), Some(line) if line.contains("A2")),
            "expected scroll to advance into first paragraph tail: {:?}",
            scrolled_lines
        );
        assert!(
            scrolled_lines.iter().any(|line| line.contains("B0")),
            "expected scrolled view to include following paragraphs: {:?}",
            scrolled_lines
        );
        assert!(
            !scrolled_lines.iter().any(|line| line.contains("A0")),
            "expected earliest lines to be scrolled away: {:?}",
            scrolled_lines
        );
    }

    #[test]
    fn scrollbar_renders_thumb_for_overflow() {
        let lines = render_scrollable_lines(0.0);
        let thumb_rows = thumb_rows(&lines);
        assert!(
            !thumb_rows.is_empty(),
            "expected thumb cells in {:?}",
            lines
        );
        assert_eq!(thumb_rows.first().copied().unwrap(), 0);
    }

    #[test]
    fn scrollbar_thumb_moves_with_scroll() {
        let top_lines = render_scrollable_lines(0.0);
        let bottom_lines = render_scrollable_lines(15.0);

        let top_rows = thumb_rows(&top_lines);
        let bottom_rows = thumb_rows(&bottom_lines);
        assert!(
            !top_rows.is_empty() && !bottom_rows.is_empty(),
            "expected thumb rows top={:?} bottom={:?}",
            top_lines,
            bottom_lines
        );

        let top_center = average(&top_rows);
        let bottom_center = average(&bottom_rows);
        assert!(
            bottom_center > top_center,
            "expected thumb center to increase: top={top_center}, bottom={bottom_center}"
        );
        assert!(
            bottom_rows.last().copied().unwrap() >= bottom_lines.len().saturating_sub(2),
            "expected thumb near bottom: {:?}",
            bottom_rows
        );
    }

    #[test]
    fn scrollbar_shares_block_border_column() {
        let lines = render_scrollable_block_lines(5.0);
        assert!(!lines.is_empty(), "expected rendered lines");

        let right_column: Vec<char> = lines
            .iter()
            .filter_map(|line| line.chars().nth(7))
            .collect();

        assert_eq!(right_column.first().copied(), Some('┐'));
        assert_eq!(right_column.last().copied(), Some('┘'));

        let interior = &right_column[1..right_column.len().saturating_sub(1)];
        assert!(
            interior.contains(&'│'),
            "expected shared border track in {:?}",
            interior
        );
        assert!(
            interior.contains(&SCROLLBAR_THUMB_CHAR),
            "expected thumb to reuse border column in {:?}",
            interior
        );
    }

    #[test]
    fn scrollbar_preserves_block_top_border() {
        let lines = render_scrollable_block_lines(8.0);
        assert!(!lines.is_empty(), "expected rendered lines");
        let top = lines.first().expect("expected top line");
        assert_eq!(top, "┌──────┐");
        assert_eq!(
            lines.get(1).expect("expected first content row"),
            "│Item  │"
        );
    }

    #[test]
    fn input_overflow_hides_latest_text() {
        // Confirms bug: when input content exceeds available width, newest text is not visible.
        let mut buffer = DoubleBuffer::new(8, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        let value = "abcdefghijklmnopqrstuvwxyz";
        let state = crate::InputState::with_value(value);
        let style = crate::InputStyle::default();
        let node = crate::input::<()>("input", &state, &style, |_| ());
        let mut node: RetainedNode<()> = node.into();
        prepare_layout(&mut node, Size::new(8, 1));

        renderer
            .render(&node, Size::new(8, 1))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        // It currently shows the prefix and hides the end (bug: latest text not visible).
        assert_eq!(first_line, &value[..8]);
        assert!(!first_line.contains(value.chars().last().unwrap()));
    }
}
