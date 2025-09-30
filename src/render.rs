use crate::buffer::{CellAttributes, DoubleBuffer};
use crate::dom::{ElementKind, ElementNode, Node, NodeContent, Style, TextNode};
use crate::error::ProgramError;
use crate::event::Size;
use crate::palette::{Palette, Rgba};

use termwiz::cell::unicode_column_width;

use taffy::Layout as TaffyLayout;

pub struct Renderer<'a> {
    buffer: &'a mut DoubleBuffer,
    palette: &'a Palette,
}

#[derive(Clone, Copy)]
struct Rect {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
}

impl Rect {
    fn intersection(self, other: Self) -> Self {
        let x = self.x.max(other.x);
        let y = self.y.max(other.y);

        let self_right = self.x.saturating_add(self.width);
        let other_right = other.x.saturating_add(other.width);
        let self_bottom = self.y.saturating_add(self.height);
        let other_bottom = other.y.saturating_add(other.height);

        let width = self_right.min(other_right).saturating_sub(x);
        let height = self_bottom.min(other_bottom).saturating_sub(y);

        Self {
            x,
            y,
            width,
            height,
        }
    }

    fn has_area(self) -> bool {
        self.width > 0 && self.height > 0
    }
}

#[derive(Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

const SCROLLBAR_TRACK_CHAR: char = ' ';
const SCROLLBAR_THUMB_CHAR: char = '█';

impl<'a> Renderer<'a> {
    pub fn new(buffer: &'a mut DoubleBuffer, palette: &'a Palette) -> Self {
        Self { buffer, palette }
    }

    pub fn render<Msg>(&mut self, root: &Node<Msg>, size: Size) -> Result<(), ProgramError> {
        let width = size.width as usize;
        let height = size.height as usize;

        self.buffer.resize(width, height);
        self.buffer.clear();

        let clip = Rect {
            x: 0,
            y: 0,
            width,
            height,
        };
        self.render_node(root, Point { x: 0, y: 0 }, clip, 0.0);

        Ok(())
    }

    fn render_node<Msg>(
        &mut self,
        node: &Node<Msg>,
        parent_origin: Point,
        clip: Rect,
        inherited_scroll_y: f32,
    ) {
        let layout = node.layout_state.layout;
        let node_origin = Point {
            x: parent_origin.x + layout.location.x as usize,
            y: parent_origin.y + (layout.location.y - inherited_scroll_y).max(0.0) as usize,
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

        match &node.content {
            NodeContent::Text(text) => self.render_text(text, node_origin, area),
            NodeContent::Element(element) => {
                let is_scroll = node.layout_state.style.overflow.y == taffy::Overflow::Scroll;
                let border_style =
                    if matches!(element.kind, ElementKind::Block) && element.attrs.style.border {
                        Some(&element.attrs.style)
                    } else {
                        None
                    };
                let next_scroll = if is_scroll {
                    inherited_scroll_y + node.scroll_y
                } else {
                    inherited_scroll_y
                };
                self.render_element(element, node_origin, area, next_scroll);
                if is_scroll {
                    self.render_scrollbar(node_origin, area, &layout, node.scroll_y, border_style);
                }
            }
        }
    }

    fn render_text<Msg>(&mut self, text: &TextNode<Msg>, _parent_origin: Point, clip: Rect) {
        let mut remaining = clip.width;
        if remaining == 0 {
            return;
        }

        let mut cursor_x = clip.x;

        let mut fill_style: Option<Style> = None;
        for span in text.spans() {
            if remaining == 0 {
                break;
            }

            let mut collected = String::new();
            let mut taken = 0;

            for ch in span.content.chars() {
                if taken == remaining {
                    break;
                }
                collected.push(ch);
                taken += 1;
            }

            if collected.is_empty() {
                continue;
            }

            if span.style.bg.is_some() || span.style.dim {
                fill_style = Some(span.style.clone());
            }

            let attrs = self.style_to_attributes(&span.style);
            self.buffer.write_text(cursor_x, clip.y, &collected, &attrs);

            cursor_x += taken;
            remaining = remaining.saturating_sub(taken);
        }

        if remaining > 0
            && let Some(style) = fill_style
        {
            let attrs = self.style_to_attributes(&style);
            let padding = " ".repeat(remaining);
            self.buffer.write_text(cursor_x, clip.y, &padding, &attrs);
        }
    }

    fn style_to_attributes(&self, style: &Style) -> CellAttributes {
        let mut attrs = CellAttributes::default();
        if let Some(fg) = style.fg {
            attrs.set_foreground(self.color_to_rgba(fg));
        }
        if let Some(bg) = style.bg {
            attrs.set_background(self.color_to_rgba(bg));
        }
        if style.bold {
            attrs.set_bold(true);
        } else if style.dim {
            attrs.set_dim(true);
        }
        attrs
    }

    fn color_to_rgba(&self, color: crate::dom::Color) -> Rgba {
        match color {
            crate::dom::Color::Reset => self.palette.foreground,
            crate::dom::Color::Rgba { r, g, b, a } => Rgba::new(r, g, b, a),
        }
    }

    fn render_element<Msg>(
        &mut self,
        element: &ElementNode<Msg>,
        parent_origin: Point,
        clip: Rect,
        scroll_y: f32,
    ) {
        match element.kind {
            ElementKind::Block => self.render_block(element, parent_origin, clip, scroll_y),
            ElementKind::Modal => self.render_modal(element, parent_origin, clip, scroll_y),
            ElementKind::Column | ElementKind::Row => {
                self.render_children(&element.children, parent_origin, clip, scroll_y)
            }
        }
    }

    fn render_children<Msg>(
        &mut self,
        children: &[Node<Msg>],
        parent_origin: Point,
        clip: Rect,
        scroll_y: f32,
    ) {
        for child in children {
            self.render_node(child, parent_origin, clip, scroll_y);
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
            Some(style) => self.style_to_attributes(style),
            None => CellAttributes::default(),
        };
        thumb_attrs.set_bold(true);

        for y in thumb_top..thumb_bottom {
            for x in scrollbar_x..(scrollbar_x + scrollbar_width) {
                self.write_char(x, y, SCROLLBAR_THUMB_CHAR, &thumb_attrs);
            }
        }
    }

    fn render_modal<Msg>(
        &mut self,
        element: &ElementNode<Msg>,
        parent_origin: Point,
        clip: Rect,
        scroll_y: f32,
    ) {
        self.apply_overlay_style(clip, &element.attrs.style);
        self.render_children(&element.children, parent_origin, clip, scroll_y);
    }

    fn render_block<Msg>(
        &mut self,
        element: &ElementNode<Msg>,
        parent_origin: Point,
        clip: Rect,
        scroll_y: f32,
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
        self.render_children(&element.children, child_origin, child_area, scroll_y);
    }

    fn draw_border(&mut self, area: Rect, style: &Style) {
        if area.width < 1 || area.height < 1 {
            return;
        }

        let attrs = self.style_to_attributes(style);

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
            let mut buf = [0u8; 4];
            let ch_str = ch.encode_utf8(&mut buf);
            let width = unicode_column_width(ch_str, None);

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

        let attrs = self.style_to_attributes(style);
        self.buffer.write_text(clip.x + 1, clip.y, &rendered, &attrs);
    }

    fn write_char(&mut self, x: usize, y: usize, ch: char, attrs: &CellAttributes) {
        self.buffer.write_char(x, y, ch, attrs);
    }

    fn apply_overlay_style(&mut self, area: Rect, style: &Style) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        // Convert colors before the loop to avoid borrow checker issues
        let fg_color = style.fg.map(|c| self.color_to_rgba(c));
        let bg_color = style.bg.map(|c| self.color_to_rgba(c));

        for y in area.y..(area.y + area.height).min(self.buffer.dimensions().1) {
            for x in area.x..(area.x + area.width).min(self.buffer.dimensions().0) {
                if let Some(cell) = self.buffer.get_cell_mut(x, y) {
                    if let Some(fg) = fg_color {
                        cell.attrs.set_foreground(fg);
                    }
                    if let Some(bg) = bg_color {
                        cell.attrs.set_background(bg);
                    }
                    if style.bold {
                        cell.attrs.set_bold(true);
                    } else if style.dim {
                        cell.attrs.set_dim(true);
                    }
                }
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
    use crate::dom::rounding::round_layout;
    use crate::dom::{
        Color, Style, TextSpan, block, block_with_title, column, modal, rich_text, row, text,
    };
    use crate::palette::Palette;
    use taffy::{AvailableSpace, Dimension, compute_root_layout};

    fn prepare_layout<Msg>(node: &mut Node<Msg>, size: Size) {
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
        let mut root = column(vec![scrollable])
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));
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
        let mut root = scrollable;
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
        let mut node = text::<()>("hello");
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
            TextSpan::new("+", Style::fg(Color::Green)),
            TextSpan::new("fn", Style::fg(Color::Cyan)),
        ];
        let mut node = rich_text::<()>(spans);
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
    fn rich_text_respects_width_clipping() {
        let mut buffer = DoubleBuffer::new(4, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let spans = vec![
            TextSpan::new("abc", Style::fg(Color::Red)),
            TextSpan::new("def", Style::fg(Color::Green)),
        ];
        let mut node = rich_text::<()>(spans);
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
        let mut node = column(vec![text::<()>("top"), text::<()>("bottom")]);
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
    fn row_places_children_horizontally() {
        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let mut node = row(vec![text::<()>("left"), text::<()>("right")]);
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
        let mut node = block::<()>(Vec::new());
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
        let mut node = block_with_title::<()>(TITLE, Vec::new())
            .with_min_width(Dimension::length(10.))
            .with_min_height(Dimension::length(3.));
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

        let mut root = column(vec![base, overlay]).with_fill();
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

        let back_buffer = renderer.buffer().back_buffer();
        let first_cell = &back_buffer[0][0];
        assert!(first_cell.attrs.is_dim());
    }

    #[test]
    fn style_applies_foreground_color() {
        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        let mut node = text::<()>("color").with_style(Style::fg(Color::Blue));
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
        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let renderer = Renderer::new(&mut buffer, &palette);
        let attrs = renderer.style_to_attributes(&Style::dim());
        assert!(attrs.is_dim());
    }

    #[test]
    fn translates_rgb_color() {
        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let renderer = Renderer::new(&mut buffer, &palette);
        let rgba = renderer.color_to_rgba(Color::rgb(10, 20, 30));
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

        let mut root = block::<()>(vec![column(vec![header, input_block, items])]);

        let mut buffer = DoubleBuffer::new(90, 12);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);
        prepare_layout(&mut root, Size::new(90, 12));

        renderer
            .render(&root, Size::new(90, 12))
            .expect("render should succeed");

        let screen = renderer.buffer().to_string();
        let lines: Vec<&str> = screen.lines().collect();

        assert!(lines[0].starts_with("┌"));
        assert_eq!(&lines.get(1).unwrap()[..8], "│TODOs");
        assert_eq!(&lines.get(2).unwrap()[..6], "│┌");
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
        let mut root = column(vec![
            column(lines)
                .with_scroll(3.0)
                .with_height(taffy::Dimension::length(5.0)),
        ]);
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
}
