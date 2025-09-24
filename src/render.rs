use crate::dom::{ElementKind, ElementNode, Node, NodeContent, Style, TextNode};
use crate::error::ProgramError;
use crate::event::Size;

use termwiz::cell::{CellAttributes, Intensity, unicode_column_width};
use termwiz::color::{AnsiColor, ColorAttribute};
use termwiz::surface::{Change, CursorVisibility, Position, Surface};

use taffy::Layout as TaffyLayout;

pub struct Renderer<'a> {
    surface: &'a mut Surface,
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

impl<'a> Renderer<'a> {
    pub fn new(surface: &'a mut Surface) -> Self {
        Self { surface }
    }

    pub fn render<Msg>(&mut self, root: &Node<Msg>, size: Size) -> Result<(), ProgramError> {
        let width = size.width as usize;
        let height = size.height as usize;

        self.surface.add_change(Change::CursorPosition {
            x: Position::Absolute(0),
            y: Position::Absolute(0),
        });
        self.surface
            .add_change(Change::ClearScreen(ColorAttribute::Default));
        self.surface.resize(width, height);
        self.surface
            .add_change(Change::ClearScreen(ColorAttribute::Default));

        let origin = Rect {
            x: 0,
            y: 0,
            width,
            height,
        };
        self.render_node(root, origin, 0.0);
        self.surface
            .add_change(Change::CursorVisibility(CursorVisibility::Hidden));

        Ok(())
    }

    fn render_node<Msg>(&mut self, node: &Node<Msg>, parent_origin: Rect, inherited_scroll_y: f32) {
        let layout = node.layout_state.layout;
        let node_origin = Point {
            x: parent_origin.x + layout.location.x as usize,
            y: parent_origin.y + (layout.location.y - inherited_scroll_y).max(0.0) as usize,
        };

        let Some(area) = rect_from_layout(&layout, node_origin) else {
            return;
        };
        if !area.has_area() {
            return;
        }

        let area = area.intersection(parent_origin);
        if !area.has_area() {
            return;
        }

        match &node.content {
            NodeContent::Text(text) => self.render_text(text, area),
            NodeContent::Element(element) => {
                let next_scroll = if node.layout_state.style.overflow.y == taffy::Overflow::Scroll {
                    inherited_scroll_y + node.scroll_y
                } else {
                    inherited_scroll_y
                };
                self.render_element(element, area, next_scroll);
            }
        }
    }

    fn render_text<Msg>(&mut self, text: &TextNode<Msg>, area: Rect) {
        let mut remaining = area.width;
        if remaining == 0 {
            return;
        }

        let mut cursor_x = area.x;

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

            self.surface.add_change(Change::CursorPosition {
                x: Position::Absolute(cursor_x),
                y: Position::Absolute(area.y),
            });
            self.surface
                .add_change(Change::AllAttributes(style_to_attributes(&span.style)));
            self.surface.add_change(Change::Text(collected));

            cursor_x += taken;
            remaining = remaining.saturating_sub(taken);
        }
    }

    fn render_element<Msg>(&mut self, element: &ElementNode<Msg>, area: Rect, scroll_y: f32) {
        match element.kind {
            ElementKind::Block => self.render_block(element, area, scroll_y),
            ElementKind::Column | ElementKind::Row => {
                self.render_children(&element.children, area, scroll_y)
            }
        }
    }

    fn render_children<Msg>(&mut self, children: &[Node<Msg>], clip: Rect, scroll_y: f32) {
        for child in children {
            self.render_node(child, clip, scroll_y);
        }
    }

    fn render_block<Msg>(&mut self, element: &ElementNode<Msg>, area: Rect, scroll_y: f32) {
        self.draw_border(area, &element.attrs.style);
        if let Some(title) = &element.title {
            self.render_block_title(title, area, &element.attrs.style);
        }
        self.render_children(
            &element.children,
            Rect {
                x: area.x,
                y: area.y,
                width: area.width.saturating_sub(1),
                height: area.height.saturating_sub(1),
            },
            scroll_y,
        );
    }

    fn draw_border(&mut self, area: Rect, style: &Style) {
        if area.width < 1 || area.height < 1 {
            return;
        }

        let attrs = style_to_attributes(style);

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

    fn render_block_title(&mut self, title: &str, area: Rect, style: &Style) {
        if area.width <= 2 {
            return;
        }

        let mut rendered = String::new();
        let mut available_columns = area.width.saturating_sub(2);

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

        let attrs = style_to_attributes(style);
        self.surface.add_change(Change::CursorPosition {
            x: Position::Absolute(area.x + 1),
            y: Position::Absolute(area.y),
        });
        self.surface
            .add_change(Change::AllAttributes(attrs.clone()));
        self.surface.add_change(Change::Text(rendered));
    }

    fn write_char(&mut self, x: usize, y: usize, ch: char, attrs: &CellAttributes) {
        let (width, height) = self.surface.dimensions();
        if x >= width || y >= height {
            return;
        }

        self.surface.add_change(Change::CursorPosition {
            x: Position::Absolute(x),
            y: Position::Absolute(y),
        });
        self.surface
            .add_change(Change::AllAttributes(attrs.clone()));
        self.surface.add_change(Change::Text(ch.to_string()));
    }

    pub fn surface(&self) -> &Surface {
        &*self.surface
    }

    pub fn surface_mut(&mut self) -> &mut Surface {
        &mut *self.surface
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

fn style_to_attributes(style: &Style) -> CellAttributes {
    let mut attrs = CellAttributes::default();
    if let Some(fg) = style.fg {
        attrs.set_foreground(color_to_attribute(fg));
    }
    if let Some(bg) = style.bg {
        attrs.set_background(color_to_attribute(bg));
    }
    if style.bold {
        attrs.set_intensity(Intensity::Bold);
    }
    attrs
}

fn color_to_attribute(color: crate::dom::Color) -> ColorAttribute {
    match color {
        crate::dom::Color::Reset => ColorAttribute::Default,
        crate::dom::Color::Black => AnsiColor::Black.into(),
        crate::dom::Color::Red => AnsiColor::Red.into(),
        crate::dom::Color::Green => AnsiColor::Green.into(),
        crate::dom::Color::Yellow => AnsiColor::Yellow.into(),
        crate::dom::Color::Blue => AnsiColor::Blue.into(),
        crate::dom::Color::Magenta => AnsiColor::Fuchsia.into(),
        crate::dom::Color::Cyan => AnsiColor::Aqua.into(),
        crate::dom::Color::White => AnsiColor::White.into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dom::rounding::round_layout;
    use crate::dom::{
        Color, Style, TextSpan, block, block_with_title, column, rich_text, row, text,
    };
    use taffy::{AvailableSpace, Dimension, compute_root_layout};
    use termwiz::surface::CursorVisibility;

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

    #[test]
    fn renders_text_node() {
        let mut surface = Surface::new(10, 4);
        let mut renderer = Renderer::new(&mut surface);
        let mut node = text::<()>("hello");
        prepare_layout(&mut node, Size::new(10, 4));

        renderer
            .render(&node, Size::new(10, 4))
            .expect("render should succeed");

        let contents = renderer.surface.screen_chars_to_string();
        assert!(contents.starts_with("hello"));
    }

    #[test]
    fn renders_rich_text_spans_with_styles() {
        let mut surface = Surface::new(10, 2);
        let mut renderer = Renderer::new(&mut surface);
        let spans = vec![
            TextSpan::new("+", Style::fg(Color::Green)),
            TextSpan::new("fn", Style::fg(Color::Cyan)),
        ];
        let mut node = rich_text::<()>(spans);
        prepare_layout(&mut node, Size::new(10, 2));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let mut lines = renderer.surface().screen_lines();
        let line = lines.remove(0);
        let mut cells = line.visible_cells();
        let prefix = cells.next().expect("expected prefix cell");
        assert_eq!(prefix.attrs().foreground(), AnsiColor::Green.into());
        let keyword = cells.next().expect("expected keyword cell");
        assert_eq!(keyword.attrs().foreground(), AnsiColor::Aqua.into());
    }

    #[test]
    fn rich_text_respects_width_clipping() {
        let mut surface = Surface::new(4, 2);
        let mut renderer = Renderer::new(&mut surface);
        let spans = vec![
            TextSpan::new("abc", Style::fg(Color::Red)),
            TextSpan::new("def", Style::fg(Color::Green)),
        ];
        let mut node = rich_text::<()>(spans);
        prepare_layout(&mut node, Size::new(4, 2));

        renderer
            .render(&node, Size::new(4, 2))
            .expect("render should succeed");

        let screen = renderer.surface().screen_chars_to_string();
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
        let mut surface = Surface::new(6, 4);
        let mut renderer = Renderer::new(&mut surface);
        let mut node = column(vec![text::<()>("top"), text::<()>("bottom")]);
        prepare_layout(&mut node, Size::new(6, 4));

        renderer
            .render(&node, Size::new(6, 4))
            .expect("render should succeed");

        let screen = renderer.surface().screen_chars_to_string();
        let lines: Vec<&str> = screen.lines().collect();
        assert_eq!(lines[0].trim_end(), "top");
        assert_eq!(lines[1].trim_end(), "bottom");
    }

    #[test]
    fn row_places_children_horizontally() {
        let mut surface = Surface::new(10, 2);
        let mut renderer = Renderer::new(&mut surface);
        let mut node = row(vec![text::<()>("left"), text::<()>("right")]);
        prepare_layout(&mut node, Size::new(10, 2));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let first_line = renderer.surface().screen_chars_to_string();
        let first = first_line.lines().next().unwrap();
        assert_eq!(first.trim_end(), "leftright");
    }

    #[test]
    fn block_draws_border() {
        let mut surface = Surface::new(4, 3);
        let mut renderer = Renderer::new(&mut surface);
        let mut node = block::<()>(Vec::new());
        prepare_layout(&mut node, Size::new(4, 3));

        renderer
            .render(&node, Size::new(4, 3))
            .expect("render should succeed");

        let screen = renderer.surface().screen_chars_to_string();
        let lines: Vec<&str> = screen.lines().collect();
        assert_eq!(lines[0].chars().take(1).collect::<String>(), "┌");
        let last = lines
            .iter()
            .rev()
            .find(|line| line.trim_end().len() > 0)
            .unwrap();
        assert_eq!(last.chars().next().unwrap(), '└');
    }

    #[test]
    fn block_renders_title() {
        const TITLE: &str = "Title";
        let mut surface = Surface::new(10, 4);
        let mut renderer = Renderer::new(&mut surface);
        let mut node = block_with_title::<()>(TITLE, Vec::new())
            .with_min_width(Dimension::length(10.))
            .with_min_height(Dimension::length(3.));
        prepare_layout(&mut node, Size::new(10, 4));

        renderer
            .render(&node, Size::new(10, 4))
            .expect("render should succeed");

        let screen = renderer.surface().screen_chars_to_string();
        let top_line = screen.lines().next().unwrap();
        let chars: Vec<char> = top_line.chars().collect();
        assert!(chars.len() >= TITLE.len() + 2);
        assert_eq!(chars.first(), Some(&'┌'));
        assert_eq!(chars.last(), Some(&'┐'));
        let rendered_title: String = chars[1..1 + TITLE.len()].iter().collect();
        assert_eq!(rendered_title, TITLE);
    }

    #[test]
    fn style_applies_foreground_color() {
        let mut surface = Surface::new(10, 2);
        let mut renderer = Renderer::new(&mut surface);
        let mut node = text::<()>("color").with_style(Style::fg(Color::Blue));
        prepare_layout(&mut node, Size::new(10, 2));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let mut lines = renderer.surface().screen_lines();
        let line = lines.remove(0);
        let cell = line.visible_cells().next().unwrap();
        assert_eq!(cell.attrs().foreground(), AnsiColor::Blue.into());
    }

    #[test]
    fn render_hides_cursor() {
        let mut surface = Surface::new(10, 2);
        let mut renderer = Renderer::new(&mut surface);
        let mut node = text::<()>("cursor");
        prepare_layout(&mut node, Size::new(10, 2));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        assert_eq!(
            renderer.surface().cursor_visibility(),
            CursorVisibility::Hidden
        );
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

        let mut surface = Surface::new(90, 12);
        let mut renderer = Renderer::new(&mut surface);
        prepare_layout(&mut root, Size::new(90, 12));

        renderer
            .render(&root, Size::new(90, 12))
            .expect("render should succeed");

        let screen = renderer.surface().screen_chars_to_string();
        let lines: Vec<&str> = screen.lines().collect();

        assert!(lines[0].starts_with("┌"));
        assert!(lines[1].starts_with("│TODOs"));
        assert!(
            lines.iter().any(|line| line.starts_with("│┌")),
            "expected nested block border"
        );
        assert!(
            lines
                .iter()
                .any(|line| line.contains("[ ] Add focus styles"))
        );
    }

    #[test]
    fn scroll_offset_shifts_children_up() {
        let mut surface = Surface::new(10, 6);
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
        let mut renderer = Renderer::new(&mut surface);
        renderer
            .render(&root, Size::new(10, 6))
            .expect("render should succeed");
        let screen = renderer.surface().screen_chars_to_string();
        // After scrolling by 3, first visible should be L3
        assert!(
            screen.lines().next().unwrap().contains("L3"),
            "screen=\n{}",
            screen
        );
    }
}
