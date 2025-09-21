use crate::dom::{ElementKind, ElementNode, Node, Style, TextNode};
use crate::error::ProgramError;
use crate::event::Size;

use termwiz::cell::{CellAttributes, Intensity};
use termwiz::color::{AnsiColor, ColorAttribute};
use termwiz::surface::{Change, Position, Surface};

pub struct Renderer {
    surface: Surface,
}

#[derive(Clone, Copy)]
struct Rect {
    x: usize,
    y: usize,
    width: u16,
    height: u16,
}

impl Renderer {
    pub fn new() -> Self {
        Self {
            surface: Surface::new(0, 0),
        }
    }

    pub fn render<Msg>(&mut self, root: &Node<Msg>, size: Size) -> Result<(), ProgramError> {
        let width = size.width as usize;
        let height = size.height as usize;

        self.surface.resize(width, height);
        self.surface
            .add_change(Change::CursorPosition {
                x: Position::Absolute(0),
                y: Position::Absolute(0),
            });
        self.surface
            .add_change(Change::ClearScreen(ColorAttribute::Default));

        let area = Rect {
            x: 0,
            y: 0,
            width: size.width,
            height: size.height,
        };
        self.render_node(root, area);

        Ok(())
    }

    fn render_node<Msg>(&mut self, node: &Node<Msg>, area: Rect) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        match node {
            Node::Text(text) => self.render_text(text, area),
            Node::Element(element) => self.render_element(element, area),
        }
    }

    fn render_text<Msg>(&mut self, text: &TextNode<Msg>, area: Rect) {
        let attrs = style_to_attributes(&text.style);
        let content: String = text
            .content
            .chars()
            .take(area.width as usize)
            .collect();

        if content.is_empty() {
            return;
        }

        self.surface
            .add_change(Change::CursorPosition {
                x: Position::Absolute(area.x),
                y: Position::Absolute(area.y),
            });
        self.surface
            .add_change(Change::AllAttributes(attrs.clone()));
        self.surface.add_change(Change::Text(content));
    }

    fn render_element<Msg>(&mut self, element: &ElementNode<Msg>, area: Rect) {
        match element.kind {
            ElementKind::Column => self.render_column(&element.children, area),
            ElementKind::Row => self.render_row(&element.children, area),
            ElementKind::Block => self.render_block(element, area),
        }
    }

    fn render_column<Msg>(&mut self, children: &[Node<Msg>], area: Rect) {
        if children.is_empty() {
            return;
        }

        let heights = distribute(area.height, children.len());
        let mut cursor_y = area.y;

        for (child, height) in children.iter().zip(heights.into_iter()) {
            if height == 0 {
                continue;
            }

            let child_area = Rect {
                x: area.x,
                y: cursor_y,
                width: area.width,
                height,
            };
            self.render_node(child, child_area);
            cursor_y += height as usize;
        }
    }

    fn render_row<Msg>(&mut self, children: &[Node<Msg>], area: Rect) {
        if children.is_empty() {
            return;
        }

        let widths = distribute(area.width, children.len());
        let mut cursor_x = area.x;

        for (child, width) in children.iter().zip(widths.into_iter()) {
            if width == 0 {
                continue;
            }

            let child_area = Rect {
                x: cursor_x,
                y: area.y,
                width,
                height: area.height,
            };
            self.render_node(child, child_area);
            cursor_x += width as usize;
        }
    }

    fn render_block<Msg>(&mut self, element: &ElementNode<Msg>, area: Rect) {
        self.draw_border(area, &element.attrs.style);

        if area.width < 2 || area.height < 2 {
            return;
        }

        let inner = Rect {
            x: area.x + 1,
            y: area.y + 1,
            width: area.width - 2,
            height: area.height - 2,
        };

        self.render_column(&element.children, inner);
    }

    fn draw_border(&mut self, area: Rect, style: &Style) {
        if area.width < 1 || area.height < 1 {
            return;
        }

        let attrs = style_to_attributes(style);

        // Top border
        self.write_char(area.x, area.y, '┌', &attrs);
        for x in (area.x + 1)..(area.x + area.width as usize - 1) {
            self.write_char(x, area.y, '─', &attrs);
        }
        if area.width > 1 {
            self.write_char(area.x + area.width as usize - 1, area.y, '┐', &attrs);
        }

        // Bottom border
        if area.height > 1 {
            let bottom = area.y + area.height as usize - 1;
            self.write_char(area.x, bottom, '└', &attrs);
            for x in (area.x + 1)..(area.x + area.width as usize - 1) {
                self.write_char(x, bottom, '─', &attrs);
            }
            if area.width > 1 {
                self.write_char(area.x + area.width as usize - 1, bottom, '┘', &attrs);
            }

            // Side borders
            for y in (area.y + 1)..bottom {
                self.write_char(area.x, y, '│', &attrs);
                if area.width > 1 {
                    self.write_char(area.x + area.width as usize - 1, y, '│', &attrs);
                }
            }
        }
    }

    fn write_char(&mut self, x: usize, y: usize, ch: char, attrs: &CellAttributes) {
        let (width, height) = self.surface.dimensions();
        if x >= width || y >= height {
            return;
        }

        self.surface
            .add_change(Change::CursorPosition {
                x: Position::Absolute(x),
                y: Position::Absolute(y),
            });
        self.surface
            .add_change(Change::AllAttributes(attrs.clone()));
        self.surface
            .add_change(Change::Text(ch.to_string()));
    }

    pub fn surface(&self) -> &Surface {
        &self.surface
    }

    pub fn surface_mut(&mut self) -> &mut Surface {
        &mut self.surface
    }
}

impl Default for Renderer {
    fn default() -> Self {
        Self::new()
    }
}

fn distribute(total: u16, count: usize) -> Vec<u16> {
    if count == 0 {
        return Vec::new();
    }

    let total = total as usize;
    let base = total / count;
    let mut remainder = total % count;
    let mut segments = Vec::with_capacity(count);

    for _ in 0..count {
        let mut size = base;
        if remainder > 0 {
            size += 1;
            remainder -= 1;
        }
        segments.push(size as u16);
    }

    segments
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
    use crate::dom::{block, column, row, text, Color};

    #[test]
    fn renders_text_node() {
        let mut renderer = Renderer::new();
        let node = text::<()>("hello");

        renderer
            .render(&node, Size::new(10, 4))
            .expect("render should succeed");

        let contents = renderer.surface.screen_chars_to_string();
        assert!(contents.starts_with("hello"));
    }

    #[test]
    fn column_stacks_children_vertically() {
        let mut renderer = Renderer::new();
        let node = column(vec![text::<()>("top"), text::<()>("bottom")]);

        renderer
            .render(&node, Size::new(6, 4))
            .expect("render should succeed");

        let screen = renderer.surface.screen_chars_to_string();
        let lines: Vec<&str> = screen.lines().collect();
        assert_eq!(lines[0].trim_end(), "top");
        assert_eq!(lines[2].trim_end(), "bottom");
    }

    #[test]
    fn row_places_children_horizontally() {
        let mut renderer = Renderer::new();
        let node = row(vec![text::<()>("left"), text::<()>("right")]);

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let first_line = renderer.surface.screen_chars_to_string();
        assert!(first_line.starts_with("left"));
        assert!(first_line.contains("right"));
    }

    #[test]
    fn block_draws_border() {
        let mut renderer = Renderer::new();
        let node = block::<()>(Vec::new());

        renderer
            .render(&node, Size::new(4, 3))
            .expect("render should succeed");

        let screen = renderer.surface.screen_chars_to_string();
        let lines: Vec<&str> = screen.lines().collect();
        assert_eq!(lines[0].chars().take(1).collect::<String>(), "┌");
        assert_eq!(lines[2].chars().take(1).collect::<String>(), "└");
    }

    #[test]
    fn style_applies_foreground_color() {
        let mut renderer = Renderer::new();
        let node = text::<()>("color").with_style(Style::fg(Color::Blue));

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let mut lines = renderer.surface.screen_lines();
        let line = lines.remove(0);
        let cell = line.visible_cells().next().unwrap();
        assert_eq!(cell.attrs().foreground(), AnsiColor::Blue.into());
    }
}
