use chatui::Style;
use chatui::dom::Renderable;
use chatui::render::RenderContext;
use taffy::{self, AvailableSpace};
use tracing::info;
use unicode_width::UnicodeWidthChar;

use super::{DiffLine, highlight};

const TAB_WIDTH: usize = 8;

#[derive(Clone, Debug)]
pub(super) struct DiffLeaf {
    lines: Vec<DiffLine>,
    gutter: Option<GutterConfig>,
}

impl DiffLeaf {
    pub(super) fn new(lines: Vec<DiffLine>, show_line_numbers: bool) -> Self {
        let gutter = if show_line_numbers {
            GutterConfig::from_lines(&lines)
        } else {
            None
        };

        Self { lines, gutter }
    }

    fn line_display_width(line: &DiffLine) -> usize {
        let mut column = 0usize;
        for span in &line.spans {
            for ch in span.content.chars() {
                let width = if ch == '\t' {
                    let next_tab_stop = ((column / TAB_WIDTH) + 1) * TAB_WIDTH;
                    (next_tab_stop - column).max(1)
                } else {
                    UnicodeWidthChar::width(ch).unwrap_or(0).max(1)
                };
                column += width;
            }
        }
        column
    }
}

impl Renderable for DiffLeaf {
    fn eq(&self, other: &dyn Renderable) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|o| o.lines == self.lines && o.gutter == self.gutter)
            .unwrap_or(false)
    }

    fn measure(
        &self,
        _style: &taffy::Style,
        _known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<AvailableSpace>,
    ) -> taffy::Size<f32> {
        let height = self.lines.len() as f32;
        let gutter_width = self
            .gutter
            .as_ref()
            .map(|gutter| gutter.total_width())
            .unwrap_or_default();
        let max_width = self
            .lines
            .iter()
            .map(|line| gutter_width + Self::line_display_width(line))
            .max()
            .unwrap_or(0) as f32;

        let width = match available_space.width {
            AvailableSpace::Definite(w) => w.max(max_width),
            AvailableSpace::MinContent => max_width,
            AvailableSpace::MaxContent => max_width,
        };

        taffy::Size { width, height }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        let area = ctx.area();
        let start_idx = ctx.scroll_y().max(0.0).floor() as usize;
        let skip_cols = ctx.scroll_x().max(0.0).floor() as usize;
        let content_skip = skip_cols;
        let end_row = area.y.saturating_add(area.height);
        let visible_height = end_row
            .saturating_sub(area.y)
            .min(self.lines.len().saturating_sub(start_idx));

        info!("diff render, {}:{}", ctx.scroll_x(), ctx.scroll_y());

        let gutter_style = line_number_style();

        for (i, line_idx) in (0..visible_height).enumerate() {
            let idx = start_idx + line_idx;
            let y = area.y + i;
            if idx >= self.lines.len() || y >= area.y + area.height {
                break;
            }

            let mut remaining = area.width;
            let mut cursor_x = area.x;
            let mut fill_style: Option<Style> = None;
            let mut remaining_skip = content_skip;
            let mut line_column = 0usize;

            if let Some(gutter) = &self.gutter {
                let gutter_text = gutter.format_line(&self.lines[idx]);
                if remaining > 0 {
                    let mut collected = String::new();
                    let mut taken = 0;
                    for ch in gutter_text.chars() {
                        if remaining == 0 {
                            break;
                        }
                        collected.push(ch);
                        taken += 1;
                        remaining = remaining.saturating_sub(1);
                    }

                    if !collected.is_empty() {
                        let attrs = ctx.style_to_attributes(&gutter_style);
                        ctx.write_text(cursor_x, y, &collected, &attrs);
                        cursor_x += taken;
                    }
                }
            }

            for span in &self.lines[idx].spans {
                if remaining == 0 {
                    break;
                }

                if span.style.bg.is_some() || span.style.dim {
                    fill_style = Some(span.style);
                }

                let mut collected = String::new();
                let mut taken_cols = 0;

                for ch in span.content.chars() {
                    if remaining == 0 {
                        break;
                    }

                    let width = if ch == '\t' {
                        let next_tab_stop = ((line_column / TAB_WIDTH) + 1) * TAB_WIDTH;
                        (next_tab_stop - line_column).max(1)
                    } else {
                        UnicodeWidthChar::width(ch).unwrap_or(0).max(1)
                    };

                    let skip_for_char = remaining_skip.min(width);
                    remaining_skip -= skip_for_char;

                    let next_column = line_column + width;

                    if skip_for_char == width {
                        line_column = next_column;
                        continue;
                    }

                    let visible_width = width - skip_for_char;

                    if visible_width > remaining {
                        line_column = next_column;
                        break;
                    }

                    if ch == '\t' || skip_for_char > 0 {
                        collected.extend(std::iter::repeat_n(' ', visible_width));
                    } else {
                        collected.push(ch);
                    }

                    taken_cols += visible_width;
                    remaining = remaining.saturating_sub(visible_width);
                    line_column = next_column;
                }

                if collected.is_empty() {
                    continue;
                }

                let attrs = ctx.style_to_attributes(&span.style);
                ctx.write_text(cursor_x, y, &collected, &attrs);
                cursor_x += taken_cols;
            }
            if remaining > 0
                && let Some(style) = &fill_style
            {
                let attrs = ctx.style_to_attributes(style);
                let padding = " ".repeat(remaining);
                ctx.write_text(cursor_x, y, &padding, &attrs);
            }
        }
    }

    fn debug_label(&self) -> &'static str {
        "diff"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
struct GutterConfig {
    old_width: usize,
    new_width: usize,
}

impl GutterConfig {
    fn from_lines(lines: &[DiffLine]) -> Option<Self> {
        let mut has_numbers = false;
        let mut old_width = 0;
        let mut new_width = 0;

        for line in lines {
            if let Some(numbers) = &line.line_numbers {
                if let Some(old) = numbers.old {
                    has_numbers = true;
                    old_width = old_width.max(digit_count(old));
                }
                if let Some(new) = numbers.new {
                    has_numbers = true;
                    new_width = new_width.max(digit_count(new));
                }
            }
        }

        has_numbers.then_some(Self {
            old_width,
            new_width,
        })
    }

    fn total_width(&self) -> usize {
        self.old_display_width() + 1 + self.new_display_width() + 3
    }

    fn format_line(&self, line: &DiffLine) -> String {
        let (old, new) = line
            .line_numbers
            .as_ref()
            .map(|numbers| {
                (
                    numbers
                        .old
                        .map(|value| value.to_string())
                        .unwrap_or_default(),
                    numbers
                        .new
                        .map(|value| value.to_string())
                        .unwrap_or_default(),
                )
            })
            .unwrap_or_else(|| (String::new(), String::new()));

        format!(
            "{old:>old_width$} {new:>new_width$} | ",
            old = old,
            new = new,
            old_width = self.old_display_width(),
            new_width = self.new_display_width(),
        )
    }

    fn old_display_width(&self) -> usize {
        self.old_width.max(1)
    }

    fn new_display_width(&self) -> usize {
        self.new_width.max(1)
    }
}

fn digit_count(value: usize) -> usize {
    let mut digits = 1;
    let mut n = value;
    while n >= 10 {
        n /= 10;
        digits += 1;
    }
    digits
}

fn line_number_style() -> Style {
    let mut style = Style::fg(highlight::EVERFOREST_GREY2);
    style.dim = true;
    style
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::components::scroll::{ScrollAxis, ScrollMsg, ScrollState};
    use chatui::{
        Size, block_with_title,
        buffer::DoubleBuffer,
        column,
        dom::{renderable, rounding},
        palette::Palette,
        render::Renderer,
        scrollable_content,
    };
    use taffy::prelude::TaffyZero;
    use taffy::{AvailableSpace, Dimension, Overflow, compute_root_layout};

    fn render_diff_leaf(leaf: DiffLeaf, scroll_x: f32, width: u16, height: u16) -> Vec<Vec<char>> {
        let mut node = renderable::<()>(leaf)
            .with_overflow_x(Overflow::Scroll)
            .with_scroll_x(scroll_x)
            .with_width(Dimension::length(width as f32))
            .with_height(Dimension::length(height as f32));

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(width as f32),
                height: AvailableSpace::Definite(height as f32),
            },
        );
        rounding::round_layout(&mut node);

        let mut buffer = DoubleBuffer::new(width as usize, height as usize);
        let palette = Palette::default();
        Renderer::new(&mut buffer, &palette)
            .render(&node, Size::new(width, height))
            .expect("render diff leaf");

        buffer
            .back_buffer()
            .iter()
            .map(|row| row.iter().map(|cell| cell.ch).collect())
            .collect()
    }

    #[test]
    fn horizontal_scroll_skips_partial_tab_columns_without_gutter() {
        let line = DiffLine::plain("\tXYZ");
        let rendered = render_diff_leaf(DiffLeaf::new(vec![line], false), 3.0, 16, 3);

        let row: String = rendered[0].iter().collect();
        let first_x = row.find('X').expect("expected X in rendered row");
        assert_eq!(
            first_x, 5,
            "expected scroll to remove three columns of the tab: {row:?}"
        );
        assert!(
            row[..first_x].chars().all(|c| c == ' '),
            "expected indentation to be spaces only: {row:?}"
        );
    }

    #[test]
    fn horizontal_scroll_skips_partial_tab_columns_with_gutter() {
        let lines = vec![DiffLine::plain("\tXYZ").with_line_numbers(Some(42), Some(43))];
        let gutter_width = GutterConfig::from_lines(&lines)
            .expect("lines with gutter should produce config")
            .total_width();
        let rendered = render_diff_leaf(DiffLeaf::new(lines, true), 3.0, 20, 3);

        let row = &rendered[0];
        assert!(
            row.len() >= gutter_width,
            "expected rendered row to contain full gutter"
        );
        let gutter: String = row[..gutter_width].iter().collect();
        assert!(
            gutter.ends_with("| "),
            "expected gutter to remain visible when scrolling: {gutter:?}"
        );

        let content: Vec<char> = row[gutter_width..].to_vec();
        let first_x = content
            .iter()
            .position(|&c| c == 'X')
            .expect("expected X in rendered row");
        assert_eq!(first_x, 5, "expected scroll to skip three tab columns");
        assert!(
            content[..first_x].iter().all(|&c| c == ' '),
            "expected indentation to contain only spaces after gutter skip: {content:?}"
        );
    }

    #[test]
    fn horizontal_scroll_reaches_line_end_with_gutter() {
        let content = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let lines = vec![DiffLine::plain(content).with_line_numbers(Some(10), Some(11))];
        let gutter_width = GutterConfig::from_lines(&lines)
            .expect("lines with gutter should produce config")
            .total_width();
        let viewport_width = (gutter_width + 6) as u16;
        let scroll_x = (gutter_width + content.len() - viewport_width as usize) as f32;

        let rendered = render_diff_leaf(DiffLeaf::new(lines, true), scroll_x, viewport_width, 1);
        let row = &rendered[0];
        assert!(
            row.len() >= gutter_width,
            "expected rendered row to contain full gutter"
        );
        let last = row
            .last()
            .copied()
            .expect("expected rendered row to contain content");
        assert_eq!(last, 'Z', "expected to reach end of content when scrolled");
    }

    #[test]
    fn horizontal_scroll_reaches_line_end_without_gutter() {
        let content = "0123456789";
        let viewport_width = 6u16;
        let scroll_x = (content.len() - viewport_width as usize) as f32;

        let rendered = render_diff_leaf(
            DiffLeaf::new(vec![DiffLine::plain(content)], false),
            scroll_x,
            viewport_width,
            1,
        );
        let row = &rendered[0];
        let last_visible = row
            .iter()
            .rev()
            .find(|&&ch| ch != ' ')
            .copied()
            .expect("expected rendered row to contain content");
        assert_eq!(
            last_visible, '9',
            "expected to reach end of content in narrow viewport"
        );
    }

    #[test]
    fn diff_pane_drops_trailing_characters_when_viewport_is_narrow() {
        let diff_text = "0123456789abcdefghijklmnopqrstuvwxyz";
        let content_width = diff_text.len() as u16;
        let viewport_width = 24u16;

        let mut scroll = ScrollState::both();
        let inner_viewport_width = viewport_width.saturating_sub(3); // block borders + reserved scrollbar column
        let inner_viewport_height = 1u16;
        scroll.update(ScrollMsg::Resize {
            viewport: Size::new(inner_viewport_width, inner_viewport_height),
            content: Size::new(content_width, inner_viewport_height),
        });
        scroll.set_offset_for(
            ScrollAxis::Horizontal,
            content_width.saturating_sub(inner_viewport_width) as f32,
        );

        let diff_lines = renderable::<()>(DiffLeaf::new(vec![DiffLine::plain(diff_text)], false))
            .with_flex_shrink(0.)
            .with_overflow_y(Overflow::Visible)
            .with_overflow_x(Overflow::Visible)
            .with_id("diff_lines");

        let block = block_with_title("Diff Preview", vec![diff_lines]);

        let pane_content = scrollable_content::<()>("diff-pane-content", &scroll, 3, |_| (), block)
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO);

        let mut pane = column::<()>(vec![pane_content])
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO)
            .with_id("diff-pane")
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));
        compute_root_layout(
            &mut pane,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(24.0),
                height: AvailableSpace::Definite(6.0),
            },
        );
        chatui::dom::rounding::round_layout(&mut pane);

        let mut buffer = DoubleBuffer::new(24, 6);
        let palette = Palette::default();
        Renderer::new(&mut buffer, &palette)
            .render(&pane, Size::new(24, 6))
            .expect("render diff pane");

        let rows: Vec<String> = buffer
            .back_buffer()
            .iter()
            .map(|row| row.iter().map(|cell| cell.ch).collect())
            .collect();
        let content_row = rows
            .iter()
            .find(|row| {
                row.starts_with('│')
                    && row.ends_with('│')
                    && row.chars().any(|ch| ch.is_ascii_alphanumeric())
            })
            .expect("expected diff content row");
        let visible = content_row
            .trim_matches(|ch| ch == '│' || ch == ' ')
            .to_string();
        let expected_tail = &diff_text[diff_text.len() - inner_viewport_width as usize..];
        assert_eq!(
            visible, expected_tail,
            "expected diff pane to reveal the full tail when scrolled"
        );
    }
}
