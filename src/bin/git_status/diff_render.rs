use chatui::dom::Renderable;
use chatui::render::RenderContext;
use chatui::Style;
use taffy::{self, AvailableSpace};
use tracing::info;

use super::{highlight, DiffLine};

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
            .map(|line| {
                gutter_width
                    + line
                        .spans
                        .iter()
                        .map(|span| span.content.len())
                        .sum::<usize>()
            })
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
            let mut remaining_skip = skip_cols;

            if let Some(gutter) = &self.gutter {
                let gutter_text = gutter.format_line(&self.lines[idx]);
                let mut chars = gutter_text.chars();

                if remaining_skip > 0 {
                    let skipped = chars.by_ref().take(remaining_skip).count();
                    remaining_skip = remaining_skip.saturating_sub(skipped);
                }

                if remaining_skip == 0 && remaining > 0 {
                    let mut collected = String::new();
                    let mut taken = 0;
                    for ch in chars {
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
                let mut chars = span.content.chars();

                if span.style.bg.is_some() || span.style.dim {
                    fill_style = Some(span.style.clone());
                }
                if remaining_skip > 0 {
                    let skipped = chars.by_ref().take(remaining_skip).count();
                    remaining_skip = remaining_skip.saturating_sub(skipped);
                    if remaining_skip > 0 {
                        continue;
                    }
                }

                let mut collected = String::new();
                let mut taken = 0;
                for ch in chars {
                    if remaining == 0 {
                        break;
                    }
                    collected.push(ch);
                    taken += 1;
                    remaining = remaining.saturating_sub(1);
                }

                if collected.is_empty() {
                    continue;
                }
                let attrs = ctx.style_to_attributes(&span.style);
                ctx.write_text(cursor_x, y, &collected, &attrs);
                cursor_x += taken;
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
