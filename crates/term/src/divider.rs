use chatui::dom::{Node, Renderable, Style, renderable};
use chatui::render::RenderContext;
use taffy::Dimension;

#[derive(Clone, Copy, Debug)]
pub enum DividerOrientation {
    Horizontal,
    Vertical,
}

#[derive(Clone, Copy, Debug)]
pub struct Divider {
    pub orientation: DividerOrientation,
    pub style: Style,
}

impl Divider {
    pub fn vertical(style: Style) -> Self {
        Self {
            orientation: DividerOrientation::Vertical,
            style,
        }
    }

    pub fn horizontal(style: Style) -> Self {
        Self {
            orientation: DividerOrientation::Horizontal,
            style,
        }
    }
}

impl Renderable for Divider {
    fn measure(
        &self,
        _style: &taffy::Style,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
    ) -> taffy::Size<f32> {
        let width = known_dimensions.width.unwrap_or(match self.orientation {
            DividerOrientation::Vertical => 1.0,
            DividerOrientation::Horizontal => match available_space.width {
                taffy::AvailableSpace::Definite(w) => w,
                taffy::AvailableSpace::MinContent => 1.0,
                taffy::AvailableSpace::MaxContent => 1.0,
            },
        });

        let height = known_dimensions.height.unwrap_or(match self.orientation {
            DividerOrientation::Horizontal => 1.0,
            DividerOrientation::Vertical => match available_space.height {
                taffy::AvailableSpace::Definite(h) => h,
                taffy::AvailableSpace::MinContent => 1.0,
                taffy::AvailableSpace::MaxContent => 1.0,
            },
        });

        taffy::Size { width, height }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        let area = ctx.area();
        if area.width == 0 || area.height == 0 {
            return;
        }

        let attrs = ctx.style_to_attributes(&self.style);
        match self.orientation {
            DividerOrientation::Vertical => {
                for y in area.y..area.y + area.height {
                    ctx.write_char(area.x, y, '│', &attrs);
                }
            }
            DividerOrientation::Horizontal => {
                for x in area.x..area.x + area.width {
                    ctx.write_char(x, area.y, '─', &attrs);
                }
            }
        }
    }

    fn debug_label(&self) -> &'static str {
        "divider"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub fn vertical_divider<Msg>(style: Style) -> Node<'static, Msg> {
    renderable(Divider::vertical(style))
        .with_width(Dimension::length(1.0))
        .with_min_width(Dimension::length(1.0))
        .with_max_width(Dimension::length(1.0))
        .with_height(Dimension::percent(1.0))
        .with_flex_shrink(0.0)
}

pub fn horizontal_divider<Msg>(style: Style) -> Node<'static, Msg> {
    renderable(Divider::horizontal(style))
        .with_height(Dimension::length(1.0))
        .with_min_height(Dimension::length(1.0))
        .with_max_height(Dimension::length(1.0))
        .with_width(Dimension::percent(1.0))
        .with_flex_shrink(0.0)
}
