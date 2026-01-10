use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::dom::{Node, Renderable, RenderablePatch, renderable};
use crate::geometry::Rect;
use crate::render::RenderContext;

use taffy::style::AvailableSpace;
use taffy::{Size as TaffySize, Style as TaffyStyle};

type RenderItem = dyn for<'a> Fn(usize, &mut RenderContext<'a>);
type MeasureItem =
    dyn Fn(usize, &TaffyStyle, TaffySize<Option<f32>>, TaffySize<AvailableSpace>) -> TaffySize<f32>;

pub struct VirtualizedColumn {
    item_count: usize,
    render_item: Rc<RenderItem>,
    measure_item: Rc<MeasureItem>,
    last_item_size: RefCell<Option<TaffySize<f32>>>,
}

impl VirtualizedColumn {
    pub fn new(
        item_count: usize,
        measure_item: impl Fn(
            usize,
            &TaffyStyle,
            TaffySize<Option<f32>>,
            TaffySize<AvailableSpace>,
        ) -> TaffySize<f32>
        + 'static,
        render_item: impl for<'a> Fn(usize, &mut RenderContext<'a>) + 'static,
    ) -> Self {
        Self {
            item_count,
            render_item: Rc::new(render_item),
            measure_item: Rc::new(measure_item),
            last_item_size: RefCell::new(None),
        }
    }

    pub fn into_node<'a, Msg: 'static>(self) -> Node<'a, Msg> {
        renderable(self)
    }
}

impl fmt::Debug for VirtualizedColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VirtualizedColumn")
            .field("item_count", &self.item_count)
            .finish()
    }
}

impl Renderable for VirtualizedColumn {
    fn patch_retained(&self, _other: &mut dyn Renderable) -> RenderablePatch {
        RenderablePatch::Replace
    }

    fn measure(
        &self,
        style: &TaffyStyle,
        known_dimensions: TaffySize<Option<f32>>,
        available_space: TaffySize<AvailableSpace>,
    ) -> TaffySize<f32> {
        if self.item_count == 0 {
            *self.last_item_size.borrow_mut() = None;
            return TaffySize {
                width: 0.0,
                height: 0.0,
            };
        }

        let item_size = (self.measure_item)(0, style, known_dimensions, available_space);
        *self.last_item_size.borrow_mut() = Some(item_size);

        TaffySize {
            width: item_size.width,
            height: item_size.height * self.item_count as f32,
        }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        if self.item_count == 0 {
            return;
        }

        let Some(item_size) = (*self.last_item_size.borrow()).or_else(|| {
            let layout = ctx.layout();
            if layout.size.height <= 0.0 {
                None
            } else {
                Some(TaffySize {
                    width: layout.size.width,
                    height: layout.size.height / self.item_count as f32,
                })
            }
        }) else {
            return;
        };

        let item_height = item_size.height.round().max(1.0) as usize;
        let item_width = item_size.width.round().max(1.0) as usize;
        if item_height == 0 || item_width == 0 {
            return;
        }

        let area = ctx.area();
        if area.width == 0 || area.height == 0 {
            return;
        }

        let scroll_y = ctx.scroll_y().max(0.0).round() as usize;
        let start = (scroll_y / item_height).min(self.item_count);
        let end = (scroll_y + area.height)
            .div_ceil(item_height)
            .min(self.item_count);
        let origin = ctx.origin();

        for index in start..end {
            let item_top = area.y as i32 + (index as i32 * item_height as i32) - scroll_y as i32;
            let item_bottom = item_top + item_height as i32;
            let clip_top = area.y as i32;
            let clip_bottom = (area.y + area.height) as i32;

            if item_bottom <= clip_top || item_top >= clip_bottom {
                continue;
            }

            let draw_top = item_top.max(clip_top);
            let draw_bottom = item_bottom.min(clip_bottom);
            let draw_height = (draw_bottom - draw_top) as usize;
            let draw_width = area.width.min(item_width);
            if draw_height == 0 || draw_width == 0 {
                continue;
            }

            let item_origin_y = origin.1.saturating_add(index.saturating_mul(item_height));
            let item_area = Rect::new(area.x, draw_top as usize, draw_width, draw_height);
            let mut item_ctx =
                ctx.sub_context((origin.0, item_origin_y), item_area, 0.0, ctx.scroll_x());

            (self.render_item)(index, &mut item_ctx);
        }
    }

    fn debug_label(&self) -> &'static str {
        "virtualized_column"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

pub fn virtualized_column<'a, Msg: 'static>(
    item_count: usize,
    measure_item: impl Fn(
        usize,
        &TaffyStyle,
        TaffySize<Option<f32>>,
        TaffySize<AvailableSpace>,
    ) -> TaffySize<f32>
    + 'static,
    render_item: impl for<'r> Fn(usize, &mut RenderContext<'r>) + 'static,
) -> Node<'a, Msg> {
    VirtualizedColumn::new(item_count, measure_item, render_item).into_node()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dom::RetainedNode;
    use crate::dom::rounding::round_layout;
    use crate::test_utils::render_node_to_lines;
    use taffy::prelude::AvailableSpace;
    use taffy::{Size as TaffySize, compute_root_layout};

    #[test]
    fn virtualized_column_renders_visible_rows_only() {
        let rendered = Rc::new(RefCell::new(Vec::new()));
        let rendered_handle = Rc::clone(&rendered);

        let measure_item = |_index: usize,
                            _style: &TaffyStyle,
                            _known: TaffySize<Option<f32>>,
                            _available: TaffySize<AvailableSpace>| {
            TaffySize {
                width: 4.0,
                height: 1.0,
            }
        };

        let render_item = move |index: usize, _ctx: &mut RenderContext<'_>| {
            rendered_handle.borrow_mut().push(index);
        };

        let node: Node<()> = virtualized_column(10, measure_item, render_item).with_scroll(2.0);
        render_node_to_lines(node, 4, 3).expect("render should succeed");

        assert_eq!(*rendered.borrow(), vec![2, 3, 4]);
    }

    #[test]
    fn virtualized_column_measure_scales_with_item_count() {
        let measure_item = |_index: usize,
                            _style: &TaffyStyle,
                            _known: TaffySize<Option<f32>>,
                            _available: TaffySize<AvailableSpace>| {
            TaffySize {
                width: 6.0,
                height: 2.0,
            }
        };

        let render_item = |_index: usize, _ctx: &mut RenderContext<'_>| {};

        let node: Node<()> = virtualized_column(5, measure_item, render_item);
        let mut node: RetainedNode<()> = node.into();

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            TaffySize {
                width: AvailableSpace::Definite(100.0),
                height: AvailableSpace::Definite(100.0),
            },
        );
        round_layout(&mut node);

        let layout = node.layout_state.layout;
        assert_eq!(layout.size.width, 6.0);
        assert_eq!(layout.size.height, 10.0);
    }

    #[test]
    fn virtualized_column_handles_zero_items() {
        let rendered = Rc::new(RefCell::new(Vec::new()));
        let rendered_handle = Rc::clone(&rendered);

        let measure_item = |_index: usize,
                            _style: &TaffyStyle,
                            _known: TaffySize<Option<f32>>,
                            _available: TaffySize<AvailableSpace>| {
            TaffySize {
                width: 6.0,
                height: 2.0,
            }
        };

        let render_item = move |index: usize, _ctx: &mut RenderContext<'_>| {
            rendered_handle.borrow_mut().push(index);
        };

        let node: Node<()> = virtualized_column(0, measure_item, render_item);
        let mut node: RetainedNode<()> = node.into();

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            TaffySize {
                width: AvailableSpace::Definite(100.0),
                height: AvailableSpace::Definite(100.0),
            },
        );
        round_layout(&mut node);

        let layout = node.layout_state.layout;
        assert_eq!(layout.size.width, 0.0);
        assert_eq!(layout.size.height, 0.0);

        render_node_to_lines(node, 4, 3).expect("render should succeed");
        assert!(rendered.borrow().is_empty());
    }
}
