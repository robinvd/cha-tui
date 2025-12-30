use std::{
    cell::Cell,
    rc::Rc,
    time::{Duration, Instant},
};

use crate::{
    dom::{Node, PendingScroll},
    event::{LocalMouseEvent, MouseEventKind, MouseScrollAxis, MouseScrollDirection, Size},
    hash,
    scroll::ScrollAlignment,
};
use taffy::Overflow;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScrollAxis {
    Vertical,
    Horizontal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScrollBehavior {
    Vertical,
    Horizontal,
    Both,
}

#[derive(Clone, Debug)]
pub struct ScrollState {
    offset_x: f32,
    offset_y: f32,
    viewport: Size,
    content: Size,
    pending_request: Cell<Option<PendingScrollRequest>>,
    behavior: ScrollBehavior,
    axis_lock: Option<AxisLock>,
}

impl Default for ScrollState {
    fn default() -> Self {
        Self::vertical()
    }
}

impl ScrollState {
    pub fn new(behavior: ScrollBehavior) -> Self {
        Self {
            offset_x: 0.0,
            offset_y: 0.0,
            viewport: Size {
                width: 0,
                height: 0,
            },
            content: Size {
                width: 0,
                height: 0,
            },
            pending_request: Cell::new(None),
            behavior,
            axis_lock: None,
        }
    }

    pub fn vertical() -> Self {
        Self::new(ScrollBehavior::Vertical)
    }

    pub fn horizontal() -> Self {
        Self::new(ScrollBehavior::Horizontal)
    }

    pub fn both() -> Self {
        Self::new(ScrollBehavior::Both)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ScrollMsg {
    AxisDelta { axis: ScrollAxis, amount: i32 },
    AxisDeltaPercent { axis: ScrollAxis, ratio: f64 },
    Resize { viewport: Size, content: Size },
    JumpTo(f32),
    AxisJumpTo { axis: ScrollAxis, offset: f32 },
}

#[derive(Clone, Copy, Debug)]
pub struct ScrollTarget {
    id: &'static str,
    mixin: Option<u64>,
}

impl ScrollTarget {
    pub fn new(id: &'static str) -> Self {
        Self { id, mixin: None }
    }

    pub fn with_mixin(id: &'static str, mixin: u64) -> Self {
        Self {
            id,
            mixin: Some(mixin),
        }
    }

    fn hashed(&self) -> u64 {
        hash::hash_str(self.mixin.unwrap_or(0), self.id)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ScrollRequestParams {
    pub alignment: ScrollAlignment,
}

impl Default for ScrollRequestParams {
    fn default() -> Self {
        Self {
            alignment: ScrollAlignment::Nearest,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct PendingScrollRequest {
    container_hash: u64,
    target_hash: u64,
    params: ScrollRequestParams,
}

impl ScrollState {
    pub fn offset(&self) -> f32 {
        self.offset_for(self.primary_axis())
    }

    pub fn offset_x(&self) -> f32 {
        self.offset_x
    }

    pub fn offset_y(&self) -> f32 {
        self.offset_y
    }

    pub fn offset_for(&self, axis: ScrollAxis) -> f32 {
        match axis {
            ScrollAxis::Vertical => self.offset_y,
            ScrollAxis::Horizontal => self.offset_x,
        }
    }

    pub fn behavior(&self) -> ScrollBehavior {
        self.behavior
    }

    pub fn is_at_end(&self, axis: ScrollAxis) -> bool {
        let max_offset = self.max_offset_for(axis) as f32;
        let offset = self.offset_for(axis);
        max_offset <= 0.0 || (max_offset - offset).abs() <= SCROLL_END_TOLERANCE
    }

    pub fn set_offset(&mut self, offset: f32) {
        let axis = self.primary_axis();
        self.set_offset_for(axis, offset);
    }

    pub fn set_offset_for(&mut self, axis: ScrollAxis, offset: f32) {
        match axis {
            ScrollAxis::Vertical => self.offset_y = offset,
            ScrollAxis::Horizontal => self.offset_x = offset,
        }
        self.clamp_offsets();
    }

    pub fn update(&mut self, msg: ScrollMsg) {
        match msg {
            ScrollMsg::AxisDelta { axis, amount } => self.apply_delta(axis, amount),
            ScrollMsg::AxisDeltaPercent { axis, ratio } => self.apply_delta_percent(axis, ratio),
            ScrollMsg::Resize { viewport, content } => {
                self.viewport = viewport;
                self.content = content;
                self.clamp_offsets();
                self.axis_lock = None;
            }
            ScrollMsg::JumpTo(offset) => {
                let axis = self.primary_axis();
                self.set_offset_for(axis, offset);
            }
            ScrollMsg::AxisJumpTo { axis, offset } => self.set_offset_for(axis, offset),
        }
    }

    pub fn reset(&mut self) {
        self.offset_x = 0.0;
        self.offset_y = 0.0;
        self.axis_lock = None;
        self.clamp_offsets();
    }

    pub fn request_scroll_to(
        &mut self,
        container_id: &'static str,
        target: ScrollTarget,
        params: ScrollRequestParams,
    ) {
        let container_hash = hash::hash_str(0, container_id);
        let target_hash = target.hashed();
        self.pending_request.set(Some(PendingScrollRequest {
            container_hash,
            target_hash,
            params,
        }));
    }

    pub fn ensure_visible(&mut self, container_id: &'static str, target: ScrollTarget) {
        self.request_scroll_to(container_id, target, ScrollRequestParams::default());
    }

    pub(crate) fn take_pending_request(&self, container_hash: u64) -> Option<PendingScrollRequest> {
        if let Some(request) = self.pending_request.get()
            && request.container_hash == container_hash
        {
            self.pending_request.set(None);
            return Some(request);
        }
        None
    }

    fn primary_axis(&self) -> ScrollAxis {
        match self.behavior {
            ScrollBehavior::Vertical => ScrollAxis::Vertical,
            ScrollBehavior::Horizontal => ScrollAxis::Horizontal,
            ScrollBehavior::Both => ScrollAxis::Vertical,
        }
    }

    fn apply_delta(&mut self, axis: ScrollAxis, delta: i32) {
        let now = Instant::now();
        self.apply_delta_internal(axis, delta, now);
    }

    fn apply_delta_percent(&mut self, axis: ScrollAxis, ratio: f64) {
        let size = match axis {
            ScrollAxis::Vertical => self.viewport.height,
            ScrollAxis::Horizontal => self.viewport.width,
        };

        let delta = (size as f64 * ratio).round() as i32;

        self.apply_delta(axis, delta);
    }
    fn apply_delta_internal(&mut self, axis: ScrollAxis, delta: i32, now: Instant) {
        if !self.should_handle_axis(axis, now) {
            return;
        }

        let max_offset = self.max_offset_for(axis);
        let offset = match axis {
            ScrollAxis::Vertical => &mut self.offset_y,
            ScrollAxis::Horizontal => &mut self.offset_x,
        };
        let mut next = *offset as i32 + delta;
        if next < 0 {
            next = 0;
        }
        if next > max_offset {
            next = max_offset;
        }
        *offset = next as f32;
    }

    #[cfg(test)]
    pub(crate) fn apply_delta_at(&mut self, axis: ScrollAxis, delta: i32, now: Instant) {
        self.apply_delta_internal(axis, delta, now);
    }

    fn should_handle_axis(&mut self, axis: ScrollAxis, now: Instant) -> bool {
        if !self.axis_enabled(axis) {
            return false;
        }

        if self.behavior != ScrollBehavior::Both {
            return true;
        }

        if self
            .axis_lock
            .is_some_and(|lock| lock.expires_at >= now && lock.axis != axis)
        {
            return false;
        }

        self.axis_lock = Some(AxisLock {
            axis,
            expires_at: now + AXIS_LOCK_DURATION,
        });
        true
    }

    fn axis_enabled(&self, axis: ScrollAxis) -> bool {
        match self.behavior {
            ScrollBehavior::Vertical => axis == ScrollAxis::Vertical,
            ScrollBehavior::Horizontal => axis == ScrollAxis::Horizontal,
            ScrollBehavior::Both => true,
        }
    }

    fn max_offset_for(&self, axis: ScrollAxis) -> i32 {
        let (viewport, content) = match axis {
            ScrollAxis::Vertical => (
                i32::from(self.viewport.height),
                i32::from(self.content.height),
            ),
            ScrollAxis::Horizontal => (
                i32::from(self.viewport.width),
                i32::from(self.content.width),
            ),
        };
        let available = content.saturating_sub(viewport);
        available.max(0)
    }

    fn clamp_offsets(&mut self) {
        match self.behavior {
            ScrollBehavior::Vertical => self.clamp_offset_for(ScrollAxis::Vertical),
            ScrollBehavior::Horizontal => self.clamp_offset_for(ScrollAxis::Horizontal),
            ScrollBehavior::Both => {
                self.clamp_offset_for(ScrollAxis::Vertical);
                self.clamp_offset_for(ScrollAxis::Horizontal);
            }
        }
    }

    fn clamp_offset_for(&mut self, axis: ScrollAxis) {
        let max_offset = self.max_offset_for(axis) as f32;
        let offset = match axis {
            ScrollAxis::Vertical => &mut self.offset_y,
            ScrollAxis::Horizontal => &mut self.offset_x,
        };
        if max_offset <= 0.0 {
            *offset = 0.0;
        } else {
            *offset = offset.clamp(0.0, max_offset);
        }
    }
}

const AXIS_LOCK_DURATION: Duration = Duration::from_millis(200);
const SCROLL_END_TOLERANCE: f32 = 0.5;

#[derive(Clone, Copy, Debug)]
struct AxisLock {
    axis: ScrollAxis,
    expires_at: Instant,
}

pub fn scrollable_content<Msg>(
    id: &'static str,
    state: &ScrollState,
    wheel_step: i32,
    map_msg: impl Fn(ScrollMsg) -> Msg + 'static,
    child: Node<Msg>,
) -> Node<Msg> {
    let behavior = state.behavior();
    let map_msg = Rc::new(map_msg);
    let mouse_handler = Rc::clone(&map_msg);
    let resize_handler = Rc::clone(&map_msg);
    let step = wheel_step.abs().max(1);
    let container_hash = hash::hash_str(0, id);
    let pending_request = state.take_pending_request(container_hash);

    let mut node = child.with_id(id);
    node = match behavior {
        ScrollBehavior::Vertical => node
            .with_overflow_y(Overflow::Scroll)
            .with_scroll(state.offset()),
        ScrollBehavior::Horizontal => node
            .with_overflow_x(Overflow::Scroll)
            .with_scroll_x(state.offset()),
        ScrollBehavior::Both => node
            .with_overflow_y(Overflow::Scroll)
            .with_scroll(state.offset_y())
            .with_overflow_x(Overflow::Scroll)
            .with_scroll_x(state.offset_x()),
    };

    node = node.on_mouse(move |event: LocalMouseEvent| {
        let MouseEventKind::Scroll(scroll) = event.event.kind else {
            return None;
        };

        let direction = match scroll.direction {
            MouseScrollDirection::Positive => -step,
            MouseScrollDirection::Negative => step,
        };

        match behavior {
            ScrollBehavior::Vertical => {
                if scroll.axis != MouseScrollAxis::Vertical {
                    return None;
                }
                Some(mouse_handler(ScrollMsg::AxisDelta {
                    axis: ScrollAxis::Vertical,
                    amount: direction,
                }))
            }
            ScrollBehavior::Horizontal => {
                let scroll_axis =
                    if scroll.axis == MouseScrollAxis::Vertical && event.event.modifiers.shift {
                        MouseScrollAxis::Horizontal
                    } else {
                        scroll.axis
                    };

                if scroll_axis != MouseScrollAxis::Horizontal {
                    return None;
                }

                Some(mouse_handler(ScrollMsg::AxisDelta {
                    axis: ScrollAxis::Horizontal,
                    amount: direction,
                }))
            }
            ScrollBehavior::Both => {
                let scroll_axis =
                    if scroll.axis == MouseScrollAxis::Vertical && event.event.modifiers.shift {
                        MouseScrollAxis::Horizontal
                    } else {
                        scroll.axis
                    };

                let axis = match scroll_axis {
                    MouseScrollAxis::Horizontal => ScrollAxis::Horizontal,
                    MouseScrollAxis::Vertical => ScrollAxis::Vertical,
                };

                Some(mouse_handler(ScrollMsg::AxisDelta {
                    axis,
                    amount: direction,
                }))
            }
        }
    });

    node = node.on_resize(move |layout| {
        let to_u16 = |value: f32| value.max(0.0).round() as u16;
        let viewport_box = layout.content_box_size();
        let scrollbar = layout.scrollbar_size;
        let viewport_width = (viewport_box.width - scrollbar.width).max(0.0);
        let viewport_height = (viewport_box.height - scrollbar.height).max(0.0);
        Some(resize_handler(ScrollMsg::Resize {
            viewport: Size {
                width: to_u16(viewport_width),
                height: to_u16(viewport_height),
            },
            content: Size {
                width: to_u16(layout.content_size.width),
                height: to_u16(layout.content_size.height),
            },
        }))
    });

    if let Some(request) = pending_request {
        let jump_handler = Rc::clone(&map_msg);
        let callback = Rc::new(move |offset: f32| jump_handler(ScrollMsg::JumpTo(offset)));
        node = node.with_pending_scroll(PendingScroll {
            target_hash: request.target_hash,
            alignment: request.params.alignment,
            callback,
        });
    }

    node
}

#[cfg(test)]
mod tests;
