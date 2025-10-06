use std::{cell::Cell, rc::Rc};

use crate::{
    dom::{Node, PendingScroll},
    event::Size,
    hash,
    scroll::ScrollAlignment,
};
use taffy::Overflow;
use tracing::info;

#[derive(Clone, Debug)]
pub struct ScrollState {
    offset: f32,
    viewport: Size,
    content: Size,
    pending_request: Cell<Option<PendingScrollRequest>>,
}

impl Default for ScrollState {
    fn default() -> Self {
        Self {
            offset: 0.0,
            viewport: Size {
                width: 0,
                height: 0,
            },
            content: Size {
                width: 0,
                height: 0,
            },
            pending_request: Cell::new(None),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ScrollMsg {
    Delta(i32),
    Resize { viewport: Size, content: Size },
    JumpTo(f32),
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
        self.offset
    }

    pub fn set_offset(&mut self, offset: f32) {
        self.offset = offset;
        self.clamp_offset();
    }

    pub fn update(&mut self, msg: ScrollMsg) {
        match msg {
            ScrollMsg::Delta(delta) => self.apply_delta(delta),
            ScrollMsg::Resize { viewport, content } => {
                self.viewport = viewport;
                self.content = content;
                self.clamp_offset();
            }
            ScrollMsg::JumpTo(offset) => self.set_offset(offset),
        }
        info!("scroll state update {self:?}");
    }

    pub fn reset(&mut self) {
        self.offset = 0.0;
        self.clamp_offset();
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

    fn apply_delta(&mut self, delta: i32) {
        let mut next = self.offset as i32 + delta;
        if next < 0 {
            next = 0;
        }
        let max_offset = self.max_offset();
        if next > max_offset {
            next = max_offset;
        }
        self.offset = next as f32;
    }

    fn max_offset(&self) -> i32 {
        let viewport_height = i32::from(self.viewport.height);
        let content_height = i32::from(self.content.height);
        let available = content_height.saturating_sub(viewport_height);
        available.max(0)
    }

    fn clamp_offset(&mut self) {
        let max_offset = self.max_offset() as f32;
        if max_offset <= 0.0 {
            self.offset = 0.0;
        } else {
            self.offset = self.offset.clamp(0.0, max_offset);
        }
    }
}

pub fn scrollable_content<Msg>(
    id: &'static str,
    state: &ScrollState,
    wheel_step: i32,
    map_msg: impl Fn(ScrollMsg) -> Msg + 'static,
    child: Node<Msg>,
) -> Node<Msg> {
    let map_msg = Rc::new(map_msg);
    let mouse_handler = Rc::clone(&map_msg);
    let resize_handler = Rc::clone(&map_msg);
    let step = wheel_step.abs().max(1);
    let container_hash = hash::hash_str(0, id);
    let pending_request = state.take_pending_request(container_hash);

    info!("scoll offset {} {}", id, state.offset());

    let mut node = child
        .with_id(id)
        .with_overflow_y(Overflow::Scroll)
        .with_scroll(state.offset());

    node = node.on_mouse(move |event| {
        if event.buttons.vert_wheel {
            let delta = if event.buttons.wheel_positive {
                -step
            } else {
                step
            };
            Some(mouse_handler(ScrollMsg::Delta(delta)))
        } else {
            None
        }
    });

    node = node.on_resize(move |layout| {
        Some(resize_handler(ScrollMsg::Resize {
            viewport: Size {
                width: layout.size.width as u16,
                height: layout.size.height as u16,
            },
            content: Size {
                width: layout.content_size.width as u16,
                height: layout.content_size.height as u16,
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
