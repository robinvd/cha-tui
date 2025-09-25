use std::rc::Rc;

use crate::{dom::Node, event::Size};
use taffy::Overflow;

#[derive(Clone, Copy, Debug, Default)]
pub struct ScrollState {
    offset: f32,
    viewport: Size,
    content: Size,
}

#[derive(Clone, Copy, Debug)]
pub enum ScrollMsg {
    Delta(i32),
    Resize { viewport: Size, content: Size },
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
        }
    }

    pub fn reset(&mut self) {
        self.offset = 0.0;
        self.clamp_offset();
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

    child
        .with_id(id)
        .with_overflow_y(Overflow::Scroll)
        .with_scroll(state.offset())
        .on_mouse(move |event| {
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
        })
        .on_resize(move |layout| {
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
        })
}
