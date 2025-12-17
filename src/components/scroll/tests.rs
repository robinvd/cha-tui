use super::*;
use crate::{column, text};
use std::time::{Duration, Instant};
use taffy::{
    prelude::{AvailableSpace, TaffyZero},
    style::Dimension,
};

fn scroll_state_with_content() -> ScrollState {
    let mut state = ScrollState::both();
    state.update(ScrollMsg::Resize {
        viewport: Size {
            width: 80,
            height: 20,
        },
        content: Size {
            width: 200,
            height: 200,
        },
    });
    state
}

#[test]
fn axis_lock_blocks_perpendicular_scroll_jitter() {
    let mut state = scroll_state_with_content();

    let base = Instant::now();
    state.apply_delta_at(ScrollAxis::Vertical, 5, base);
    assert_eq!(state.offset_y(), 5.0);
    assert_eq!(state.offset_x(), 0.0);

    state.apply_delta_at(ScrollAxis::Horizontal, 10, base + Duration::from_millis(50));

    assert_eq!(state.offset_y(), 5.0);
    assert_eq!(state.offset_x(), 0.0);
}

#[test]
fn axis_lock_expires_to_allow_axis_change() {
    let mut state = scroll_state_with_content();

    let base = Instant::now();
    state.apply_delta_at(ScrollAxis::Vertical, 5, base);
    assert_eq!(state.offset_y(), 5.0);

    state.apply_delta_at(
        ScrollAxis::Horizontal,
        7,
        base + AXIS_LOCK_DURATION + Duration::from_millis(1),
    );

    assert_eq!(state.offset_x(), 7.0);
    assert_eq!(state.offset_y(), 5.0);
}

#[test]
fn horizontal_gesture_blocks_vertical_until_expired() {
    let mut state = scroll_state_with_content();

    let base = Instant::now();
    state.apply_delta_at(ScrollAxis::Horizontal, 12, base);
    assert_eq!(state.offset_x(), 12.0);
    assert_eq!(state.offset_y(), 0.0);

    state.apply_delta_at(ScrollAxis::Vertical, 8, base + Duration::from_millis(50));

    assert_eq!(state.offset_x(), 12.0);
    assert_eq!(state.offset_y(), 0.0);

    state.apply_delta_at(
        ScrollAxis::Vertical,
        6,
        base + AXIS_LOCK_DURATION + Duration::from_millis(1),
    );

    assert_eq!(state.offset_x(), 12.0);
    assert_eq!(state.offset_y(), 6.0);
}

#[test]
fn reports_when_at_end_of_axis() {
    let mut state = scroll_state_with_content();
    assert!(!state.is_at_end(ScrollAxis::Vertical));

    state.update(ScrollMsg::AxisJumpTo {
        axis: ScrollAxis::Vertical,
        offset: 180.0,
    });
    assert!(state.is_at_end(ScrollAxis::Vertical));

    state.update(ScrollMsg::AxisJumpTo {
        axis: ScrollAxis::Vertical,
        offset: 179.6,
    });
    assert!(state.is_at_end(ScrollAxis::Vertical));

    state.update(ScrollMsg::AxisJumpTo {
        axis: ScrollAxis::Vertical,
        offset: 178.0,
    });
    assert!(!state.is_at_end(ScrollAxis::Vertical));
}

#[test]
fn scrollable_content_reports_overflow_height_with_flex_end() {
    let scroll_state = ScrollState::new(ScrollBehavior::Vertical);
    let messages = (0..10)
        .map(|idx| text::<ScrollMsg>(&format!("line {idx}")))
        .collect();
    let spacer = column(vec![])
        .with_flex_grow(1.)
        .with_flex_shrink(1.)
        .with_min_height(Dimension::ZERO);
    let mut node = scrollable_content(
        "chat",
        &scroll_state,
        1,
        |msg| msg,
        column(vec![
            spacer,
            column(messages)
                .with_min_height(Dimension::ZERO)
                .with_flex_shrink(0.),
        ])
        .with_min_height(Dimension::ZERO),
    )
    .with_fill();

    taffy::compute_root_layout(
        &mut node,
        u64::MAX.into(),
        taffy::Size {
            width: AvailableSpace::Definite(20.0),
            height: AvailableSpace::Definite(5.0),
        },
    );
    crate::dom::rounding::round_layout(&mut node);

    let layout = node.layout_state().layout;
    let element = node
        .as_element()
        .expect("scrollable_content returns an element node");
    let total_child_height: f32 = element
        .children
        .iter()
        .map(|child| child.layout_state().layout.size.height)
        .sum();

    assert_eq!(layout.size.height, 5.0);
    assert!(
        total_child_height > layout.size.height,
        "children height {} <= viewport {}",
        total_child_height,
        layout.size.height
    );
    assert!(
        layout.content_size.height > layout.size.height,
        "content height {} <= viewport {} (children {})",
        layout.content_size.height,
        layout.size.height,
        total_child_height
    );
}
