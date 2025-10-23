use super::*;
use std::time::{Duration, Instant};

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
