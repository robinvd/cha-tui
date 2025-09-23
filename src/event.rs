#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Event {
    Key(Key),
    Resize(Size),
    Mouse(MouseEvent),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Key {
    pub code: KeyCode,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MouseEvent {
    pub x: u16,
    pub y: u16,
    pub buttons: MouseButtons,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct MouseButtons {
    pub left: bool,
    pub right: bool,
    pub middle: bool,
    pub horz_wheel: bool,
    pub vert_wheel: bool,
    pub wheel_positive: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeyCode {
    Char(char),
    Enter,
    Esc,
    Backspace,
    Left,
    Right,
    Up,
    Down,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Size {
    pub width: u16,
    pub height: u16,
}

impl Event {
    pub fn key(code: KeyCode) -> Self {
        Self::Key(Key::new(code))
    }

    pub fn resize(width: u16, height: u16) -> Self {
        Self::Resize(Size::new(width, height))
    }

    pub fn mouse(x: u16, y: u16, buttons: MouseButtons) -> Self {
        Self::Mouse(MouseEvent::new(x, y, buttons))
    }
}

impl Key {
    pub fn new(code: KeyCode) -> Self {
        Self {
            code,
            ctrl: false,
            alt: false,
            shift: false,
        }
    }

    pub fn with_modifiers(code: KeyCode, ctrl: bool, alt: bool, shift: bool) -> Self {
        Self {
            code,
            ctrl,
            alt,
            shift,
        }
    }
}

impl MouseEvent {
    pub fn new(x: u16, y: u16, buttons: MouseButtons) -> Self {
        Self {
            x,
            y,
            buttons,
            ctrl: false,
            alt: false,
            shift: false,
        }
    }

    pub fn with_modifiers(
        x: u16,
        y: u16,
        buttons: MouseButtons,
        ctrl: bool,
        alt: bool,
        shift: bool,
    ) -> Self {
        Self {
            x,
            y,
            buttons,
            ctrl,
            alt,
            shift,
        }
    }
}

impl MouseButtons {
    pub fn new(left: bool, right: bool, middle: bool) -> Self {
        Self {
            left,
            right,
            middle,
            horz_wheel: false,
            vert_wheel: false,
            wheel_positive: true,
        }
    }

    pub fn is_left_pressed(self) -> bool {
        self.left
    }

    pub fn is_right_pressed(self) -> bool {
        self.right
    }

    pub fn is_middle_pressed(self) -> bool {
        self.middle
    }
}

impl Size {
    pub fn new(width: u16, height: u16) -> Self {
        Self { width, height }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn creates_key_event_with_default_modifiers() {
        let event = Event::key(KeyCode::Enter);

        match event {
            Event::Key(key) => {
                assert_eq!(key.code, KeyCode::Enter);
                assert!(!key.ctrl);
                assert!(!key.alt);
                assert!(!key.shift);
            }
            other => panic!("expected key event, got {:?}", other),
        }
    }

    #[test]
    fn key_new_sets_modifiers_to_false() {
        let key = Key::new(KeyCode::Char('a'));

        assert_eq!(key.code, KeyCode::Char('a'));
        assert!(!key.ctrl);
        assert!(!key.alt);
        assert!(!key.shift);
    }

    #[test]
    fn size_new_assigns_dimensions() {
        let size = Size::new(40, 20);

        assert_eq!(size.width, 40);
        assert_eq!(size.height, 20);
    }

    #[test]
    fn resize_event_constructs_size() {
        let event = Event::resize(10, 5);

        match event {
            Event::Resize(size) => {
                assert_eq!(size.width, 10);
                assert_eq!(size.height, 5);
            }
            other => panic!("expected resize event, got {:?}", other),
        }
    }

    #[test]
    fn mouse_event_constructor_sets_defaults() {
        let buttons = MouseButtons::new(true, false, false);
        let event = Event::mouse(4, 7, buttons);

        match event {
            Event::Mouse(mouse) => {
                assert_eq!(mouse.x, 4);
                assert_eq!(mouse.y, 7);
                assert!(mouse.buttons.is_left_pressed());
                assert!(!mouse.ctrl);
                assert!(!mouse.alt);
                assert!(!mouse.shift);
            }
            other => panic!("expected mouse event, got {:?}", other),
        }
    }
}
