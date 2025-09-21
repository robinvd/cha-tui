#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Event {
    Key(Key),
    Resize(Size),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Key {
    pub code: KeyCode,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
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
        Self { code, ctrl, alt, shift }
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
}
