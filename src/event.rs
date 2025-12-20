#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Event {
    Key(Key),
    Resize(Size),
    Mouse(MouseEvent),
    FocusGained,
    FocusLost,
}

/// The kind of key event (press, release, or repeat).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum KeyEventKind {
    #[default]
    Press,
    Release,
    Repeat,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Key {
    pub code: KeyCode,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub super_key: bool,
    pub hyper: bool,
    pub meta: bool,
    /// The kind of key event (press, release, repeat).
    pub kind: KeyEventKind,
    /// Whether this key originated from the numpad.
    pub keypad: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MouseEvent {
    pub x: u16,
    pub y: u16,
    pub local_x: u16,
    pub local_y: u16,
    pub buttons: MouseButtons,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub super_key: bool,
    pub click_count: u8,
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
    PageUp,
    PageDown,
    Home,
    End,
    Tab,
    Function(u8),
    /// Insert key
    Insert,
    /// Delete key
    Delete,
    /// Caps Lock key
    CapsLock,
    /// Scroll Lock key
    ScrollLock,
    /// Num Lock key
    NumLock,
    /// Print Screen key
    PrintScreen,
    /// Pause key
    Pause,
    /// Context Menu key
    Menu,
    /// A modifier key (Shift, Ctrl, Alt, etc.)
    Modifier(ModifierKeyCode),
    /// A media key
    Media(MediaKeyCode),
}

/// Modifier key codes for the kitty keyboard protocol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModifierKeyCode {
    LeftShift,
    LeftControl,
    LeftAlt,
    LeftSuper,
    LeftHyper,
    LeftMeta,
    RightShift,
    RightControl,
    RightAlt,
    RightSuper,
    RightHyper,
    RightMeta,
}

/// Media key codes for the kitty keyboard protocol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MediaKeyCode {
    Play,
    Pause,
    PlayPause,
    Stop,
    FastForward,
    Rewind,
    TrackNext,
    TrackPrevious,
    Record,
    LowerVolume,
    RaiseVolume,
    MuteVolume,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
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

    pub fn focus_gained() -> Self {
        Self::FocusGained
    }

    pub fn focus_lost() -> Self {
        Self::FocusLost
    }
}

impl Key {
    pub fn new(code: KeyCode) -> Self {
        Self {
            code,
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            hyper: false,
            meta: false,
            kind: KeyEventKind::Press,
            keypad: false,
        }
    }

    pub fn with_modifiers(
        code: KeyCode,
        ctrl: bool,
        alt: bool,
        shift: bool,
        super_key: bool,
    ) -> Self {
        Self {
            code,
            ctrl,
            alt,
            shift,
            super_key,
            hyper: false,
            meta: false,
            kind: KeyEventKind::Press,
            keypad: false,
        }
    }

    pub fn with_all_modifiers(
        code: KeyCode,
        ctrl: bool,
        alt: bool,
        shift: bool,
        super_key: bool,
        hyper: bool,
        meta: bool,
    ) -> Self {
        Self {
            code,
            ctrl,
            alt,
            shift,
            super_key,
            hyper,
            meta,
            kind: KeyEventKind::Press,
            keypad: false,
        }
    }

    /// Create a key with full kitty keyboard protocol support.
    pub fn with_kitty_extras(
        code: KeyCode,
        ctrl: bool,
        alt: bool,
        shift: bool,
        super_key: bool,
        hyper: bool,
        meta: bool,
        kind: KeyEventKind,
        keypad: bool,
    ) -> Self {
        Self {
            code,
            ctrl,
            alt,
            shift,
            super_key,
            hyper,
            meta,
            kind,
            keypad,
        }
    }
}

impl MouseEvent {
    pub fn new(x: u16, y: u16, buttons: MouseButtons) -> Self {
        Self {
            x,
            y,
            local_x: x,
            local_y: y,
            buttons,
            ctrl: false,
            alt: false,
            shift: false,
            super_key: false,
            click_count: 0,
        }
    }

    pub fn with_modifiers(
        x: u16,
        y: u16,
        buttons: MouseButtons,
        ctrl: bool,
        alt: bool,
        shift: bool,
        super_key: bool,
    ) -> Self {
        Self {
            x,
            y,
            local_x: x,
            local_y: y,
            buttons,
            ctrl,
            alt,
            shift,
            super_key,
            click_count: 0,
        }
    }

    pub fn is_single_click(self) -> bool {
        self.click_count == 1
    }

    pub fn is_double_click(self) -> bool {
        self.click_count == 2
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
                assert!(!key.hyper);
                assert!(!key.meta);
                assert_eq!(key.kind, KeyEventKind::Press);
                assert!(!key.keypad);
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
        assert!(!key.hyper);
        assert!(!key.meta);
        assert_eq!(key.kind, KeyEventKind::Press);
        assert!(!key.keypad);
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

    #[test]
    fn focus_event_constructor_returns_variant() {
        match Event::focus_gained() {
            Event::FocusGained => {}
            other => panic!("expected focus gained event, got {:?}", other),
        }
    }

    #[test]
    fn key_with_kitty_extras_sets_all_fields() {
        let key = Key::with_kitty_extras(
            KeyCode::Char('a'),
            true,  // ctrl
            true,  // alt
            true,  // shift
            false, // super
            false, // hyper
            false, // meta
            KeyEventKind::Release,
            true, // keypad
        );

        assert_eq!(key.code, KeyCode::Char('a'));
        assert!(key.ctrl);
        assert!(key.alt);
        assert!(key.shift);
        assert!(!key.super_key);
        assert!(!key.hyper);
        assert!(!key.meta);
        assert_eq!(key.kind, KeyEventKind::Release);
        assert!(key.keypad);
    }

    #[test]
    fn key_event_kind_default_is_press() {
        assert_eq!(KeyEventKind::default(), KeyEventKind::Press);
    }

    #[test]
    fn modifier_key_code_variants_exist() {
        // Just verify the variants exist and can be used
        let _left_shift = KeyCode::Modifier(ModifierKeyCode::LeftShift);
        let _right_ctrl = KeyCode::Modifier(ModifierKeyCode::RightControl);
        let _left_alt = KeyCode::Modifier(ModifierKeyCode::LeftAlt);
        let _left_super = KeyCode::Modifier(ModifierKeyCode::LeftSuper);
    }

    #[test]
    fn media_key_code_variants_exist() {
        // Just verify the variants exist and can be used
        let _play = KeyCode::Media(MediaKeyCode::Play);
        let _pause = KeyCode::Media(MediaKeyCode::Pause);
        let _next = KeyCode::Media(MediaKeyCode::TrackNext);
        let _mute = KeyCode::Media(MediaKeyCode::MuteVolume);
    }

    #[test]
    fn new_key_codes_exist() {
        // Verify new key codes can be created
        let _insert = KeyCode::Insert;
        let _delete = KeyCode::Delete;
        let _caps = KeyCode::CapsLock;
        let _scroll = KeyCode::ScrollLock;
        let _num = KeyCode::NumLock;
        let _print = KeyCode::PrintScreen;
        let _pause = KeyCode::Pause;
        let _menu = KeyCode::Menu;
    }
}
