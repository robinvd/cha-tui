use std::fmt::Write as _;

use crate::error::ProgramError;
use crate::palette::Rgba;

pub use crate::geometry::Rect;

/// Cell attributes for styling terminal cells
#[derive(Clone, Debug, PartialEq, Default)]
pub struct CellAttributes {
    pub foreground: Option<Rgba>,
    pub background: Option<Rgba>,
    pub bold: bool,
    pub dim: bool,
    pub reverse: bool,
}

impl CellAttributes {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn foreground(&self) -> Option<Rgba> {
        self.foreground
    }

    pub fn background(&self) -> Option<Rgba> {
        self.background
    }

    pub fn set_foreground(&mut self, color: Rgba) {
        self.foreground = Some(color);
    }

    pub fn set_background(&mut self, color: Rgba) {
        self.background = Some(color);
    }

    pub fn set_bold(&mut self, bold: bool) {
        self.bold = bold;
    }

    pub fn set_dim(&mut self, dim: bool) {
        self.dim = dim;
    }

    pub fn set_reverse(&mut self, reverse: bool) {
        self.reverse = reverse;
    }

    pub fn is_bold(&self) -> bool {
        self.bold
    }

    pub fn is_dim(&self) -> bool {
        self.dim
    }

    pub fn is_reverse(&self) -> bool {
        self.reverse
    }
}

/// A cell in the buffer, representing a single character with its attributes
#[derive(Clone, Debug, PartialEq)]
pub struct Cell {
    pub ch: char,
    pub attrs: CellAttributes,
}

impl Cell {
    pub fn new(ch: char, attrs: CellAttributes) -> Self {
        Self { ch, attrs }
    }

    pub fn blank() -> Self {
        Self {
            ch: ' ',
            attrs: CellAttributes::default(),
        }
    }
}

/// Cursor render shape options supported by the terminal
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorShape {
    BlinkingBlock,
    SteadyBlock,
    BlinkingUnderline,
    SteadyUnderline,
    BlinkingBar,
    SteadyBar,
}

impl CursorShape {
    fn parameter(self) -> u8 {
        match self {
            Self::BlinkingBlock => 1,
            Self::SteadyBlock => 2,
            Self::BlinkingUnderline => 3,
            Self::SteadyUnderline => 4,
            Self::BlinkingBar => 5,
            Self::SteadyBar => 6,
        }
    }
}

impl Default for CursorShape {
    fn default() -> Self {
        Self::BlinkingBlock
    }
}

/// Cursor state tracked by the double buffer
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorState {
    Hidden,
    Position {
        x: usize,
        y: usize,
        shape: CursorShape,
    },
}

impl CursorState {
    pub fn hidden() -> Self {
        Self::Hidden
    }

    pub fn positioned(x: usize, y: usize, shape: CursorShape) -> Self {
        Self::Position { x, y, shape }
    }
}

impl Default for CursorState {
    fn default() -> Self {
        Self::Hidden
    }
}

/// Double-buffered in-memory buffer that renders diffs to the terminal
pub struct DoubleBuffer {
    width: usize,
    height: usize,
    /// The front buffer - what was last flushed to the terminal
    front: Vec<Vec<Cell>>,
    /// The current terminal state is unkown. Force a full rerender
    front_invalid: bool,
    /// The back buffer - what we're currently rendering to
    back: Vec<Vec<Cell>>,
    /// Cursor state last rendered
    front_cursor: CursorState,
    /// Desired cursor state for next flush
    back_cursor: CursorState,
}

impl DoubleBuffer {
    /// Create a new double buffer with the given dimensions
    pub fn new(width: usize, height: usize) -> Self {
        let blank_row = vec![Cell::blank(); width];
        let front = vec![blank_row.clone(); height];
        let back = vec![blank_row; height];

        Self {
            width,
            height,
            front,
            back,
            front_invalid: false,
            front_cursor: CursorState::default(),
            back_cursor: CursorState::default(),
        }
    }

    /// Get the current dimensions
    pub fn dimensions(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    /// Resize the buffer, clearing both buffers
    pub fn resize(&mut self, width: usize, height: usize) {
        if self.width == width && self.height == height {
            return;
        }

        self.width = width;
        self.height = height;

        let blank_row = vec![Cell::blank(); width];
        self.front = vec![blank_row.clone(); height];
        self.back = vec![blank_row; height];
        self.front_invalid = true;
        self.front_cursor = CursorState::Hidden;
        self.back_cursor = CursorState::Hidden;
    }

    /// Clear the back buffer to blank cells
    pub fn clear(&mut self) {
        for row in &mut self.back {
            for cell in row {
                *cell = Cell::blank();
            }
        }
    }

    /// Clear a specific rectangular area in the back buffer
    pub fn clear_area(&mut self, rect: Rect) {
        let end_y = (rect.y + rect.height).min(self.height);
        let end_x = (rect.x + rect.width).min(self.width);

        for y in rect.y..end_y {
            if y >= self.back.len() {
                break;
            }
            for x in rect.x..end_x {
                if x >= self.back[y].len() {
                    break;
                }
                self.back[y][x] = Cell::blank();
            }
        }
    }

    /// Write text at the given position in the back buffer
    pub fn write_text(&mut self, x: usize, y: usize, text: &str, attrs: &CellAttributes) {
        if y >= self.height {
            return;
        }

        let mut col = x;
        for ch in text.chars() {
            if col >= self.width {
                break;
            }
            self.back[y][col] = Cell::new(ch, attrs.clone());
            col += 1;
        }
    }

    /// Write a single character at the given position in the back buffer
    pub fn write_char(&mut self, x: usize, y: usize, ch: char, attrs: &CellAttributes) {
        if x < self.width && y < self.height {
            // Check if we need to blend colors due to transparency
            let mut final_attrs = attrs.clone();

            // Blend background color if it has transparency
            if let Some(new_bg) = attrs.background
                && new_bg.a < 255
            {
                // Get the existing cell's background color
                let existing_bg = self.back[y][x]
                    .attrs
                    .background
                    .unwrap_or(Rgba::opaque(0, 0, 0)); // Default to black if no background

                // Blend using oklab
                let bottom = existing_bg.to_straight_rgba();
                let top = new_bg.to_straight_rgba();
                let blended = bottom.oklab_blend(top);
                final_attrs.background = Some(Rgba::from_straight_rgba(blended));
            }

            // Blend foreground color if it has transparency
            if let Some(new_fg) = attrs.foreground
                && new_fg.a < 255
            {
                // Get the existing cell's foreground color
                let existing_fg = self.back[y][x]
                    .attrs
                    .foreground
                    .unwrap_or(Rgba::opaque(255, 255, 255)); // Default to white if no foreground

                // Blend using oklab
                let bottom = existing_fg.to_straight_rgba();
                let top = new_fg.to_straight_rgba();
                let blended = bottom.oklab_blend(top);
                final_attrs.foreground = Some(Rgba::from_straight_rgba(blended));
            }

            self.back[y][x] = Cell::new(ch, final_attrs);
        }
    }

    /// Set the attributes for a cell without changing its character
    pub fn set_attrs(&mut self, x: usize, y: usize, attrs: &CellAttributes) {
        if x < self.width && y < self.height {
            self.back[y][x].attrs = attrs.clone();
        }
    }

    /// Get a reference to a cell in the back buffer
    pub fn get_cell(&self, x: usize, y: usize) -> Option<&Cell> {
        self.back.get(y).and_then(|row| row.get(x))
    }

    /// Get a mutable reference to a cell in the back buffer
    pub fn get_cell_mut(&mut self, x: usize, y: usize) -> Option<&mut Cell> {
        self.back.get_mut(y).and_then(|row| row.get_mut(x))
    }

    /// Set the desired cursor state for the next flush
    pub fn set_cursor_state(&mut self, state: CursorState) {
        self.back_cursor = match state {
            CursorState::Position { x, y, shape } => {
                if self.width == 0 || self.height == 0 {
                    CursorState::Hidden
                } else {
                    let clamped_x = x.min(self.width.saturating_sub(1));
                    let clamped_y = y.min(self.height.saturating_sub(1));
                    CursorState::Position {
                        x: clamped_x,
                        y: clamped_y,
                        shape,
                    }
                }
            }
            CursorState::Hidden => CursorState::Hidden,
        };
    }

    /// Convenience helper to configure a visible cursor
    pub fn set_cursor(&mut self, x: usize, y: usize, shape: CursorShape) {
        self.set_cursor_state(CursorState::Position { x, y, shape });
    }

    /// Hide the cursor on next flush
    pub fn clear_cursor(&mut self) {
        self.set_cursor_state(CursorState::Hidden);
    }

    /// Inspect the desired cursor state (exposed for testing)
    pub fn cursor_state(&self) -> CursorState {
        self.back_cursor
    }

    /// Fill a rectangular area with the given character and attributes
    pub fn fill_rect(&mut self, rect: Rect, ch: char, attrs: &CellAttributes) {
        let end_y = (rect.y + rect.height).min(self.height);
        let end_x = (rect.x + rect.width).min(self.width);

        for y in rect.y..end_y {
            for x in rect.x..end_x {
                self.back[y][x] = Cell::new(ch, attrs.clone());
            }
        }
    }

    /// Compute the diff between front and back buffers and render to terminal
    pub fn flush<W: std::io::Write>(&mut self, writer: &mut W) -> Result<(), ProgramError> {
        let mut buffer = String::new();
        let mut current_attrs = CellAttributes::default();
        let mut wrote_anything = false;
        let cursor_changed = self.front_invalid || self.front_cursor != self.back_cursor;

        for y in 0..self.height {
            let mut run_start = None;
            let mut run_text = String::new();
            let mut run_attrs = CellAttributes::default();

            for x in 0..self.width {
                let back_cell = &self.back[y][x];
                let front_cell = &self.front[y][x];

                // Skip if the cell hasn't changed
                if back_cell == front_cell && !self.front_invalid {
                    // Flush any pending run
                    if let Some(start_x) = run_start {
                        if current_attrs != run_attrs {
                            Self::write_sgr_attrs(&mut buffer, &run_attrs)?;
                            current_attrs = run_attrs.clone();
                        }
                        // Move cursor and write text
                        write!(buffer, "\x1b[{};{}H{}", y + 1, start_x + 1, run_text)?;
                        wrote_anything = true;
                        run_start = None;
                        run_text.clear();
                    }
                    continue;
                }

                // Start a new run or continue the current one
                if run_start.is_some() {
                    // Check if we can continue this run (same attributes)
                    if back_cell.attrs != run_attrs {
                        // Flush the current run
                        if current_attrs != run_attrs {
                            Self::write_sgr_attrs(&mut buffer, &run_attrs)?;
                            current_attrs = run_attrs.clone();
                        }
                        write!(
                            buffer,
                            "\x1b[{};{}H{}",
                            y + 1,
                            run_start.unwrap() + 1,
                            run_text
                        )?;
                        wrote_anything = true;

                        // Start a new run
                        run_start = Some(x);
                        run_text = String::from(back_cell.ch);
                        run_attrs = back_cell.attrs.clone();
                    } else {
                        // Continue the current run
                        run_text.push(back_cell.ch);
                    }
                } else {
                    // Start a new run
                    run_start = Some(x);
                    run_text = String::from(back_cell.ch);
                    run_attrs = back_cell.attrs.clone();
                }
            }

            // Flush any pending run at the end of the line
            if let Some(start_x) = run_start {
                if current_attrs != run_attrs {
                    Self::write_sgr_attrs(&mut buffer, &run_attrs)?;
                    current_attrs = run_attrs.clone();
                }
                write!(buffer, "\x1b[{};{}H{}", y + 1, start_x + 1, run_text)?;
                wrote_anything = true;
            }
        }

        // Reset attributes at the end to clean terminal state
        // Only if we wrote something
        if wrote_anything {
            write!(buffer, "\x1b[0m")?;
        }

        if cursor_changed {
            match self.back_cursor {
                CursorState::Hidden => {
                    buffer.push_str("\x1b[?25l");
                }
                CursorState::Position { x, y, shape } => {
                    write!(buffer, "\x1b[{} q", shape.parameter())?;
                    buffer.push_str("\x1b[?25h");
                    write!(buffer, "\x1b[{};{}H", y + 1, x + 1)?;
                }
            }
        }

        // Send all changes to the terminal
        if !buffer.is_empty() {
            writer.write_all(buffer.as_bytes())?;
            writer.flush()?;
        }

        // Swap buffers - back becomes front
        std::mem::swap(&mut self.front, &mut self.back);
        self.front_cursor = self.back_cursor;
        self.front_invalid = false;

        Ok(())
    }

    fn write_sgr_attrs(buffer: &mut String, attrs: &CellAttributes) -> Result<(), ProgramError> {
        // Check if we need to write anything at all
        let has_any_attrs = attrs.bold
            || attrs.dim
            || attrs.reverse
            || attrs.foreground.is_some()
            || attrs.background.is_some();

        if !has_any_attrs {
            // Just reset to defaults
            write!(buffer, "\x1b[0m")?;
            return Ok(());
        }

        // Always start with a reset to ensure clean state when transitioning
        // This ensures that if we had a background before and don't now, it gets cleared
        write!(buffer, "\x1b[0")?;

        // Write bold/dim
        if attrs.bold {
            write!(buffer, ";1")?;
        } else if attrs.dim {
            write!(buffer, ";2")?;
        }

        // Write reverse
        if attrs.reverse {
            write!(buffer, ";7")?;
        }

        // Write foreground
        if let Some(fg) = attrs.foreground {
            write!(buffer, ";")?;
            Self::write_rgba_color(buffer, fg, true)?;
        }

        // Write background
        if let Some(bg) = attrs.background {
            write!(buffer, ";")?;
            Self::write_rgba_color(buffer, bg, false)?;
        }

        write!(buffer, "m")?;
        Ok(())
    }

    fn write_rgba_color(
        buffer: &mut String,
        color: Rgba,
        is_foreground: bool,
    ) -> Result<(), ProgramError> {
        let base = if is_foreground { 38 } else { 48 };
        if color.a == 255 {
            write!(buffer, "{};2;{};{};{}", base, color.r, color.g, color.b)?;
        } else {
            // Use colon-separated format for alpha channel (less widely supported)
            write!(
                buffer,
                "{}:2::{}:{}:{}:{}",
                base, color.r, color.g, color.b, color.a
            )?;
        }
        Ok(())
    }

    /// Get the back buffer for direct access (useful for testing)
    pub fn back_buffer(&self) -> &[Vec<Cell>] {
        &self.back
    }

    /// Convert the back buffer to a string for debugging/testing
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        for (i, row) in self.back.iter().enumerate() {
            if i > 0 {
                result.push('\n');
            }
            for cell in row {
                result.push(cell.ch);
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_buffer_is_blank() {
        let buffer = DoubleBuffer::new(10, 5);
        assert_eq!(buffer.dimensions(), (10, 5));

        let text = buffer.to_string();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 5);
        assert_eq!(lines[0].len(), 10);
        assert!(lines[0].chars().all(|c| c == ' '));
    }

    #[test]
    fn write_text_renders_to_back_buffer() {
        let mut buffer = DoubleBuffer::new(20, 3);
        let attrs = CellAttributes::default();

        buffer.write_text(0, 0, "hello", &attrs);
        buffer.write_text(0, 1, "world", &attrs);

        let text = buffer.to_string();
        let lines: Vec<&str> = text.lines().collect();
        assert!(lines[0].starts_with("hello"));
        assert!(lines[1].starts_with("world"));
    }

    #[test]
    fn write_char_sets_single_character() {
        let mut buffer = DoubleBuffer::new(10, 3);
        let attrs = CellAttributes::default();

        buffer.write_char(5, 1, 'X', &attrs);

        let cell = buffer.get_cell(5, 1).unwrap();
        assert_eq!(cell.ch, 'X');
    }

    #[test]
    fn clear_resets_back_buffer() {
        let mut buffer = DoubleBuffer::new(10, 3);
        let attrs = CellAttributes::default();

        buffer.write_text(0, 0, "test", &attrs);
        buffer.clear();

        let text = buffer.to_string();
        assert!(text.chars().all(|c| c == ' ' || c == '\n'));
    }

    #[test]
    fn clear_area_clears_rect() {
        let mut buffer = DoubleBuffer::new(10, 5);
        let attrs = CellAttributes::default();

        buffer.write_text(0, 0, "0123456789", &attrs);
        buffer.write_text(0, 1, "abcdefghij", &attrs);
        buffer.write_text(0, 2, "ABCDEFGHIJ", &attrs);

        buffer.clear_area(Rect::new(2, 1, 5, 1));

        let text = buffer.to_string();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "0123456789");
        assert_eq!(lines[1], "ab     hij");
        assert_eq!(lines[2], "ABCDEFGHIJ");
    }

    #[test]
    fn fill_rect_fills_area() {
        let mut buffer = DoubleBuffer::new(10, 5);
        let attrs = CellAttributes::default();

        buffer.fill_rect(Rect::new(2, 1, 4, 2), '#', &attrs);

        let text = buffer.to_string();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[1], "  ####    ");
        assert_eq!(lines[2], "  ####    ");
    }

    #[test]
    fn resize_changes_dimensions() {
        let mut buffer = DoubleBuffer::new(10, 5);
        buffer.resize(20, 10);

        assert_eq!(buffer.dimensions(), (20, 10));

        let text = buffer.to_string();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 10);
        assert_eq!(lines[0].len(), 20);
    }

    #[test]
    fn write_text_clips_at_boundary() {
        let mut buffer = DoubleBuffer::new(5, 3);
        let attrs = CellAttributes::default();

        buffer.write_text(3, 0, "testing", &attrs);

        let text = buffer.to_string();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "   te");
    }

    #[test]
    fn set_attrs_preserves_character() {
        let mut buffer = DoubleBuffer::new(10, 3);
        let default_attrs = CellAttributes::default();

        buffer.write_char(5, 1, 'X', &default_attrs);

        let mut blue_attrs = CellAttributes::default();
        blue_attrs.set_foreground(Rgba::opaque(0, 0, 255)); // Blue color
        buffer.set_attrs(5, 1, &blue_attrs);

        let cell = buffer.get_cell(5, 1).unwrap();
        assert_eq!(cell.ch, 'X');
        assert_eq!(cell.attrs.foreground(), blue_attrs.foreground());
    }

    #[test]
    fn get_cell_mut_allows_modification() {
        let mut buffer = DoubleBuffer::new(10, 3);

        if let Some(cell) = buffer.get_cell_mut(2, 1) {
            cell.ch = 'Z';
        }

        assert_eq!(buffer.get_cell(2, 1).unwrap().ch, 'Z');
    }

    #[test]
    fn write_char_blends_transparent_background() {
        let mut buffer = DoubleBuffer::new(10, 3);

        // First, write a cell with a red background
        let mut attrs1 = CellAttributes::default();
        attrs1.set_background(Rgba::opaque(255, 0, 0)); // Red
        buffer.write_char(5, 1, 'X', &attrs1);

        // Now write over it with a semi-transparent blue background
        let mut attrs2 = CellAttributes::default();
        attrs2.set_background(Rgba::new(0, 0, 255, 128)); // 50% transparent blue
        buffer.write_char(5, 1, 'Y', &attrs2);

        // The resulting background should be a blend of red and blue
        let cell = buffer.get_cell(5, 1).unwrap();
        assert_eq!(cell.ch, 'Y');
        let bg = cell.attrs.background().expect("Background should be set");

        // The blended color should not be pure red or pure blue
        assert_ne!(bg, Rgba::opaque(255, 0, 0), "Should not be pure red");
        assert_ne!(bg, Rgba::opaque(0, 0, 255), "Should not be pure blue");
        // It should have some red and some blue component
        assert!(bg.r > 0, "Should have some red component");
        assert!(bg.b > 0, "Should have some blue component");
    }

    #[test]
    fn write_char_preserves_opaque_colors() {
        let mut buffer = DoubleBuffer::new(10, 3);

        // Write with opaque color (should not blend)
        let mut attrs = CellAttributes::default();
        attrs.set_background(Rgba::opaque(100, 150, 200));
        buffer.write_char(3, 1, 'A', &attrs);

        let cell = buffer.get_cell(3, 1).unwrap();
        assert_eq!(cell.ch, 'A');
        assert_eq!(cell.attrs.background(), Some(Rgba::opaque(100, 150, 200)));
    }

    #[test]
    fn flush_generates_color_codes() {
        let mut buffer = DoubleBuffer::new(10, 3);

        // Write with specific colors
        let mut attrs = CellAttributes::default();
        attrs.set_foreground(Rgba::opaque(255, 0, 0)); // Red
        attrs.set_background(Rgba::opaque(0, 255, 0)); // Green
        buffer.write_text(0, 0, "test", &attrs);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("output should be valid UTF-8");

        // Should contain SGR codes for colors
        assert!(
            output_str.contains("\x1b["),
            "Should contain SGR escape sequence"
        );
        // Should contain 38;2 for foreground RGB
        assert!(
            output_str.contains("38;2;255;0;0"),
            "Should contain red foreground"
        );
        // Should contain 48;2 for background RGB
        assert!(
            output_str.contains("48;2;0;255;0"),
            "Should contain green background"
        );
        // Should contain the text
        assert!(output_str.contains("test"), "Should contain the text");
    }

    #[test]
    fn flush_handles_color_transitions() {
        let mut buffer = DoubleBuffer::new(10, 2);

        // First cell: red foreground
        let mut red_attrs = CellAttributes::default();
        red_attrs.set_foreground(Rgba::opaque(255, 0, 0));
        buffer.write_char(0, 0, 'R', &red_attrs);

        // Second cell: blue foreground
        let mut blue_attrs = CellAttributes::default();
        blue_attrs.set_foreground(Rgba::opaque(0, 0, 255));
        buffer.write_char(1, 0, 'B', &blue_attrs);

        // Third cell: no color (default)
        let default_attrs = CellAttributes::default();
        buffer.write_char(2, 0, 'D', &default_attrs);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("output should be valid UTF-8");

        // Should handle multiple color changes
        assert!(output_str.contains("38;2;255;0;0"), "Should have red");
        assert!(output_str.contains("38;2;0;0;255"), "Should have blue");
    }

    #[test]
    fn flush_resets_attrs_at_end() {
        let mut buffer = DoubleBuffer::new(10, 2);

        let mut attrs = CellAttributes::default();
        attrs.set_foreground(Rgba::opaque(255, 0, 0));
        buffer.write_text(0, 0, "test", &attrs);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("output should be valid UTF-8");

        // Should reset attributes at the end
        assert!(
            output_str.contains("\x1b[0m"),
            "Should contain reset code at end"
        );
    }

    #[test]
    fn flush_only_updates_changed_cells() {
        let mut buffer = DoubleBuffer::new(10, 2);

        let attrs = CellAttributes::default();
        buffer.write_text(0, 0, "hello", &attrs);

        // First flush - will output everything
        let mut output1 = Vec::new();
        buffer
            .flush(&mut output1)
            .expect("first flush should succeed");
        assert!(!output1.is_empty(), "First flush should produce output");

        // Write the same content to back buffer again
        buffer.write_text(0, 0, "hello", &attrs);

        // Second flush - should produce no output because content hasn't changed
        let mut output2 = Vec::new();
        buffer
            .flush(&mut output2)
            .expect("second flush should succeed");

        assert!(
            output2.is_empty(),
            "Second flush should produce no output when content is identical"
        );
    }

    #[test]
    fn flush_handles_bold_and_dim() {
        let mut buffer = DoubleBuffer::new(10, 2);

        // Bold text
        let mut bold_attrs = CellAttributes::default();
        bold_attrs.set_bold(true);
        buffer.write_char(0, 0, 'B', &bold_attrs);

        // Dim text
        let mut dim_attrs = CellAttributes::default();
        dim_attrs.set_dim(true);
        buffer.write_char(1, 0, 'D', &dim_attrs);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("output should be valid UTF-8");

        // Should contain SGR codes with reset followed by bold/dim
        // Format is: ESC[0;1m for bold, ESC[0;2m for dim
        assert!(
            output_str.contains(";1m") || output_str.contains(";1;"),
            "Should contain bold code (1)"
        );
        assert!(
            output_str.contains(";2m") || output_str.contains(";2;"),
            "Should contain dim code (2)"
        );
    }

    #[test]
    fn flush_handles_reverse() {
        let mut buffer = DoubleBuffer::new(10, 2);

        // Reverse text
        let mut reverse_attrs = CellAttributes::default();
        reverse_attrs.set_reverse(true);
        buffer.write_char(0, 0, 'R', &reverse_attrs);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("output should be valid UTF-8");

        // Should contain SGR code 7 for reverse
        // Format is: ESC[0;7m
        assert!(
            output_str.contains(";7m") || output_str.contains(";7;"),
            "Should contain reverse code (7), got: {}",
            output_str
        );
    }

    #[test]
    fn flush_correctly_resets_partial_attributes() {
        let mut buffer = DoubleBuffer::new(10, 2);

        // First cell: both foreground and background
        let mut full_attrs = CellAttributes::default();
        full_attrs.set_foreground(Rgba::opaque(255, 0, 0)); // Red
        full_attrs.set_background(Rgba::opaque(0, 255, 0)); // Green
        buffer.write_char(0, 0, 'A', &full_attrs);

        // Second cell: only foreground (background should be reset)
        let mut partial_attrs = CellAttributes::default();
        partial_attrs.set_foreground(Rgba::opaque(0, 0, 255)); // Blue
        // No background
        buffer.write_char(1, 0, 'B', &partial_attrs);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("output should be valid UTF-8");

        // First cell should have both foreground and background
        assert!(
            output_str.contains("38;2;255;0;0"),
            "Should set red foreground"
        );
        assert!(
            output_str.contains("48;2;0;255;0"),
            "Should set green background"
        );

        // Second cell should have blue foreground
        assert!(
            output_str.contains("38;2;0;0;255"),
            "Should set blue foreground"
        );

        // The key test: verify that the second SGR sequence starts with a reset (0)
        // This ensures the green background is cleared
        // Format should be: ESC[0;38;2;0;0;255m (reset + blue foreground)
        assert!(
            output_str.contains("\x1b[0;38;2;0;0;255m"),
            "Second SGR should reset (0) before setting blue foreground"
        );
    }

    #[test]
    fn flush_handles_transition_from_colored_to_default() {
        let mut buffer = DoubleBuffer::new(10, 2);

        // First cell: colored
        let mut colored_attrs = CellAttributes::default();
        colored_attrs.set_foreground(Rgba::opaque(255, 0, 0));
        buffer.write_char(0, 0, 'C', &colored_attrs);

        // Second cell: default (no colors)
        let default_attrs = CellAttributes::default();
        buffer.write_char(1, 0, 'D', &default_attrs);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("output should be valid UTF-8");

        // Should have colored attributes for first cell
        assert!(
            output_str.contains("38;2;255;0;0"),
            "Should set red foreground"
        );

        // Should have a reset for second cell (ESC[0m)
        // Since default attrs just outputs ESC[0m
        let positions: Vec<_> = output_str.match_indices("\x1b[0m").collect();
        assert!(
            !positions.is_empty(),
            "Should have at least one reset sequence"
        );
    }

    #[test]
    fn flush_emits_cursor_sequences_when_position_changes() {
        let mut buffer = DoubleBuffer::new(4, 2);
        buffer.set_cursor(1, 1, CursorShape::SteadyUnderline);

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("cursor output must be valid UTF-8");

        assert!(
            output_str.contains("\x1b[4 q"),
            "Expected steady underline cursor shape sequence"
        );
        assert!(
            output_str.contains("\x1b[?25h"),
            "Expected cursor to be made visible"
        );
        assert!(
            output_str.contains("\x1b[2;2H"),
            "Expected cursor to move to requested position"
        );
    }

    #[test]
    fn flush_hides_cursor_when_cleared() {
        let mut buffer = DoubleBuffer::new(3, 1);
        buffer.set_cursor(0, 0, CursorShape::BlinkingBlock);
        buffer
            .flush(&mut Vec::new())
            .expect("initial flush should succeed");

        buffer.clear_cursor();

        let mut output = Vec::new();
        buffer.flush(&mut output).expect("flush should succeed");
        let output_str = String::from_utf8(output).expect("cursor hide output must be valid UTF-8");

        assert!(
            output_str.contains("\x1b[?25l"),
            "Expected cursor hide sequence to be emitted"
        );
    }

    #[test]
    fn set_cursor_clamps_to_buffer_bounds() {
        let mut buffer = DoubleBuffer::new(2, 2);
        buffer.set_cursor(10, 5, CursorShape::BlinkingBar);

        match buffer.cursor_state() {
            CursorState::Position { x, y, .. } => {
                assert_eq!(x, 1);
                assert_eq!(y, 1);
            }
            other => panic!("Expected positioned cursor, got {:?}", other),
        }
    }
}
