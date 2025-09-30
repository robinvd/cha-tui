use std::fmt::Write as _;

use termina::style::{ColorSpec, Intensity, RgbaColor};

use crate::error::ProgramError;

/// Cell attributes for styling terminal cells
#[derive(Clone, Debug, PartialEq)]
pub struct CellAttributes {
    pub foreground: ColorSpec,
    pub background: ColorSpec,
    pub intensity: Intensity,
}

impl CellAttributes {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn foreground(&self) -> ColorSpec {
        self.foreground
    }

    pub fn background(&self) -> ColorSpec {
        self.background
    }

    pub fn set_foreground(&mut self, color: ColorSpec) {
        self.foreground = color;
    }

    pub fn set_background(&mut self, color: ColorSpec) {
        self.background = color;
    }

    pub fn set_intensity(&mut self, intensity: Intensity) {
        self.intensity = intensity;
    }

    pub fn intensity(&self) -> Intensity {
        self.intensity
    }
}

impl Default for CellAttributes {
    fn default() -> Self {
        Self {
            foreground: ColorSpec::Reset,
            background: ColorSpec::Reset,
            intensity: Intensity::Normal,
        }
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

/// A rectangular area in the buffer
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rect {
    pub x: usize,
    pub y: usize,
    pub width: usize,
    pub height: usize,
}

impl Rect {
    pub fn new(x: usize, y: usize, width: usize, height: usize) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
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
            self.back[y][x] = Cell::new(ch, attrs.clone());
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
        let mut needs_attr_reset = false;

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
                            needs_attr_reset = true;
                        }
                        // Move cursor and write text
                        write!(buffer, "\x1b[{};{}H{}", y + 1, start_x + 1, run_text)?;
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
                            needs_attr_reset = true;
                        }
                        write!(buffer, "\x1b[{};{}H{}", y + 1, run_start.unwrap() + 1, run_text)?;
                        
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
                    needs_attr_reset = true;
                }
                write!(buffer, "\x1b[{};{}H{}", y + 1, start_x + 1, run_text)?;
            }
        }

        // Reset attributes if we changed them
        if needs_attr_reset {
            write!(buffer, "\x1b[0m")?;
        }

        // Send all changes to the terminal
        if !buffer.is_empty() {
            writer.write_all(buffer.as_bytes())?;
            writer.flush()?;
        }

        // Swap buffers - back becomes front
        std::mem::swap(&mut self.front, &mut self.back);

        Ok(())
    }

    fn write_sgr_attrs(buffer: &mut String, attrs: &CellAttributes) -> Result<(), ProgramError> {
        write!(buffer, "\x1b[")?;
        
        let mut first = true;

        // Write intensity
        match attrs.intensity {
            Intensity::Bold => {
                if !first {
                    write!(buffer, ";")?;
                }
                first = false;
                write!(buffer, "1")?;
            }
            Intensity::Dim => {
                if !first {
                    write!(buffer, ";")?;
                }
                first = false;
                write!(buffer, "2")?;
            }
            Intensity::Normal => {
                // Don't write anything for normal intensity
            }
        }

        // Write foreground
        if attrs.foreground != ColorSpec::Reset {
            if !first {
                write!(buffer, ";")?;
            }
            first = false;
            Self::write_color_spec(buffer, attrs.foreground, true)?;
        }

        // Write background
        if attrs.background != ColorSpec::Reset {
            if !first {
                write!(buffer, ";")?;
            }
            Self::write_color_spec(buffer, attrs.background, false)?;
        }

        write!(buffer, "m")?;
        Ok(())
    }

    fn write_color_spec(
        buffer: &mut String,
        color: ColorSpec,
        is_foreground: bool,
    ) -> Result<(), ProgramError> {
        match color {
            ColorSpec::Reset => {
                write!(buffer, "{}", if is_foreground { 39 } else { 49 })?;
            }
            ColorSpec::PaletteIndex(idx) => {
                let base = if is_foreground { 38 } else { 48 };
                write!(buffer, "{};5;{}", base, idx)?;
            }
            ColorSpec::TrueColor(RgbaColor {
                red,
                green,
                blue,
                alpha,
            }) => {
                let base = if is_foreground { 38 } else { 48 };
                if alpha == 255 {
                    write!(buffer, "{};2;{};{};{}", base, red, green, blue)?;
                } else {
                    write!(buffer, "{}:6::{}:{}:{}:{}", base, red, green, blue, alpha)?;
                }
            }
        }
        Ok(())
    }

    /// Get the back buffer for direct access (useful for testing)
    pub fn back_buffer(&self) -> &[Vec<Cell>] {
        &self.back
    }

    /// Convert the back buffer to a string for debugging/testing
    #[cfg(test)]
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
        blue_attrs.set_foreground(ColorSpec::PaletteIndex(4)); // Blue color
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
}
