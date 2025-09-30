use std::io::{Read, Write};
use std::time::Duration;

use crate::error::ProgramError;

/// RGBA color representation
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rgba {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Rgba {
    pub const fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    pub const fn opaque(r: u8, g: u8, b: u8) -> Self {
        Self::new(r, g, b, 255)
    }
}

/// Terminal color palette containing the 16 ANSI colors and default fg/bg
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Palette {
    /// ANSI colors 0-15
    pub colors: [Rgba; 16],
    /// Default foreground color
    pub foreground: Rgba,
    /// Default background color
    pub background: Rgba,
}

impl Default for Palette {
    fn default() -> Self {
        Self {
            // Standard xterm colors
            colors: [
                Rgba::opaque(0, 0, 0),       // 0: Black
                Rgba::opaque(205, 0, 0),     // 1: Red
                Rgba::opaque(0, 205, 0),     // 2: Green
                Rgba::opaque(205, 205, 0),   // 3: Yellow
                Rgba::opaque(0, 0, 238),     // 4: Blue
                Rgba::opaque(205, 0, 205),   // 5: Magenta
                Rgba::opaque(0, 205, 205),   // 6: Cyan
                Rgba::opaque(229, 229, 229), // 7: White
                Rgba::opaque(127, 127, 127), // 8: Bright Black (Gray)
                Rgba::opaque(255, 0, 0),     // 9: Bright Red
                Rgba::opaque(0, 255, 0),     // 10: Bright Green
                Rgba::opaque(255, 255, 0),   // 11: Bright Yellow
                Rgba::opaque(92, 92, 255),   // 12: Bright Blue
                Rgba::opaque(255, 0, 255),   // 13: Bright Magenta
                Rgba::opaque(0, 255, 255),   // 14: Bright Cyan
                Rgba::opaque(255, 255, 255), // 15: Bright White
            ],
            foreground: Rgba::opaque(229, 229, 229),
            background: Rgba::opaque(0, 0, 0),
        }
    }
}

impl Palette {
    /// Query the terminal for its actual color palette
    /// 
    /// This sends OSC queries to the terminal to fetch the actual colors.
    /// Note: This requires raw terminal access and may not work with all terminals.
    /// The terminal must be in a state where it can send responses back.
    /// 
    /// For automatic palette detection during program initialization, consider
    /// querying before entering alternate screen mode.
    pub fn query_from_terminal<T>(terminal: &mut T) -> Result<Self, ProgramError>
    where
        T: Read + Write,
    {
        let mut palette = Self::default();

        // Query OSC 4 for colors 0-15
        // We split into two queries to avoid overwhelming the terminal
        write!(
            terminal,
            "\x1b]4;0;?;1;?;2;?;3;?;4;?;5;?;6;?;7;?\x07"
        )
        .map_err(|e| ProgramError::terminal(format!("Failed to write color query: {}", e)))?;
        
        write!(
            terminal,
            "\x1b]4;8;?;9;?;10;?;11;?;12;?;13;?;14;?;15;?\x07"
        )
        .map_err(|e| ProgramError::terminal(format!("Failed to write color query: {}", e)))?;

        // Query OSC 10 and 11 for foreground and background
        write!(terminal, "\x1b]10;?\x07\x1b]11;?\x07")
            .map_err(|e| ProgramError::terminal(format!("Failed to write fg/bg query: {}", e)))?;

        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush: {}", e)))?;

        // Read responses with a timeout
        // Responses come in the format: \x1b]4;INDEX;rgb:RRRR/GGGG/BBBB\x07
        // or \x1b]10;rgb:RRRR/GGGG/BBBB\x07 for foreground
        // or \x1b]11;rgb:RRRR/GGGG/BBBB\x07 for background
        
        let responses = Self::read_responses(terminal, Duration::from_millis(100))?;
        
        // Parse responses
        for response in responses {
            if let Some(parsed) = Self::parse_color_response(&response) {
                match parsed {
                    ColorResponse::Indexed(index, rgba) => {
                        if index < 16 {
                            palette.colors[index] = rgba;
                        }
                    }
                    ColorResponse::Foreground(rgba) => {
                        palette.foreground = rgba;
                    }
                    ColorResponse::Background(rgba) => {
                        palette.background = rgba;
                    }
                }
            }
        }

        Ok(palette)
    }

    fn read_responses<T>(terminal: &mut T, timeout: Duration) -> Result<Vec<String>, ProgramError>
    where
        T: Read,
    {
        use std::time::Instant;
        
        let deadline = Instant::now() + timeout;
        let mut buffer = Vec::new();
        let mut responses = Vec::new();
        
        loop {
            if Instant::now() >= deadline {
                break;
            }
            
            let mut byte = [0u8; 1];
            match terminal.read(&mut byte) {
                Ok(0) => {
                    // No more data
                    std::thread::sleep(Duration::from_millis(5));
                    continue;
                }
                Ok(_) => {
                    buffer.push(byte[0]);
                    
                    // Check for end of response (BEL or ST)
                    if byte[0] == 0x07 || (buffer.len() >= 2 && buffer[buffer.len() - 2] == 0x1b && buffer[buffer.len() - 1] == b'\\') {
                        if let Ok(response) = String::from_utf8(buffer.clone()) {
                            responses.push(response);
                        }
                        buffer.clear();
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    std::thread::sleep(Duration::from_millis(5));
                    continue;
                }
                Err(e) => {
                    return Err(ProgramError::terminal(format!(
                        "Failed to read response: {}",
                        e
                    )));
                }
            }
        }
        
        Ok(responses)
    }

    fn parse_color_response(response: &str) -> Option<ColorResponse> {
        // Response formats:
        // \x1b]4;INDEX;rgb:RRRR/GGGG/BBBB\x07
        // \x1b]10;rgb:RRRR/GGGG/BBBB\x07
        // \x1b]11;rgb:RRRR/GGGG/BBBB\x07
        
        if !response.starts_with("\x1b]") {
            return None;
        }
        
        let content = response.trim_start_matches("\x1b]");
        let content = content.trim_end_matches('\x07').trim_end_matches("\x1b\\");
        
        let parts: Vec<&str> = content.split(';').collect();
        if parts.is_empty() {
            return None;
        }
        
        let osc_number = parts[0];
        
        match osc_number {
            "4" => {
                // Format: 4;INDEX;rgb:RRRR/GGGG/BBBB
                if parts.len() < 3 {
                    return None;
                }
                let index = parts[1].parse::<usize>().ok()?;
                let rgb_part = parts[2];
                let rgba = Self::parse_rgb_value(rgb_part)?;
                Some(ColorResponse::Indexed(index, rgba))
            }
            "10" | "11" => {
                // Format: 10;rgb:RRRR/GGGG/BBBB or 11;rgb:RRRR/GGGG/BBBB
                if parts.len() < 2 {
                    return None;
                }
                let rgb_part = parts[1];
                let rgba = Self::parse_rgb_value(rgb_part)?;
                if osc_number == "10" {
                    Some(ColorResponse::Foreground(rgba))
                } else {
                    Some(ColorResponse::Background(rgba))
                }
            }
            _ => None,
        }
    }

    fn parse_rgb_value(rgb_part: &str) -> Option<Rgba> {
        // Parse rgb:RRRR/GGGG/BBBB
        if !rgb_part.starts_with("rgb:") {
            return None;
        }
        
        let rgb_values = rgb_part.trim_start_matches("rgb:");
        let color_parts: Vec<&str> = rgb_values.split('/').collect();
        if color_parts.len() != 3 {
            return None;
        }
        
        // Parse hex values (can be 1-4 digits, we'll use the high byte)
        let r = u16::from_str_radix(color_parts[0], 16).ok()?;
        let g = u16::from_str_radix(color_parts[1], 16).ok()?;
        let b = u16::from_str_radix(color_parts[2], 16).ok()?;
        
        // Convert to 8-bit (take high byte if more than 8 bits)
        Some(Rgba::opaque(
            (r >> 8) as u8,
            (g >> 8) as u8,
            (b >> 8) as u8,
        ))
    }
}

enum ColorResponse {
    Indexed(usize, Rgba),
    Foreground(Rgba),
    Background(Rgba),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_palette_has_16_colors() {
        let palette = Palette::default();
        assert_eq!(palette.colors.len(), 16);
    }

    #[test]
    fn default_palette_has_black_at_0() {
        let palette = Palette::default();
        assert_eq!(palette.colors[0], Rgba::opaque(0, 0, 0));
    }

    #[test]
    fn parse_color_response_parses_indexed() {
        // OSC 4 format: \x1b]4;INDEX;rgb:RRRR/GGGG/BBBB\x07
        let response = "\x1b]4;0;rgb:0000/0000/0000\x07";
        if let Some(ColorResponse::Indexed(idx, color)) = Palette::parse_color_response(response) {
            assert_eq!(idx, 0);
            assert_eq!(color, Rgba::opaque(0, 0, 0));
        } else {
            panic!("Failed to parse indexed color response");
        }
    }

    #[test]
    fn parse_color_response_parses_foreground() {
        let response = "\x1b]10;rgb:ffff/ffff/ffff\x07";
        if let Some(ColorResponse::Foreground(color)) = Palette::parse_color_response(response) {
            assert_eq!(color, Rgba::opaque(255, 255, 255));
        } else {
            panic!("Failed to parse foreground color response");
        }
    }

    #[test]
    fn parse_color_response_parses_background() {
        let response = "\x1b]11;rgb:1234/5678/9abc\x07";
        if let Some(ColorResponse::Background(color)) = Palette::parse_color_response(response) {
            // High byte of 0x1234 is 0x12, etc.
            assert_eq!(color, Rgba::opaque(0x12, 0x56, 0x9a));
        } else {
            panic!("Failed to parse background color response");
        }
    }

    #[test]
    fn rgba_new_creates_color() {
        let color = Rgba::new(255, 128, 64, 200);
        assert_eq!(color.r, 255);
        assert_eq!(color.g, 128);
        assert_eq!(color.b, 64);
        assert_eq!(color.a, 200);
    }

    #[test]
    fn rgba_opaque_sets_full_alpha() {
        let color = Rgba::opaque(100, 150, 200);
        assert_eq!(color.a, 255);
    }
}
