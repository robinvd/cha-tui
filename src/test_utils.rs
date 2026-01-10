use std::convert::TryInto;

use crate::ProgramError;
use crate::buffer::DoubleBuffer;
use crate::dom::{RetainedNode, rounding::round_layout};
use crate::event::Size;
use crate::palette::Palette;
use crate::render::Renderer;

use taffy::compute_root_layout;
use taffy::prelude::AvailableSpace;

fn layout_node<Msg>(node: &mut RetainedNode<Msg>, size: Size) {
    compute_root_layout(
        node,
        u64::MAX.into(),
        taffy::Size {
            width: AvailableSpace::Definite(size.width as f32),
            height: AvailableSpace::Definite(size.height as f32),
        },
    );
    round_layout(node);
}

fn buffer_to_lines(buffer: &DoubleBuffer) -> Vec<String> {
    buffer
        .back_buffer()
        .iter()
        .map(|row| row.iter().map(|cell| cell.ch).collect())
        .collect()
}

fn try_size(width: usize, height: usize) -> Result<Size, ProgramError> {
    let width_u16: u16 = width
        .try_into()
        .map_err(|_| ProgramError::render("width exceeds u16::MAX"))?;
    let height_u16: u16 = height
        .try_into()
        .map_err(|_| ProgramError::render("height exceeds u16::MAX"))?;
    Ok(Size::new(width_u16, height_u16))
}

/// Render a node into a vector of lines for assertions in tests.
///
/// The layout is recomputed for the provided bounds before rendering.
pub fn render_node_to_lines<Msg>(
    node: impl Into<RetainedNode<Msg>>,
    width: usize,
    height: usize,
) -> Result<Vec<String>, ProgramError> {
    let mut node: RetainedNode<Msg> = node.into();
    let size = try_size(width, height)?;

    layout_node(&mut node, size);

    let mut buffer = DoubleBuffer::new(width, height);
    let palette = Palette::default();
    {
        let mut renderer = Renderer::new(&mut buffer, &palette);
        renderer.render(&node, size)?;
    }

    Ok(buffer_to_lines(&buffer))
}

/// Render a node into a single string with newline-separated rows.
pub fn render_node_to_string<Msg>(
    node: impl Into<RetainedNode<Msg>>,
    width: usize,
    height: usize,
) -> Result<String, ProgramError> {
    let lines = render_node_to_lines(node, width, height)?;
    Ok(lines.join("\n"))
}
