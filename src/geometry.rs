/// Common geometry primitives shared across the renderer and buffer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rect {
    pub x: usize,
    pub y: usize,
    pub width: usize,
    pub height: usize,
}

impl Rect {
    /// Creates a new rectangle located at `(x, y)` with the given dimensions.
    pub const fn new(x: usize, y: usize, width: usize, height: usize) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }

    /// Returns the overlapping region between two rectangles.
    ///
    /// The resulting rectangle collapses to zero width or height when the inputs
    /// do not overlap on a given axis.
    pub fn intersection(self, other: Self) -> Self {
        let x = self.x.max(other.x);
        let y = self.y.max(other.y);

        let self_right = self.x.saturating_add(self.width);
        let other_right = other.x.saturating_add(other.width);
        let self_bottom = self.y.saturating_add(self.height);
        let other_bottom = other.y.saturating_add(other.height);

        let width = self_right.min(other_right).saturating_sub(x);
        let height = self_bottom.min(other_bottom).saturating_sub(y);

        Self {
            x,
            y,
            width,
            height,
        }
    }

    /// Indicates whether the rectangle encloses any area.
    pub fn has_area(self) -> bool {
        self.width > 0 && self.height > 0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Point {
    pub x: usize,
    pub y: usize,
}

impl Point {
    pub const fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }
}
