use crate::Node;
use taffy::{Layout, Size};

/// Rounds the calculated layout to exact pixel values
///
/// In order to ensure that no gaps in the layout are introduced we:
///   - Always round based on the cumulative x/y coordinates (relative to the viewport) rather than
///     parent-relative coordinates
///   - Compute width/height by first rounding the top/bottom/left/right and then computing the difference
///     rather than rounding the width/height directly
///
/// See <https://github.com/facebook/yoga/commit/aa5b296ac78f7a22e1aeaf4891243c6bb76488e2> for more context
///
/// In order to prevent innacuracies caused by rounding already-rounded values, we read from `unrounded_layout`
/// and write to `final_layout`.
pub fn round_layout<M>(root: &mut Node<M>) {
    return round_layout_inner(root, 0.0, 0.0);

    /// Recursive function to apply rounding to all descendents
    fn round_layout_inner<M>(
        // tree: &mut impl RoundTree,
        node: &mut Node<M>,
        cumulative_x: f32,
        cumulative_y: f32,
    ) {
        let unrounded_layout = node.get_unrounded_layout();
        let mut layout = unrounded_layout;

        let cumulative_x = cumulative_x + unrounded_layout.location.x;
        let cumulative_y = cumulative_y + unrounded_layout.location.y;

        layout.location.x = f32::round(unrounded_layout.location.x);
        layout.location.y = f32::round(unrounded_layout.location.y);
        layout.size.width =
            f32::round(cumulative_x + unrounded_layout.size.width) - f32::round(cumulative_x);
        layout.size.height =
            f32::round(cumulative_y + unrounded_layout.size.height) - f32::round(cumulative_y);
        layout.scrollbar_size.width = f32::round(unrounded_layout.scrollbar_size.width);
        layout.scrollbar_size.height = f32::round(unrounded_layout.scrollbar_size.height);
        layout.border.left =
            f32::round(cumulative_x + unrounded_layout.border.left) - f32::round(cumulative_x);
        layout.border.right = f32::round(cumulative_x + unrounded_layout.size.width)
            - f32::round(
                cumulative_x + unrounded_layout.size.width - unrounded_layout.border.right,
            );
        layout.border.top =
            f32::round(cumulative_y + unrounded_layout.border.top) - f32::round(cumulative_y);
        layout.border.bottom = f32::round(cumulative_y + unrounded_layout.size.height)
            - f32::round(
                cumulative_y + unrounded_layout.size.height - unrounded_layout.border.bottom,
            );
        layout.padding.left =
            f32::round(cumulative_x + unrounded_layout.padding.left) - f32::round(cumulative_x);
        layout.padding.right = f32::round(cumulative_x + unrounded_layout.size.width)
            - f32::round(
                cumulative_x + unrounded_layout.size.width - unrounded_layout.padding.right,
            );
        layout.padding.top =
            f32::round(cumulative_y + unrounded_layout.padding.top) - f32::round(cumulative_y);
        layout.padding.bottom = f32::round(cumulative_y + unrounded_layout.size.height)
            - f32::round(
                cumulative_y + unrounded_layout.size.height - unrounded_layout.padding.bottom,
            );

        // #[cfg(feature = "content_size")]
        round_content_size(
            &mut layout,
            unrounded_layout.content_size,
            cumulative_x,
            cumulative_y,
        );

        node.set_final_layout(&layout);

        match &mut node.content {
            crate::dom::NodeContent::Element(element_node) => {
                for child in &mut element_node.children {
                    round_layout_inner(child, cumulative_x, cumulative_y);
                }
            }
            crate::dom::NodeContent::Text(_) => {}
            crate::dom::NodeContent::Leaf(_) => {}
        }
    }

    // #[cfg(feature = "content_size")]
    #[inline(always)]
    /// Round content size variables.
    /// This is split into a separate function to make it easier to feature flag.
    fn round_content_size(
        layout: &mut Layout,
        unrounded_content_size: Size<f32>,
        cumulative_x: f32,
        cumulative_y: f32,
    ) {
        layout.content_size.width =
            f32::round(cumulative_x + unrounded_content_size.width) - f32::round(cumulative_x);
        layout.content_size.height =
            f32::round(cumulative_y + unrounded_content_size.height) - f32::round(cumulative_y);
    }
}
