//! Contains the print_tree function for printing a debug representation of the tree

use super::RetainedNode;

/// Prints a debug representation of the computed layout for a tree of nodes, starting with the passed root node.
pub fn print_tree<Msg>(root: &RetainedNode<Msg>) {
    if tracing::Level::DEBUG < tracing::level_filters::LevelFilter::current() {
        return;
    }
    tracing::debug!("TREE");
    print_node(root, false, String::new());

    /// Recursive function that prints each node in the tree
    fn print_node<Msg>(node: &RetainedNode<Msg>, has_sibling: bool, lines_string: String) {
        let layout = &node.get_final_layout();
        let label = node.get_debug_label();

        let fork_string = if has_sibling {
            "├── "
        } else {
            "└── "
        };
        tracing::debug!(
            "{lines}{fork} {label} [x: {x:<4} y: {y:<4} w: {width:<4} h: {height:<4} content_w: {content_width:<4} content_h: {content_height:<4} border: l:{bl} r:{br} t:{bt} b:{bb}, padding: l:{pl} r:{pr} t:{pt} b:{pb}] ({key:?})",
            lines = lines_string,
            fork = fork_string,
            label = label,
            x = layout.location.x,
            y = layout.location.y,
            width = layout.size.width,
            height = layout.size.height,
            content_width = layout.content_size.width,
            content_height = layout.content_size.height,
            bl = layout.border.left,
            br = layout.border.right,
            bt = layout.border.top,
            bb = layout.border.bottom,
            pl = layout.padding.left,
            pr = layout.padding.right,
            pt = layout.padding.top,
            pb = layout.padding.bottom,
            key = node.id,
        );
        let bar = if has_sibling { "│   " } else { "    " };
        let new_string = lines_string + bar;

        match &node.content {
            crate::dom::RetainedNodeContent::Element(element_node) => {
                let num_children = element_node.children.len();
                for (index, child) in &mut element_node.children.iter().enumerate() {
                    let has_sibling = index < num_children - 1;
                    print_node(child, has_sibling, new_string.clone());
                }
            }
            crate::dom::RetainedNodeContent::Renderable(_) => {}
        }

        // Recurse into children
        // for (index, child) in node.child_ids().enumerate() {
        //     let has_sibling = index < num_children - 1;
        //     print_node(child, has_sibling, new_string.clone());
        // }
    }
}
