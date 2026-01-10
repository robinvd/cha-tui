use crate::dom::{
    ElementNode, Node, NodeContent, RenderablePatch, RetainedElementNode,
    RetainedNode, RetainedNodeContent, TextNode, TextNodeRef,
};

pub type NodeRef<'a, Msg> = Node<'a, Msg>;
pub type ElementNodeRef<'a, Msg> = ElementNode<'a, Msg>;
pub type NodeContentRef<'a, Msg> = NodeContent<'a, Msg>;

pub enum PatchResult<Msg> {
    Patched {
        node: RetainedNode<Msg>,
        layout_changed: bool,
    },
    Replaced(RetainedNode<Msg>),
}

/// Attempts to patch `existing` in place using the structure from `new_node`.
///
/// Returns [`PatchResult::Patched`] with a flag indicating whether the node's
/// layout needs recomputing when the node could be updated in place. If the
/// incoming node cannot be merged with the existing one, [`PatchResult::Replaced`]
/// is returned containing the new node to install instead.
pub fn patch<Msg>(
    mut existing: RetainedNode<Msg>,
    new_node: RetainedNode<Msg>,
) -> PatchResult<Msg> {
    if !can_patch(&existing, &new_node) {
        return PatchResult::Replaced(new_node);
    }

    let RetainedNode {
        content: new_content,
        classname: new_classname,
        id: new_id,
        layout_state: new_layout_state,
        on_mouse: new_on_mouse,
        on_resize: new_on_resize,
        scroll_x: new_scroll_x,
        scroll_y: new_scroll_y,
        pending_scroll: new_pending_scroll,
    } = new_node;

    existing.classname = new_classname;
    existing.id = new_id;
    existing.scroll_x = new_scroll_x;
    existing.scroll_y = new_scroll_y;
    existing.on_mouse = new_on_mouse;
    existing.on_resize = new_on_resize;
    existing.pending_scroll = new_pending_scroll;

    let mut layout_changed = false;

    if existing.layout_state.style != new_layout_state.style {
        existing.layout_state.style = new_layout_state.style;
        layout_changed = true;
    }

    match (&mut existing.content, new_content) {
        (RetainedNodeContent::Text(existing_text), RetainedNodeContent::Text(new_text)) => {
            let spans_changed = existing_text.spans() != new_text.spans();
            if spans_changed {
                *existing_text = new_text;
                layout_changed = true;
            }
        }
        (
            RetainedNodeContent::Element(existing_element),
            RetainedNodeContent::Element(new_element),
        ) => {
            let element_changed = patch_element(existing_element, new_element);
            layout_changed |= element_changed;
        }
        (
            RetainedNodeContent::Renderable(existing_leaf),
            RetainedNodeContent::Renderable(new_leaf),
        ) => match new_leaf.patch_retained(&mut **existing_leaf) {
            RenderablePatch::NoChange => {}
            RenderablePatch::ChangedNoLayout => {}
            RenderablePatch::ChangedLayout => {
                layout_changed = true;
            }
            RenderablePatch::Replace => {
                *existing_leaf = new_leaf;
                layout_changed = true;
            }
        },
        _ => unreachable!("content mismatch should have been excluded by can_patch"),
    }

    if layout_changed {
        existing.layout_state.cache.clear();
        existing.layout_state.size_dirty = true;
    }

    PatchResult::Patched {
        node: existing,
        layout_changed,
    }
}

/// Patch a retained Node with a borrowed Node.
/// This is used when the view function returns a borrowed node that needs to be
/// patched into the existing retained node tree.
pub fn patch_borrowed<'a, Msg>(
    mut existing: RetainedNode<Msg>,
    new_node: NodeRef<'a, Msg>,
) -> PatchResult<Msg> {
    if !can_patch_borrowed(&existing, &new_node) {
        return PatchResult::Replaced(new_node.into());
    }

    existing.classname = new_node.classname;
    existing.id = new_node.id;
    existing.scroll_x = new_node.scroll_x;
    existing.scroll_y = new_node.scroll_y;
    existing.on_mouse = new_node.on_mouse;
    existing.on_resize = new_node.on_resize;
    existing.pending_scroll = new_node.pending_scroll;

    let mut layout_changed = false;

    // Check if layout style changed
    if existing.layout_state.style != new_node.layout_style {
        existing.layout_state.style = new_node.layout_style;
        layout_changed = true;
    }

    match (&mut existing.content, new_node.content) {
        (RetainedNodeContent::Text(existing_text), NodeContent::Text(new_text)) => {
            let spans_changed = text_node_spans_differ(existing_text, &new_text);
            if spans_changed {
                *existing_text = new_text.into();
                layout_changed = true;
            }
        }
        (RetainedNodeContent::Element(existing_element), NodeContent::Element(new_element)) => {
            let element_changed = patch_element_ref(existing_element, new_element);
            layout_changed |= element_changed;
        }
        (RetainedNodeContent::Renderable(existing_leaf), NodeContent::Renderable(new_leaf_ref)) => {
            match new_leaf_ref.patch_retained(&mut **existing_leaf) {
                RenderablePatch::NoChange => {}
                RenderablePatch::ChangedNoLayout => {}
                RenderablePatch::ChangedLayout => {
                    layout_changed = true;
                }
                RenderablePatch::Replace => {
                    *existing_leaf = new_leaf_ref.into_retained();
                    layout_changed = true;
                }
            }
        }
        _ => unreachable!("content mismatch should have been excluded by can_patch_borrowed"),
    }

    if layout_changed {
        existing.layout_state.cache.clear();
        existing.layout_state.size_dirty = true;
    }

    PatchResult::Patched {
        node: existing,
        layout_changed,
    }
}

fn text_node_spans_differ(existing: &TextNode, new: &TextNodeRef) -> bool {
    let existing_spans = existing.spans();
    let new_spans = new.spans();

    if existing_spans.len() != new_spans.len() {
        return true;
    }

    for (e, n) in existing_spans.iter().zip(new_spans.iter()) {
        if e.style != n.style || e.content != n.content.as_ref() {
            return true;
        }
    }

    false
}

fn can_patch_borrowed<'a, Msg>(existing: &RetainedNode<Msg>, new_node: &NodeRef<'a, Msg>) -> bool {
    if existing.id != 0 && new_node.id != 0 && existing.id != new_node.id {
        return false;
    }
    if !existing.classname.is_empty()
        && !new_node.classname.is_empty()
        && existing.classname != new_node.classname
    {
        return false;
    }

    match (&existing.content, &new_node.content) {
        (RetainedNodeContent::Text(_), NodeContent::Text(_)) => true,
        (RetainedNodeContent::Element(existing_element), NodeContent::Element(new_element)) => {
            existing_element.kind == new_element.kind
        }
        (RetainedNodeContent::Renderable(_), NodeContent::Renderable(_)) => true,
        _ => false,
    }
}

fn patch_element_ref<'a, Msg>(
    existing: &mut RetainedElementNode<Msg>,
    new_element: ElementNode<'a, Msg>,
) -> bool {
    debug_assert_eq!(existing.kind, new_element.kind);

    let mut layout_changed = false;

    let attrs_changed = existing.attrs != new_element.attrs;
    existing.attrs = new_element.attrs;
    if attrs_changed {
        layout_changed = true;
    }

    if existing.title != new_element.title {
        existing.title = new_element.title;
    }

    let children_changed = patch_children_ref(&mut existing.children, new_element.children);
    if children_changed {
        layout_changed = true;
    }

    layout_changed
}

fn patch_children_ref<'a, Msg>(
    existing_children: &mut Vec<RetainedNode<Msg>>,
    new_children: Vec<Node<'a, Msg>>,
) -> bool {
    if existing_children.is_empty() && new_children.is_empty() {
        return false;
    }

    let mut layout_changed = false;

    let mut remaining: Vec<(usize, RetainedNode<Msg>)> =
        existing_children.drain(..).enumerate().collect();

    let mut next_children = Vec::with_capacity(new_children.len());

    for (index, new_child) in new_children.into_iter().enumerate() {
        if let Some(existing_child) = take_matching_child_ref(&mut remaining, &new_child, index) {
            match patch_borrowed(existing_child, new_child) {
                PatchResult::Patched {
                    node,
                    layout_changed: child_layout_changed,
                } => {
                    if child_layout_changed {
                        layout_changed = true;
                    }
                    next_children.push(node);
                }
                PatchResult::Replaced(replacement) => {
                    layout_changed = true;
                    next_children.push(replacement);
                }
            }
        } else {
            layout_changed = true;
            next_children.push(new_child.into());
        }
    }

    if !remaining.is_empty() {
        layout_changed = true;
    }

    *existing_children = next_children;

    layout_changed
}

fn take_matching_child_ref<'a, Msg>(
    remaining: &mut Vec<(usize, RetainedNode<Msg>)>,
    target: &Node<'a, Msg>,
    new_index: usize,
) -> Option<RetainedNode<Msg>> {
    if let Some(pos) = find_child_by_id_ref(remaining, target.id) {
        return Some(remaining.swap_remove(pos).1);
    }

    if let Some(pos) = find_child_by_class_ref(remaining, target.classname) {
        return Some(remaining.swap_remove(pos).1);
    }

    if let Some(pos) = remaining.iter().position(|(original_index, node)| {
        node.id == 0 && node.classname.is_empty() && *original_index == new_index
    }) {
        return Some(remaining.remove(pos).1);
    }

    if !remaining.is_empty() {
        return Some(remaining.remove(0).1);
    }

    None
}

fn find_child_by_id_ref<Msg>(remaining: &[(usize, RetainedNode<Msg>)], id: u64) -> Option<usize> {
    if id == 0 {
        None
    } else {
        remaining
            .iter()
            .position(|(_, node)| node.id != 0 && node.id == id)
    }
}

fn find_child_by_class_ref<Msg>(
    remaining: &[(usize, RetainedNode<Msg>)],
    classname: &'static str,
) -> Option<usize> {
    if classname.is_empty() {
        None
    } else {
        remaining
            .iter()
            .position(|(_, node)| !node.classname.is_empty() && node.classname == classname)
    }
}

fn can_patch<Msg>(existing: &RetainedNode<Msg>, new_node: &RetainedNode<Msg>) -> bool {
    if existing.id != 0 && new_node.id != 0 && existing.id != new_node.id {
        return false;
    }
    if !existing.classname.is_empty()
        && !new_node.classname.is_empty()
        && existing.classname != new_node.classname
    {
        return false;
    }

    match (&existing.content, &new_node.content) {
        (RetainedNodeContent::Text(_), RetainedNodeContent::Text(_)) => true,
        (
            RetainedNodeContent::Element(existing_element),
            RetainedNodeContent::Element(new_element),
        ) => existing_element.kind == new_element.kind,
        (RetainedNodeContent::Renderable(_), RetainedNodeContent::Renderable(_)) => true,
        _ => false,
    }
}

fn patch_element<Msg>(
    existing: &mut RetainedElementNode<Msg>,
    new_element: RetainedElementNode<Msg>,
) -> bool {
    debug_assert_eq!(existing.kind, new_element.kind);

    let RetainedElementNode {
        kind: _,
        attrs: new_attrs,
        children: new_children,
        title: new_title,
    } = new_element;

    let mut layout_changed = false;

    let attrs_changed = existing.attrs != new_attrs;
    existing.attrs = new_attrs;
    if attrs_changed {
        layout_changed = true;
    }

    if existing.title != new_title {
        existing.title = new_title;
    }

    let children_changed = patch_children(&mut existing.children, new_children);
    if children_changed {
        layout_changed = true;
    }

    layout_changed
}

fn patch_children<Msg>(
    existing_children: &mut Vec<RetainedNode<Msg>>,
    new_children: Vec<RetainedNode<Msg>>,
) -> bool {
    if existing_children.is_empty() && new_children.is_empty() {
        return false;
    }

    let mut layout_changed = false;

    let mut remaining: Vec<(usize, RetainedNode<Msg>)> =
        existing_children.drain(..).enumerate().collect();

    let mut next_children = Vec::with_capacity(new_children.len());

    for (index, new_child) in new_children.into_iter().enumerate() {
        if let Some(existing_child) = take_matching_child(&mut remaining, &new_child, index) {
            match patch(existing_child, new_child) {
                PatchResult::Patched {
                    node,
                    layout_changed: child_layout_changed,
                } => {
                    if child_layout_changed {
                        layout_changed = true;
                    }
                    next_children.push(node);
                }
                PatchResult::Replaced(replacement) => {
                    layout_changed = true;
                    next_children.push(replacement);
                }
            }
        } else {
            layout_changed = true;
            next_children.push(new_child);
        }
    }

    if !remaining.is_empty() {
        layout_changed = true;
    }

    *existing_children = next_children;

    layout_changed
}

fn take_matching_child<Msg>(
    remaining: &mut Vec<(usize, RetainedNode<Msg>)>,
    target: &RetainedNode<Msg>,
    new_index: usize,
) -> Option<RetainedNode<Msg>> {
    if let Some(pos) = find_child_by_id(remaining, target.id) {
        return Some(remaining.swap_remove(pos).1);
    }

    if let Some(pos) = find_child_by_class(remaining, target.classname) {
        return Some(remaining.swap_remove(pos).1);
    }

    if let Some(pos) = remaining.iter().position(|(original_index, node)| {
        node.id == 0 && node.classname.is_empty() && *original_index == new_index
    }) {
        return Some(remaining.remove(pos).1);
    }

    if !remaining.is_empty() {
        return Some(remaining.remove(0).1);
    }

    None
}

fn find_child_by_id<Msg>(remaining: &[(usize, RetainedNode<Msg>)], id: u64) -> Option<usize> {
    if id == 0 {
        None
    } else {
        remaining
            .iter()
            .position(|(_, node)| node.id != 0 && node.id == id)
    }
}

fn find_child_by_class<Msg>(
    remaining: &[(usize, RetainedNode<Msg>)],
    classname: &'static str,
) -> Option<usize> {
    if classname.is_empty() {
        None
    } else {
        remaining
            .iter()
            .position(|(_, node)| !node.classname.is_empty() && node.classname == classname)
    }
}

#[cfg(test)]
mod tests {
    use super::{PatchResult, patch};
    use crate::dom::{RetainedNode, column_retained, text_retained};

    #[test]
    fn identical_nodes_do_not_trigger_layout_change() {
        let existing: RetainedNode<()> =
            column_retained(vec![text_retained("a").with_id("child")]).with_id("root");
        let fresh: RetainedNode<()> =
            column_retained(vec![text_retained("a").with_id("child")]).with_id("root");

        let patched = match patch(existing, fresh) {
            PatchResult::Patched {
                node,
                layout_changed,
            } => {
                assert!(!layout_changed);
                node
            }
            PatchResult::Replaced(_) => panic!("expected patch"),
        };
        assert_eq!(patched.as_element().unwrap().children.len(), 1);
    }

    #[test]
    fn text_change_marks_layout_dirty() {
        let existing: RetainedNode<()> = text_retained("short").with_id("text");
        let fresh: RetainedNode<()> = text_retained("longer text").with_id("text");

        let patched = match patch(existing, fresh) {
            PatchResult::Patched {
                node,
                layout_changed,
            } => {
                assert!(layout_changed);
                node
            }
            PatchResult::Replaced(_) => panic!("expected patch"),
        };
        assert_eq!(
            patched.as_text().expect("text node").spans()[0].content,
            "longer text"
        );
    }

    #[test]
    fn replaces_mismatched_nodes() {
        let existing: RetainedNode<()> = column_retained(vec![]).with_id("root");
        let fresh: RetainedNode<()> = text_retained("hi").with_id("root");

        match patch(existing, fresh) {
            PatchResult::Patched { .. } => panic!("expected replacement"),
            PatchResult::Replaced(_) => {}
        }
    }
}
