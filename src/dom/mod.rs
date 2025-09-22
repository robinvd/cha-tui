use std::collections::HashMap;
use std::marker::PhantomData;

use crate::event::Size;
use taffy::{
    style::Style as TaffyStyle,
    tree::{Cache as TaffyCache, Layout as TaffyLayout, NodeId, TraversePartialTree},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Node<Msg> {
    pub content: NodeContent<Msg>,
    classname: &'static str,
    id: u64,
    pub layout_state: LayoutState,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeContent<Msg> {
    Element(ElementNode<Msg>),
    Text(TextNode<Msg>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LayoutState {
    pub style: TaffyStyle,
    pub cache: TaffyCache,
    pub layout: TaffyLayout,
}

impl Default for LayoutState {
    fn default() -> Self {
        Self {
            style: TaffyStyle::default(),
            cache: TaffyCache::new(),
            layout: TaffyLayout::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElementNode<Msg> {
    pub kind: ElementKind,
    pub attrs: Attributes,
    pub children: Vec<Node<Msg>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextNode<Msg> {
    pub content: String,
    pub style: Style,
    _marker: PhantomData<Msg>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ElementKind {
    Column,
    Row,
    Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Attributes {
    pub width: SizePolicy,
    pub height: SizePolicy,
    pub style: Style,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SizePolicy {
    Content,
    Fill,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Style {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub bold: bool,
    pub border: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Color {
    Reset,
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
}

pub fn text<Msg>(content: impl Into<String>) -> Node<Msg> {
    Node::new(NodeContent::Text(TextNode::new(content)))
}

pub fn column<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    Node::new(NodeContent::Element(ElementNode::new(
        ElementKind::Column,
        children,
    )))
}

pub fn row<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    Node::new(NodeContent::Element(ElementNode::new(
        ElementKind::Row,
        children,
    )))
}

pub fn block<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    let mut element = ElementNode::new(ElementKind::Block, children);
    element.attrs.style.border = true;
    Node::new(NodeContent::Element(element))
}

impl<Msg> Node<Msg> {
    pub fn new(content: NodeContent<Msg>) -> Self {
        Self {
            content,
            layout_state: LayoutState::default(),
            classname: "",
            id: 0,
        }
    }

    // Add an ID to this node, this is used so node references are stable
    // across renders.
    pub fn with_id(mut self, id: &'static str) -> Self {
        self.classname = id;
        self.id = crate::hash::hash_str(0, self.classname);
        self
    }

    // Same as with_id, but add an additional mixin.
    //
    // Used when there are a variable number of items (like a list), then each
    // list item has a unique ID.
    pub fn with_id_mixin(mut self, id: &'static str, mixin: u64) -> Self {
        self.classname = id;
        self.id = crate::hash::hash_str(mixin, self.classname);
        self
    }

    pub fn with_style(mut self, style: Style) -> Self {
        match &mut self.content {
            NodeContent::Element(element) => {
                element.attrs.style = style;
            }
            NodeContent::Text(text) => {
                text.style = style;
            }
        }
        self
    }

    pub fn layout_state(&self) -> &LayoutState {
        &self.layout_state
    }

    pub fn layout_state_mut(&mut self) -> &mut LayoutState {
        &mut self.layout_state
    }

    pub fn as_element(&self) -> Option<&ElementNode<Msg>> {
        match &self.content {
            NodeContent::Element(element) => Some(element),
            _ => None,
        }
    }

    pub fn as_text(&self) -> Option<&TextNode<Msg>> {
        match &self.content {
            NodeContent::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn into_element(self) -> Option<ElementNode<Msg>> {
        match self.content {
            NodeContent::Element(element) => Some(element),
            _ => None,
        }
    }

    pub fn into_text(self) -> Option<TextNode<Msg>> {
        match self.content {
            NodeContent::Text(text) => Some(text),
            _ => None,
        }
    }

    fn build_children_map(&self) -> HashMap<NodeId, Vec<NodeId>> {
        fn collect<Msg>(
            node: &Node<Msg>,
            next_id: &mut usize,
            map: &mut HashMap<NodeId, Vec<NodeId>>,
        ) -> NodeId {
            let current_id = NodeId::from(*next_id);
            *next_id += 1;

            let mut child_ids = Vec::new();
            if let NodeContent::Element(element) = &node.content {
                for child in &element.children {
                    let child_id = collect(child, next_id, map);
                    child_ids.push(child_id);
                }
            }

            map.insert(current_id, child_ids);
            current_id
        }

        let mut map = HashMap::new();
        let mut next_id = 0;
        collect(self, &mut next_id, &mut map);
        map
    }
}

impl Style {
    pub fn fg(color: Color) -> Self {
        Self {
            fg: Some(color),
            ..Self::default()
        }
    }

    pub fn bg(color: Color) -> Self {
        Self {
            bg: Some(color),
            ..Self::default()
        }
    }

    pub fn bold() -> Self {
        Self {
            bold: true,
            ..Self::default()
        }
    }
}

impl Default for Attributes {
    fn default() -> Self {
        Self {
            width: SizePolicy::Content,
            height: SizePolicy::Content,
            style: Style::default(),
        }
    }
}

impl Attributes {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_size(mut self, width: SizePolicy, height: SizePolicy) -> Self {
        self.width = width;
        self.height = height;
        self
    }

    pub fn with_style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
}

impl<Msg> ElementNode<Msg> {
    pub fn new(kind: ElementKind, children: Vec<Node<Msg>>) -> Self {
        Self {
            kind,
            attrs: Attributes::default(),
            children,
        }
    }

    pub fn layout(&self, available: Size) -> Size {
        available
    }
}

impl<Msg> TextNode<Msg> {
    pub fn new(content: impl Into<String>) -> Self {
        Self {
            content: content.into(),
            style: Style::default(),
            _marker: PhantomData,
        }
    }
}

impl<Msg> TraversePartialTree for Node<Msg> {
    type ChildIter<'a>
        = std::vec::IntoIter<NodeId>
    where
        Self: 'a;

    fn child_ids(&self, parent_node_id: NodeId) -> Self::ChildIter<'_> {
        let mut map = self.build_children_map();
        map.remove(&parent_node_id).unwrap_or_default().into_iter()
    }

    fn child_count(&self, parent_node_id: NodeId) -> usize {
        let map = self.build_children_map();
        map.get(&parent_node_id).map_or(0, Vec::len)
    }

    fn get_child_id(&self, parent_node_id: NodeId, child_index: usize) -> NodeId {
        let map = self.build_children_map();
        *map.get(&parent_node_id)
            .and_then(|children| children.get(child_index))
            .expect("child index out of bounds")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_node_preserves_content() {
        let node: Node<()> = text("hello");

        match node.into_text() {
            Some(text) => {
                assert_eq!(text.content, "hello");
                assert_eq!(text.style, Style::default());
            }
            None => panic!("expected text node"),
        }
    }

    #[test]
    fn column_node_wraps_children() {
        let child: Node<()> = text("child");
        let node = column(vec![child.clone()]);

        match node.into_element() {
            Some(element) => {
                assert_eq!(element.kind, ElementKind::Column);
                assert_eq!(element.children, vec![child]);
                assert_eq!(element.attrs, Attributes::default());
            }
            None => panic!("expected element node"),
        }
    }

    #[test]
    fn row_node_wraps_children() {
        let child: Node<()> = text("row child");
        let node = row(vec![child.clone()]);

        match node.into_element() {
            Some(element) => {
                assert_eq!(element.kind, ElementKind::Row);
                assert_eq!(element.children, vec![child]);
            }
            None => panic!("expected element node"),
        }
    }

    #[test]
    fn block_node_sets_border() {
        let node = block::<()>(Vec::new());

        match node.into_element() {
            Some(element) => {
                assert!(element.attrs.style.border);
            }
            None => panic!("expected element node"),
        }
    }

    #[test]
    fn with_style_replaces_styles() {
        let style = Style::fg(Color::Blue);
        let node = text::<()>("styled").with_style(style.clone());

        match node.into_text() {
            Some(text) => assert_eq!(text.style, style),
            None => panic!("expected text node"),
        }
    }

    #[test]
    fn size_policy_builder_updates_fields() {
        let attrs = Attributes::new().with_size(SizePolicy::Fill, SizePolicy::Content);

        assert_eq!(attrs.width, SizePolicy::Fill);
        assert_eq!(attrs.height, SizePolicy::Content);
    }

    #[test]
    fn layout_returns_available_size() {
        let node = ElementNode::<()>::new(ElementKind::Column, Vec::new());
        let available = Size::new(10, 5);

        assert_eq!(node.layout(available), available);
    }
}
