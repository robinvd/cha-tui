pub mod rounding;

use std::marker::PhantomData;

use crate::event::Size;
use taffy::{
    CacheTree, LayoutFlexboxContainer, compute_cached_layout, compute_flexbox_layout,
    compute_leaf_layout,
};
use taffy::{
    LayoutPartialTree,
    geometry::Rect,
    style::{FlexDirection, LengthPercentage, Style as TaffyStyle},
    tree::{Cache as TaffyCache, Layout as TaffyLayout, NodeId, TraversePartialTree},
};
use termwiz::cell::unicode_column_width;

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
    pub unrounded_layout: TaffyLayout,
    pub layout: TaffyLayout,
}

impl Default for LayoutState {
    fn default() -> Self {
        Self {
            style: TaffyStyle::default(),
            cache: TaffyCache::new(),
            unrounded_layout: TaffyLayout::new(),
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
    let mut node = Node::new(NodeContent::Element(ElementNode::new(
        ElementKind::Column,
        children,
    )));
    node.layout_state.style.flex_direction = FlexDirection::Column;
    node
}

pub fn row<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    let mut node = Node::new(NodeContent::Element(ElementNode::new(
        ElementKind::Row,
        children,
    )));
    node.layout_state.style.flex_direction = FlexDirection::Row;
    node
}

pub fn block<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    let mut element = ElementNode::new(ElementKind::Block, children);
    element.attrs.style.border = true;
    let mut node = Node::new(NodeContent::Element(element));
    node.layout_state.style.flex_direction = FlexDirection::Column;
    node.layout_state.style.padding = Rect {
        left: LengthPercentage::length(1.0),
        right: LengthPercentage::length(1.0),
        top: LengthPercentage::length(1.0),
        bottom: LengthPercentage::length(1.0),
    };
    node
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

    fn node_from_id(&self, id: NodeId) -> &Self {
        let index: u64 = id.into();
        if index == u64::MAX {
            self
        } else {
            match &self.content {
                NodeContent::Element(element_node) => &element_node.children[index as usize],
                NodeContent::Text(_) => panic!("text has no children"),
            }
        }
    }

    fn node_from_id_mut(&mut self, id: NodeId) -> &mut Self {
        let index: u64 = id.into();
        if index == u64::MAX {
            self
        } else {
            match &mut self.content {
                NodeContent::Element(element_node) => &mut element_node.children[index as usize],
                NodeContent::Text(_) => panic!("text has no children"),
            }
        }
    }

    pub(crate) fn get_unrounded_layout(&self) -> TaffyLayout {
        self.layout_state.unrounded_layout
    }

    pub(crate) fn set_final_layout(&mut self, layout: &TaffyLayout) {
        self.layout_state.layout = *layout
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
        = std::iter::Map<std::ops::Range<usize>, fn(usize) -> NodeId>
    where
        Self: 'a;

    fn child_ids(&self, parent_node_id: NodeId) -> Self::ChildIter<'_> {
        let node = self.node_from_id(parent_node_id);
        (0..node.child_count(parent_node_id)).map(Into::into)
    }

    fn child_count(&self, parent_node_id: NodeId) -> usize {
        let node = self.node_from_id(parent_node_id);
        match &node.content {
            NodeContent::Element(element_node) => element_node.children.len(),
            NodeContent::Text(_) => 0,
        }
    }

    fn get_child_id(&self, parent_node_id: NodeId, child_index: usize) -> NodeId {
        self.child_ids(parent_node_id).nth(child_index).unwrap()
    }
}

impl<Msg> CacheTree for Node<Msg> {
    fn cache_get(
        &self,
        node_id: NodeId,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
        run_mode: taffy::RunMode,
    ) -> Option<taffy::LayoutOutput> {
        let node = self.node_from_id(node_id);
        node.layout_state
            .cache
            .get(known_dimensions, available_space, run_mode)
    }

    fn cache_store(
        &mut self,
        node_id: NodeId,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
        run_mode: taffy::RunMode,
        layout_output: taffy::LayoutOutput,
    ) {
        let node = self.node_from_id_mut(node_id);
        node.layout_state
            .cache
            .store(known_dimensions, available_space, run_mode, layout_output);
    }

    fn cache_clear(&mut self, node_id: NodeId) {
        let node = self.node_from_id_mut(node_id);
        node.layout_state.cache.clear();
    }
}

impl<Msg> LayoutPartialTree for Node<Msg> {
    type CoreContainerStyle<'a>
        = taffy::Style
    where
        Self: 'a;

    type CustomIdent = String;

    fn get_core_container_style(&self, node_id: NodeId) -> Self::CoreContainerStyle<'_> {
        let node = self.node_from_id(node_id);
        node.layout_state.style.clone()
    }

    fn set_unrounded_layout(&mut self, node_id: NodeId, layout: &TaffyLayout) {
        let node = self.node_from_id_mut(node_id);
        node.layout_state.unrounded_layout = *layout
    }

    fn compute_child_layout(
        &mut self,
        node_id: NodeId,
        inputs: taffy::LayoutInput,
    ) -> taffy::LayoutOutput {
        compute_cached_layout(self, node_id, inputs, |parent, node_id, inputs| {
            let node = parent.node_from_id_mut(node_id);

            match &node.content {
                NodeContent::Text(text) => compute_leaf_layout(
                    inputs,
                    &node.layout_state.style,
                    |_val, _basis| 0.0,
                    |_known_dimensions, _available_space| taffy::Size {
                        width: unicode_column_width(&text.content, None) as f32,
                        height: 1.,
                    },
                ),
                NodeContent::Element(_) => compute_flexbox_layout(node, u64::MAX.into(), inputs),
            }
        })
    }
}

impl<Msg> LayoutFlexboxContainer for Node<Msg> {
    type FlexboxContainerStyle<'a>
        = TaffyStyle
    where
        Self: 'a;

    type FlexboxItemStyle<'a>
        = TaffyStyle
    where
        Self: 'a;

    fn get_flexbox_container_style(&self, node_id: NodeId) -> Self::FlexboxContainerStyle<'_> {
        self.node_from_id(node_id).layout_state.style.clone()
    }

    fn get_flexbox_child_style(&self, child_node_id: NodeId) -> Self::FlexboxItemStyle<'_> {
        self.node_from_id(child_node_id).layout_state.style.clone()
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
