pub mod print;
pub mod rounding;

use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::event::{MouseEvent, Size};
use taffy::{
    CacheTree, LayoutFlexboxContainer, Overflow, compute_cached_layout, compute_flexbox_layout,
    compute_leaf_layout,
};
use taffy::{
    LayoutPartialTree,
    geometry::Rect,
    style::{FlexDirection, LengthPercentage, Style as TaffyStyle},
    tree::{Cache as TaffyCache, Layout as TaffyLayout, NodeId, TraversePartialTree},
};
use termwiz::cell::unicode_column_width;

#[derive(Clone)]
pub struct Node<Msg> {
    pub content: NodeContent<Msg>,
    classname: &'static str,
    id: u64,
    pub layout_state: LayoutState,
    on_mouse: Option<Rc<dyn Fn(MouseEvent) -> Option<Msg>>>,
    pub(crate) scroll_y: f32,
}

impl<Msg: fmt::Debug> fmt::Debug for Node<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Node")
            .field("content", &self.content)
            .field("classname", &self.classname)
            .field("id", &self.id)
            .field("layout_state", &self.layout_state)
            .field("clickable", &self.on_mouse.is_some())
            .finish()
    }
}

impl<Msg: PartialEq> PartialEq for Node<Msg> {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
            && self.classname == other.classname
            && self.id == other.id
            && self.layout_state == other.layout_state
            && self.on_mouse.is_some() == other.on_mouse.is_some()
            && (self.scroll_y - other.scroll_y).abs() < f32::EPSILON
    }
}

#[derive(Clone)]
pub enum NodeContent<Msg> {
    Element(ElementNode<Msg>),
    Text(TextNode<Msg>),
}

impl<Msg: fmt::Debug> fmt::Debug for NodeContent<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Element(e) => f.debug_tuple("Element").field(e).finish(),
            Self::Text(t) => f.debug_tuple("Text").field(t).finish(),
        }
    }
}

impl<Msg: PartialEq> PartialEq for NodeContent<Msg> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Element(a), Self::Element(b)) => a == b,
            (Self::Text(a), Self::Text(b)) => a == b,
            _ => false,
        }
    }
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
        let mut new = Self {
            style: TaffyStyle::default(),
            cache: TaffyCache::new(),
            unrounded_layout: TaffyLayout::new(),
            layout: TaffyLayout::new(),
        };
        new.style.overflow.x = Overflow::Clip;
        new.style.overflow.y = Overflow::Clip;
        new.style.scrollbar_width = 1.;
        new
    }
}

#[derive(Clone)]
pub struct ElementNode<Msg> {
    pub kind: ElementKind,
    pub attrs: Attributes,
    pub children: Vec<Node<Msg>>,
    pub title: Option<String>,
}

impl<Msg: fmt::Debug> fmt::Debug for ElementNode<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ElementNode")
            .field("kind", &self.kind)
            .field("attrs", &self.attrs)
            .field("children_len", &self.children.len())
            .field("title", &self.title)
            .finish()
    }
}

impl<Msg: PartialEq> PartialEq for ElementNode<Msg> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
            && self.attrs == other.attrs
            && self.children == other.children
            && self.title == other.title
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextSpan {
    pub content: String,
    pub style: Style,
}

impl TextSpan {
    pub fn new(content: impl Into<String>, style: Style) -> Self {
        Self {
            content: content.into(),
            style,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextNode<Msg> {
    spans: Vec<TextSpan>,
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
    pub dim: bool,
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
    Indexed(u8),
    Rgb { r: u8, g: u8, b: u8 },
}

impl Color {
    pub const fn indexed(value: u8) -> Self {
        Self::Indexed(value)
    }

    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self::Rgb { r, g, b }
    }
}

pub fn text<Msg>(content: impl Into<String>) -> Node<Msg> {
    Node::new(NodeContent::Text(TextNode::new(content)))
}

pub fn rich_text<Msg>(spans: impl Into<Vec<TextSpan>>) -> Node<Msg> {
    Node::new(NodeContent::Text(TextNode::from_spans(spans.into())))
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

fn block_node_with_title<Msg>(title: Option<String>, children: Vec<Node<Msg>>) -> Node<Msg> {
    let mut element = ElementNode::new(ElementKind::Block, children);
    element.attrs.style.border = true;
    element.title = title;
    let mut node = Node::new(NodeContent::Element(element));
    node.layout_state.style.flex_direction = FlexDirection::Column;
    node.layout_state.style.border = Rect {
        left: LengthPercentage::length(1.0),
        right: LengthPercentage::length(1.0),
        top: LengthPercentage::length(1.0),
        bottom: LengthPercentage::length(1.0),
    };
    node
}

pub fn block<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    block_node_with_title(None, children)
}

pub fn block_with_title<Msg>(title: impl Into<String>, children: Vec<Node<Msg>>) -> Node<Msg> {
    block_node_with_title(Some(title.into()), children)
}

impl<Msg> Node<Msg> {
    pub fn new(content: NodeContent<Msg>) -> Self {
        Self {
            content,
            layout_state: LayoutState::default(),
            classname: "",
            id: 0,
            on_mouse: None,
            scroll_y: 0.0,
        }
    }

    fn get_debug_label(&self) -> &'static str {
        if !self.classname.is_empty() {
            return self.classname;
        }
        match &self.content {
            NodeContent::Element(element_node) => match element_node.kind {
                ElementKind::Column => "col",
                ElementKind::Row => "row",
                ElementKind::Block => "block",
            },
            NodeContent::Text(_text_node) => "text",
        }
    }

    fn get_final_layout(&self) -> &TaffyLayout {
        &self.layout_state.layout
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

    pub fn on_click(mut self, handler: impl Fn() -> Msg + 'static) -> Self {
        self.on_mouse = Some(Rc::new(move |e: MouseEvent| {
            if e.buttons.left {
                Some(handler())
            } else {
                None
            }
        }));
        self
    }

    pub fn on_mouse(mut self, handler: impl Fn(MouseEvent) -> Option<Msg> + 'static) -> Self {
        self.on_mouse = Some(Rc::new(handler));
        self
    }

    pub fn with_flex_grow(mut self, val: f32) -> Self {
        self.layout_state.style.flex_grow = val;
        self
    }

    pub fn with_flex_basis(mut self, val: taffy::Dimension) -> Self {
        self.layout_state.style.flex_basis = val;
        self
    }

    pub fn with_flex_shrink(mut self, val: f32) -> Self {
        self.layout_state.style.flex_shrink = val;
        self
    }

    pub fn with_height(mut self, val: taffy::Dimension) -> Self {
        self.layout_state.style.size.height = val;
        self
    }

    pub fn with_min_height(mut self, val: taffy::Dimension) -> Self {
        self.layout_state.style.min_size.height = val;
        self
    }

    pub fn with_min_width(mut self, val: taffy::Dimension) -> Self {
        self.layout_state.style.min_size.width = val;
        self
    }

    pub fn with_width(mut self, val: taffy::Dimension) -> Self {
        self.layout_state.style.size.width = val;
        self
    }

    pub fn with_fill(self) -> Self {
        self.with_height(taffy::Dimension::percent(1.))
            .with_width(taffy::Dimension::percent(1.))
    }

    pub fn with_overflow_y(mut self, val: taffy::Overflow) -> Self {
        self.layout_state.style.overflow.y = val;
        self
    }

    pub fn with_overflow_x(mut self, val: taffy::Overflow) -> Self {
        self.layout_state.style.overflow.x = val;
        self
    }

    pub fn with_style(mut self, style: Style) -> Self {
        match &mut self.content {
            NodeContent::Element(element) => {
                element.attrs.style = style;
            }
            NodeContent::Text(text) => {
                text.apply_uniform_style(style);
            }
        }
        self
    }

    pub fn with_scroll(mut self, y: f32) -> Self {
        self.scroll_y = if y.is_sign_negative() { 0.0 } else { y };
        self.layout_state.style.overflow.y = Overflow::Scroll;
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

    pub(crate) fn mouse_message(&self, event: &MouseEvent) -> Option<Msg> {
        // First see if there is a general mouse handler
        if let Some(handler) = &self.on_mouse {
            return handler(*event);
        }
        None
    }

    pub(crate) fn hit_test(&self, x: u16, y: u16) -> Option<&Self> {
        Self::hit_test_inner(self, x, y, 0.0, 0.0, 0.0)
    }

    fn hit_test_inner(
        node: &Node<Msg>,
        x: u16,
        y: u16,
        origin_x: f32,
        origin_y: f32,
        ancestor_scroll_y: f32,
    ) -> Option<&Node<Msg>> {
        let layout = node.layout_state.layout;
        let abs_x = origin_x + layout.location.x;
        let abs_y = origin_y + layout.location.y - ancestor_scroll_y;
        let width = layout.size.width;
        let height = layout.size.height;

        if width <= 0.0 || height <= 0.0 {
            return None;
        }

        let target_x = f32::from(x);
        let target_y = f32::from(y);
        if target_x < abs_x
            || target_x >= abs_x + width
            || target_y < abs_y
            || target_y >= abs_y + height
        {
            return None;
        }

        if let NodeContent::Element(element) = &node.content {
            let next_scroll = if node.layout_state.style.overflow.y == Overflow::Scroll {
                ancestor_scroll_y + node.scroll_y
            } else {
                ancestor_scroll_y
            };
            for child in &element.children {
                if let Some(hit) = Self::hit_test_inner(child, x, y, abs_x, abs_y, next_scroll) {
                    return Some(hit);
                }
            }
        }

        if node.on_mouse.is_some() {
            Some(node)
        } else {
            None
        }
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

    pub fn dim() -> Self {
        Self {
            dim: true,
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
            title: None,
        }
    }

    pub fn layout(&self, available: Size) -> Size {
        available
    }
}

impl<Msg> TextNode<Msg> {
    pub fn new(content: impl Into<String>) -> Self {
        Self::from_span(content, Style::default())
    }

    pub fn from_span(content: impl Into<String>, style: Style) -> Self {
        Self {
            spans: vec![TextSpan::new(content, style)],
            _marker: PhantomData,
        }
    }

    pub fn from_spans(spans: Vec<TextSpan>) -> Self {
        Self {
            spans,
            _marker: PhantomData,
        }
    }

    pub fn spans(&self) -> &[TextSpan] {
        &self.spans
    }

    pub fn spans_mut(&mut self) -> &mut [TextSpan] {
        &mut self.spans
    }

    pub fn push_span(&mut self, span: TextSpan) {
        self.spans.push(span);
    }

    pub fn apply_uniform_style(&mut self, style: Style) {
        for span in &mut self.spans {
            span.style = style.clone();
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
                    |_known_dimensions, _available_space| {
                        let width: usize = text
                            .spans()
                            .iter()
                            .map(|span| unicode_column_width(&span.content, None))
                            .sum();
                        taffy::Size {
                            width: width as f32,
                            height: 1.,
                        }
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
                assert_eq!(text.spans(), &[TextSpan::new("hello", Style::default())]);
            }
            None => panic!("expected text node"),
        }
    }

    #[test]
    fn rich_text_creates_multiple_spans() {
        let node: Node<()> = rich_text(vec![
            TextSpan::new("a", Style::fg(Color::Red)),
            TextSpan::new("b", Style::fg(Color::Green)),
        ]);

        let text = node.into_text().expect("expected text node");
        let spans = text.spans();
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].content, "a");
        assert_eq!(spans[0].style.fg, Some(Color::Red));
        assert_eq!(spans[1].content, "b");
        assert_eq!(spans[1].style.fg, Some(Color::Green));
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
            Some(text) => {
                assert!(text.spans().iter().all(|span| span.style == style));
            }
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

    #[test]
    fn hit_test_finds_clickable_node() {
        let mut node = text::<()>("btn").on_mouse(|_| Some(()));

        taffy::compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(4.0),
                height: taffy::AvailableSpace::Definite(2.0),
            },
        );
        crate::dom::rounding::round_layout(&mut node);

        let hit = node.hit_test(0, 0).expect("expected hit");
        assert!(
            hit.mouse_message(&crate::event::MouseEvent::new(
                0,
                0,
                crate::event::MouseButtons::new(true, false, false)
            ))
            .is_some()
        );
    }

    #[test]
    fn with_scroll_marks_overflow_and_offset() {
        let node = column(vec![text::<()>("a"), text::<()>("b")]).with_scroll(5.0);
        assert_eq!(node.scroll_y, 5.0);
        assert_eq!(node.layout_state.style.overflow.y, Overflow::Scroll);
    }
}
