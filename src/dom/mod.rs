pub mod patch;
pub mod print;
pub mod rounding;
pub mod table;

pub use table::{TableColumn, TableColumnWidth, TableRow, table};

use std::any::Any;
use std::fmt;
use std::rc::Rc;

use crate::event::{MouseEvent, Size};
use crate::render::RenderContext;
use crate::scroll::ScrollAlignment;
use taffy::{
    CacheTree, LayoutFlexboxContainer, LayoutGridContainer, Overflow, compute_cached_layout,
    compute_flexbox_layout, compute_grid_layout, compute_leaf_layout,
};
use taffy::{
    LayoutPartialTree,
    geometry::{Rect, Size as TaffySize},
    style::{
        AlignItems, FlexDirection, FlexWrap, JustifyContent, LengthPercentage,
        LengthPercentageAuto, Position, Style as TaffyStyle,
    },
    tree::{Cache as TaffyCache, Layout as TaffyLayout, NodeId, TraversePartialTree},
};
use termwiz::cell::unicode_column_width;

type ResizeHandler<Msg> = Rc<dyn Fn(&TaffyLayout) -> Option<Msg>>;

pub trait Renderable: fmt::Debug + 'static {
    fn eq(&self, _other: &dyn Renderable) -> bool {
        false
    }

    fn measure(
        &self,
        style: &TaffyStyle,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
    ) -> taffy::Size<f32>;

    fn render(&self, ctx: &mut RenderContext<'_>);

    fn debug_label(&self) -> &'static str {
        "leaf"
    }

    fn as_any(&self) -> &dyn Any;
}

pub type RenderableNode = Box<dyn Renderable>;

pub struct Node<Msg> {
    classname: &'static str,
    id: u64,

    pub(crate) content: NodeContent<Msg>,
    pub(crate) layout_state: LayoutState,
    pub(crate) scroll_x: f32,
    pub(crate) scroll_y: f32,

    pub(crate) on_mouse: Option<Rc<dyn Fn(MouseEvent) -> Option<Msg>>>,
    pub(crate) on_resize: Option<ResizeHandler<Msg>>,
    pub(crate) pending_scroll: Option<PendingScroll<Msg>>,
}

#[derive(Clone)]
pub(crate) struct PendingScroll<Msg> {
    pub target_hash: u64,
    pub alignment: ScrollAlignment,
    pub callback: Rc<dyn Fn(f32) -> Msg>,
}

impl<Msg> fmt::Debug for Node<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Node")
            .field("content", &self.content)
            .field("classname", &self.classname)
            .field("id", &self.id)
            .field("layout_state", &self.layout_state)
            .field("clickable", &self.on_mouse.is_some())
            .field("pending_scroll", &self.pending_scroll.is_some())
            .finish()
    }
}

pub enum NodeContent<Msg> {
    Element(ElementNode<Msg>),
    Text(TextNode),
    Renderable(RenderableNode),
}

impl<Msg> fmt::Debug for NodeContent<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Element(e) => f.debug_tuple("Element").field(e).finish(),
            Self::Text(t) => f.debug_tuple("Text").field(t).finish(),
            Self::Renderable(l) => f.debug_tuple("Renderable").field(l).finish(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LayoutState {
    pub style: TaffyStyle,
    pub cache: TaffyCache,
    pub unrounded_layout: TaffyLayout,
    pub layout: TaffyLayout,
    size_dirty: bool,
}

impl Default for LayoutState {
    fn default() -> Self {
        let mut new = Self {
            style: TaffyStyle::default(),
            cache: TaffyCache::new(),
            unrounded_layout: TaffyLayout::new(),
            layout: TaffyLayout::new(),
            size_dirty: false,
        };
        new.style.scrollbar_width = 1.;
        new
    }
}

pub struct ElementNode<Msg> {
    pub kind: ElementKind,
    pub attrs: Attributes,
    pub children: Vec<Node<Msg>>,
    pub title: Option<String>,
}

impl<Msg> fmt::Debug for ElementNode<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ElementNode")
            .field("kind", &self.kind)
            .field("attrs", &self.attrs)
            .field("children_len", &self.children.len())
            .field("title", &self.title)
            .finish()
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
pub struct TextNode {
    base_style: Style,
    spans: Vec<TextSpan>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ElementKind {
    Column,
    Row,
    Block,
    Modal,
    Table,
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
    pub reverse: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Color {
    Reset,
    Rgba { r: u8, g: u8, b: u8, a: u8 },
}

impl Color {
    pub const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self::Rgba { r, g, b, a }
    }

    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self::Rgba { r, g, b, a: 255 }
    }

    // ANSI color constants (using standard xterm colors)
    #[allow(non_upper_case_globals)]
    pub const Black: Self = Self::rgb(0, 0, 0);
    #[allow(non_upper_case_globals)]
    pub const Red: Self = Self::rgb(205, 0, 0);
    #[allow(non_upper_case_globals)]
    pub const Green: Self = Self::rgb(0, 205, 0);
    #[allow(non_upper_case_globals)]
    pub const Yellow: Self = Self::rgb(205, 205, 0);
    #[allow(non_upper_case_globals)]
    pub const Blue: Self = Self::rgb(0, 0, 238);
    #[allow(non_upper_case_globals)]
    pub const Magenta: Self = Self::rgb(205, 0, 205);
    #[allow(non_upper_case_globals)]
    pub const Cyan: Self = Self::rgb(0, 205, 205);
    #[allow(non_upper_case_globals)]
    pub const White: Self = Self::rgb(229, 229, 229);
    #[allow(non_upper_case_globals)]
    pub const BrightBlack: Self = Self::rgb(127, 127, 127);
    #[allow(non_upper_case_globals)]
    pub const BrightRed: Self = Self::rgb(255, 0, 0);
    #[allow(non_upper_case_globals)]
    pub const BrightGreen: Self = Self::rgb(0, 255, 0);
    #[allow(non_upper_case_globals)]
    pub const BrightYellow: Self = Self::rgb(255, 255, 0);
    #[allow(non_upper_case_globals)]
    pub const BrightBlue: Self = Self::rgb(92, 92, 255);
    #[allow(non_upper_case_globals)]
    pub const BrightMagenta: Self = Self::rgb(255, 0, 255);
    #[allow(non_upper_case_globals)]
    pub const BrightCyan: Self = Self::rgb(0, 255, 255);
    #[allow(non_upper_case_globals)]
    pub const BrightWhite: Self = Self::rgb(255, 255, 255);
}

pub fn text<Msg>(content: impl Into<String>) -> Node<Msg> {
    Node::new(NodeContent::Text(TextNode::new(content)))
}

pub fn rich_text<Msg>(spans: impl Into<Vec<TextSpan>>) -> Node<Msg> {
    Node::new(NodeContent::Text(TextNode::from_spans(spans.into())))
}

pub fn renderable<Msg>(widget: impl Renderable + 'static) -> Node<Msg> {
    Node::new(NodeContent::Renderable(Box::new(widget)))
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

pub fn modal<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    let mut element = ElementNode::new(ElementKind::Modal, children);
    // Use a semi-transparent black background to dim the content behind the modal
    element.attrs.style.bg = Some(Color::rgba(0, 0, 0, 8));
    element.attrs.style.fg = Some(Color::rgba(0, 0, 0, 8));

    let mut node = Node::new(NodeContent::Element(element));
    node.layout_state.style.flex_direction = FlexDirection::Column;
    node.layout_state.style.align_items = Some(AlignItems::Center);
    node.layout_state.style.justify_content = Some(JustifyContent::Center);
    node.layout_state.style.position = Position::Absolute;
    node.layout_state.style.inset = Rect {
        left: LengthPercentageAuto::length(0.0),
        right: LengthPercentageAuto::length(0.0),
        top: LengthPercentageAuto::length(0.0),
        bottom: LengthPercentageAuto::length(0.0),
    };
    node.layout_state.style.size.width = taffy::Dimension::percent(1.0);
    node.layout_state.style.size.height = taffy::Dimension::percent(1.0);
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
            on_resize: None,
            scroll_x: 0.0,
            scroll_y: 0.0,
            pending_scroll: None,
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
                ElementKind::Modal => "modal",
                ElementKind::Table => "table",
            },
            NodeContent::Text(_text_node) => "text",
            NodeContent::Renderable(leaf) => leaf.debug_label(),
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
            if e.is_single_click() {
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

    pub fn on_resize(mut self, handler: impl Fn(&TaffyLayout) -> Option<Msg> + 'static) -> Self {
        self.on_resize = Some(Rc::new(handler));
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

    pub fn with_flex_wrap(mut self, val: FlexWrap) -> Self {
        self.layout_state.style.flex_wrap = val;
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

    pub fn with_scroll_x(mut self, x: f32) -> Self {
        self.scroll_x = if x.is_sign_negative() { 0.0 } else { x };
        self.layout_state.style.overflow.x = Overflow::Scroll;
        self
    }

    pub fn with_padding(mut self, val: u32) -> Self {
        self.layout_state.style.padding = Rect {
            top: LengthPercentage::length(val as f32),
            bottom: LengthPercentage::length(val as f32),
            left: LengthPercentage::length(val as f32),
            right: LengthPercentage::length(val as f32),
        };
        self
    }

    pub fn with_padding_2d(mut self, x: u32, y: u32) -> Self {
        self.layout_state.style.padding = Rect {
            top: LengthPercentage::length(y as f32),
            bottom: LengthPercentage::length(y as f32),
            left: LengthPercentage::length(x as f32),
            right: LengthPercentage::length(x as f32),
        };
        self
    }

    pub fn with_gap(mut self, column: u16, row: u16) -> Self {
        self.layout_state.style.gap = TaffySize {
            width: LengthPercentage::length(column as f32),
            height: LengthPercentage::length(row as f32),
        };
        self
    }

    pub fn with_style(mut self, style: Style) -> Self {
        match &mut self.content {
            NodeContent::Element(element) => {
                element.attrs.style = style;
            }
            NodeContent::Text(text) => {
                let prev = text.base_style.clone();
                text.base_style = style.clone();
                for span in &mut text.spans {
                    if span.style == prev {
                        span.style = style.clone();
                    }
                }
            }
            NodeContent::Renderable(_leaf) => {}
        }
        self
    }

    pub fn with_scroll(mut self, y: f32) -> Self {
        self.scroll_y = if y.is_sign_negative() { 0.0 } else { y };
        self.layout_state.style.overflow.y = Overflow::Scroll;
        self
    }

    pub(crate) fn hashed_id(&self) -> u64 {
        self.id
    }

    pub(crate) fn with_pending_scroll(mut self, pending: PendingScroll<Msg>) -> Self {
        self.pending_scroll = Some(pending);
        self
    }

    pub(crate) fn take_pending_scroll(&mut self) -> Option<PendingScroll<Msg>> {
        self.pending_scroll.take()
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

    pub fn as_text(&self) -> Option<&TextNode> {
        match &self.content {
            NodeContent::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn as_renderable(&self) -> Option<&dyn Renderable> {
        match &self.content {
            NodeContent::Renderable(leaf) => Some(leaf.as_ref()),
            _ => None,
        }
    }

    pub fn into_element(self) -> Option<ElementNode<Msg>> {
        match self.content {
            NodeContent::Element(element) => Some(element),
            _ => None,
        }
    }

    pub fn into_text(self) -> Option<TextNode> {
        match self.content {
            NodeContent::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn into_renderable(self) -> Option<RenderableNode> {
        match self.content {
            NodeContent::Renderable(leaf) => Some(leaf),
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
                NodeContent::Renderable(_) => panic!("renderable has no children"),
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
                NodeContent::Renderable(_) => panic!("renderable has no children"),
            }
        }
    }

    pub(crate) fn get_unrounded_layout(&self) -> TaffyLayout {
        self.layout_state.unrounded_layout
    }

    pub(crate) fn set_final_layout(&mut self, layout: &TaffyLayout) {
        if &self.layout_state.layout != layout {
            self.layout_state.layout = *layout;
            // Or only on size != newsize?
            self.layout_state.size_dirty = true
        }
    }

    pub(crate) fn report_changed(&mut self, callback: &mut impl FnMut(&Self)) {
        if self.layout_state.size_dirty {
            callback(self);
            self.layout_state.size_dirty = false;
        }

        match &mut self.content {
            NodeContent::Element(element_node) => {
                for child in &mut element_node.children {
                    child.report_changed(callback);
                }
            }
            NodeContent::Text(_) => {}
            NodeContent::Renderable(_) => {}
        }
    }

    pub(crate) fn mouse_message(&self, event: MouseEvent) -> Option<Msg> {
        if let Some(handler) = &self.on_mouse {
            return handler(event);
        }
        None
    }

    pub(crate) fn hit_test<T>(
        &self,
        x: u16,
        y: u16,
        callback: &mut impl FnMut(&Node<Msg>, f32, f32) -> Option<T>,
    ) -> Option<T> {
        // info!("hit test: {:?}", Self::hit_test_inner(self, x, y, 0.0, 0.0, 0.0, callback).map(|n| n.classname));
        Self::hit_test_inner(self, x, y, 0.0, 0.0, 0.0, callback)
    }

    fn hit_test_inner<T>(
        node: &Node<Msg>,
        x: u16,
        y: u16,
        origin_x: f32,
        origin_y: f32,
        ancestor_scroll_y: f32,
        callback: &mut impl FnMut(&Node<Msg>, f32, f32) -> Option<T>,
    ) -> Option<T> {
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
                if let Some(hit) =
                    Self::hit_test_inner(child, x, y, abs_x, abs_y, next_scroll, callback)
                {
                    return Some(hit);
                }
            }
        }

        callback(node, abs_x, abs_y)
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

    /// Overlay another style on top of this one, with the overlay taking precedence
    /// whenever it specifies a foreground/background color or enables a flag.
    pub fn apply_overlay(&mut self, overlay: &Self) {
        if let Some(fg) = overlay.fg {
            self.fg = Some(fg);
        }
        if let Some(bg) = overlay.bg {
            self.bg = Some(bg);
        }
        if overlay.bold {
            self.bold = true;
        }
        if overlay.dim {
            self.dim = true;
        }
        if overlay.border {
            self.border = true;
        }
        if overlay.reverse {
            self.reverse = true;
        }
    }

    /// Return a new style with the overlay applied on top of `self`.
    pub fn merged(mut self, overlay: &Self) -> Self {
        self.apply_overlay(overlay);
        self
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

impl TextNode {
    pub fn new(content: impl Into<String>) -> Self {
        Self::from_span(content, Style::default())
    }

    pub fn from_span(content: impl Into<String>, style: Style) -> Self {
        Self::from_spans(vec![TextSpan::new(content, style)])
    }

    pub fn from_spans(spans: Vec<TextSpan>) -> Self {
        Self {
            spans,
            base_style: Style::default(),
        }
    }

    pub fn base_style(&self) -> &Style {
        &self.base_style
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
        let count = match &node.content {
            NodeContent::Element(element_node) => element_node.children.len(),
            NodeContent::Text(_) => 0,
            NodeContent::Renderable(_) => 0,
        };
        (0..count).map(Into::into)
    }

    fn child_count(&self, parent_node_id: NodeId) -> usize {
        let node = self.node_from_id(parent_node_id);
        match &node.content {
            NodeContent::Element(element_node) => element_node.children.len(),
            NodeContent::Text(_) => 0,
            NodeContent::Renderable(_) => 0,
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
                NodeContent::Renderable(leaf) => compute_leaf_layout(
                    inputs,
                    &node.layout_state.style,
                    |_val, _basis| 0.0,
                    |known_dimensions, available_space| {
                        leaf.measure(&node.layout_state.style, known_dimensions, available_space)
                    },
                ),
                NodeContent::Element(element) => match element.kind {
                    ElementKind::Table => compute_grid_layout(node, u64::MAX.into(), inputs),
                    _ => compute_flexbox_layout(node, u64::MAX.into(), inputs),
                },
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

impl<Msg> LayoutGridContainer for Node<Msg> {
    type GridContainerStyle<'a>
        = TaffyStyle
    where
        Self: 'a;

    type GridItemStyle<'a>
        = TaffyStyle
    where
        Self: 'a;

    fn get_grid_container_style(&self, node_id: NodeId) -> Self::GridContainerStyle<'_> {
        self.node_from_id(node_id).layout_state.style.clone()
    }

    fn get_grid_child_style(&self, child_node_id: NodeId) -> Self::GridItemStyle<'_> {
        self.node_from_id(child_node_id).layout_state.style.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::render::RenderContext;
    use std::any::Any;
    use taffy::style::{AlignItems, JustifyContent, LengthPercentageAuto, Position};

    #[derive(Clone, Debug)]
    struct DummyRenderable {
        label: &'static str,
        width: f32,
        height: f32,
    }

    impl DummyRenderable {
        fn new(label: &'static str, width: f32, height: f32) -> Self {
            Self {
                label,
                width,
                height,
            }
        }
    }

    impl Renderable for DummyRenderable {
        fn eq(&self, other: &dyn Renderable) -> bool {
            other
                .as_any()
                .downcast_ref::<Self>()
                .map(|other| {
                    other.label == self.label
                        && other.width == self.width
                        && other.height == self.height
                })
                .unwrap_or(false)
        }

        fn measure(
            &self,
            _style: &TaffyStyle,
            _known_dimensions: taffy::Size<Option<f32>>,
            _available_space: taffy::Size<taffy::AvailableSpace>,
        ) -> taffy::Size<f32> {
            taffy::Size {
                width: self.width,
                height: self.height,
            }
        }

        fn render(&self, _ctx: &mut RenderContext<'_>) {}

        fn debug_label(&self) -> &'static str {
            self.label
        }

        fn as_any(&self) -> &dyn Any {
            self
        }
    }

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
        let node = column(vec![text::<()>("child")]);

        match node.as_element() {
            Some(element) => {
                assert_eq!(element.kind, ElementKind::Column);
                assert_eq!(element.children.len(), 1);
                let text_child = element.children[0].as_text().expect("expected text child");
                assert_eq!(
                    text_child.spans(),
                    &[TextSpan::new("child", Style::default())]
                );
                assert_eq!(element.attrs, Attributes::default());
            }
            None => panic!("expected element node"),
        }
    }

    #[test]
    fn row_node_wraps_children() {
        let node = row(vec![text::<()>("row child")]);

        match node.as_element() {
            Some(element) => {
                assert_eq!(element.kind, ElementKind::Row);
                assert_eq!(element.children.len(), 1);
                let text_child = element.children[0].as_text().expect("expected text child");
                assert_eq!(
                    text_child.spans(),
                    &[TextSpan::new("row child", Style::default())]
                );
            }
            None => panic!("expected element node"),
        }
    }

    #[test]
    fn modal_node_configures_overlay() {
        let node = modal(vec![text::<()>("modal child")]);

        let element = node.as_element().expect("expected element node");
        assert_eq!(element.kind, ElementKind::Modal);
        assert_eq!(element.children.len(), 1);
        let text_child = element.children[0].as_text().expect("expected text child");
        assert_eq!(
            text_child.spans(),
            &[TextSpan::new("modal child", Style::default())]
        );
        // Check that modal has a semi-transparent background instead of dim
        assert_eq!(element.attrs.style.bg, Some(Color::rgba(0, 0, 0, 8)));

        let style = node.layout_state().style.clone();
        assert_eq!(style.position, Position::Absolute);
        assert_eq!(style.align_items, Some(AlignItems::Center));
        assert_eq!(style.justify_content, Some(JustifyContent::Center));
        assert_eq!(style.inset.left, LengthPercentageAuto::length(0.0));
        assert_eq!(style.inset.right, LengthPercentageAuto::length(0.0));
        assert_eq!(style.inset.top, LengthPercentageAuto::length(0.0));
        assert_eq!(style.inset.bottom, LengthPercentageAuto::length(0.0));
        assert_eq!(style.size.width, taffy::Dimension::percent(1.0));
        assert_eq!(style.size.height, taffy::Dimension::percent(1.0));
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
    fn leaf_node_preserves_widget_access() {
        let node: Node<()> = renderable(DummyRenderable::new("dummy", 5.0, 2.0));

        assert_eq!(node.get_debug_label(), "dummy");
        let leaf_ref = node.as_renderable().expect("expected leaf reference");
        assert!(leaf_ref.eq(leaf_ref));

        let leaf_rc = node.into_renderable().expect("expected leaf rc");
        let concrete = leaf_rc
            .as_ref()
            .as_any()
            .downcast_ref::<DummyRenderable>()
            .expect("expected DummyLeaf");
        assert_eq!(concrete.width, 5.0);
        assert_eq!(concrete.height, 2.0);
    }

    #[test]
    fn leaf_partial_eq_delegates_to_widget() {
        let a: Node<()> = renderable(DummyRenderable::new("widget", 3.0, 1.0));
        let b: Node<()> = renderable(DummyRenderable::new("widget", 3.0, 1.0));
        let c: Node<()> = renderable(DummyRenderable::new("widget", 4.0, 1.0));

        let renderable_a = a.as_renderable().expect("expected renderable node");
        let renderable_b = b.as_renderable().expect("expected renderable node");
        let renderable_c = c.as_renderable().expect("expected renderable node");

        assert!(renderable_a.eq(renderable_b));
        assert!(renderable_b.eq(renderable_a));
        assert!(!renderable_a.eq(renderable_c));
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

        node.hit_test(0, 0, &mut |n, _, _| {
            n.mouse_message(crate::event::MouseEvent::new(
                0,
                0,
                crate::event::MouseButtons::new(true, false, false),
            ))
        })
        .expect("expected hit");
    }

    #[test]
    fn with_scroll_marks_overflow_and_offset() {
        let node = column(vec![text::<()>("a"), text::<()>("b")]).with_scroll(5.0);
        assert_eq!(node.scroll_y, 5.0);
        assert_eq!(node.layout_state.style.overflow.y, Overflow::Scroll);
    }
}
