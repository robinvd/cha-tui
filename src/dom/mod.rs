pub mod patch;
pub mod print;
pub mod rounding;
pub mod table;

pub use table::{TableColumn, TableColumnWidth, TableRow, table};

use std::any::Any;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

use crate::event::{LocalMouseEvent, Size};
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
use unicode_width::UnicodeWidthChar;

const TAB_WIDTH: usize = 8;

type ResizeHandler<Msg> = Rc<dyn Fn(&TaffyLayout) -> Option<Msg>>;

pub trait Renderable: fmt::Debug + 'static {
    fn patch_retained(&self, other: &mut dyn Renderable) -> RenderablePatch {
        let _ = other;
        RenderablePatch::Replace
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

    fn as_any_mut(&mut self) -> &mut dyn Any;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderablePatch {
    NoChange,
    ChangedNoLayout,
    ChangedLayout,
    Replace,
}

pub trait RenderableRef: fmt::Debug {
    fn debug_label(&self) -> &'static str {
        "leaf"
    }

    fn patch_retained(&self, retained: &mut dyn Renderable) -> RenderablePatch {
        let _ = retained;
        RenderablePatch::Replace
    }

    fn into_retained(self: Box<Self>) -> Box<dyn Renderable>;
}

impl<T: Renderable> RenderableRef for T {
    fn into_retained(self: Box<Self>) -> Box<dyn Renderable> {
        self
    }

    fn debug_label(&self) -> &'static str {
        Renderable::debug_label(self)
    }

    fn patch_retained(&self, retained: &mut dyn Renderable) -> RenderablePatch {
        Renderable::patch_retained(self, retained)
    }
}

impl RenderableRef for Box<dyn Renderable> {
    fn into_retained(self: Box<Self>) -> Box<dyn Renderable> {
        *self
    }

    fn debug_label(&self) -> &'static str {
        (**self).debug_label()
    }

    fn patch_retained(&self, retained: &mut dyn Renderable) -> RenderablePatch {
        (**self).patch_retained(retained)
    }
}

pub type RenderableNode = Box<dyn Renderable>;

pub struct RetainedNode<Msg> {
    classname: &'static str,
    id: u64,

    pub(crate) content: RetainedNodeContent<Msg>,
    pub(crate) layout_state: LayoutState,
    pub(crate) scroll_x: f32,
    pub(crate) scroll_y: f32,

    pub(crate) on_mouse: Vec<Rc<dyn Fn(LocalMouseEvent) -> Option<Msg>>>,
    pub(crate) on_resize: Option<ResizeHandler<Msg>>,
    pub(crate) pending_scroll: Option<PendingScroll<Msg>>,
}

pub(crate) struct PendingScroll<Msg> {
    pub target_hash: u64,
    pub alignment: ScrollAlignment,
    pub callback: Rc<dyn Fn(f32) -> Msg>,
}

impl<Msg> Clone for PendingScroll<Msg> {
    fn clone(&self) -> Self {
        Self {
            target_hash: self.target_hash,
            alignment: self.alignment,
            callback: self.callback.clone(),
        }
    }
}

impl<Msg> fmt::Debug for RetainedNode<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RetainedNode")
            .field("content", &self.content)
            .field("classname", &self.classname)
            .field("id", &self.id)
            .field("layout_state", &self.layout_state)
            .field("clickable", &!self.on_mouse.is_empty())
            .field("pending_scroll", &self.pending_scroll.is_some())
            .finish()
    }
}

/// Borrowed version of Node that can hold borrowed string data from the model.
/// This is the return type of view functions - they can borrow from &Model.
pub struct Node<'a, Msg> {
    classname: &'static str,
    id: u64,
    pub(crate) content: NodeContent<'a, Msg>,
    pub(crate) layout_style: TaffyStyle,
    pub(crate) scroll_x: f32,
    pub(crate) scroll_y: f32,
    pub(crate) on_mouse: Vec<Rc<dyn Fn(LocalMouseEvent) -> Option<Msg>>>,
    pub(crate) on_resize: Option<ResizeHandler<Msg>>,
    pub(crate) pending_scroll: Option<PendingScroll<Msg>>,
}

impl<'a, Msg> fmt::Debug for Node<'a, Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Node")
            .field("content", &self.content)
            .field("classname", &self.classname)
            .field("id", &self.id)
            .field("layout_style", &self.layout_style)
            .field("clickable", &!self.on_mouse.is_empty())
            .finish()
    }
}

pub enum RetainedNodeContent<Msg> {
    Element(RetainedElementNode<Msg>),
    Text(TextNode),
    Renderable(RenderableNode),
}

impl<Msg> fmt::Debug for RetainedNodeContent<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Element(e) => f.debug_tuple("Element").field(e).finish(),
            Self::Text(t) => f.debug_tuple("Text").field(t).finish(),
            Self::Renderable(l) => f.debug_tuple("Renderable").field(l).finish(),
        }
    }
}

/// Borrowed version of NodeContent that can reference data from the model
pub enum NodeContent<'a, Msg> {
    Element(ElementNode<'a, Msg>),
    Text(TextNodeRef<'a>),
    Renderable(Box<dyn RenderableRef + 'a>),
}

impl<'a, Msg> fmt::Debug for NodeContent<'a, Msg> {
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

pub struct RetainedElementNode<Msg> {
    pub kind: ElementKind,
    pub attrs: Attributes,
    pub children: Vec<RetainedNode<Msg>>,
    pub title: Option<String>,
}

impl<Msg> fmt::Debug for RetainedElementNode<Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RetainedElementNode")
            .field("kind", &self.kind)
            .field("attrs", &self.attrs)
            .field("children_len", &self.children.len())
            .field("title", &self.title)
            .finish()
    }
}

/// Borrowed version of ElementNode - children are borrowed NodeRefs
pub struct ElementNode<'a, Msg> {
    pub kind: ElementKind,
    pub attrs: Attributes,
    pub children: Vec<Node<'a, Msg>>,
    pub title: Option<String>,
}

impl<'a, Msg> ElementNode<'a, Msg> {
    pub fn new(kind: ElementKind, children: Vec<Node<'a, Msg>>) -> Self {
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

impl<'a, Msg> fmt::Debug for ElementNode<'a, Msg> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ElementNode")
            .field("kind", &self.kind)
            .field("attrs", &self.attrs)
            .field("children_len", &self.children.len())
            .field("title", &self.title)
            .finish()
    }
}

impl<'a, Msg> From<&'a RetainedElementNode<Msg>> for ElementNode<'a, Msg> {
    fn from(value: &'a RetainedElementNode<Msg>) -> Self {
        Self {
            kind: value.kind,
            attrs: value.attrs.clone(),
            children: value.children.iter().map(Into::into).collect(),
            title: value.title.clone(),
        }
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

fn text_span_display_width(span: &TextSpan) -> usize {
    span.content
        .chars()
        .map(|ch| {
            if ch == '\n' {
                return 0;
            }
            if ch == '\t' {
                return TAB_WIDTH;
            }
            UnicodeWidthChar::width(ch).unwrap_or(0).max(1)
        })
        .sum()
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextNode {
    base_style: Style,
    spans: Vec<TextSpan>,
}

/// Borrowed version of TextSpan that can hold either borrowed or owned text
#[derive(Clone, Debug, PartialEq)]
pub struct TextSpanRef<'a> {
    pub content: Cow<'a, str>,
    pub style: Style,
}

impl<'a> TextSpanRef<'a> {
    pub fn new(content: impl Into<Cow<'a, str>>, style: Style) -> Self {
        Self {
            content: content.into(),
            style,
        }
    }

    pub fn borrowed(content: &'a str, style: Style) -> Self {
        Self {
            content: Cow::Borrowed(content),
            style,
        }
    }

    pub fn owned(content: String, style: Style) -> Self {
        Self {
            content: Cow::Owned(content),
            style,
        }
    }
}

impl<'a> From<TextSpanRef<'a>> for TextSpan {
    fn from(value: TextSpanRef<'a>) -> Self {
        TextSpan {
            content: value.content.into_owned(),
            style: value.style,
        }
    }
}

impl<'a> From<&'a TextSpan> for TextSpanRef<'a> {
    fn from(value: &'a TextSpan) -> Self {
        TextSpanRef {
            content: Cow::Borrowed(&value.content),
            style: value.style,
        }
    }
}

impl<'a> PartialEq<TextSpan> for TextSpanRef<'a> {
    fn eq(&self, other: &TextSpan) -> bool {
        self.content.as_ref() == other.content.as_str() && self.style == other.style
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextNodeRef<'a> {
    base_style: Style,
    spans: Vec<TextSpanRef<'a>>,
}

impl<'a> TextNodeRef<'a> {
    pub fn new(content: impl Into<Cow<'a, str>>) -> Self {
        Self::from_span(content, Style::default())
    }

    pub fn from_span(content: impl Into<Cow<'a, str>>, style: Style) -> Self {
        Self::from_spans(vec![TextSpanRef::new(content, style)])
    }

    pub fn from_spans(spans: Vec<TextSpanRef<'a>>) -> Self {
        Self {
            spans,
            base_style: Style::default(),
        }
    }

    pub fn base_style(&self) -> &Style {
        &self.base_style
    }

    pub fn spans(&self) -> &[TextSpanRef<'a>] {
        &self.spans
    }
}

impl<'a> From<TextNodeRef<'a>> for TextNode {
    fn from(value: TextNodeRef<'a>) -> Self {
        TextNode {
            spans: value.spans.into_iter().map(Into::into).collect(),
            base_style: value.base_style,
        }
    }
}

impl<'a> From<&'a TextNode> for TextNodeRef<'a> {
    fn from(value: &'a TextNode) -> Self {
        TextNodeRef {
            spans: value.spans.iter().map(Into::into).collect(),
            base_style: value.base_style,
        }
    }
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

#[derive(Clone, Copy, Debug, PartialEq, Default)]
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
    Palette(u16),
    PaletteFg,
    PaletteBg,
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
    pub const Black: Self = Self::Palette(0);
    #[allow(non_upper_case_globals)]
    pub const Red: Self = Self::Palette(1);
    #[allow(non_upper_case_globals)]
    pub const Green: Self = Self::Palette(2);
    #[allow(non_upper_case_globals)]
    pub const Yellow: Self = Self::Palette(3);
    #[allow(non_upper_case_globals)]
    pub const Blue: Self = Self::Palette(4);
    #[allow(non_upper_case_globals)]
    pub const Magenta: Self = Self::Palette(5);
    #[allow(non_upper_case_globals)]
    pub const Cyan: Self = Self::Palette(6);
    #[allow(non_upper_case_globals)]
    pub const White: Self = Self::Palette(7);
    #[allow(non_upper_case_globals)]
    pub const BrightBlack: Self = Self::Palette(8);
    #[allow(non_upper_case_globals)]
    pub const BrightRed: Self = Self::Palette(9);
    #[allow(non_upper_case_globals)]
    pub const BrightGreen: Self = Self::Palette(10);
    #[allow(non_upper_case_globals)]
    pub const BrightYellow: Self = Self::Palette(11);
    #[allow(non_upper_case_globals)]
    pub const BrightBlue: Self = Self::Palette(12);
    #[allow(non_upper_case_globals)]
    pub const BrightMagenta: Self = Self::Palette(13);
    #[allow(non_upper_case_globals)]
    pub const BrightCyan: Self = Self::Palette(14);
    #[allow(non_upper_case_globals)]
    pub const BrightWhite: Self = Self::Palette(15);
}

pub fn text_retained<Msg>(content: impl Into<String>) -> RetainedNode<Msg> {
    RetainedNode::new(RetainedNodeContent::Text(TextNode::new(content)))
}

pub fn rich_text_retained<Msg>(spans: impl Into<Vec<TextSpan>>) -> RetainedNode<Msg> {
    RetainedNode::new(RetainedNodeContent::Text(TextNode::from_spans(
        spans.into(),
    )))
}

pub fn renderable_retained<Msg>(widget: impl Renderable + 'static) -> RetainedNode<Msg> {
    RetainedNode::new(RetainedNodeContent::Renderable(Box::new(widget)))
}

pub fn column_retained<Msg>(children: Vec<RetainedNode<Msg>>) -> RetainedNode<Msg> {
    let mut node = RetainedNode::new(RetainedNodeContent::Element(RetainedElementNode::new(
        ElementKind::Column,
        children,
    )));
    node.layout_state.style.flex_direction = FlexDirection::Column;
    node
}

pub fn row_retained<Msg>(children: Vec<RetainedNode<Msg>>) -> RetainedNode<Msg> {
    let mut node = RetainedNode::new(RetainedNodeContent::Element(RetainedElementNode::new(
        ElementKind::Row,
        children,
    )));
    node.layout_state.style.flex_direction = FlexDirection::Row;
    node
}

pub fn modal_retained<Msg>(children: Vec<RetainedNode<Msg>>) -> RetainedNode<Msg> {
    let mut element = RetainedElementNode::new(ElementKind::Modal, children);
    // Use a semi-transparent black background to dim the content behind the modal
    element.attrs.style.bg = Some(Color::rgba(0, 0, 0, 8));
    element.attrs.style.fg = Some(Color::rgba(0, 0, 0, 8));

    let mut node = RetainedNode::new(RetainedNodeContent::Element(element));
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

fn block_node_with_title<Msg>(
    title: Option<String>,
    children: Vec<RetainedNode<Msg>>,
) -> RetainedNode<Msg> {
    let mut element = RetainedElementNode::new(ElementKind::Block, children);
    element.attrs.style.border = true;
    element.title = title;
    let mut node = RetainedNode::new(RetainedNodeContent::Element(element));
    node.layout_state.style.flex_direction = FlexDirection::Column;
    node.layout_state.style.border = Rect {
        left: LengthPercentage::length(1.0),
        right: LengthPercentage::length(1.0),
        top: LengthPercentage::length(1.0),
        bottom: LengthPercentage::length(1.0),
    };
    node
}

pub fn block_retained<Msg>(children: Vec<RetainedNode<Msg>>) -> RetainedNode<Msg> {
    block_node_with_title(None, children)
}

pub fn block_with_title_retained<Msg>(
    title: impl Into<String>,
    children: Vec<RetainedNode<Msg>>,
) -> RetainedNode<Msg> {
    block_node_with_title(Some(title.into()), children)
}

// ============================================================================
// Borrowed node helpers - these return NodeRef which can borrow from the model
// ============================================================================

/// Create a text node that borrows its content from the model
pub fn text<'a, Msg>(content: impl Into<Cow<'a, str>>) -> Node<'a, Msg> {
    Node::new(NodeContent::Text(TextNodeRef::new(content)))
}

/// Create a text node with owned (formatted) content
pub fn text_owned<'a, Msg>(content: String) -> Node<'a, Msg> {
    text(Cow::Owned(content))
}

/// Create a rich text node with borrowed spans
pub fn rich_text<'a, Msg>(spans: Vec<TextSpanRef<'a>>) -> Node<'a, Msg> {
    Node::new(NodeContent::Text(TextNodeRef::from_spans(spans)))
}

pub fn renderable<'a, Msg>(widget: impl Renderable + 'static) -> Node<'a, Msg> {
    Node::new(NodeContent::Renderable(Box::new(
        widget,
    )))
}

pub fn renderable_ref<'a, Msg>(widget: impl RenderableRef + 'a) -> Node<'a, Msg> {
    Node::new(NodeContent::Renderable(Box::new(
        widget,
    )))
}

pub fn column<'a, Msg>(children: Vec<Node<'a, Msg>>) -> Node<'a, Msg> {
    Node::new(NodeContent::Element(ElementNode {
        kind: ElementKind::Column,
        attrs: Attributes::default(),
        children,
        title: None,
    }))
}

pub fn row<'a, Msg>(children: Vec<Node<'a, Msg>>) -> Node<'a, Msg> {
    Node::new(NodeContent::Element(ElementNode {
        kind: ElementKind::Row,
        attrs: Attributes::default(),
        children,
        title: None,
    }))
}

pub fn block<'a, Msg>(children: Vec<Node<'a, Msg>>) -> Node<'a, Msg> {
    let mut attrs = Attributes::default();
    attrs.style.border = true;
    Node::new(NodeContent::Element(ElementNode {
        kind: ElementKind::Block,
        attrs,
        children,
        title: None,
    }))
}

pub fn block_with_title<'a, Msg>(
    title: impl Into<String>,
    children: Vec<Node<'a, Msg>>,
) -> Node<'a, Msg> {
    let mut attrs = Attributes::default();
    attrs.style.border = true;
    Node::new(NodeContent::Element(ElementNode {
        kind: ElementKind::Block,
        attrs,
        children,
        title: Some(title.into()),
    }))
}

pub fn modal<'a, Msg>(children: Vec<Node<'a, Msg>>) -> Node<'a, Msg> {
    let mut attrs = Attributes::default();
    // Use a semi-transparent black background to dim the content behind the modal
    attrs.style.bg = Some(Color::rgba(0, 0, 0, 8));
    attrs.style.fg = Some(Color::rgba(0, 0, 0, 8));
    Node::new(NodeContent::Element(ElementNode {
        kind: ElementKind::Modal,
        attrs,
        children,
        title: None,
    }))
}

impl<'a, Msg> Node<'a, Msg> {
    pub fn new(content: NodeContent<'a, Msg>) -> Self {
        let mut layout_style = TaffyStyle {
            scrollbar_width: 1.,
            ..TaffyStyle::default()
        };

        // Set flex direction based on content type
        if let NodeContent::Element(ref e) = content {
            match e.kind {
                ElementKind::Column => layout_style.flex_direction = FlexDirection::Column,
                ElementKind::Row => layout_style.flex_direction = FlexDirection::Row,
                ElementKind::Block => {
                    layout_style.flex_direction = FlexDirection::Column;
                    layout_style.border = Rect {
                        left: LengthPercentage::length(1.0),
                        right: LengthPercentage::length(1.0),
                        top: LengthPercentage::length(1.0),
                        bottom: LengthPercentage::length(1.0),
                    };
                }
                ElementKind::Modal => {
                    layout_style.flex_direction = FlexDirection::Column;
                    layout_style.align_items = Some(AlignItems::Center);
                    layout_style.justify_content = Some(JustifyContent::Center);
                    layout_style.position = Position::Absolute;
                    layout_style.inset = Rect {
                        left: LengthPercentageAuto::length(0.0),
                        right: LengthPercentageAuto::length(0.0),
                        top: LengthPercentageAuto::length(0.0),
                        bottom: LengthPercentageAuto::length(0.0),
                    };
                    layout_style.size.width = taffy::Dimension::percent(1.0);
                    layout_style.size.height = taffy::Dimension::percent(1.0);
                }
                ElementKind::Table => {}
            }
        }

        Self {
            content,
            classname: "",
            id: 0,
            layout_style,
            on_mouse: Vec::new(),
            on_resize: None,
            scroll_x: 0.0,
            scroll_y: 0.0,
            pending_scroll: None,
        }
    }

    pub fn get_debug_label(&self) -> &'static str {
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
            NodeContent::Renderable(view) => view.debug_label(),
        }
    }

    pub fn with_id(mut self, id: &'static str) -> Self {
        self.classname = id;
        self.id = crate::hash::hash_str(0, self.classname);
        self
    }

    pub fn with_id_mixin(mut self, id: &'static str, mixin: u64) -> Self {
        self.classname = id;
        self.id = crate::hash::hash_str(mixin, self.classname);
        self
    }

    pub fn on_click(mut self, handler: impl Fn() -> Msg + 'static) -> Self {
        self.on_mouse.push(Rc::new(move |e: LocalMouseEvent| {
            if e.is_single_click() {
                Some(handler())
            } else {
                None
            }
        }));
        self
    }

    pub fn on_mouse(mut self, handler: impl Fn(LocalMouseEvent) -> Option<Msg> + 'static) -> Self {
        self.on_mouse.push(Rc::new(handler));
        self
    }

    pub fn on_resize(mut self, handler: impl Fn(&TaffyLayout) -> Option<Msg> + 'static) -> Self {
        self.on_resize = Some(Rc::new(handler));
        self
    }

    pub fn with_flex_grow(mut self, val: f32) -> Self {
        self.layout_style.flex_grow = val;
        self
    }

    pub fn with_style(mut self, style: Style) -> Self {
        match &mut self.content {
            NodeContent::Element(element) => {
                element.attrs.style = style;
            }
            NodeContent::Text(text) => {
                text.base_style = style;
                for span in &mut text.spans {
                    span.style = style.merged(&span.style)
                }
            }
            NodeContent::Renderable(_leaf) => {}
        }
        self
    }

    pub fn with_scroll(mut self, y: f32) -> Self {
        self.scroll_y = if y.is_sign_negative() { 0.0 } else { y };
        self.layout_style.overflow.y = Overflow::Scroll;
        self
    }

    pub fn with_fill(mut self) -> Self {
        self.layout_style.size.height = taffy::Dimension::percent(1.);
        self.layout_style.size.width = taffy::Dimension::percent(1.);
        self
    }

    pub fn with_min_height(mut self, val: taffy::Dimension) -> Self {
        self.layout_style.min_size.height = val;
        self
    }

    pub fn with_min_width(mut self, val: taffy::Dimension) -> Self {
        self.layout_style.min_size.width = val;
        self
    }

    pub fn with_flex_basis(mut self, val: taffy::Dimension) -> Self {
        self.layout_style.flex_basis = val;
        self
    }

    pub fn with_flex_shrink(mut self, val: f32) -> Self {
        self.layout_style.flex_shrink = val;
        self
    }

    pub fn with_height(mut self, val: taffy::Dimension) -> Self {
        self.layout_style.size.height = val;
        self
    }

    pub fn with_width(mut self, val: taffy::Dimension) -> Self {
        self.layout_style.size.width = val;
        self
    }

    pub fn with_padding(mut self, val: u32) -> Self {
        self.layout_style.padding = Rect {
            top: LengthPercentage::length(val as f32),
            bottom: LengthPercentage::length(val as f32),
            left: LengthPercentage::length(val as f32),
            right: LengthPercentage::length(val as f32),
        };
        self
    }

    pub fn with_gap(mut self, column: u16, row: u16) -> Self {
        self.layout_style.gap = TaffySize {
            width: LengthPercentage::length(column as f32),
            height: LengthPercentage::length(row as f32),
        };
        self
    }

    pub fn with_align_items(mut self, val: taffy::AlignItems) -> Self {
        self.layout_style.align_items = Some(val);
        self
    }

    pub fn with_justify_content(mut self, val: taffy::AlignContent) -> Self {
        self.layout_style.justify_content = Some(val);
        self
    }

    pub fn with_overflow_y(mut self, val: Overflow) -> Self {
        self.layout_style.overflow.y = val;
        self
    }

    pub fn with_overflow_x(mut self, val: Overflow) -> Self {
        self.layout_style.overflow.x = val;
        self
    }

    pub fn with_scroll_x(mut self, x: f32) -> Self {
        self.scroll_x = if x.is_sign_negative() { 0.0 } else { x };
        self.layout_style.overflow.x = Overflow::Scroll;
        self
    }

    pub fn with_padding_2d(mut self, x: u32, y: u32) -> Self {
        self.layout_style.padding = Rect {
            top: LengthPercentage::length(y as f32),
            bottom: LengthPercentage::length(y as f32),
            left: LengthPercentage::length(x as f32),
            right: LengthPercentage::length(x as f32),
        };
        self
    }

    pub fn with_padding_bottom(mut self, count: u32) -> Self {
        self.layout_style.padding.bottom = LengthPercentage::length(count as f32);
        self
    }

    pub fn with_max_height(mut self, val: taffy::Dimension) -> Self {
        self.layout_style.max_size.height = val;
        self
    }

    pub fn with_max_width(mut self, val: taffy::Dimension) -> Self {
        self.layout_style.max_size.width = val;
        self
    }

    pub fn with_flex_wrap(mut self, val: FlexWrap) -> Self {
        self.layout_style.flex_wrap = val;
        self
    }

    pub(crate) fn with_pending_scroll(mut self, pending: PendingScroll<Msg>) -> Self {
        self.pending_scroll = Some(pending);
        self
    }

    /// Get the layout style (borrowed version of layout_state)
    pub fn layout_style(&self) -> &TaffyStyle {
        &self.layout_style
    }

    /// Convert this Node into its text content
    pub fn into_text(self) -> Option<TextNode> {
        match self.content {
            NodeContent::Text(t) => Some(t.into()),
            _ => None,
        }
    }

    /// Get a reference to the element content, if this is an element node
    pub fn as_element(&self) -> Option<&ElementNode<'a, Msg>> {
        match &self.content {
            NodeContent::Element(e) => Some(e),
            _ => None,
        }
    }

    /// Convert this Node into its element content
    pub fn into_element(self) -> Option<ElementNode<'a, Msg>> {
        match self.content {
            NodeContent::Element(e) => Some(e),
            _ => None,
        }
    }

    /// Get a reference to the text content, if this is a text node
    pub fn as_text(&self) -> Option<&TextNodeRef<'a>> {
        match &self.content {
            NodeContent::Text(t) => Some(t),
            _ => None,
        }
    }

    /// Get a reference to the renderable content, if this is a renderable node.
    pub fn as_renderable(&self) -> Option<&(dyn RenderableRef + 'a)> {
        self.as_renderable_ref()
    }

    /// Get a reference to the renderable content, if this is a renderable node
    pub fn as_renderable_ref(&self) -> Option<&(dyn RenderableRef + 'a)> {
        match &self.content {
            NodeContent::Renderable(r) => Some(r.as_ref()),
            _ => None,
        }
    }

    /// Convert this Node into its renderable content, converting to a retained renderable
    pub fn into_renderable_retained(self) -> Option<RenderableNode> {
        match self.content {
            NodeContent::Renderable(r) => Some(r.into_retained()),
            _ => None,
        }
    }

    /// Convert this Node into its renderable content.
    /// Only works for owned renderables (those implementing `Renderable`).
    pub fn into_renderable(self) -> Option<RenderableNode> {
        self.into_renderable_retained()
    }

    /// Get mouse messages from handlers on this node
    pub fn mouse_message(&self, event: LocalMouseEvent) -> Option<Vec<Msg>> {
        let mut messages = Vec::new();
        for handler in &self.on_mouse {
            if let Some(msg) = handler(event) {
                messages.push(msg);
            }
        }
        if messages.is_empty() {
            None
        } else {
            Some(messages)
        }
    }
}

/// Convert a Node to a retained Node
impl<'a, Msg> From<Node<'a, Msg>> for RetainedNode<Msg> {
    fn from(value: Node<'a, Msg>) -> Self {
        let content = match value.content {
            NodeContent::Text(t) => RetainedNodeContent::Text(t.into()),
            NodeContent::Element(e) => RetainedNodeContent::Element(RetainedElementNode {
                kind: e.kind,
                attrs: e.attrs,
                children: e.children.into_iter().map(Into::into).collect(),
                title: e.title,
            }),
            NodeContent::Renderable(r) => RetainedNodeContent::Renderable(r.into_retained()),
        };

        let mut node = RetainedNode::new(content);
        node.classname = value.classname;
        node.id = value.id;
        node.layout_state.style = value.layout_style;
        node.scroll_x = value.scroll_x;
        node.scroll_y = value.scroll_y;
        node.on_mouse = value.on_mouse;
        node.on_resize = value.on_resize;
        node.pending_scroll = value.pending_scroll;
        node
    }
}

impl<'a, Msg> From<&'a RetainedNode<Msg>> for Node<'a, Msg> {
    fn from(value: &'a RetainedNode<Msg>) -> Self {
        let content = match &value.content {
            RetainedNodeContent::Text(t) => NodeContent::Text(t.into()),
            RetainedNodeContent::Element(e) => NodeContent::Element(e.into()),
            RetainedNodeContent::Renderable(_r) => {
                // We can't actually clone a Box<dyn Renderable>, but since Renderable
                // is 'static, we can use the as_any method to check if we can downcast
                // For now, we'll create a new Renderable by cloning the inner value if possible
                // In practice, this case is rare since Renderable nodes are typically
                // created fresh in view functions
                //
                // As a workaround, we'll need to reconstruct the Renderable.
                // This is a known limitation - if you need to patch Renderable nodes,
                // the view should recreate them.
                //
                // For now, we use a workaround: the Renderable is moved into the new node
                // and the old node is consumed. This works because the caller passes
                // ownership of the old node.
                //
                // Since we can't actually clone, we'll reconstruct via a different path
                // This is handled at the call site by not using From for Renderables
                panic!(
                    "Cannot create Node from &RetainedNode containing Renderable - Renderable nodes must be created fresh in view functions"
                );
            }
        };

        Self {
            classname: value.classname,
            id: value.id,
            content,
            layout_style: value.layout_state.style.clone(),
            scroll_x: value.scroll_x,
            scroll_y: value.scroll_y,
            on_mouse: value.on_mouse.clone(),
            on_resize: value.on_resize.clone(),
            pending_scroll: value.pending_scroll.clone(),
        }
    }
}

impl<Msg> RetainedNode<Msg> {
    /// Convert a RetainedNode into a Node with a 'static lifetime.
    /// This is useful when you need to embed a component that returns RetainedNode
    /// into a Node tree.
    pub fn into_node(self) -> Node<'static, Msg> {
        let content = match self.content {
            RetainedNodeContent::Text(t) => NodeContent::Text(TextNodeRef {
                base_style: t.base_style,
                spans: t
                    .spans
                    .into_iter()
                    .map(|s| TextSpanRef {
                        content: Cow::Owned(s.content),
                        style: s.style,
                    })
                    .collect(),
            }),
            RetainedNodeContent::Element(e) => NodeContent::Element(ElementNode {
                kind: e.kind,
                attrs: e.attrs,
                children: e.children.into_iter().map(|c| c.into_node()).collect(),
                title: e.title,
            }),
            RetainedNodeContent::Renderable(r) => NodeContent::Renderable(Box::new(r)),
        };

        Node {
            classname: self.classname,
            id: self.id,
            content,
            layout_style: self.layout_state.style,
            scroll_x: self.scroll_x,
            scroll_y: self.scroll_y,
            on_mouse: self.on_mouse,
            on_resize: self.on_resize,
            pending_scroll: self.pending_scroll,
        }
    }

    pub fn new(content: RetainedNodeContent<Msg>) -> Self {
        Self {
            content,
            layout_state: LayoutState::default(),
            classname: "",
            id: 0,
            on_mouse: Vec::new(),
            on_resize: None,
            scroll_x: 0.0,
            scroll_y: 0.0,
            pending_scroll: None,
        }
    }

    pub fn get_debug_label(&self) -> &'static str {
        if !self.classname.is_empty() {
            return self.classname;
        }
        match &self.content {
            RetainedNodeContent::Element(element_node) => match element_node.kind {
                ElementKind::Column => "col",
                ElementKind::Row => "row",
                ElementKind::Block => "block",
                ElementKind::Modal => "modal",
                ElementKind::Table => "table",
            },
            RetainedNodeContent::Text(_text_node) => "text",
            RetainedNodeContent::Renderable(leaf) => leaf.debug_label(),
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
        self.on_mouse.push(Rc::new(move |e: LocalMouseEvent| {
            if e.is_single_click() {
                Some(handler())
            } else {
                None
            }
        }));
        self
    }

    pub fn on_mouse(mut self, handler: impl Fn(LocalMouseEvent) -> Option<Msg> + 'static) -> Self {
        self.on_mouse.push(Rc::new(handler));
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

    pub fn with_max_height(mut self, val: taffy::Dimension) -> Self {
        self.layout_state.style.max_size.height = val;
        self
    }

    pub fn with_max_width(mut self, val: taffy::Dimension) -> Self {
        self.layout_state.style.max_size.width = val;
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

    pub fn with_padding_bottom(mut self, count: u32) -> Self {
        self.layout_state.style.padding.bottom = LengthPercentage::length(count as f32);
        self
    }

    pub fn with_gap(mut self, column: u16, row: u16) -> Self {
        self.layout_state.style.gap = TaffySize {
            width: LengthPercentage::length(column as f32),
            height: LengthPercentage::length(row as f32),
        };
        self
    }

    pub fn with_align_items(mut self, val: taffy::AlignItems) -> Self {
        self.layout_state.style.align_items = Some(val);
        self
    }

    pub fn with_justify_content(mut self, val: taffy::AlignContent) -> Self {
        self.layout_state.style.justify_content = Some(val);
        self
    }

    pub fn with_style(mut self, style: Style) -> Self {
        match &mut self.content {
            RetainedNodeContent::Element(element) => {
                element.attrs.style = style;
            }
            RetainedNodeContent::Text(text) => {
                text.base_style = style;
                for span in &mut text.spans {
                    span.style = style.merged(&span.style)
                }
            }
            RetainedNodeContent::Renderable(_leaf) => {}
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

    pub(crate) fn take_pending_scroll(&mut self) -> Option<PendingScroll<Msg>> {
        self.pending_scroll.take()
    }

    pub fn layout_state(&self) -> &LayoutState {
        &self.layout_state
    }

    pub fn layout_state_mut(&mut self) -> &mut LayoutState {
        &mut self.layout_state
    }

    pub fn as_element(&self) -> Option<&RetainedElementNode<Msg>> {
        match &self.content {
            RetainedNodeContent::Element(element) => Some(element),
            _ => None,
        }
    }

    pub fn as_text(&self) -> Option<&TextNode> {
        match &self.content {
            RetainedNodeContent::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn as_renderable(&self) -> Option<&dyn Renderable> {
        match &self.content {
            RetainedNodeContent::Renderable(leaf) => Some(leaf.as_ref()),
            _ => None,
        }
    }

    pub fn into_element(self) -> Option<RetainedElementNode<Msg>> {
        match self.content {
            RetainedNodeContent::Element(element) => Some(element),
            _ => None,
        }
    }

    pub fn into_text(self) -> Option<TextNode> {
        match self.content {
            RetainedNodeContent::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn into_renderable(self) -> Option<RenderableNode> {
        match self.content {
            RetainedNodeContent::Renderable(leaf) => Some(leaf),
            _ => None,
        }
    }

    fn node_from_id(&self, id: NodeId) -> &Self {
        let index: u64 = id.into();
        if index == u64::MAX {
            self
        } else {
            match &self.content {
                RetainedNodeContent::Element(element_node) => {
                    &element_node.children[index as usize]
                }
                RetainedNodeContent::Text(_) => panic!("text has no children"),
                RetainedNodeContent::Renderable(_) => panic!("renderable has no children"),
            }
        }
    }

    fn node_from_id_mut(&mut self, id: NodeId) -> &mut Self {
        let index: u64 = id.into();
        if index == u64::MAX {
            self
        } else {
            match &mut self.content {
                RetainedNodeContent::Element(element_node) => {
                    &mut element_node.children[index as usize]
                }
                RetainedNodeContent::Text(_) => panic!("text has no children"),
                RetainedNodeContent::Renderable(_) => panic!("renderable has no children"),
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
            RetainedNodeContent::Element(element_node) => {
                for child in &mut element_node.children {
                    child.report_changed(callback);
                }
            }
            RetainedNodeContent::Text(_) => {}
            RetainedNodeContent::Renderable(_) => {}
        }
    }

    pub(crate) fn mouse_message(&self, event: LocalMouseEvent) -> Option<Vec<Msg>> {
        let mut messages = Vec::new();
        for handler in &self.on_mouse {
            if let Some(msg) = handler(event) {
                messages.push(msg);
            }
        }
        if messages.is_empty() {
            None
        } else {
            Some(messages)
        }
    }

    pub(crate) fn hit_test<T>(
        &self,
        x: u16,
        y: u16,
        callback: &mut impl FnMut(&RetainedNode<Msg>, f32, f32) -> Option<T>,
    ) -> Option<T> {
        // info!("hit test: {:?}", Self::hit_test_inner(self, x, y, 0.0, 0.0, 0.0, callback).map(|n| n.classname));
        Self::hit_test_inner(self, x, y, 0.0, 0.0, 0.0, callback)
    }

    fn hit_test_inner<T>(
        node: &RetainedNode<Msg>,
        x: u16,
        y: u16,
        origin_x: f32,
        origin_y: f32,
        ancestor_scroll_y: f32,
        callback: &mut impl FnMut(&RetainedNode<Msg>, f32, f32) -> Option<T>,
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

        if let RetainedNodeContent::Element(element) = &node.content {
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

    pub fn with_bg(mut self, color: Color) -> Self {
        self.bg = Some(color);
        self
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

    pub fn with_fg(mut self, color: Color) -> Self {
        self.fg = Some(color);
        self
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
    /// TODO this works weird?
    ///         text::<Msg>("> ").with_style(Style::fg(Color::Green).merged(&Style::bg(Color::Palette(8))));
    /// doesnt give what i think
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

impl<Msg> RetainedElementNode<Msg> {
    pub fn new(kind: ElementKind, children: Vec<RetainedNode<Msg>>) -> Self {
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
            span.style = style;
        }
    }
}

impl<Msg> TraversePartialTree for RetainedNode<Msg> {
    type ChildIter<'a>
        = std::iter::Map<std::ops::Range<usize>, fn(usize) -> NodeId>
    where
        Self: 'a;

    fn child_ids(&self, parent_node_id: NodeId) -> Self::ChildIter<'_> {
        let node = self.node_from_id(parent_node_id);
        let count = match &node.content {
            RetainedNodeContent::Element(element_node) => element_node.children.len(),
            RetainedNodeContent::Text(_) => 0,
            RetainedNodeContent::Renderable(_) => 0,
        };
        (0..count).map(Into::into)
    }

    fn child_count(&self, parent_node_id: NodeId) -> usize {
        let node = self.node_from_id(parent_node_id);
        match &node.content {
            RetainedNodeContent::Element(element_node) => element_node.children.len(),
            RetainedNodeContent::Text(_) => 0,
            RetainedNodeContent::Renderable(_) => 0,
        }
    }

    fn get_child_id(&self, parent_node_id: NodeId, child_index: usize) -> NodeId {
        self.child_ids(parent_node_id).nth(child_index).unwrap()
    }
}

impl<Msg> CacheTree for RetainedNode<Msg> {
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

impl<Msg> LayoutPartialTree for RetainedNode<Msg> {
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
                RetainedNodeContent::Text(text) => compute_leaf_layout(
                    inputs,
                    &node.layout_state.style,
                    |_val, _basis| 0.0,
                    |_known_dimensions, _available_space| {
                        let width: usize = text.spans().iter().map(text_span_display_width).sum();
                        taffy::Size {
                            width: width as f32,
                            height: 1.,
                        }
                    },
                ),
                RetainedNodeContent::Renderable(leaf) => compute_leaf_layout(
                    inputs,
                    &node.layout_state.style,
                    |_val, _basis| 0.0,
                    |known_dimensions, available_space| {
                        leaf.measure(&node.layout_state.style, known_dimensions, available_space)
                    },
                ),
                RetainedNodeContent::Element(element) => match element.kind {
                    ElementKind::Table => compute_grid_layout(node, u64::MAX.into(), inputs),
                    _ => compute_flexbox_layout(node, u64::MAX.into(), inputs),
                },
            }
        })
    }
}

impl<Msg> LayoutFlexboxContainer for RetainedNode<Msg> {
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

impl<Msg> LayoutGridContainer for RetainedNode<Msg> {
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
        fn patch_retained(&self, other: &mut dyn Renderable) -> RenderablePatch {
            if let Some(other) = other.as_any_mut().downcast_ref::<Self>() {
                if other.label == self.label
                    && other.width == self.width
                    && other.height == self.height
                {
                    RenderablePatch::NoChange
                } else {
                    RenderablePatch::Replace
                }
            } else {
                RenderablePatch::Replace
            }
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

        fn as_any_mut(&mut self) -> &mut dyn Any {
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
            TextSpanRef::new("a", Style::fg(Color::Red)),
            TextSpanRef::new("b", Style::fg(Color::Green)),
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

        let style = node.layout_style().clone();
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
        let _leaf_ref = node.as_renderable().expect("expected leaf reference");

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

        let _renderable_a = a.as_renderable().expect("expected renderable node");
        let renderable_b = b.as_renderable().expect("expected renderable node");
        let renderable_c = c.as_renderable().expect("expected renderable node");

        let mut retained_a = a.into_renderable().unwrap();

        assert_eq!(
            renderable_b.patch_retained(&mut *retained_a),
            RenderablePatch::NoChange
        );
        assert_eq!(
            renderable_c.patch_retained(&mut *retained_a),
            RenderablePatch::Replace
        );
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
        let node = text::<()>("btn").on_mouse(|_| Some(()));
        let mut node: RetainedNode<()> = node.into();

        taffy::compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: taffy::AvailableSpace::Definite(4.0),
                height: taffy::AvailableSpace::Definite(2.0),
            },
        );
        crate::dom::rounding::round_layout(&mut node);

        let messages = node
            .hit_test(0, 0, &mut |n, _, _| {
                n.mouse_message(crate::event::LocalMouseEvent::new(
                    crate::event::MouseEvent::new(
                        0,
                        0,
                        crate::event::MouseEventKind::Down(crate::event::MouseButton::Left),
                    ),
                    0,
                    0,
                ))
            })
            .expect("expected hit");
        assert_eq!(messages, vec![()]);
    }

    #[test]
    fn mouse_message_runs_multiple_handlers_in_order() {
        let node = text::<&'static str>("btn")
            .on_mouse(|_| Some("first"))
            .on_mouse(|_| Some("second"))
            .on_mouse(|_| None);

        let event = crate::event::LocalMouseEvent::new(
            crate::event::MouseEvent::new(
                0,
                0,
                crate::event::MouseEventKind::Down(crate::event::MouseButton::Left),
            ),
            0,
            0,
        );

        let messages = node.mouse_message(event);
        assert_eq!(messages, Some(vec!["first", "second"]));
    }

    #[test]
    fn with_scroll_marks_overflow_and_offset() {
        let node = column(vec![text::<()>("a"), text::<()>("b")]).with_scroll(5.0);
        assert_eq!(node.scroll_y, 5.0);
        assert_eq!(node.layout_style().overflow.y, Overflow::Scroll);
    }
}
