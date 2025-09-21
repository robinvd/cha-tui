use std::marker::PhantomData;

use crate::event::Size;

#[derive(Clone, Debug, PartialEq)]
pub enum Node<Msg> {
    Element(ElementNode<Msg>),
    Text(TextNode<Msg>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElementNode<Msg> {
    pub kind: ElementKind,
    pub attrs: Attributes,
    pub children: Vec<Node<Msg>>,
    _marker: PhantomData<Msg>,
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
    Node::Text(TextNode::new(content))
}

pub fn column<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    Node::Element(ElementNode::new(ElementKind::Column, children))
}

pub fn row<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    Node::Element(ElementNode::new(ElementKind::Row, children))
}

pub fn block<Msg>(children: Vec<Node<Msg>>) -> Node<Msg> {
    let mut node = ElementNode::new(ElementKind::Block, children);
    node.attrs.style.border = true;
    Node::Element(node)
}

impl<Msg> Node<Msg> {
    pub fn with_style(self, style: Style) -> Self {
        match self {
            Node::Element(mut element) => {
                element.attrs.style = style;
                Node::Element(element)
            }
            Node::Text(mut text) => {
                text.style = style;
                Node::Text(text)
            }
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
            _marker: PhantomData,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_node_preserves_content() {
        let node: Node<()> = text("hello");

        match node {
            Node::Text(text) => {
                assert_eq!(text.content, "hello");
                assert_eq!(text.style, Style::default());
            }
            _ => panic!("expected text node"),
        }
    }

    #[test]
    fn column_node_wraps_children() {
        let child: Node<()> = text("child");
        let node = column(vec![child.clone()]);

        match node {
            Node::Element(element) => {
                assert_eq!(element.kind, ElementKind::Column);
                assert_eq!(element.children, vec![child]);
                assert_eq!(element.attrs, Attributes::default());
            }
            _ => panic!("expected element node"),
        }
    }

    #[test]
    fn row_node_wraps_children() {
        let child: Node<()> = text("row child");
        let node = row(vec![child.clone()]);

        match node {
            Node::Element(element) => {
                assert_eq!(element.kind, ElementKind::Row);
                assert_eq!(element.children, vec![child]);
            }
            _ => panic!("expected element node"),
        }
    }

    #[test]
    fn block_node_sets_border() {
        let node = block::<()>(Vec::new());

        match node {
            Node::Element(element) => {
                assert!(element.attrs.style.border);
            }
            _ => panic!("expected element node"),
        }
    }

    #[test]
    fn with_style_replaces_styles() {
        let style = Style::fg(Color::Blue);
        let node = text::<()>("styled").with_style(style.clone());

        match node {
            Node::Text(text) => assert_eq!(text.style, style),
            _ => panic!("expected text node"),
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
