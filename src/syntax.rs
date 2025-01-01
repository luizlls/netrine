use crate::ast;
use crate::error::Result;
use crate::lexer;
use crate::parser;
use crate::span::Span;

pub struct SyntaxData {
    pub span: Span,
}

impl SyntaxData {
    pub fn new(span: Span) -> SyntaxData {
        SyntaxData { span }
    }
}

pub type Node = ast::Node<SyntaxData>;
pub type NodeKind = ast::NodeKind<SyntaxData>;
pub type Unary = ast::Unary<SyntaxData>;
pub type Binary = ast::Binary<SyntaxData>;
pub type Group = ast::Group<SyntaxData>;

pub fn parse(source: &str) -> Result<Vec<Node>> {
    let tokens = lexer::tokens(source);
    let module = parser::parse(source, tokens);
    module
}
