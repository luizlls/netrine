use crate::collections::IndexVec;
use crate::macros::entity_id;
use crate::source::Span;
use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Syntax {
    pub nodes: IndexVec<NodeIndex, Node>,
    pub sizes: IndexVec<NodeIndex, u32>,
    pub tokens: IndexVec<TokenIndex, Token>,
    pub spans: IndexVec<TokenIndex, Span>,
}

impl Syntax {
    pub fn new() -> Syntax {
        Syntax {
            nodes: IndexVec::new(),
            sizes: IndexVec::new(),
            tokens: IndexVec::new(),
            spans: IndexVec::new(),
        }
    }

    pub fn push_node(&mut self, node: Node, size: u32) -> NodeIndex {
        let index = self.nodes.push(node);
        self.sizes.insert(index, size);
        index
    }

    pub fn push_token(&mut self, token: Token, span: Span) -> TokenIndex {
        let index = self.tokens.push(token);
        self.spans.insert(index, span);
        index
    }

    pub fn token_index(&self) -> TokenIndex {
        self.tokens.index()
    }

    pub fn node_index(&self) -> NodeIndex {
        self.nodes.index()
    }

    pub fn resize(&mut self, index: NodeIndex, size: u32) {
        self.sizes[index] = size;
    }

    pub fn replace(&mut self, index: NodeIndex, kind: NodeKind) {
        let node = self.nodes[index];
        self.nodes[index] = Node { kind, ..node };
    }
}

entity_id!(TokenIndex, u32);
entity_id!(NodeIndex, u32);

#[derive(Debug, Clone, Copy)]
pub struct Node {
    pub kind: NodeKind,
    pub token: TokenIndex,
}

impl Node {
    pub fn new(kind: NodeKind, token: TokenIndex) -> Node {
        Node { kind, token }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    Identifier,
    Name,
    LetInit,
    LetEnd,
    FnInit,
    FnEnd,
    ParameterInit,
    ParameterEnd,
    GroupInit,
    GroupEnd,
    Unary(Operator),
    Binary(Operator),
    Number,
    Integer,
    True,
    False,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    And, // and
    Or,  // or
    Not, // not
    Pos, // +
    Neg, // -
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // ^
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
}

pub type Precedence = u8;

impl Operator {
    #[rustfmt::skip]
    pub fn precedence(self) -> Precedence {
        match self {
            Operator::Pos
          | Operator::Neg
          | Operator::Not => 0,
            Operator::Pow => 7,
            Operator::Mul
          | Operator::Div => 6,
            Operator::Add
          | Operator::Sub => 5,
            Operator::Mod => 4,
            Operator::Lt
          | Operator::Le
          | Operator::Gt
          | Operator::Ge
          | Operator::Ne
          | Operator::Eq => 3,
            Operator::And => 2,
            Operator::Or => 1,
        }
    }
}
