use crate::source::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

impl Node {
    pub fn new(kind: NodeKind, span: Span) -> Node {
        Node { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Define(Box<Define>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Name(Name),
    Number,
    Integer,
    True,
    False,
}

#[derive(Debug, Clone)]
pub struct Define {
    pub name: Name,
    pub value: Node,
}

impl Node {
    pub fn define(name: Name, value: Node) -> Node {
        let span = Span::from(name.span, value.span);
        Node {
            kind: NodeKind::Define(Define { name, value }.into()),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
}

impl Node {
    pub fn unary(operator: Operator, expr: Node) -> Node {
        let span = Span::from(operator.span, expr.span);
        Node {
            kind: NodeKind::Unary(Unary { operator, expr }.into()),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
}

impl Node {
    pub fn binary(operator: Operator, lexpr: Node, rexpr: Node) -> Node {
        let span = Span::from(operator.span, lexpr.span);
        Node {
            kind: NodeKind::Binary(
                Binary {
                    operator,
                    lexpr,
                    rexpr,
                }
                .into(),
            ),
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OperatorKind {
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
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => 0,
            OperatorKind::Pow => 7,
            OperatorKind::Mul
          | OperatorKind::Div => 6,
            OperatorKind::Add
          | OperatorKind::Sub => 5,
            OperatorKind::Mod => 4,
            OperatorKind::Lt
          | OperatorKind::Le
          | OperatorKind::Gt
          | OperatorKind::Ge
          | OperatorKind::Ne
          | OperatorKind::Eq => 3,
            OperatorKind::And => 2,
            OperatorKind::Or => 1,
        }
    }

    pub fn next_precedence(self) -> Precedence {
        if self.kind == OperatorKind::Pow {
            self.precedence()
        } else {
            self.precedence() + 1
        }
    }
}
