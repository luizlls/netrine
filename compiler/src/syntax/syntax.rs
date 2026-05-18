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
    Function(Box<Function>),
    Define(Box<Define>),
    Apply(Box<Apply>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Name(Name),
    Number,
    Integer,
    True,
    False,
}

impl Node {
    pub fn pattern_like(&self) -> bool {
        matches!(self.kind, NodeKind::Name(..))
    }
}

#[derive(Debug, Clone)]
pub struct Define {
    pub name: Name,
    pub value: Node,
    pub type_: Option<Type>,
}

impl Node {
    pub fn define(name: Name, value: Node, type_: Option<Type>) -> Node {
        let span = Span::from(name.span, value.span);
        Node {
            kind: NodeKind::Define(
                Define {
                    name,
                    value,
                    type_,
                }
                .into(),
            ),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Node,
    pub type_: Option<Type>,
}

impl Node {
    pub fn function(name: Name, parameters: Vec<Parameter>, value: Node, type_: Option<Type>) -> Node {
        let span = Span::from(name.span, value.span);
        Node {
            kind: NodeKind::Function(
                Function {
                    name,
                    parameters,
                    value,
                    type_,
                }
                .into(),
            ),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Name,
    pub span: Span,
    pub type_: Option<Type>,
}

impl Parameter {
    pub fn new(name: Name, type_: Option<Type>) -> Parameter {
        let span = if let Some(type_) = &type_ {
            Span::from(name.span, type_.span)
        } else {
            name.span
        };

        Parameter {
            name,
            span,
            type_,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub expr: Node,
    pub arguments: Vec<Node>,
}

impl Node {
    pub fn apply(expr: Node, arguments: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Apply(Apply { expr, arguments }.into()),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParameterLike {
    pub value: Node,
    pub span: Span,
    pub type_: Option<Type>,
}

impl ParameterLike {
    pub fn new(value: Node, type_: Option<Type>) -> ParameterLike {
        let span = if let Some(type_) = &type_ {
            Span::from(value.span, type_.span)
        } else {
            value.span
        };

        ParameterLike {
            value,
            span,
            type_,
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

impl Node {
    pub fn name(name: Name) -> Node {
        let span = name.span;
        Node {
            kind: NodeKind::Name(name),
            span,
        }
    }
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
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Name(Name),
}

impl Type {
    pub fn name(name: Name) -> Type {
        let span = name.span;
        Type {
            kind: TypeKind::Name(name),
            span,
        }
    }
}
