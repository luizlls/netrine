use crate::span::Span;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Name(Name),

    Def(Box<Def>),

    Unary(Box<Unary>),

    Binary(Box<Binary>),

    Number(Literal),

    String(Literal),

    True(Span),

    False(Span),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Unary {
    pub operator: Operator,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binary {
    pub operator: Operator,
    pub left: Expr,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Not,
    Is,
    Pipe,
    Range,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            OperatorKind::And   => write!(f, "and"),
            OperatorKind::Or    => write!(f, "or"),
            OperatorKind::Not   => write!(f, "not"),
            OperatorKind::Is    => write!(f, "is"),
            OperatorKind::Add   => write!(f, "+"),
            OperatorKind::Sub   => write!(f, "-"),
            OperatorKind::Mul   => write!(f, "*"),
            OperatorKind::Div   => write!(f, "/"),
            OperatorKind::Rem   => write!(f, "%"),
            OperatorKind::Eq    => write!(f, "=="),
            OperatorKind::Ne    => write!(f, "!="),
            OperatorKind::Lt    => write!(f, "<"),
            OperatorKind::Le    => write!(f, "<="),
            OperatorKind::Gt    => write!(f, ">"),
            OperatorKind::Ge    => write!(f, ">="),
            OperatorKind::Pipe  => write!(f, "|>"),
            OperatorKind::Range => write!(f, ".."),
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Name(e) => e.span,
            Expr::Def(e) => e.span,
            Expr::Unary(e) => e.span,
            Expr::Binary(e) => e.span,
            Expr::Number(e) => e.span,
            Expr::String(e) => e.span,
            Expr::True(span) => *span,
            Expr::False(span) => *span,
        }
    }
}
