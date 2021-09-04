use crate::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub expressions: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub enum Expr {

    Name(Name),

    Fn(Box<Fn>),

    Def(Box<Def>),

    Mut(Box<Mut>),

    Set(Box<Set>),

    Get(Box<Get>),

    Lambda(Box<Lambda>),

    Binary(Box<Binary>),

    Unary(Box<Unary>),

    Partial(Box<Partial>),

    Call(Box<Call>),

    Block(Box<Block>),
    
    List(Box<List>),

    Seq(Box<Seq>),

    Record(Box<Record>),

    If(Box<If>),

    Variant(Box<Variant>),

    Number(Literal),

    String(Literal),

    Unit(Span),

    True(Span),

    False(Span),

    Anything(Span),
}


#[derive(Debug, Clone)]
pub struct Name {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Name,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub parameters: Vec<Parameter>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Mut {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Get {
    pub source: Expr,
    pub value : Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Argument>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub value: Expr,
    pub name: Option<Name>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub left: Expr,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Partial {
    pub operator: Operator,
    pub left: Option<Expr>,
    pub right: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub predicate: Expr,
    pub value_then: Expr,
    pub value_else: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Seq {
    pub values: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct List {
    pub values: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub properties: Vec<(Expr, Option<Expr>)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Template {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Name,
    pub values: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Is,
    Not,
    Pipe,
    Range,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            OperatorKind::And   => write!(f, "and"),
            OperatorKind::Or    => write!(f, "or"),
            OperatorKind::Is    => write!(f, "is"),
            OperatorKind::Not   => write!(f, "not"),
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

impl Operator {
    pub fn is_unary(&self) -> bool {
        self.kind == OperatorKind::Not
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Name(e) => e.span,
            Expr::Fn(e) => e.span,
            Expr::Def(e) => e.span,
            Expr::Mut(e) => e.span,
            Expr::Set(e) => e.span,
            Expr::Get(e) => e.span,
            Expr::Lambda(e) => e.span,
            Expr::Binary(e) => e.span,
            Expr::Unary(e) => e.span,
            Expr::Partial(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::Block(e) => e.span,
            Expr::List(e) => e.span,
            Expr::Seq(e) => e.span,
            Expr::Record(e) => e.span,
            Expr::If(e) => e.span,
            Expr::Number(e) => e.span,
            Expr::String(e) => e.span,
            Expr::Variant(e) => e.span,
            Expr::Unit(span) => *span,
            Expr::True(span) => *span,
            Expr::False(span) => *span,
            Expr::Anything(span) => *span,
        }
    }
}
