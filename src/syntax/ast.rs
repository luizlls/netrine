use crate::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub expressions: Vec<Expr>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expr {

    Name(Name),

    Fn(Box<Fn>),

    Def(Box<Def>),

    Set(Box<Set>),

    Get(Box<Get>),

    Lambda(Box<Lambda>),

    Block(Box<Block>),

    Binary(Box<Binary>),

    Unary(Box<Unary>),

    Partial(Box<Partial>),

    Call(Box<Call>),
    
    Tuple(Box<Tuple>),

    List(Box<List>),

    Dict(Box<Dict>),

    Record(Box<Record>),

    If(Box<If>),

    Variant(Box<Variant>),

    Number(Literal),

    String(Literal),

    True(Span),

    False(Span),

    It(Span),
}


#[derive(Debug, Clone, PartialEq)]
pub struct Name {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub patt: Expr,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Get {
    pub source: Expr,
    pub value : Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Argument>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub value: Expr,
    pub name: Option<Name>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub parameters: Vec<Parameter>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub expressions: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Operator,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub operator: Operator,
    pub left: Expr,
    pub right: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Partial {
    pub operator: Operator,
    pub left: Option<Expr>,
    pub right: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub pred: Expr,
    pub then: Expr,
    pub otherwise: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub values: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub values: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    pub properties: Vec<(Expr, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub properties: Vec<(Name, Option<Expr>)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Template {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: Name,
    pub value: Option<Expr>,
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
    Not,
    Thread,
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
            OperatorKind::And => write!(f, "and"),
            OperatorKind::Or  => write!(f, "or"),
            OperatorKind::Not => write!(f, "not"),
            OperatorKind::Add => write!(f, "+"),
            OperatorKind::Sub => write!(f, "-"),
            OperatorKind::Mul => write!(f, "*"),
            OperatorKind::Div => write!(f, "/"),
            OperatorKind::Rem => write!(f, "%"),
            OperatorKind::Eq  => write!(f, "=="),
            OperatorKind::Ne  => write!(f, "!="),
            OperatorKind::Lt  => write!(f, "<"),
            OperatorKind::Le  => write!(f, "<="),
            OperatorKind::Gt  => write!(f, ">"),
            OperatorKind::Ge  => write!(f, ">="),
            OperatorKind::Thread => write!(f, "|>"),
            OperatorKind::Range  => write!(f, ".."),
        }
    }
}

impl Operator {
    pub fn is_unary(&self) -> bool {
        self.kind == OperatorKind::Not
    }
}

impl Expr {
    pub fn is_pattern(&self) -> bool {
        matches!(self,
            Expr::Name(_)
          | Expr::List(_)
          | Expr::Dict(_)
          | Expr::String(_)
          | Expr::Number(_)
          | Expr::Variant(_)
          | Expr::True(_)
          | Expr::False(_))
    }

    pub fn span(&self) -> Span {
        match self {
            Expr::Name(e) => e.span,
            Expr::Fn(e) => e.span,
            Expr::Def(e) => e.span,
            Expr::Set(e) => e.span,
            Expr::Get(e) => e.span,
            Expr::Lambda(e) => e.span,
            Expr::Block(e) => e.span,
            Expr::Binary(e) => e.span,
            Expr::Unary(e) => e.span,
            Expr::Partial(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::Tuple(e) => e.span,
            Expr::List(e) => e.span,
            Expr::Dict(e) => e.span,
            Expr::Record(e) => e.span,
            Expr::If(e) => e.span,
            Expr::Number(e) => e.span,
            Expr::String(e) => e.span,
            Expr::Variant(e) => e.span,
            Expr::True(span) => *span,
            Expr::False(span) => *span,
            Expr::It(span) => *span,
        }
    }
}
