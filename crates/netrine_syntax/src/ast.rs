use netrine_core::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Expr),

    Type(Type),

    Multi(Vec<Item>),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Expr(e) => e.span(),
            Item::Type(t) => t.span,
            Item::Multi(_) => {
                panic!("`multi` spans shouldn't be used")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Name(Name),

    Function(Box<Function>),

    Def(Box<Def>),

    Let(Box<Let>),

    Set(Box<Set>),

    Get(Box<Get>),

    Lambda(Box<Lambda>),

    Apply(Box<Apply>),

    Binary(Box<Binary>),

    If(Box<If>),

    Do(Box<Do>),

    Number(Literal),

    String(Literal),

    True(Span),

    False(Span),

    Variant(Box<Variant>),

    Multi(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Form {
    pub main: Expr,
    pub rest: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Option<Name>,
    pub parameters: Vec<Parameter>,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Name,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
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
pub struct Let {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
    pub left: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Get {
    pub from: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Apply {
    pub main: Expr,
    pub arguments: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Do {
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
    pub values: Vec<(Expr, Expr)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub properties: Vec<(Name, Option<Expr>)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Name,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: Name,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Operator {
    pub kind: OperatorKind,
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
    Pipe,
    Range,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            OperatorKind::And => write!(f, "and"),
            OperatorKind::Or => write!(f, "or"),
            OperatorKind::Is => write!(f, "is"),
            OperatorKind::Add => write!(f, "+"),
            OperatorKind::Sub => write!(f, "-"),
            OperatorKind::Mul => write!(f, "*"),
            OperatorKind::Div => write!(f, "/"),
            OperatorKind::Rem => write!(f, "%"),
            OperatorKind::Eq => write!(f, "=="),
            OperatorKind::Ne => write!(f, "!="),
            OperatorKind::Lt => write!(f, "<"),
            OperatorKind::Le => write!(f, "<="),
            OperatorKind::Gt => write!(f, ">"),
            OperatorKind::Ge => write!(f, ">="),
            OperatorKind::Pipe => write!(f, "|>"),
            OperatorKind::Range => write!(f, ".."),
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Name(e) => e.span,
            Expr::Function(e) => e.span,
            Expr::Let(e) => e.span,
            Expr::Def(e) => e.span,
            Expr::Set(e) => e.span,
            Expr::Get(e) => e.span,
            Expr::Lambda(e) => e.span,
            Expr::Apply(e) => e.span,
            Expr::Binary(e) => e.span,
            Expr::Do(e) => e.span,
            Expr::If(e) => e.span,
            Expr::Number(e) => e.span,
            Expr::String(e) => e.span,
            Expr::Variant(e) => e.span,
            Expr::True(span) => *span,
            Expr::False(span) => *span,
            Expr::Multi(_) => {
                panic!("`multi` spans shouldn't be used")
            }
        }
    }
}
