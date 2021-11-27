use crate::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub expressions: Vec<Expression>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expression {

    Name(Name),

    Fn(Box<Fn>),

    Def(Box<Def>),

    Set(Box<Set>),

    Get(Box<Get>),

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
    pub name: Name,
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub name: Name,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
    pub name: Name,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Get {
    pub source: Expression,
    pub value : Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Expression,
    pub arguments: Vec<Argument>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub value: Expression,
    pub name: Option<Name>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub parameters: Vec<Parameter>,
    pub expressions: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub left: Expression,
    pub right: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Operator,
    pub right: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub operator: Operator,
    pub left: Expression,
    pub right: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Partial {
    pub operator: Operator,
    pub left: Option<Expression>,
    pub right: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub pred: Expression,
    pub then: Expression,
    pub otherwise: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub values: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub values: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    pub values: Vec<(Expression, Expression)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub properties: Vec<(Name, Option<Expression>)>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Template {
    pub elements: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: Name,
    pub value: Option<Expression>,
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

impl Expression {
    pub fn is_pattern(&self) -> bool {
        matches!(self,
            Expression::Name(_)
          | Expression::List(_)
          | Expression::Dict(_)
          | Expression::String(_)
          | Expression::Number(_)
          | Expression::Variant(_)
          | Expression::True(_)
          | Expression::False(_))
    }

    pub fn span(&self) -> Span {
        match self {
            Expression::Name(e) => e.span,
            Expression::Fn(e) => e.span,
            Expression::Def(e) => e.span,
            Expression::Set(e) => e.span,
            Expression::Get(e) => e.span,
            Expression::Block(e) => e.span,
            Expression::Binary(e) => e.span,
            Expression::Unary(e) => e.span,
            Expression::Partial(e) => e.span,
            Expression::Call(e) => e.span,
            Expression::Tuple(e) => e.span,
            Expression::List(e) => e.span,
            Expression::Dict(e) => e.span,
            Expression::Record(e) => e.span,
            Expression::If(e) => e.span,
            Expression::Number(e) => e.span,
            Expression::String(e) => e.span,
            Expression::Variant(e) => e.span,
            Expression::True(span) => *span,
            Expression::False(span) => *span,
            Expression::It(span) => *span,
        }
    }
}
