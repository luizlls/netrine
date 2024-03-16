use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub enum Node {
    Def(Box<Def>),

    Get(Box<Get>),

    Lambda(Box<Lambda>),
    
    Apply(Box<Apply>),

    Unary(Box<Unary>),

    Binary(Box<Binary>),

    Block(Box<Block>),

    Group(Box<Group>),

    Array(Box<Array>),

    Tuple(Box<Tuple>),
    
    Name(Name),
    
    String(Literal),
    
    Number(Literal),
    
    Integer(Literal),

    Import(Import),
}

impl Node {
    pub fn span(&self) -> Span {
        match self {
            Node::Def(node) => node.span,
            Node::Get(node) => node.span,
            Node::Lambda(node) => node.span,
            Node::Apply(node) => node.span,
            Node::Unary(node) => node.span,
            Node::Binary(node) => node.span,
            Node::Block(node) => node.span,
            Node::Group(node) => node.span,
            Node::Array(node) => node.span,
            Node::Tuple(node) => node.span,
            Node::Name(node) => node.span,
            Node::String(node) => node.span,
            Node::Number(node) => node.span,
            Node::Integer(node) => node.span,
            Node::Import(node) => node.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Def {
    pub lvalue: Node,
    pub rvalue: Node,
    pub span: Span,
}

impl Node {
    pub fn def(lvalue: Node, rvalue: Node) -> Node {
        let span = lvalue.span().to(rvalue.span());
        Node::Def(Def { lvalue, rvalue, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub nodes: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn block(nodes: Vec<Node>, span: Span) -> Node {
        Node::Block(Block { nodes, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Get {
    pub node: Node,
    pub field: Node,
    pub span: Span,
}

impl Node {
    pub fn get(node: Node, field: Node) -> Node {
        let span = node.span().to(field.span());
        Node::Get(Get { node, field, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub callee: Node,
    pub arguments: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn apply(callee: Node, arguments: Vec<Node>, span: Span) -> Node {
        Node::Apply(Apply { callee, arguments, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub parameters: Vec<Node>,
    pub body: Block,
    pub span: Span,
}

impl Node {
    pub fn lambda(parameters: Vec<Node>, body: Block, span: Span) -> Node {
        Node::Lambda(Lambda { parameters, body, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
    pub span: Span,
}

impl Node {
    pub fn unary(operator: Operator, expr: Node) -> Node {
        let span = operator.span.to(expr.span());
        Node::Unary(Unary { operator, expr, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: Operator,
    pub lexpr: Node,
    pub rexpr: Node,
    pub span: Span,
}

impl Node {
    pub fn binary(operator: Operator, lexpr: Node, rexpr: Node) -> Node {
        let span = lexpr.span().to(rexpr.span());
        Node::Binary(Binary { operator, lexpr, rexpr, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Group {
    pub inner: Node,
    pub span: Span,
}

impl Node {
    pub fn group(mut items: Vec<Node>, span: Span) -> Node {
        let inner = items.pop().unwrap();
        Node::Group(Group { inner, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub items: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn tuple(items: Vec<Node>, span: Span) -> Node {
        Node::Tuple(Tuple { items, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub items: Vec<Node>,
    pub span: Span,
}

impl Node {
    pub fn array(items: Vec<Node>, span: Span) -> Node {
        Node::Array(Array { items, span }.into())
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

impl Node {
    pub fn literal(kind: fn(Literal) -> Node, value: String, span: Span) -> Node {
        kind(Literal { value, span })
    }
}

pub type Name = Literal;

#[derive(Debug, Clone)]
pub struct Import {
    pub module: Name,
    pub names: Vec<Name>,
    pub span: Span,
}

impl Node {
    pub fn import(module: Name, names: Vec<Name>, span: Span) -> Node {
        Node::Import(Import { module, names, span })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Operator {
    pub kind: OperatorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum OperatorKind {
    Is,    // is
    In,    // in
    And,   // and
    Or,    // or
    Not,   // not
    Pos,   // +
    Neg,   // -
    Add,   // +
    Sub,   // -
    Mul,   // *
    Div,   // /
    Mod,   // %
    Exp,   // ^
    Eq,    // ==
    Ne,    // !=
    Lt,    // <
    Le,    // <=
    Gt,    // >
    Ge,    // >=
    Range, // ..

    // Assignment
    Sets, // :=
    Adds, // +
    Subs, // -
    Muls, // *
    Divs, // /
    Mods, // %
}

pub type Precedence = i8;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    None,
    Left,
    Right,
}

impl Operator {
    pub fn new(kind: OperatorKind, span: Span) -> Operator {
        Operator { kind, span }
    }

    pub fn is_unary(self) -> bool {
        matches!(
            self.kind,
            OperatorKind::Pos | OperatorKind::Neg | OperatorKind::Not
        )
    }

    pub fn precedence(self) -> Precedence {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => -1,
            OperatorKind::Exp => 9,
            OperatorKind::Mul | OperatorKind::Div => 8,
            OperatorKind::Add | OperatorKind::Sub => 7,
            OperatorKind::Mod => 6,
            OperatorKind::Lt
          | OperatorKind::Le
          | OperatorKind::Gt
          | OperatorKind::Ge
          | OperatorKind::Ne
          | OperatorKind::Eq => 5,
            OperatorKind::Is | OperatorKind::In => 4,
            OperatorKind::And => 3,
            OperatorKind::Or => 2,
            OperatorKind::Range => 1,
            OperatorKind::Sets
          | OperatorKind::Adds
          | OperatorKind::Subs
          | OperatorKind::Muls
          | OperatorKind::Divs
          | OperatorKind::Mods => 0,
        }
    }

    pub fn associativity(self) -> Associativity {
        match self.kind {
            OperatorKind::Pos
          | OperatorKind::Neg
          | OperatorKind::Not => Associativity::None,
            OperatorKind::Exp => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}
