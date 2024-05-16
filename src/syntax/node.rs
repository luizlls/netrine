use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: Box<NodeKind>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Fn(Function),
    Def(Define),
    Get(Get),
    Lambda(Lambda),
    Apply(Apply),
    Unary(Unary),
    Binary(Binary),
    Block(Block),
    Group(Group),
    Array(Array),
    Tuple(Tuple),
    Name(Name),
    String(Literal),
    Number(Literal),
    Integer(Literal),
    Import(Import),
}

impl Node {
    pub fn new(kind: NodeKind, span: Span) -> Node {
        Node {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn function_like(&self) -> bool {
        matches!(
            *self.kind,
            NodeKind::Apply(Apply { callee: Node { kind: box NodeKind::Name(_), .. }, .. })
        )
    }

    pub fn pattern_like(&self) -> bool {
        matches!(
            *self.kind,
            NodeKind::Name(_)
          | NodeKind::Array(_) | NodeKind::Tuple(_)
          | NodeKind::String(_) | NodeKind::Number(_)
          | NodeKind::Integer(_))
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Node>,
    pub value: Node,
}

impl Node {
    pub fn function(name: Name, parameters: Vec<Node>, value: Node) -> Node {
        let span = name.span.to(value.span);
        Node::new(NodeKind::Fn(Function { name, parameters, value, }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Define {
    pub lvalue: Node,
    pub rvalue: Node,
}

impl Node {
    pub fn def(lvalue: Node, rvalue: Node) -> Node {
        let span = lvalue.span.to(rvalue.span);
        Node::new(NodeKind::Def(Define { lvalue, rvalue }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub nodes: Vec<Node>,
}

impl Node {
    pub fn block(nodes: Vec<Node>, span: Span) -> Node {
        Node::new(NodeKind::Block(Block { nodes }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Get {
    pub node: Node,
    pub field: Node,
}

impl Node {
    pub fn get(node: Node, field: Node) -> Node {
        let span = node.span.to(field.span);
        Node::new(NodeKind::Get(Get { node, field }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub callee: Node,
    pub arguments: Vec<Node>,
}

impl Node {
    pub fn apply(callee: Node, arguments: Vec<Node>, span: Span) -> Node {
        Node::new(NodeKind::Apply(Apply { callee, arguments }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub parameters: Vec<Node>,
    pub body: Block,
}

impl Node {
    pub fn lambda(parameters: Vec<Node>, body: Block, span: Span) -> Node {
        Node::new(NodeKind::Lambda(Lambda { parameters, body }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Operator,
    pub expr: Node,
}

impl Node {
    pub fn unary(operator: Operator, expr: Node) -> Node {
        let span = operator.span.to(expr.span);
        Node::new(NodeKind::Unary(Unary { operator, expr }), span)
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
        let span = lexpr.span.to(rexpr.span);
        Node::new(NodeKind::Binary(Binary { operator, lexpr, rexpr }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Group {
    pub inner: Node,
}

impl Node {
    pub fn group(mut items: Vec<Node>, span: Span) -> Node {
        let inner = items.pop().unwrap();
        Node::new(NodeKind::Group(Group { inner }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub items: Vec<Node>,
}

impl Node {
    pub fn tuple(items: Vec<Node>, span: Span) -> Node {
        Node::new(NodeKind::Tuple(Tuple { items }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub items: Vec<Node>,
}

impl Node {
    pub fn array(items: Vec<Node>, span: Span) -> Node {
        Node::new(NodeKind::Array(Array { items }), span)
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

impl Node {
    pub fn literal(kind: fn(Literal) -> NodeKind, value: String, span: Span) -> Node {
        Node::new(kind(Literal { value, span }), span)
    }
}

pub type Name = Literal;

#[derive(Debug, Clone)]
pub struct Import {
    pub module: Name,
    pub alias: Option<Name>,
    pub names: Vec<Name>,
}

impl Node {
    pub fn import(module: Name, names: Vec<Name>, span: Span) -> Node {
        Node::new(NodeKind::Import(Import { module, names, alias: None }), span)
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
