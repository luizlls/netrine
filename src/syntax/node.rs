use crate::span::Span;
use crate::error::{Error, ErrorKind, Result};

#[derive(Debug, Clone)]
pub struct Module {
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: Box<NodeKind>,
    pub span: Span,
}

impl Node {
    pub fn new(kind: NodeKind, span: Span) -> Node {
        Node {
            kind: Box::new(kind),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Function(Function),

    Def(Def),

    Get(Get),

    Set(Set),
    
    Closure(Closure),
    
    Apply(Apply),

    Unary(Unary),

    Binary(Binary),

    Block(Block),

    Array(Array<Node>),

    Tuple(Tuple<Node>),

    Name(Name),

    String(Literal),

    Number(Literal),

    Integer(Literal),

    Import(Import),
}


#[derive(Debug, Clone)]
pub struct Function {
    pub name: Name,
    pub parameters: Vec<Parameter>,
    pub value: Node,
}

impl Node {
    pub fn function(
        name: Name,
        parameters: Vec<Parameter>,
        value: Node,
        span: Span) -> Node {
        Node {
            kind: NodeKind::Function(Function {
                name,
                parameters,
                value,
            })
            .into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub patt: Patt,
    pub value: Option<Node>,
    pub span: Span,
}

impl Parameter {
    pub fn new(patt: Patt, value: Option<Node>, span: Span) -> Parameter {
        Parameter { patt, value, span }
    }
}

impl TryFrom<Argument> for Parameter {
    type Error = Error;

    fn try_from(arg: Argument) -> Result<Parameter> {
        let (patt, value) = if let Some(name) = arg.name {
            (Patt::name(name), Some(arg.value))
        } else {
            (Patt::try_from(arg.value)?, None)
        };

        Ok(Parameter::new(patt, value, arg.span))
    }
}

#[derive(Debug, Clone)]
pub struct Def {
    pub patt: Patt,
    pub value: Node,
}

impl Node {
    pub fn def(patt: Patt, value: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Def(Def { patt, value }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub nodes: Vec<Node>,
}

impl Node {
    pub fn block(nodes: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Block(Block { nodes }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Get {
    pub node: Node,
    pub field: Node,
}

impl Node {
    pub fn get(node: Node, field: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Get(Get { node, field }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub value: Node,
    pub name: Option<Name>,
    pub span: Span,
}

impl Argument {
    pub fn new(value: Node, name: Option<Name>, span: Span) -> Argument {
        Argument { value, name, span }
    }
}

#[derive(Debug, Clone)]
pub struct Apply {
    pub callee: Node,
    pub arguments: Vec<Argument>,
}

impl Node {
    pub fn apply(callee: Node, arguments: Vec<Argument>, span: Span) -> Node {
        Node {
            kind: NodeKind::Apply(Apply { callee, arguments }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub node: Node,
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

impl Node {
    pub fn closure(
        node: Node,
        parameters: Vec<Parameter>,
        body: Block,
        span: Span) -> Node {
        Node {
            kind: NodeKind::Closure(Closure { node, parameters, body }).into(),
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
    pub fn unary(operator: Operator, expr: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Unary(Unary { operator, expr }).into(),
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
    pub fn binary(operator: Operator, lexpr: Node, rexpr: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Binary(Binary {
                operator,
                lexpr,
                rexpr,
            })
            .into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Set {
    pub operator: Operator,
    pub node: Node,
    pub value: Node,
}

impl Node {
    pub fn set(operator: Operator, node: Node, value: Node, span: Span) -> Node {
        Node {
            kind: NodeKind::Set(Set {
                operator,
                node,
                value,
            })
            .into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tuple<T> {
    pub items: Vec<T>,
}

impl Node {
    pub fn tuple(items: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Tuple(Tuple { items }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Array<T> {
    pub items: Vec<T>,
}

impl Node {
    pub fn array(items: Vec<Node>, span: Span) -> Node {
        Node {
            kind: NodeKind::Array(Array { items }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: String,
    pub span: Span,
}

impl Node {
    pub fn literal(kind: fn(Literal) -> NodeKind, value: String, span: Span) -> Node {
        Node {
            kind: kind(Literal { value, span }).into(),
            span,
        }
    }

    pub fn name(literal: Literal) -> Node {
        let span = literal.span;
        Node::new(NodeKind::Name(literal), span)
    }
}

pub type Name = Literal;

#[derive(Debug, Clone)]
pub struct Import {
    pub module: Name,
    pub names: Vec<Name>,
}

impl Node {
    pub fn import(module: Name, names: Vec<Name>, span: Span) -> Node {
        Node {
            kind: NodeKind::Import(Import { module, names }).into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Patt {
    pub kind: Box<PattKind>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PattKind {
    Array(Array<Patt>),

    Tuple(Tuple<Patt>),

    Name(Name),

    String(Literal),

    Number(Literal),

    Integer(Literal),
}

impl Patt {
    pub fn new(kind: PattKind, span: Span) -> Patt {
        Patt {
            kind: kind.into(),
            span,
        }
    }

    pub fn name(name: Name) -> Patt {
        let span = name.span;
        Patt {
            kind: PattKind::Name(name).into(),
            span,
        }
    }

    pub fn structural(self) -> Result<Patt> {
        if matches!(
            *self.kind,
            PattKind::String(_)
          | PattKind::Number(_)
          | PattKind::Integer(_)  
        ) {
            return Err(Error::new(ErrorKind::StructuralPattern, self.span))
        }

        Ok(self)
    }
}

impl TryFrom<Node> for Patt {
    type Error = Error;

    fn try_from(node: Node) -> Result<Patt> {
        let span = node.span;
    
        let kind = match *node.kind {
            NodeKind::Name(name) => PattKind::Name(name),
            NodeKind::Array(Array { items }) => {
                let items = items.into_iter().map(Patt::try_from).collect::<Result<Vec<_>, _>>()?;
                PattKind::Array(Array { items })
            }
            NodeKind::Tuple(Tuple { items }) => {
                let items = items.into_iter().map(Patt::try_from).collect::<Result<Vec<_>, _>>()?;
                PattKind::Tuple(Tuple { items })
            }
            NodeKind::String(literal) => PattKind::String(literal),
            NodeKind::Number(literal) => PattKind::Number(literal),
            NodeKind::Integer(literal) => PattKind::Integer(literal),
            _ => return Err(Error::new(ErrorKind::InvalidPattern, span)),
        };
    
        Ok(Patt::new(kind, span))
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
