use crate::error::{Error, Result};
use crate::mir::*;
use crate::syntax::{self, Node, OperatorKind};

struct Lower {
    var_id: u32,
}

impl Lower {
    fn new() -> Lower {
        Lower {
            var_id: 0,
        }
    }

    fn variable(&mut self) -> VariableId {
        let var_id = self.var_id;
        self.var_id += 1;
        VariableId::new(var_id)
    }
}

pub fn lower(syntax_node: &Node) -> Result<Block> {
    let mut lower = Lower::new();
    let mut block = Block::new();

    node(&mut lower, &mut block, syntax_node)?;
    
    Ok(block)
}

fn node(l: &mut Lower, block: &mut Block, syntax_node: &Node) -> Result<VariableId> {
    match syntax_node {
        Node::Binary(node) => binary(l, block, node),
        Node::Unary(node) => unary(l, block, node),
        Node::Group(group) => node(l, block, &group.inner),
        Node::Number(literal) => number(l, block, literal),
        Node::Integer(literal) => integer(l, block, literal),
    }
}

fn binary(l: &mut Lower, block: &mut Block, binary: &syntax::Binary) -> Result<VariableId> {
    let operator = operator(binary.operator);
    let loperand = node(l, block, &binary.lexpr)?;
    let roperand = node(l, block, &binary.rexpr)?;
    let dest = l.variable();

    block.emit(Instruction::Binary(
        Binary {
            dest,
            operator,
            loperand: Operand::Variable(loperand),
            roperand: Operand::Variable(roperand),
        }
    ));

    Ok(dest)
}

fn unary(l: &mut Lower, block: &mut Block, unary: &syntax::Unary) -> Result<VariableId> {
    let operator = operator(unary.operator);
    let operand = node(l, block, &unary.expr)?;
    let dest = l.variable();

    block.emit(Instruction::Unary(
        Unary {
            dest,
            operator,
            operand: Operand::Variable(operand),
        }
    ));

    Ok(dest)
}

fn number(l: &mut Lower, block: &mut Block, number: &syntax::Literal) -> Result<VariableId> {
    let Ok(value) = str::parse(&number.value)
    else {
        return Err(Error::new("value is not supported as a number".to_string(), number.span));
    };

    let dest = l.variable();

    block.emit(Instruction::Number(
        Number {
            dest,
            value,
        }
    ));

    Ok(dest)
}

fn integer(l: &mut Lower, block: &mut Block, integer: &syntax::Literal) -> Result<VariableId> {
    let value = match &integer.value[0..2] {
        "0b" => i64::from_str_radix(&integer.value[2..], 2),
        "0x" => i64::from_str_radix(&integer.value[2..], 16),
        _ => str::parse(&integer.value),
    };

    let Ok(value) = value
    else {
        return Err(Error::new("value is not supported as an integer".to_string(), integer.span));
    };

    let dest = l.variable();

    block.emit(Instruction::Integer(
        Integer {
            dest,
            value,
        }
    ));

    Ok(dest)
}

fn operator(operator: syntax::Operator) -> Operator {
    match operator.kind {
        OperatorKind::Is    => Operator::Is,
        OperatorKind::In    => Operator::In,
        OperatorKind::And   => Operator::And,
        OperatorKind::Or    => Operator::Or,
        OperatorKind::Not   => Operator::Not,
        OperatorKind::Pos   => Operator::Pos,
        OperatorKind::Neg   => Operator::Neg,
        OperatorKind::Add   => Operator::Add,
        OperatorKind::Sub   => Operator::Sub,
        OperatorKind::Mul   => Operator::Mul,
        OperatorKind::Div   => Operator::Div,
        OperatorKind::Mod   => Operator::Mod,
        OperatorKind::Exp   => Operator::Exp,
        OperatorKind::Eq    => Operator::Eq,
        OperatorKind::Ne    => Operator::Ne,
        OperatorKind::Lt    => Operator::Lt,
        OperatorKind::Le    => Operator::Le,
        OperatorKind::Gt    => Operator::Gt,
        OperatorKind::Ge    => Operator::Ge,
        OperatorKind::Range => Operator::Range,
    }
}
