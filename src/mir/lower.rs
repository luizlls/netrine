use crate::error::{Error, Result};
use crate::syntax::{self, Node, OperatorKind};

use super::node::{Binary, Block, Instruction, Integer, Number, Operator, Unary, VariableId};

struct Lower {
    var_id: u32,
    block: Block,
}

impl Lower {
    fn new() -> Lower {
        Lower {
            var_id: 0,
            block: Block::new(),
        }
    }

    fn variable(&mut self) -> VariableId {
        let var_id = self.var_id;
        self.var_id += 1;
        VariableId::new(var_id)
    }

    fn emit(&mut self, instruction: Instruction) {
        self.block.instructions.push(instruction);
    }

    fn node(&mut self, node: &Node) -> Result<VariableId> {
        match node {
            Node::Binary(node) => self.binary(node),
            Node::Unary(node) => self.unary(node),
            Node::Group(group) => self.node(&group.inner),
            Node::Number(literal) => self.number(literal),
            Node::Integer(literal) => self.integer(literal),
        }
    }

    fn binary(&mut self, binary: &syntax::Binary) -> Result<VariableId> {
        let loperand = self.node(&binary.lexpr)?;
        let roperand = self.node(&binary.rexpr)?;
        let target = self.variable();

        self.emit(Instruction::Binary(
            Binary {
                target,
                operator: self.operator(binary.operator),
                loperand,
                roperand,
            }
        ));

        Ok(target)
    }

    fn unary(&mut self, unary: &syntax::Unary) -> Result<VariableId> {
        let operand = self.node(&unary.expr)?;
        let target = self.variable();

        self.emit(Instruction::Unary(
            Unary {
                target,
                operator: self.operator(unary.operator),
                operand,
            }
        ));

        Ok(target)
    }

    fn number(&mut self, number: &syntax::Literal) -> Result<VariableId> {
        let Ok(value) = str::parse(&number.value) else {
            return Err(Error::new(
                "value is not supported as a number".to_string(),
                number.span,
            ));
        };

        let target = self.variable();

        self.emit(Instruction::Number(
            Number {
                target,
                value,
            }
        ));

        Ok(target)
    }

    fn integer(&mut self, integer: &syntax::Literal) -> Result<VariableId> {
        let value = match &integer.value.get(0..2) {
            Some("0b") => i64::from_str_radix(&integer.value[2..], 2),
            Some("0x") => i64::from_str_radix(&integer.value[2..], 16),
            _ => str::parse(&integer.value),
        };

        let Ok(value) = value else {
            return Err(Error::new(
                "value is not supported as an integer".to_string(),
                integer.span,
            ));
        };

        let target = self.variable();

        self.emit(Instruction::Integer(
            Integer {
                target,
                value,
            }
        ));

        Ok(target)
    }

    fn operator(&self, operator: syntax::Operator) -> Operator {
        match operator.kind {
            OperatorKind::Pos => Operator::Pos,
            OperatorKind::Neg => Operator::Neg,
            OperatorKind::Add => Operator::Add,
            OperatorKind::Sub => Operator::Sub,
            OperatorKind::Mul => Operator::Mul,
            OperatorKind::Div => Operator::Div,
            OperatorKind::Mod => Operator::Mod,
            OperatorKind::Exp => Operator::Exp,
            OperatorKind::Eq => Operator::Eq,
            OperatorKind::Ne => Operator::Ne,
            OperatorKind::Lt => Operator::Lt,
            OperatorKind::Le => Operator::Le,
            OperatorKind::Gt => Operator::Gt,
            OperatorKind::Ge => Operator::Ge,
            OperatorKind::And => Operator::And,
            OperatorKind::Or => Operator::Or,
            OperatorKind::Not => Operator::Not,
        }
    }
}

pub fn lower(node: &Node) -> Result<Block> {
    let mut lower = Lower::new();
    lower.node(node)?;
    Ok(lower.block)
}
