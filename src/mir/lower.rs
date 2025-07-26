use crate::error::{Error, Result};
use crate::syntax;

use super::node::*;

struct Lower {
    instructions: Vec<Instruction>,
    block: BlockId,
}

impl Lower {
    fn new() -> Lower {
        Lower {
            instructions: vec![],
            block: BlockId(0),
        }
    }

    fn emit(&mut self, kind: InstructionKind) -> InstructionId {
        let instruction_id = InstructionId(self.instructions.len() as u32);

        self.instructions.push(Instruction {
            kind,
            type_id: TYPE_UNKNOWN,
            block_id: self.block,
        });

        instruction_id
    }

    fn node(&mut self, node: &syntax::Node) -> Result<InstructionId> {
        match node {
            syntax::Node::Binary(node) => self.binary(node),
            syntax::Node::Unary(node) => self.unary(node),
            syntax::Node::Number(literal) => self.number(literal),
            syntax::Node::Integer(literal) => self.integer(literal),
        }
    }

    fn binary(&mut self, binary: &syntax::Binary) -> Result<InstructionId> {
        let loperand = self.node(&binary.lexpr)?;
        let roperand = self.node(&binary.rexpr)?;
        let operator = self.operator(binary.operator);

        Ok(self.emit(InstructionKind::Binary(
            Binary {
                operator,
                loperand,
                roperand,
            }
        )))
    }

    fn unary(&mut self, unary: &syntax::Unary) -> Result<InstructionId> {
        let operand = self.node(&unary.expr)?;
        let operator = self.operator(unary.operator);

        Ok(self.emit(InstructionKind::Unary(
            Unary {
                operator,
                operand,
            }
        )))
    }

    fn number(&mut self, number: &syntax::Literal) -> Result<InstructionId> {
        let Ok(value) = str::parse(&number.value) else {
            return Err(Error::new(
                "value is not supported as a number".to_string(),
                number.span,
            ));
        };

        Ok(self.emit(InstructionKind::Number(
            Number {
                value,
            }
        )))
    }

    fn integer(&mut self, integer: &syntax::Literal) -> Result<InstructionId> {
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

        Ok(self.emit(InstructionKind::Integer(
            Integer {
                value,
            }
        )))
    }

    fn operator(&self, operator: syntax::Operator) -> Operator {
        match operator.kind {
            syntax::OperatorKind::Pos => Operator::Pos,
            syntax::OperatorKind::Neg => Operator::Neg,
            syntax::OperatorKind::Add => Operator::Add,
            syntax::OperatorKind::Sub => Operator::Sub,
            syntax::OperatorKind::Mul => Operator::Mul,
            syntax::OperatorKind::Div => Operator::Div,
            syntax::OperatorKind::Mod => Operator::Mod,
            syntax::OperatorKind::Exp => Operator::Exp,
            syntax::OperatorKind::Eq => Operator::Eq,
            syntax::OperatorKind::Ne => Operator::Ne,
            syntax::OperatorKind::Lt => Operator::Lt,
            syntax::OperatorKind::Le => Operator::Le,
            syntax::OperatorKind::Gt => Operator::Gt,
            syntax::OperatorKind::Ge => Operator::Ge,
            syntax::OperatorKind::And => Operator::And,
            syntax::OperatorKind::Or => Operator::Or,
            syntax::OperatorKind::Not => Operator::Not,
        }
    }
}

pub fn lower(module: &syntax::Module) -> Result<Vec<Instruction>> {
    let mut lower = Lower::new();

    for node in &module.nodes {
        lower.node(node)?;
    }

    Ok(lower.instructions)
}
