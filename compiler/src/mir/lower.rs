use crate::error::Result;
use crate::semantics::{self as sema, Type};

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

    fn emit(&mut self, kind: InstructionKind, type_: Type) -> InstructionId {
        let instruction_id = InstructionId(self.instructions.len() as u32);

        self.instructions.push(Instruction {
            kind,
            type_,
            block: self.block,
        });

        instruction_id
    }

    fn node(&mut self, node: &sema::Node) -> Result<InstructionId> {
        match node {
            sema::Node::Binary(node) => self.binary(node),
            sema::Node::Unary(node) => self.unary(node),
            sema::Node::Number(literal) => self.number(literal),
            sema::Node::Integer(literal) => self.integer(literal),
        }
    }

    fn binary(&mut self, binary: &sema::Binary) -> Result<InstructionId> {
        let loperand = self.node(&binary.lexpr)?;
        let roperand = self.node(&binary.rexpr)?;

        Ok(self.emit(InstructionKind::Binary(
            Binary {
                operator: binary.operator,
                loperand,
                roperand,
            }
        ), binary.type_))
    }

    fn unary(&mut self, unary: &sema::Unary) -> Result<InstructionId> {
        let operand = self.node(&unary.expr)?;

        Ok(self.emit(InstructionKind::Unary(
            Unary {
                operator: unary.operator,
                operand,
            }
        ), unary.type_))
    }

    fn number(&mut self, number: &sema::Number) -> Result<InstructionId> {
        Ok(self.emit(InstructionKind::Number(
            Number {
                value: number.value,
            }
        ), Type::Number))
    }

    fn integer(&mut self, integer: &sema::Integer) -> Result<InstructionId> {
        Ok(self.emit(InstructionKind::Integer(
            Integer {
                value: integer.value,
            }
        ), Type::Integer))
    }
}

pub fn lower(module: &sema::Module) -> Result<Vec<Instruction>> {
    let mut lower = Lower::new();

    for node in &module.nodes {
        lower.node(node)?;
    }

    Ok(lower.instructions)
}
