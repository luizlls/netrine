use std::fmt::{self, Display};

use crate::error::Result;
use crate::types::Type;
use crate::{hir, types};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) instructions: Vec<Instruction>,
}

impl Module {
    pub fn get_type(&self, instruction_id: &InstructionId) -> Type {
        self.instructions[instruction_id.id() as usize].type_
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "v{idx} := {instruction}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub(crate) instructions: Vec<InstructionId>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            instructions: vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub(crate) u32);

impl BlockId {
    #[inline(always)]
    pub fn id(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub(crate) kind: InstructionKind,
    pub(crate) type_: Type,
    pub(crate) block: BlockId,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct InstructionId(pub(crate) u32);

impl InstructionId {
    #[inline(always)]
    pub fn id(self) -> u32 {
        self.0
    }
}

impl Display for InstructionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    Unary(Unary),
    Binary(Binary),
    Number(Number),
    Integer(Integer),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            InstructionKind::Unary(unary) => write!(f, "{unary}"),
            InstructionKind::Binary(binary) => write!(f, "{binary}"),
            InstructionKind::Number(number) => write!(f, "{number}"),
            InstructionKind::Integer(integer) => write!(f, "{integer}"),
        }
    }
}

pub(crate) type Operator = crate::hir::Operator;

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub(crate) operator: Operator,
    pub(crate) operand: InstructionId,
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.operator, self.operand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub(crate) operator: Operator,
    pub(crate) loperand: InstructionId,
    pub(crate) roperand: InstructionId,
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.operator, self.loperand, self.roperand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Integer {
    pub(crate) value: i64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub(crate) value: f64,
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

struct LowerHir {
    instructions: Vec<Instruction>,
    block: BlockId,
}

impl LowerHir {
    fn new() -> LowerHir {
        LowerHir {
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

    fn node(&mut self, node: &hir::Node) -> Result<InstructionId> {
        match node {
            hir::Node::Binary(node) => self.binary(node),
            hir::Node::Unary(node) => self.unary(node),
            hir::Node::Number(literal) => self.number(literal),
            hir::Node::Integer(literal) => self.integer(literal),
        }
    }

    fn binary(&mut self, binary: &hir::Binary) -> Result<InstructionId> {
        let loperand = self.node(&binary.loperand)?;
        let roperand = self.node(&binary.roperand)?;

        Ok(self.emit(
            InstructionKind::Binary(Binary {
                operator: binary.operator,
                loperand,
                roperand,
            }),
            binary.type_,
        ))
    }

    fn unary(&mut self, unary: &hir::Unary) -> Result<InstructionId> {
        let operand = self.node(&unary.operand)?;

        Ok(self.emit(
            InstructionKind::Unary(Unary {
                operator: unary.operator,
                operand,
            }),
            unary.type_,
        ))
    }

    fn number(&mut self, number: &hir::Number) -> Result<InstructionId> {
        Ok(self.emit(
            InstructionKind::Number(Number {
                value: number.value,
            }),
            types::NUMBER,
        ))
    }

    fn integer(&mut self, integer: &hir::Integer) -> Result<InstructionId> {
        Ok(self.emit(
            InstructionKind::Integer(Integer {
                value: integer.value,
            }),
            types::INTEGER,
        ))
    }
}

pub fn from_hir(module: &hir::Module) -> Result<Module> {
    let mut lower = LowerHir::new();

    for node in &module.nodes {
        lower.node(node)?;
    }

    Ok(Module {
        instructions: lower.instructions,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir;
    use crate::source::*;

    #[test]
    fn integer() {
        let module = hir::Module {
            nodes: vec![hir::Node::Integer(hir::Integer {
                value: 42,
                span: Span::new(0, 2),
            })],
        };

        let module = from_hir(&module).unwrap();

        assert_eq!(
            module.instructions,
            vec![Instruction {
                kind: InstructionKind::Integer(Integer { value: 42 }),
                block: BlockId(0),
                type_: types::INTEGER,
            }]
        )
    }

    #[test]
    fn number() {
        let module = hir::Module {
            nodes: vec![hir::Node::Number(hir::Number {
                value: 3.14,
                span: Span::new(0, 4),
            })],
        };

        let module = from_hir(&module).unwrap();

        assert_eq!(
            module.instructions,
            vec![Instruction {
                kind: InstructionKind::Number(Number { value: 3.14 }),
                block: BlockId(0),
                type_: types::NUMBER,
            }]
        )
    }

    #[test]
    fn unary() {
        // -7
        let module = hir::Module {
            nodes: vec![hir::Node::Unary(
                hir::Unary {
                    operator: hir::Operator::Neg,
                    operand: hir::Node::Integer(hir::Integer {
                        value: 7,
                        span: Span::new(1, 2),
                    }),
                    span: Span::new(0, 2),
                    type_: types::INTEGER,
                }
                .into(),
            )],
        };

        let module = from_hir(&module).unwrap();

        assert_eq!(
            module.instructions,
            vec![
                Instruction {
                    kind: InstructionKind::Integer(Integer { value: 7 }),
                    block: BlockId(0),
                    type_: types::INTEGER,
                },
                Instruction {
                    kind: InstructionKind::Unary(Unary {
                        operator: hir::Operator::Neg,
                        operand: InstructionId(0),
                    }),
                    block: BlockId(0),
                    type_: types::INTEGER,
                },
            ]
        )
    }

    #[test]
    fn binary() {
        // 1 + 2.5
        let module = hir::Module {
            nodes: vec![hir::Node::Binary(
                hir::Binary {
                    operator: hir::Operator::Add,
                    loperand: hir::Node::Integer(hir::Integer {
                        value: 1,
                        span: Span::new(0, 3),
                    }),
                    roperand: hir::Node::Number(hir::Number {
                        value: 2.5,
                        span: Span::new(4, 7),
                    }),
                    span: Span::new(0, 7),
                    type_: types::NUMBER,
                }
                .into(),
            )],
        };

        let module = from_hir(&module).unwrap();

        assert_eq!(
            module.instructions,
            vec![
                Instruction {
                    kind: InstructionKind::Integer(Integer { value: 1 }),
                    block: BlockId(0),
                    type_: types::INTEGER,
                },
                Instruction {
                    kind: InstructionKind::Number(Number { value: 2.5 }),
                    block: BlockId(0),
                    type_: types::NUMBER,
                },
                Instruction {
                    kind: InstructionKind::Binary(
                        Binary {
                            operator: hir::Operator::Add,
                            loperand: InstructionId(0),
                            roperand: InstructionId(1),
                        }
                        .into()
                    ),
                    block: BlockId(0),
                    type_: types::NUMBER,
                },
            ]
        )
    }

    #[test]
    fn unary_binary() {
        // -5 * 3
        let module = hir::Module {
            nodes: vec![hir::Node::Binary(
                hir::Binary {
                    operator: hir::Operator::Mul,
                    loperand: hir::Node::Unary(
                        hir::Unary {
                            operator: hir::Operator::Neg,
                            operand: hir::Node::Integer(hir::Integer {
                                value: 5,
                                span: Span::new(1, 2),
                            }),
                            span: Span::new(0, 2),
                            type_: types::INTEGER,
                        }
                        .into(),
                    ),
                    roperand: hir::Node::Integer(hir::Integer {
                        value: 3,
                        span: Span::new(3, 4),
                    }),
                    span: Span::new(0, 4),
                    type_: types::INTEGER,
                }
                .into(),
            )],
        };

        let module = from_hir(&module).unwrap();

        assert_eq!(
            module.instructions,
            vec![
                Instruction {
                    kind: InstructionKind::Integer(Integer { value: 5 }),
                    block: BlockId(0),
                    type_: types::INTEGER,
                },
                Instruction {
                    kind: InstructionKind::Unary(Unary {
                        operator: hir::Operator::Neg,
                        operand: InstructionId(0),
                    }),
                    block: BlockId(0),
                    type_: types::INTEGER,
                },
                Instruction {
                    kind: InstructionKind::Integer(Integer { value: 3 }),
                    block: BlockId(0),
                    type_: types::INTEGER,
                },
                Instruction {
                    kind: InstructionKind::Binary(Binary {
                        operator: hir::Operator::Mul,
                        loperand: InstructionId(1),
                        roperand: InstructionId(2),
                    }),
                    block: BlockId(0),
                    type_: types::INTEGER,
                },
            ]
        )
    }
}
