use std::collections::HashMap;
use std::fmt::{self, Display};

use crate::error::{Error, Result};
use crate::hir;
use crate::source::Span;
use crate::state::{State, SymbolId};
use crate::types::{self, Type};

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) instructions: Vec<Instruction>,
}

impl Module {
    pub fn get_type(&self, instruction_id: &Variable) -> Type {
        self.instructions[instruction_id.id() as usize].type_
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "v{}: {} = {}", idx, instruction.type_, instruction.kind)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub(crate) instructions: Vec<Variable>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub(crate) kind: InstructionKind,
    pub(crate) type_: Type,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Variable {
    id: u32,
    v: u32,
}

impl Variable {
    #[inline(always)]
    pub fn id(self) -> u32 {
        self.id
    }

    fn new(id: u32) -> Variable {
        Variable { id, v: 0 }
    }

    fn next(self) -> Variable {
        Variable::new(self.id + 1)
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    Unary(Unary),
    Binary(Binary),
    Number(Number),
    Integer(Integer),
}

impl Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            InstructionKind::Unary(unary) => write!(f, "{unary}"),
            InstructionKind::Binary(binary) => write!(f, "{binary}"),
            InstructionKind::Number(number) => write!(f, "{number}"),
            InstructionKind::Integer(integer) => write!(f, "{integer}"),
        }
    }
}

pub type Operator = hir::Operator;

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub(crate) target: Variable,
    pub(crate) operator: Operator,
    pub(crate) operand: Variable,
}

impl From<Unary> for InstructionKind {
    fn from(unary: Unary) -> InstructionKind {
        InstructionKind::Unary(unary)
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.operator, self.operand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub(crate) target: Variable,
    pub(crate) operator: Operator,
    pub(crate) loperand: Variable,
    pub(crate) roperand: Variable,
}

impl From<Binary> for InstructionKind {
    fn from(binary: Binary) -> InstructionKind {
        InstructionKind::Binary(binary)
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.operator, self.loperand, self.roperand)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Integer {
    pub(crate) target: Variable,
    pub(crate) value: i64,
}

impl From<Integer> for InstructionKind {
    fn from(integer: Integer) -> InstructionKind {
        InstructionKind::Integer(integer)
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub(crate) target: Variable,
    pub(crate) value: f64,
}

impl From<Number> for InstructionKind {
    fn from(number: Number) -> InstructionKind {
        InstructionKind::Number(number)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
struct LowerHir<'mir> {
    instructions: Vec<Instruction>,
    state: &'mir State,
    variable: Variable,
}

impl<'mir> LowerHir<'mir> {
    fn new(state: &'mir State) -> LowerHir<'mir> {
        LowerHir {
            instructions: Vec::new(),
            state,
            variable: Variable::new(0),
        }
    }

    fn emit(&mut self, kind: impl Into<InstructionKind>, type_: Type) {
        let kind = kind.into();
        self.instructions.push(Instruction { kind, type_ });
    }

    fn variable(&mut self) -> Variable {
        let variable = self.variable;
        self.variable = self.variable.next();
        variable
    }

    #[rustfmt::skip]
    fn lower(&mut self, node: &hir::Node) -> Result<Variable> {
        match &node.kind {
            hir::NodeKind::Define(define) => self.define(node, define),
            hir::NodeKind::Local(local) => self.local(node, local),
            hir::NodeKind::Binary(binary) => self.binary(node, binary),
            hir::NodeKind::Unary(unary) => self.unary(node, unary),
            hir::NodeKind::Number(literal) => self.number(node, literal),
            hir::NodeKind::Integer(literal) => self.integer(node, literal),
        }
    }

    fn define(&mut self, _node: &hir::Node, define: &hir::Define) -> Result<Variable> {
        let variable_id = self.lower(&define.value)?;
        // self.environment.insert(define.symbol, variable_id);
        Ok(variable_id)
    }

    fn local(&mut self, node: &hir::Node, local: &hir::Local) -> Result<Variable> {
        // let Some(&instruction_id) = self.environment.get(&local.symbol_id) else {
        //     let symbol = self
        //         .state
        //         .symbol_by_id(local.symbol_id)
        //         .expect("invalid symbol id");

        //     return self.fail(node.span, format!("variable `{}` not defined", symbol.name));
        // };

        // Ok(instruction_id)
        todo!()
    }

    fn binary(&mut self, node: &hir::Node, binary: &hir::Binary) -> Result<Variable> {
        let loperand = self.lower(&binary.loperand)?;
        let roperand = self.lower(&binary.roperand)?;
        let target = self.variable();

        self.emit(
            Binary {
                target,
                operator: binary.operator,
                loperand,
                roperand,
            },
            node.type_,
        );

        Ok(target)
    }

    fn unary(&mut self, node: &hir::Node, unary: &hir::Unary) -> Result<Variable> {
        let operand = self.lower(&unary.operand)?;
        let target = self.variable();

        self.emit(
            Unary {
                target,
                operator: unary.operator,
                operand,
            },
            node.type_,
        );

        Ok(target)
    }

    fn number(&mut self, _node: &hir::Node, number: &hir::Number) -> Result<Variable> {
        let target = self.variable();

        self.emit(
            Number {
                target,
                value: number.value,
            },
            types::NUMBER,
        );

        Ok(target)
    }

    fn integer(&mut self, _node: &hir::Node, integer: &hir::Integer) -> Result<Variable> {
        let target = self.variable();

        self.emit(
            Integer {
                target,
                value: integer.value,
            },
            types::INTEGER,
        );

        Ok(target)
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }
}

pub fn from_hir(module: &hir::Module, state: &State) -> Result<Module> {
    let mut lower = LowerHir::new(state);

    for node in &module.nodes {
        lower.lower(node)?;
    }

    Ok(Module {
        instructions: lower.instructions,
    })
}
