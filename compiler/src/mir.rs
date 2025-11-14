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
    pub fn get_type(&self, instruction_id: Variable) -> Type {
        self.instructions[instruction_id.id() as usize].type_
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for instruction in &self.instructions {
            if let Some(target) = instruction.target() {
                write!(f, "{target} := ")?;
            }
            writeln!(f, "{}", instruction.kind)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Variable(u32);

impl Variable {
    pub fn new(id: u32) -> Variable {
        Variable(id)
    }

    #[inline(always)]
    pub fn id(self) -> u32 {
        self.0
    }

    #[inline(always)]
    pub fn index(self) -> usize {
        self.0 as usize
    }

    fn next(self) -> Variable {
        Variable(self.0 + 1)
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
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

impl Instruction {
    fn target(&self) -> Option<Variable> {
        let target = match &self.kind {
            InstructionKind::Unary(unary) => unary.target,
            InstructionKind::Binary(binary) => binary.target,
            InstructionKind::Integer(integer) => integer.target,
            InstructionKind::Number(number) => number.target,
            InstructionKind::Boolean(boolean) => boolean.target,
            InstructionKind::ToNumber(to_number) => to_number.target,
            InstructionKind::Return(_) => return None,
        };
        Some(target)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    Unary(Unary),
    Binary(Binary),
    Integer(Integer),
    Number(Number),
    Boolean(Boolean),
    ToNumber(ToNumber),
    Return(Return),
}

impl Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            InstructionKind::Unary(unary) => write!(f, "{unary}"),
            InstructionKind::Binary(binary) => write!(f, "{binary}"),
            InstructionKind::Integer(integer) => write!(f, "{integer}"),
            InstructionKind::Number(number) => write!(f, "{number}"),
            InstructionKind::Boolean(boolean) => write!(f, "{boolean}"),
            InstructionKind::ToNumber(to_number) => write!(f, "{to_number}"),
            InstructionKind::Return(return_) => write!(f, "{return_}"),
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
        write!(f, "integer {}", self.value)
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
        write!(f, "number {}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Boolean {
    pub(crate) target: Variable,
    pub(crate) value: bool,
}

impl From<Boolean> for InstructionKind {
    fn from(boolean: Boolean) -> InstructionKind {
        InstructionKind::Boolean(boolean)
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ToNumber {
    pub(crate) source: Variable,
    pub(crate) target: Variable,
}

impl From<ToNumber> for InstructionKind {
    fn from(to_number: ToNumber) -> InstructionKind {
        InstructionKind::ToNumber(to_number)
    }
}

impl Display for ToNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "to.number {}", self.source)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub(crate) source: Option<Variable>,
}

impl From<Return> for InstructionKind {
    fn from(return_: Return) -> InstructionKind {
        InstructionKind::Return(return_)
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(source) = self.source {
            write!(f, "return {source}")
        } else {
            write!(f, "return")
        }
    }
}

#[derive(Debug)]
struct LowerHir<'mir> {
    instructions: Vec<Instruction>,
    variable: Variable,
    variables: HashMap<SymbolId, Variable>,
    state: &'mir mut State,
}

impl<'mir> LowerHir<'mir> {
    fn new(state: &'mir mut State) -> LowerHir<'mir> {
        LowerHir {
            instructions: Vec::new(),
            variable: Variable::new(0),
            variables: HashMap::new(),
            state,
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
            hir::NodeKind::Unary(unary) => self.unary(node, unary),
            hir::NodeKind::Binary(binary) => self.binary(node, binary),
            hir::NodeKind::Integer(literal) => self.integer(node, literal),
            hir::NodeKind::Number(literal) => self.number(node, literal),
            hir::NodeKind::Boolean(boolean) => self.boolean(node, boolean),
        }
    }

    fn define(&mut self, _node: &hir::Node, define: &hir::Define) -> Result<Variable> {
        let variable_id = self.lower(&define.value)?;
        self.variables.insert(define.symbol, variable_id);

        Ok(variable_id)
    }

    fn local(&mut self, node: &hir::Node, local: &hir::Local) -> Result<Variable> {
        let Some(&instruction_id) = self.variables.get(&local.symbol_id) else {
            let symbol = self
                .state
                .symbol_by_id(local.symbol_id)
                .expect("invalid symbol id");

            return self.fail(node.span, format!("variable `{}` not defined", symbol.name));
        };

        Ok(instruction_id)
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

    fn boolean(&mut self, _node: &hir::Node, boolean: &hir::Boolean) -> Result<Variable> {
        let target = self.variable();

        self.emit(
            Boolean {
                target,
                value: boolean.value,
            },
            types::BOOLEAN,
        );

        Ok(target)
    }

    fn convert(&mut self, source: Variable, node_type: Type, other_type: Type) -> Variable {
        if node_type == types::INTEGER && other_type == types::NUMBER {
            let target = self.variable();
            self.emit(ToNumber { source, target }, types::NUMBER);
            target
        } else {
            source
        }
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

    fn binary(&mut self, node: &hir::Node, binary: &hir::Binary) -> Result<Variable> {
        let loperand = self.lower(&binary.loperand)?;
        let roperand = self.lower(&binary.roperand)?;
        let loperand = self.convert(loperand, binary.loperand.type_, binary.roperand.type_);
        let roperand = self.convert(roperand, binary.roperand.type_, binary.loperand.type_);

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

    fn finish(&mut self) -> Result<()> {
        if let Some(instruction) = self.instructions.last() {
            self.emit(
                Return {
                    source: instruction.target(),
                },
                instruction.type_,
            );
        } else {
            self.emit(Return { source: None }, types::NOTHING);
        }
        Ok(())
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }
}

pub fn from_hir(module: &hir::Module, state: &mut State) -> Result<Module> {
    let mut lower = LowerHir::new(state);

    for node in &module.nodes {
        lower.lower(node)?;
    }

    lower.finish()?;

    Ok(Module {
        instructions: lower.instructions,
    })
}
