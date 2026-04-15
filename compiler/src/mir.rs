use crate::collections::{IndexMap, IndexVec};
use crate::error::{Error, Result};
use crate::interner::{Interner, Name};
use crate::macros::entity_id;
use crate::source::{Source, Span, WithSpan};
use crate::syntax::{self, NodeKind, Syntax};
use crate::types::{self, TypeId};

pub const ENTRYPOINT_NAME: &str = "__entrypoint";

#[derive(Debug)]
pub struct Module {
    globals: IndexMap<Name, GlobalId, Global>,
    functions: IndexMap<Name, FunctionId, Function>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            globals: IndexMap::new(),
            functions: IndexMap::new(),
        }
    }
}

entity_id!(InstructionId, u32);

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Argument(Argument),
    LoadGlobal(LoadGlobal),
    Binary(Binary),
    Unary(Unary),
    Integer(Integer),
    Number(Number),
    True,
    False,
}

#[derive(Debug, Clone, Copy)]
pub struct Argument {}

impl Instruction {
    const fn argument() -> Instruction {
        Instruction::Argument(Argument {})
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoadGlobal {
    pub global: GlobalId,
}

impl Instruction {
    const fn load_global(global: GlobalId) -> Instruction {
        Instruction::LoadGlobal(LoadGlobal { global })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    And, // and
    Or,  // or
    Not, // not
    Pos, // +
    Neg, // -
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // ^
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
}

#[derive(Debug, Clone, Copy)]
pub struct Unary {
    pub operator: Operator,
    pub operand: InstructionId,
}

impl Instruction {
    const fn unary(operator: Operator, operand: InstructionId) -> Instruction {
        Instruction::Unary(Unary {
            operator,
            operand,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Binary {
    pub operator: Operator,
    pub loperand: InstructionId,
    pub roperand: InstructionId,
}

impl Instruction {
    const fn binary(
        operator: Operator,
        loperand: InstructionId,
        roperand: InstructionId,
    ) -> Instruction {
        Instruction::Binary(Binary {
            operator,
            loperand,
            roperand,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    pub value: i64,
}

impl Instruction {
    const fn integer(value: i64) -> Instruction {
        Instruction::Integer(Integer { value })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Number {
    pub value: f64,
}

impl Instruction {
    const fn number(value: f64) -> Instruction {
        Instruction::Number(Number { value })
    }
}

entity_id!(LocalId, u32);

#[derive(Debug, Clone, Copy)]
pub struct Local {
    value: InstructionId,
    type_id: TypeId,
}

entity_id!(GlobalId, u32);

#[derive(Debug)]
pub struct GlobalSignature {
    pub name: Name,
    pub span: Span,
    pub type_id: TypeId,
}

#[derive(Debug)]
pub struct Global {
    pub signature: GlobalSignature,
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
    pub locals: IndexVec<LocalId, Local>,
}

entity_id!(FunctionId, u32);

#[derive(Debug)]
pub struct FunctionSignature {
    pub name: Name,
    pub span: Span,
    pub type_id: TypeId,
}

#[derive(Debug)]
pub struct Function {
    pub signature: FunctionSignature,
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
    pub locals: IndexVec<LocalId, Local>,
    pub parameters: u8,
}

#[derive(Debug)]
pub struct Builder {
    pub instructions: IndexVec<InstructionId, Instruction>,
    pub types: IndexVec<InstructionId, TypeId>,
    pub spans: IndexVec<InstructionId, Span>,
    pub locals: IndexMap<Name, LocalId, Local>,
    pub parameters: u8,
}

impl Builder {
    fn new() -> Builder {
        Builder {
            instructions: IndexVec::new(),
            types: IndexVec::new(),
            spans: IndexVec::new(),
            locals: IndexMap::new(),
            parameters: 0,
        }
    }

    fn local(&self, name: Name) -> Option<&Local> {
        self.locals.get_by_key(name)
    }

    fn emit(&mut self, instruction: Instruction, span: Span) -> InstructionId {
        let id = self.instructions.push(instruction);
        self.spans.insert(id, span);
        self.types.insert(id, types::UNKNOWN);
        id
    }

    fn build_function(self, signature: FunctionSignature) -> Function {
        Function {
            signature,
            instructions: self.instructions,
            types: self.types,
            spans: self.spans,
            locals: self.locals.values(),
            parameters: self.parameters,
        }
    }

    fn build_global(self, signature: GlobalSignature) -> Global {
        Global {
            signature,
            instructions: self.instructions,
            types: self.types,
            spans: self.spans,
            locals: self.locals.values(),
        }
    }
}

#[derive(Debug)]
struct Context {
    values: Vec<InstructionId>,
    names: Vec<WithSpan<Name>>,
    current: Builder,
    builders: Vec<Builder>,
}

impl Context {
    fn new() -> Context {
        Context {
            values: Vec::new(),
            names: Vec::new(),
            current: Builder::new(),
            builders: Vec::new(),
        }
    }

    fn at_top_level(&self) -> bool {
        self.builders.is_empty()
    }

    fn push_name(&mut self, name: Name, span: Span) {
        self.names.push(WithSpan::new(name, span));
    }

    fn pop_name(&mut self) -> WithSpan<Name> {
        self.names.pop().expect("mir: missing name in the context stack")
    }

    fn push_value(&mut self, value: InstructionId) {
        self.values.push(value);
    }

    fn pop_value(&mut self) -> InstructionId {
        self.values.pop().expect("mir: missing value in the context stack")
    }

    fn local(&self, name: Name) -> Option<&Local> {
        self.current.local(name)
    }

    fn insert_parameter(&mut self, name: Name, value: InstructionId) -> LocalId {
        self.current.parameters += 1;
        self.insert_local(
            name,
            Local {
                value,
                type_id: types::UNKNOWN,
            },
        )
    }

    fn insert_local(&mut self, name: Name, local: Local) -> LocalId {
        self.current.locals.insert(name, local)
    }

    fn emit(&mut self, instruction: Instruction, span: Span) -> InstructionId {
        let id = self.current.emit(instruction, span);
        self.push_value(id);
        id
    }

    fn push_builder(&mut self) {
        let current = std::mem::replace(&mut self.current, Builder::new());
        self.builders.push(current);
    }

    fn pop_builder(&mut self) -> Builder {
        std::mem::replace(&mut self.current, self.builders.pop().unwrap_or_else(Builder::new))
    }
}

#[derive(Debug)]
struct LowerSyntax<'mir> {
    globals: IndexMap<Name, GlobalId, Global>,
    functions: IndexMap<Name, FunctionId, Function>,
    context: Context,
    interner: &'mir mut Interner,
    source: &'mir Source,
    syntax: &'mir Syntax,
}

impl<'mir> LowerSyntax<'mir> {
    fn new(
        source: &'mir Source,
        interner: &'mir mut Interner,
        syntax: &'mir Syntax,
    ) -> LowerSyntax<'mir> {
        LowerSyntax {
            globals: IndexMap::new(),
            functions: IndexMap::new(),
            context: Context::new(),
            interner,
            source,
            syntax,
        }
    }

    fn lower(&mut self) -> Result<()> {
        for node in &self.syntax.nodes {
            let span = self.syntax.spans[node.token];

            match node.kind {
                NodeKind::LetInit => self.setup_definition()?,
                NodeKind::LetEnd => self.finish_definition()?,
                NodeKind::FnInit => self.setup_function()?,
                NodeKind::FnEnd => self.finish_function()?,
                NodeKind::ParameterInit => self.setup_parameter()?,
                NodeKind::ParameterEnd => self.finish_parameter()?,
                NodeKind::Identifier => self.identifier(span)?,
                NodeKind::Name => self.name(span)?,
                NodeKind::Unary(operator) => self.unary(span, operator)?,
                NodeKind::Binary(operator) => self.binary(span, operator)?,
                NodeKind::Integer => self.integer(span)?,
                NodeKind::Number => self.number(span)?,
                NodeKind::True => self.boolean(span, true)?,
                NodeKind::False => self.boolean(span, false)?,
                // group nodes do not require any explicit action, as their inner expressions will be handled automatically
                NodeKind::GroupInit => {}
                NodeKind::GroupEnd => {}
            }
        }

        Ok(())
    }

    fn setup_definition(&mut self) -> Result<()> {
        // there's nothing to setup for now
        Ok(())
    }

    fn global_definition(&mut self) -> Result<()> {
        let (name, span) = self.context.pop_name().parts();

        let global = self.context.pop_builder().build_global(GlobalSignature {
            name,
            span,
            type_id: types::UNKNOWN,
        });

        if self.globals.get(name).is_some() {
            let name = &self.interner[name];
            return self.fail(span, format!("{name} already defined"));
        }

        self.globals.insert(name, global);

        Ok(())
    }

    fn local_definition(&mut self) -> Result<()> {
        let (name, span) = self.context.pop_name().parts();
        let value = self.context.pop_value();

        let local = Local {
            value,
            type_id: types::UNKNOWN,
        };

        if self.context.local(name).is_some() {
            let name = &self.interner[name];
            return self.fail(span, format!("{name} already defined"));
        }

        self.context.insert_local(name, local);

        Ok(())
    }

    fn finish_definition(&mut self) -> Result<()> {
        if self.context.at_top_level() {
            self.global_definition()
        } else {
            self.local_definition()
        }
    }

    fn setup_function(&mut self) -> Result<()> {
        self.context.push_builder();
        Ok(())
    }

    fn finish_function(&mut self) -> Result<()> {
        let (name, span) = self.context.pop_name().parts();

        let function = self.context.pop_builder().build_function(FunctionSignature {
            name,
            span,
            type_id: types::UNKNOWN,
        });

        if self.functions.get(name).is_some() {
            let name = &self.interner[name];
            return self.fail(span, format!("{name} already defined"));
        }

        self.functions.insert(name, function);

        Ok(())
    }

    fn setup_parameter(&mut self) -> Result<()> {
        // there's nothing to setup for now
        Ok(())
    }

    fn finish_parameter(&mut self) -> Result<()> {
        let (name, span) = self.context.pop_name().parts();

        if self.context.local(name).is_some() {
            let name = &self.interner[name];
            return self.fail(span, format!("Parameter {name} already defined"));
        }

        let value = self.context.emit(Instruction::argument(), span);
        self.context.insert_parameter(name, value);

        Ok(())
    }

    fn intern(&mut self, span: Span) -> Name {
        let value = self.source.slice(span);
        let name = self.interner.intern(value);
        name
    }

    fn name(&mut self, span: Span) -> Result<()> {
        let name = self.intern(span);
        self.context.push_name(name, span);

        Ok(())
    }

    fn identifier(&mut self, span: Span) -> Result<()> {
        let value = self.source.slice(span);
        let name = self.interner.intern(value);

        // no support for capturing variables for now
        // either find it in the local function scope or look for a global value
        if let Some(local) = self.context.local(name) {
            self.context.push_value(local.value);
        } else if let Some(global) = self.globals.id(name) {
            self.context.emit(Instruction::load_global(global), span);
        } else {
            return self.fail(span, format!("{value} not found"));
        };

        Ok(())
    }

    fn unary(&mut self, span: Span, operator: syntax::Operator) -> Result<()> {
        let operand = self.context.pop_value();

        let operator = match operator {
            syntax::Operator::Pos => Operator::Pos,
            syntax::Operator::Neg => Operator::Neg,
            syntax::Operator::Not => Operator::Not,
            _ => {
                return self.fail(span, "unsupported unary operator");
            }
        };

        self.context.emit(Instruction::unary(operator, operand), span);

        Ok(())
    }

    fn binary(&mut self, span: Span, operator: syntax::Operator) -> Result<()> {
        let roperand = self.context.pop_value();
        let loperand = self.context.pop_value();

        let operator = match operator {
            syntax::Operator::Add => Operator::Add,
            syntax::Operator::Sub => Operator::Sub,
            syntax::Operator::Mul => Operator::Mul,
            syntax::Operator::Div => Operator::Div,
            syntax::Operator::Mod => Operator::Mod,
            syntax::Operator::Pow => Operator::Pow,
            syntax::Operator::Eq => Operator::Eq,
            syntax::Operator::Ne => Operator::Ne,
            syntax::Operator::Lt => Operator::Lt,
            syntax::Operator::Le => Operator::Le,
            syntax::Operator::Gt => Operator::Gt,
            syntax::Operator::Ge => Operator::Ge,
            syntax::Operator::And => Operator::And,
            syntax::Operator::Or => Operator::Or,
            _ => {
                return self.fail(span, "unsupported binary operator");
            }
        };

        self.context.emit(Instruction::binary(operator, loperand, roperand), span);

        Ok(())
    }

    fn integer(&mut self, span: Span) -> Result<()> {
        let value = self.source.slice(span);

        let value = match &value.get(0..2) {
            Some("0b") => i64::from_str_radix(&value[2..], 2),
            Some("0x") => i64::from_str_radix(&value[2..], 16),
            _ => str::parse(value),
        };

        let Ok(value) = value else {
            return self.fail(span, "value is not supported as an integer");
        };

        self.context.emit(Instruction::integer(value), span);

        Ok(())
    }

    fn number(&mut self, span: Span) -> Result<()> {
        let value = self.source.slice(span);

        let Ok(value) = str::parse(value) else {
            return self.fail(span, "value is not supported as an number");
        };

        self.context.emit(Instruction::number(value), span);

        Ok(())
    }

    fn boolean(&mut self, span: Span, truthy: bool) -> Result<()> {
        self.context.emit(
            if truthy {
                Instruction::True
            } else {
                Instruction::False
            },
            span,
        );

        Ok(())
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }
}

pub fn from_syntax(syntax: &Syntax, source: &Source, interner: &mut Interner) -> Result<Module> {
    let mut lower = LowerSyntax::new(source, interner, syntax);
    lower.lower()?;

    Ok(Module {
        globals: lower.globals,
        functions: lower.functions,
    })
}
