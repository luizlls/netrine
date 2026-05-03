use super::context::{BuilderKind, Context};
use super::ir::*;
use crate::collections::IndexMap;
use crate::error::{Error, Result};
use crate::interner::{Interner, Name};
use crate::source::{Source, Span};
use crate::syntax::{self, NodeKind, Syntax};

#[derive(Debug)]
struct LowerSyntax<'mir> {
    definitions: IndexMap<Name, DefinitionId, Definition>,
    functions: IndexMap<Name, FunctionId, Function>,
    context: Context,
    interner: &'mir mut Interner,
    source: &'mir Source,
    syntax: &'mir Syntax,
}

impl<'mir> LowerSyntax<'mir> {
    fn new(source: &'mir Source, interner: &'mir mut Interner, syntax: &'mir Syntax) -> LowerSyntax<'mir> {
        LowerSyntax {
            definitions: IndexMap::new(),
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
                NodeKind::GroupInit => {}
                NodeKind::GroupEnd => {}
            }
        }

        Ok(())
    }

    fn setup_definition(&mut self) -> Result<()> {
        if self.context.top_level() {
            self.context.setup_builder(BuilderKind::Let);
        }
        Ok(())
    }

    fn global_definition(&mut self) -> Result<()> {
        let (name, span) = self.context.pop_name().parts();

        if self.definitions.contains(name) {
            let name = &self.interner[name];
            return self.fail(span, format!("`{name}` is already defined"));
        }

        let definition = self.context.pop_builder().build_definition();
        self.definitions.insert(name, definition);

        Ok(())
    }

    fn local_definition(&mut self) -> Result<()> {
        let (name, _span) = self.context.pop_name().parts();

        let value = self.context.pop_value();
        self.context.insert_name(name, value);

        Ok(())
    }

    fn finish_definition(&mut self) -> Result<()> {
        if self.context.builder().is(BuilderKind::Let) {
            self.global_definition()
        } else {
            self.local_definition()
        }
    }

    fn setup_function(&mut self) -> Result<()> {
        self.context.setup_builder(BuilderKind::Fn);
        Ok(())
    }

    fn finish_function(&mut self) -> Result<()> {
        let (name, span) = self.context.pop_name().parts();

        if self.functions.contains(name) {
            let name = &self.interner[name];
            return self.fail(span, format!("function `{name}` is already defined"));
        }

        let function = self.context.pop_builder().build_function();
        self.functions.insert(name, function);

        Ok(())
    }

    fn setup_parameter(&mut self) -> Result<()> {
        // there's nothing to setup for now
        Ok(())
    }

    fn finish_parameter(&mut self) -> Result<()> {
        let (name, span) = self.context.pop_name().parts();

        if self.context.lookup_name(name).is_some() {
            let name = &self.interner[name];
            return self.fail(span, format!("parameter `{name}` is already defined"));
        }

        let parameter_pos = self.context.builder().next_parameter();
        let instruction = Instruction::parameter(parameter_pos);
        let instruction_id = self.context.emit(instruction, span);
        self.context.insert_name(name, instruction_id);

        Ok(())
    }

    fn name(&mut self, span: Span) -> Result<()> {
        let value = self.source.slice(span);
        let name = self.interner.intern(value);
        self.context.push_name(name, span);

        Ok(())
    }

    fn identifier(&mut self, span: Span) -> Result<()> {
        let value = self.source.slice(span);
        let name = self.interner.intern(value);

        if let Some(&instruction) = self.context.lookup_name(name) {
            self.context.push_value(instruction);
        } else if let Some(definition) = self.definitions.id(name) {
            let instruction = Instruction::global(definition);
            self.context.emit(instruction, span);
        } else {
            return self.fail(span, format!("value `{value}` not found"));
        }

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

        let instruction = Instruction::unary(operator, operand);
        self.context.emit(instruction, span);

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

        let instruction = Instruction::binary(operator, loperand, roperand);
        self.context.emit(instruction, span);

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

    fn finalize(self) -> Module {
        Module {
            definitions: self.definitions,
            functions: self.functions,
            entrypoint: self.context.finalize(),
        }
    }
}

pub fn lower_syntax(source: &Source, interner: &mut Interner, syntax: &Syntax) -> Result<Module> {
    let mut lower = LowerSyntax::new(source, interner, syntax);
    lower.lower()?;

    Ok(lower.finalize())
}
