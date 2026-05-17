use hashbrown::HashMap;

use super::ir::*;
use crate::collections::IndexVec;
use crate::error::{Error, Result};
use crate::interner::{Interner, Name};
use crate::macros::entity_id;
use crate::source::{Source, Span};
use crate::syntax;
use crate::types;

entity_id!(SymbolId, u32);

#[derive(Debug, Clone)]
struct Symbol {
    kind: SymbolKind,
    name: Name,
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolKind {
    Local,
    Global,
    Function,
}

#[derive(Debug)]
struct Symbols {
    symbols: IndexVec<SymbolId, Symbol>,
}

impl Symbols {
    fn new() -> Symbols {
        Symbols {
            symbols: IndexVec::new(),
        }
    }

    fn define(&mut self, name: Name, kind: SymbolKind, span: Span) -> SymbolId {
        self.symbols.push(Symbol {
            name,
            kind,
            span,
        })
    }

    fn symbol(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(id)
    }
}

#[derive(Debug)]
struct Scope {
    entries: HashMap<Name, SymbolId>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            entries: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct Context<'ctx> {
    symbols: Symbols,
    scopes: Vec<Scope>,
    source: &'ctx Source,
    interner: &'ctx mut Interner,
}

impl<'ctx> Context<'ctx> {
    fn new(source: &'ctx Source, interner: &'ctx mut Interner) -> Context<'ctx> {
        Context {
            symbols: Symbols::new(),
            scopes: vec![Scope::new()],
            source,
            interner,
        }
    }

    fn slice(&self, span: Span) -> &str {
        self.source.slice(span)
    }

    fn intern(&mut self, span: Span) -> Name {
        self.interner.intern(self.source.slice(span))
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_symbol(&self, name: Name) -> Option<&Symbol> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.entries.get(&name))
            .and_then(|&id| self.symbols.symbol(id))
    }

    fn lookup_local_symbol(&self, name: Name) -> Option<&Symbol> {
        self.scopes
            .last()
            .expect("at least one scope must always exist")
            .entries
            .get(&name)
            .and_then(|&id| self.symbols.symbol(id))
    }

    fn define_symbol(&mut self, kind: SymbolKind, name: Name, span: Span) -> SymbolId {
        let symbol_id = self.symbols.define(name, kind, span);

        self.scopes
            .last_mut()
            .expect("at least one scope must always exist")
            .entries
            .insert(name, symbol_id);

        symbol_id
    }
}

fn lower(ctx: &mut Context, node: &syntax::Node) -> Result<Node> {
    match &node.kind {
        syntax::NodeKind::Function(function) => lower_function(ctx, node, function),
        syntax::NodeKind::Define(define) => lower_define(ctx, node, define),
        syntax::NodeKind::Apply(apply) => lower_apply(ctx, node, apply),
        syntax::NodeKind::Unary(unary) => lower_unary(ctx, node, unary),
        syntax::NodeKind::Binary(binary) => lower_binary(ctx, node, binary),
        syntax::NodeKind::Name(name) => lower_name(ctx, node, name),
        syntax::NodeKind::Number => lower_number(ctx, node),
        syntax::NodeKind::Integer => lower_integer(ctx, node),
        syntax::NodeKind::True => lower_bool(ctx, node, true),
        syntax::NodeKind::False => lower_bool(ctx, node, false),
    }
}

fn lower_function(
    ctx: &mut Context,
    node: &syntax::Node,
    function: &syntax::Function,
) -> Result<Node> {
    let name = ctx.intern(function.name.span);

    if ctx.lookup_symbol(name).is_some() {
        let name = &ctx.interner[name];
        return Err(fail(format!("`{name}` is already defined"), node.span));
    };
    ctx.define_symbol(SymbolKind::Function, name, node.span);

    ctx.enter_scope();

    let parameters = function
        .parameters
        .iter()
        .map(|parameter| {
            let name = ctx.intern(parameter.name.span);

            if ctx.lookup_local_symbol(name).is_some() {
                let name = &ctx.interner[name];
                return Err(fail(format!("`Parameter {name}` is already defined"), node.span));
            };
            ctx.define_symbol(SymbolKind::Local, name, node.span);

            Ok(Parameter::new(name, types::UNKNOWN))
        })
        .collect::<Result<_>>()?;

    let value = lower(ctx, &function.value)?;

    ctx.exit_scope();

    Ok(Node::function(name, parameters, value, node.span, types::UNKNOWN))
}

fn lower_define(ctx: &mut Context, node: &syntax::Node, define: &syntax::Define) -> Result<Node> {
    let name = ctx.intern(define.name.span);

    if ctx.lookup_symbol(name).is_some() {
        let name = &ctx.interner[name];
        return Err(fail(format!("`{name}` is already defined"), node.span));
    };
    ctx.define_symbol(SymbolKind::Global, name, node.span);

    let value = lower(ctx, &define.value)?;

    Ok(Node::global(name, value, node.span, types::UNKNOWN))
}

fn lower_name(ctx: &mut Context, node: &syntax::Node, name: &syntax::Name) -> Result<Node> {
    let name = ctx.intern(name.span);

    let Some(symbol) = ctx.lookup_symbol(name) else {
        let name = &ctx.interner[name];
        return Err(fail(format!("value `{name}` not found in this scope"), node.span));
    };

    let kind = match symbol.kind {
        SymbolKind::Global => ReferenceKind::Global,
        SymbolKind::Local => ReferenceKind::Local,
        SymbolKind::Function => ReferenceKind::Function,
    };

    Ok(Node::reference(kind, symbol.name, symbol.span, types::UNKNOWN))
}

fn lower_apply(ctx: &mut Context, node: &syntax::Node, apply: &syntax::Apply) -> Result<Node> {
    let callee = lower(ctx, &apply.expr)?;
    let arguments = apply.arguments.iter().map(|node| lower(ctx, &node)).collect::<Result<_>>()?;

    Ok(Node::apply(callee, arguments, node.span, types::UNKNOWN))
}

fn lower_binary(ctx: &mut Context, node: &syntax::Node, binary: &syntax::Binary) -> Result<Node> {
    let loperand = lower(ctx, &binary.lexpr)?;
    let roperand = lower(ctx, &binary.rexpr)?;

    let operator = match binary.operator.kind {
        syntax::OperatorKind::Add => Operator::Add,
        syntax::OperatorKind::Sub => Operator::Sub,
        syntax::OperatorKind::Mul => Operator::Mul,
        syntax::OperatorKind::Div => Operator::Div,
        syntax::OperatorKind::Mod => Operator::Mod,
        syntax::OperatorKind::Pow => Operator::Pow,
        syntax::OperatorKind::Eq => Operator::Eq,
        syntax::OperatorKind::Ne => Operator::Ne,
        syntax::OperatorKind::Lt => Operator::Lt,
        syntax::OperatorKind::Le => Operator::Le,
        syntax::OperatorKind::Gt => Operator::Gt,
        syntax::OperatorKind::Ge => Operator::Ge,
        syntax::OperatorKind::And => Operator::And,
        syntax::OperatorKind::Or => Operator::Or,
        _ => {
            return Err(fail("invalid binary operator", node.span));
        }
    };

    Ok(Node::binary(operator, loperand, roperand, node.span, types::UNKNOWN))
}

fn lower_unary(ctx: &mut Context, node: &syntax::Node, unary: &syntax::Unary) -> Result<Node> {
    let operand = lower(ctx, &unary.expr)?;

    let operator = match unary.operator.kind {
        syntax::OperatorKind::Pos => Operator::Pos,
        syntax::OperatorKind::Neg => Operator::Neg,
        syntax::OperatorKind::Not => Operator::Not,
        _ => {
            return Err(fail("invalid unary operator", node.span));
        }
    };

    Ok(Node::unary(operator, operand, node.span, types::UNKNOWN))
}

fn lower_number(ctx: &mut Context, node: &syntax::Node) -> Result<Node> {
    let value = ctx.slice(node.span);

    let Ok(value) = str::parse(&value) else {
        return Err(fail("value is not supported as an number", node.span));
    };

    Ok(Node::number(value, node.span))
}

fn lower_integer(ctx: &mut Context, node: &syntax::Node) -> Result<Node> {
    let value = ctx.slice(node.span);

    let value = match &value.get(0..2) {
        Some("0b") => i64::from_str_radix(&value[2..], 2),
        Some("0x") => i64::from_str_radix(&value[2..], 16),
        _ => str::parse(&value),
    };

    let Ok(value) = value else {
        return Err(fail("value is not supported as an integer", node.span));
    };

    Ok(Node::integer(value, node.span))
}

fn lower_bool(_ctx: &mut Context, node: &syntax::Node, truthy: bool) -> Result<Node> {
    Ok(Node::bool(truthy, node.span))
}

fn fail(message: impl Into<String>, span: Span) -> Error {
    Error::error(span, message.into())
}

pub fn lower_syntax(
    syntax: &syntax::Module,
    source: &Source,
    interner: &mut Interner,
) -> Result<Module> {
    let mut context = Context::new(source, interner);
    let mut nodes = vec![];

    for node in &syntax.nodes {
        nodes.push(lower(&mut context, node)?);
    }

    Ok(Module { nodes })
}
