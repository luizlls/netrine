use crate::error::{Error, Result};
use crate::source::Span;
use crate::state::{State, Symbol};
use crate::syntax::{Binary, Define, Module, Name, Node, NodeKind, Unary};

struct Resolver<'a> {
    state: &'a mut State,
}

impl<'a> Resolver<'a> {
    fn new(state: &'a mut State) -> Resolver<'a> {
        Resolver { state }
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }

    #[rustfmt::skip]
    fn node(&mut self, node: &Node) -> Result<()> {
        match &node.kind {
            NodeKind::Define(define) => self.define(node, define),
            NodeKind::Unary(unary) => self.unary(unary),
            NodeKind::Binary(binary) => self.binary(binary),
            NodeKind::Name(name) => self.name(node, name),
            NodeKind::Number(_)
          | NodeKind::Integer(_) => Ok(()),
        }
    }

    fn define(&mut self, node: &Node, define: &Define) -> Result<()> {
        self.node(&define.value)?;

        self.state.symbols.define(Symbol {
            name: define.name.id,
            node: node.id,
            span: node.span,
        });

        Ok(())
    }

    fn name(&mut self, _node: &Node, name: &Name) -> Result<()> {
        if self.state.symbols.lookup(name.id).is_none() {
            return self.fail(
                name.span,
                format!("variable `{}` not defined", self.state.interner.get(name.id).unwrap()),
            );
        };

        Ok(())
    }

    fn unary(&mut self, unary: &Unary) -> Result<()> {
        self.node(&unary.expr)
    }

    fn binary(&mut self, binary: &Binary) -> Result<()> {
        self.node(&binary.lexpr)?;
        self.node(&binary.rexpr)?;
        Ok(())
    }
}

pub fn resolve(module: &Module, state: &mut State) -> Result<()> {
    let mut resolver = Resolver::new(state);

    for node in &module.nodes {
        resolver.node(node)?;
    }

    Ok(())
}
