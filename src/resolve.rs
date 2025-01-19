use crate::ast;
use crate::error::{Error, Result};
use crate::hir::{Node, NodeKind, Unary, Binary};
use crate::types::TYPE_UNKNOWN;

pub struct Resolver {}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {}
    }

    pub fn resolve(&mut self, node: &ast::Node) -> Result<Node> {
        resolve(self, node)
    }
}

fn resolve(r: &mut Resolver, ast_node: &ast::Node) -> Result<Node> {
    match &ast_node.kind {
        ast::NodeKind::Group(ast::Group { box inner }) => {
            resolve(r, inner)
        }
        ast::NodeKind::Number(ast::Literal { value }) => {
            if let Ok(value) = value.parse::<f64>() {
                Ok(node(ast_node, NodeKind::Number(value)))
            } else {
                Err(fail(ast_node, "Invalid number".to_string()))
            }
        }
        ast::NodeKind::Integer(ast::Literal { value }) => {
            if let Ok(value) = value.parse::<i64>() {
                Ok(node(ast_node, NodeKind::Integer(value)))
            } else {
                Err(fail(ast_node, "Invalid integer".to_string()))
            }
        }
        ast::NodeKind::Unary(ast::Unary { operator, box expr }) => {
            let expr = resolve(r, expr)?;
            
            let kind = NodeKind::Unary(
                Unary {
                    operator: *operator,
                    expr: Box::new(expr),
                }
            );
            Ok(node(ast_node, kind))
        }
        ast::NodeKind::Binary(ast::Binary { operator, box lexpr, box rexpr }) => {
            let lexpr = resolve(r, lexpr)?;
            let rexpr = resolve(r, rexpr)?;

            let kind = NodeKind::Binary(
                Binary {
                    operator: *operator,
                    lexpr: Box::new(lexpr),
                    rexpr: Box::new(rexpr),
                }
            );
            Ok(node(ast_node, kind))
        }
    }
}

fn node(original: &ast::Node, kind: NodeKind) -> Node {
    Node {
        kind,
        span: original.span,
        type_id: TYPE_UNKNOWN,
    }
}

fn fail(original: &ast::Node, error: String) -> Error {
    Error::new(error, original.span)
}
