use crate::error::{Error, Result};
use crate::source::{Span, ToSpan};
use crate::syntax::{Binary, Module, Node, NodeId, NodeKind, OperatorKind, Unary};
use crate::types::{self, Type};

#[derive(Debug, Clone)]
pub struct Types {
    types: Vec<Type>,
}

impl Types {
    fn new() -> Types {
        Types { types: vec![] }
    }

    fn insert(&mut self, id: NodeId, type_: Type) {
        debug_assert!(id.index() == self.types.len());
        self.types.push(type_)
    }

    pub fn get(&self, id: NodeId) -> Type {
        self.types.get(id.index()).copied().unwrap_or(Type::Unknown)
    }
}

struct TypeCheck {
    types: Types,
}

impl TypeCheck {
    fn new() -> TypeCheck {
        TypeCheck {
            types: Types::new(),
        }
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }

    fn check(&mut self, node: &Node) -> Result<Type> {
        let node_type = match &node.kind {
            NodeKind::Define(_) => todo!(),
            NodeKind::Unary(unary) => self.unary(unary, node.span)?,
            NodeKind::Binary(binary) => self.binary(binary, node.span)?,
            NodeKind::Name(_) => todo!(),
            NodeKind::Number(_) => types::NUMBER,
            NodeKind::Integer(_) => types::INTEGER,
        };

        self.types.insert(node.id, node_type);
        Ok(node_type)
    }

    fn unary(&mut self, unary: &Unary, span: Span) -> Result<Type> {
        let operand_type = self.check(&unary.expr)?;

        let result_type = match unary.operator.kind {
            OperatorKind::Not => {
                self.expect(&unary.expr, operand_type, types::BOOLEAN)?;
                types::BOOLEAN
            }
            OperatorKind::Pos | OperatorKind::Neg => {
                self.expect(&unary.expr, operand_type, types::NUMBER)?;
                operand_type
            }
            _ => {
                return self.fail(span, "invalid unary operator");
            }
        };

        Ok(result_type)
    }

    fn binary(&mut self, binary: &Binary, span: Span) -> Result<Type> {
        let loperand_type = self.check(&binary.lexpr)?;
        let roperand_type = self.check(&binary.rexpr)?;

        let result_type = match binary.operator.kind {
            OperatorKind::And | OperatorKind::Or => {
                self.expect(&binary.lexpr, loperand_type, types::BOOLEAN)?;
                self.expect(&binary.rexpr, roperand_type, types::BOOLEAN)?;
                types::BOOLEAN
            }
            OperatorKind::Eq | OperatorKind::Ne => {
                match loperand_type {
                    types::BOOLEAN => self.expect(&binary.rexpr, roperand_type, types::BOOLEAN)?,
                    types::INTEGER | types::NUMBER => {
                        self.expect(&binary.rexpr, roperand_type, types::NUMBER)?
                    }
                    _ => {
                        return self.fail(span, "invalid type for equality comparison");
                    }
                }
                types::BOOLEAN
            }
            OperatorKind::Lt | OperatorKind::Le | OperatorKind::Gt | OperatorKind::Ge => {
                self.expect(&binary.lexpr, loperand_type, types::NUMBER)?;
                self.expect(&binary.rexpr, roperand_type, types::NUMBER)?;
                types::BOOLEAN
            }
            OperatorKind::Add | OperatorKind::Sub | OperatorKind::Mul | OperatorKind::Pow => {
                self.expect(&binary.lexpr, loperand_type, types::NUMBER)?;
                self.expect(&binary.rexpr, roperand_type, types::NUMBER)?;

                if loperand_type == types::INTEGER && roperand_type == types::INTEGER {
                    types::INTEGER
                } else {
                    types::NUMBER
                }
            }
            OperatorKind::Div => {
                self.expect(&binary.lexpr, loperand_type, types::NUMBER)?;
                self.expect(&binary.rexpr, roperand_type, types::NUMBER)?;
                types::NUMBER
            }
            OperatorKind::Mod => {
                self.expect(&binary.lexpr, loperand_type, types::INTEGER)?;
                self.expect(&binary.rexpr, roperand_type, types::INTEGER)?;
                types::INTEGER
            }
            _ => {
                return self.fail(span, "invalid binary operator");
            }
        };

        Ok(result_type)
    }

    fn expect(&self, node: &Node, node_type: Type, expected: Type) -> Result<()> {
        if node_type.is(expected) {
            Ok(())
        } else {
            self.fail(node.span(), format!("expected `{}`, found `{}`", expected, node_type))
        }
    }
}

pub fn check(module: &Module) -> Result<Types> {
    let mut type_checker = TypeCheck::new();

    for node in &module.nodes {
        type_checker.check(node)?;
    }

    Ok(type_checker.types)
}
