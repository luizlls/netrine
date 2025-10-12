use crate::error::{Error, Result};
use crate::hir::{Binary, Module, Node, Operator, Unary};
use crate::source::{Span, ToSpan};
use crate::types::{self, Type};

struct TypeCheck {}

impl TypeCheck {
    fn new() -> TypeCheck {
        TypeCheck {}
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }

    fn check(&mut self, node: &mut Node) -> Result<Type> {
        match node {
            Node::Define(define) => todo!(),
            Node::Unary(unary) => self.unary(unary.as_mut()),
            Node::Binary(binary) => self.binary(binary.as_mut()),
            Node::Name(name) => todo!(),
            Node::Number(_) => Ok(types::NUMBER),
            Node::Integer(_) => Ok(types::INTEGER),
        }
    }

    fn unary(&mut self, unary: &mut Unary) -> Result<Type> {
        let operand_type = self.check(&mut unary.operand)?;

        unary.type_ = match unary.operator {
            Operator::Not => {
                self.expect(&unary.operand, operand_type, types::BOOLEAN)?;
                types::BOOLEAN
            }
            Operator::Pos | Operator::Neg => {
                self.expect(&unary.operand, operand_type, types::NUMBER)?;
                operand_type
            }
            _ => unreachable!(),
        };

        Ok(unary.type_)
    }

    fn binary(&mut self, binary: &mut Binary) -> Result<Type> {
        let loperand_type = self.check(&mut binary.loperand)?;
        let roperand_type = self.check(&mut binary.roperand)?;

        binary.type_ = match binary.operator {
            Operator::And | Operator::Or => {
                self.expect(&binary.loperand, loperand_type, types::BOOLEAN)?;
                self.expect(&binary.roperand, roperand_type, types::BOOLEAN)?;
                types::BOOLEAN
            }
            Operator::Eq | Operator::Ne => {
                match loperand_type {
                    types::BOOLEAN => {
                        self.expect(&binary.roperand, roperand_type, types::BOOLEAN)?
                    }
                    types::INTEGER | types::NUMBER => {
                        self.expect(&binary.roperand, roperand_type, types::NUMBER)?
                    }
                    _ => unreachable!(),
                }
                types::BOOLEAN
            }
            Operator::Lt | Operator::Le | Operator::Gt | Operator::Ge => {
                self.expect(&binary.loperand, loperand_type, types::NUMBER)?;
                self.expect(&binary.roperand, roperand_type, types::NUMBER)?;
                types::BOOLEAN
            }
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Pow => {
                self.expect(&binary.loperand, loperand_type, types::NUMBER)?;
                self.expect(&binary.roperand, roperand_type, types::NUMBER)?;

                if loperand_type == types::INTEGER && roperand_type == types::INTEGER {
                    types::INTEGER
                } else {
                    types::NUMBER
                }
            }
            Operator::Div => {
                self.expect(&binary.loperand, loperand_type, types::NUMBER)?;
                self.expect(&binary.roperand, roperand_type, types::NUMBER)?;
                types::NUMBER
            }
            Operator::Mod => {
                self.expect(&binary.loperand, loperand_type, types::INTEGER)?;
                self.expect(&binary.roperand, roperand_type, types::INTEGER)?;
                types::INTEGER
            }
            _ => unreachable!(),
        };

        Ok(binary.type_)
    }

    fn expect(&self, node: &Node, node_type: Type, expected: Type) -> Result<()> {
        if node_type.is(expected) {
            Ok(())
        } else {
            self.fail(node.span(), format!("expected `{}`, found `{}`", expected, node_type))
        }
    }
}

pub fn check(mut module: Module) -> Result<Module> {
    let mut type_checker = TypeCheck::new();

    for node in &mut module.nodes {
        type_checker.check(node)?;
    }

    Ok(module)
}
