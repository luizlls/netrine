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

    fn expect(&self, node: &Node, node_type: Type, expected_type: Type) -> Result<()> {
        if node_type.is(expected_type) {
            Ok(())
        } else {
            self.fail(node.span(), format!("expected `{}`, found `{}`", expected_type, node_type))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::*;

    fn type_check(node: Node) -> Vec<Type> {
        check(&Module {
            nodes: vec![node],
        })
        .unwrap()
        .types
    }

    fn type_check_err(node: Node) -> Error {
        check(&Module {
            nodes: vec![node],
        })
        .unwrap_err()
    }

    fn node(id: NodeId, kind: impl Into<NodeKind>) -> Node {
        Node {
            id,
            kind: kind.into(),
            span: Span::ZERO,
        }
    }

    #[test]
    fn integer() {
        assert_eq!(
            type_check(node(
                NodeId(0),
                NodeKind::Integer(Literal {
                    value: "42".into(),
                })
            )),
            vec![types::INTEGER],
        );
    }

    #[test]
    fn number() {
        assert_eq!(
            type_check(node(
                NodeId(0),
                NodeKind::Number(Literal {
                    value: "3.14".into(),
                })
            )),
            vec![types::NUMBER],
        );
    }

    #[test]
    fn unary() {
        let unary_operators = vec![OperatorKind::Neg, OperatorKind::Pos];

        for operator in unary_operators {
            assert_eq!(
                type_check(node(
                    NodeId(1),
                    Unary {
                        operator: Operator {
                            kind: operator,
                            span: Span::ZERO,
                        },
                        expr: node(
                            NodeId(0),
                            NodeKind::Integer(Literal {
                                value: "1".into(),
                            })
                        ),
                    }
                )),
                vec![types::INTEGER, types::INTEGER]
            );

            assert_eq!(
                type_check(node(
                    NodeId(1),
                    Unary {
                        operator: Operator {
                            kind: operator,
                            span: Span::ZERO,
                        },
                        expr: node(
                            NodeId(0),
                            NodeKind::Number(Literal {
                                value: "1.23".into(),
                            })
                        ),
                    }
                )),
                vec![types::NUMBER, types::NUMBER]
            );
        }
    }

    #[test]
    fn unary_not_boolean() {
        assert_eq!(
            type_check_err(node(
                NodeId(1),
                Unary {
                    operator: Operator {
                        kind: OperatorKind::Not,
                        span: Span::new(0, 1),
                    },
                    expr: Node {
                        id: NodeId(0),
                        kind: NodeKind::Integer(Literal {
                            value: "10".into(),
                        }),
                        span: Span::new(1, 3),
                    }
                }
            )),
            Error::error(Span::new(1, 3), "expected `boolean`, found `integer`".into())
        );
    }

    #[test]
    fn binary() {
        let binary_operators = vec![
            OperatorKind::Add,
            OperatorKind::Sub,
            OperatorKind::Mul,
            OperatorKind::Pow,
        ];
        for operator in binary_operators {
            assert_eq!(
                type_check(node(
                    NodeId(2),
                    Binary {
                        operator: Operator {
                            kind: operator,
                            span: Span::ZERO,
                        },
                        lexpr: node(
                            NodeId(0),
                            NodeKind::Integer(Literal {
                                value: "1".into(),
                            })
                        ),
                        rexpr: node(
                            NodeId(1),
                            NodeKind::Integer(Literal {
                                value: "2".into(),
                            })
                        ),
                    }
                )),
                vec![types::INTEGER, types::INTEGER, types::INTEGER],
            );
        }
    }

    #[test]
    fn binary_integer_promotion() {
        let binary_operators = vec![
            OperatorKind::Add,
            OperatorKind::Sub,
            OperatorKind::Mul,
            OperatorKind::Pow,
        ];
        for operator in binary_operators {
            assert_eq!(
                type_check(node(
                    NodeId(2),
                    Binary {
                        operator: Operator {
                            kind: operator,
                            span: Span::ZERO,
                        },
                        lexpr: node(
                            NodeId(0),
                            NodeKind::Integer(Literal {
                                value: "100".into(),
                            })
                        ),
                        rexpr: node(
                            NodeId(1),
                            NodeKind::Number(Literal {
                                value: "3.14".into(),
                            })
                        ),
                    }
                )),
                vec![types::INTEGER, types::NUMBER, types::NUMBER],
            );
        }
    }

    #[test]
    fn binary_division_promotion() {
        assert_eq!(
            type_check(node(
                NodeId(2),
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Div,
                        span: Span::ZERO,
                    },
                    lexpr: node(
                        NodeId(0),
                        NodeKind::Integer(Literal {
                            value: "10".into(),
                        })
                    ),
                    rexpr: node(
                        NodeId(1),
                        NodeKind::Integer(Literal {
                            value: "2".into(),
                        })
                    ),
                }
            )),
            vec![types::INTEGER, types::INTEGER, types::NUMBER],
        );
    }

    #[test]
    fn binary_modulo_integer() {
        assert_eq!(
            type_check(node(
                NodeId(2),
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Mod,
                        span: Span::ZERO,
                    },
                    lexpr: node(
                        NodeId(0),
                        NodeKind::Integer(Literal {
                            value: "10".into(),
                        })
                    ),
                    rexpr: node(
                        NodeId(1),
                        NodeKind::Integer(Literal {
                            value: "5".into(),
                        })
                    ),
                }
            )),
            vec![types::INTEGER, types::INTEGER, types::INTEGER],
        );
    }

    #[test]
    fn binary_modulo_number() {
        assert_eq!(
            type_check_err(node(
                NodeId(2),
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Mod,
                        span: Span::new(3, 4),
                    },
                    lexpr: Node {
                        id: NodeId(0),
                        kind: NodeKind::Integer(Literal {
                            value: "10".into(),
                        }),
                        span: Span::new(0, 2),
                    },
                    rexpr: Node {
                        id: NodeId(1),
                        kind: NodeKind::Number(Literal {
                            value: "1.1".into(),
                        }),
                        span: Span::new(4, 7),
                    },
                }
            )),
            Error::error(Span::new(4, 7), "expected `integer`, found `number`".into())
        );
    }

    #[test]
    fn binary_comparison_integer() {
        let comparison_operators = vec![
            OperatorKind::Eq,
            OperatorKind::Ne,
            OperatorKind::Lt,
            OperatorKind::Le,
            OperatorKind::Gt,
            OperatorKind::Ge,
        ];
        for operator in comparison_operators {
            assert_eq!(
                type_check(node(
                    NodeId(2),
                    Binary {
                        operator: Operator {
                            kind: operator,
                            span: Span::ZERO,
                        },
                        lexpr: node(
                            NodeId(0),
                            NodeKind::Integer(Literal {
                                value: "1".into(),
                            })
                        ),
                        rexpr: node(
                            NodeId(1),
                            NodeKind::Integer(Literal {
                                value: "1".into(),
                            })
                        ),
                    }
                )),
                vec![types::INTEGER, types::INTEGER, types::BOOLEAN],
            );
        }
    }

    #[test]
    fn binary_comparison_mixed() {
        let comparison_operators = vec![
            OperatorKind::Eq,
            OperatorKind::Ne,
            OperatorKind::Lt,
            OperatorKind::Le,
            OperatorKind::Gt,
            OperatorKind::Ge,
        ];
        for operator in comparison_operators {
            assert_eq!(
                type_check(node(
                    NodeId(2),
                    Binary {
                        operator: Operator {
                            kind: operator,
                            span: Span::ZERO,
                        },
                        lexpr: node(
                            NodeId(0),
                            NodeKind::Number(Literal {
                                value: "1.2".into(),
                            })
                        ),
                        rexpr: node(
                            NodeId(1),
                            NodeKind::Integer(Literal {
                                value: "100".into(),
                            })
                        ),
                    }
                )),
                vec![types::NUMBER, types::INTEGER, types::BOOLEAN],
            );
        }
    }
}
