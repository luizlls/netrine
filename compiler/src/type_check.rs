use crate::error::{Error, Result};
use crate::source::Span;
use crate::state::State;
use crate::syntax::{Binary, Define, Module, Name, Node, NodeKind, OperatorKind, Unary};
use crate::types::{self, Type};

struct TypeCheck<'r> {
    state: &'r mut State,
}

impl<'r> TypeCheck<'r> {
    fn new(state: &'r mut State) -> TypeCheck<'r> {
        TypeCheck { state }
    }

    fn fail<T>(&self, span: Span, message: impl Into<String>) -> Result<T> {
        Err(Error::error(span, message.into()))
    }

    fn check(&mut self, node: &Node) -> Result<Type> {
        let node_type = match &node.kind {
            NodeKind::Define(define) => self.define(node, define)?,
            NodeKind::Unary(unary) => self.unary(node, unary)?,
            NodeKind::Binary(binary) => self.binary(node, binary)?,
            NodeKind::Name(name) => self.name(node, name)?,
            NodeKind::Number(_) => types::NUMBER,
            NodeKind::Integer(_) => types::INTEGER,
        };

        self.state.types.insert(node.id, node_type);
        Ok(node_type)
    }

    fn define(&mut self, _node: &Node, define: &Define) -> Result<Type> {
        self.check(&define.value)
    }

    fn name(&mut self, _node: &Node, name: &Name) -> Result<Type> {
        let Some(symbol) = self.state.symbols.lookup(name.id) else {
            return self.fail(
                name.span,
                format!("variable `{}` not defined", self.state.interner.get(name.id).unwrap()),
            );
        };

        Ok(self.state.types.get(symbol.node))
    }

    fn unary(&mut self, node: &Node, unary: &Unary) -> Result<Type> {
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
                return self.fail(node.span, "invalid unary operator");
            }
        };

        Ok(result_type)
    }

    fn binary(&mut self, node: &Node, binary: &Binary) -> Result<Type> {
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
                        return self.fail(node.span, "invalid type for equality comparison");
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
                return self.fail(node.span, "invalid binary operator");
            }
        };

        Ok(result_type)
    }

    fn expect(&self, node: &Node, node_type: Type, expected_type: Type) -> Result<()> {
        if node_type.is(expected_type) {
            Ok(())
        } else {
            self.fail(node.span, format!("expected `{}`, found `{}`", expected_type, node_type))
        }
    }
}

pub fn check(module: &Module, state: &mut State) -> Result<()> {
    let mut type_checker = TypeCheck::new(state);

    for node in &module.nodes {
        type_checker.check(node)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::*;

    fn type_check(mut state: State, node: Node) -> Vec<Type> {
        check(
            &Module {
                nodes: vec![node],
            },
            &mut state,
        )
        .unwrap();

        state.types.types().into()
    }

    fn type_check_err(mut state: State, node: Node) -> Error {
        check(
            &Module {
                nodes: vec![node],
            },
            &mut state,
        )
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
            type_check(
                State::new(),
                node(
                    NodeId(0),
                    NodeKind::Integer(Literal {
                        value: "42".into(),
                    })
                )
            ),
            vec![types::INTEGER],
        );
    }

    #[test]
    fn number() {
        assert_eq!(
            type_check(
                State::new(),
                node(
                    NodeId(0),
                    NodeKind::Number(Literal {
                        value: "3.14".into(),
                    })
                )
            ),
            vec![types::NUMBER],
        );
    }

    #[test]
    fn unary() {
        let unary_operators = vec![OperatorKind::Neg, OperatorKind::Pos];

        for operator in unary_operators {
            assert_eq!(
                type_check(
                    State::new(),
                    node(
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
                    )
                ),
                vec![types::INTEGER, types::INTEGER]
            );

            assert_eq!(
                type_check(
                    State::new(),
                    node(
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
                    )
                ),
                vec![types::NUMBER, types::NUMBER]
            );
        }
    }

    #[test]
    fn unary_not_boolean() {
        assert_eq!(
            type_check_err(
                State::new(),
                node(
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
                )
            ),
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
                type_check(
                    State::new(),
                    node(
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
                    )
                ),
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
                type_check(
                    State::new(),
                    node(
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
                    )
                ),
                vec![types::INTEGER, types::NUMBER, types::NUMBER],
            );
        }
    }

    #[test]
    fn binary_division_promotion() {
        assert_eq!(
            type_check(
                State::new(),
                node(
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
                )
            ),
            vec![types::INTEGER, types::INTEGER, types::NUMBER],
        );
    }

    #[test]
    fn binary_modulo_integer() {
        assert_eq!(
            type_check(
                State::new(),
                node(
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
                )
            ),
            vec![types::INTEGER, types::INTEGER, types::INTEGER],
        );
    }

    #[test]
    fn binary_modulo_number() {
        assert_eq!(
            type_check_err(
                State::new(),
                node(
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
                )
            ),
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
                type_check(
                    State::new(),
                    node(
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
                    )
                ),
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
                type_check(
                    State::new(),
                    node(
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
                    )
                ),
                vec![types::NUMBER, types::INTEGER, types::BOOLEAN],
            );
        }
    }
}
