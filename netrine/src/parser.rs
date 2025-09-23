use crate::error::{Error, Result};
use crate::lexer::Tokens;
use crate::source::Span;
use crate::syntax::{Binary, Literal, Module, Node, Operator, OperatorKind, Precedence, Unary};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
struct Parser<'src> {
    tokens: Tokens<'src>,
    token: Token,
}

impl<'src> Parser<'src> {
    fn new(tokens: Tokens<'src>) -> Parser<'src> {
        Parser {
            tokens,
            token: Token::default(),
        }
        .init()
    }

    fn init(mut self) -> Parser<'src> {
        self.token = self.tokens.token();
        self
    }

    fn bump(&mut self) {
        self.tokens.bump();
        self.token = self.tokens.token();
    }

    fn fail<T>(&mut self, message: impl Into<String>) -> Result<T> {
        let span = self.token.span;
        self.recover();
        Err(Error::error(span, message.into()))
    }

    fn recover(&mut self) {
        while !self.tokens.done() {
            self.bump();
        }
    }

    pub fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn done(&self) -> bool {
        self.tokens.done()
    }

    fn top_level(&mut self) -> Result<Node> {
        self.expr()
    }

    fn expr(&mut self) -> Result<Node> {
        self.binary(0 as Precedence)
    }

    fn atom(&mut self) -> Result<Node> {
        let token = self.token;
        match token.kind {
            TokenKind::Number => self.number(),
            TokenKind::Integer => self.integer(),
            TokenKind::LParen => self.parens(),
            _ => self.fail(match token.kind {
                TokenKind::UnexpectedCharacter => "unexpected character",
                TokenKind::UnterminatedString => "unterminated string",
                _ => "unexpected expression",
            }),
        }
    }

    fn literal(&mut self, kind: TokenKind, ctor: fn(Literal) -> Node) -> Result<Node> {
        let token = self.token;
        self.expect(kind)?;
        let span = token.span;
        let value = self.tokens.value(token).to_string();

        Ok(ctor(Literal { value, span }))
    }

    fn number(&mut self) -> Result<Node> {
        self.literal(TokenKind::Number, Node::Number)
    }

    fn integer(&mut self) -> Result<Node> {
        self.literal(TokenKind::Integer, Node::Integer)
    }

    fn parens(&mut self) -> Result<Node> {
        self.expect(TokenKind::LParen)?;
        let expr = self.expr()?;
        self.expect(TokenKind::RParen)?;

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Node> {
        let Some(operator) = self.operator(0 as Precedence, true) else {
            return self.atom();
        };

        let expr = self.unary()?;

        Ok(Node::Unary(
            Unary {
                span: Span::from(&operator, &expr),
                expr,
                operator,
            }
            .into(),
        ))
    }

    fn binary(&mut self, precedence: Precedence) -> Result<Node> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.operator(precedence, false) {
            let lexpr = expr;
            let rexpr = self.binary(operator.next_precedence())?;

            expr = Node::Binary(
                Binary {
                    span: Span::from(&lexpr, &rexpr),
                    lexpr,
                    rexpr,
                    operator,
                }
                .into(),
            );
        }

        Ok(expr)
    }

    fn operator(&mut self, precedence: Precedence, unary: bool) -> Option<Operator> {
        let token = self.token;

        let kind = match token.kind {
            TokenKind::Plus if unary => OperatorKind::Pos,
            TokenKind::Minus if unary => OperatorKind::Neg,
            TokenKind::Not if unary => OperatorKind::Not,
            TokenKind::Plus => OperatorKind::Add,
            TokenKind::Minus => OperatorKind::Sub,
            TokenKind::Star => OperatorKind::Mul,
            TokenKind::Slash => OperatorKind::Div,
            TokenKind::Caret => OperatorKind::Exp,
            TokenKind::Mod => OperatorKind::Mod,
            TokenKind::And => OperatorKind::And,
            TokenKind::Or => OperatorKind::Or,
            TokenKind::EqEq => OperatorKind::Eq,
            TokenKind::NoEq => OperatorKind::Ne,
            TokenKind::Lt => OperatorKind::Lt,
            TokenKind::LtEq => OperatorKind::Le,
            TokenKind::Gt => OperatorKind::Gt,
            TokenKind::GtEq => OperatorKind::Ge,
            _ => return None,
        };

        let operator = Operator {
            kind,
            span: token.span,
        };

        if operator.precedence() >= precedence {
            self.bump();
            Some(operator)
        } else {
            None
        }
    }

    fn newline(&mut self) {
        self.maybe(TokenKind::EOL);
    }

    fn endline(&mut self) -> Result<()> {
        if self.maybe(TokenKind::EOL) || self.maybe(TokenKind::EOF) {
            Ok(())
        } else {
            self.fail(self.unexpected(&[TokenKind::EOL]))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.at(kind) {
            self.bump();
            Ok(())
        } else {
            self.fail(self.unexpected(&[kind]))
        }
    }

    fn maybe(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn seq<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.newline();

        while !self.at(until) {
            result.push(parse(self)?);

            self.newline();
            if !self.maybe(TokenKind::Comma) {
                break;
            }
            self.newline();
        }

        Ok(result)
    }

    fn many<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.newline();

        while !self.at(until) {
            result.push(parse(self)?);
            self.endline()?;
        }

        Ok(result)
    }

    fn unexpected(&self, expected: &[TokenKind]) -> String {
        let expected = expected
            .iter()
            .map(|it| format!("`{it}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let actual = self.token.kind;
        format!("expected {expected}, found `{actual}`")
    }
}

pub fn parse(tokens: Tokens<'_>) -> Result<Module> {
    let mut parser = Parser::new(tokens);
    let mut nodes = vec![];

    parser.newline();

    while !parser.done() {
        let node = parser.top_level()?;
        nodes.push(node);
        parser.endline()?;
    }

    Ok(Module { nodes })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::source::*;

    fn parse_module(input: &str) -> Result<Module> {
        let source = Source::new("<test>".to_string(), input);
        parse(tokens(&source))
    }

    #[test]
    fn empty() {
        let module = parse_module("").unwrap();

        assert_eq!(module.nodes, vec![]);
    }

    #[test]
    fn empty_lines() {
        let module = parse_module("\n\n\n").unwrap();

        assert_eq!(module.nodes, vec![]);
    }

    #[test]
    fn integer() {
        let module = parse_module("42").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Integer(Literal {
                value: "42".to_string(),
                span: Span::new(0, 2),
            })]
        );
    }

    #[test]
    fn number() {
        let module = parse_module("3.14").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Number(Literal {
                value: "3.14".to_string(),
                span: Span::new(0, 4),
            })]
        );
    }

    #[test]
    fn multiple_items() {
        let module = parse_module("1\n2\n\n3").unwrap();

        assert_eq!(
            module.nodes,
            vec![
                Node::Integer(Literal {
                    value: "1".to_string(),
                    span: Span::new(0, 1),
                }),
                Node::Integer(Literal {
                    value: "2".to_string(),
                    span: Span::new(2, 3),
                }),
                Node::Integer(Literal {
                    value: "3".to_string(),
                    span: Span::new(5, 6),
                }),
            ]
        );
    }

    #[test]
    fn unary_negative() {
        let module = parse_module("-10").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Unary(
                Unary {
                    operator: Operator {
                        kind: OperatorKind::Neg,
                        span: Span::new(0, 1),
                    },
                    expr: Node::Integer(Literal {
                        value: "10".to_string(),
                        span: Span::new(1, 3),
                    }),
                    span: Span::new(0, 3)
                }
                .into()
            )]
        );
    }

    #[test]
    fn unary_positive() {
        let module = parse_module("+3.14").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Unary(
                Unary {
                    operator: Operator {
                        kind: OperatorKind::Pos,
                        span: Span::new(0, 1),
                    },
                    expr: Node::Number(Literal {
                        value: "3.14".to_string(),
                        span: Span::new(1, 5),
                    }),
                    span: Span::new(0, 5)
                }
                .into()
            )]
        );
    }

    #[test]
    fn unary_chained() {
        let module = parse_module("---5").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Unary(
                Unary {
                    operator: Operator {
                        kind: OperatorKind::Neg,
                        span: Span::new(0, 1),
                    },
                    expr: Node::Unary(
                        Unary {
                            operator: Operator {
                                kind: OperatorKind::Neg,
                                span: Span::new(1, 2),
                            },
                            expr: Node::Unary(
                                Unary {
                                    operator: Operator {
                                        kind: OperatorKind::Neg,
                                        span: Span::new(2, 3),
                                    },
                                    expr: Node::Integer(Literal {
                                        value: "5".to_string(),
                                        span: Span::new(3, 4),
                                    }),
                                    span: Span::new(2, 4)
                                }
                                .into()
                            ),
                            span: Span::new(1, 4)
                        }
                        .into()
                    ),
                    span: Span::new(0, 4)
                }
                .into()
            )]
        );
    }

    #[test]
    fn unary_confusing() {
        let module = parse_module("1--10").unwrap();
        // 1 - -10

        assert_eq!(
            module.nodes,
            vec![Node::Binary(
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Sub,
                        span: Span::new(1, 2),
                    },
                    lexpr: Node::Integer(Literal {
                        value: "1".to_string(),
                        span: Span::new(0, 1),
                    }),
                    rexpr: Node::Unary(
                        Unary {
                            operator: Operator {
                                kind: OperatorKind::Neg,
                                span: Span::new(2, 3),
                            },
                            expr: Node::Integer(Literal {
                                value: "10".to_string(),
                                span: Span::new(3, 5),
                            }),
                            span: Span::new(2, 5)
                        }
                        .into()
                    ),
                    span: Span::new(0, 5)
                }
                .into()
            )]
        );
    }

    #[test]
    fn binary() {
        let module = parse_module("50 + 2.10").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Binary(
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Add,
                        span: Span::new(3, 4),
                    },
                    lexpr: Node::Integer(Literal {
                        value: "50".to_string(),
                        span: Span::new(0, 2),
                    }),
                    rexpr: Node::Number(Literal {
                        value: "2.10".to_string(),
                        span: Span::new(5, 9),
                    }),
                    span: Span::new(0, 9)
                }
                .into()
            ),]
        );
    }

    #[test]
    fn logical() {
        let module = parse_module("1 or not 0 and 1").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Binary(
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Or,
                        span: Span::new(2, 4),
                    },
                    lexpr: Node::Integer(Literal {
                        value: "1".to_string(),
                        span: Span::new(0, 1),
                    }),
                    rexpr: Node::Binary(
                        Binary {
                            operator: Operator {
                                kind: OperatorKind::And,
                                span: Span::new(11, 14),
                            },
                            lexpr: Node::Unary(
                                Unary {
                                    operator: Operator {
                                        kind: OperatorKind::Not,
                                        span: Span::new(5, 8),
                                    },
                                    expr: Node::Integer(Literal {
                                        value: "0".to_string(),
                                        span: Span::new(9, 10),
                                    }),
                                    span: Span::new(5, 10),
                                }
                                .into()
                            ),
                            rexpr: Node::Integer(Literal {
                                value: "1".to_string(),
                                span: Span::new(15, 16),
                            }),
                            span: Span::new(5, 16),
                        }
                        .into()
                    ),
                    span: Span::new(0, 16)
                }
                .into()
            ),]
        );
    }

    #[test]
    fn precendece() {
        let module = parse_module("1 + 2 * 3 / 4 - 5").unwrap();
        // ((1 + ((2 * 3) / 4)) - 5)

        assert_eq!(
            module.nodes,
            vec![Node::Binary(
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Sub,
                        span: Span::new(14, 15),
                    },
                    lexpr: Node::Binary(
                        Binary {
                            operator: Operator {
                                kind: OperatorKind::Add,
                                span: Span::new(2, 3),
                            },
                            lexpr: Node::Integer(Literal {
                                value: "1".to_string(),
                                span: Span::new(0, 1),
                            }),
                            rexpr: Node::Binary(
                                Binary {
                                    operator: Operator {
                                        kind: OperatorKind::Div,
                                        span: Span::new(10, 11),
                                    },
                                    lexpr: Node::Binary(
                                        Binary {
                                            operator: Operator {
                                                kind: OperatorKind::Mul,
                                                span: Span::new(6, 7),
                                            },
                                            lexpr: Node::Integer(Literal {
                                                value: "2".to_string(),
                                                span: Span::new(4, 5),
                                            }),
                                            rexpr: Node::Integer(Literal {
                                                value: "3".to_string(),
                                                span: Span::new(8, 9),
                                            }),
                                            span: Span::new(4, 9)
                                        }
                                        .into()
                                    ),
                                    rexpr: Node::Integer(Literal {
                                        value: "4".to_string(),
                                        span: Span::new(12, 13),
                                    }),
                                    span: Span::new(4, 13)
                                }
                                .into()
                            ),
                            span: Span::new(0, 13)
                        }
                        .into()
                    ),
                    rexpr: Node::Integer(Literal {
                        value: "5".to_string(),
                        span: Span::new(16, 17),
                    }),
                    span: Span::new(0, 17)
                }
                .into()
            )]
        );
    }

    #[test]
    fn parenthesis_precedence() {
        let module = parse_module("1 * (2 + 3)").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Binary(
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Mul,
                        span: Span::new(2, 3),
                    },
                    lexpr: Node::Integer(Literal {
                        value: "1".to_string(),
                        span: Span::new(0, 1),
                    }),
                    rexpr: Node::Binary(
                        Binary {
                            operator: Operator {
                                kind: OperatorKind::Add,
                                span: Span::new(7, 8),
                            },
                            lexpr: Node::Integer(Literal {
                                value: "2".to_string(),
                                span: Span::new(5, 6),
                            }),
                            rexpr: Node::Integer(Literal {
                                value: "3".to_string(),
                                span: Span::new(9, 10),
                            }),
                            span: Span::new(5, 10)
                        }
                        .into()
                    ),
                    span: Span::new(0, 10)
                }
                .into()
            )]
        );
    }

    #[test]
    fn unary_precedence() {
        let module = parse_module("-2 ^ 3").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Binary(
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Exp,
                        span: Span::new(3, 4),
                    },
                    lexpr: Node::Unary(
                        Unary {
                            operator: Operator {
                                kind: OperatorKind::Neg,
                                span: Span::new(0, 1),
                            },
                            expr: Node::Integer(Literal {
                                value: "2".to_string(),
                                span: Span::new(1, 2),
                            }),
                            span: Span::new(0, 2)
                        }
                        .into()
                    ),
                    rexpr: Node::Integer(Literal {
                        value: "3".to_string(),
                        span: Span::new(5, 6),
                    }),
                    span: Span::new(0, 6)
                }
                .into()
            )]
        );
    }

    #[test]
    fn right_associativity() {
        let module = parse_module("1 ^ 2 ^ 3").unwrap();

        assert_eq!(
            module.nodes,
            vec![Node::Binary(
                Binary {
                    operator: Operator {
                        kind: OperatorKind::Exp,
                        span: Span::new(2, 3),
                    },
                    lexpr: Node::Integer(Literal {
                        value: "1".to_string(),
                        span: Span::new(0, 1),
                    }),
                    rexpr: Node::Binary(
                        Binary {
                            operator: Operator {
                                kind: OperatorKind::Exp,
                                span: Span::new(6, 7),
                            },
                            lexpr: Node::Integer(Literal {
                                value: "2".to_string(),
                                span: Span::new(4, 5),
                            }),
                            rexpr: Node::Integer(Literal {
                                value: "3".to_string(),
                                span: Span::new(8, 9),
                            }),
                            span: Span::new(4, 9)
                        }
                        .into()
                    ),
                    span: Span::new(0, 9)
                }
                .into()
            )]
        );
    }

    #[test]
    fn unsupported_expression() {
        let error = parse_module("foo").unwrap_err();

        assert_eq!(error, Error::error(Span::new(0, 3), "unexpected expression".to_string()));
    }

    #[test]
    fn incomplete_expression() {
        let error = parse_module("(1 + 2").unwrap_err();

        assert_eq!(
            error,
            Error::error(Span::new(6, 6), "expected `)`, found `end of input`".to_string())
        );
    }

    #[test]
    fn missing_newlines() {
        let error = parse_module("1 2").unwrap_err();

        assert_eq!(
            error,
            Error::error(Span::new(2, 3), "expected `new line`, found `integer`".to_string())
        );
    }
}
