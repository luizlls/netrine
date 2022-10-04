use super::token::{Token, TokenKind};
use super::lexer::Lexer;
use super::nodes::*;

use crate::error::{NetrineError, Result, error};
use crate::span::Span;
use crate::source::Source;

#[derive(Debug, Clone)]
struct Parser<'s> {
    lexer: Lexer<'s>,
    token: Token,
    prev : Token,
    peek : Token,
    source: &'s Source,
}

impl<'s> Parser<'s> {
    fn new(source: &'s Source) -> Parser {
        Parser {
            lexer: Lexer::new(&source.content),
            source,
            token: Token::default(),
            prev : Token::default(),
            peek : Token::default(),
        }
    }

    fn initialize(mut self) -> Result<Parser<'s>> {
        self.bump()?; // peek token
        self.bump()?; // token token

        while self.token.is(TokenKind::NewLine) {
            self.bump()?;
        }

        Ok(self)
    }

    fn top_level(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::Lower if self.peek.is(TokenKind::Equals) => {
                self.define()
            }
            _ => error!(self.token.span, "expected definition")
        }
    }

    fn expression(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::Lower if self.peek.is(TokenKind::Equals) => {
                self.define()
            }
            _ => self.binary(0)
        }
    }

    fn term(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::LParen => self.parens(),
            TokenKind::Lower  => self.lower(),
            TokenKind::Upper  => self.upper(),
            TokenKind::Number => self.number(),
            TokenKind::String => self.string(),
            _ => error!(self.token.span, "expected a term"),
        }
    }

    fn identifier(&mut self) -> Result<Identifier> {
        let span = self.expect(TokenKind::Lower)?;
        let value = self.source.content[span.range()].to_string();
        Ok(Identifier { value, span })
    }

    fn lower(&mut self) -> Result<Node> {
        Ok(Node::Id(self.identifier()?))
    }

    fn upper(&mut self) -> Result<Node> {
        let span = self.expect(TokenKind::Upper)?;
        let value = self.source.content[span.range()].to_string();

        match &value[..] {
            "True"  => Ok(Node::True(span)),
            "False" => Ok(Node::False(span)),
            _ => {
                error!(span, "anonymous variants are not supported yet")
            }
        }
    }

    fn parens(&mut self) -> Result<Node> {
        let start = self.token;

        let (mut elements, trailing_comma) = self.sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::expression,
        )?;

        let span = Span::of(&start, &self.prev);

        if elements.len() == 1 && !trailing_comma {
            Ok(elements.pop().unwrap())
        } else {
            error!(span, "tuples are not supported yet")
        }
    }

    fn number(&mut self) -> Result<Node> {
        let span = self.expect(TokenKind::Number)?;
        let value = self.source.content[span.range()].to_string();

        Ok(Node::Number(Literal { value, span }))
    }

    fn string(&mut self) -> Result<Node> {
        let span = self.expect(TokenKind::String)?;
        let value = self.source.content[span.range()].to_string();

        Ok(Node::String(Literal { value, span }))
    }

    fn define(&mut self) -> Result<Node> {
        let name = self.identifier()?;
        self.expect(TokenKind::Equals)?;
        let value = self.expression()?;

        let span = Span::of(&name, &value);

        Ok(Node::Define(box Define { name, value, span }))
    }

    fn initial(&mut self) -> Result<Node> {
        let mut expression = self.term()?;

        while !self.done() && self.token.is(TokenKind::LParen) {
            expression = self.call(expression)?;
        }

        Ok(expression)
    }

    fn call(&mut self, function: Node) -> Result<Node> {
        let (arguments, _) = self.sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::argument
        )?;

        let span = Span::of(&function, &self.prev);

        Ok(Node::Call(box Call {
            function,
            arguments,
            span,
        }))
    }

    fn argument(&mut self) -> Result<Argument> {
        let start = self.token.span;
        
        let name = if self.token.is(TokenKind::Lower) && self.peek.is(TokenKind::Equals) {
            Some(self.identifier()?)
        } else {
            None
        };

        if name.is_some() {
            self.expect(TokenKind::Equals)?;
        }

        let value = self.expression()?;

        let span = Span::of(&start, &value);

        Ok(Argument { name, value, span })
    }

    fn unary(&mut self) -> Result<Node> {
        if let Ok(operator) = self.operator() {
            self.bump()?;

            if self.token.terminator() {
                let span = operator.span;
                Ok(Node::Partial(box Partial {
                    operator,
                    rhs: None,
                    lhs: None,
                    span,
                }))
            } else {
                let rhs = self.expression()?;
                let span = Span::of(&operator, &rhs);

                if operator.unary() {
                    Ok(Node::Unary(box Unary {
                        operator,
                        rhs,
                        span,
                    }))
                } else {
                    Ok(Node::Partial(box Partial {
                        operator,
                        lhs: None,
                        rhs: Some(rhs),
                        span,
                    }))
                }
            }
        } else {
            self.initial()
        }
    }

    fn binary(&mut self, mininum: u8) -> Result<Node> {
        let mut expr = self.unary()?;

        while let Ok(operator) = self.operator() {

            if operator.precedence() < mininum {
                break;
            }

            self.bump()?;

            // partial operator application
            if self.token.terminator() {
                let span = Span::of(&expr, &operator);

                return Ok(Node::Partial(box Partial {
                    operator,
                    rhs: None,
                    lhs: Some(expr),
                    span,
                }));
            }

            let rhs = self.binary(operator.precedence() + 1)?;
            let lhs = expr;
            let span = Span::of(&lhs, &rhs);

            expr = Node::Binary(box Binary {
                operator,
                lhs, rhs,
                span,
            });
        }

        Ok(expr)
    }

    fn is_unary(&self) -> bool {
        match self.token.kind {
            TokenKind::Plus
          | TokenKind::Minus => {
                match self.peek.kind {
                    TokenKind::Lower
                  | TokenKind::LParen
                  | TokenKind::LBrace
                  | TokenKind::Number => {
                    // there is no spaces between the operator and the next token
                    self.peek.span.start() - self.token.span.end() == 0
                  }
                  _ => false
                }
            }
            _ => false
        }
    }

    fn operator(&mut self) -> Result<Operator> {
        let span = self.token.span;

        let kind = match self.token.kind {
            TokenKind::Plus  if self.is_unary() => OperatorKind::Pos,
            TokenKind::Minus if self.is_unary() => OperatorKind::Neg,
            TokenKind::Plus   => OperatorKind::Add,
            TokenKind::Minus  => OperatorKind::Sub,
            TokenKind::Star   => OperatorKind::Mul,
            TokenKind::Slash  => OperatorKind::Div,
            TokenKind::Mod    => OperatorKind::Mod,
            TokenKind::And    => OperatorKind::And,
            TokenKind::Or     => OperatorKind::Or,
            TokenKind::Not    => OperatorKind::Not,
            TokenKind::Is     => OperatorKind::Is,
            TokenKind::EqEq   => OperatorKind::Eq,
            TokenKind::BangEq => OperatorKind::Ne,
            TokenKind::Lt     => OperatorKind::Lt,
            TokenKind::LeEq   => OperatorKind::Le,
            TokenKind::Gt     => OperatorKind::Gt,
            TokenKind::GtEq   => OperatorKind::Ge,
            TokenKind::Pipe   => OperatorKind::Pipe,
            TokenKind::DotDot => OperatorKind::Range,
            _ => {
                return error!(span, "`{}` is not a valid operator", self.token.kind);
            }
        };

        Ok(Operator { kind, span })
    }

    fn sequence_of<T, F>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        mut f: F,
    ) -> Result<(Vec<T>, bool)>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.expect(open)?;

        let mut trailing_comma = false;

        while !self.done() && !self.token.is(close) {
            result.push(f(self)?);

            if !self.expect_maybe(TokenKind::Comma)? {
                trailing_comma = false;
                break;
            }

            trailing_comma = true;
        }

        self.expect(close)?;

        Ok((result, trailing_comma))
    }

    fn bump(&mut self) -> Result<Span> {
        let span = self.token.span;

        self.prev = self.token;
        self.token = self.peek;
        self.peek = self.lexer.next()?;

        Ok(span)
    }

    fn done(&self) -> bool {
        self.token.kind == TokenKind::Eof
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Span> {
        if self.token.kind == expected {
            self.bump()
        } else {
            error!(self.token.span, "Expected `{}` but found `{}`", expected, self.token.kind)
        }
    }

    fn expect_maybe(&mut self, expected: TokenKind) -> Result<bool> {
        if self.token.kind == expected {
            self.bump()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub fn parse(source: &Source) -> Result<Vec<Node>> {
    let mut parser = Parser::new(source).initialize()?;
    let mut module = vec![];

    while !parser.done() {
        module.push(parser.top_level()?);
    }

    Ok(module)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use super::*;

    fn source(code: &str) -> Source {
        Source {
            path: PathBuf::from("test"),
            content: code.to_string(),
        }
    }

    fn module(code: &str) -> Vec<Node> {
        parse(&source(&code)).unwrap()
    }

    fn expr(code: &str) -> Node {
        Parser::new(&source(&code))
            .initialize()
            .unwrap()
            .expression()
            .unwrap()
    }

    macro_rules! assert_error_expr {
        ($code:expr) => {
            if let Err(_) =
                Parser::new(&source(&$code))
                    .initialize()
                    .and_then(|mut p| p.expression()) {
                assert!(true)
            } else {
                assert!(false)
            }
        };
    }

    #[test]
    fn test_empty_module() {
        assert_eq!(module(""), vec![]);

        assert_eq!(module("  "), vec![]);

        assert_eq!(module("\n\n"), vec![]);
    }

    #[test]
    fn test_name() {
        assert_eq!(
            expr("netrine"),
            Node::Id(Identifier {
                value: "netrine".to_string(),
                span: Span(0, 7)
            })
        );

        assert_eq!(
            expr("maybe?"),
            Node::Id(Identifier {
                value: "maybe?".to_string(),
                span: Span(0, 6)
            })
        );

        assert_eq!(
            expr("dangerous!"),
            Node::Id(Identifier {
                value: "dangerous!".to_string(),
                span: Span(0, 10)
            })
        );

        assert_eq!(
            expr("prime'"),
            Node::Id(Identifier {
                value: "prime'".to_string(),
                span: Span(0, 6)
            })
        );

        assert_eq!(
            expr("primes''''"),
            Node::Id(Identifier {
                value: "primes''''".to_string(),
                span: Span(0, 10)
            })
        );
    }

    #[test]
    fn test_literals() {
        assert_eq!(
            expr("42"),
            Node::Number(Literal {
                value: "42".to_string(),
                span: Span(0, 2)
            })
        );

        assert_eq!(
            expr("3.14"),
            Node::Number(Literal {
                value: "3.14".to_string(),
                span: Span(0, 4)
            })
        );

        assert_eq!(
            expr("\"hello\""),
            Node::String(Literal {
                value: "\"hello\"".to_string(),
                span: Span(0, 7)
            })
        );

        assert_eq!(
            expr("_"),
            Node::Id(Identifier {
                value: "_".to_string(),
                span: Span(0, 1)
            })
        );

        assert_eq!(expr("True"), Node::True(Span(0, 4)));

        assert_eq!(expr("False"), Node::False(Span(0, 5)));

        assert_error_expr!("\"unclosed");

        // assert_error_expr!("1.");
    }

    #[test]
    fn test_basic_definition() {
        assert_eq!(
            expr("value = 10"),
            Node::Define(box Define {
                name: Identifier {
                    value: "value".into(), span: Span(0, 5)
                },
                value: Node::Number(
                    Literal {
                        value: "10".into(), span: Span(8, 10)
                    }
                ),
                span: Span(0, 10)
            })
        );

        assert_error_expr!("value = ");
    }

    #[test]
    fn test_unary_operation() {
        assert_eq!(
            expr("not True"),
            Node::Unary(box Unary {
                operator: Operator {
                    kind: OperatorKind::Not,
                    span: Span(0, 3)
                },
                rhs: Node::True(Span(4, 8)),
                span: Span(0, 8)
            })
        );

        assert_eq!(
            expr("+x"),
            Node::Unary(box Unary {
                operator: Operator {
                    kind: OperatorKind::Pos,
                    span: Span(0, 1)
                },
                rhs: Node::Id(Identifier {
                    value: "x".to_string(),
                    span: Span(1, 2)
                }),
                span: Span(0, 2)
            })
        );

        assert_eq!(
            expr("-x"),
            Node::Unary(box Unary {
                operator: Operator {
                    kind: OperatorKind::Neg,
                    span: Span(0, 1)
                },
                rhs: Node::Id(Identifier {
                    value: "x".to_string(),
                    span: Span(1, 2)
                }),
                span: Span(0, 2)
            })
        );

        assert_error_expr!("+");

        assert_error_expr!("-");

        assert_error_expr!("not");
    }

    #[test]
    fn test_basic_binary_operation() {
        let operations = vec![
            ("+", OperatorKind::Add),
            ("-", OperatorKind::Sub),
            ("*", OperatorKind::Mul),
            ("/", OperatorKind::Div),
            ("%", OperatorKind::Mod),
        ];

        for (symbol, operator) in operations {
            assert_eq!(
                expr(&format!("1 {} 1", symbol)),
                Node::Binary(box Binary {
                    operator: Operator {
                        kind: operator,
                        span: Span(2, 3)
                    },
                    lhs: Node::Number(
                        Literal {
                            value: "1".into(),
                            span: Span(0, 1)
                        }
                    ),
                    rhs: Node::Number(
                        Literal {
                            value: "1".into(),
                            span: Span(4, 5)
                        }
                    ),
                    span: Span(0, 5)
                })
            );
        }
    }

    #[test]
    fn test_complex_binary_operation() {
        assert_eq!(
            expr("a + b - c * d / e"),
            // sub(add(a, b), div(mul(c, d), e))
            Node::Binary(box Binary {
                operator: Operator {
                    kind: OperatorKind::Sub,
                    span: Span(6, 7),
                },
                lhs: Node::Binary(box Binary {
                    operator: Operator {
                        kind: OperatorKind::Add,
                        span: Span(2, 3),
                    },
                    lhs: Node::Id(Identifier {
                        value: "a".to_string(),
                        span: Span(0, 1),
                    }),
                    rhs: Node::Id(Identifier {
                        value: "b".to_string(),
                        span: Span(4, 5),
                    }),
                    span: Span(0, 5),
                }),
                rhs: Node::Binary(box Binary {
                    operator: Operator {
                        kind: OperatorKind::Div,
                        span: Span(14, 15),
                    },
                    lhs: Node::Binary(box Binary {
                        operator: Operator {
                            kind: OperatorKind::Mul,
                            span: Span(10, 11),
                        },
                        lhs: Node::Id(Identifier {
                            value: "c".to_string(),
                            span: Span(8, 9),
                        }),
                        rhs: Node::Id(Identifier {
                            value: "d".to_string(),
                            span: Span(12, 13),
                        }),
                        span: Span(8, 13),
                    }),
                    rhs: Node::Id(Identifier {
                        value: "e".to_string(),
                        span: Span(16, 17),
                    }),
                    span: Span(8, 17),
                }),
                span: Span(0, 17),
            }),
        );
    }

    #[test]
    fn test_empty_partial_operation() {
        assert_eq!(
            expr("(+)"),
            Node::Partial(box Partial {
                operator: Operator {
                    kind: OperatorKind::Add,
                    span: Span(1, 2)
                },
                lhs: None,
                rhs: None,
                span: Span(1, 2)
            })
        );
    }

    #[test]
    fn test_left_partial_operation() {
        assert_eq!(
            expr("(+ x)"),
            Node::Partial(box Partial {
                operator: Operator {
                    kind: OperatorKind::Add,
                    span: Span(1, 2)
                },
                rhs: Some(Node::Id(
                    Identifier {
                        value: "x".to_string(),
                        span: Span(3, 4)
                    })
                ),
                lhs: None,
                span: Span(1, 4)
            })
        );
    }

    #[test]
    fn test_right_partial_operation() {
        assert_eq!(
            expr("(x +)"),
            Node::Partial(box Partial {
                operator: Operator {
                    kind: OperatorKind::Add,
                    span: Span(3, 4)
                },
                lhs: Some(Node::Id(
                    Identifier {
                        value: "x".to_string(),
                        span: Span(1, 2)
                    })
                ),
                rhs: None,
                span: Span(1, 4)
            })
        );
    }

    #[test]
    fn test_function_call() {
        assert_eq!(
            expr(r#"print("hello, world")"#),
            Node::Call(box Call {
                function: Node::Id(Identifier {
                    value: "print".to_string(),
                    span: Span(0, 5)
                }),
                arguments: vec![
                    Argument {
                        value: Node::String(
                            Literal {
                                value: "\"hello, world\"".to_string(),
                                span: Span(6, 20)
                            }
                        ),
                        name: None,
                        span: Span(6, 20)
                    }
                ],
                span: Span(0, 21)
            })
        )
    }

    #[test]
    fn test_function_call_with_arguments() {
        assert_eq!(
            expr("point(x=1, y=0, z=5)"),
            Node::Call(box Call {
                function: Node::Id(Identifier {
                    value: "point".to_string(),
                    span: Span(0, 5)
                }),
                arguments: vec![
                    Argument {
                        name: Some(Identifier {
                            value: "x".to_string(),
                            span: Span(6, 7)
                        }),
                        value: Node::Number(
                            Literal {
                                value: "1".to_string(),
                                span: Span(8, 9)
                            }
                        ),
                        span: Span(6, 9)
                    },

                    Argument {
                        name: Some(Identifier {
                            value: "y".to_string(),
                            span: Span(11, 12)
                        }),
                        value: Node::Number(
                            Literal {
                                value: "0".to_string(),
                                span: Span(13, 14)
                            }
                        ),
                        span: Span(11, 14)
                    },

                    Argument {
                        name: Some(Identifier {
                            value: "z".to_string(),
                            span: Span(16, 17)
                        }),
                        value: Node::Number(
                            Literal {
                                value: "5".to_string(),
                                span: Span(18, 19)
                            }
                        ),
                        span: Span(16, 19)
                    },
                ],
                span: Span(0, 20)
            })
        );
    }
}
