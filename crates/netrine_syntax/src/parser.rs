use super::ast::*;
use super::lexer::Lexer;
use super::token::{Token, TokenKind};

use netrine_core::{NetrineError, Result, Source, Span};

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
        let mut parser = Parser {
            lexer: Lexer::new(&source.content),
            source,
            token: Token::default(),
            prev : Token::default(),
            peek : Token::default(),
        };

        parser.bump(); // peek token
        parser.bump(); // token token
        parser
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::LParen => self.parse_form(),
            TokenKind::Lower => self.parse_lower(),
            TokenKind::Upper => self.parse_upper(),
            TokenKind::Number => self.parse_number(),
            TokenKind::String => self.parse_string(),
            _ => Err(NetrineError::error(
                self.token.span,
                "Expect a basic expression or form".to_string(),
            )),
        }
    }

    fn parse_form(&mut self) -> Result<Expr> {
        self.expect(TokenKind::LParen)?;

        let mut expr = match self.token.kind {
            TokenKind::Fn => self.parse_fn()?,
            TokenKind::If => self.parse_if()?,
            TokenKind::Do => self.parse_do()?,
            TokenKind::Let => self.parse_let()?,
            TokenKind::Set => self.parse_set()?,
            TokenKind::Get => self.parse_get()?,
            TokenKind::Dot => todo!(),
            TokenKind::LParen
          | TokenKind::Lower
          | TokenKind::Upper
          | TokenKind::Number
          | TokenKind::String => self.parse_expr()?,
            _ => {
                if self.token.is_operator() {
                    todo!()
                } else {
                    todo!()
                }
            }
        };

        if self.maybe(TokenKind::RParen)? {
            return Ok(expr)
        }

        if self.token.is_operator() {
            expr = self.parse_binary(expr, 0)?;
        } else {
            expr = self.parse_apply(expr)?;
        }

        self.expect(TokenKind::RParen)?;

        Ok(expr)
    }

    fn parse_fn(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::Fn)?;

        let name = if self.token.is(TokenKind::Lower) {
            Some(self.parse_name()?)
        } else {
            None
        };

        let parameters = self.parse_sequence_of(
            TokenKind::LBracket,
            TokenKind::RBracket,
            Self::parse_parameter,
        )?;

        let value = self.parse_expr()?;

        let span = self.span(start);

        Ok(Expr::Function(box Function { name, parameters, value, span, }))
    }

    fn parse_parameter(&mut self) -> Result<Parameter> {
        let name = self.parse_name()?;
        let span = name.span;
        Ok(Parameter { name, value: None, span })
    }

    fn parse_if(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::If)?;

        let pred = self.parse_expr()?;
        let then = self.parse_expr()?;
        let otherwise = if self.token.is(TokenKind::RParen) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        let span = self.span(start);

        Ok(Expr::If(box If { pred, then, otherwise, span }))
    }

    fn parse_do(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::Do)?;

        let expressions = self.parse_sequence_until(
            TokenKind::RParen,
            Self::parse_expr,
        )?;

        let span = self.span(start);

        Ok(Expr::Do(box Do { expressions, span }))
    }

    fn parse_let(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::Let)?;

        let name = self.parse_name()?;
        let value = self.parse_expr()?;
        
        let span = self.span(start);

        Ok(Expr::Let(box Let { name, value, span }))
    }

    fn parse_set(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::Set)?;

        let name = self.parse_name()?;
        let value = self.parse_expr()?;
        
        let span = self.span(start);

        Ok(Expr::Let(box Let { name, value, span }))
    }

    fn parse_get(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::Get)?;

        let from = self.parse_expr()?;
        let value = self.parse_expr()?;
        
        let span = self.span(start);

        Ok(Expr::Get(box Get { from, value, span }))
    }

    fn parse_apply(&mut self, main: Expr) -> Result<Expr> {
        let arguments = self.parse_sequence_until(
            TokenKind::RParen,
            Self::parse_expr,
        )?;

        let span = self.span(main.span());

        Ok(Expr::Apply(box Apply { main, arguments, span }))
    }

    fn parse_name(&mut self) -> Result<Name> {
        let span = self.expect(TokenKind::Lower)?;
        let value = self.source.content[span.range()].into();
        Ok(Name { value, span })
    }

    fn parse_lower(&mut self) -> Result<Expr> {
        Ok(Expr::Name(self.parse_name()?))
    }

    fn parse_upper(&mut self) -> Result<Expr> {
        let span = self.expect(TokenKind::Upper)?;
        let value = self.source.content[span.range()].to_string();

        match &value[..] {
            "True" => return Ok(Expr::True(span)),
            "False" => return Ok(Expr::False(span)),
            _ => {}
        }

        let name = Name { value, span };
        let value = None;

        let span = self.span(span);

        Ok(Expr::Variant(box Variant { name, value, span }))
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let span = self.expect(TokenKind::Number)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::Number(Literal { value, span }))
    }

    fn parse_string(&mut self) -> Result<Expr> {
        let span = self.expect(TokenKind::String)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::String(Literal { value, span }))
    }

    fn parse_binary(&mut self, expr: Expr, minimum: u8) -> Result<Expr> {
        let mut expr = expr;
        
        while self.token.is_operator() {
            if let Some(precedence) = self.token.precedence() {
                if precedence < minimum {
                    break;
                }

                let operator = self.parse_operator()?;
                self.bump()?;

                let next = self.parse_expr()?;
                let right = self.parse_binary(next, precedence + 1)?;
                let left = expr;

                let span = Span::from(
                    left.span(),
                    right.span(),
                );

                expr = Expr::Binary(box Binary { operator, left, right, span });
            }
        }

        Ok(expr)
    }

    fn parse_operator(&self) -> Result<Operator> {
        let span = self.token.span;

        let kind = match self.token.kind {
            TokenKind::Add => OperatorKind::Add,
            TokenKind::Sub => OperatorKind::Sub,
            TokenKind::Mul => OperatorKind::Mul,
            TokenKind::Div => OperatorKind::Div,
            TokenKind::Rem => OperatorKind::Rem,
            TokenKind::And => OperatorKind::And,
            TokenKind::Or => OperatorKind::Or,
            TokenKind::Is => OperatorKind::Is,
            TokenKind::Eq => OperatorKind::Eq,
            TokenKind::Ne => OperatorKind::Ne,
            TokenKind::Lt => OperatorKind::Lt,
            TokenKind::Le => OperatorKind::Le,
            TokenKind::Gt => OperatorKind::Gt,
            TokenKind::Ge => OperatorKind::Ge,
            TokenKind::Pipe => OperatorKind::Pipe,
            TokenKind::Range => OperatorKind::Range,
            _ => {
                return Err(NetrineError::error(
                    span,
                    format!("`{}` is not a valid operator", self.token.kind),
                ));
            }
        };

        Ok(Operator { kind, span })
    }

    fn parse_while<T, P, F>(&mut self, pred: P, mut f: F) -> Result<Vec<T>>
    where
        P: std::ops::Fn(&Self) -> bool,
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        while pred(self) {
            result.push(f(self)?);
        }

        Ok(result)
    }

    fn parse_sequence_until<T, F>(&mut self, until: TokenKind, mut f: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        if self.token.is(until) {
            return Ok(result);
        }

        while !self.done() && !self.token.is(until) {
            result.push(f(self)?);
        }

        Ok(result)
    }

    fn parse_sequence_of<T, F>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        mut f: F,
    ) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.expect(open)?;

        while !self.done() && !self.token.is(close) {
            result.push(f(self)?);
        }

        self.expect(close)?;

        Ok(result)
    }

    fn span(&self, start: Span) -> Span {
        Span::from(start, self.prev.span)
    }

    fn bump(&mut self) -> Result<Span> {
        let span = self.token.span;

        self.prev = self.token;
        self.token = self.peek;
        self.peek = self.lexer.next()?;

        Ok(span)
    }

    fn done(&self) -> bool {
        self.token.kind == TokenKind::EOF
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Span> {
        if self.token.kind == expected {
            self.bump()
        } else {
            Err(NetrineError::error(
                self.token.span,
                format!("Expected `{}` but found `{}`", expected, self.token.kind),
            ))
        }
    }

    fn maybe(&mut self, expected: TokenKind) -> Result<bool> {
        if self.token.kind == expected {
            self.bump()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn unexpected(&mut self) -> NetrineError {
        NetrineError::error(self.token.span, format!("Unexpected `{}`", self.token.kind))
    }
}

pub fn parse(source: &Source) -> Result<Expr> {
    Parser::new(source).parse_form()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    fn source(code: &str) -> Source {
        Source {
            path: PathBuf::from("test"),
            content: code.into(),
        }
    }

    fn expr(code: &str) -> Expr {
        Parser::new(&source(&code)).parse_expr().unwrap()
    }

    fn module(code: &str) -> Module {
        Parser::new(&source(&code)).parse_module().unwrap()
    }

    fn top_level(code: &str) -> Expr {
        module(code).items.pop().unwrap()
    }

    macro_rules! assert_error_expr {
        ($code:expr) => {
            if let Err(_) = Parser::new(&source(&$code)).parse_expr() {
                assert!(true)
            } else {
                assert!(false)
            }
        };
    }

    macro_rules! assert_error_top_level {
        ($code:expr) => {
            if let Err(_) = Parser::new(&source(&$code)).parse_module() {
                assert!(true)
            } else {
                assert!(false)
            }
        };
    }

    #[test]
    fn test_empty_module() {
        assert_eq!(module(""), Module { items: Vec::new() });

        assert_eq!(module("  "), Module { items: Vec::new() });

        assert_eq!(module("\n\n\n"), Module { items: Vec::new() });
    }

    #[test]
    fn test_name() {
        assert_eq!(
            expr("netrine"),
            Expr::Name(Name {
                value: "netrine".into(),
                span: Span::new(1, 0, 7)
            })
        );

        assert_eq!(
            expr("maybe?"),
            Expr::Name(Name {
                value: "maybe?".into(),
                span: Span::new(1, 0, 6)
            })
        );

        assert_eq!(
            expr("dangerous!"),
            Expr::Name(Name {
                value: "dangerous!".into(),
                span: Span::new(1, 0, 10)
            })
        )
    }

    #[test]
    fn test_literals() {
        assert_eq!(
            expr("42"),
            Expr::Number(Literal {
                value: "42".into(),
                span: Span::new(1, 0, 2)
            })
        );

        assert_eq!(
            expr("3.14"),
            Expr::Number(Literal {
                value: "3.14".into(),
                span: Span::new(1, 0, 4)
            })
        );

        assert_eq!(
            expr("\"hello\""),
            Expr::String(Literal {
                value: "\"hello\"".into(),
                span: Span::new(1, 0, 7)
            })
        );

        assert_eq!(expr("True"), Expr::True(Span::new(1, 0, 4)));

        assert_eq!(expr("False"), Expr::False(Span::new(1, 0, 5)));

        assert_eq!(expr("_"), Expr::Any(Span::new(1, 0, 2)));

        assert_error_expr!("\"unclosed");

        assert_error_expr!("1.");
    }

    #[test]
    fn test_basic_definition() {
        assert_eq!(
            top_level("(let value 10)"),
            Expr::Def(box Def {
                name: Name {
                    value: "value".into(),
                    span: Span::new(1, 0, 5)
                },
                value: Expr::Number(Literal {
                    value: "10".into(),
                    span: Span::new(1, 8, 10)
                }),
                span: Span::new(1, 0, 10)
            })
        );

        assert_error_top_level!("(let value)");
    }

    #[test]
    #[ignore = "not implemented yet"]
    fn test_destructing_definition() {
        unimplemented!()
    }

    #[test]
    fn test_basic_function() {
        assert_eq!(
            top_level("(fn name [] 1)"),
            Expr::Fn(box Fn {
                parameters: vec![],
                name: Name {
                    value: "name".into(),
                    span: Span::new(1, 0, 4)
                },
                value: Expr::Number(Literal {
                    value: "1".into(),
                    span: Span::new(1, 7, 8)
                }),
                span: Span::new(1, 0, 8)
            })
        );

        assert_error_top_level!("(fn name [])");
    }

    #[test]
    fn test_basic_multiline_function() {
        assert_eq!(
            top_level(
                "(fn name []
                   multilines)"
            ),
            Expr::Fn(box Fn {
                parameters: vec![],
                name: Name {
                    value: "name".into(),
                    span: Span::new(1, 0, 4)
                },
                value: Expr::Name(Name {
                    value: "multiline".into(),
                    span: Span::new(2, 33, 42)
                }),
                span: Span::new(1, 0, 42)
            })
        );
    }

    #[test]
    fn test_multiline_function() {
        assert_eq!(
            top_level(
                "(fn name []
                   (let a 1)
                   (let b 2))"
            ),
            Expr::Fn(box Fn {
                parameters: vec![],
                name: Name {
                    value: "name".into(),
                    span: Span::new(1, 0, 4)
                },
                value: Expr::Block(box Block {
                    expressions: vec![
                        Expr::Def(box Def {
                            name: Name {
                                value: "a".into(),
                                span: Span::new(2, 32, 33)
                            },
                            value: Expr::Number(Literal {
                                value: "1".into(),
                                span: Span::new(2, 36, 37)
                            }),
                            span: Span::new(2, 32, 37)
                        }),
                        Expr::Def(box Def {
                            name: Name {
                                value: "b".into(),
                                span: Span::new(3, 63, 64)
                            },
                            value: Expr::Number(Literal {
                                value: "2".into(),
                                span: Span::new(3, 67, 68)
                            }),
                            span: Span::new(3, 63, 68)
                        })
                    ],
                    span: Span::new(1, 5, 93)
                }),
                span: Span::new(1, 0, 93)
            })
        );

        assert_eq!(
            top_level("(fn name [] _)"),
            Expr::Block(box Block {
                expressions: vec![Expr::Any(Span::new(1, 5, 6))],
                span: Span::new(1, 5, 7)
            },)
        );

        assert_error_top_level!("(fn name)");
    }

    #[test]
    fn test_basic_function_with_parameters() {
        assert_eq!(
            top_level("(fn name [arg] arg)"),
            Expr::Fn(box Fn {
                name: Name {
                    value: "name".into(),
                    span: Span::new(1, 0, 2)
                },
                parameters: vec![Parameter {
                    name: Expr::Name(Name {
                        value: "arg".into(),
                        span: Span::new(1, 3, 4)
                    }),
                    value: None,
                    span: Span::new(1, 3, 4)
                }],
                value: Expr::Name(Name {
                    value: "arg".into(),
                    span: Span::new(1, 8, 9)
                }),
                span: Span::new(1, 0, 9)
            })
        )
    }

    #[test]
    #[ignore = "not implemented yet"]
    fn test_basic_function_with_default_values() {
        assert_eq!(
            top_level("(fn name [arg:1] arg)"),
            Expr::Fn(box Fn {
                name: Name {
                    value: "name".into(),
                    span: Span::new(1, 0, 2)
                },
                parameters: vec![Parameter {
                    name: Expr::Name(Name {
                        value: "arg".into(),
                        span: Span::new(1, 3, 4)
                    }),
                    value: Some(Expr::Number(Literal {
                        value: "1".into(),
                        span: Span::new(1, 7, 8)
                    })),
                    span: Span::new(1, 3, 8)
                }],
                value: Expr::Name(Name {
                    value: "arg".into(),
                    span: Span::new(1, 12, 13)
                }),
                span: Span::new(1, 0, 13)
            })
        );

        assert_error_expr!("(fn name [arg:])");
    }
}
