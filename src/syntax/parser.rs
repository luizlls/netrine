use crate::{Source, Span};
use super::ast::*;
use super::token::{Token, TokenKind};
use super::lexer::Lexer;
use crate::error::{Result, NetrineError};

#[derive(Debug, Clone)]
struct Parser<'s> {
    lexer: Lexer<'s>,
    prev:  Token,
    token: Token,
    peek:  Token,
    source: &'s Source,
}

impl<'s> Parser<'s> {
    fn new(source: &'s Source) -> Parser {
        let mut parser = Parser {
            source,
            lexer: Lexer::new(&source.content),
            prev : Token::default(),
            token: Token::default(),
            peek : Token::default(),
        };

        parser.bump(); // token
        parser.bump(); // peek

        parser
    }

    fn parse_module(mut self) -> Result<Module> {
        let mut expressions = vec![];

        while !self.done() {
            expressions.push(self.parse_expr()?);
        }

        Ok(Module { expressions })
    }

    fn parse_expr(&mut self) -> Result<Expression> {
        match self.token.kind {
            TokenKind::Fn => self.parse_fn(),
            TokenKind::If => self.parse_if(),
            TokenKind::Lower if self.match_peek(TokenKind::Equals) => {
                self.parse_def()
            }
            _ => self.parse_binary(0)
        }
    }

    fn match_term(&self) -> bool {
        matches!(self.token.kind,
            TokenKind::Lower
          | TokenKind::Upper
          | TokenKind::Number
          | TokenKind::String
          | TokenKind::LParen
          | TokenKind::LBrace
          | TokenKind::LBracket
          | TokenKind::Hash)
    }

    fn parse_term(&mut self) -> Result<Expression> {
        match self.token.kind {
            TokenKind::It => self.parse_it(),
            TokenKind::Lower  => self.parse_lower(),
            TokenKind::Upper  => self.parse_upper(),
            TokenKind::Number => self.parse_number(),
            TokenKind::String => self.parse_string(),
            TokenKind::LParen => self.parse_parens(),
            TokenKind::LBrace => self.parse_record(),
            TokenKind::LBracket => self.parse_brackets(),
            _ => {
                Err(self.unexpected())
            }
        }
    }

    fn parse_name(&mut self) -> Result<Name> {
        let span  = self.expect(TokenKind::Lower)?;
        let value = self.source.content[span.range()].into();
        Ok(Name { value, span })
    }

    fn parse_lower(&mut self) -> Result<Expression> {
        Ok(Expression::Name(self.parse_name()?))
    }

    fn parse_upper(&mut self) -> Result<Expression> {
        let start = self.expect(TokenKind::Upper)?;
        let value = self.source.content[start.range()].to_string();

        match &value[..] {
            "True"  => return Ok(Expression::True(start)),
            "False" => return Ok(Expression::False(start)),
            _ => {}
        }

        let name = Name { value, span: start };

        let value = if self.match_lines() && self.match_term() {
            match self.token.kind {
                TokenKind::LParen => {
                    Some(self.parse_parens()?)
                }
                TokenKind::LBrace => {
                    Some(self.parse_record()?)
                }
                TokenKind::Lower => {
                    Some(self.parse_term()?)
                }
                _ => return Err(self.unexpected())
            }
        } else {
            None
        };

        Ok(Expression::Variant(
            box Variant { name, value, span: self.span(start) }))
    }

    fn parse_number(&mut self) -> Result<Expression> {
        let span  = self.expect(TokenKind::Number)?;
        let value = self.source.content[span.range()].into();
        Ok(Expression::Number(Literal { value, span }))
    }

    fn parse_string(&mut self) -> Result<Expression> {
        let span  = self.expect(TokenKind::String)?;
        let value = self.source.content[span.range()].into();
        Ok(Expression::String(Literal { value, span }))
    }

    fn parse_it(&mut self) -> Result<Expression> {
        let span = self.expect(TokenKind::It)?;
        Ok(Expression::It(span))
    }

    fn parse_fn(&mut self) -> Result<Expression> {
        let start = self.token.span;
        
        self.expect(TokenKind::Fn)?;

        let name = self.parse_name()?;

        let parameters = self.parse_sequence_until(
            TokenKind::Colon,
            Self::parse_parameter)?;

        let value = self.parse_block()?;

        Ok(Expression::Fn(
            box Fn { name, parameters, value, span: self.span(start) }))
    }

    fn parse_block(&mut self) -> Result<Expression> {
        let start = self.token.span;

        self.expect(TokenKind::Colon)?;

        let parameters = if self.match_lines() {
            self.parse_sequence_until(
                TokenKind::Arrow,
                Self::parse_parameter)?
        } else {
            vec![]
        };

        let mut expressions = vec![];

        Ok(Expression::Block(
            box Block { parameters, expressions, span: self.span(start) } ))
    }

    fn parse_parens(&mut self) -> Result<Expression> {
        let start = self.token.span;

        let mut values = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_expr)?;

        if values.len() == 1 {
            Ok(values.pop().unwrap())
        } else {
            Ok(Expression::Tuple(
                box Tuple { values, span: self.span(start) }))
        }
    }

    fn parse_brackets(&mut self) -> Result<Expression> {
        let start = self.token.span;

        let values = self.parse_sequence_of(
            TokenKind::LBrace,
            TokenKind::RBrace,
            Self::parse_expr)?;

        Ok(Expression::List(
            box List { values, span: self.span(start) }))
    }

    fn parse_record(&mut self) -> Result<Expression> {
        let start = self.token.span;

        let properties = self.parse_sequence_of(
            TokenKind::LBrace,
            TokenKind::RBrace,
            Self::parse_property)?;

        Ok(Expression::Record(
            box Record { properties, span: self.span(start) }))
    }

    fn parse_property(&mut self) -> Result<(Name, Option<Expression>)> {
        let key = self.parse_name()?;

        let val = match self.token.kind {
            TokenKind::Equals => {
                self.bump();
                Some(self.parse_expr()?)
            }
            TokenKind::LBrace => {
                Some(self.parse_record()?)
            }
            TokenKind::Comma
          | TokenKind::RBrace => {
                None
            }
            _ => return Err(self.unexpected())
        };

        Ok((key, val))
    }

    fn parse_def(&mut self) -> Result<Expression> {
        let start = self.token.span;

        let name = self.parse_name()?;
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expr()?;

        Ok(Expression::Def(
            box Def { name, value, span: self.span(start) }))
    }

    fn parse_parameter(&mut self) -> Result<Parameter> {
        let start = self.token.span;

        let name = self.parse_name()?;

        let value = if self.match_lines() && self.maybe(TokenKind::Equals) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Parameter { name, value, span: self.span(start) })
    }

    fn parse_if(&mut self) -> Result<Expression> {
        let start = self.token.span;

        self.expect(TokenKind::If)?;
        let pred = self.parse_expr()?;

        self.expect(TokenKind::Then)?;
        let then = self.parse_expr()?;

        self.expect(TokenKind::Else)?;
        let otherwise = Some(self.parse_expr()?);

        Ok(Expression::If(
            box If { pred, then, otherwise, span: self.span(start), }))
    }

    fn parse_initial(&mut self) -> Result<Expression> {
        let mut expression = self.parse_term()?;

        while !self.done() && self.match_lines() {
            match self.token.kind {
                TokenKind::LParen => {
                    expression = self.parse_call(expression)?;
                }
                TokenKind::Dot => {
                    expression = self.parse_dot(expression)?;
                }
                _ => return Ok(expression)
            }
        }

        Ok(expression)
    }

    fn parse_call(&mut self, callee: Expression) -> Result<Expression> {
        let start = callee.span();

        let arguments = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_argument)?;

        Ok(Expression::Call(
            box Call { callee, arguments, span: self.span(start) }))
    }

    fn parse_argument(&mut self) -> Result<Argument> {
        let start = self.token.span;
        let value = self.parse_expr()?;
        Ok(Argument { name: None, value, span: self.span(start) })
    }

    fn parse_dot(&mut self, source: Expression) -> Result<Expression> {
        let start = source.span();

        self.expect(TokenKind::Dot)?;
        let value = self.parse_term()?;

        Ok(Expression::Get(
            box Get { source, value, span: self.span(start) }))
    }

    fn parse_unary(&mut self) -> Result<Expression> {
        if !self.token.is_operator() {
            return self.parse_initial();
        }

        let operator = self.parse_operator()?;

        if self.prev.is_opening()
        && self.peek.is_closing() {
            let span = operator.span;
            return Ok(Expression::Partial(
                box Partial { operator, left: None, right: None, span, }));
        }

        self.bump();

        if !self.match_lines() {
            return Err(NetrineError::error(
                self.token.span,
                "Unary or partial operators must be in the same line as the operand".into()));
        }

        let right = self.parse_expr()?;

        let span = Span::from(operator.span, right.span());

        if operator.is_unary() {
            Ok(Expression::Unary(
                box Unary { operator, right, span }))
        } else {
            Ok(Expression::Partial(
                box Partial { operator, left: None, right: Some(right), span, }))
        }
    }

    fn parse_binary(&mut self, minimum: u8) -> Result<Expression> {
        let mut expr = self.parse_unary()?;

        while self.token.is_operator() {
            if let Some(precedence) = self.token.precedence() {

                if precedence < minimum {
                    break;
                }

                let operator = self.parse_operator()?;
                self.bump();

                if self.token.is_closing() {
                    let span = Span::from(expr.span(), operator.span);
                    return Ok(Expression::Partial(
                        box Partial { operator, left: Some(expr), right: None, span }))
                }

                let right = self.parse_binary(precedence + 1)?;
                let left  = expr;

                let span = Span::from(left.span(), right.span());

                expr = Expression::Binary(
                    box Binary { operator, left, right, span });
            }
        }

        Ok(expr)
    }

    fn parse_operator(&self) -> Result<Operator> {
        let span = self.token.span;

        let kind = match self.token.kind {
            TokenKind::Add => OperatorKind::Add,
          | TokenKind::Sub => OperatorKind::Sub,
          | TokenKind::Mul => OperatorKind::Mul,
          | TokenKind::Div => OperatorKind::Div,
          | TokenKind::Rem => OperatorKind::Rem,
          | TokenKind::And => OperatorKind::And,
          | TokenKind::Or  => OperatorKind::Or,
          | TokenKind::Not => OperatorKind::Not,
          | TokenKind::Eq  => OperatorKind::Eq,
          | TokenKind::Ne  => OperatorKind::Ne,
          | TokenKind::Lt  => OperatorKind::Lt,
          | TokenKind::Le  => OperatorKind::Le,
          | TokenKind::Gt  => OperatorKind::Gt,
          | TokenKind::Ge  => OperatorKind::Ge,
          | TokenKind::Pipe => OperatorKind::Pipe,
          | TokenKind::Range  => OperatorKind::Range,
            _ => {
                return Err(NetrineError::error(
                    span,
                    format!("`{}` is not a valid operator", self.token.kind)));
            }
        };

        Ok(Operator { kind, span })
    }

    fn parse_sequence_until<T, F>(
        &mut self,
        until: TokenKind,
        mut f: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>
    {
        let mut result = vec![];

        if self.match_token(until) {
            return Ok(result)
        }
         
        while !self.done()
           && !self.match_token(until) {

            result.push(f(self)?);

            if !self.maybe(TokenKind::Comma) {
                break;
            }
        }

        Ok(result)
    }

    fn parse_sequence_of<T, F>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        mut f: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>
    {
        let mut result = vec![];

        self.expect(open)?;
         
        while !self.done()
           && !self.match_token(close) {

            result.push(f(self)?);

            if !self.maybe(TokenKind::Comma) {
                break;
            }
        }

        self.expect(close)?;

        Ok(result)
    }

    fn bump(&mut self) -> Span {
        self.prev  = self.token;
        self.token = self.peek;
        self.peek  = if let Some(token) = self.lexer.next() {
            token
        } else {
            let span = self.token.span;
            Token {
                kind: TokenKind::EOF,
                span: Span::new(span.line, span.end, span.end),
            }
        };

        self.prev.span
    }

    fn done(&self) -> bool {
        self.token.kind == TokenKind::EOF
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Span> {
        if self.token.kind == expected {
            Ok(self.bump())
        } else {
            Err(NetrineError::error(
                self.token.span,
                format!("Expected `{}` but found `{}`", expected, self.token.kind)))
        }
    }

    fn maybe(&mut self, expected: TokenKind) -> bool {
        if self.token.kind == expected {
            self.bump();
            true
        } else {
            false
        }
    }

    fn span(&self, start: Span) -> Span {
        Span::from(start, self.prev.span)
    }

    fn match_token(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn match_peek(&self, kind: TokenKind) -> bool {
        self.peek.kind == kind
    }

    fn match_lines(&self) -> bool {
        self.prev.span.line == self.token.span.line
    }

    fn unexpected(&mut self) -> NetrineError {
        let msg = match self.token.kind {
            TokenKind::Error(_) => {
                format!("{}", self.token.kind)
            }
            _ => {
                format!("Unexpected `{}`", self.token.kind)
            }
        };

        NetrineError::error(self.token.span, msg)
    }
}

pub fn parse(source: &Source) -> Result<Module> {
    Parser::new(source).parse_module()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    fn source(code: &str) -> Source {
        Source {
            path: PathBuf::from("test"), content: code.into()
        }
    }

    fn expr(code: &str) -> Expression {
        Parser::new(&source(&code)).parse_expr().unwrap()
    }

    fn module(code: &str) -> Module {
        Parser::new(&source(&code)).parse_module().unwrap()
    }

    fn top_level(code: &str) -> Expression {
        module(code).expressions.pop().unwrap()
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
        assert_eq!(
            module(""),
            Module {
                expressions: Vec::new()
            }
        );

        assert_eq!(
            module("  "),
            Module {
                expressions: Vec::new()
            }
        );

        assert_eq!(
            module("\n\n\n"),
            Module {
                expressions: Vec::new()
            }
        );
    }

    #[test]
    fn test_name() {
        assert_eq!(
            expr("netrine"),
            Expression::Name(
                Name {
                    value: "netrine".into(), span: Span::new(1, 0, 7)
                }
            )
        );

        assert_eq!(
            expr("maybe?"),
            Expression::Name(
                Name {
                    value: "maybe?".into(), span: Span::new(1, 0, 6)
                }
            )
        );

        assert_eq!(
            expr("dangerous!"),
            Expression::Name(
                Name {
                    value: "dangerous!".into(), span: Span::new(1, 0, 10)
                }
            )
        )
    }

    #[test]
    fn test_literals() {
        assert_eq!(
            expr("42"),
            Expression::Number(
                Literal {
                    value: "42".into(), span: Span::new(1, 0, 2)
                }
            )
        );

        assert_eq!(
            expr("3.14"),
            Expression::Number(
                Literal {
                    value: "3.14".into(), span: Span::new(1, 0, 4)
                }
            )
        );

        assert_eq!(
            expr("\"hello\""),
            Expression::String(
                Literal {
                    value: "\"hello\"".into(), span: Span::new(1, 0, 7)
                }
            )
        );

        assert_eq!(
            expr("True"),
            Expression::True(Span::new(1, 0, 4))
        );

        assert_eq!(
            expr("False"),
            Expression::False(Span::new(1, 0, 5))
        );

        assert_eq!(
            expr("it"),
            Expression::It(Span::new(1, 0, 2))
        );

        assert_error_expr!("\"unclosed");

        assert_error_expr!("1.");
    }

    #[test]
    fn test_basic_definition() {
        assert_eq!(
            top_level("value = 10"),
            Expression::Def(
                box Def {
                    name: Name {
                        value: "value".into(), span: Span::new(1, 0, 5)
                    },
                    value: Expression::Number(
                        Literal {
                            value: "10".into(), span: Span::new(1, 8, 10)
                        }
                    ),
                    span: Span::new(1, 0, 10)
                }
            )
        );

        assert_error_top_level!("value = ");
    }

    #[test]
    #[ignore = "not implemented yet"]
    fn test_destructing_definition() {
        unimplemented!()
    }

    #[test]
    fn test_basic_function() {
        assert_eq!(
            top_level("function fn: 1"),
            Expression::Fn(
                box Fn {
                    parameters: vec![],
                    name: Name {
                        value: "fn".into(), span: Span::new(1, 0, 2)
                    },
                    value: Expression::Number(
                        Literal {
                            value: "1".into(), span: Span::new(1, 7, 8)
                        }
                    ),
                    span: Span::new(1, 0, 8)
                }
            )
        );

        assert_error_top_level!("function fn");
    }

    #[test]
    fn test_basic_multiline_function() {
        assert_eq!(
            top_level("function fn:
                         multiline"),
            Expression::Fn(
                box Fn {
                    parameters: vec![],
                    name: Name {
                        value: "fn".into(), span: Span::new(1, 0, 2)
                    },
                    value: Expression::Name(
                        Name {
                            value: "multiline".into(), span: Span::new(2, 33, 42)
                        }
                    ),
                    span: Span::new(1, 0, 42)
                }
            )
        );
    }

    #[test]
    fn test_multiline_block_function() {
        assert_eq!(
            top_level("function fn:
                         a = 1
                         b = 2"),
            Expression::Fn(
                box Fn {
                    parameters: vec![],
                    name: Name {
                        value: "fn".into(), span: Span::new(1, 0, 2)
                    },
                    value: Expression::Block(
                        box Block {
                            parameters: vec![],
                            expressions: vec![
                                Expression::Def(
                                    box Def {
                                        name: Name {
                                            value: "a".into(), span: Span::new(2, 32, 33)
                                        },
                                        value: Expression::Number(
                                            Literal {
                                                value: "1".into(), span: Span::new(2, 36, 37)
                                            }
                                        ),
                                        span: Span::new(2, 32, 37)
                                    }
                                ),
                                Expression::Def(
                                    box Def {
                                        name: Name {
                                            value: "b".into(), span: Span::new(3, 63, 64)
                                        },
                                        value: Expression::Number(
                                            Literal {
                                                value: "2".into(), span: Span::new(3, 67, 68)
                                            }
                                        ),
                                        span: Span::new(3, 63, 68)
                                    }
                                )
                            ],
                            span: Span::new(1, 5, 93)
                        }
                    ),
                    span: Span::new(1, 0, 93)
                }
            )
        );

        assert_eq!(
            top_level("function: _"),
            Expression::Fn(
                box Fn {
                    parameters: vec![],
                    name: Name {
                        value: "fn".into(), span: Span::new(1, 0, 2)
                    },
                    value: Expression::Block(
                        box Block {
                            parameters: vec![],
                            expressions: vec![],
                            span: Span::new(1, 5, 7)
                        }
                    ),
                    span: Span::new(1, 0, 7)
                }
            )
        );

        assert_error_top_level!("fn() {");
    }

    #[test]
    fn test_basic_function_with_parameters() {
        assert_eq!(
            top_level("fn(x) = x"),
            Expression::Fn(
                box Fn {
                    name: Name {
                        value: "fn".into(), span: Span::new(1, 0, 2)
                    },
                    parameters: vec![
                        Parameter {
                            name: Name {
                                value: "x".into(), span: Span::new(1, 3, 4)
                            },
                            value: None,
                            span: Span::new(1, 3, 4)
                        }
                    ],
                    value: Expression::Name(
                        Name {
                            value: "x".into(), span: Span::new(1, 8, 9)
                        }
                    ),
                    span: Span::new(1, 0, 9)
                }
            )
        )
    }

    #[test]
    fn test_basic_function_with_default_values() {
        assert_eq!(
            top_level("fn(x = 2) = x"),
            Expression::Fn(
                box Fn {
                    name: Name {
                        value: "fn".into(), span: Span::new(1, 0, 2)
                    },
                    parameters: vec![
                        Parameter {
                            name: Name {
                                value: "x".into(), span: Span::new(1, 3, 4)
                            },
                            value: Some(Expression::Number(
                                Literal {
                                    value: "2".into(), span: Span::new(1, 7, 8)
                                }
                            )),
                            span: Span::new(1, 3, 8)
                        }
                    ],
                    value: Expression::Name(
                        Name {
                            value: "x".into(), span: Span::new(1, 12, 13)
                        }
                    ),
                    span: Span::new(1, 0, 13)
                }
            )
        );

        assert_error_expr!("f(x = ) = x");
    }

    #[test]
    fn test_get_property_access() {
        assert_eq!(
            expr("a.b"),
            Expression::Get(
                box Get {
                    source: Expression::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expression::Name(
                        Name {
                            value: "b".into(), span: Span::new(1, 2, 3),
                        }
                    ),
                    span: Span::new(1, 0, 3)
                }
            )
        );

        assert_eq!(
            expr("a.b.c"),
            Expression::Get(
                box Get {
                    source: Expression::Get(
                        box Get {
                            source: Expression::Name(
                                Name {
                                    value: "a".into(), span: Span::new(1, 0, 1),
                                }
                            ),
                            value: Expression::Name(
                                Name {
                                    value: "b".into(), span: Span::new(1, 2, 3),
                                }
                            ),
                            span: Span::new(1, 0, 3)
                        }
                    ),
                    value: Expression::Name(
                        Name {
                            value: "c".into(), span: Span::new(1, 4, 5),
                        }
                    ),
                    span: Span::new(1, 0, 5)
                }
            )
        );

        assert_eq!(
            expr("a.0"),
            Expression::Get(
                box Get {
                    source: Expression::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expression::Number(
                        Literal {
                            value: "0".into(), span: Span::new(1, 2, 3),
                        }
                    ),
                    span: Span::new(1, 0, 3)
                }
            )
        );

        assert_eq!(
            expr("a.\"string\""),
            Expression::Get(
                box Get {
                    source: Expression::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expression::String(
                        Literal {
                            value: "\"string\"".into(), span: Span::new(1, 2, 10),
                        }
                    ),
                    span: Span::new(1, 0, 10)
                }
            )
        );

        assert_eq!(
            expr("a.[x, y]"),
            Expression::Get(
                box Get {
                    source: Expression::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expression::List(
                        box List {
                            values: vec![
                                Expression::Name(
                                    Name { value: "x".into(), span: Span::new(1, 3, 4) }
                                ),
                                Expression::Name(
                                    Name { value: "y".into(), span: Span::new(1, 6, 7) }
                                )
                            ],
                            span: Span::new(1, 2, 8)
                        }
                    ),
                    span: Span::new(1, 0, 8)
                }
            )
        );

        assert_error_expr!("a.");
    }

    #[test]
    fn test_list() {
        assert_eq!(
            expr("[]"),
            Expression::List(
                box List {
                    values: vec![], span: Span::new(1, 0, 2)
                }
            )
        );

        assert_eq!(
            expr("[1,]"),
            Expression::List(
                box List {
                    values: vec![
                        Expression::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                    ],
                    span: Span::new(1, 0, 4)
                }
            )
        );

        assert_eq!(
            expr("[x, y, z]"),
            Expression::List(
                box List {
                    values: vec![
                        Expression::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expression::Name(
                            Name { value: "y".into(), span: Span::new(1, 4, 5) }
                        ),
                        Expression::Name(
                            Name { value: "z".into(), span: Span::new(1, 7, 8) }
                        )
                    ],
                    span: Span::new(1, 0, 9)
                }
            )
        );

        assert_eq!(
            expr("[x,
                   y
                  ,z]"),
            Expression::List(
                box List {
                    values: vec![
                        Expression::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expression::Name(
                            Name { value: "y".into(), span: Span::new(2, 23, 24) }
                        ),
                        Expression::Name(
                            Name { value: "z".into(), span: Span::new(3, 44, 45) }
                        )
                    ],
                    span: Span::new(1, 0, 46)
                }
            )
        );

        assert_eq!(
            expr("[1, \"a\", [3.14]]"),
            Expression::List(
                box List {
                    values: vec![
                        Expression::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expression::String(
                            Literal { value: "\"a\"".into(), span: Span::new(1, 4, 7) }
                        ),
                        Expression::List(
                            box List {
                                values: vec![
                                    Expression::Number(
                                        Literal { value: "3.14".into(), span: Span::new(1, 10, 14) }
                                    ),
                                ],
                                span: Span::new(1, 9, 15)
                            }
                        )
                    ],
                    span: Span::new(1, 0, 16)
                }
            )
        );

        assert_error_expr!("[x y z]");

        assert_error_expr!("[");

        assert_error_expr!("]");
    }

    #[test]
    fn test_tuple() {
        assert_eq!(
            expr("()"),
            Expression::Tuple(
                box Tuple {
                    values: vec![], span: Span::new(1, 0, 2)
                }
            )
        );

        assert_eq!(
            expr("(1)"),
            Expression::Number(
                Literal {
                    value: "1".into(), span: Span::new(1, 1, 2)
                }
            )
        );

        assert_eq!(
            expr("(1,)"),
            Expression::Tuple(
                box Tuple {
                    values: vec![
                        Expression::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                    ],
                    span: Span::new(1, 0, 4)
                }
            )
        );

        assert_eq!(
            expr("(x, y, z)"),
            Expression::Tuple(
                box Tuple {
                    values: vec![
                        Expression::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expression::Name(
                            Name { value: "y".into(), span: Span::new(1, 4, 5) }
                        ),
                        Expression::Name(
                            Name { value: "z".into(), span: Span::new(1, 7, 8) }
                        )
                    ],
                    span: Span::new(1, 0, 9)
                }
            )
        );

        assert_eq!(
            expr("(x,
                   y
                  ,z)"),
            Expression::Tuple(
                box Tuple {
                    values: vec![
                        Expression::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expression::Name(
                            Name { value: "y".into(), span: Span::new(2, 23, 24) }
                        ),
                        Expression::Name(
                            Name { value: "z".into(), span: Span::new(3, 44, 45) }
                        )
                    ],
                    span: Span::new(1, 0, 46)
                }
            )
        );

        assert_eq!(
            expr("(1, \"a\", ())"),
            Expression::Tuple(
                box Tuple {
                    values: vec![
                        Expression::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expression::String(
                            Literal { value: "\"a\"".into(), span: Span::new(1, 4, 7) }
                        ),
                        Expression::Tuple(
                            box Tuple {
                                values: vec![], span: Span::new(1, 9, 11)
                            }
                        )
                    ],
                    span: Span::new(1, 0, 12)
                }
            )
        );

        assert_error_expr!("(x y z)");

        assert_error_expr!("(");

        assert_error_expr!(")");
    }

    #[test]
    fn test_dict() {
        assert_eq!(
            expr("[]"),
            Expression::List(
                box List {
                    values: vec![], span: Span::new(1, 0, 3)
                }
            )
        );

        assert_eq!(
            expr("[x = 1, y = 2, z = 3]"),
            Expression::Dict(
                box Dict {
                    values: vec![
                        (
                            Expression::Name(
                                Name { value: "x".into(), span: Span::new(1, 1, 2) }
                            ),
                            Expression::Number(
                                Literal { value: "1".into(), span: Span::new(1, 5, 6) }
                            ),
                        ),
                        (
                            Expression::Name(
                                Name { value: "y".into(), span: Span::new(1, 8, 9) }
                            ),
                            Expression::Number(
                                Literal { value: "2".into(), span: Span::new(1, 12, 13) }
                            ),
                        ),
                        (
                            Expression::Name(
                                Name { value: "z".into(), span: Span::new(1, 15, 16) }
                            ),
                            Expression::Number(
                                Literal { value: "3".into(), span: Span::new(1, 19, 20) }
                            ),
                        ),
                    ],
                    span: Span::new(1, 0, 21)
                }
            )
        );

        assert_eq!(
            expr("[x = 1,
                   y = 2
                  ,z = 3]"),
            Expression::Dict(
                box Dict {
                    values: vec![
                        (
                            Expression::Name(
                                Name { value: "x".into(), span: Span::new(1, 1, 2) }
                            ),
                            Expression::Number(
                                Literal { value: "1".into(), span: Span::new(1, 4, 5) }
                            ),
                        ),
                        (
                            Expression::Name(
                                Name { value: "y".into(), span: Span::new(2, 26, 27) }
                            ),
                            Expression::Number(
                                Literal { value: "2".into(), span: Span::new(2, 29, 30) }
                            ),
                        ),
                        (
                            Expression::Name(
                                Name { value: "z".into(), span: Span::new(3, 50, 51) }
                            ),
                            Expression::Number(
                                Literal { value: "3".into(), span: Span::new(3, 53, 54) }
                            ),
                        ),
                    ],
                    span: Span::new(1, 0, 55)
                }
            )
        );

        assert_eq!(
            expr("[1 = \"a\", list = [[nested = \"values\"]]]"),
            Expression::Dict(
                box Dict {
                    values: vec![
                        (
                            Expression::Number(
                                Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                            ),
                            Expression::String(
                                Literal { value: "\"a\"".into(), span: Span::new(1, 5, 8) }
                            ),
                        ),
                        (
                            Expression::Name(
                                Name { value: "list".into(), span: Span::new(1, 10, 14) }
                            ),
                            Expression::List(
                                box List {
                                    values: vec![
                                        Expression::Dict(
                                            box Dict {
                                                values: vec![
                                                    (
                                                        Expression::Name(
                                                            Name { value: "nested".into(), span: Span::new(1, 19, 25) }
                                                        ),
                                                        Expression::String(
                                                            Literal { value: "\"values\"".into(), span: Span::new(1, 28, 36) }
                                                        ),
                                                    ),
                                                ],
                                                span: Span::new(1, 18, 37)
                                            }
                                        )
                                    ],
                                    span: Span::new(1, 17, 38)
                                }
                            ),
                        ),
                    ],
                    span: Span::new(1, 0, 39)
                }
            )
        );

        assert_error_expr!("[x = 1 y = 2]");

        assert_error_expr!("[x =]");

        assert_error_expr!("[= x]");

        assert_error_expr!("[=]");
    }
}
