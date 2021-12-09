use super::ast::*;
use super::lexer::Lexer;
use super::token::Token;
use crate::{Source, Span};
use crate::error::{Result, NetrineError};

#[derive(Debug, Clone)]
struct Parser<'s> {
    lexer: Lexer<'s>,
    token: Token,
    source: &'s Source,
}

impl<'s> Parser<'s> {
    fn new(source: &'s Source) -> Parser {
        let mut parser = Parser {
            source,
            lexer: Lexer::new(&source.content),
            token: Token::EOF,
        };

        parser.bump(); // token
        parser
    }

    fn parse_module(mut self) -> Result<Module> {
        let mut expressions = vec![];

        while !self.done() {
            expressions.push(self.parse_expr()?);
        }

        Ok(Module { expressions })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token {
            Token::Fn => self.parse_fn(),
            Token::If => self.parse_if(),
            Token::Lower if self.match_peek(Token::Equals) => {
                self.parse_def()
            }
            _ => self.parse_binary(0)
        }
    }

    fn match_term(&self) -> bool {
        matches!(
            self.token,
            Token::Lower
          | Token::Upper
          | Token::Number
          | Token::String
          | Token::LParen
          | Token::LBrace
          | Token::LBracket
          | Token::Anything)
    }

    fn parse_term(&mut self) -> Result<Expr> {
        match self.token {
            Token::Anything => self.parse_anything(),
            Token::Lower    => self.parse_lower(),
            Token::Upper    => self.parse_upper(),
            Token::Number   => self.parse_number(),
            Token::String   => self.parse_string(),
            Token::LParen   => self.parse_parens(),
            Token::LBrace   => self.parse_record(),
            Token::LBracket => self.parse_brackets(),
            _ => {
                Err(self.unexpected())
            }
        }
    }

    fn parse_patt(&mut self) -> Result<Expr> {
        let patt = match self.token {
            Token::Anything => self.parse_anything(),
            Token::Lower    => self.parse_lower(),
            Token::Upper    => self.parse_upper(),
            Token::Number   => self.parse_number(),
            Token::String   => self.parse_string(),
            Token::LParen   => self.parse_parens(),
            Token::LBrace   => self.parse_record(),
            Token::LBracket => self.parse_brackets(),
            _ => {
                Err(self.unexpected())
            }
        }?;

        if !patt.is_pattern() {
            Err(NetrineError::error(patt.span(), "Not a valid pattern".into()))
        } else {
            Ok(patt)
        }
    }

    fn parse_name(&mut self) -> Result<Name> {
        let span  = self.expect(Token::Lower)?;
        let value = self.source.content[span.range()].into();
        Ok(Name { value, span })
    }

    fn parse_lower(&mut self) -> Result<Expr> {
        Ok(Expr::Name(self.parse_name()?))
    }

    fn parse_upper(&mut self) -> Result<Expr> {
        let start = self.expect(Token::Upper)?;
        let value = self.source.content[start.range()].to_string();

        match &value[..] {
            "True"  => return Ok(Expr::True(start)),
            "False" => return Ok(Expr::False(start)),
            _ => {}
        }

        let name = Name { value, span: start };

        let value = if self.match_lines() && self.match_term() {
            match self.token {
                Token::LParen => {
                    Some(self.parse_parens()?)
                }
                Token::LBrace => {
                    Some(self.parse_record()?)
                }
                Token::Lower => {
                    Some(self.parse_term()?)
                }
                _ => return Err(self.unexpected())
            }
        } else {
            None
        };

        Ok(Expr::Variant(
            box Variant { name, value, span: self.span(start) }))
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let span  = self.expect(Token::Number)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::Number(Literal { value, span }))
    }

    fn parse_string(&mut self) -> Result<Expr> {
        let span  = self.expect(Token::String)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::String(Literal { value, span }))
    }

    fn parse_anything(&mut self) -> Result<Expr> {
        let span = self.expect(Token::Anything)?;
        Ok(Expr::Any(span))
    }

    fn parse_fn(&mut self) -> Result<Expr> {
        let start = self.token.span;
        
        self.expect(Token::Fn)?;

        let name = self.parse_name()?;

        let parameters = self.parse_sequence_until(
            Token::Colon,
            Self::parse_parameter)?;

        let value = self.parse_block()?;

        Ok(Expr::Fn(
            box Fn { name, parameters, value, span: self.span(start) }))
    }

    fn parse_block(&mut self) -> Result<Expr> {
        let start = self.token.span;

        self.expect(Token::Colon)?;

        let parameters = if self.match_lines() {
            self.parse_sequence_until(
                Token::Arrow,
                Self::parse_parameter)?
        } else {
            vec![]
        };

        let mut expressions = vec![];

        Ok(Expr::Block(
            box Block { parameters, expressions, span: self.span(start) } ))
    }

    fn parse_parens(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let mut values = self.parse_sequence_of(
            Token::LParen,
            Token::RParen,
            Self::parse_expr)?;

        if values.len() == 1 {
            Ok(values.pop().unwrap())
        } else {
            Ok(Expr::Tuple(
                box Tuple { values, span: self.span(start) }))
        }
    }

    fn parse_brackets(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let values = self.parse_sequence_of(
            Token::LBrace,
            Token::RBrace,
            Self::parse_expr)?;

        Ok(Expr::List(
            box List { values, span: self.span(start) }))
    }

    fn parse_record(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let properties = self.parse_sequence_of(
            Token::LBrace,
            Token::RBrace,
            Self::parse_property)?;

        Ok(Expr::Record(
            box Record { properties, span: self.span(start) }))
    }

    fn parse_property(&mut self) -> Result<(Name, Option<Expr>)> {
        let key = self.parse_name()?;

        let val = match self.token {
            Token::Equals => {
                self.bump();
                Some(self.parse_expr()?)
            }
            Token::LBrace => {
                Some(self.parse_record()?)
            }
            Token::Comma
          | Token::RBrace => {
                None
            }
            _ => return Err(self.unexpected())
        };

        Ok((key, val))
    }

    fn parse_def(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let patt = self.parse_patt()?;
        self.expect(Token::Equals)?;
        let value = self.parse_expr()?;

        Ok(Expr::Def(
            box Def { patt, value, span: self.span(start) }))
    }

    fn parse_parameter(&mut self) -> Result<Param> {
        let start = self.token.span;

        let patt = match self.token {
            Token::Anything    => self.parse_anything(),
            Token::Lower  => self.parse_lower(),
            Token::Upper  => self.parse_upper(),
            Token::LParen => self.parse_parens(),
            Token::LBrace => self.parse_record(),
            Token::LBracket => self.parse_brackets(),
            _ => {
                Err(self.unexpected())
            }
        }?;

        if !patt.is_pattern() {
            return Err(NetrineError::error(
                patt.span(),
                "Not a valid pattern".into()))
        }

        let value = if self.match_lines() && self.maybe(Token::Equals) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Param { patt, value, span: self.span(start) })
    }

    fn parse_if(&mut self) -> Result<Expr> {
        let start = self.token.span;

        self.expect(Token::If)?;
        let pred = self.parse_expr()?;

        self.expect(Token::Then)?;
        let then = self.parse_expr()?;

        self.expect(Token::Else)?;
        let otherwise = Some(self.parse_expr()?);

        Ok(Expr::If(
            box If { pred, then, otherwise, span: self.span(start), }))
    }

    fn parse_call(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let callee = self.parse_initial()?;

        let arguments = self.parse_while(
            Self::match_term,
            Self::parse_argument)?;

        Ok(Expr::Call(
            box Call { callee, arguments, span: self.span(start) }))
    }

    fn parse_argument(&mut self) -> Result<Argument> {
        let start = self.token.span;
        let value = self.parse_expr()?;
        Ok(Argument { name: None, value, span: self.span(start) })
    }

    fn parse_initial(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let mut source = self.parse_term()?;

        while self.match_lines() && self.maybe(Token::Dot) {
            let value = self.parse_term()?;
            source = Expr::Get(
                box Get { source, value, span: self.span(start) })
        }
        
        Ok(source)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        if !self.token.is_operator() {
            return self.parse_initial();
        }

        let operator = self.parse_operator()?;

        if self.prev.is_opening()
        && self.peek.is_closing() {
            let span = operator.span;
            return Ok(Expr::Partial(
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
            Ok(Expr::Unary(
                box Unary { operator, right, span }))
        } else {
            Ok(Expr::Partial(
                box Partial { operator, left: None, right: Some(right), span, }))
        }
    }

    fn parse_binary(&mut self, minimum: u8) -> Result<Expr> {
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
                    return Ok(Expr::Partial(
                        box Partial { operator, left: Some(expr), right: None, span }))
                }

                let right = self.parse_binary(precedence + 1)?;
                let left  = expr;

                let span = Span::from(left.span(), right.span());

                expr = Expr::Binary(
                    box Binary { operator, left, right, span });
            }
        }

        Ok(expr)
    }

    fn parse_operator(&self) -> Result<Operator> {
        let span = self.token.span;

        let kind = match self.token {
            Token::Add   => OperatorKind::Add,
          | Token::Sub   => OperatorKind::Sub,
          | Token::Mul   => OperatorKind::Mul,
          | Token::Div   => OperatorKind::Div,
          | Token::Rem   => OperatorKind::Rem,
          | Token::And   => OperatorKind::And,
          | Token::Or    => OperatorKind::Or,
          | Token::Not   => OperatorKind::Not,
          | Token::Eq    => OperatorKind::Eq,
          | Token::Ne    => OperatorKind::Ne,
          | Token::Lt    => OperatorKind::Lt,
          | Token::Le    => OperatorKind::Le,
          | Token::Gt    => OperatorKind::Gt,
          | Token::Ge    => OperatorKind::Ge,
          | Token::Pipe  => OperatorKind::Pipe,
          | Token::Range => OperatorKind::Range,
            _ => {
                return Err(NetrineError::error(
                    span,
                    format!("`{}` is not a valid operator", self.token)));
            }
        };

        Ok(Operator { kind, span })
    }

    fn parse_while<T, P, F>(
        &mut self,
        pred: P,
        mut f: F) -> Result<Vec<T>>
    where
        P: std::ops::Fn(&Self) -> bool, F: FnMut(&mut Self) -> Result<T>
    {
        let mut result = vec![];
         
        while pred(self) {
            result.push(f(self)?);
        }

        Ok(result)
    }

    fn parse_sequence_until<T, F>(
        &mut self,
        until: Token,
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

            if !self.maybe(Token::Comma) {
                break;
            }
        }

        Ok(result)
    }

    fn parse_sequence_of<T, F>(
        &mut self,
        open: Token,
        close: Token,
        mut f: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>
    {
        let mut result = vec![];

        self.expect(open)?;
         
        while !self.done()
           && !self.match_token(close) {

            result.push(f(self)?);

            if !self.maybe(Token::Comma) {
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
                kind: Token::EOF,
                span: Span::new(span.line, span.end, span.end),
            }
        };

        self.prev.span
    }

    fn done(&self) -> bool {
        self.token == Token::EOF
    }

    fn expect(&mut self, expected: Token) -> Result<Span> {
        if self.token == expected {
            Ok(self.bump())
        } else {
            Err(NetrineError::error(
                self.token.span,
                format!("Expected `{}` but found `{}`", expected, self.token)))
        }
    }

    fn maybe(&mut self, expected: Token) -> bool {
        if self.token == expected {
            self.bump();
            true
        } else {
            false
        }
    }

    fn span(&self, start: Span) -> Span {
        Span::from(start, self.prev.span)
    }

    fn match_token(&self, kind: Token) -> bool {
        self.token == kind
    }

    fn match_peek(&self, kind: Token) -> bool {
        self.peek.kind == kind
    }

    fn match_lines(&self) -> bool {
        self.prev.span.line == self.token.span.line
    }

    fn unexpected(&mut self) -> NetrineError {
        let msg = match self.token {
            Token::Error(_) => {
                format!("{}", self.token)
            }
            _ => {
                format!("Unexpected `{}`", self.token)
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

    fn expr(code: &str) -> Expr {
        Parser::new(&source(&code)).parse_expr().unwrap()
    }

    fn module(code: &str) -> Module {
        Parser::new(&source(&code)).parse_module().unwrap()
    }

    fn top_level(code: &str) -> Expr {
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
            Expr::Name(
                Name {
                    value: "netrine".into(), span: Span::new(1, 0, 7)
                }
            )
        );

        assert_eq!(
            expr("maybe?"),
            Expr::Name(
                Name {
                    value: "maybe?".into(), span: Span::new(1, 0, 6)
                }
            )
        );

        assert_eq!(
            expr("dangerous!"),
            Expr::Name(
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
            Expr::Number(
                Literal {
                    value: "42".into(), span: Span::new(1, 0, 2)
                }
            )
        );

        assert_eq!(
            expr("3.14"),
            Expr::Number(
                Literal {
                    value: "3.14".into(), span: Span::new(1, 0, 4)
                }
            )
        );

        assert_eq!(
            expr("\"hello\""),
            Expr::String(
                Literal {
                    value: "\"hello\"".into(), span: Span::new(1, 0, 7)
                }
            )
        );

        assert_eq!(
            expr("True"),
            Expr::True(Span::new(1, 0, 4))
        );

        assert_eq!(
            expr("False"),
            Expr::False(Span::new(1, 0, 5))
        );

        assert_eq!(
            expr("_"),
            Expr::Any(Span::new(1, 0, 2))
        );

        assert_error_expr!("\"unclosed");

        assert_error_expr!("1.");
    }

    #[test]
    fn test_basic_definition() {
        assert_eq!(
            top_level("value = 10"),
            Expr::Def(
                box Def {
                    patt: Expr::Name(
                            Name {
                            value: "value".into(), span: Span::new(1, 0, 5)
                        }
                    ),
                    value: Expr::Number(
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
            top_level("fn name: 1"),
            Expr::Fn(
                box Fn {
                    parameters: vec![],
                    name: Name {
                        value: "name".into(), span: Span::new(1, 0, 4)
                    },
                    value: Expr::Number(
                        Literal {
                            value: "1".into(), span: Span::new(1, 7, 8)
                        }
                    ),
                    span: Span::new(1, 0, 8)
                }
            )
        );

        assert_error_top_level!("fn name");
    }

    #[test]
    fn test_basic_multiline_function() {
        assert_eq!(
            top_level("fn name:
                         multiline"),
            Expr::Fn(
                box Fn {
                    parameters: vec![],
                    name: Name {
                        value: "name".into(), span: Span::new(1, 0, 4)
                    },
                    value: Expr::Name(
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
            top_level("fn name:
                         a = 1
                         b = 2"),
            Expr::Fn(
                box Fn {
                    parameters: vec![],
                    name: Name {
                        value: "name".into(), span: Span::new(1, 0, 4)
                    },
                    value: Expr::Block(
                        box Block {
                            parameters: vec![],
                            expressions: vec![
                                Expr::Def(
                                    box Def {
                                        patt: Expr::Name(
                                            Name {
                                                value: "a".into(), span: Span::new(2, 32, 33)
                                            }
                                        ),
                                        value: Expr::Number(
                                            Literal {
                                                value: "1".into(), span: Span::new(2, 36, 37)
                                            }
                                        ),
                                        span: Span::new(2, 32, 37)
                                    }
                                ),
                                Expr::Def(
                                    box Def {
                                        patt: Expr::Name(
                                            Name {
                                                value: "b".into(), span: Span::new(3, 63, 64)
                                            }
                                        ),
                                        value: Expr::Number(
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
            top_level("fn: _"),
            Expr::Block(
                box Block {
                    parameters: vec![],
                    expressions: vec![
                        Expr::Any(Span::new(1, 5, 6))
                    ],
                    span: Span::new(1, 5, 7)
                },
            )
        );

        assert_error_top_level!("fn name");
    }

    #[test]
    fn test_basic_function_with_parameters() {
        assert_eq!(
            top_level("fn name arg: arg"),
            Expr::Fn(
                box Fn {
                    name: Name {
                        value: "function".into(), span: Span::new(1, 0, 2)
                    },
                    parameters: vec![
                        Param {
                            patt: Expr::Name(
                                Name {
                                    value: "x".into(), span: Span::new(1, 3, 4)
                                }
                            ),
                            value: None,
                            span: Span::new(1, 3, 4)
                        }
                    ],
                    value: Expr::Name(
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
            top_level("fn name arg=1: arg"),
            Expr::Fn(
                box Fn {
                    name: Name {
                        value: "fn".into(), span: Span::new(1, 0, 2)
                    },
                    parameters: vec![
                        Param {
                            patt: Expr::Name(
                                Name {
                                    value: "x".into(), span: Span::new(1, 3, 4)
                                }
                            ),
                            value: Some(Expr::Number(
                                Literal {
                                    value: "2".into(), span: Span::new(1, 7, 8)
                                }
                            )),
                            span: Span::new(1, 3, 8)
                        }
                    ],
                    value: Expr::Name(
                        Name {
                            value: "x".into(), span: Span::new(1, 12, 13)
                        }
                    ),
                    span: Span::new(1, 0, 13)
                }
            )
        );

        assert_error_expr!("fn name arg=");
    }

    #[test]
    fn test_get_property_access() {
        assert_eq!(
            expr("a.b"),
            Expr::Get(
                box Get {
                    source: Expr::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expr::Name(
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
            Expr::Get(
                box Get {
                    source: Expr::Get(
                        box Get {
                            source: Expr::Name(
                                Name {
                                    value: "a".into(), span: Span::new(1, 0, 1),
                                }
                            ),
                            value: Expr::Name(
                                Name {
                                    value: "b".into(), span: Span::new(1, 2, 3),
                                }
                            ),
                            span: Span::new(1, 0, 3)
                        }
                    ),
                    value: Expr::Name(
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
            Expr::Get(
                box Get {
                    source: Expr::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expr::Number(
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
            Expr::Get(
                box Get {
                    source: Expr::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expr::String(
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
            Expr::Get(
                box Get {
                    source: Expr::Name(
                        Name {
                            value: "a".into(), span: Span::new(1, 0, 1),
                        }
                    ),
                    value: Expr::List(
                        box List {
                            values: vec![
                                Expr::Name(
                                    Name { value: "x".into(), span: Span::new(1, 3, 4) }
                                ),
                                Expr::Name(
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
            Expr::List(
                box List {
                    values: vec![], span: Span::new(1, 0, 2)
                }
            )
        );

        assert_eq!(
            expr("[1,]"),
            Expr::List(
                box List {
                    values: vec![
                        Expr::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                    ],
                    span: Span::new(1, 0, 4)
                }
            )
        );

        assert_eq!(
            expr("[x, y, z]"),
            Expr::List(
                box List {
                    values: vec![
                        Expr::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expr::Name(
                            Name { value: "y".into(), span: Span::new(1, 4, 5) }
                        ),
                        Expr::Name(
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
            Expr::List(
                box List {
                    values: vec![
                        Expr::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expr::Name(
                            Name { value: "y".into(), span: Span::new(2, 23, 24) }
                        ),
                        Expr::Name(
                            Name { value: "z".into(), span: Span::new(3, 44, 45) }
                        )
                    ],
                    span: Span::new(1, 0, 46)
                }
            )
        );

        assert_eq!(
            expr("[1, \"a\", [3.14]]"),
            Expr::List(
                box List {
                    values: vec![
                        Expr::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expr::String(
                            Literal { value: "\"a\"".into(), span: Span::new(1, 4, 7) }
                        ),
                        Expr::List(
                            box List {
                                values: vec![
                                    Expr::Number(
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
            Expr::Tuple(
                box Tuple {
                    values: vec![], span: Span::new(1, 0, 2)
                }
            )
        );

        assert_eq!(
            expr("(1)"),
            Expr::Number(
                Literal {
                    value: "1".into(), span: Span::new(1, 1, 2)
                }
            )
        );

        assert_eq!(
            expr("(1,)"),
            Expr::Tuple(
                box Tuple {
                    values: vec![
                        Expr::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                    ],
                    span: Span::new(1, 0, 4)
                }
            )
        );

        assert_eq!(
            expr("(x, y, z)"),
            Expr::Tuple(
                box Tuple {
                    values: vec![
                        Expr::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expr::Name(
                            Name { value: "y".into(), span: Span::new(1, 4, 5) }
                        ),
                        Expr::Name(
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
            Expr::Tuple(
                box Tuple {
                    values: vec![
                        Expr::Name(
                            Name { value: "x".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expr::Name(
                            Name { value: "y".into(), span: Span::new(2, 23, 24) }
                        ),
                        Expr::Name(
                            Name { value: "z".into(), span: Span::new(3, 44, 45) }
                        )
                    ],
                    span: Span::new(1, 0, 46)
                }
            )
        );

        assert_eq!(
            expr("(1, \"a\", ())"),
            Expr::Tuple(
                box Tuple {
                    values: vec![
                        Expr::Number(
                            Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                        ),
                        Expr::String(
                            Literal { value: "\"a\"".into(), span: Span::new(1, 4, 7) }
                        ),
                        Expr::Tuple(
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
            Expr::List(
                box List {
                    values: vec![], span: Span::new(1, 0, 3)
                }
            )
        );

        assert_eq!(
            expr("[x = 1, y = 2, z = 3]"),
            Expr::Dict(
                box Dict {
                    values: vec![
                        (
                            Expr::Name(
                                Name { value: "x".into(), span: Span::new(1, 1, 2) }
                            ),
                            Expr::Number(
                                Literal { value: "1".into(), span: Span::new(1, 5, 6) }
                            ),
                        ),
                        (
                            Expr::Name(
                                Name { value: "y".into(), span: Span::new(1, 8, 9) }
                            ),
                            Expr::Number(
                                Literal { value: "2".into(), span: Span::new(1, 12, 13) }
                            ),
                        ),
                        (
                            Expr::Name(
                                Name { value: "z".into(), span: Span::new(1, 15, 16) }
                            ),
                            Expr::Number(
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
            Expr::Dict(
                box Dict {
                    values: vec![
                        (
                            Expr::Name(
                                Name { value: "x".into(), span: Span::new(1, 1, 2) }
                            ),
                            Expr::Number(
                                Literal { value: "1".into(), span: Span::new(1, 4, 5) }
                            ),
                        ),
                        (
                            Expr::Name(
                                Name { value: "y".into(), span: Span::new(2, 26, 27) }
                            ),
                            Expr::Number(
                                Literal { value: "2".into(), span: Span::new(2, 29, 30) }
                            ),
                        ),
                        (
                            Expr::Name(
                                Name { value: "z".into(), span: Span::new(3, 50, 51) }
                            ),
                            Expr::Number(
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
            Expr::Dict(
                box Dict {
                    values: vec![
                        (
                            Expr::Number(
                                Literal { value: "1".into(), span: Span::new(1, 1, 2) }
                            ),
                            Expr::String(
                                Literal { value: "\"a\"".into(), span: Span::new(1, 5, 8) }
                            ),
                        ),
                        (
                            Expr::Name(
                                Name { value: "list".into(), span: Span::new(1, 10, 14) }
                            ),
                            Expr::List(
                                box List {
                                    values: vec![
                                        Expr::Dict(
                                            box Dict {
                                                values: vec![
                                                    (
                                                        Expr::Name(
                                                            Name { value: "nested".into(), span: Span::new(1, 19, 25) }
                                                        ),
                                                        Expr::String(
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
