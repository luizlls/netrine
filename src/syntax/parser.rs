use super::ast::*;
use super::token::{Token, TokenKind};
use super::lexer::Lexer;

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
        Ok(self)
    }

    fn parse_top_level(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Lower if self.peek.is(TokenKind::Equals) => {
                self.parse_def()
            }
            _ => error!(self.token.span, "expected definition")
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Lower if self.peek.is(TokenKind::Equals) => {
                self.parse_def()
            }
            _ => self.parse_binary(0)
        }
    }

    fn parse_term(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::LParen => self.parse_parens(),
            TokenKind::Lower  => self.parse_lower(),
            TokenKind::Upper  => self.parse_upper(),
            TokenKind::Number => self.parse_number(),
            TokenKind::String => self.parse_string(),
            _ => error!(self.token.span, "expected a term"),
        }
    }

    fn parse_name(&mut self) -> Result<Name> {
        let span = self.expect(TokenKind::Lower)?;
        let value = self.source.content[span.range()].to_string();
        Ok(Name { value, span })
    }

    fn parse_lower(&mut self) -> Result<Expr> {
        Ok(Expr::Name(self.parse_name()?))
    }

    fn parse_upper(&mut self) -> Result<Expr> {
        let span = self.expect(TokenKind::Upper)?;
        let value = self.source.content[span.range()].to_string();

        match &value[..] {
            "True"  => Ok(Expr::True(span)),
            "False" => Ok(Expr::False(span)),
            _ => {
                error!(span, "anonymous variants are not supported yet")
            }
        }
    }

    fn parse_parens(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let mut elements = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_expr
        )?;

        let span = self.span(start);

        if elements.len() == 1 {
            Ok(elements.pop().unwrap())
        } else {
            error!(span, "tuples are not supported yet")
        }
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let span = self.expect(TokenKind::Number)?;
        let value = self.source.content[span.range()].to_string();
        Ok(Expr::Number(Literal { value, span }))
    }

    fn parse_string(&mut self) -> Result<Expr> {
        let span = self.expect(TokenKind::String)?;
        let value = self.source.content[span.range()].to_string();
        Ok(Expr::String(Literal { value, span }))
    }

    fn parse_def(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let name = self.parse_name()?;
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expr()?;

        let span = self.span(start);

        Ok(Expr::Def(box Def { name, value, span }))
    }
    
    fn parse_unary(&mut self) -> Result<Expr> {
        if self.token.is(TokenKind::Not) {
            let operator = self.parse_operator()?;
            let right = self.parse_term()?;
            let span = Span::from(operator.span, right.span());

            Ok(Expr::Unary(box Unary { operator, right, span }))
        } else {
            self.parse_term()
        }
    }

    fn parse_binary(&mut self, minimum_precedence: u8) -> Result<Expr> {
        let mut expr = self.parse_unary()?;
        
        while self.token.is_operator() {
            if let Some(precedence) = self.token.precedence() {
                if precedence < minimum_precedence {
                    break;
                }

                let operator = self.parse_operator()?;

                let right = self.parse_binary(precedence + 1)?;
                let left = expr;

                let span = Span::from(
                    left.span(), right.span(),
                );

                expr = Expr::Binary(box Binary { operator, left, right, span });
            }
        }

        Ok(expr)
    }

    fn parse_operator(&mut self) -> Result<Operator> {
        let span = self.token.span;

        let kind = match self.token.kind {
            TokenKind::Add   => OperatorKind::Add,
            TokenKind::Sub   => OperatorKind::Sub,
            TokenKind::Mul   => OperatorKind::Mul,
            TokenKind::Div   => OperatorKind::Div,
            TokenKind::Rem   => OperatorKind::Rem,
            TokenKind::And   => OperatorKind::And,
            TokenKind::Or    => OperatorKind::Or,
            TokenKind::Not   => OperatorKind::Not,
            TokenKind::Is    => OperatorKind::Is,
            TokenKind::Eq    => OperatorKind::Eq,
            TokenKind::Ne    => OperatorKind::Ne,
            TokenKind::Lt    => OperatorKind::Lt,
            TokenKind::Le    => OperatorKind::Le,
            TokenKind::Gt    => OperatorKind::Gt,
            TokenKind::Ge    => OperatorKind::Ge,
            TokenKind::Pipe  => OperatorKind::Pipe,
            TokenKind::Range => OperatorKind::Range,
            _ => {
                return error!(span, "`{}` is not a valid operator", self.token.kind);
            }
        };

        self.bump()?;

        Ok(Operator { kind, span })
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
            error!(self.token.span, "Expected `{}` but found `{}`", expected, self.token.kind)
        }
    }
}

pub fn parse(source: &Source) -> Result<Vec<Expr>> {
    let mut parser = Parser::new(source).initialize()?;
    let mut module = vec![];

    while !parser.done() {
        module.push(parser.parse_top_level()?);
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

    fn module(code: &str) -> Vec<Expr> {
        parse(&source(&code)).unwrap()
    }

    fn expr(code: &str) -> Expr {
        Parser::new(&source(&code))
            .initialize()
            .unwrap()
            .parse_expr()
            .unwrap()
    }

    fn span(start: u32, end: u32) -> Span {
        Span { line: 1, start, end }
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
            Expr::Name(Name {
                value: "netrine".to_string(),
                span: span(0, 7)
            })
        );

        assert_eq!(
            expr("maybe?"),
            Expr::Name(Name {
                value: "maybe?".to_string(),
                span: span(0, 6)
            })
        );

        assert_eq!(
            expr("dangerous!"),
            Expr::Name(Name {
                value: "dangerous!".to_string(),
                span: span(0, 10)
            })
        );

        assert_eq!(
            expr("prime'"),
            Expr::Name(Name {
                value: "prime'".to_string(),
                span: span(0, 6)
            })
        );

        assert_eq!(
            expr("primes''''"),
            Expr::Name(Name {
                value: "primes''''".to_string(),
                span: span(0, 10)
            })
        );
    }

    #[test]
    fn test_literals() {
        assert_eq!(
            expr("42"),
            Expr::Number(Literal {
                value: "42".to_string(),
                span: span(0, 2)
            })
        );

        assert_eq!(
            expr("3.14"),
            Expr::Number(Literal {
                value: "3.14".to_string(),
                span: span(0, 4)
            })
        );

        assert_eq!(
            expr("\"hello\""),
            Expr::String(Literal {
                value: "\"hello\"".to_string(),
                span: span(0, 7)
            })
        );

        assert_eq!(
            expr("_"),
            Expr::Name(Name {
                value: "_".to_string(),
                span: span(0, 1)
            })
        );

        assert_eq!(expr("True"), Expr::True(span(0, 4)));

        assert_eq!(expr("False"), Expr::False(span(0, 5)));

        assert_error_expr!("\"unclosed");

        assert_error_expr!("1.");
    }
}
