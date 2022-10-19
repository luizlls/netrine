use super::lexer::Lexer;
use super::nodes::*;
use super::token::{Token, TokenKind, IdKind, OpKind};

use crate::error::{NetrineError, Result, err};
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

    fn init(mut self) -> Result<Parser<'s>> {
        self.bump()?; // peek token
        self.bump()?; // token token

        while self.token.is(TokenKind::NewLine) {
            self.bump()?;
        }

        Ok(self)
    }

    fn top_level(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::Id(_)
          | TokenKind::Op(_) if self.peek.is(TokenKind::Equals) => {
                self.define()
            }
            _ => err!(self.token.span, "expected definition")
        }
    }

    fn expr(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::Id(_) if self.peek.is(TokenKind::Equals) => {
                self.define()
            }
            _ => self.apply()
        }
    }

    fn start_term(&self) -> bool {
        match self.token.kind {
            TokenKind::Id(_)
          | TokenKind::Op(_)
          | TokenKind::LParen
          | TokenKind::Number
          | TokenKind::String => true, _ => false
        }
    }

    fn term(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::Id(IdKind::Lower) => self.lower(),
            TokenKind::Id(IdKind::Upper) => self.upper(),
            TokenKind::Op(_)  => self.operation(),
            TokenKind::LParen => self.parens(),
            TokenKind::Number => self.number(),
            TokenKind::String => self.string(),
            _ => err!(self.token.span, "expected a term"),
        }
    }

    fn ident(&mut self) -> Result<Identifier> {
        let span = self.token.span;
        if matches!(self.token.kind, TokenKind::Id(_)) {
            let value = self.source.content[span.range()].to_string();
            self.bump()?;
            Ok(Identifier { value, span })
        } else {
            err!(span, "expected an identifier")
        }
    }

    fn is_unary(&self) -> bool {
        match self.token.kind {
            TokenKind::Op(OpKind::Plus)
          | TokenKind::Op(OpKind::Minus) => {
                match self.peek.kind {
                    TokenKind::Id(_)
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

        if let TokenKind::Op(kind) = self.token.kind {
            let kind = match kind {
                OpKind::Plus  if self.is_unary() => OperatorKind::Pos,
                OpKind::Minus if self.is_unary() => OperatorKind::Neg,
                OpKind::Plus   => OperatorKind::Add,
                OpKind::Minus  => OperatorKind::Sub,
                OpKind::Star   => OperatorKind::Mul,
                OpKind::Slash  => OperatorKind::Div,
                OpKind::Mod    => OperatorKind::Mod,
                OpKind::And    => OperatorKind::And,
                OpKind::Or     => OperatorKind::Or,
                OpKind::Not    => OperatorKind::Not,
                OpKind::Is     => OperatorKind::Is,
                OpKind::EqEq   => OperatorKind::Eq,
                OpKind::BangEq => OperatorKind::Ne,
                OpKind::Lt     => OperatorKind::Lt,
                OpKind::LeEq   => OperatorKind::Le,
                OpKind::Gt     => OperatorKind::Gt,
                OpKind::GtEq   => OperatorKind::Ge,
                OpKind::Pipe   => OperatorKind::Pipe,
            };

            self.bump()?;

            Ok(Operator { kind, span })
        } else {
            err!(span, "`{}` is not a valid operator", self.token.kind)
        }
    }

    fn name(&mut self) -> Result<Name> {
        let span = self.token.span;

        match self.token.kind {
            TokenKind::Id(_) => {
                let identifier = self.ident()?;
                Ok(Name::Id(identifier))
            }
            TokenKind::Op(_) => {
                let operator = self.operator()?;
                Ok(Name::Op(operator))
            }
            _ => err!(span, "expected an identifier or operator")
        }
    }

    fn lower(&mut self) -> Result<Node> {
        Ok(Node::Id(self.ident()?))
    }
    
    fn operation(&mut self) -> Result<Node> {
        Ok(Node::Op(self.operator()?))
    }

    fn upper(&mut self) -> Result<Node> {
        let span = self.expect(TokenKind::Id(IdKind::Upper))?;
        let value = self.source.content[span.range()].to_string();

        match &value[..] {
            "True"  => Ok(Node::True(span)),
            "False" => Ok(Node::False(span)),
            _ => {
                err!(span, "anonymous variants are not supported yet")
            }
        }
    }

    fn parens(&mut self) -> Result<Node> {
        self.between(TokenKind::LParen, TokenKind::RParen, Self::expr)
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
        let name = self.name()?;
        self.expect(TokenKind::Equals)?;
        let value = self.expr()?;

        let span = Span::of(&name, &value);

        Ok(Node::Define(box Define { name, value, span }))
    }

    fn apply(&mut self) -> Result<Node> {
        let (values, span) = self.parse_while(Self::start_term, Self::term)?;
        Ok(Node::Apply(box Apply { values, span }))
    }

    fn parse_while<T, P, F>(
        &mut self,
        p: P,
        mut f: F,
    ) -> Result<(Vec<T>, Span)>
    where
      P: Fn(&Self) -> bool, F: FnMut(&mut Self) -> Result<T>
    {
        let start = self.token.span;

        let mut result = vec![];

        while p(self) {
            result.push(f(self)?);
        }

        let span = Span::of(&start, &self.prev.span);

        Ok((result, span))
    }

    fn between<T, F>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        mut f: F,
    ) -> Result<T>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        self.expect(open)?;
        let result = f(self)?;
        self.expect(close)?;

        Ok(result)
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
            err!(self.token.span, "Expected `{}` but found `{}`", expected, self.token.kind)
        }
    }
}

pub fn parse(source: &Source) -> Result<Vec<Node>> {
    let mut parser = Parser::new(source).init()?;
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
            .init()
            .unwrap()
            .expr()
            .unwrap()
    }

    macro_rules! assert_error_expr {
        ($code:expr) => {
            if let Err(_) =
                Parser::new(&source(&$code))
                    .init()
                    .and_then(|mut p| p.expr()) {
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
                name: Name::Id(Identifier {
                    value: "value".into(), span: Span(0, 5)
                }),
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
}
