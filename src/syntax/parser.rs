use crate::error::{Error, Result};
use crate::source::Source;
use crate::span::Span;

use super::nodes::*;
use super::token::{Token, TokenKind};
use super::lexer::Lexer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Context {
    Default,
    Sequence,
}

#[derive(Debug, Clone)]
pub struct Parser<'s> {
    lexer: Lexer<'s>,
    prev: Token,
    peek: Token,
    token: Token,
    source: &'s Source,
    ctx: Vec<Context>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s Source) -> Parser {
        let mut parser = Parser {
            source,
            lexer: Lexer::new(source.content.as_bytes()),
            prev: Token::default(),
            peek: Token::default(),
            token: Token::default(),
            ctx: vec![
                Context::Default
            ],
        };
        parser.bump(); // token
        parser.bump(); // peek
        parser
    }

    pub fn expr(&mut self) -> Result<Node> {
        self.binary(0)
    }

    fn start_term(&self) -> bool {
        match self.token.kind {
            TokenKind::LParen
          | TokenKind::Ident
          | TokenKind::Number
          | TokenKind::String => true, _ => false
        }
    }

    fn term(&mut self) -> Result<Node> {
        match self.token.kind {
            TokenKind::LParen => self.parens(),
            TokenKind::Ident  => self.ident(),
            TokenKind::Number => self.number(),
            TokenKind::String => self.string(),
            _ => Err(Error::new("expected an expression term", self.token.span)),
        }
    }
    
    fn name(&mut self) -> Result<Identifier> {
        let span = self.expect(TokenKind::Ident)?;
        let value = self.source.content[span.range()].to_string();
        Ok(Identifier { value, span })
    }

    fn ident(&mut self) -> Result<Node> {
        Ok(Node::Identifier(self.name()?))
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

    fn parens(&mut self) -> Result<Node> {
        self.ctx.push(Context::Default);
        
        self.expect(TokenKind::LParen)?;
        let node = self.expr()?;
        self.expect(TokenKind::RParen)?;

        self.ctx.pop();
        
        Ok(node)
    }

    fn initial(&mut self) -> Result<Node> {
        let mut node = self.term()?;
        loop {
            match self.token.kind {
                TokenKind::LParen => {
                    node = self.apply(node)?;
                }
                TokenKind::Dot => {
                    node = self.access(node)?;
                }
                _ => {
                    return Ok(node);
                }
            }
        }
    }

    fn apply(&mut self, node: Node) -> Result<Node> {        
        let arguments = self.sequence_of(
            TokenKind::Comma,
            TokenKind::LParen,
            TokenKind::RParen,
            Self::expr)?;

        let span = self.span();

        Ok(Node::Apply(box Apply { callee: node, arguments, span }))
    }

    fn access(&mut self, node: Node) -> Result<Node> {
        self.expect(TokenKind::Dot)?;
        let term = self.term()?;
        let span = self.span();
        
        Ok(Node::Get(box Get { from: node, value: term, span }))
    }

    fn unary(&mut self) -> Result<Node> {
        if let Some(operator) = self.operator() {
            self.bump();

            if !operator.is_unary() {
                return Err(Error::new("not an unary operator", operator.span));
            }
            
            let expr = self.expr()?;
            let span = self.span();
            Ok(Node::Unary(box Unary { operator, expr, span }))
        } else {
            self.initial()
        }
    }

    fn binary(&mut self, precedence: u8) -> Result<Node> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.operator() && operator.precedence() >= precedence {
            self.bump();

            let next_precedence = match operator.associativity() {
                 Associativity::None
               | Associativity::Left => operator.precedence() + 1,
                 Associativity::Right => operator.precedence(),
            };

            let rexpr = self.binary(next_precedence)?;
            let lexpr = expr;
            
            expr = match operator.kind {
                OperatorKind::Comma => self.tuple(lexpr, rexpr)?,
                OperatorKind::Semi  => self.chain(lexpr, rexpr)?,
                OperatorKind::Equals => self.define(lexpr, rexpr, false)?,
                OperatorKind::Walrus => self.define(lexpr, rexpr, true)?,
                _ => {
                    let span = Span::from(&lexpr, &rexpr);
                    Node::Binary(box Binary { operator, lexpr, rexpr, span })
                }
            };
        }

        Ok(expr)
    }

    fn is_unary_operator(&self) -> bool {
        match self.token.kind {
            TokenKind::Plus
          | TokenKind::Minus => {
                match self.peek.kind {
                    TokenKind::Ident
                  | TokenKind::LParen
                  | TokenKind::LBrace
                  | TokenKind::Number => {
                    // if there are no spaces between the operator and the next token
                    self.peek.span.start() - self.token.span.end() == 0
                  }
                  _ => false
                }
            }
            _ => false
        }
    }

    fn is_default_context(&self) -> bool {
        self.ctx.last().map(|ctx| ctx == &Context::Default).unwrap_or(false)
    }

    fn operator(&mut self) -> Option<Operator> {
        let span = self.token.span;

        let kind = match self.token.kind {
            TokenKind::Plus  if self.is_unary_operator() => OperatorKind::Pos,
            TokenKind::Minus if self.is_unary_operator() => OperatorKind::Neg,
            TokenKind::Comma if self.is_default_context() => OperatorKind::Comma,
            TokenKind::Semi  if self.is_default_context() => OperatorKind::Semi,
            TokenKind::Equals => OperatorKind::Equals,
            TokenKind::Walrus => OperatorKind::Walrus,
            TokenKind::Plus   => OperatorKind::Add,
            TokenKind::Minus  => OperatorKind::Sub,
            TokenKind::Star   => OperatorKind::Mul,
            TokenKind::Slash  => OperatorKind::Div,
            TokenKind::Mod    => OperatorKind::Mod,
            TokenKind::Caret  => OperatorKind::Exp,
            TokenKind::And    => OperatorKind::And,
            TokenKind::Or     => OperatorKind::Or,
            TokenKind::Not    => OperatorKind::Not,
            TokenKind::Is     => OperatorKind::Is,
            TokenKind::EqEq   => OperatorKind::Eq,
            TokenKind::NoEq   => OperatorKind::Ne,
            TokenKind::Lt     => OperatorKind::Lt,
            TokenKind::LtEq   => OperatorKind::Le,
            TokenKind::Gt     => OperatorKind::Gt,
            TokenKind::GtEq   => OperatorKind::Ge,
            TokenKind::Pipe   => OperatorKind::Pipe,
            _ => {
                return None;
            }
        };

        Some(Operator { kind, span })
    }

    fn chain(&self, lexpr: Node, rexpr: Node) -> Result<Node> {
        let span = Span::from(&lexpr, &rexpr);

        let nodes = if let Node::Block(box Block { mut nodes, .. }) = lexpr {
            nodes.push(rexpr);
            nodes
        } else {
            vec![lexpr, rexpr]
        };

        Ok(Node::Block(box Block { nodes, span }))
    }

    fn tuple(&self, lexpr: Node, rexpr: Node) -> Result<Node> {
        let span = Span::from(&lexpr, &rexpr);

        let elements = match lexpr {
            Node::Tuple(box Tuple { mut elements, .. }) => {
                elements.push(rexpr);
                elements
            }
            _ => vec![lexpr, rexpr],
        };

        Ok(Node::Tuple(box Tuple { elements, span }))
    }

    fn define(&self, lexpr: Node, rexpr: Node, mutable: bool) -> Result<Node> {
        let span = Span::from(&lexpr, &rexpr);
        Ok(Node::Def(box Def { target: lexpr, value: rexpr, mutable, span }))
    }

    fn sequence_of<T, F>(
        &mut self,
        separator: TokenKind,
        open: TokenKind,
        close: TokenKind,
        mut f: F,
    ) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        self.ctx.push(Context::Sequence);

        let mut result = vec![];

        self.expect(open)?;

        while !self.done() && !self.token.is(close) {
            result.push(f(self)?);

            if !self.maybe(separator) {
                break;
            }
        }

        self.expect(close)?;

        self.ctx.pop();

        Ok(result)
    }

    fn bump(&mut self) -> Span {
        self.prev  = self.token;
        self.token = self.peek;
        self.peek  = self.lexer.next().unwrap_or_default();
        self.prev.span
    }
    
    fn span(&self) -> Span {
        // TODO span from start to prev
        self.prev.span
    }

    fn done(&self) -> bool {
        self.token.kind == TokenKind::EOF
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Span> {
        if self.token.kind == expected {
            Ok(self.bump())
        } else {
            Err(Error::new(format!("Expected `{}`", expected), self.token.span))
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
        vec![]
    }

    fn expr(code: &str) -> Node {
        Parser::new(&source(&code))
            .expr()
            .unwrap()
    }

    macro_rules! assert_error_expr {
        ($code:expr) => {
            if let Err(_) = Parser::new(&source(&$code)).expr() {
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
            Node::Identifier(Identifier {
                value: "netrine".to_string(),
                span: Span(0, 7)
            })
        );

        assert_eq!(
            expr("prime'"),
            Node::Identifier(Identifier {
                value: "prime'".to_string(),
                span: Span(0, 6)
            })
        );

        assert_eq!(
            expr("primes''''"),
            Node::Identifier(Identifier {
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
            Node::Identifier(Identifier {
                value: "_".to_string(),
                span: Span(0, 1)
            })
        );

        assert_eq!(
            expr("True"),
            Node::Identifier(
            Identifier {
                value: "True".into(), span: Span(0, 4)
            }
        ));

        assert_eq!(
            expr("False"),
            Node::Identifier(
            Identifier {
                value: "False".into(), span: Span(0, 5)
            }
        ));

        assert_error_expr!("\"unclosed");

        // assert_error_expr!("1.");
    }

    #[test]
    fn test_basic_definition() {
        assert_eq!(
            expr("value = 10"),
            Node::Def(box Def {
                target: Node::Identifier(
                    Identifier {
                        value: "value".into(), span: Span(0, 5)
                    }
                ),
                value: Node::Number(
                    Literal {
                        value: "10".into(), span: Span(8, 10)
                    }
                ),
                mutable: false,
                span: Span(0, 10),
            })
        );

        assert_error_expr!("value = ");
    }

    #[test]
    fn test_tuple_definition() {
        assert_eq!(
            expr("a, b = 1, 2"),
            Node::Def(box Def {
                target: Node::Tuple(
                    box Tuple {
                        elements: vec![
                            Node::Identifier(
                                Identifier {
                                    value: "a".into(), span: Span(0, 1)
                                 }
                            ),
                            Node::Identifier(
                                Identifier {
                                    value: "b".into(), span: Span(3, 4)
                                 }
                            )
                        ],
                        span: Span(0, 4)
                    }
                ),
                value: Node::Tuple(
                    box Tuple {
                        elements: vec![
                            Node::Number(
                                Literal {
                                    value: "1".into(), span: Span(7, 8)
                                 }
                            ),
                            Node::Number(
                                Literal {
                                    value: "2".into(), span: Span(10, 11)
                                 }
                            )
                        ],
                        span: Span(0, 4)
                    }
                ),
                mutable: false,
                span: Span(0, 11),
            })
        );
    }

    #[test]
    fn test_basic_apply() {

        assert_eq!(
            expr(r#"print("Hello", "World")"#),
            Node::Apply(box Apply {
                callee: Node::Identifier(
                    Identifier {
                        value: "print".into(), span: Span(0, 5)
                    }
                ),
                arguments: vec![
                    Node::String(
                        Literal {
                            value: "\"Hello\"".into(), span: Span(6, 13)
                         }
                    ),
                    Node::String(
                        Literal {
                            value: "\"World\"".into(), span: Span(15, 22)
                         }
                    )
                ],
                span: Span(0, 23),
            })
        );
    }
}
