use std::vec;

use crate::span::Span;

use super::lexer::Lexer;
use super::node::{Associativity, Precedence, Node, SyntaxKind, SyntaxToken};


#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Marker(usize);

#[derive(Debug, Clone)]
pub struct Parser<'p> {
    lexer: Lexer<'p>,
    nodes: Vec<Node>,
    token: SyntaxToken,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str) -> Parser {
        let mut parser = Parser {
            lexer: Lexer::new(source),
            nodes: vec![],
            token: SyntaxToken::default(),
        };
        parser.bump(); // first token
        parser
    }

    pub fn parse(mut self) -> Vec<Node> {
        while !self.eof() {
            self.skip_trivia();
            self.node();
            self.skip_trivia();
        }
        self.nodes
    }

    fn node(&mut self) {
        match self.token {
            SyntaxToken::If => self.r#if(),
            SyntaxToken::Yield => self.r#yield(),
            SyntaxToken::Break => self.r#break(),
            SyntaxToken::Import => self.import(),
            _ => self.define(),
        }
    }

    fn expr(&mut self) {
        if self.at(SyntaxToken::If) {
            self.r#if();
        } else {
            self.pipe();
        }
    }

    fn define(&mut self) {
        let start = self.mark();

        self.lvalue();

        match self.token {
            SyntaxToken::Equals => {
                self.expect(SyntaxToken::Equals);
                self.expr();
                let where_ = self.mark();
                if self.maybe(SyntaxToken::Where) {
                    self.expr();
                    self.collect(SyntaxKind::Where, where_);
                }
                self.collect(SyntaxKind::Let, start);
            }
            SyntaxToken::Walrus => {
                self.expect(SyntaxToken::Walrus);
                self.expr();
                self.collect(SyntaxKind::Set, start);
            }
            _ => {}
        }
    }

    fn ident(&mut self) {
        self.expect(SyntaxToken::Lower);
    }

    fn upper(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::Upper);

        match self.token {
            SyntaxToken::LParen => self.parens(),
            SyntaxToken::LBrace => self.record(),
            _ => {}
        }

        self.collect(SyntaxKind::Variant, start);
    }

    fn number(&mut self) {
        self.expect(SyntaxToken::Number);
    }

    fn string(&mut self) {
        self.expect(SyntaxToken::String);
    }

    fn field(&mut self) {
        let start = self.mark();

        while self.at(SyntaxToken::Dot) {
            self.make(); // dot
            self.bump(); // next
            match self.token {
                SyntaxToken::Lower => self.ident(),
                SyntaxToken::Upper => self.upper(),
                SyntaxToken::Number => self.number(),
                SyntaxToken::String => self.string(),
                SyntaxToken::LParen => self.parens(),
                SyntaxToken::LBracket => self.brackets(),
                _ => {
                    return self.unexpected(&[SyntaxKind::Field]);
                }
            }
            self.collect(SyntaxKind::Field, start);
        }
    }

    fn element(&mut self) {
        let start = self.mark();

        if self.maybe(SyntaxToken::Range) {
            self.apply();
            self.collect(SyntaxKind::Spread, start);
        } else {
            self.expr();
        }
    }

    fn parens(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::LParen);
        let total = self.sequence(
            |parser| !parser.at(SyntaxToken::RParen),
            Parser::element,
        );
        self.expect(SyntaxToken::RParen);

        self.collect(
            match total {
                0 => SyntaxKind::Empty,
                1 => SyntaxKind::Group,
                _ => SyntaxKind::Tuple,
            },
            start,
        );
    }

    fn brackets(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::LBracket);

        self.sequence(
            |parser| !parser.at(SyntaxToken::RBracket),
            Parser::element,
        );

        self.expect(SyntaxToken::RBracket);

        self.collect(SyntaxKind::List, start);
    }

    fn braces(&mut self) {
        if matches!(self.peek(), SyntaxToken::Dot | SyntaxToken::Range) {
            self.record();
        } else {
            self.lambda();
        }
    }

    fn record(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::LBrace);
        self.sequence(
            |parser| !parser.at(SyntaxToken::RBrace),
            Parser::property,
        );
        self.expect(SyntaxToken::RBrace);

        self.collect(SyntaxKind::Record, start);
    }

    fn property(&mut self) {
        let start = self.mark();

        match self.token {
            SyntaxToken::Range => {
                self.take();
                if !self.at(SyntaxToken::RBrace) {
                    self.apply();
                }
            }
            SyntaxToken::Dot => {
                self.field();

                match self.token {
                    SyntaxToken::Equals => {
                        self.take();
                        self.expr();
                    }
                    SyntaxToken::LBrace => {
                        self.braces();
                    }
                    SyntaxToken::Comma
                  | SyntaxToken::RBrace => {}
                    _ => self.unexpected(&[
                        SyntaxToken::Equals,
                        SyntaxToken::Comma,
                        SyntaxToken::RBrace,
                    ])
                }
            }
            _ => self.unexpected(&[
                SyntaxToken::Dot,
                SyntaxToken::Range,
            ])
        }

        self.collect(SyntaxKind::Property, start);
    }

    fn atom(&mut self) {
        match self.token {
            SyntaxToken::Lower => self.ident(),
            SyntaxToken::Upper => self.upper(),
            SyntaxToken::Number => self.number(),
            SyntaxToken::String => self.string(),
            SyntaxToken::LParen => self.parens(),
            SyntaxToken::LBrace => self.braces(),
            SyntaxToken::LBracket => self.brackets(),
            SyntaxToken::Dot => self.field(),
            SyntaxToken::Error => self.error(),
            _ => self.unexpected(&[SyntaxKind::Expr])
        }
    }

    fn get(&mut self, start: Marker) {
        self.expect(SyntaxToken::Dot);
        self.atom();
        self.collect(SyntaxKind::Get, start);
    }

    fn call(&mut self, start: Marker) {
        self.expect(SyntaxToken::LParen);
        self.function_arguments();
        self.expect(SyntaxToken::RParen);
        self.collect(SyntaxKind::Call, start);
    }

    fn function_arguments(&mut self) {
        let start = self.mark();
        self.sequence(
            |parser| !parser.at(SyntaxToken::RParen),
            |parser| {
                let named = parser.mark();
                parser.element();
                if parser.at(SyntaxToken::Equals) && parser.was(SyntaxToken::Lower) {
                    parser.expect(SyntaxToken::Equals);
                    parser.expr();
                    parser.collect(SyntaxKind::Let, named);
                }
            },
        );
        self.collect(SyntaxKind::Arguments, start);
    }

    fn lambda(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::LBrace);

        if self.maybe(SyntaxToken::RBrace) {
            return self.collect(SyntaxKind::Lambda, start);
        }

        if self.at(SyntaxToken::Case) {
            self.lambda_cases();
        } else {
            self.lambda_params();
        }

        self.until(
            |parser| parser.at(SyntaxToken::RBrace),
            Parser::node,
        );

        self.expect(SyntaxToken::RBrace);

        self.collect(SyntaxKind::Lambda, start);
    }

    fn lambda_params(&mut self) {
        let start = self.mark();

        self.node();

        match self.token {
            SyntaxToken::Arrow => {},
            SyntaxToken::Comma => {
                self.sequence(
                    |parser| !parser.at(SyntaxToken::Arrow),
                    Parser::atom,
                );
            }
            _ => {
                return;
            }
        }

        self.expect(SyntaxToken::Arrow);

        self.collect(SyntaxKind::Parameters, start);
    }

    fn lambda_cases(&mut self) {
        while self.at(SyntaxToken::Case) {
            let start = self.mark();

            self.expect(SyntaxToken::Case);

            let params = self.mark();
            self.sequence(
                |parser| !parser.at(SyntaxToken::Arrow),
                Parser::atom,
            );

            self.expect(SyntaxToken::Arrow);
            self.collect(SyntaxKind::Parameters, params);

            self.until(
                |parser| matches!(parser.token, SyntaxToken::Case | SyntaxToken::Else | SyntaxToken::RBrace),
                Parser::node,
            );

            self.collect(SyntaxKind::Case, start);
        }

        let else_ = self.mark();
        if self.maybe(SyntaxToken::Else) {
            self.until(
                |parser| parser.at(SyntaxToken::RBrace),
                Parser::node,
            );
            self.collect(SyntaxKind::Else, else_);
        }
    }

    fn apply(&mut self) {
        let start = self.mark();

        self.atom();

        if self.at(SyntaxToken::Colon) {
            self.type_annotation(start);
        }

        loop {
            match self.token {
                SyntaxToken::Dot    => self.get(start),
                SyntaxToken::LParen => self.call(start),
                _ => break,
            }

            if self.at(SyntaxToken::Colon) {
                self.type_annotation(start);
            }
        }
    }

    fn unary(&mut self) {
        let start = self.mark();

        match self.token {
            SyntaxToken::Not
          | SyntaxToken::Pos
          | SyntaxToken::Neg => {
                self.expect(self.token);
                self.apply();
                self.collect(SyntaxKind::Unary, start);
            }
            _ => {
                self.apply();
            }
        }
    }

    fn binary(&mut self, minimum_precedence: Precedence) {
        let start = self.mark();

        self.unary();

        while let Some((precedence, associativity)) = self.token.precedence()
          && precedence >= minimum_precedence
          && associativity != Associativity::None
        {
            self.take();

            let next_precedence = match associativity {
                 Associativity::None
               | Associativity::Left => precedence + 1,
                 Associativity::Right => precedence,
            };

            self.binary(next_precedence);
            self.collect(SyntaxKind::Binary, start);
        }
    }

    fn lvalue(&mut self) {
        self.binary(0 as Precedence);
    }

    fn xlvalue(&mut self) {
        let start = self.mark();

        self.lvalue();

        if self.at(SyntaxToken::LBrace) {
            self.lambda();
            self.collect(SyntaxKind::LambdaCall, start);
        }
    }

    fn rvalue(&mut self) {
        if self.at(SyntaxToken::If) {
            self.r#if();
        } else {
            self.xlvalue();
        }
    }

    fn pipe(&mut self) {
        let start = self.mark();

        self.xlvalue();

        while self.maybe(SyntaxToken::Pipe) {
            self.rvalue();
            self.collect(SyntaxKind::Pipe, start);
        }
    }

    fn r#if(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::If);
        self.expr();

        let then_ = self.mark();
        if self.maybe(SyntaxToken::Then) {
            self.node();
            self.collect(SyntaxKind::Then, then_);
        } else {
            self.node();
        }

        let else_ = self.mark();
        if self.maybe(SyntaxToken::Else) {
            self.node();
            self.collect(SyntaxKind::Else, else_);
        }

        self.collect(SyntaxKind::If, start);
    }

    fn r#yield(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::Yield);

        if !self.token.is_terminal() {
            self.expr();
        }

        self.collect(SyntaxKind::Yield, start);
    }

    fn r#break(&mut self) {
        let start = self.mark();
        self.expect(SyntaxToken::Break);
        self.collect(SyntaxKind::Break, start);
    }

    fn import(&mut self) {
        let start = self.mark();

        self.expect(SyntaxToken::Import);

        self.expect(SyntaxToken::Upper);

        while self.maybe(SyntaxToken::Dot) {
              self.expect(SyntaxToken::Upper);
        }

        let import = self.mark();
        if self.at(SyntaxToken::LParen) {
            self.expect(SyntaxToken::LParen);
            self.sequence(
                |parser| !parser.at(SyntaxToken::RParen),
                Parser::ident,
            );
            self.expect(SyntaxToken::RParen);
            self.collect(SyntaxKind::Import, import);
        }

        self.collect(SyntaxKind::Import, start);
    }

    fn type_annotation(&mut self, start: Marker) {
        self.expect(SyntaxToken::Colon);
        self.atom();
        self.collect(SyntaxKind::TypeAnnotation, start);
    }

    fn at(&self, kind: SyntaxToken) -> bool {
        self.token == kind
    }

    fn was(&self, kind: SyntaxToken) -> bool {
        self.nodes
            .iter()
            .rev()
            .skip_while(|node| node.is_trivia())
            .next()
            .map_or(false, |node| node.is_token(kind))
    }

    fn expect(&mut self, kind: SyntaxToken) {
        if self.at(kind) {
            self.take();
        } else {
            self.fail(format!("expected {kind}"));
            self.recover();
        }
    }

    fn maybe(&mut self, kind: SyntaxToken) -> bool {
        if self.at(kind) {
            self.take();
            true
        } else {
            false
        }
    }

    fn take(&mut self) {
        self.make();
        self.bump();
        self.skip_trivia();
    }

    fn collect(&mut self, kind: SyntaxKind, Marker(start): Marker) {
        let nodes = self.nodes.drain(start..).collect::<Vec<_>>();
        let span = if let (Some(first), Some(last)) = (nodes.first(), nodes.last()) {
            Span::combine(first.span, last.span)
        } else {
            Span::default()
        };
        self.nodes.push(Node::nodes(kind, nodes, span));
    }

    fn make(&mut self) {
        let span = self.lexer.span();

        let kind = match self.token {
            SyntaxToken::Lower => SyntaxKind::Lower,
            SyntaxToken::Upper => SyntaxKind::Upper,
            SyntaxToken::String => SyntaxKind::String,
            SyntaxToken::Number => SyntaxKind::Number,
            _ => {
                return self.nodes.push(Node::token(self.token, span));
            }
        };
        let value = self.lexer.slice().to_string();
        self.nodes.push(Node::node(kind, value, span));
    }

    fn sequence<F, P>(
        &mut self,
        predicate: P,
        mut parser: F,
    ) -> usize
    where
        P: Fn(&Self) -> bool, F: FnMut(&mut Self),
    {
        let mut counter = 0;

        while predicate(self) {
            parser(self);
            counter += 1;
            if !self.maybe(SyntaxToken::Comma) { break; }
        }

        counter
    }

    fn until<F, P>(
        &mut self,
        predicate: P,
        mut parser: F,
    )
    where
        P: Fn(&Self) -> bool, F: FnMut(&mut Self),
    {
        while !predicate(self) { parser(self); }
    }

    fn skip_trivia(&mut self) {
        while matches!(self.token, SyntaxToken::Space | SyntaxToken::Comment) {
            self.make();
            self.bump();
        }
    }

    fn unexpected(&mut self, expected: &[impl std::fmt::Display]) {
        let formatted = expected
            .iter()
            .map(|it| {
                format!("`{it}`")
            })
            .collect::<Vec<_>>()
            .join(" or ");

        self.fail(format!("expected {formatted}"));
        self.recover();
    }

    fn error(&mut self) {
        debug_assert!(self.at(SyntaxToken::Error));

        let error = self.lexer.slice().to_string();
        let span = self.lexer.span();
        self.nodes.push(Node::error(error, span));

        self.recover();
    }

    fn fail(&mut self, error: impl Into<String>) {
        self.nodes.push(Node::error(error.into(), self.lexer.span()));
    }

    // TODO: better recover from errors
    fn recover(&mut self) {
        self.bump();
    }

    fn peek(&mut self) -> SyntaxToken {
        self.lexer.lookahead(0)
    }

    fn bump(&mut self) -> SyntaxToken {
        self.token = self.lexer.next();
        self.token
    }

    fn mark(&self) -> Marker {
        Marker(self.nodes.len())
    }

    fn eof(&self) -> bool {
        self.at(SyntaxToken::EOF)
    }
}
