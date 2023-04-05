use std::vec;

use super::lexer::Lexer;
use super::node::{Associativity, Precedence, SyntaxKind, SyntaxNode};


#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Marker(usize);

#[derive(Debug, Clone)]
pub struct Parser<'p> {
    lexer: Lexer<'p>,
    nodes: Vec<SyntaxNode>,
    kind: SyntaxKind,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str) -> Parser {
        let mut parser = Parser {
            lexer: Lexer::new(source),
            nodes: vec![],
            kind: SyntaxKind::default(),
        };
        parser.bump(); // first token
        parser
    }

    fn mark(&self) -> Marker {
        Marker(self.nodes.len())
    }

    pub fn parse(mut self) -> Vec<SyntaxNode> {
        while !self.eof() {
            self.skip_trivia();
            self.node();
            self.skip_trivia();
        }
        self.nodes
    }

    fn node(&mut self) {
        match self.kind {
            SyntaxKind::If    => self.r#if(),
            SyntaxKind::Yield => self.r#yield(),
            SyntaxKind::Break => self.r#break(),
            SyntaxKind::Import => self.import(),
            _ => self.define(),
        }
    }

    fn expr(&mut self) {
        if self.at(SyntaxKind::If) {
            self.r#if();
        } else {
            self.pipe();
        }
    }

    fn define(&mut self) {
        let start = self.mark();

        self.lvalue();

        match self.kind {
            SyntaxKind::Equals => {
                self.expect(SyntaxKind::Equals);
                self.expr();
                let where_ = self.mark();
                if self.maybe(SyntaxKind::Where) {
                    self.expr();
                    self.collect(SyntaxKind::Where, where_);
                }
                self.collect(SyntaxKind::Let, start);
            }
            SyntaxKind::Walrus => {
                self.expect(SyntaxKind::Walrus);
                self.expr();
                self.collect(SyntaxKind::Set, start);
            }
            _ => {}
        }
    }

    fn ident(&mut self) {
        self.expect(SyntaxKind::Lower);
    }

    fn upper(&mut self) {
        let start = self.mark();

        self.expect(SyntaxKind::Upper);

        match self.kind {
            SyntaxKind::LParen => self.parens(),
            SyntaxKind::LBrace => self.record(),
            _ => {}
        }

        self.collect(SyntaxKind::Variant, start);
    }

    fn number(&mut self) {
        self.expect(SyntaxKind::Number);
    }

    fn string(&mut self) {
        self.expect(SyntaxKind::String);
    }

    fn field(&mut self) {
        let start = self.mark();

        while self.at(SyntaxKind::Dot) {
            self.make(); // dot
            self.bump(); // next
            match self.kind {
                SyntaxKind::Lower => self.ident(),
                SyntaxKind::Upper => self.upper(),
                SyntaxKind::Number => self.number(),
                SyntaxKind::String => self.string(),
                SyntaxKind::LParen => self.parens(),
                SyntaxKind::LBracket => self.brackets(),
                _ => {
                    return self.unexpected(&[SyntaxKind::Field]);
                }
            }
            self.collect(SyntaxKind::Field, start);
        }
    }

    fn element(&mut self) {
        let start = self.mark();

        if self.maybe(SyntaxKind::Range) {
            self.apply();
            self.collect(SyntaxKind::Spread, start);
        } else {
            self.expr();
        }
    }

    fn parens(&mut self) {
        let start = self.mark();

        self.expect(SyntaxKind::LParen);
        let total = self.sequence(
            |parser| !parser.at(SyntaxKind::RParen),
            Parser::element,
        );
        self.expect(SyntaxKind::RParen);

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

        self.expect(SyntaxKind::LBracket);

        self.sequence(
            |parser| !parser.at(SyntaxKind::RBracket),
            Parser::element,
        );

        self.expect(SyntaxKind::RBracket);

        self.collect(SyntaxKind::List, start);
    }

    fn braces(&mut self) {
        if matches!(self.peek(), SyntaxKind::Dot | SyntaxKind::Range) {
            self.record();
        } else {
            self.lambda();
        }
    }

    fn record(&mut self) {
        let start = self.mark();

        self.expect(SyntaxKind::LBrace);
        self.sequence(
            |parser| !parser.at(SyntaxKind::RBrace),
            Parser::property,
        );
        self.expect(SyntaxKind::RBrace);

        self.collect(SyntaxKind::Record, start);
    }

    fn property(&mut self) {
        let start = self.mark();

        match self.kind {
            SyntaxKind::Range => {
                self.take();
                if !self.at(SyntaxKind::RBrace) {
                    self.apply();
                }
            }
            SyntaxKind::Dot => {
                self.field();

                match self.kind {
                    SyntaxKind::Equals => {
                        self.take();
                        self.expr();
                    }
                    SyntaxKind::LBrace => {
                        self.braces();
                    }
                    SyntaxKind::Comma
                  | SyntaxKind::RBrace => {}
                    _ => self.unexpected(&[
                        SyntaxKind::Equals,
                        SyntaxKind::Comma,
                        SyntaxKind::RBrace,
                    ])
                }
            }
            _ => self.unexpected(&[
                SyntaxKind::Dot,
                SyntaxKind::Range,
            ])
        }

        self.collect(SyntaxKind::Property, start);
    }

    fn atom(&mut self) {
        match self.kind {
            SyntaxKind::Lower => self.ident(),
            SyntaxKind::Upper => self.upper(),
            SyntaxKind::Number => self.number(),
            SyntaxKind::String => self.string(),
            SyntaxKind::LParen => self.parens(),
            SyntaxKind::LBrace => self.braces(),
            SyntaxKind::LBracket => self.brackets(),
            SyntaxKind::Dot => self.field(),
            SyntaxKind::Error => self.error(),
            _ => self.unexpected(&[SyntaxKind::Expr])
        }
    }

    fn get(&mut self, start: Marker) {
        self.expect(SyntaxKind::Dot);
        self.atom();
        self.collect(SyntaxKind::Get, start);
    }

    fn call(&mut self, start: Marker) {
        self.expect(SyntaxKind::LParen);
        self.function_arguments();
        self.expect(SyntaxKind::RParen);
        self.collect(SyntaxKind::Call, start);
    }

    fn function_arguments(&mut self) {
        let start = self.mark();
        self.sequence(
            |parser| !parser.at(SyntaxKind::RParen),
            Parser::element,
        );
        self.collect(SyntaxKind::Arguments, start);
    }

    fn trailing_lambda(&mut self, start: Marker) {
        self.lambda();
        self.collect(SyntaxKind::Call, start);
    }

    fn lambda(&mut self) {
        let start = self.mark();

        self.expect(SyntaxKind::LBrace);

        if self.maybe(SyntaxKind::RBrace) {
            return self.collect(SyntaxKind::Lambda, start);
        }

        if self.at(SyntaxKind::Case) {
            self.lambda_cases();
        } else {
            self.lambda_params();
        }

        self.until(
            |parser| parser.at(SyntaxKind::RBrace),
            Parser::node,
        );

        self.expect(SyntaxKind::RBrace);

        self.collect(SyntaxKind::Lambda, start);
    }

    fn lambda_params(&mut self) {
        let start = self.mark();

        self.node();

        match self.kind {
            SyntaxKind::Arrow => {},
            SyntaxKind::Comma => {
                self.sequence(
                    |parser| !parser.at(SyntaxKind::Arrow),
                    Parser::atom,
                );
            }
            _ => {
                return;
            }
        }

        self.collect(SyntaxKind::Parameters, start);

        self.expect(SyntaxKind::Arrow);
    }

    fn lambda_cases(&mut self) {
        while self.at(SyntaxKind::Case) {
            let start = self.mark();

            self.expect(SyntaxKind::Case);

            let params = self.mark();
            self.sequence(
                |parser| !parser.at(SyntaxKind::Arrow),
                Parser::atom,
            );
            self.collect(SyntaxKind::Parameters, params);

            self.expect(SyntaxKind::Arrow);

            self.until(
                |parser| matches!(parser.kind, SyntaxKind::Case | SyntaxKind::Else | SyntaxKind::RBrace),
                Parser::node,
            );

            self.collect(SyntaxKind::Case, start);
        }

        let else_ = self.mark();
        if self.maybe(SyntaxKind::Else) {
            self.until(
                |parser| parser.at(SyntaxKind::RBrace),
                Parser::node,
            );
            self.collect(SyntaxKind::Else, else_);
        }
    }

    fn apply(&mut self) {
        let start = self.mark();

        self.atom();

        loop {
            match self.kind {
                SyntaxKind::Dot    => self.get(start),
                SyntaxKind::LParen => self.call(start),
                SyntaxKind::LBrace => self.trailing_lambda(start),
                _ => break,
            }
        }
    }

    fn unary(&mut self) {
        let start = self.mark();

        match self.kind {
            SyntaxKind::Not
          | SyntaxKind::Pos
          | SyntaxKind::Neg => {
                self.expect(self.kind);
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

        while let Some((precedence, associativity)) = self.kind.precedence()
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

    fn rvalue(&mut self) {
        if self.at(SyntaxKind::If) {
            self.r#if();
        } else {
            self.lvalue();
        }
    }

    fn pipe(&mut self) {
        let start = self.mark();

        self.lvalue();

        while self.maybe(SyntaxKind::Pipe) {
            self.rvalue();
            self.collect(SyntaxKind::Pipe, start);
        }
    }

    fn r#if(&mut self) {
        let start = self.mark();

        self.expect(SyntaxKind::If);
        self.expr();

        let then_ = self.mark();
        if self.maybe(SyntaxKind::Then) {
            self.node();
            self.collect(SyntaxKind::Then, then_);
        } else {
            self.node();
        }

        let else_ = self.mark();
        if self.maybe(SyntaxKind::Else) {
            self.node();
            self.collect(SyntaxKind::Else, else_);
        }

        self.collect(SyntaxKind::If, start);
    }

    fn r#yield(&mut self) {
        let start = self.mark();

        self.expect(SyntaxKind::Yield);

        if !self.kind.is_terminal() {
            self.expr();
        }

        self.collect(SyntaxKind::Yield, start);
    }

    fn r#break(&mut self) {
        self.expect(SyntaxKind::Break);
    }

    fn import(&mut self) {
        let start = self.mark();

        self.expect(SyntaxKind::Import);

        self.expect(SyntaxKind::Upper);

        while self.maybe(SyntaxKind::Dot) {
              self.expect(SyntaxKind::Upper);
        }

        let imported_names = self.mark();

        if self.at(SyntaxKind::LParen) {
            self.expect(SyntaxKind::LParen);
            self.sequence(
                |parser| !parser.at(SyntaxKind::RParen),
                Parser::ident,
            );
            self.expect(SyntaxKind::RParen);
        }

        self.collect(SyntaxKind::Import, imported_names);

        self.collect(SyntaxKind::Import, start);
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.kind == kind
    }

    fn expect(&mut self, kind: SyntaxKind) {
        if self.at(kind) {
            self.take();
        } else {
            self.fail(format!("expected {kind}"));
            self.recover();
        }
    }

    fn maybe(&mut self, kind: SyntaxKind) -> bool {
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
        let nodes = self.nodes.drain(start..).collect();
        self.nodes.push(SyntaxNode::nodes(kind, nodes));
    }

    fn make(&mut self) {
        match self.kind {
            SyntaxKind::Lower
          | SyntaxKind::Upper
          | SyntaxKind::String
          | SyntaxKind::Number => {
                let value = self.lexer.slice().to_string();
                self.nodes.push(SyntaxNode::node(self.kind, value));
            }
            _ => {
                let size = self.lexer.size() as u32;
                self.nodes.push(SyntaxNode::basic(self.kind, size));
            }
        }
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
            if !self.maybe(SyntaxKind::Comma) { break; }
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
        while matches!(self.kind, SyntaxKind::Space | SyntaxKind::Comment) {
            self.make();
            self.bump();
        }
    }

    fn unexpected(&mut self, expected: &[SyntaxKind]) {
        let formatted = expected
            .iter()
            .map(|it| {
                format!("`{it}`")
            })
            .collect::<Vec<_>>()
            .join(" or ");

        self.fail(format!("expected {formatted}"));

        self.bump();
    }

    fn error(&mut self) {
        debug_assert!(self.at(SyntaxKind::Error));

        let error = self.lexer.slice().to_string();
        let size = self.lexer.size() as u32;
        self.nodes.push(SyntaxNode::error(error, size));

        self.recover();
    }

    fn fail(&mut self, error: impl Into<String>) {
        self.nodes.push(SyntaxNode::error(error.into(), 0));
    }

    // TODO: better recover from errors
    fn recover(&mut self) {
        self.bump();
    }

    fn bump(&mut self) -> SyntaxKind {
        self.kind = self.lexer.next();
        self.kind
    }

    fn peek(&mut self) -> SyntaxKind {
        self.lexer.lookahead(0)
    }

    fn eof(&self) -> bool {
        self.at(SyntaxKind::EOF)
    }
}
