use crate::error::{Error, Result};
use crate::lexer::Tokens;
use crate::source::WithSpan;
use crate::syntax::{End, Node, NodeIndex, NodeKind, Operator, Precedence, Syntax, TokenIndex};
use crate::token::Token;

#[derive(Debug)]
struct Parser<'parser> {
    tokens: Tokens<'parser>,
    current: WithSpan<Token>,
    syntax: Syntax,
}

impl<'parser> Parser<'parser> {
    fn new(tokens: Tokens<'parser>) -> Parser<'parser> {
        Parser {
            tokens,
            current: WithSpan::default(),
            syntax: Syntax::new(),
        }
        .init()
    }

    fn init(mut self) -> Parser<'parser> {
        self.current = self.tokens.token();
        self
    }

    fn bump(&mut self) {
        self.syntax.push_token(self.current.value, self.current.span);
        self.tokens.bump();
        self.current = self.tokens.token();
    }

    fn fail<T>(&mut self, message: impl Into<String>) -> Result<T> {
        let span = self.current.span;
        self.recover();
        Err(Error::error(span, message.into()))
    }

    fn recover(&mut self) {
        while !self.tokens.done() {
            self.bump();
        }
    }

    fn at(&self, kind: Token) -> bool {
        self.current.value == kind
    }

    fn peek_is(&self, kind: Token) -> bool {
        self.tokens.peek().value == kind
    }

    fn done(&self) -> bool {
        self.tokens.done()
    }

    fn node(&mut self, kind: NodeKind, token: TokenIndex, size: u32) -> NodeIndex {
        self.syntax.push_node(Node::new(kind, token), size)
    }

    fn start(&mut self, kind: NodeKind) -> NodeIndex {
        let token = self.syntax.token_index();
        self.node(kind, token, 0)
    }

    fn finish(&mut self, start: NodeIndex, end: End) {
        let token = self.syntax.token_index().prev();
        let size = self.size(start);
        let node = NodeKind::End(end);

        self.node(node, token, size);
        self.syntax.resize(start, size);
    }

    fn size(&self, start: NodeIndex) -> u32 {
        let curr = self.syntax.node_index();
        u32::from(curr) - u32::from(start)
    }

    fn top_level(&mut self) -> Result<()> {
        if self.at(Token::Let) {
            self.define()
        } else {
            self.expr()
        }
    }

    fn define(&mut self) -> Result<()> {
        let start = self.start(NodeKind::Let);

        self.expect(Token::Let)?;

        if self.at(Token::Identifier) && self.peek_is(Token::LParen) {
            return self.function(start);
        }

        self.name()?;
        self.expect(Token::Equals)?;
        self.expr()?;

        self.finish(start, End::Let);

        Ok(())
    }

    fn function(&mut self, start: NodeIndex) -> Result<()> {
        self.syntax.replace(start, NodeKind::Fn);

        self.name()?;
        self.params()?;
        self.expect(Token::Equals)?;
        self.expr()?;

        self.finish(start, End::Fn);

        Ok(())
    }

    fn params(&mut self) -> Result<()> {
        let start = self.start(NodeKind::Parameters);

        self.seq(Token::LParen, Token::RParen, |parser| {
            let start = parser.start(NodeKind::Parameter);
            parser.name()?;
            parser.finish(start, End::Parameter);
            Ok(())
        })?;

        self.finish(start, End::Parameters);

        Ok(())
    }

    fn expr(&mut self) -> Result<()> {
        self.binary(0 as Precedence)
    }

    fn atom(&mut self) -> Result<()> {
        let token = self.current;
        match token.value {
            Token::Identifier => self.ident(),
            Token::Number => self.number(),
            Token::Integer => self.integer(),
            Token::LParen => self.parens(),
            _ => {
                self.fail(match token.value {
                    Token::UnexpectedCharacter => "unexpected character".into(),
                    Token::UnterminatedString => "unterminated string".into(),
                    _ => format!("unexpected {}", token.value),
                })
            }
        }
    }

    fn literal(&mut self, kind: Token, node: NodeKind) -> Result<()> {
        let token = self.syntax.token_index();
        self.expect(kind)?;
        self.node(node, token, 0);
        Ok(())
    }

    fn name(&mut self) -> Result<()> {
        self.literal(Token::Identifier, NodeKind::Name)
    }

    fn ident(&mut self) -> Result<()> {
        self.literal(Token::Identifier, NodeKind::Identifier)
    }

    fn number(&mut self) -> Result<()> {
        self.literal(Token::Number, NodeKind::Number)
    }

    fn integer(&mut self) -> Result<()> {
        self.literal(Token::Integer, NodeKind::Integer)
    }

    fn parens(&mut self) -> Result<()> {
        let start = self.start(NodeKind::Group);

        self.expect(Token::LParen)?;
        self.expr()?;
        self.expect(Token::RParen)?;

        self.finish(start, End::Group);

        Ok(())
    }

    fn unary(&mut self) -> Result<()> {
        if let Some((operator, token)) = self.operator(0 as Precedence, true) {
            let start = self.syntax.node_index();
            self.unary()?;
            self.node(NodeKind::Unary(operator), token, self.size(start));
        } else {
            self.atom()?;
        }

        Ok(())
    }

    fn binary(&mut self, precedence: Precedence) -> Result<()> {
        let start = self.syntax.node_index();

        self.unary()?;

        while let Some((operator, token)) = self.operator(precedence, false) {
            self.newline(); // accepts newlines if the line ends with an operator

            let next_precedence = if operator == Operator::Pow {
                operator.precedence()
            } else {
                operator.precedence() + 1
            };

            self.binary(next_precedence)?;
            self.node(NodeKind::Binary(operator), token, self.size(start));
        }

        Ok(())
    }

    fn operator(&mut self, precedence: Precedence, unary: bool) -> Option<(Operator, TokenIndex)> {
        let operator = match self.current.value {
            Token::Plus if unary => Operator::Pos,
            Token::Minus if unary => Operator::Neg,
            Token::Not if unary => Operator::Not,
            Token::Plus => Operator::Add,
            Token::Minus => Operator::Sub,
            Token::Star => Operator::Mul,
            Token::Slash => Operator::Div,
            Token::Caret => Operator::Pow,
            Token::Mod => Operator::Mod,
            Token::And => Operator::And,
            Token::Or => Operator::Or,
            Token::EqEq => Operator::Eq,
            Token::NoEq => Operator::Ne,
            Token::Lt => Operator::Lt,
            Token::LtEq => Operator::Le,
            Token::Gt => Operator::Gt,
            Token::GtEq => Operator::Ge,
            _ => return None,
        };

        if operator.precedence() >= precedence {
            let token = self.syntax.token_index();
            self.bump();
            Some((operator, token))
        } else {
            None
        }
    }

    fn newline(&mut self) -> bool {
        self.maybe(Token::EOL)
    }

    fn endline(&mut self) -> Result<()> {
        if self.maybe(Token::EOL) || self.maybe(Token::Semi) || self.maybe(Token::EOF) {
            Ok(())
        } else {
            self.fail(self.unexpected(&[Token::EOL]))
        }
    }

    fn expect(&mut self, kind: Token) -> Result<()> {
        if self.at(kind) {
            self.bump();
            Ok(())
        } else {
            self.fail(self.unexpected(&[kind]))
        }
    }

    fn maybe(&mut self, kind: Token) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn seq<F>(&mut self, before: Token, after: Token, mut parse: F) -> Result<usize>
    where
        F: FnMut(&mut Self) -> Result<()>,
    {
        let mut count = 0;

        self.newline();

        self.expect(before)?;

        while !self.at(after) {
            parse(self)?;
            count += 1;

            self.newline();
            if !self.maybe(Token::Comma) {
                break;
            }
            self.newline();
        }

        self.expect(after)?;

        Ok(count)
    }

    fn many<F>(&mut self, before: Token, after: Token, mut parse: F) -> Result<usize>
    where
        F: FnMut(&mut Self) -> Result<()>,
    {
        let mut count = 0;

        self.newline();

        self.expect(before)?;

        while !self.at(after) {
            parse(self)?;
            count += 1;
            self.newline();
        }

        self.expect(after)?;

        Ok(count)
    }

    fn unexpected(&self, expected: &[Token]) -> String {
        let expected: Vec<_> = expected.iter().map(|it| format!("`{it}`")).collect();
        format!("expected {}, found `{}`", expected.join(", "), self.current.value)
    }
}

pub fn parse<'parser>(tokens: Tokens<'parser>) -> Result<Syntax> {
    let mut parser = Parser::new(tokens);

    parser.newline();

    while !parser.done() {
        parser.top_level()?;
        parser.endline()?;
    }

    Ok(parser.syntax)
}
