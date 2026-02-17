use crate::error::{Error, Result};
use crate::lexer::Tokens;
use crate::syntax::{Module, Name, Node, NodeKind, Operator, OperatorKind, Precedence};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
struct Parser<'parser> {
    tokens: Tokens<'parser>,
    token: Token,
}

impl<'parser> Parser<'parser> {
    fn new(tokens: Tokens<'parser>) -> Parser<'parser> {
        Parser {
            tokens,
            token: Token::default(),
        }
        .init()
    }

    fn init(mut self) -> Parser<'parser> {
        self.token = self.tokens.token();
        self
    }

    fn bump(&mut self) {
        self.tokens.bump();
        self.token = self.tokens.token();
    }

    fn fail<T>(&mut self, message: impl Into<String>) -> Result<T> {
        let span = self.token.span;
        self.recover();
        Err(Error::error(span, message.into()))
    }

    fn recover(&mut self) {
        while !self.tokens.done() {
            self.bump();
        }
    }

    pub fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn done(&self) -> bool {
        self.tokens.done()
    }

    fn top_level(&mut self) -> Result<Node> {
        if self.at(TokenKind::Identifier) && self.tokens.peek().is(TokenKind::Equals) {
            self.define()
        } else {
            self.expr()
        }
    }

    fn define(&mut self) -> Result<Node> {
        let name = self.name()?;
        self.expect(TokenKind::Equals)?;
        let value = self.expr()?;

        Ok(Node::define(name, value))
    }

    fn expr(&mut self) -> Result<Node> {
        self.binary(0 as Precedence)
    }

    fn atom(&mut self) -> Result<Node> {
        let token = self.token;
        match token.kind {
            TokenKind::Identifier => self.ident(),
            TokenKind::Number => self.number(),
            TokenKind::Integer => self.integer(),
            TokenKind::LParen => self.parens(),
            _ => {
                self.fail(match token.kind {
                    TokenKind::UnexpectedCharacter => "unexpected character".into(),
                    TokenKind::UnterminatedString => "unterminated string".into(),
                    _ => format!("unexpected {}", token.kind),
                })
            }
        }
    }

    fn name(&mut self) -> Result<Name> {
        let span = self.token.span;
        self.expect(TokenKind::Identifier)?;

        Ok(Name { span })
    }

    fn literal(&mut self, kind: TokenKind, node: NodeKind) -> Result<Node> {
        let span = self.token.span;
        self.expect(kind)?;
        Ok(Node::new(node, span))
    }

    fn ident(&mut self) -> Result<Node> {
        let name = self.name()?;
        let span = name.span;
        Ok(Node::new(NodeKind::Name(name), span))
    }

    fn number(&mut self) -> Result<Node> {
        self.literal(TokenKind::Number, NodeKind::Number)
    }

    fn integer(&mut self) -> Result<Node> {
        self.literal(TokenKind::Integer, NodeKind::Integer)
    }

    fn parens(&mut self) -> Result<Node> {
        self.expect(TokenKind::LParen)?;
        let expr = self.expr()?;
        self.expect(TokenKind::RParen)?;

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Node> {
        let Some(operator) = self.operator(0 as Precedence, true) else {
            return self.atom();
        };

        let expr = self.unary()?;

        Ok(Node::unary(operator, expr))
    }

    fn binary(&mut self, precedence: Precedence) -> Result<Node> {
        let mut expr = self.unary()?;

        loop {
            // accept newlines before binary operators
            let newline = self.newline();

            let Some(operator) = self.operator(precedence, false) else {
                break;
            };

            if !newline {
                // accept newlines after binary operators
                // but only if there wasn't a new line before the operator
                self.newline();
            }

            let lexpr = expr;
            let rexpr = self.binary(operator.next_precedence())?;

            expr = Node::binary(operator, lexpr, rexpr);
        }

        Ok(expr)
    }

    fn operator(&mut self, precedence: Precedence, unary: bool) -> Option<Operator> {
        let token = self.token;

        let kind = match token.kind {
            TokenKind::Plus if unary => OperatorKind::Pos,
            TokenKind::Minus if unary => OperatorKind::Neg,
            TokenKind::Not if unary => OperatorKind::Not,
            TokenKind::Plus => OperatorKind::Add,
            TokenKind::Minus => OperatorKind::Sub,
            TokenKind::Star => OperatorKind::Mul,
            TokenKind::Slash => OperatorKind::Div,
            TokenKind::Caret => OperatorKind::Pow,
            TokenKind::Mod => OperatorKind::Mod,
            TokenKind::And => OperatorKind::And,
            TokenKind::Or => OperatorKind::Or,
            TokenKind::EqEq => OperatorKind::Eq,
            TokenKind::NoEq => OperatorKind::Ne,
            TokenKind::Lt => OperatorKind::Lt,
            TokenKind::LtEq => OperatorKind::Le,
            TokenKind::Gt => OperatorKind::Gt,
            TokenKind::GtEq => OperatorKind::Ge,
            _ => return None,
        };

        let operator = Operator {
            kind,
            span: token.span,
        };

        if operator.precedence() >= precedence {
            self.bump();
            Some(operator)
        } else {
            None
        }
    }

    fn newline(&mut self) -> bool {
        self.maybe(TokenKind::EOL)
    }

    fn endline(&mut self) -> Result<()> {
        if self.maybe(TokenKind::EOL) || self.maybe(TokenKind::Semi) || self.maybe(TokenKind::EOF) {
            Ok(())
        } else {
            self.fail(self.unexpected(&[TokenKind::EOL]))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.at(kind) {
            self.bump();
            Ok(())
        } else {
            self.fail(self.unexpected(&[kind]))
        }
    }

    fn maybe(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn seq<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.newline();

        while !self.at(until) {
            result.push(parse(self)?);

            self.newline();
            if !self.maybe(TokenKind::Comma) {
                break;
            }
            self.newline();
        }

        Ok(result)
    }

    fn many<F, T>(&mut self, until: TokenKind, mut parse: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];

        self.newline();

        while !self.at(until) {
            result.push(parse(self)?);
            self.endline()?;
        }

        Ok(result)
    }

    fn unexpected(&self, expected: &[TokenKind]) -> String {
        let expected: Vec<_> = expected.iter().map(|it| format!("`{it}`")).collect();
        format!("expected {}, found `{}`", expected.join(", "), self.token.kind)
    }
}

pub fn parse<'parser>(tokens: Tokens<'parser>) -> Result<Module> {
    let mut parser = Parser::new(tokens);
    let mut nodes = vec![];

    parser.newline();

    while !parser.done() {
        let node = parser.top_level()?;
        nodes.push(node);
        parser.endline()?;
    }

    Ok(Module { nodes })
}
