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
            lexer: Lexer::new(&source.content),
            prev : Token::default(),
            token: Token::default(),
            peek : Token::default(),
            source,
        };

        parser.bump(); // token
        parser.bump(); // peek

        parser
    }

    fn parse_module(mut self) -> Result<Module> {
        let mut expressions = vec![];

        while !self.done() {
            expressions.push(self.parse_top_level()?);
        }

        Ok(Module { expressions })
    }

    fn parse_top_level(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Lower => {
                match self.peek.kind {
                    TokenKind::Equals => self.parse_def(),
                    TokenKind::LParen => self.parse_fn(),
                    TokenKind::Do     => self.parse_block(),
                    _ => {
                        return Err(self.unexpected())
                    }
                }
            }
            TokenKind::Upper => {
                match self.peek.kind {
                    TokenKind::LParen => self.parse_ctor(),
                    _ => {
                        return Err(self.unexpected())
                    }
                }
            }
            _ => Err(self.unexpected())
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Match => self.parse_match(),
            TokenKind::Do    => self.parse_block(),
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
          | TokenKind::LBracket)
    }

    fn parse_term(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Lower => self.parse_lower(),
            TokenKind::Upper => self.parse_upper(),
            TokenKind::Number => self.parse_number(),
            TokenKind::String => self.parse_string(),
            TokenKind::LParen => self.parse_parens(),
            TokenKind::LBrace => self.parse_braces(),
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

    fn parse_lower(&mut self) -> Result<Expr> {
        Ok(Expr::Name(self.parse_name()?))
    }

    fn parse_upper(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::Upper)?;
        let value = self.source.content[start.range()].to_string();
        
        match &value[..] {
            "True"  => return Ok(Expr::True(start)),
            "False" => return Ok(Expr::False(start)),
            _ => {}
        }

        let name = Name { value, span: start };

        let value = if self.match_term() && self.match_lines() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Expr::Variant(
            box Variant { name, value, span: self.span(start) }))
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let span  = self.expect(TokenKind::Number)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::Number(Literal { value, span }))
    }

    fn parse_string(&mut self) -> Result<Expr> {
        let span  = self.expect(TokenKind::String)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::String(Literal { value, span }))
    }

    fn parse_parens(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let mut values = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
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
            TokenKind::LBracket,
            TokenKind::RBracket,
            Self::parse_expr)?;

        Ok(Expr::List(
            box List { values, span: self.span(start) }))
    }

    fn parse_braces(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let properties = self.parse_sequence_of(
            TokenKind::LBrace,
            TokenKind::RBrace,
            Self::parse_property)?;

        Ok(Expr::Record(
            box Record { properties, span: self.span(start) }))
    }

    fn parse_property(&mut self) -> Result<(Name, Option<Expr>)> {
        let key = self.parse_name()?;

        let val = match self.token.kind {
            TokenKind::Equals => {
                self.bump();
                Some(self.parse_expr()?)
            }
            TokenKind::LBrace => {
                Some(self.parse_braces()?)
            }
            TokenKind::Comma
          | TokenKind::RParen => {
                None
            }
            _ => return Err(self.unexpected())
        };

        Ok((key, val))
    }

    fn parse_def(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let name = self.parse_name()?;
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expr()?;

        Ok(Expr::Def(
            box Def { name, value, span: self.span(start) }))
    }

    fn parse_ctor(&mut self) -> Result<Expr> {
        let start = self.expect(TokenKind::Upper)?;

        let value = self.source.content[start.range()].to_string();
        let name  = Name { value, span: start };

        let parameters = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_parameter)?;

        let block = self.parse_inner_block()?;

        Ok(Expr::Ctor(
            box Fn { name, parameters, block, span: self.span(start) }))
    }

    fn parse_fn(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let name = self.parse_name()?;

        let parameters = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_parameter)?;

        let block = self.parse_inner_block()?;

        Ok(Expr::Fn(
            box Fn { name, parameters, block, span: self.span(start) }))
    }

    fn parse_parameter(&mut self) -> Result<Parameter> {
        let start = self.token.span;

        let patt = self.parse_expr()?;

        let value = if self.match_lines() && self.maybe_eat(TokenKind::Equals) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        
        Ok(Parameter { patt, value, span: self.span(start) })
    }

    fn parse_block(&mut self) -> Result<Expr> {
        Ok(Expr::Block(box self.parse_inner_block()?))
    }

    fn parse_inner_block(&mut self) -> Result<Block> {
        let start = self.token.span;

        let block = self.maybe_eat(TokenKind::Do);

        let parameters = if block && self.match_token(TokenKind::LParen) && self.match_lines() {
            Some(self.parse_sequence_of(
                    TokenKind::LParen,
                    TokenKind::RParen,
                    Self::parse_parameter)?)
        } else {
            None
        };

        let expressions = self.parse_while(
            Self::parse_expr,
            |this| !this.match_token(TokenKind::End))?;

        self.expect_exactly(TokenKind::End)?;

        Ok(Block { parameters, expressions, span: self.span(start) })
    }

    fn parse_match(&mut self) -> Result<Expr> {
        let start = self.token.span;

        self.expect(TokenKind::Match)?;

        let predicate = self.parse_term()?;

        let mut cases = vec![];
        
        while self.match_token(TokenKind::Case) {
            self.expect(TokenKind::Case)?;
            let cond = self.parse_expr()?;
            self.expect(TokenKind::Arrow)?;
            let then = self.parse_expr()?;
            cases.push((cond, then));
        }

        let otherwise = if self.maybe_eat(TokenKind::Else) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(TokenKind::End)?;

        Ok(Expr::Match(
            box Match { predicate, cases, otherwise, span: self.span(start), }))
    }

    fn parse_initial(&mut self) -> Result<Expr> {
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

    fn parse_call(&mut self, callee: Expr) -> Result<Expr> {
        let start = callee.span();

        let arguments = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_argument)?;

        Ok(Expr::Call(
            box Call { callee, arguments, span: self.span(start) }))
    }

    fn parse_argument(&mut self) -> Result<Argument> {
        let start = self.token.span;

        let mut name = if self.match_token(TokenKind::Lower)
                        && self.match_peek(TokenKind::Colon) {
            let name = self.parse_name()?;
            self.expect(TokenKind::Colon)?;
            Some(name)
        } else {
            None
        };
        
        let value = self.parse_expr()?;
        
        if let Expr::Fn(box Fn { name: function_name, .. }) = &value {
            if name.is_none() {
                name = Some(function_name.clone());
            }
        }
        
        Ok(Argument { name, value, span: self.span(start) })
    }

    fn parse_dot(&mut self, source: Expr) -> Result<Expr> {
        let start = source.span();

        self.expect(TokenKind::Dot)?;
        let value = self.parse_term()?;

        Ok(Expr::Get(
            box Get { source, value, span: self.span(start) }))
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        if !self.token.is_operator() {
            return self.parse_initial();
        }

        let operator = self.parse_operator()?;

        if self.prev.is_delimiter()
        && self.peek.is_delimiter() {
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

                // partial application of operators
                if self.token.is_delimiter() {
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

        let kind = match self.token.kind {
            TokenKind::Add   => OperatorKind::Add,
          | TokenKind::Sub   => OperatorKind::Sub,
          | TokenKind::Mul   => OperatorKind::Mul,
          | TokenKind::Div   => OperatorKind::Div,
          | TokenKind::Rem   => OperatorKind::Rem,
          | TokenKind::And   => OperatorKind::And,
          | TokenKind::Or    => OperatorKind::Or,
          | TokenKind::Not   => OperatorKind::Not,
          | TokenKind::Eq    => OperatorKind::Eq,
          | TokenKind::Ne    => OperatorKind::Ne,
          | TokenKind::Lt    => OperatorKind::Lt,
          | TokenKind::Le    => OperatorKind::Le,
          | TokenKind::Gt    => OperatorKind::Gt,
          | TokenKind::Ge    => OperatorKind::Ge,
          | TokenKind::Pipe  => OperatorKind::Pipe,
          | TokenKind::Range => OperatorKind::Range,
            _ => {
                return Err(NetrineError::error(
                    span,
                    format!("`{}` is not a valid operator", self.token.kind)));
            }
        };

        Ok(Operator { kind, span })
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

        while !self.match_token(close) {

            result.push(f(self)?);

            if !self.maybe_eat(TokenKind::Comma) {
                break;
            }
        }

        self.expect(close)?;

        Ok(result)
    }

    fn parse_while<T, P, F>(&mut self, mut f: F, pred: P)
    -> Result<Vec<T>>
    where
        P: core::ops::Fn(&Self) -> bool, F: FnMut(&mut Self) -> Result<T>
    {
        let mut result = vec![];

        while pred(&self) {
            result.push(f(self)?);
        }

        Ok(result)
    }

    fn bump(&mut self) -> Span {
        let span = self.token.span;

        loop {
            self.prev  = self.token;
            self.token = self.peek;
            self.peek  = if let Some(token) = self.lexer.next() {
                token
            } else {
                Token::default()
            };

            if self.token.kind != TokenKind::Line {
                break
            }
        }

        span
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
                format!("Expected `{}`, but found `{}`", expected, self.token.kind)))
        }
    }

    fn expect_exactly(&mut self, expected: TokenKind) -> Result<Span> {
        if self.token.kind == expected {
            Ok(self.bump())
        } else if self.prev.kind == expected {
            Ok(self.prev.span)
        } else {
            Err(NetrineError::error(
                self.token.span,
                format!("Expected `{}`, but found `{}`", expected, self.token.kind)))
        }
    }

    fn maybe_eat(&mut self, expected: TokenKind) -> bool {
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
        self.prev.kind != TokenKind::Line
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

    fn parse(source: &str) -> Result<Expr> {
        let source = Source {
            path: PathBuf::from("test"), content: source.into()
        };
        Parser::new(&source).parse_expr()
    }
}
