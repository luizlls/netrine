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
                    _ => {
                        Err(self.unexpected())
                    }
                }
            }
            _ => Err(self.unexpected())
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::If => self.parse_if(),
            TokenKind::Lower
            if self.match_peek(TokenKind::Equals) => {
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

    fn parse_term(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::It => self.parse_it(),
            TokenKind::Lower => self.parse_lower(),
            TokenKind::Upper => self.parse_upper(),
            TokenKind::Number => self.parse_number(),
            TokenKind::String => self.parse_string(),
            TokenKind::LParen => self.parse_parens(),
            TokenKind::LBrace => self.parse_braces(),
            TokenKind::LBracket => self.parse_lambda(),
            TokenKind::Hash => self.parse_record(true),
            _ => {
                Err(self.unexpected())
            }
        }
    }

    fn parse_patt(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Lower => self.parse_lower(),
            TokenKind::Upper => self.parse_upper(),
            TokenKind::Number => self.parse_number(),
            TokenKind::String => self.parse_string(),
            TokenKind::LParen => self.parse_parens(),
            TokenKind::LBrace => self.parse_braces(),
            TokenKind::Hash   => self.parse_record(true),
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

        let value = if self.match_lines() && self.match_term() {
            match self.token.kind {
                TokenKind::LParen => {
                    Some(self.parse_parens()?)
                }
                TokenKind::LBrace => {
                    Some(self.parse_record(false)?)
                }
                TokenKind::Lower => {
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
        let span  = self.expect(TokenKind::Number)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::Number(Literal { value, span }))
    }

    fn parse_string(&mut self) -> Result<Expr> {
        let span  = self.expect(TokenKind::String)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::String(Literal { value, span }))
    }
    
    fn parse_it(&mut self) -> Result<Expr> {
        let span  = self.expect(TokenKind::It)?;
        Ok(Expr::It(span))
    }

    fn parse_block(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let expressions = self.parse_sequence_of(
            TokenKind::LBrace,
            TokenKind::RBrace,
            Self::parse_expr)?;

        Ok(Expr::Block(
            box Block { expressions, span: self.span(start) }))
    }

    fn parse_inner(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let mut expressions = vec![];

        while !self.match_token(TokenKind::RBrace)
           && !self.done() {
            expressions.push(self.parse_expr()?);
        }

        Ok(Expr::Block(
            box Block { expressions, span: self.span(start) }))
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

    fn parse_lambda(&mut self) -> Result<Expr> {
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

        self.expect(TokenKind::LBrace)?;

        let parameters = if self.match_token(TokenKind::Pipe) {
            self.parse_sequence_of(
                TokenKind::Pipe,
                TokenKind::Pipe,
                Self::parse_parameter)?
        } else {
            vec![]
        };

        let value = self.parse_inner()?;

        self.expect(TokenKind::RBrace)?;

        Ok(Expr::Lambda(
            box Lambda { parameters, value, span: self.span(start) } ))
    }

    fn parse_record(&mut self, anonymous: bool) -> Result<Expr> {
        let start = self.token.span;

        if anonymous {
            self.expect(TokenKind::Hash)?;
        }

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
          | TokenKind::RBrace => {
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

    fn parse_fn(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let name = self.parse_name()?;

        let parameters = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_parameter)?;

        let value = match self.token.kind {
            TokenKind::Equals => {
                self.expect(TokenKind::Equals)?;
                self.parse_expr()?
            }
            TokenKind::LBrace => {
                self.parse_block()?
            }
            _ => return Err(self.unexpected())
        };

        Ok(Expr::Fn(
            box Fn { name, parameters, value, span: self.span(start) }))
    }

    fn parse_parameter(&mut self) -> Result<Parameter> {
        let start = self.token.span;

        let patt = self.parse_patt()?;

        let value = if self.match_lines() && self.maybe_eat(TokenKind::Colon) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        
        Ok(Parameter { patt, value, span: self.span(start) })
    }

    fn parse_if(&mut self) -> Result<Expr> {
        let start = self.token.span;

        self.expect(TokenKind::If)?;
        let pred = self.parse_expr()?;

        self.expect(TokenKind::Then)?;
        let then = self.parse_expr()?;
        
        self.expect(TokenKind::Else)?;
        let otherwise = Some(self.parse_expr()?);

        Ok(Expr::If(
            box If { pred, then, otherwise, span: self.span(start), }))
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
                TokenKind::LBrace => {
                    expression = self.parse_trailling(expression)?;
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
        let value = self.parse_expr()?;
        Ok(Argument { name: None, value, span: self.span(start) })
    }

    fn parse_dot(&mut self, source: Expr) -> Result<Expr> {
        let start = source.span();

        self.expect(TokenKind::Dot)?;
        let value = self.parse_term()?;

        Ok(Expr::Get(
            box Get { source, value, span: self.span(start) }))
    }

    fn parse_trailling(&mut self, callee: Expr) -> Result<Expr> {
        let start = callee.span();
        match callee {
            Expr::Name(_) => {
                let value = self.parse_braces()?;
                let arguments = vec![
                    Argument { name: None, value, span: self.span(start) }
                ];

                Ok(Expr::Call(
                    box Call { callee, arguments, span: self.span(start) }))
            }
            Expr::Call(box Call { callee, mut arguments, .. }) => {
                let value = self.parse_braces()?;
                arguments.push(
                    Argument { name: None, value, span: self.span(start) });

                Ok(Expr::Call(
                    box Call { callee, arguments, span: self.span(start) }))
            }
            _ => {
                return Err(self.unexpected());
            }
        }
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

                // partial application of operators
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
          | TokenKind::Thread => OperatorKind::Thread,
          | TokenKind::Range  => OperatorKind::Range,
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

        while !self.done()
           && !self.match_token(close) {

            result.push(f(self)?);

            if !self.maybe_eat(TokenKind::Comma) {
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

    fn parse(source: &str) -> Result<Expr> {
        let source = Source {
            path: PathBuf::from("test"), content: source.into()
        };
        Parser::new(&source).parse_expr()
    }
}
