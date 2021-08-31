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
    parsing: Vec<Parsing>
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Parsing {
    TopLevel,
    Definition,
    Function,
    FunctionCall,
    If,
    Do,
}

impl<'s> Parser<'s> {
    fn new(source: &'s Source) -> Parser {
        let mut parser = Parser {
            lexer: Lexer::new(&source.content),
            prev : Token::default(),
            token: Token::default(),
            peek : Token::default(),
            source,
            parsing: vec![Parsing::TopLevel],
        };

        parser.bump(); // token
        parser.bump(); // peek

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
        match self.token.kind {
            TokenKind::If => self.parse_if(),
            TokenKind::Do => self.parse_do(),
            TokenKind::Lower if self.top_level() => {
                match self.peek.kind {
                    TokenKind::Equals => {
                        self.parse_def()
                    }
                    TokenKind::LParen => {
                        self.parse_fn()
                    }
                    _ => {
                        Err(self.handle_unexpected())
                    }
                }
            }
            _ => self.parse_binary_expr(0)
        }
    }

    fn parse_term(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Lower => {
                Ok(Expr::Name(self.parse_name()?))
            }
            TokenKind::Number => self.parse_number(),
            TokenKind::String => self.parse_string(),
            _ => {
                Err(self.handle_unexpected())
            }
        }
    }

    fn parse_name(&mut self) -> Result<Name> {
        let span  = self.eat(TokenKind::Lower)?;
        let value = self.source.content[span.range()].into();
        Ok(Name { value, span })
    }

    fn parse_number(&mut self) -> Result<Expr> {
        let span  = self.eat(TokenKind::Number)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::Number(Literal { value, span }))
    }

    fn parse_string(&mut self) -> Result<Expr> {
        let span  = self.eat(TokenKind::String)?;
        let value = self.source.content[span.range()].into();
        Ok(Expr::String(Literal { value, span }))
    }

    fn parse_def(&mut self) -> Result<Expr> {
        self.start(Parsing::Definition);

        let name = self.parse_name()?;
        self.eat(TokenKind::Equals)?;
        let value = self.parse_expr()?;

        let span = Span::combine(name.span, value.span());

        self.finish(Parsing::Definition);

        Ok(Expr::Def(
            box Def { name, value, span }))
    }

    fn parse_fn(&mut self) -> Result<Expr> {
        self.start(Parsing::Function);

        let name = self.parse_name()?;
        let parameters = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_parameter)?;
        self.eat(TokenKind::Equals)?;
        let value = self.parse_expr()?;
        
        let span = Span::combine(name.span, value.span());

        self.finish(Parsing::Function);

        Ok(Expr::Fn(
            box Fn { name, parameters, value, span }))
    }

    fn parse_parameter(&mut self) -> Result<Parameter> {
        let name = self.parse_name()?;

        let value = if self.match_lines()
                    && self.match_token(TokenKind::Equals) {
            self.eat(TokenKind::Equals)?;
            
            Some(self.parse_expr()?)
        } else {
            None
        };
        
        let span = Span::combine(name.span, self.last_span());
        
        Ok(Parameter { name, value, span })
    }

    fn parse_if(&mut self) -> Result<Expr> {
        self.start(Parsing::If);

        let start = self.token.span;

        self.eat(TokenKind::If)?;
        let predicate = self.parse_expr()?;

        self.eat(TokenKind::Then)?;
        let value_then = self.parse_expr()?;

        self.eat(TokenKind::Else)?;
        let value_else = self.parse_expr()?;

        let span = Span::combine(start, self.last_span());

        self.finish(Parsing::If);

        Ok(Expr::If(
            box If { predicate, value_then, value_else, span, }))
    }

    fn parse_do(&mut self) -> Result<Expr> {
        self.start(Parsing::Do);

        let start = self.token.span;

        let mut items = vec![];
        
        self.eat(TokenKind::Do)?;

        while !self.match_token(TokenKind::End) {
            items.push(self.parse_expr()?);
        }

        self.eat(TokenKind::End)?;

        let span = Span::combine(start, self.last_span());

        self.finish(Parsing::Do);

        Ok(Expr::Block(
            box Block { items, span }))
    }

    fn parse_function_call(&mut self) -> Result<Expr> {
        let callee = self.parse_term()?;

        if !self.match_lines()
        || !self.match_token(TokenKind::LParen) {
           return Ok(callee);
        }

        self.start(Parsing::FunctionCall);

        let arguments = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_argument)?;

        self.finish(Parsing::FunctionCall);

        let span = Span::combine(callee.span(), self.last_span());

        Ok(Expr::Call(
            box Call { callee, arguments, span }))
    }

    fn parse_argument(&mut self) -> Result<Argument> {
        let start = self.token.span;

        let mut name = if self.match_token(TokenKind::Lower)
                        && self.match_peek(TokenKind::Equals) {
            Some(self.parse_name()?)
        } else {
            None
        };
        
        let value = self.parse_expr()?;
        
        if let Expr::Fn(box Fn { name: function_name, .. }) = &value {
            if name.is_none() {
                name = Some(function_name.clone());
            }
        }
        
        let span = Span::combine(start, self.last_span());
        
        Ok(Argument { name, value, span })
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        if !self.token.is_operator() {
            return self.parse_function_call();
        }

        let operator = self.parse_operator()?;

        if self.prev.is_delimiter()
        && self.peek.is_delimiter() {
            let span = operator.span;
            return Ok(Expr::Partial(
                box Partial { operator, left: None, right: None, span, }))
        }

        self.bump();

        if !self.match_lines() {
            return Err(NetrineError::error(
                self.token.span,
                "Unary or partial operators must be in the same line as the operand".into()));
        }

        let right = self.parse_function_call()?;

        let span = Span::combine(operator.span, right.span());

        if operator.is_unary() {
            Ok(Expr::Unary(
                box Unary { operator, right, span }))
        } else {
            Ok(Expr::Partial(
                box Partial { operator, left: None, right: Some(right), span, }))
        }
    }

    fn parse_binary_expr(&mut self, minimum: u8) -> Result<Expr> {
        let mut expr = self.parse_unary_expr()?;

        while self.token.is_operator() {
            if let Some(precedence) = self.token.precedence() {

                if precedence < minimum {
                    break;
                }

                let operator = self.parse_operator()?;
                self.bump();

                // partial application of operators
                if self.token.is_delimiter() {
                    let span = Span::combine(expr.span(), operator.span);

                    return Ok(Expr::Partial(
                        box Partial { operator, left: Some(expr), right: None, span }))
                }

                let right = self.parse_binary_expr(precedence + 1)?;
                let left  = expr;

                let span = Span::combine(left.span(), right.span());

                expr = Expr::Binary(box Binary { operator, left, right, span });
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
          | TokenKind::Is  => OperatorKind::Is,
          | TokenKind::Not => OperatorKind::Not,
          | TokenKind::Eq  => OperatorKind::Eq,
          | TokenKind::Ne  => OperatorKind::Ne,
          | TokenKind::Lt  => OperatorKind::Lt,
          | TokenKind::Le  => OperatorKind::Le,
          | TokenKind::Gt  => OperatorKind::Gt,
          | TokenKind::Ge  => OperatorKind::Ge,
          | TokenKind::Pipe  => OperatorKind::Pipe,
          | TokenKind::Range => OperatorKind::Range,
            _ => return Err(NetrineError::error(
                span,
                format!("`{}` is not a valid operator", self.token.kind)))
        };
        return Ok(Operator { kind, span })
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

        self.eat(open)?;

        while !self.match_token(close) {

            result.push(f(self)?);

            if !self.maybe_eat(TokenKind::Comma) {
                break;
            }
        }

        self.eat(close)?;

        Ok(result)
    }

    fn bump(&mut self) -> Span {
        let span = self.token.span;

        self.prev  = self.token;
        self.token = self.peek;
        self.peek  = if let Some(token) = self.lexer.next() {
            token
        } else {
            Token::default()
        };

        span
    }

    fn done(&self) -> bool {
        self.token.kind == TokenKind::EOS
    }

    fn eat(&mut self, expected: TokenKind) -> Result<Span> {
        let token = self.token.kind;
        let span  = self.token.span;

        if token == expected {
            self.bump();
            Ok(span)
        } else {
            Err(NetrineError::error(
                span,
                format!("Expected `{}`, but found `{}`", expected, token)))
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

    fn last_span(&self) -> Span {
        self.prev.span
    }

    fn match_token(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn match_peek(&self, kind: TokenKind) -> bool {
        self.peek.kind == kind
    }

    fn match_lines(&self) -> bool {
        self.token.span.line == self.prev.span.line
    }

    fn start(&mut self, parsing: Parsing) {
        self.parsing.push(parsing);
    }

    fn finish(&mut self, parsing: Parsing) {
        debug_assert!(
            self.match_parsing(parsing),
            "invalid 'parsing' state, expected {:?}, but found {:?}",
            parsing,
            self.parsing.last());

        self.parsing.pop();
    }

    fn match_parsing(&self, expected: Parsing) -> bool {
        self.parsing
            .last()
            .map(|it| it == &expected)
            .unwrap_or(false)
    }

    fn top_level(&self) -> bool {
        self.match_parsing(Parsing::TopLevel)
    }

    fn handle_unexpected(&mut self) -> NetrineError {
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
