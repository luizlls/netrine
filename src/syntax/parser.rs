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
                    TokenKind::Equals => self.parse_define(),
                    TokenKind::LParen => self.parse_function(),
                    _ => Err(self.unexpected())
                }
            }
            _ => Err(self.unexpected())
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::Match => self.parse_match(),
            TokenKind::Lower => {
                match self.peek.kind {
                    TokenKind::Equals => self.parse_define(),
                    TokenKind::Arrow  => self.parse_lambda(),
                    _ => {
                        self.parse_binary_expr(0)
                    }
                }
            }
            _ => self.parse_binary_expr(0)
        }
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
        let span  = self.eat(TokenKind::Lower)?;
        let value = self.source.content[span.range()].into();
        Ok(Name { value, span })
    }

    fn parse_lower(&mut self) -> Result<Expr> {
        Ok(Expr::Name(self.parse_name()?))
    }

    fn parse_upper(&mut self) -> Result<Expr> {
        let span  = self.eat(TokenKind::Upper)?;
        let value = self.source.content[span.range()].to_string();
        
        match &value[..] {
            "True" => {
                return Ok(Expr::True(span));
            }
            "False" => {
                return Ok(Expr::False(span));
            }
            _ => ()
        }

        let name = Name { value, span };

        let arguments = if self.match_token(TokenKind::LParen)
                     && self.match_lines() {
            self.parse_sequence_of(
                TokenKind::LParen,
                TokenKind::RParen,
                Self::parse_argument)?
        } else if self.match_token(TokenKind::LBrace)
               && self.match_lines() {
            let value = self.parse_brackets()?;
            let span  = value.span();
            vec![
                Argument { value, name: None, span }
            ]
        } else {
            vec![]
        };

        let span = Span::combine(span, self.last_span());

        Ok(Expr::Variant(
            box Variant { name, arguments, span }))
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

    fn parse_parens(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let values = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_expr)?;

        let span = Span::combine(start, self.last_span());

        if self.match_token(TokenKind::Arrow)
        && self.match_lines() {
            return self.into_lambda(values, span);
        }

        if values.len() == 1 {
            Ok(values.into_iter().next().unwrap())
        } else {
            Ok(Expr::Tuple(
                box Tuple { values, span }))
        }
    }

    fn parse_brackets(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let values = self.parse_sequence_of(
            TokenKind::LBracket,
            TokenKind::RBracket,
            Self::parse_expr)?;

        let span = Span::combine(start, self.last_span());

        Ok(Expr::List(
            box List { values, span }))
    }

    fn parse_braces(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let properties = self.parse_sequence_of(
            TokenKind::LBrace,
            TokenKind::RBrace,
            Self::parse_property)?;

        let span = Span::combine(start, self.last_span());

        Ok(Expr::Record(
            box Record { properties, span }))
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

    fn parse_define(&mut self) -> Result<Expr> {

        let name = self.parse_name()?;
        self.eat(TokenKind::Equals)?;
        let value = self.parse_expr()?;

        let span = Span::combine(name.span, value.span());

        Ok(Expr::Def(
            box Def { name, value, span }))
    }

    fn parse_function(&mut self) -> Result<Expr> {

        let name = self.parse_name()?;

        let parameters = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_parameter)?;

        self.eat(TokenKind::Equals)?;

        let value = self.parse_expr()?;
        
        let span = Span::combine(name.span, value.span());

        Ok(Expr::Fn(
            box Fn { name, parameters, value, span }))
    }

    fn parse_parameter(&mut self) -> Result<Parameter> {
        let mut vararg = false;
        let mut kwargs = false;

        if self.match_token(TokenKind::Mul) {
            self.bump();
            
            if self.match_token(TokenKind::Mul) {
                self.bump();
                kwargs = true;
            } else {
                vararg = true;
            }
        }

        let patt = self.parse_term()?;
        
        let span = Span::combine(patt.span(), self.last_span());
        
        Ok(Parameter { patt, vararg, kwargs, span })
    }

    fn parse_lambda(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let name = self.parse_name()?;
        self.eat(TokenKind::Arrow)?;
        let value = self.parse_expr()?;
        
        let patt = Expr::Name(name);

        let parameters = vec![
            Parameter { patt, vararg: false, kwargs: false, span: start }
        ];

        let span = Span::combine(start, value.span());

        Ok(Expr::Lambda(
            box Lambda { parameters, value, span }))
    }

    fn into_lambda(&mut self, values: Vec<Expr>, start: Span) -> Result<Expr> {
        let parameters = values
            .into_iter()
            .map(|value| {
                let span = value.span();
                
                if value.is_pattern() {
                    Ok(Parameter { patt: value, vararg: false, kwargs: false, span })
                } else {
                    Err(NetrineError::error(
                        span,
                        "is not a valid pattern for a lambda argument".into()))
                }
            })
            .collect::<Result<Vec<_>>>()?;

        self.eat(TokenKind::Arrow)?;

        let value = self.parse_expr()?;
        
        let span = Span::combine(start, value.span());
        
        Ok(Expr::Lambda(
            box Lambda { parameters, value, span }))
    }

    fn parse_match(&mut self) -> Result<Expr> {
        let start = self.token.span;

        self.eat(TokenKind::Match)?;

        let pred = self.parse_term()?;

        let cases = self.parse_sequence_of(
            TokenKind::LBrace,
            TokenKind::RBrace, 
            |this| {
                let cond = this.parse_expr()?;
                this.eat(TokenKind::Arrow)?;
                let then = this.parse_expr()?;
                Ok((cond, then))
            })?;

        let span = Span::combine(start, self.last_span());

        Ok(Expr::Match(
            box Match { pred, cases, span, }))
    }

    fn parse_initial_expr(&mut self) -> Result<Expr> {
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
        let arguments = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_argument)?;

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

    fn parse_dot(&mut self, source: Expr) -> Result<Expr> {
        self.eat(TokenKind::Dot)?;

        let value = self.parse_term()?;
        let span  = Span::combine(source.span(), self.last_span());

        Ok(Expr::Get(
            box Get { source, value, span }))
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        if !self.token.is_operator() {
            return self.parse_initial_expr();
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

                let right = self.parse_binary_expr(precedence + 1)?;
                let left  = expr;

                let span = Span::combine(left.span(), right.span());

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
          | TokenKind::Is    => OperatorKind::Is,
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

    #[test]
    fn parse_basic_name() {
        let result = parse("netrine").unwrap();
        match result {
            Expr::Name(name) => {
                assert_eq!(name.value, "netrine".to_string());
            }
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_basic_fn() {
        let result = parse("hello(name) = name").unwrap();
        match result {
            Expr::Fn(box Fn {
                name, parameters, value: Expr::Name(value), ..
            }) => {
                assert_eq!(name.value, "hello".to_string());
                assert_eq!(value.value, "name".to_string());
                assert_eq!(parameters.len(), 1);
                match &parameters[0] {
                    Parameter { patt: Expr::Name(name), .. } => {
                        assert_eq!(name.value, "name".to_string());
                    }
                    _ => assert!(false)
                }
            }
            _ => assert!(false)
        }
    }

    #[test]
    fn parse_basic_def() {
        let result = parse("num = 100").unwrap();
        match result {
            Expr::Def(box Def {
                name, value: Expr::Number(number), ..
            }) => {
                assert_eq!(name.value, "num".to_string());
                assert_eq!(number.value, "100".to_string());
            }
            _ => assert!(false)
        }
    }
}
