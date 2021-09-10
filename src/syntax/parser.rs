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
            expressions.push(self.parse_expr()?);
        }

        Ok(Module { expressions })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.token.kind {
            TokenKind::If => self.parse_if(),
            TokenKind::Do => self.parse_do(),
            TokenKind::Lower => {
                match self.peek.kind {
                    TokenKind::Equals => self.parse_def(),
                    TokenKind::LParen => self.parse_fn(),
                    TokenKind::Arrow  => self.parse_lambda(),
                    _ => self.parse_lower()
                }
            }
            _ => self.parse_binary_expr(0)
        }
    }

    fn start_term(&self) -> bool {
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
                Err(self.handle_unexpected())
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

        let name  = Name { value, span };

        let value = if self.start_term()
                    && self.match_lines() {
            Some(self.parse_term()?)
        } else {
            None
        };

        let span = Span::combine(span, self.last_span());

        Ok(Expr::Variant(
            box Variant { name, value, span }
        ))
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

        let (mut values, trailing) = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_expr)?;

        let span = Span::combine(start, self.last_span());

        if self.match_token(TokenKind::Arrow)
        && self.match_lines() {
            return self.into_lambda(values, span);
        }

        match values.len() {
            1 if !trailing => {
                Ok(values.pop().unwrap())
            }
            _ => {
                Ok(Expr::Tuple(
                    box Tuple { values, span }))
            }
        }
    }

    fn parse_brackets(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let values = self.parse_sequence_of(
            TokenKind::LBracket,
            TokenKind::RBracket,
            Self::parse_expr)?.0;

        let span = Span::combine(start, self.last_span());

        Ok(Expr::List(
            box List { values, span }))
    }

    fn parse_braces(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let properties = self.parse_sequence_of(
            TokenKind::LBrace,
            TokenKind::RBrace,
            Self::parse_property)?.0;

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
            TokenKind::LParen => {
                Some(self.parse_anonymous_fn()?)
            }
            _ => None
        };

        Ok((key, val))
    }

    fn parse_def(&mut self) -> Result<Expr> {

        let name = self.parse_name()?;
        self.eat(TokenKind::Equals)?;
        let value = self.parse_expr()?;

        let span = Span::combine(name.span, value.span());

        Ok(Expr::Def(
            box Def { name, value, span }))
    }

    fn parse_fn(&mut self) -> Result<Expr> {

        let name = self.parse_name()?;

        let parameters = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_parameter)?.0;

        if !self.match_token(TokenKind::Equals) {
            return self.into_call(name, parameters);
        }

        self.eat(TokenKind::Equals)?;

        let value = self.parse_expr()?;
        
        let span = Span::combine(name.span, value.span());

        Ok(Expr::Fn(
            box Fn { name, parameters, value, span }))
    }

    fn parse_parameter(&mut self) -> Result<Parameter> {
        let patt = self.parse_term()?;

        let default = if self.match_lines()
                      && self.match_token(TokenKind::Equals) {
            self.eat(TokenKind::Equals)?;
            
            Some(self.parse_expr()?)
        } else {
            None
        };
        
        let span = Span::combine(patt.span(), self.last_span());
        
        Ok(Parameter { patt, default, span })
    }

    fn parse_anonymous_fn(&mut self) -> Result<Expr> {
        let start = self.last_span();

        let parameters = self.parse_sequence_of(
            TokenKind::LParen,
            TokenKind::RParen,
            Self::parse_parameter)?.0;

        self.eat(TokenKind::Equals)?;

        let value = self.parse_expr()?;

        let span = Span::combine(start, self.last_span());

        Ok(Expr::Lambda(
            box Lambda { parameters, value, span }))
    }

    fn parse_lambda(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let name = self.parse_name()?;
        self.eat(TokenKind::Arrow)?;
        let value = self.parse_expr()?;
        
        let patt = Expr::Name(name);

        let parameters = vec![
            Parameter { patt, default: None, span: start }
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
                match value {
                    Expr::Def(box Def { name, value, .. }) => {
                        let patt = Expr::Name(name);
                        Ok(Parameter { patt, default: Some(value), span })
                    }
                    _ if value.is_pattern() => {
                        let patt = value;
                        Ok(Parameter { patt, default: None, span })
                    }
                    _ => {
                        Err(NetrineError::error(
                            span,
                            "Invalid pattern for lambda argument".into()))
                    }
                }
            })
            .collect::<Result<Vec<_>>>()?;

        self.eat(TokenKind::Arrow)?;

        let value = self.parse_expr()?;
        
        let span = Span::combine(start, value.span());
        
        Ok(Expr::Lambda(
            box Lambda { parameters, value, span }))
    }

    fn parse_if(&mut self) -> Result<Expr> {
        let start = self.token.span;

        self.eat(TokenKind::If)?;
        let predicate = self.parse_expr()?;
        self.eat(TokenKind::Then)?;
        let value_then = self.parse_expr()?;
        self.eat(TokenKind::Else)?;
        let value_else = self.parse_expr()?;

        let span = Span::combine(start, self.last_span());

        Ok(Expr::If(
            box If { predicate, value_then, value_else, span, }))
    }

    fn parse_do(&mut self) -> Result<Expr> {
        let start = self.token.span;

        let mut items = vec![];
        
        self.eat(TokenKind::Do)?;

        while !self.match_token(TokenKind::End) {
            items.push(self.parse_expr()?);
        }

        self.eat(TokenKind::End)?;

        let span = Span::combine(start, self.last_span());

        Ok(Expr::Block(
            box Block { items, span }))
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
                TokenKind::Hash => {
                    expression = self.parse_hash(expression)?;
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
            Self::parse_argument)?.0;

        let span = Span::combine(callee.span(), self.last_span());

        Ok(Expr::Call(
            box Call { callee, arguments, span }))
    }

    fn into_call(&mut self, callee: Name, parameters: Vec<Parameter>) -> Result<Expr> {
        let callee = Expr::Name(callee);

        let arguments = parameters
            .into_iter()
            .map(|parameter| {
                let Parameter { patt, default, span } = parameter;
                match patt {
                    Expr::Name(name) => {
                        if let Some(value) = default {
                            Ok(Argument { name: Some(name), value, span })
                        } else {
                            Ok(Argument { name: None,
                                          value: Expr::Name(name), span })
                        }
                    }
                    _ if default.is_none() => {
                        Ok(Argument { name: None, value: patt, span })
                    }
                    _ => {
                        Err(NetrineError::error(
                            span,
                            "Pattern arguments cannot have a default value".into()))
                    }
                }
            })
            .collect::<Result<Vec<_>>>()?;

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

        let value = Expr::Name(self.parse_name()?);
        let span = Span::combine(source.span(), self.last_span());

        Ok(Expr::Get(
            box Get { source, value, span }))
    }

    fn parse_hash(&mut self, source: Expr) -> Result<Expr> {
        self.eat(TokenKind::Hash)?;

        let value = self.parse_expr()?;
        let span = Span::combine(source.span(), self.last_span());

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

                // partial application of operators
                if self.peek.is_delimiter() {
                    let span = Span::combine(expr.span(), operator.span);
                    return Ok(Expr::Partial(
                        box Partial { operator, left: Some(expr), right: None, span }));
                }
                
                self.bump();

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
        mut f: F) -> Result<(Vec<T>, bool)>
    where
        F: FnMut(&mut Self) -> Result<T>
    {
        let mut result = vec![];

        let mut trailing = false;

        self.eat(open)?;

        while !self.match_token(close) {
            trailing = false;

            result.push(f(self)?);

            if !self.maybe_eat(TokenKind::Comma) {
                break;
            }

            trailing = true;
        }

        self.eat(close)?;

        Ok((result, trailing))
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
                    Parameter { patt: Expr::Name(name), default: None, .. } => {
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
