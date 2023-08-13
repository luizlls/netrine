use crate::error::{Error, Result};
use crate::span::{Span, IntoSpan};

use super::lexer::{Lexer, token};
use super::node::*;
use super::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Parser<'s> {
    source: &'s str,
    lexer: Lexer<'s>,
    token: Token,
    last:  Token,
    peek:  Token,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str, lexer: Lexer<'s>) -> Parser<'s> {
        let mut parser = Parser {
            source,
            lexer,
            last: Token::default(),
            peek: Token::default(),
            token: Token::default(),
        };
        parser.bump();
        parser.bump();
        parser
    }

    fn bump(&mut self) -> Span {
        self.last  = self.token;
        self.token = self.peek;
        self.peek = token(&mut self.lexer);
        self.last.span
    }

    fn token(&self) -> TokenKind {
        self.token.kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn peek(&self) -> TokenKind {
        self.peek.kind
    }

    fn peek_span(&self) -> Span {
        self.peek.span
    }

    fn last_span(&self) -> Span {
        self.last.span
    }

    fn slice(&self, span: Span) -> &str {
        &self.source[span.range()]
    }

    fn fail(&mut self, message: String, span: Span) -> Error {
        Error::new(message, span)
    }
}

pub fn parse(p: &mut Parser) -> Result<Vec<Node>> {
    sequence_while(p, None, |p| !p.token().is(TokenKind::EOF), node)
}

fn node(p: &mut Parser) -> Result<Node> {
    let lvalue = expr(p)?;
    match p.token() {
        TokenKind::Equals => r#let(p, lvalue),
        TokenKind::Walrus => r#mut(p, lvalue),
        _ => Ok(lvalue)
    }
}

fn r#let(p: &mut Parser, lvalue: Node) -> Result<Node> {
    expect(p, TokenKind::Equals)?;
    let lvalue = lvalue;
    let rvalue = expr(p)?;
    let constraints = if maybe(p, TokenKind::Where) {
        if p.token().is(TokenKind::LBrace) {
            expect(p, TokenKind::LBrace)?;
            let constraints = sequence_while(p, Some(TokenKind::Comma), |p| p.token().is(TokenKind::LBrace), constraint)?;
            expect(p, TokenKind::RBrace)?;
            constraints
        } else {
            vec![constraint(p)?]
        }
    } else {
        vec![]
    };
    Ok(Node::Let(
        Let {
            span: Span::from(&lvalue, &rvalue),
            lvalue,
            rvalue,
            constraints,
        }.into()
    ))
}

fn r#mut(p: &mut Parser, lvalue: Node) -> Result<Node> {
    if !lvalue.is_identifier() {
        return Err(p.fail("only an identifier can be a mutable value".to_string(), lvalue.span()))
    };
    expect(p, TokenKind::Walrus)?;
    let lvalue = lvalue;
    let rvalue = expr(p)?;
    Ok(Node::Mut(
        Mut {
            span: Span::from(&lvalue, &rvalue),
            lvalue,
            rvalue,
        }.into()
    ))
}

fn expr(p: &mut Parser) -> Result<Node> {
    binary(p, 0 as Precedence)
}

fn constraint(p: &mut Parser) -> Result<Constraint> {
    Ok(match binary(p, 0 as Precedence)? {
        Node::Apply(constraint) => Constraint::Apply(constraint),
        Node::Unary(constraint) => Constraint::Unary(constraint),
        Node::Binary(constraint) => Constraint::Binary(constraint),
        invalid => {
            return Err(p.fail("invalid constraint expression".to_string(), invalid.span()))
        }
    })
}

fn atom(p: &mut Parser) -> Result<Node> {
    match p.token() {
        TokenKind::Ident  => ident(p),
        TokenKind::Number => number(p),
        TokenKind::String => string(p),
        TokenKind::LParen => parens(p),
        TokenKind::LBrace => braces(p),
        TokenKind::LBracket => brackets(p),
        _ => Err(Error::new("expected an expression".to_string(), p.span()))
    }
}

fn is_atom(p: &Parser) -> bool {
    matches!(p.token(), 
    TokenKind::Ident
  | TokenKind::Number
  | TokenKind::String
  | TokenKind::LParen
  | TokenKind::LBrace
  | TokenKind::LBracket)
}

fn literal(p: &mut Parser, kind: TokenKind) -> Result<Literal> {
    let span = expect(p, kind)?;
    let value = p.slice(span).to_string();
    Ok(Literal { value, span })
}

fn name(p: &mut Parser) -> Result<Name> {
    literal(p, TokenKind::Ident)
}

fn ident(p: &mut Parser) -> Result<Node> {
    literal(p, TokenKind::Ident).map(Node::Name)
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, TokenKind::Number).map(Node::Number)
}

fn string(p: &mut Parser) -> Result<Node> {
    literal(p, TokenKind::String).map(Node::String)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LParen)?;
    let nodes = sequence_while(p, Some(TokenKind::Comma), |p| !p.token().is(TokenKind::RParen), expr)?;
    let finish = expect(p, TokenKind::RParen)?;

    let span = Span::from(&start, &finish);

    Ok(match nodes.len() {
        0 => Node::Empty(span),
        1 => Node::Group(
                Group {
                node: nodes.into_iter().next().unwrap(),
                span,
            }.into()
        ),
        _ => Node::Tuple(
            Tuple {
                items: nodes,
                span,
            }
        ),
    })
}

fn braces(p: &mut Parser) -> Result<Node> {
    todo!()
}

fn brackets(p: &mut Parser) -> Result<Node> {
    let start = expect(p, TokenKind::LBracket)?;
    let items = sequence_while(p, Some(TokenKind::Comma), |p| !p.token().is(TokenKind::RBracket), expr)?;
    let finish = expect(p, TokenKind::RBracket)?;
    
    Ok(Node::List(
        List {
            items,
            span: Span::from(&start, &finish),
        })
    )
}

fn initial(p: &mut Parser) -> Result<Node> {
    let mut node = atom(p)?;
    while maybe(p, TokenKind::Dot) {
        let field = atom(p)?;
        node = Node::Get(
            Get {
                span: Span::from(&node, &field),
                node,
                field,
            }.into()
        )
    }
    Ok(node)
}

fn apply(p: &mut Parser) -> Result<Node> {
    let start = p.span();
    let nodes = sequence_while(p, None, is_atom, initial)?;
    let finish = p.last_span();

    Ok(Node::Apply(
        Apply {
            nodes,
            span: Span::from(&start, &finish),
        }.into()
    ))
}

fn unary(p: &mut Parser) -> Result<Node> {
    if let Some(operator) = operator(p) {
        p.bump();

        if !operator.is_unary() {
            return Err(p.fail("not an unary operator".to_string(), operator.span));
        }

        let expr = expr(p)?;

        Ok(Node::Unary(
            Unary {
                span: Span::from(&operator, &expr),
                expr,
                operator,
            }.into()
        ))
    } else {
        apply(p)
    }
}

fn binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
    let mut expr = unary(p)?;

    while let Some(operator) = operator(p) && operator.precedence() >= precedence {
        p.bump();

        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left  => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = binary(p, precedence)?;
        let lexpr = expr;

        expr = Node::Binary(
            Binary {
                span: Span::from(&lexpr, &rexpr),
                lexpr,
                rexpr,
                operator,
            }.into()
        );
    }

    Ok(expr)
}

fn is_unary_operator(p: &Parser) -> bool {
    match p.token() {
        TokenKind::Plus
      | TokenKind::Minus => {
            match p.peek() {
                TokenKind::Ident
              | TokenKind::Number
              | TokenKind::LParen => p.span().hi() == p.peek_span().lo(),
              _ => false
            }
        }
        _ => false
    }
}

fn operator(p: &mut Parser) -> Option<Operator> {
    let span = p.span();

    let kind = match p.token() {
        TokenKind::Plus  if is_unary_operator(p) => OperatorKind::Pos,
        TokenKind::Minus if is_unary_operator(p) => OperatorKind::Neg,
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
        TokenKind::DotDot => OperatorKind::Range,
        _ => {
            return None;
        }
    };

    Some(Operator { kind, span })
}

fn sequence_while<T, F, P>(
    p: &mut Parser,
    separator: Option<TokenKind>,
    predicate: P,
    mut parse: F,
) -> Result<Vec<T>>
where
    P: Fn(&Parser) -> bool, F: FnMut(&mut Parser) -> Result<T>,
{
    let mut result = vec![];

    while !predicate(p) {
        result.push(parse(p)?);
        if let Some(separator) = separator && !maybe(p, separator) { break; }
    }

    Ok(result)
}

fn expect(p: &mut Parser, expected: TokenKind) -> Result<Span> {
    if p.token() == expected {
        Ok(p.bump())
    } else {
        Err(p.fail(format!("Expected `{}`", expected), p.span()))
    }
}

fn maybe(p: &mut Parser, expected: TokenKind) -> bool {
    if p.token() == expected {
        p.bump();
        true
    } else {
        false
    }
}