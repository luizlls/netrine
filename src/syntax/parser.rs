use crate::error::{error, Error, Result};
use crate::span::Span;

use super::token::{
    Token,
    TokenKind::{self, *},
};
use super::node::*;

#[derive(Debug, Clone)]
pub struct Parser<'p> {
    source: &'p str,
    tokens: Vec<Token>,
    token: Token,
    index: usize,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str, tokens: Vec<Token>) -> Parser<'p> {
        Parser {
            source,
            tokens,
            token: Token::default(),
            index: 0,
        }.init()
    }

    fn init(mut self) -> Parser<'p> {
        self.token = self.tokens.first().copied().unwrap_or_default();
        self
    }

    fn bump(&mut self) {
        self.index += 1;
        self.token = self.nth(self.index);
        if self.token.is(NewLine) && (self.prev().non_terminal() || self.peek().non_terminal()) {
            self.index += 1;
            self.token = self.tokens[self.index];
        }
    }

    fn nth(&self, idx: usize) -> Token {
        if idx < self.tokens.len() {
            self.tokens[idx]
        } else {
            Token::default()
        }
    }

    fn prev(&self) -> Token {
        self.nth(self.index.saturating_sub(1))
    }

    fn peek(&self) -> Token {
        self.nth(self.index + 1)
    }

    fn kind(&self) -> TokenKind {
        self.token.kind
    }

    fn span(&self) -> Span {
        self.token.span
    }

    fn end(&self) -> Span {
        self.prev().span
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn slice(&self, span: Span) -> &str {
        &self.source[span.range()]
    }
}

pub fn parse(source: &str, tokens: Vec<Token>) -> Result<Vec<Node>> {
    let mut parser = Parser::new(source, tokens);
    many(&mut parser, EOF, top_level)
}

fn top_level(p: &mut Parser) -> Result<Node> {
    let value = expr(p)?;

    match p.kind() {
        Equals => define(p, value),
        Colon => typedef(p, value),
        _ => Ok(value)
    }
}

fn define(p: &mut Parser, lvalue: Node) -> Result<Node> {
    token(p, Equals)?;
    let rvalue = expr(p)?;

    if let NodeKind::Apply(
        box Apply {
            callable: Node {
                kind: NodeKind::Identifier(ident),
                span,
            },
            arguments: parameters,
            ..
        }
    ) = lvalue.kind {
        let name = Name::new(ident.value, span);

        for parameter in &parameters {
            if !parameter.pattern_like() {
                error!("not a valid pattern", parameter.span);
            }
        }

        Ok(Node::function(name, parameters, rvalue))
    }
    else {
        if !lvalue.pattern_like() {
            error!("not a valid pattern", lvalue.span);
        }

        Ok(Node::define(lvalue, rvalue))
    }
}

fn typedef(_p: &mut Parser, _lvalue: Node) -> Result<Node> {
    todo!()
}

fn expr(p: &mut Parser) -> Result<Node> {
    binary(p, 0 as Precedence)
}

fn atom(p: &mut Parser) -> Result<Node> {
    match p.kind() {
        Identifier => identifier(p),
        Underscore => underscore(p),
        String => string(p),
        Number => number(p),
        Integer => integer(p),
        LParen => parens(p),
        LBrace => braces(p),
        LBracket => brackets(p),
        _ => {
            error!(
                match p.kind() {
                    UnexpectedCharacter => "unexpected character",
                    UnterminatedString => "unterminated string",
                    _ => "expected a valid expression",
                },
                p.span()
            );
        }
    }
}

fn literal(p: &mut Parser, kind: TokenKind, ctor: fn(Literal) -> NodeKind) -> Result<Node> {
    let span = token(p, kind)?;
    let value = p.slice(span).to_string();

    Ok(Node::literal(value, span, ctor))
}

fn identifier(p: &mut Parser) -> Result<Node> {
    literal(p, Identifier, NodeKind::Identifier)
}

fn underscore(p: &mut Parser) -> Result<Node> {
    literal(p, Underscore, NodeKind::Underscore)
}

fn number(p: &mut Parser) -> Result<Node> {
    literal(p, Number, NodeKind::Number)
}

fn integer(p: &mut Parser) -> Result<Node> {
    literal(p, Integer, NodeKind::Integer)
}

fn string(p: &mut Parser) -> Result<Node> {
    literal(p, String, NodeKind::String)
}

fn parens(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LParen)?;
    let items = comma(p, RParen, expr)?;
    token(p, RParen)?;

    let span = start.to(p.end());

    if items.len() == 1 {
        Ok(Node::group(items, span))
    } else {
        Ok(Node::tuple(items, span))
    }
}

fn brackets(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LBracket)?;
    let items = comma(p, RBracket, expr)?;
    token(p, RBracket)?;

    let span = start.to(p.end());

    Ok(Node::array(items, span))
}

fn braces(p: &mut Parser) -> Result<Node> {
    block(p)
}

fn block(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    token(p, LBrace)?;
    let nodes = many(p, RBrace, top_level)?;
    token(p, RBrace)?;

    let span = start.to(p.end());

    Ok(Node::block(nodes, span))
}

fn basic(p: &mut Parser) -> Result<Node> {
    let start = p.span();

    let mut node = atom(p)?;
    loop {
        node = match p.kind() {
            LParen => {
                let callable = node;

                token(p, LParen)?;
                let arguments = comma(p, RParen, expr)?;
                token(p, RParen)?;

                let span = start.to(p.end());

                Node::apply(callable, arguments, span)
            }
            Dot => {
                let owner = node;
                token(p, Dot)?;
                let field = atom(p)?;

                Node::get(owner, field)
            }
            _ => break,
        }
    }

    Ok(node)
}

fn unary(p: &mut Parser) -> Result<Node> {
    if let Some(operator) = operator(p, true) && operator.is_unary() {
        p.bump(); // operator
        let expr = unary(p)?;

        Ok(Node::unary(operator, expr))
    } else {
        basic(p)
    }
}

fn binary(p: &mut Parser, precedence: Precedence) -> Result<Node> {
    let mut expr = unary(p)?;

    while let Some(operator) = operator(p, false) && operator.precedence() >= precedence {
        p.bump(); // operator

        let precedence = match operator.associativity() {
            Associativity::None
          | Associativity::Left => operator.precedence() + 1,
            Associativity::Right => operator.precedence(),
        };

        let rexpr = binary(p, precedence)?;
        let lexpr = expr;

        expr = Node::binary(operator, lexpr, rexpr);
    }

    Ok(expr)
}

fn operator(p: &mut Parser, unary: bool) -> Option<Operator> {
    let span = p.span();

    let kind = if unary {
        match p.kind() {
            Plus  => OperatorKind::Pos,
            Minus => OperatorKind::Neg,
            Not   => OperatorKind::Not,
            _ => return None
        }
    } else {
        match p.kind() {
           Plus  => OperatorKind::Add,
           Minus => OperatorKind::Sub,
           Star  => OperatorKind::Mul,
           Slash => OperatorKind::Div,
           Caret => OperatorKind::Exp,
           Mod   => OperatorKind::Mod,
           And   => OperatorKind::And,
           Or    => OperatorKind::Or,
           Is    => OperatorKind::Is,
           EqEq  => OperatorKind::Eq,
           NoEq  => OperatorKind::Ne,
           Lt    => OperatorKind::Lt,
           LtEq  => OperatorKind::Le,
           Gt    => OperatorKind::Gt,
           GtEq  => OperatorKind::Ge,
           Dots  => OperatorKind::Range,
           _ => return None,
       }
    };

    Some(Operator::new(kind, span))
}

fn newline(p: &mut Parser) {
    maybe(p, NewLine);
}

fn endline(p: &mut Parser) -> Result<()> {
    if p.at(NewLine) || p.at(EOF) {
        p.bump();
    } else {
        error!(unexpected(&[NewLine]), p.span());
    }
    Ok(())
}

fn token(p: &mut Parser, kind: TokenKind) -> Result<Span> {
    let span = p.span();
    if p.at(kind) {
        p.bump();
        Ok(span)
    } else {
        error!(unexpected(&[kind]), span);
    }
}

fn maybe(p: &mut Parser, kind: TokenKind) -> bool {
    if p.at(kind) {
        p.bump();
        true
    } else {
        false
    }
}

fn comma<F, T>(p: &mut Parser, until: TokenKind, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut res = vec![];

    newline(p);
    while !p.at(until) {
        res.push(parse(p)?);
        newline(p);
        if !maybe(p, Comma) {
            break;
        }
        newline(p);
    }

    Ok(res)
}

fn many<F, T>(p: &mut Parser, until: TokenKind, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Parser) -> Result<T>,
{
    let mut res = vec![];

    newline(p);
    while !p.at(until) {
        res.push(parse(p)?);
        endline(p)?;
    }

    Ok(res)
}

fn unexpected(expected: &[TokenKind]) -> std::string::String {
    let expected = expected.iter().map(|it| format!("`{it}`")).collect::<Vec<_>>().join(", ");
    format!("expected {expected}")
}

#[cfg(test)]
mod tests {
    use crate::syntax;

    use super::*;

    macro_rules! assert_node {
        ($source:expr, $node:expr) => {{
            let node = syntax::parse($source).unwrap();
            assert_eq!(node, vec![$node])
        }};
    }

    #[test]
    fn basic_function() {
        assert_node!(
            "f(x) = x + 1",
            Node {
                kind: NodeKind::Function(
                    Function {
                        name: Name {
                            value: "f".to_string(),
                            span: Span {
                                lo: 0,
                                hi: 1,
                            },
                        },
                        parameters: vec![
                            Node {
                                kind: NodeKind::Identifier(
                                    Literal {
                                        value: "x".to_string(),
                                    },
                                ),
                                span: Span {
                                    lo: 2,
                                    hi: 3,
                                },
                            }
                        ],
                        value: Node {
                            kind: NodeKind::Binary(
                                Binary {
                                    operator: Operator {
                                        kind: OperatorKind::Add,
                                        span: Span {
                                            lo: 9,
                                            hi: 10,
                                        },
                                    },
                                    lexpr: Node {
                                        kind: NodeKind::Identifier(
                                            Literal {
                                                value: "x".to_string(),
                                            },
                                        ),
                                        span: Span {
                                            lo: 7,
                                            hi: 8,
                                        },
                                    },
                                    rexpr: Node {
                                        kind: NodeKind::Integer(
                                            Literal {
                                                value: "1".to_string(),
                                            },
                                        ),
                                        span: Span {
                                            lo: 11,
                                            hi: 12,
                                        },
                                    },
                                }.into(),
                            ),
                            span: Span {
                                lo: 7,
                                hi: 12,
                            }
                        }
                    }.into()
                ),
                span: Span {
                    lo: 0,
                    hi: 12,
                }
            }
        )
    }

    #[test]
    fn multiline_function() {
        assert_node!(
            "f(x) = {
                y = x * x
                z = x ^ x
                y + z
            }",
            Node {
                kind: NodeKind::Function(
                    Function {
                        name: Name {
                            value: "f".to_string(),
                            span: Span {
                                lo: 0,
                                hi: 1,
                            },
                        },
                        parameters: vec![
                            Node {
                                kind: NodeKind::Identifier(
                                    Literal {
                                        value: "x".to_string(),
                                    },
                                ),
                                span: Span {
                                    lo: 2,
                                    hi: 3,
                                },
                            }
                        ],
                        value: Node {
                            kind: NodeKind::Block(
                                Block {
                                    nodes: vec![
                                        Node {
                                            kind: NodeKind::Def(
                                                Def {
                                                    lvalue: Node {
                                                        kind: NodeKind::Identifier(
                                                            Literal {
                                                                value: "y".to_string(),
                                                            },
                                                        ),
                                                        span: Span {
                                                            lo: 17,
                                                            hi: 18,
                                                        },
                                                    },
                                                    rvalue: Node {
                                                        kind: NodeKind::Binary(
                                                            Binary {
                                                                operator: Operator {
                                                                    kind: OperatorKind::Mul,
                                                                    span: Span {
                                                                        lo: 21,
                                                                        hi: 22,
                                                                    },
                                                                },
                                                                lexpr: Node {
                                                                    kind: NodeKind::Identifier(
                                                                        Literal {
                                                                            value: "x".to_string(),
                                                                        },
                                                                    ),
                                                                    span: Span {
                                                                        lo: 19,
                                                                        hi: 20,
                                                                    },
                                                                },
                                                                rexpr: Node {
                                                                    kind: NodeKind::Identifier(
                                                                        Literal {
                                                                            value: "x".to_string(),
                                                                        },
                                                                    ),
                                                                    span: Span {
                                                                        lo: 23,
                                                                        hi: 24,
                                                                    },
                                                                },
                                                            }.into(),
                                                        ),
                                                        span: Span {
                                                            lo: 19,
                                                            hi: 24,
                                                        },
                                                    },
                                                }.into(),
                                            ),
                                            span: Span {
                                                lo: 17,
                                                hi: 24,
                                            },
                                        },
                                        Node {
                                            kind: NodeKind::Def(
                                                Def {
                                                    lvalue: Node {
                                                        kind: NodeKind::Identifier(
                                                            Literal {
                                                                value: "z".to_string(),
                                                            },
                                                        ),
                                                        span: Span {
                                                            lo: 29,
                                                            hi: 30,
                                                        },
                                                    },
                                                    rvalue: Node {
                                                        kind: NodeKind::Binary(
                                                            Binary {
                                                                operator: Operator {
                                                                    kind: OperatorKind::Exp,
                                                                    span: Span {
                                                                        lo: 33,
                                                                        hi: 34,
                                                                    },
                                                                },
                                                                lexpr: Node {
                                                                    kind: NodeKind::Identifier(
                                                                        Literal {
                                                                            value: "x".to_string(),
                                                                        },
                                                                    ),
                                                                    span: Span {
                                                                        lo: 31,
                                                                        hi: 32,
                                                                    },
                                                                },
                                                                rexpr: Node {
                                                                    kind: NodeKind::Identifier(
                                                                        Literal {
                                                                            value: "x".to_string(),
                                                                        },
                                                                    ),
                                                                    span: Span {
                                                                        lo: 35,
                                                                        hi: 36,
                                                                    },
                                                                },
                                                            }.into(),
                                                        ),
                                                        span: Span {
                                                            lo: 31,
                                                            hi: 36,
                                                        },
                                                    },
                                                }.into(),
                                            ),
                                            span: Span {
                                                lo: 29,
                                                hi: 36,
                                            },
                                        },
                                        Node {
                                            kind: NodeKind::Binary(
                                                Binary {
                                                    operator: Operator {
                                                        kind: OperatorKind::Add,
                                                        span: Span {
                                                            lo: 41,
                                                            hi: 42,
                                                        },
                                                    },
                                                    lexpr: Node {
                                                        kind: NodeKind::Identifier(
                                                            Literal {
                                                                value: "y".to_string(),
                                                            },
                                                        ),
                                                        span: Span {
                                                            lo: 39,
                                                            hi: 40,
                                                        },
                                                    },
                                                    rexpr: Node {
                                                        kind: NodeKind::Identifier(
                                                            Literal {
                                                                value: "z".to_string(),
                                                            },
                                                        ),
                                                        span: Span {
                                                            lo: 43,
                                                            hi: 44,
                                                        },
                                                    },
                                                }.into(),
                                            ),
                                            span: Span {
                                                lo: 39,
                                                hi: 44,
                                            },
                                        },
                                    ],
                                }.into(),
                            ),
                            span: Span {
                                lo: 7,
                                hi: 24,
                            },
                        },
                    }.into(),
                ),
                span: Span {
                    lo: 0,
                    hi: 24,
                },
            }
        )
    }

    #[test]
    fn basic_definition() {
        assert_node!(
            "x = 1",
            Node {
                kind: NodeKind::Def(
                    Def {
                        lvalue: Node {
                            kind: NodeKind::Identifier(
                                Literal {
                                    value: "x".to_string(),
                                },
                            ),
                            span: Span {
                                lo: 0,
                                hi: 1,
                            },
                        },
                        rvalue: Node {
                            kind: NodeKind::Integer(
                                Literal {
                                    value: "1".to_string(),
                                },
                            ),
                            span: Span {
                                lo: 4,
                                hi: 5,
                            },
                        },
                    }.into()
                ),
                span: Span {
                    lo: 0,
                    hi: 5,
                },
            }
        )
    }

    #[test]
    fn basic_apply() {
        assert_node!(
            "f(x)",
            Node {
                kind: NodeKind::Apply(
                    Apply {
                        callable: Node {
                            kind: NodeKind::Identifier(
                                Literal {
                                    value: "f".to_string(),
                                },
                            ),
                            span: Span {
                                lo: 0,
                                hi: 1,
                            },
                        },
                        arguments: vec![
                            Node {
                                kind: NodeKind::Identifier(
                                    Literal {
                                        value: "x".to_string(),
                                    },
                                ),
                                span: Span {
                                    lo: 2,
                                    hi: 3,
                                },
                            },
                        ],
                    }.into()
                ),
                span: Span {
                    lo: 0,
                    hi: 4,
                },
            }
        )
    }
}
