use super::lexer::TokenStream;
use super::token::Token;
use crate::collections::IndexVec;
use crate::error::{Error, Result};
use crate::source::Span;
use crate::syntax::{Module, Node, NodeIndex, NodeKind, Operator, Precedence, TokenIndex};

#[derive(Debug)]
struct Context<'ctx> {
    stream: TokenStream<'ctx>,
    current: (Token, Span),
    nodes: IndexVec<NodeIndex, Node>,
    sizes: IndexVec<NodeIndex, u32>,
    tokens: IndexVec<TokenIndex, Token>,
    spans: IndexVec<TokenIndex, Span>,
}

impl<'ctx> Context<'ctx> {
    fn new(stream: TokenStream<'ctx>) -> Context<'ctx> {
        Context {
            stream,
            current: Default::default(),
            nodes: IndexVec::new(),
            sizes: IndexVec::new(),
            tokens: IndexVec::new(),
            spans: IndexVec::new(),
        }
        .init()
    }

    fn init(mut self) -> Context<'ctx> {
        self.current = self.stream.token();
        self
    }

    fn bump(&mut self) {
        self.push_token(self.current.0, self.current.1);
        self.stream.bump();
        self.current = self.stream.token();
    }

    fn at(&self, kind: Token) -> bool {
        self.current.0 == kind
    }

    fn peek_is(&self, kind: Token) -> bool {
        self.stream.peek().0 == kind
    }

    fn token(&self) -> Token {
        self.current.0
    }

    fn span(&self) -> Span {
        self.current.1
    }

    fn done(&self) -> bool {
        self.stream.done()
    }

    fn token_index(&self) -> TokenIndex {
        self.tokens.index()
    }

    fn node_index(&self) -> NodeIndex {
        self.nodes.index()
    }

    fn push_token(&mut self, token: Token, span: Span) -> TokenIndex {
        let index = self.tokens.push(token);
        self.spans.insert(index, span);
        index
    }

    fn push_node(&mut self, kind: NodeKind, token: TokenIndex, size: u32) -> NodeIndex {
        let index = self.nodes.push(Node::new(kind, token));
        self.sizes.insert(index, size);
        index
    }

    fn replace_node(&mut self, index: NodeIndex, kind: NodeKind) {
        let node = self.nodes[index];
        self.nodes[index] = Node { kind, ..node };
    }

    fn size(&self, node: NodeIndex) -> u32 {
        let curr = self.node_index();
        u32::from(curr) - u32::from(node)
    }

    fn resize(&mut self, node: NodeIndex, size: u32) {
        self.sizes[node] = size;
    }

    fn finalize(self) -> Module {
        Module {
            nodes: self.nodes,
            sizes: self.sizes,
            tokens: self.tokens,
            spans: self.spans,
        }
    }
}

fn start(ctx: &mut Context, kind: NodeKind) -> NodeIndex {
    let token = ctx.token_index();
    ctx.push_node(kind, token, 0)
}

fn finish(ctx: &mut Context, start: NodeIndex, end: NodeKind) {
    let token = ctx.token_index().prev();
    let size = ctx.size(start);

    ctx.push_node(end, token, size);
    ctx.resize(start, size);
}

fn top_level(ctx: &mut Context) -> Result<()> {
    if ctx.at(Token::Let) {
        define(ctx)
    } else {
        expr(ctx)
    }
}

fn define(ctx: &mut Context) -> Result<()> {
    let init = start(ctx, NodeKind::LetInit);

    expect(ctx, Token::Let)?;

    if ctx.at(Token::Identifier) && ctx.peek_is(Token::LParen) {
        return function(ctx, init);
    }

    name(ctx)?;
    expect(ctx, Token::Equals)?;
    expr(ctx)?;

    finish(ctx, init, NodeKind::LetEnd);

    Ok(())
}

fn function(ctx: &mut Context, start: NodeIndex) -> Result<()> {
    ctx.replace_node(start, NodeKind::FnInit);

    name(ctx)?;
    params(ctx)?;
    expect(ctx, Token::Equals)?;
    expr(ctx)?;

    finish(ctx, start, NodeKind::FnEnd);

    Ok(())
}

fn params(ctx: &mut Context) -> Result<()> {
    seq(ctx, Token::LParen, Token::RParen, |ctx| {
        let init = start(ctx, NodeKind::ParameterInit);
        name(ctx)?;
        finish(ctx, init, NodeKind::ParameterEnd);
        Ok(())
    })?;

    Ok(())
}

fn expr(ctx: &mut Context) -> Result<()> {
    binary(ctx, 0 as Precedence)
}

fn atom(ctx: &mut Context) -> Result<()> {
    match ctx.token() {
        Token::Identifier => ident(ctx),
        Token::Number => number(ctx),
        Token::Integer => integer(ctx),
        Token::LParen => parens(ctx),
        _ => {
            fail(
                ctx.span(),
                match ctx.token() {
                    Token::UnexpectedCharacter => "unexpected character".into(),
                    Token::UnterminatedString => "unterminated string".into(),
                    _ => format!("unexpected {}", ctx.token()),
                },
            )
        }
    }
}

fn literal(ctx: &mut Context, kind: Token, node: NodeKind) -> Result<()> {
    let token = ctx.token_index();
    expect(ctx, kind)?;
    ctx.push_node(node, token, 0);
    Ok(())
}

fn name(ctx: &mut Context) -> Result<()> {
    literal(ctx, Token::Identifier, NodeKind::Name)
}

fn ident(ctx: &mut Context) -> Result<()> {
    literal(ctx, Token::Identifier, NodeKind::Identifier)
}

fn number(ctx: &mut Context) -> Result<()> {
    literal(ctx, Token::Number, NodeKind::Number)
}

fn integer(ctx: &mut Context) -> Result<()> {
    literal(ctx, Token::Integer, NodeKind::Integer)
}

fn parens(ctx: &mut Context) -> Result<()> {
    let init = start(ctx, NodeKind::GroupInit);

    expect(ctx, Token::LParen)?;
    expr(ctx)?;
    expect(ctx, Token::RParen)?;

    finish(ctx, init, NodeKind::GroupEnd);

    Ok(())
}

fn apply(ctx: &mut Context) -> Result<()> {
    atom(ctx)?;
    while ctx.at(Token::LParen) {
        let init = start(ctx, NodeKind::ApplyInit);
        seq(ctx, Token::LParen, Token::RParen, expr)?;
        finish(ctx, init, NodeKind::ApplyEnd);
    }
    Ok(())
}

fn unary(ctx: &mut Context) -> Result<()> {
    if let Some((operator, token)) = operator(ctx, 0 as Precedence, true) {
        let start = ctx.node_index();
        unary(ctx)?;
        ctx.push_node(NodeKind::Unary(operator), token, ctx.size(start));
    } else {
        apply(ctx)?;
    }

    Ok(())
}

fn binary(ctx: &mut Context, precedence: Precedence) -> Result<()> {
    let start = ctx.node_index();

    unary(ctx)?;

    while let Some((operator, token)) = operator(ctx, precedence, false) {
        newline(ctx); // accepts newlines if the line ends with an operator

        let next_precedence = if operator == Operator::Pow {
            operator.precedence()
        } else {
            operator.precedence() + 1
        };

        binary(ctx, next_precedence)?;
        ctx.push_node(NodeKind::Binary(operator), token, ctx.size(start));
    }

    Ok(())
}

fn operator(ctx: &mut Context, precedence: Precedence, unary: bool) -> Option<(Operator, TokenIndex)> {
    let operator = match ctx.token() {
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
        let token_index = ctx.token_index();
        ctx.bump();
        Some((operator, token_index))
    } else {
        None
    }
}

fn newline(ctx: &mut Context) -> bool {
    maybe(ctx, Token::EOL)
}

fn endline(ctx: &mut Context) -> Result<()> {
    if maybe(ctx, Token::EOL) || maybe(ctx, Token::Semi) || maybe(ctx, Token::EOF) {
        Ok(())
    } else {
        fail(ctx.span(), unexpected(ctx, &[Token::EOL]))
    }
}

fn expect(ctx: &mut Context, kind: Token) -> Result<()> {
    if ctx.at(kind) {
        ctx.bump();
        Ok(())
    } else {
        fail(ctx.span(), unexpected(ctx, &[kind]))
    }
}

fn maybe(ctx: &mut Context, kind: Token) -> bool {
    if ctx.at(kind) {
        ctx.bump();
        true
    } else {
        false
    }
}

fn seq<F>(ctx: &mut Context, before: Token, after: Token, mut parse: F) -> Result<usize>
where
    F: FnMut(&mut Context) -> Result<()>,
{
    let mut count = 0;

    newline(ctx);

    expect(ctx, before)?;

    while !ctx.at(after) {
        parse(ctx)?;
        count += 1;

        newline(ctx);
        if !maybe(ctx, Token::Comma) {
            break;
        }
        newline(ctx);
    }

    expect(ctx, after)?;

    Ok(count)
}

fn many<F>(ctx: &mut Context, before: Token, after: Token, mut parse: F) -> Result<usize>
where
    F: FnMut(&mut Context) -> Result<()>,
{
    let mut count = 0;

    newline(ctx);

    expect(ctx, before)?;

    while !ctx.at(after) {
        parse(ctx)?;
        count += 1;
        newline(ctx);
    }

    expect(ctx, after)?;

    Ok(count)
}

fn unexpected(ctx: &Context, expected: &[Token]) -> String {
    let expected: Vec<_> = expected.iter().map(|it| format!("`{it}`")).collect();
    format!("expected {}, found `{}`", expected.join(", "), ctx.token())
}

fn fail<T>(span: Span, message: impl Into<String>) -> Result<T> {
    Err(Error::error(span, message.into()))
}

pub fn parse<'ctx>(stream: TokenStream<'ctx>) -> Result<Module> {
    let mut context = Context::new(stream);

    newline(&mut context);

    while !context.done() {
        top_level(&mut context)?;
        endline(&mut context)?;
    }

    Ok(context.finalize())
}
