use super::lexer::TokenStream;
use super::token::{Token, TokenKind};
use crate::error::{Error, Result};
use crate::source::Span;
use crate::syntax::{
    Module, Name, Node, NodeKind, Operator, OperatorKind, Parameter, ParameterLike, Precedence, Type,
};

#[derive(Debug, Clone)]
struct Context<'ctx> {
    stream: TokenStream<'ctx>,
    token: Token,
}

impl<'ctx> Context<'ctx> {
    fn new(stream: TokenStream<'ctx>) -> Context<'ctx> {
        Context {
            stream,
            token: Token::default(),
        }
        .init()
    }

    fn init(mut self) -> Context<'ctx> {
        self.token = self.stream.token();
        self
    }

    fn bump(&mut self) {
        self.stream.bump();
        self.token = self.stream.token();
    }

    fn peek(&self) -> TokenKind {
        self.stream.peek().kind
    }

    fn span_of(&self, start: Span) -> Span {
        Span::from(start, self.stream.prev().span)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    fn done(&self) -> bool {
        self.stream.done()
    }
}

fn parse_top_level(ctx: &mut Context) -> Result<Node> {
    if ctx.at(TokenKind::Identifier) {
        match ctx.peek() {
            TokenKind::Equals | TokenKind::Colon => {
                return parse_def(ctx);
            }
            TokenKind::LParen => {
                return parse_fn(ctx);
            }
            _ => {}
        }
    }

    parse_expr(ctx)
}

fn parse_def(ctx: &mut Context) -> Result<Node> {
    let name = parse_name(ctx)?;
    let type_ = parse_optional(ctx, TokenKind::Colon, parse_type_annotation)?;
    expect(ctx, TokenKind::Equals)?;

    let value = parse_expr(ctx)?;

    Ok(Node::define(name, value, type_))
}

fn parse_fn(ctx: &mut Context) -> Result<Node> {
    let name = parse_name(ctx)?;
    expect(ctx, TokenKind::LParen)?;
    let parameters = parse_seq(ctx, TokenKind::RParen, parse_parameter_like)?;
    expect(ctx, TokenKind::RParen)?;
    let type_ = parse_optional(ctx, TokenKind::Colon, parse_type_annotation)?;

    // if there's no equal sign that means it's a function call
    if !ctx.at(TokenKind::Equals) {
        if let Some(type_) = type_ {
            return Err(fail("type annotation not valid for function call arguments", type_.span));
        }
        return into_function_apply(ctx, name, parameters);
    }

    let parameters = into_parameters(parameters)?;

    expect(ctx, TokenKind::Equals)?;
    let value = parse_expr(ctx)?;

    Ok(Node::function(name, parameters, value, type_))
}

fn parse_parameter_like(ctx: &mut Context) -> Result<ParameterLike> {
    let value = parse_expr(ctx)?;
    let type_ = parse_optional(ctx, TokenKind::Colon, parse_type_annotation)?;

    Ok(ParameterLike::new(value, type_))
}

fn into_parameters(parameter_likes: Vec<ParameterLike>) -> Result<Vec<Parameter>> {
    parameter_likes
        .into_iter()
        .map(|parameter| {
            if let NodeKind::Name(name) = parameter.value.kind {
                Ok(Parameter::new(name, parameter.type_))
            } else {
                Err(fail("not a valid pattern for function a parameter", parameter.span))
            }
        })
        .collect()
}

fn into_function_apply(ctx: &mut Context, name: Name, parameter_likes: Vec<ParameterLike>) -> Result<Node> {
    let span = ctx.span_of(name.span);
    let function = Node::name(name);
    let arguments = into_arguments(parameter_likes)?;

    Ok(Node::apply(function, arguments, span))
}

fn into_arguments(parameter_likes: Vec<ParameterLike>) -> Result<Vec<Node>> {
    parameter_likes
        .into_iter()
        .map(|parameter| {
            if let Some(type_) = parameter.type_ {
                Err(fail("type annotation not valid for function call arguments", type_.span))
            } else {
                Ok(parameter.value)
            }
        })
        .collect()
}

fn parse_expr(ctx: &mut Context) -> Result<Node> {
    binary(ctx, 0 as Precedence)
}

fn parse_atom(ctx: &mut Context) -> Result<Node> {
    match ctx.token.kind {
        TokenKind::Identifier => parse_identifier(ctx),
        TokenKind::Number => parse_number(ctx),
        TokenKind::Integer => parse_integer(ctx),
        TokenKind::LParen => parse_parens(ctx),
        _ => {
            Err(fail(
                match ctx.token.kind {
                    TokenKind::UnexpectedCharacter => "unexpected character".into(),
                    TokenKind::UnterminatedString => "unterminated string".into(),
                    _ => format!("unexpected {}", ctx.token.kind),
                },
                ctx.token.span,
            ))
        }
    }
}

fn parse_name(ctx: &mut Context) -> Result<Name> {
    let span = ctx.token.span;
    expect(ctx, TokenKind::Identifier)?;

    Ok(Name { span })
}

fn parse_literal(ctx: &mut Context, kind: TokenKind, node: NodeKind) -> Result<Node> {
    let span = ctx.token.span;
    expect(ctx, kind)?;
    Ok(Node::new(node, span))
}

fn parse_identifier(ctx: &mut Context) -> Result<Node> {
    let name = parse_name(ctx)?;
    let span = name.span;
    Ok(Node::new(NodeKind::Name(name), span))
}

fn parse_number(ctx: &mut Context) -> Result<Node> {
    parse_literal(ctx, TokenKind::Number, NodeKind::Number)
}

fn parse_integer(ctx: &mut Context) -> Result<Node> {
    parse_literal(ctx, TokenKind::Integer, NodeKind::Integer)
}

fn parse_parens(ctx: &mut Context) -> Result<Node> {
    expect(ctx, TokenKind::LParen)?;
    let expr = parse_expr(ctx)?;
    expect(ctx, TokenKind::RParen)?;

    Ok(expr)
}

fn parse_apply(ctx: &mut Context) -> Result<Node> {
    let mut expr = parse_atom(ctx)?;

    while ctx.at(TokenKind::LParen) {
        expect(ctx, TokenKind::LParen)?;
        let arguments = parse_seq(ctx, TokenKind::RParen, parse_expr)?;
        expect(ctx, TokenKind::RParen)?;

        let span = ctx.span_of(expr.span);

        expr = Node::apply(expr, arguments, span);
    }

    Ok(expr)
}

fn parse_unary(ctx: &mut Context) -> Result<Node> {
    let Some(operator) = operator(ctx, 0 as Precedence, true) else {
        return parse_apply(ctx);
    };

    let expr = parse_unary(ctx)?;

    Ok(Node::unary(operator, expr))
}

fn binary(ctx: &mut Context, precedence: Precedence) -> Result<Node> {
    let mut expr = parse_unary(ctx)?;

    while let Some(operator) = operator(ctx, precedence, false) {
        // accepts newlines if the line ends with an operator
        newline(ctx);

        // right associative operators do not increase the precedence level
        let next_precedence = if operator.kind == OperatorKind::Pow {
            operator.precedence()
        } else {
            operator.precedence() + 1
        };

        let rexpr = binary(ctx, next_precedence)?;
        let lexpr = expr;

        expr = Node::binary(operator, lexpr, rexpr);
    }

    Ok(expr)
}

fn operator(ctx: &mut Context, precedence: Precedence, unary: bool) -> Option<Operator> {
    let token = ctx.token;

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
        ctx.bump();
        Some(operator)
    } else {
        None
    }
}

fn parse_type_annotation(ctx: &mut Context) -> Result<Type> {
    expect(ctx, TokenKind::Colon)?;

    match ctx.token.kind {
        TokenKind::Identifier => {
            let name = parse_name(ctx)?;
            Ok(Type::name(name))
        }
        _ => Err(fail(unexpected(ctx, &[TokenKind::Type]), ctx.token.span)),
    }
}

fn newline(ctx: &mut Context) -> bool {
    maybe(ctx, TokenKind::EOL)
}

fn endline(ctx: &mut Context) -> Result<()> {
    if maybe(ctx, TokenKind::EOL) || maybe(ctx, TokenKind::Semi) || maybe(ctx, TokenKind::EOF) {
        Ok(())
    } else {
        Err(fail(unexpected(ctx, &[TokenKind::EOL, TokenKind::Semi]), ctx.token.span))
    }
}

fn expect(ctx: &mut Context, kind: TokenKind) -> Result<()> {
    if ctx.at(kind) {
        ctx.bump();
        Ok(())
    } else {
        Err(fail(unexpected(ctx, &[kind]), ctx.token.span))
    }
}

fn maybe(ctx: &mut Context, kind: TokenKind) -> bool {
    if ctx.at(kind) {
        ctx.bump();
        true
    } else {
        false
    }
}

fn parse_optional<F, T>(ctx: &mut Context, token: TokenKind, mut parse: F) -> Result<Option<T>>
where
    F: FnMut(&mut Context) -> Result<T>,
{
    Ok(if ctx.at(token) {
        Some(parse(ctx)?)
    } else {
        None
    })
}

fn parse_seq<F, T>(ctx: &mut Context, until: TokenKind, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Context) -> Result<T>,
{
    let mut result = vec![];

    newline(ctx);

    while !ctx.at(until) {
        result.push(parse(ctx)?);

        newline(ctx);
        if !maybe(ctx, TokenKind::Comma) {
            break;
        }
        newline(ctx);
    }

    Ok(result)
}

fn parse_many<F, T>(ctx: &mut Context, until: TokenKind, mut parse: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Context) -> Result<T>,
{
    let mut result = vec![];

    newline(ctx);

    while !ctx.at(until) {
        result.push(parse(ctx)?);
        endline(ctx)?;
    }

    Ok(result)
}

fn unexpected(ctx: &Context, expected: &[TokenKind]) -> String {
    let expected: Vec<_> = expected.iter().map(|it| it.to_string()).collect();
    format!("expected {}, found {}", expected.join(", "), ctx.token.kind)
}

fn fail(message: impl Into<String>, span: Span) -> Error {
    Error::error(span, message.into())
}

pub fn parse<'ctx>(stream: TokenStream<'ctx>) -> Result<Module> {
    let mut context = Context::new(stream);
    let mut nodes = vec![];

    newline(&mut context);

    while !context.done() {
        nodes.push(parse_top_level(&mut context)?);
        endline(&mut context)?;
    }

    Ok(Module { nodes })
}
