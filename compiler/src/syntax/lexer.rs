use super::token::{Token, TokenKind};
use crate::source::{Source, Span};

#[derive(Debug, Clone)]
struct Context<'ctx> {
    bytes: &'ctx [u8],
    curr: u8,
    peek: u8,
    index: usize,
    start: usize,
}

impl<'ctx> Context<'ctx> {
    fn new(source: &'ctx Source) -> Context<'ctx> {
        let mut ctx = Context {
            bytes: source.content.as_bytes(),
            curr: 0,
            peek: 0,
            index: 0,
            start: 0,
        };
        ctx.curr = ctx.at(0);
        ctx.peek = ctx.at(1);
        ctx
    }

    fn align(&mut self) {
        self.start = self.index;
    }

    fn bump(&mut self) -> u8 {
        if self.index == self.bytes.len() {
            return self.curr;
        }
        self.index += 1;
        self.curr = self.peek;
        self.peek = self.at(self.index + 1);
        self.curr
    }

    fn at(&self, idx: usize) -> u8 {
        if idx < self.bytes.len() {
            self.bytes[idx]
        } else {
            b'\0'
        }
    }

    fn slice(&self) -> &'ctx [u8] {
        &self.bytes[self.start..self.index]
    }

    fn span(&self) -> Span {
        Span::new(self.start as u32, self.index as u32)
    }

    fn bump_while<P>(&mut self, pred: P)
    where
        P: Fn(u8) -> bool,
    {
        while pred(self.curr) {
            self.bump();
        }
    }

    fn bump_utf8_sequence(&mut self) {
        let len = match self.curr {
            0x00..=0x7F => 1,
            0xC0..=0xDF => 2,
            0xE0..=0xEF => 3,
            0xF0..=0xF7 => 4,
            _ => 1,
        };

        for _ in 0..len {
            self.bump();
        }
    }

    fn trivia(&mut self) {
        loop {
            if self.curr.is_ascii_whitespace() && self.curr != b'\n' {
                self.bump_while(|chr| chr != b'\n' && chr.is_ascii_whitespace());
            } else if self.curr == b'/' && self.peek == b'/' {
                self.bump_while(|chr| chr != b'\n' && chr != b'\0');
            } else {
                break;
            }
        }
    }
}

fn token(ctx: &Context, kind: TokenKind) -> Token {
    Token {
        kind,
        span: ctx.span(),
    }
}

fn next(ctx: &mut Context) -> Token {
    ctx.trivia();
    ctx.align();

    let kind = match ctx.curr {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
            return ident(ctx);
        }
        b'0'..=b'9' => {
            return number(ctx);
        }
        b'"' => {
            return string(ctx);
        }
        b'\n' => {
            return newline(ctx);
        }
        _ if is_symbol(ctx.curr) => {
            return operator(ctx);
        }
        b'(' => TokenKind::LParen,
        b')' => TokenKind::RParen,
        b'{' => TokenKind::LBrace,
        b'}' => TokenKind::RBrace,
        b'[' => TokenKind::LBracket,
        b']' => TokenKind::RBracket,
        b';' => TokenKind::Semi,
        b',' => TokenKind::Comma,
        0 => TokenKind::EOF,
        _ => {
            return unexpected_character(ctx);
        }
    };

    ctx.bump();

    token(ctx, kind)
}

fn ident(ctx: &mut Context) -> Token {
    ctx.bump_while(|chr| matches!(chr, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
    ctx.bump_while(|chr| chr == b'\'');

    let value = ctx.slice();

    let kind = match value {
        b"True" => TokenKind::True,
        b"False" => TokenKind::False,
        b"and" => TokenKind::And,
        b"or" => TokenKind::Or,
        b"not" => TokenKind::Not,
        _ => TokenKind::Identifier,
    };

    token(ctx, kind)
}

#[rustfmt::skip]
fn is_symbol(chr: u8) -> bool {
    matches!(
        chr,
        b'=' | b':' | b'.' | b'|' | b'<' | b'>' | b'!' | b'+' | b'-' | b'*' | b'/' | b'^' | b'%'
    )
}

fn operator(ctx: &mut Context) -> Token {
    let kind = match ctx.curr {
        b'.' if ctx.peek == b'.' => {
            ctx.bump();
            TokenKind::Dots
        }
        b'.' => TokenKind::Dot,
        b'=' if ctx.peek == b'=' => {
            ctx.bump();
            TokenKind::EqEq
        }
        b'=' if ctx.peek == b'>' => {
            ctx.bump();
            TokenKind::Arrow
        }
        b'=' => TokenKind::Equals,
        b':' => TokenKind::Colon,
        b'<' if ctx.peek == b'=' => {
            ctx.bump();
            TokenKind::LtEq
        }
        b'<' => TokenKind::Lt,
        b'>' if ctx.peek == b'=' => {
            ctx.bump();
            TokenKind::GtEq
        }
        b'>' => TokenKind::Gt,
        b'!' if ctx.peek == b'=' => {
            ctx.bump();
            TokenKind::NoEq
        }
        b'+' => TokenKind::Plus,
        b'-' => TokenKind::Minus,
        b'*' => TokenKind::Star,
        b'/' => TokenKind::Slash,
        b'^' => TokenKind::Caret,
        b'%' => TokenKind::Mod,
        _ => unreachable!(),
    };

    ctx.bump();

    token(ctx, kind)
}

fn number(ctx: &mut Context) -> Token {
    ctx.bump_while(|chr| chr.is_ascii_digit());

    match ctx.curr {
        b'.' if ctx.peek.is_ascii_digit() => {
            ctx.bump();
            ctx.bump_while(|chr| chr.is_ascii_digit());

            return token(ctx, TokenKind::Number);
        }
        b'b' if ctx.slice() == b"0" => {
            ctx.bump();
            ctx.bump_while(|chr| matches!(chr, b'0' | b'1'));
        }
        b'x' if ctx.slice() == b"0" => {
            ctx.bump();
            ctx.bump_while(|chr| chr.is_ascii_hexdigit());
        }
        _ => {}
    }

    token(ctx, TokenKind::Integer)
}

fn string(ctx: &mut Context) -> Token {
    loop {
        match ctx.bump() {
            b'\n' | b'\0' => {
                return token(ctx, TokenKind::UnterminatedString);
            }
            b'"' => {
                ctx.bump();
                break;
            }
            b'\\' => {
                match ctx.bump() {
                    b'\\' | b'n' | b'r' | b't' | b'"' | b'0' => {}
                    b'u' => todo!("validate escaped unicode"),
                    b'x' => todo!("validate escaped binary"),
                    _ => {
                        return invalid_escape_character(ctx);
                    }
                }
            }
            _ => {}
        }
    }

    token(ctx, TokenKind::String)
}

fn invalid_escape_character(ctx: &mut Context) -> Token {
    ctx.align();
    ctx.bump_utf8_sequence();

    let token = token(ctx, TokenKind::UnexpectedCharacter);

    // skip to the end of the string
    loop {
        match ctx.curr {
            b'"' => {
                ctx.bump();
                break;
            }
            b'\n' | b'\0' => {
                break;
            }
            _ => {}
        }
        ctx.bump();
    }

    token
}

fn unexpected_character(ctx: &mut Context) -> Token {
    ctx.bump_utf8_sequence();
    token(ctx, TokenKind::UnexpectedCharacter)
}

fn newline(ctx: &mut Context) -> Token {
    while ctx.curr == b'\n' {
        ctx.bump();
        ctx.trivia();
    }
    token(ctx, TokenKind::EOL)
}

#[derive(Debug, Clone)]
pub struct TokenStream<'ctx> {
    ctx: Context<'ctx>,
    prev: Token,
    curr: Token,
    peek: Token,
}

impl<'tokens> TokenStream<'tokens> {
    pub fn new(source: &'tokens Source) -> TokenStream<'tokens> {
        TokenStream {
            ctx: Context::new(source),
            prev: Token::default(),
            peek: Token::default(),
            curr: Token::default(),
        }
        .init()
    }

    fn init(mut self) -> TokenStream<'tokens> {
        self.bump();
        self.bump();
        self
    }

    pub fn bump(&mut self) {
        self.prev = self.curr;
        self.curr = self.peek;
        self.peek = next(&mut self.ctx);
    }

    pub fn token(&self) -> Token {
        self.curr
    }

    pub fn prev(&self) -> Token {
        self.prev
    }

    pub fn peek(&self) -> Token {
        self.peek
    }

    pub fn done(&self) -> bool {
        self.curr.kind == TokenKind::EOF
    }
}

pub fn tokens<'tokens>(source: &'tokens Source) -> TokenStream<'tokens> {
    TokenStream::new(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct TestResult {
        token: TokenKind,
        span: Span,
        value: String,
    }

    fn tokenize<'a>(input: &'a str) -> Vec<TestResult> {
        let source = Source::new("<test>".to_string(), input.into());
        let mut tokens = tokens(&source);

        let mut result = vec![];

        while tokens.curr.kind != TokenKind::EOF {
            result.push(TestResult {
                token: tokens.curr.kind,
                span: tokens.curr.span,
                value: input[tokens.curr.span.range()].to_string(),
            });

            tokens.bump();
        }

        result
    }

    #[test]
    fn identifier() {
        let tokens = tokenize("ident test_1 CONST _ ___");

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(0, 5),
                    value: "ident".to_string(),
                },
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(6, 12),
                    value: "test_1".to_string(),
                },
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(13, 18),
                    value: "CONST".to_string(),
                },
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(19, 20),
                    value: "_".to_string(),
                },
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(21, 24),
                    value: "___".to_string(),
                },
            ]
        );
    }

    #[test]
    fn keywords() {
        let tokens = tokenize("and or not True False");

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::And,
                    span: Span::new(0, 3),
                    value: "and".to_string(),
                },
                TestResult {
                    token: TokenKind::Or,
                    span: Span::new(4, 6),
                    value: "or".to_string(),
                },
                TestResult {
                    token: TokenKind::Not,
                    span: Span::new(7, 10),
                    value: "not".to_string(),
                },
                TestResult {
                    token: TokenKind::True,
                    span: Span::new(11, 15),
                    value: "True".to_string(),
                },
                TestResult {
                    token: TokenKind::False,
                    span: Span::new(16, 21),
                    value: "False".to_string(),
                },
            ]
        );
    }

    #[test]
    fn punctuation() {
        let tokens = tokenize(". , ; ( ) [ ] { }");

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::Dot,
                    span: Span::new(0, 1),
                    value: ".".to_string(),
                },
                TestResult {
                    token: TokenKind::Comma,
                    span: Span::new(2, 3),
                    value: ",".to_string(),
                },
                TestResult {
                    token: TokenKind::Semi,
                    span: Span::new(4, 5),
                    value: ";".to_string(),
                },
                TestResult {
                    token: TokenKind::LParen,
                    span: Span::new(6, 7),
                    value: "(".to_string(),
                },
                TestResult {
                    token: TokenKind::RParen,
                    span: Span::new(8, 9),
                    value: ")".to_string(),
                },
                TestResult {
                    token: TokenKind::LBracket,
                    span: Span::new(10, 11),
                    value: "[".to_string(),
                },
                TestResult {
                    token: TokenKind::RBracket,
                    span: Span::new(12, 13),
                    value: "]".to_string(),
                },
                TestResult {
                    token: TokenKind::LBrace,
                    span: Span::new(14, 15),
                    value: "{".to_string(),
                },
                TestResult {
                    token: TokenKind::RBrace,
                    span: Span::new(16, 17),
                    value: "}".to_string(),
                },
            ]
        )
    }

    #[test]
    fn operators() {
        let tokens = tokenize(". : => = == != + - * / % ^ > >= < <= ..");

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::Dot,
                    span: Span::new(0, 1),
                    value: ".".to_string(),
                },
                TestResult {
                    token: TokenKind::Colon,
                    span: Span::new(2, 3),
                    value: ":".to_string(),
                },
                TestResult {
                    token: TokenKind::Arrow,
                    span: Span::new(4, 6),
                    value: "=>".to_string(),
                },
                TestResult {
                    token: TokenKind::Equals,
                    span: Span::new(7, 8),
                    value: "=".to_string(),
                },
                TestResult {
                    token: TokenKind::EqEq,
                    span: Span::new(9, 11),
                    value: "==".to_string(),
                },
                TestResult {
                    token: TokenKind::NoEq,
                    span: Span::new(12, 14),
                    value: "!=".to_string(),
                },
                TestResult {
                    token: TokenKind::Plus,
                    span: Span::new(15, 16),
                    value: "+".to_string(),
                },
                TestResult {
                    token: TokenKind::Minus,
                    span: Span::new(17, 18),
                    value: "-".to_string(),
                },
                TestResult {
                    token: TokenKind::Star,
                    span: Span::new(19, 20),
                    value: "*".to_string(),
                },
                TestResult {
                    token: TokenKind::Slash,
                    span: Span::new(21, 22),
                    value: "/".to_string(),
                },
                TestResult {
                    token: TokenKind::Mod,
                    span: Span::new(23, 24),
                    value: "%".to_string(),
                },
                TestResult {
                    token: TokenKind::Caret,
                    span: Span::new(25, 26),
                    value: "^".to_string(),
                },
                TestResult {
                    token: TokenKind::Gt,
                    span: Span::new(27, 28),
                    value: ">".to_string(),
                },
                TestResult {
                    token: TokenKind::GtEq,
                    span: Span::new(29, 31),
                    value: ">=".to_string(),
                },
                TestResult {
                    token: TokenKind::Lt,
                    span: Span::new(32, 33),
                    value: "<".to_string(),
                },
                TestResult {
                    token: TokenKind::LtEq,
                    span: Span::new(34, 36),
                    value: "<=".to_string(),
                },
                TestResult {
                    token: TokenKind::Dots,
                    span: Span::new(37, 39),
                    value: "..".to_string(),
                },
            ]
        )
    }

    #[test]
    fn number() {
        let tokens = tokenize("42 3.14 0xABCDEF 0b0101");

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::Integer,
                    span: Span::new(0, 2),
                    value: "42".to_string(),
                },
                TestResult {
                    token: TokenKind::Number,
                    span: Span::new(3, 7),
                    value: "3.14".to_string(),
                },
                TestResult {
                    token: TokenKind::Integer,
                    span: Span::new(8, 16),
                    value: "0xABCDEF".to_string(),
                },
                TestResult {
                    token: TokenKind::Integer,
                    span: Span::new(17, 23),
                    value: "0b0101".to_string(),
                },
            ]
        )
    }

    #[test]
    fn simple_string() {
        let tokens = tokenize(r#""Hello, World""#);

        assert_eq!(
            tokens,
            vec![TestResult {
                token: TokenKind::String,
                span: Span::new(0, 14),
                value: "\"Hello, World\"".to_string(),
            }],
        );
    }

    #[test]
    fn nested_string() {
        let tokens = tokenize(r#""code = \"n = 42\"""#);

        assert_eq!(
            tokens,
            vec![TestResult {
                token: TokenKind::String,
                span: Span::new(0, 19),
                value: "\"code = \\\"n = 42\\\"\"".to_string(),
            }]
        );
    }

    #[test]
    fn empty_lines() {
        let tokens = tokenize("\n\n\n");

        assert_eq!(
            tokens,
            vec![TestResult {
                token: TokenKind::EOL,
                span: Span::new(0, 3),
                value: "\n\n\n".to_string(),
            }]
        );
    }

    #[test]
    fn unterminated_string() {
        let tokens = tokenize(r#""Hello"#);

        assert_eq!(
            tokens,
            vec![TestResult {
                token: TokenKind::UnterminatedString,
                span: Span::new(0, 6),
                value: r#""Hello"#.to_string(),
            }]
        );
    }

    #[test]
    fn unbalanced_quotes() {
        let tokens = tokenize(r#""Hello"""#);

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::String,
                    span: Span::new(0, 7),
                    value: r#""Hello""#.to_string(),
                },
                TestResult {
                    token: TokenKind::UnterminatedString,
                    span: Span::new(7, 8),
                    value: "\"".to_string(),
                }
            ]
        );
    }

    #[test]
    fn escaped_character() {
        let tokens = tokenize(r#""escape \\a".length"#);

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::String,
                    span: Span::new(0, 12),
                    value: r#""escape \\a""#.to_string(),
                },
                TestResult {
                    token: TokenKind::Dot,
                    span: Span::new(12, 13),
                    value: ".".to_string(),
                },
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(13, 19),
                    value: "length".to_string(),
                },
            ]
        );
    }

    #[test]
    fn invalid_escaped_character() {
        let tokens = tokenize(r#""escape \a".length"#);

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::UnexpectedCharacter,
                    span: Span::new(9, 10),
                    value: "a".to_string(),
                },
                TestResult {
                    token: TokenKind::Dot,
                    span: Span::new(11, 12),
                    value: ".".to_string(),
                },
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(12, 18),
                    value: "length".to_string(),
                },
            ]
        );
    }

    #[test]
    fn unexpected_character() {
        let tokens = tokenize("@test");

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::UnexpectedCharacter,
                    span: Span::new(0, 1),
                    value: "@".to_string(),
                },
                TestResult {
                    token: TokenKind::Identifier,
                    span: Span::new(1, 5),
                    value: "test".to_string(),
                }
            ]
        );
    }

    #[test]
    fn unexpected_character_uft8() {
        let tokens = tokenize("🫵 = 🎅🏾");

        assert_eq!(
            tokens,
            vec![
                TestResult {
                    token: TokenKind::UnexpectedCharacter,
                    span: Span::new(0, 4),
                    value: "🫵".to_string(),
                },
                TestResult {
                    token: TokenKind::Equals,
                    span: Span::new(5, 6),
                    value: "=".to_string(),
                },
                TestResult {
                    token: TokenKind::UnexpectedCharacter,
                    span: Span::new(7, 11),
                    value: "🎅".to_string(),
                },
                TestResult {
                    token: TokenKind::UnexpectedCharacter,
                    span: Span::new(11, 15),
                    value: "🏾".to_string(),
                }
            ]
        );
    }
}
