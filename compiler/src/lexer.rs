use crate::source::{Source, Span};

use crate::token::{Token, TokenKind};

#[derive(Debug, Clone)]
struct Lexer<'lexer> {
    source: &'lexer Source,
    bytes: &'lexer [u8],
    curr: u8,
    peek: u8,
    index: usize,
    start: usize,
}

impl<'lexer> Lexer<'lexer> {
    fn new(source: &'lexer Source) -> Lexer<'lexer> {
        let mut lexer = Lexer {
            source,
            bytes: source.content.as_bytes(),
            curr: 0,
            peek: 0,
            index: 0,
            start: 0,
        };
        lexer.curr = lexer.at(0);
        lexer.peek = lexer.at(1);
        lexer
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

    fn slice(&self) -> &'lexer str {
        &self.source.content[self.start..self.index]
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

    fn token(&mut self, kind: TokenKind) -> Token {
        Token {
            kind,
            span: self.span(),
        }
    }

    fn next(&mut self) -> Token {
        self.trivia();
        self.align();

        let kind = match self.curr {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return self.ident();
            }
            b'0'..=b'9' => {
                return self.number();
            }
            b'"' => {
                return self.string();
            }
            b'\n' => {
                return self.newline();
            }
            _ if self.is_symbol(self.curr) => {
                return self.operator();
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
                return self.unexpected_character();
            }
        };

        self.bump();

        self.token(kind)
    }

    fn ident(&mut self) -> Token {
        self.bump_while(|chr| matches!(chr, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
        self.bump_while(|chr| chr == b'\'');

        let value = self.slice();

        let kind = match value {
            "True" => TokenKind::True,
            "False" => TokenKind::False,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            _ => TokenKind::Identifier,
        };

        self.token(kind)
    }

    #[rustfmt::skip]
    fn is_symbol(&self, chr: u8) -> bool {
        matches!(
            chr,
            b'=' | b':' | b'.' | b'|' | b'<' | b'>' | b'!' | b'+' | b'-' | b'*' | b'/' | b'^' | b'%'
        )
    }

    fn operator(&mut self) -> Token {
        let kind = match self.curr {
            b'.' if self.peek == b'.' => {
                self.bump();
                TokenKind::Dots
            }
            b'.' => TokenKind::Dot,
            b'=' if self.peek == b'=' => {
                self.bump();
                TokenKind::EqEq
            }
            b'=' if self.peek == b'>' => {
                self.bump();
                TokenKind::Arrow
            }
            b'=' => TokenKind::Equals,
            b':' => TokenKind::Colon,
            b'<' if self.peek == b'=' => {
                self.bump();
                TokenKind::LtEq
            }
            b'<' => TokenKind::Lt,
            b'>' if self.peek == b'=' => {
                self.bump();
                TokenKind::GtEq
            }
            b'>' => TokenKind::Gt,
            b'!' if self.peek == b'=' => {
                self.bump();
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

        self.bump();

        self.token(kind)
    }

    fn number(&mut self) -> Token {
        self.bump_while(|chr| chr.is_ascii_digit());

        match self.curr {
            b'.' if self.peek.is_ascii_digit() => {
                self.bump();
                self.bump_while(|chr| chr.is_ascii_digit());

                return self.token(TokenKind::Number);
            }
            b'b' if self.slice() == "0" => {
                self.bump();
                self.bump_while(|chr| matches!(chr, b'0' | b'1'));
            }
            b'x' if self.slice() == "0" => {
                self.bump();
                self.bump_while(|chr| chr.is_ascii_hexdigit());
            }
            _ => {}
        }

        self.token(TokenKind::Integer)
    }

    fn string(&mut self) -> Token {
        loop {
            match self.bump() {
                b'\n' | b'\0' => {
                    return self.token(TokenKind::UnterminatedString);
                }
                b'"' => {
                    self.bump();
                    break;
                }
                b'\\' => {
                    match self.bump() {
                        b'\\' | b'n' | b'r' | b't' | b'"' | b'0' => {}
                        b'u' => todo!("validate escaped unicode"),
                        b'x' => todo!("validate escaped binary"),
                        _ => {
                            return self.invalid_escape_character();
                        }
                    }
                }
                _ => {}
            }
        }

        self.token(TokenKind::String)
    }

    fn invalid_escape_character(&mut self) -> Token {
        self.align();
        self.bump_utf8_sequence();

        let token = self.token(TokenKind::UnexpectedCharacter);

        // skip to the end of the string
        loop {
            match self.curr {
                b'"' => {
                    self.bump();
                    break;
                }
                b'\n' | b'\0' => {
                    break;
                }
                _ => {}
            }
            self.bump();
        }

        token
    }

    fn unexpected_character(&mut self) -> Token {
        self.bump_utf8_sequence();
        self.token(TokenKind::UnexpectedCharacter)
    }

    fn bump_utf8_sequence(&mut self) {
        let mut chars = self.source.content[self.index..].chars();

        if let Some(chr) = chars.next() {
            for _ in 0..chr.len_utf8() {
                self.bump();
            }
        } else {
            self.bump();
        }
    }

    fn newline(&mut self) -> Token {
        while self.curr == b'\n' {
            self.bump();
            self.trivia();
        }
        self.token(TokenKind::EOL)
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

    fn value(&self, span: Span) -> &str {
        &self.source.content[span.range()]
    }
}

#[derive(Debug, Clone)]
pub struct Tokens<'lexer> {
    lexer: Lexer<'lexer>,
    prev: Option<Token>,
    peek: Token,
    token: Token,
}

impl<'tokens> Tokens<'tokens> {
    pub fn new(source: &'tokens Source) -> Tokens<'tokens> {
        Tokens {
            lexer: Lexer::new(source),
            prev: None,
            peek: Token::default(),
            token: Token::default(),
        }
        .init()
    }

    fn init(mut self) -> Tokens<'tokens> {
        self.bump();
        self.bump();
        self
    }

    pub fn bump(&mut self) {
        self.prev = Some(self.token);
        self.token = self.peek;
        self.peek = self.lexer.next();
    }

    #[inline]
    pub fn token(&self) -> Token {
        self.token
    }

    #[inline]
    pub fn prev(&self) -> Token {
        self.prev.unwrap_or_default()
    }

    #[inline]
    pub fn peek(&self) -> Token {
        self.peek
    }

    pub fn value(&self, token: Token) -> &str {
        self.lexer.value(token.span)
    }

    pub fn done(&self) -> bool {
        self.token.kind == TokenKind::EOF
    }
}

pub fn tokens<'tokens>(source: &'tokens Source) -> Tokens<'tokens> {
    Tokens::new(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize<'a>(input: &'a str) -> Vec<(Token, String)> {
        let source = Source::new("<test>".to_string(), input.into());
        let mut tokens = tokens(&source);

        let mut result = vec![];

        while tokens.token.kind != TokenKind::EOF {
            let token = tokens.token;
            result.push((token, tokens.value(token).to_string()));
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
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(0, 5),
                    },
                    "ident".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(6, 12),
                    },
                    "test_1".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(13, 18),
                    },
                    "CONST".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(19, 20),
                    },
                    "_".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(21, 24),
                    },
                    "___".to_string()
                ),
            ]
        );
    }

    #[test]
    fn keywords() {
        let tokens = tokenize("and or not True False");

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::And,
                        span: Span::new(0, 3)
                    },
                    "and".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Or,
                        span: Span::new(4, 6)
                    },
                    "or".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Not,
                        span: Span::new(7, 10)
                    },
                    "not".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::True,
                        span: Span::new(11, 15)
                    },
                    "True".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::False,
                        span: Span::new(16, 21)
                    },
                    "False".to_string()
                ),
            ]
        );
    }

    #[test]
    fn punctuation() {
        let tokens = tokenize(". , ; ( ) [ ] { }");

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::Dot,
                        span: Span::new(0, 1)
                    },
                    ".".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Comma,
                        span: Span::new(2, 3)
                    },
                    ",".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Semi,
                        span: Span::new(4, 5)
                    },
                    ";".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::LParen,
                        span: Span::new(6, 7)
                    },
                    "(".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::RParen,
                        span: Span::new(8, 9)
                    },
                    ")".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::LBracket,
                        span: Span::new(10, 11)
                    },
                    "[".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::RBracket,
                        span: Span::new(12, 13)
                    },
                    "]".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::LBrace,
                        span: Span::new(14, 15)
                    },
                    "{".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::RBrace,
                        span: Span::new(16, 17)
                    },
                    "}".to_string()
                ),
            ]
        )
    }

    #[test]
    fn operators() {
        let tokens = tokenize(". : => = == != + - * / % ^ > >= < <= ..");

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::Dot,
                        span: Span::new(0, 1)
                    },
                    ".".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Colon,
                        span: Span::new(2, 3)
                    },
                    ":".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Arrow,
                        span: Span::new(4, 6)
                    },
                    "=>".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Equals,
                        span: Span::new(7, 8)
                    },
                    "=".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::EqEq,
                        span: Span::new(9, 11)
                    },
                    "==".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::NoEq,
                        span: Span::new(12, 14)
                    },
                    "!=".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Plus,
                        span: Span::new(15, 16)
                    },
                    "+".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Minus,
                        span: Span::new(17, 18)
                    },
                    "-".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Star,
                        span: Span::new(19, 20)
                    },
                    "*".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Slash,
                        span: Span::new(21, 22)
                    },
                    "/".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Mod,
                        span: Span::new(23, 24)
                    },
                    "%".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Caret,
                        span: Span::new(25, 26)
                    },
                    "^".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Gt,
                        span: Span::new(27, 28)
                    },
                    ">".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::GtEq,
                        span: Span::new(29, 31)
                    },
                    ">=".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Lt,
                        span: Span::new(32, 33)
                    },
                    "<".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::LtEq,
                        span: Span::new(34, 36)
                    },
                    "<=".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Dots,
                        span: Span::new(37, 39)
                    },
                    "..".to_string()
                ),
            ]
        )
    }

    #[test]
    fn number() {
        let tokens = tokenize("42 3.14 0xABCDEF 0b0101");

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::Integer,
                        span: Span::new(0, 2)
                    },
                    "42".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Number,
                        span: Span::new(3, 7)
                    },
                    "3.14".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Integer,
                        span: Span::new(8, 16)
                    },
                    "0xABCDEF".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Integer,
                        span: Span::new(17, 23)
                    },
                    "0b0101".to_string()
                ),
            ]
        )
    }

    #[test]
    fn simple_string() {
        let tokens = tokenize(r#""Hello, World""#);

        assert_eq!(
            tokens,
            vec![(
                Token {
                    kind: TokenKind::String,
                    span: Span::new(0, 14)
                },
                "\"Hello, World\"".to_string()
            )],
        );
    }

    #[test]
    fn nested_string() {
        let tokens = tokenize(r#""code = \"n = 42\"""#);

        assert_eq!(
            tokens,
            vec![(
                Token {
                    kind: TokenKind::String,
                    span: Span::new(0, 19)
                },
                "\"code = \\\"n = 42\\\"\"".to_string()
            )]
        );
    }

    #[test]
    fn empty_lines() {
        let tokens = tokenize("\n\n\n");

        assert_eq!(
            tokens,
            vec![(
                Token {
                    kind: TokenKind::EOL,
                    span: Span::new(0, 3)
                },
                "\n\n\n".to_string()
            )]
        );
    }

    #[test]
    fn unterminated_string() {
        let tokens = tokenize(r#""Hello"#);

        assert_eq!(
            tokens,
            vec![(
                Token {
                    kind: TokenKind::UnterminatedString,
                    span: Span::new(0, 6)
                },
                r#""Hello"#.to_string()
            )]
        );
    }

    #[test]
    fn unbalanced_quotes() {
        let tokens = tokenize(r#""Hello"""#);

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::String,
                        span: Span::new(0, 7)
                    },
                    r#""Hello""#.to_string()
                ),
                (
                    Token {
                        kind: TokenKind::UnterminatedString,
                        span: Span::new(7, 8)
                    },
                    "\"".to_string()
                )
            ]
        );
    }

    #[test]
    fn escaped_character() {
        let tokens = tokenize(r#""escape \\a".length"#);

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::String,
                        span: Span::new(0, 12)
                    },
                    r#""escape \\a""#.to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Dot,
                        span: Span::new(12, 13)
                    },
                    ".".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(13, 19)
                    },
                    "length".to_string()
                ),
            ]
        );
    }

    #[test]
    fn invalid_escaped_character() {
        let tokens = tokenize(r#""escape \a".length"#);

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::UnexpectedCharacter,
                        span: Span::new(9, 10)
                    },
                    "a".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Dot,
                        span: Span::new(11, 12)
                    },
                    ".".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(12, 18)
                    },
                    "length".to_string()
                ),
            ]
        );
    }

    #[test]
    fn unexpected_character() {
        let tokens = tokenize("@test");

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::UnexpectedCharacter,
                        span: Span::new(0, 1)
                    },
                    "@".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Identifier,
                        span: Span::new(1, 5)
                    },
                    "test".to_string()
                )
            ]
        );
    }

    #[test]
    fn unexpected_character_uft8() {
        let tokens = tokenize("🫵 = 🎅🏾");

        assert_eq!(
            tokens,
            vec![
                (
                    Token {
                        kind: TokenKind::UnexpectedCharacter,
                        span: Span::new(0, 4)
                    },
                    "🫵".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::Equals,
                        span: Span::new(5, 6)
                    },
                    "=".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::UnexpectedCharacter,
                        span: Span::new(7, 11)
                    },
                    "🎅".to_string()
                ),
                (
                    Token {
                        kind: TokenKind::UnexpectedCharacter,
                        span: Span::new(11, 15)
                    },
                    "🏾".to_string()
                )
            ]
        );
    }

    #[test]
    fn peek_prev() {
        let source = Source::new("<test>".to_string(), "text 3.14 _".into());
        let mut tokens = tokens(&source);

        assert_eq!(
            tokens.token(),
            Token {
                kind: TokenKind::Identifier,
                span: Span::new(0, 4)
            }
        );
        assert_eq!(tokens.value(tokens.token()), "text");

        assert_eq!(
            tokens.peek(),
            Token {
                kind: TokenKind::Number,
                span: Span::new(5, 9)
            }
        );
        assert_eq!(tokens.value(tokens.peek()), "3.14");

        tokens.bump();

        assert_eq!(
            tokens.token(),
            Token {
                kind: TokenKind::Number,
                span: Span::new(5, 9)
            }
        );
        assert_eq!(tokens.value(tokens.token()), "3.14");

        assert_eq!(
            tokens.peek(),
            Token {
                kind: TokenKind::Identifier,
                span: Span::new(10, 11)
            }
        );
        assert_eq!(tokens.value(tokens.peek()), "_");

        assert_eq!(
            tokens.prev(),
            Token {
                kind: TokenKind::Identifier,
                span: Span::new(0, 4)
            }
        );
        assert_eq!(tokens.value(tokens.prev()), "text");
    }
}
