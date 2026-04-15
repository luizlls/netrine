use crate::source::{Source, Span, WithSpan};
use crate::token::Token;

#[derive(Debug, Clone)]
struct Lexer<'lexer> {
    bytes: &'lexer [u8],
    curr: u8,
    peek: u8,
    index: usize,
    start: usize,
}

impl<'lexer> Lexer<'lexer> {
    fn new(source: &'lexer Source) -> Lexer<'lexer> {
        let mut lexer = Lexer {
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

    fn slice(&self) -> &'lexer [u8] {
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

    fn token(&mut self, token: Token) -> WithSpan<Token> {
        WithSpan::<Token> {
            value: token,
            span: self.span(),
        }
    }

    fn next(&mut self) -> WithSpan<Token> {
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
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,
            b';' => Token::Semi,
            b',' => Token::Comma,
            0 => Token::EOF,
            _ => {
                return self.unexpected_character();
            }
        };

        self.bump();

        self.token(kind)
    }

    fn ident(&mut self) -> WithSpan<Token> {
        self.bump_while(|chr| matches!(chr, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
        self.bump_while(|chr| chr == b'\'');

        let value = self.slice();

        let kind = match value {
            b"let" => Token::Let,
            b"True" => Token::True,
            b"False" => Token::False,
            b"and" => Token::And,
            b"or" => Token::Or,
            b"not" => Token::Not,
            _ => Token::Identifier,
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

    fn operator(&mut self) -> WithSpan<Token> {
        let kind = match self.curr {
            b'.' if self.peek == b'.' => {
                self.bump();
                Token::Dots
            }
            b'.' => Token::Dot,
            b'=' if self.peek == b'=' => {
                self.bump();
                Token::EqEq
            }
            b'=' if self.peek == b'>' => {
                self.bump();
                Token::Arrow
            }
            b'=' => Token::Equals,
            b':' => Token::Colon,
            b'<' if self.peek == b'=' => {
                self.bump();
                Token::LtEq
            }
            b'<' => Token::Lt,
            b'>' if self.peek == b'=' => {
                self.bump();
                Token::GtEq
            }
            b'>' => Token::Gt,
            b'!' if self.peek == b'=' => {
                self.bump();
                Token::NoEq
            }
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Star,
            b'/' => Token::Slash,
            b'^' => Token::Caret,
            b'%' => Token::Mod,
            _ => unreachable!(),
        };

        self.bump();

        self.token(kind)
    }

    fn number(&mut self) -> WithSpan<Token> {
        self.bump_while(|chr| chr.is_ascii_digit());

        match self.curr {
            b'.' if self.peek.is_ascii_digit() => {
                self.bump();
                self.bump_while(|chr| chr.is_ascii_digit());

                return self.token(Token::Number);
            }
            b'b' if self.slice() == b"0" => {
                self.bump();
                self.bump_while(|chr| matches!(chr, b'0' | b'1'));
            }
            b'x' if self.slice() == b"0" => {
                self.bump();
                self.bump_while(|chr| chr.is_ascii_hexdigit());
            }
            _ => {}
        }

        self.token(Token::Integer)
    }

    fn string(&mut self) -> WithSpan<Token> {
        loop {
            match self.bump() {
                b'\n' | b'\0' => {
                    return self.token(Token::UnterminatedString);
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

        self.token(Token::String)
    }

    fn invalid_escape_character(&mut self) -> WithSpan<Token> {
        self.align();
        self.bump_utf8_sequence();

        let token = self.token(Token::UnexpectedCharacter);

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

    fn unexpected_character(&mut self) -> WithSpan<Token> {
        self.bump_utf8_sequence();
        self.token(Token::UnexpectedCharacter)
    }

    fn bump_utf8_sequence(&mut self) {
        todo!()
    }

    fn newline(&mut self) -> WithSpan<Token> {
        while self.curr == b'\n' {
            self.bump();
            self.trivia();
        }
        self.token(Token::EOL)
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

#[derive(Debug, Clone)]
pub struct Tokens<'lexer> {
    lexer: Lexer<'lexer>,
    peek: WithSpan<Token>,
    current: WithSpan<Token>,
}

impl<'tokens> Tokens<'tokens> {
    pub fn new(source: &'tokens Source) -> Tokens<'tokens> {
        Tokens {
            lexer: Lexer::new(source),
            peek: Default::default(),
            current: Default::default(),
        }
        .init()
    }

    fn init(mut self) -> Tokens<'tokens> {
        self.bump();
        self.bump();
        self
    }

    pub fn bump(&mut self) {
        self.current = self.peek;
        self.peek = self.lexer.next();
    }

    #[inline]
    pub fn token(&self) -> WithSpan<Token> {
        self.current
    }

    #[inline]
    pub fn peek(&self) -> WithSpan<Token> {
        self.peek
    }

    pub fn done(&self) -> bool {
        self.current.value == Token::EOF
    }
}

pub fn tokens<'tokens>(source: &'tokens Source) -> Tokens<'tokens> {
    Tokens::new(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct TestResult {
        token: Token,
        span: Span,
        value: String,
    }

    fn tokenize<'a>(input: &'a str) -> Vec<TestResult> {
        let source = Source::new("<test>".to_string(), input.into());
        let mut tokens = tokens(&source);

        let mut result = vec![];

        while tokens.current.value != Token::EOF {
            let token = tokens.current.value;
            let span = tokens.current.span;

            result.push(TestResult {
                token,
                span,
                value: input[span.range()].to_string(),
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
                    token: Token::Identifier,
                    span: Span::new(0, 5),
                    value: "ident".to_string(),
                },
                TestResult {
                    token: Token::Identifier,
                    span: Span::new(6, 12),
                    value: "test_1".to_string(),
                },
                TestResult {
                    token: Token::Identifier,
                    span: Span::new(13, 18),
                    value: "CONST".to_string(),
                },
                TestResult {
                    token: Token::Identifier,
                    span: Span::new(19, 20),
                    value: "_".to_string(),
                },
                TestResult {
                    token: Token::Identifier,
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
                    token: Token::And,
                    span: Span::new(0, 3),
                    value: "and".to_string(),
                },
                TestResult {
                    token: Token::Or,
                    span: Span::new(4, 6),
                    value: "or".to_string(),
                },
                TestResult {
                    token: Token::Not,
                    span: Span::new(7, 10),
                    value: "not".to_string(),
                },
                TestResult {
                    token: Token::True,
                    span: Span::new(11, 15),
                    value: "True".to_string(),
                },
                TestResult {
                    token: Token::False,
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
                    token: Token::Dot,
                    span: Span::new(0, 1),
                    value: ".".to_string(),
                },
                TestResult {
                    token: Token::Comma,
                    span: Span::new(2, 3),
                    value: ",".to_string(),
                },
                TestResult {
                    token: Token::Semi,
                    span: Span::new(4, 5),
                    value: ";".to_string(),
                },
                TestResult {
                    token: Token::LParen,
                    span: Span::new(6, 7),
                    value: "(".to_string(),
                },
                TestResult {
                    token: Token::RParen,
                    span: Span::new(8, 9),
                    value: ")".to_string(),
                },
                TestResult {
                    token: Token::LBracket,
                    span: Span::new(10, 11),
                    value: "[".to_string(),
                },
                TestResult {
                    token: Token::RBracket,
                    span: Span::new(12, 13),
                    value: "]".to_string(),
                },
                TestResult {
                    token: Token::LBrace,
                    span: Span::new(14, 15),
                    value: "{".to_string(),
                },
                TestResult {
                    token: Token::RBrace,
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
                    token: Token::Dot,
                    span: Span::new(0, 1),
                    value: ".".to_string(),
                },
                TestResult {
                    token: Token::Colon,
                    span: Span::new(2, 3),
                    value: ":".to_string(),
                },
                TestResult {
                    token: Token::Arrow,
                    span: Span::new(4, 6),
                    value: "=>".to_string(),
                },
                TestResult {
                    token: Token::Equals,
                    span: Span::new(7, 8),
                    value: "=".to_string(),
                },
                TestResult {
                    token: Token::EqEq,
                    span: Span::new(9, 11),
                    value: "==".to_string(),
                },
                TestResult {
                    token: Token::NoEq,
                    span: Span::new(12, 14),
                    value: "!=".to_string(),
                },
                TestResult {
                    token: Token::Plus,
                    span: Span::new(15, 16),
                    value: "+".to_string(),
                },
                TestResult {
                    token: Token::Minus,
                    span: Span::new(17, 18),
                    value: "-".to_string(),
                },
                TestResult {
                    token: Token::Star,
                    span: Span::new(19, 20),
                    value: "*".to_string(),
                },
                TestResult {
                    token: Token::Slash,
                    span: Span::new(21, 22),
                    value: "/".to_string(),
                },
                TestResult {
                    token: Token::Mod,
                    span: Span::new(23, 24),
                    value: "%".to_string(),
                },
                TestResult {
                    token: Token::Caret,
                    span: Span::new(25, 26),
                    value: "^".to_string(),
                },
                TestResult {
                    token: Token::Gt,
                    span: Span::new(27, 28),
                    value: ">".to_string(),
                },
                TestResult {
                    token: Token::GtEq,
                    span: Span::new(29, 31),
                    value: ">=".to_string(),
                },
                TestResult {
                    token: Token::Lt,
                    span: Span::new(32, 33),
                    value: "<".to_string(),
                },
                TestResult {
                    token: Token::LtEq,
                    span: Span::new(34, 36),
                    value: "<=".to_string(),
                },
                TestResult {
                    token: Token::Dots,
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
                    token: Token::Integer,
                    span: Span::new(0, 2),
                    value: "42".to_string(),
                },
                TestResult {
                    token: Token::Number,
                    span: Span::new(3, 7),
                    value: "3.14".to_string(),
                },
                TestResult {
                    token: Token::Integer,
                    span: Span::new(8, 16),
                    value: "0xABCDEF".to_string(),
                },
                TestResult {
                    token: Token::Integer,
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
                token: Token::String,
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
                token: Token::String,
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
                token: Token::EOL,
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
                token: Token::UnterminatedString,
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
                    token: Token::String,
                    span: Span::new(0, 7),
                    value: r#""Hello""#.to_string(),
                },
                TestResult {
                    token: Token::UnterminatedString,
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
                    token: Token::String,
                    span: Span::new(0, 12),
                    value: r#""escape \\a""#.to_string(),
                },
                TestResult {
                    token: Token::Dot,
                    span: Span::new(12, 13),
                    value: ".".to_string(),
                },
                TestResult {
                    token: Token::Identifier,
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
                    token: Token::UnexpectedCharacter,
                    span: Span::new(9, 10),
                    value: "a".to_string(),
                },
                TestResult {
                    token: Token::Dot,
                    span: Span::new(11, 12),
                    value: ".".to_string(),
                },
                TestResult {
                    token: Token::Identifier,
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
                    token: Token::UnexpectedCharacter,
                    span: Span::new(0, 1),
                    value: "@".to_string(),
                },
                TestResult {
                    token: Token::Identifier,
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
                    token: Token::UnexpectedCharacter,
                    span: Span::new(0, 4),
                    value: "🫵".to_string(),
                },
                TestResult {
                    token: Token::Equals,
                    span: Span::new(5, 6),
                    value: "=".to_string(),
                },
                TestResult {
                    token: Token::UnexpectedCharacter,
                    span: Span::new(7, 11),
                    value: "🎅".to_string(),
                },
                TestResult {
                    token: Token::UnexpectedCharacter,
                    span: Span::new(11, 15),
                    value: "🏾".to_string(),
                }
            ]
        );
    }
}
