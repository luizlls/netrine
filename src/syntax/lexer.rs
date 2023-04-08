use crate::span::Span;

use super::node::SyntaxToken;

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    raw: &'src [u8],
    curr: Option<u8>,
    value: Option<String>,
    start: usize,
    index: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Lexer {
        let mut lexer = Lexer {
            src,
            raw: src.as_bytes(),
            curr: None,
            value: None,
            start: 0,
            index: 0,
        };
        lexer.curr = lexer.raw.first().copied();
        lexer
    }

    pub fn slice(&self) -> &str {
        if let Some(ref value) = self.value {
            value
        } else {
            &self.src[self.start .. self.index]
        }
    }

    pub fn span(&self) -> Span {
        Span(self.start as u32, self.index as u32)
    }

    fn align(&mut self) {
        self.start = self.index;
        self.value = None;
    }

    fn bump(&mut self) -> Option<u8> {
        self.index += 1;
        self.curr = self.raw.get(self.index).copied();
        self.curr
    }

    fn peek(&self) -> Option<u8> {
        self.raw.get(self.index + 1).copied()
    }

    pub fn next(&mut self) -> SyntaxToken {
        self.align();

        let Some(chr) = self.curr
        else {
            return SyntaxToken::EOF;
        };

        let kind = match chr {
            b' ' | b'\t' | b'\r' | b'\n' => {
                return self.space();
            }
            b'a'..=b'z' | b'_' => {
                return self.ident();
            }
            b'A'..=b'Z' => {
                return self.upper();
            }
            b'0'..=b'9' => {
                return self.number(false);
            }
            b'+' | b'-' if Lexer::is_number(self.peek()) => {
                return self.number(true);
            }
            b'"' => {
                return self.string();
            }
            b'/' if self.peek() == Some(b'/') => {
                return self.comment();
            }
            _ if Lexer::is_symbol(self.curr) => {
                return self.operator();
            }
            b'(' => SyntaxToken::LParen,
            b'{' => SyntaxToken::LBrace,
            b'[' => SyntaxToken::LBracket,
            b')' => SyntaxToken::RParen,
            b'}' => SyntaxToken::RBrace,
            b']' => SyntaxToken::RBracket,
            b';' => SyntaxToken::Semi,
            b',' => SyntaxToken::Comma,
            _ => self.error("unexpected character"),
        };

        self.bump();

        kind
    }

    pub fn skip_trivia(&mut self) -> SyntaxToken {
        let mut next = self.next();

        while next.is_trivia() { next = self.next(); }

        next
    }

    pub fn lookahead(&mut self, nth: usize) -> SyntaxToken {
        let start = self.start;
        let index = self.index;
        let curr = self.curr;

        let mut total = 0;

        let mut next = self.skip_trivia();

        while total < nth {
            total += 1;
            next = self.skip_trivia();
        }

        self.curr = curr;
        self.start = start;
        self.index = index;

        next
    }

    fn ident(&mut self) -> SyntaxToken {
        self.bump_while(Lexer::is_ident);
        self.bump_while(|ch| matches!(ch, Some(b'?' | b'!' | b'\'')));

        match self.slice() {
            "and" => SyntaxToken::And,
            "break" => SyntaxToken::Break,
            "case" => SyntaxToken::Case,
            "else" => SyntaxToken::Else,
            "if" => SyntaxToken::If,
            "import" => SyntaxToken::Import,
            "is" => SyntaxToken::Is,
            "not" => SyntaxToken::Not,
            "or" => SyntaxToken::Or,
            "then" => SyntaxToken::Then,
            "where"  => SyntaxToken::Where,
            "yield" => SyntaxToken::Yield,
            _ => SyntaxToken::Lower,
        }
    }

    fn upper(&mut self) -> SyntaxToken {
        self.bump_while(Lexer::is_ident);

        let value = self.slice();

        if value.chars().all(|ch| matches!(ch, 'A'..='Z' | '_')) {
            SyntaxToken::Lower // CONSTANT CASE
        } else {
            SyntaxToken::Upper
        }
    }

    fn operator(&mut self) -> SyntaxToken {
        let Some(chr) = self.curr
        else {
            return SyntaxToken::EOF;
        };

        let kind = match chr {
            b'+' => {
                let peek = self.peek();
                if Lexer::is_ident(peek) || Lexer::is_number(peek) || peek == Some(b'(') {
                    SyntaxToken::Pos
                } else {
                    SyntaxToken::Add
                }
            }
            b'-' => {
                let peek = self.peek();
                if peek == Some(b'>') {
                    self.bump();
                    SyntaxToken::Arrow
                } else if Lexer::is_ident(peek) || Lexer::is_number(peek) || peek == Some(b'(') {
                    SyntaxToken::Neg
                } else {
                    SyntaxToken::Sub
                }
            }
            b'.' => {
                if self.peek() == Some(b'.') {
                    self.bump();
                    SyntaxToken::Range
                } else {
                    SyntaxToken::Dot
                }
            }
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxToken::Eq
                } else {
                    SyntaxToken::Equals
                }
            }
            b':' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxToken::Walrus
                } else {
                    SyntaxToken::Colon
                }
            }
            b'<' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxToken::Le
                } else {
                    SyntaxToken::Lt
                }
            }
            b'>' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxToken::Ge
                } else {
                    SyntaxToken::Gt
                }
            }
            b'!' if self.peek() == Some(b'=') => {
                self.bump();
                SyntaxToken::Ne
            }
            b'|' if self.peek() == Some(b'>') => {
                self.bump();
                SyntaxToken::Pipe
            }
            b'*' => SyntaxToken::Mul,
            b'/' => SyntaxToken::Div,
            b'^' => SyntaxToken::Exp,
            b'%' => SyntaxToken::Mod,
            _ => unreachable!()
        };

        self.bump();
        kind
    }

    fn number(&mut self, prefix: bool) -> SyntaxToken {
        if prefix {
            self.bump();
        }

        self.bump_while(Lexer::is_number);

        match self.curr {
            Some(b'.') if Lexer::is_number(self.peek()) => {
                self.bump();
                self.bump_while(Lexer::is_number);
            }
            Some(b'x' | b'X') if self.slice() == "0" => {
                self.bump();
                self.bump_while(Lexer::is_hex);
            }
            Some(b'b' | b'B') if self.slice() == "0" => {
                self.bump();
                self.bump_while(Lexer::is_bin);
            }
            _ => {}
        }

        SyntaxToken::Number
    }

    fn string(&mut self) -> SyntaxToken {
        let mut string = String::new();

        loop {
            match self.bump() {
                Some(b'\n')
              | None => {
                    return self.error("unterminated string");
                }
                Some(b'"') => {
                    self.bump();
                    break;
                }
                Some(b'\\') => {
                    match self.bump() {
                        Some(b'\\') => string.push('\\'),
                        Some(b'n')  => string.push('\n'),
                        Some(b'r')  => string.push('\r'),
                        Some(b't')  => string.push('\t'),
                        Some(b'"')  => string.push('"'),
                        Some(b'0')  => string.push(0 as char),
                        Some(b'u')  => todo!("Validate escaped unicode"),
                        Some(b'x')  => todo!("Validate escaped binary"),
                        _ => {
                            return self.error("invalid escape character");
                        }
                    }
                }
                Some(chr) => {
                    string.push(chr as char);
                }
            }
        }

        self.value(string, SyntaxToken::String)
    }

    fn is_number(ch: Option<u8>) -> bool {
        matches!(ch, Some(b'0'..=b'9'))
    }

    fn is_hex(ch: Option<u8>) -> bool {
        matches!(ch, Some(b'a'..=b'f' | b'A'..=b'F' | b'0'..=b'9'))
    }

    fn is_bin(ch: Option<u8>) -> bool {
        matches!(ch, Some(b'0'..=b'1'))
    }

    fn is_symbol(ch: Option<u8>) -> bool {
        matches!(ch, Some(b'=' | b':' | b'.' | b'|' | b'<' | b'>' | b'!' |
                          b'+' | b'-' | b'*' | b'/' | b'^' | b'%'))
    }

    fn is_ident(ch: Option<u8>) -> bool {
        matches!(ch, Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'))
    }

    fn comment(&mut self) -> SyntaxToken {
        self.bump_while(|ch| !matches!(ch, Some(b'\n') | None));
        SyntaxToken::Comment
    }

    fn space(&mut self) -> SyntaxToken {
        self.bump_while(|ch| matches!(ch, Some(b' ' | b'\t' | b'\n' | b'\r')));
        self.next()
    }

    fn bump_while<P>(&mut self, pred: P)
    where
        P: Fn(Option<u8>) -> bool,
    {
        while pred(self.curr) { self.bump(); }
    }

    fn error(&mut self, value: impl Into<String>) -> SyntaxToken {
        self.value = Some(value.into());
        SyntaxToken::Error
    }

    fn value(&mut self, value: impl Into<String>, kind: SyntaxToken) -> SyntaxToken {
        self.value = Some(value.into());
        kind
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_syntax_kind {
        ($lexer: expr, $kind: expr) => {{
            assert_eq!($lexer.next(), $kind);
        }};
        ($lexer: expr, $kind: expr, $value: expr) => {{
            assert_eq!($lexer.next(), $kind);
            assert_eq!($lexer.slice(), $value);
        }};
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("variable True CONST _");

        assert_syntax_kind!(lexer, SyntaxToken::Lower, "variable");
        assert_syntax_kind!(lexer, SyntaxToken::Upper, "True");
        assert_syntax_kind!(lexer, SyntaxToken::Lower, "CONST");
        assert_syntax_kind!(lexer, SyntaxToken::Lower, "_");
    }

    #[test]
    fn keywords() {
        let mut lexer = Lexer::new("and or is not if then else case yield break where import");

        assert_syntax_kind!(lexer, SyntaxToken::And);
        assert_syntax_kind!(lexer, SyntaxToken::Or);
        assert_syntax_kind!(lexer, SyntaxToken::Is);
        assert_syntax_kind!(lexer, SyntaxToken::Not);
        assert_syntax_kind!(lexer, SyntaxToken::If);
        assert_syntax_kind!(lexer, SyntaxToken::Then);
        assert_syntax_kind!(lexer, SyntaxToken::Else);
        assert_syntax_kind!(lexer, SyntaxToken::Case);
        assert_syntax_kind!(lexer, SyntaxToken::Yield);
        assert_syntax_kind!(lexer, SyntaxToken::Break);
        assert_syntax_kind!(lexer, SyntaxToken::Where);
        assert_syntax_kind!(lexer, SyntaxToken::Import);
    }

    #[test]
    fn punctuation() {
        let mut lexer = Lexer::new(". , ; ( ) [ ] { }");
        assert_syntax_kind!(lexer, SyntaxToken::Dot);
        assert_syntax_kind!(lexer, SyntaxToken::Comma);
        assert_syntax_kind!(lexer, SyntaxToken::Semi);
        assert_syntax_kind!(lexer, SyntaxToken::LParen);
        assert_syntax_kind!(lexer, SyntaxToken::RParen);
        assert_syntax_kind!(lexer, SyntaxToken::LBracket);
        assert_syntax_kind!(lexer, SyntaxToken::RBracket);
        assert_syntax_kind!(lexer, SyntaxToken::LBrace);
        assert_syntax_kind!(lexer, SyntaxToken::RBrace);
    }

    #[test]
    fn operators() {
        let mut lexer = Lexer::new(". -> : := = == != + - * / % ^ > < >= <= |> ..");
        assert_syntax_kind!(lexer, SyntaxToken::Dot, ".");
        assert_syntax_kind!(lexer, SyntaxToken::Arrow, "->");
        assert_syntax_kind!(lexer, SyntaxToken::Colon, ":");
        assert_syntax_kind!(lexer, SyntaxToken::Walrus, ":=");
        assert_syntax_kind!(lexer, SyntaxToken::Equals, "=");
        assert_syntax_kind!(lexer, SyntaxToken::Eq, "==");
        assert_syntax_kind!(lexer, SyntaxToken::Ne, "!=");
        assert_syntax_kind!(lexer, SyntaxToken::Add, "+");
        assert_syntax_kind!(lexer, SyntaxToken::Sub, "-");
        assert_syntax_kind!(lexer, SyntaxToken::Mul, "*");
        assert_syntax_kind!(lexer, SyntaxToken::Div, "/");
        assert_syntax_kind!(lexer, SyntaxToken::Mod, "%");
        assert_syntax_kind!(lexer, SyntaxToken::Exp, "^");
        assert_syntax_kind!(lexer, SyntaxToken::Gt, ">");
        assert_syntax_kind!(lexer, SyntaxToken::Lt, "<");
        assert_syntax_kind!(lexer, SyntaxToken::Ge, ">=");
        assert_syntax_kind!(lexer, SyntaxToken::Le, "<=");
        assert_syntax_kind!(lexer, SyntaxToken::Pipe, "|>");
        assert_syntax_kind!(lexer, SyntaxToken::Range, "..");
    }

    #[test]
    fn number() {
        let mut lexer = Lexer::new("42 3.14 0xABCDEF 0b0101");

        assert_syntax_kind!(lexer, SyntaxToken::Number, "42");
        assert_syntax_kind!(lexer, SyntaxToken::Number, "3.14");
        assert_syntax_kind!(lexer, SyntaxToken::Number, "0xABCDEF");
        assert_syntax_kind!(lexer, SyntaxToken::Number, "0b0101");
    }

    #[test]
    fn simple_string() {
        let mut lexer = Lexer::new(r#""Hello, World""#);

        assert_syntax_kind!(lexer, SyntaxToken::String, "Hello, World");
    }

    #[test]
    fn nested_string() {
        let mut lexer = Lexer::new(r#""code = \"n = 42\"""#);

        assert_syntax_kind!(lexer, SyntaxToken::String, r#"code = "n = 42""#);
    }

    #[test]
    fn empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");

        assert_syntax_kind!(lexer, SyntaxToken::EOF);
    }

    #[test]
    fn unterminated_string() {
        let mut lexer = Lexer::new(r#""Hello"#);

        assert_syntax_kind!(lexer, SyntaxToken::Error, "unterminated string");
    }

    #[test]
    fn unbalanced_quotes() {
        let mut lexer = Lexer::new(r#""Hello"""#);

        assert_syntax_kind!(lexer, SyntaxToken::String, "Hello");
        assert_syntax_kind!(lexer, SyntaxToken::Error, "unterminated string");
    }

    #[test]
    fn invalid_escaped_string() {
        let mut lexer = Lexer::new(r#""escape \a""#);

        assert_syntax_kind!(lexer, SyntaxToken::Error, "invalid escape character");
    }

    #[test]
    fn unexpected_character() {
        let mut lexer = Lexer::new("😀");

        assert_syntax_kind!(lexer, SyntaxToken::Error, "unexpected character");
    }

    #[test]
    fn lookahead() {
        let mut lexer = Lexer::new("a 1 c");

        assert_syntax_kind!(lexer, SyntaxToken::Lower, "a");
        assert_eq!(lexer.lookahead(0), SyntaxToken::Number);
        assert_eq!(lexer.lookahead(1), SyntaxToken::Lower);
        assert_syntax_kind!(lexer, SyntaxToken::Number, "1");
        assert_syntax_kind!(lexer, SyntaxToken::Lower, "c");
    }
}
