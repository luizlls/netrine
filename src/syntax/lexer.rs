use super::node::SyntaxKind;

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

    pub fn size(&self) -> usize {
        self.index - self.start
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

    pub fn next(&mut self) -> SyntaxKind {
        self.align();

        let Some(chr) = self.curr
        else {
            return SyntaxKind::EOF;
        };

        let kind = match chr {
            b' ' | b'\t' | b'\r' | b'\n' => {
                return self.space();
            }
            b'a'..=b'z' |  b'A'..=b'Z' | b'_' => {
                return self.ident();
            }
            b'0'..=b'9' => {
                return self.number();
            }
            b'"' => {
                return self.string();
            }
            b'/' if self.peek() == Some(b'/') => {
                return self.comment();
            }
            _ if Self::is_symbol(self.curr) => {
                return self.operator();
            }
            b'(' => SyntaxKind::LParen,
            b'{' => SyntaxKind::LBrace,
            b'[' => SyntaxKind::LBracket,
            b')' => SyntaxKind::RParen,
            b'}' => SyntaxKind::RBrace,
            b']' => SyntaxKind::RBracket,
            b';' => SyntaxKind::Semi,
            b',' => SyntaxKind::Comma,
            _ => self.error("unexpected character"),
        };

        self.bump();

        kind
    }

    pub fn skip_trivia(&mut self) -> SyntaxKind {
        let mut next = self.next();

        while next.is_trivia() { next = self.next(); }

        next
    }

    pub fn lookahead(&mut self, nth: usize) -> SyntaxKind {
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

    fn ident(&mut self) -> SyntaxKind {
        self.bump_while(Self::is_ident);
        self.bump_while(|ch| matches!(ch, Some(b'?' | b'!' | b'\'')));
        
        match self.slice() {
            "and" => SyntaxKind::And,
            "break" => SyntaxKind::Break,
            "else" => SyntaxKind::Else,
            "if" => SyntaxKind::If,
            "import" => SyntaxKind::Import,
            "is" => SyntaxKind::Is,
            "not" => SyntaxKind::Not,
            "or" => SyntaxKind::Or,
            "then" => SyntaxKind::Then,
            "where"  => SyntaxKind::Where,
            "yield" => SyntaxKind::Yield,
            _ => SyntaxKind::Ident,
        }
    }

    fn operator(&mut self) -> SyntaxKind {
        let Some(chr) = self.curr
        else {
            return SyntaxKind::EOF;
        };

        let kind = match chr {
            b'+' => {
                let peek = self.peek();
                if Self::is_ident(peek) || Self::is_number(peek) || peek == Some(b'(') {
                    SyntaxKind::Pos
                } else {
                    SyntaxKind::Add
                }
            }
            b'-' => {
                let peek = self.peek();
                if peek == Some(b'>') {
                    self.bump();
                    SyntaxKind::Arrow
                } else if Self::is_ident(peek) || Self::is_number(peek) || peek == Some(b'(') {
                    SyntaxKind::Neg
                } else {
                    SyntaxKind::Sub
                }
            }
            b'.' => {
                if self.peek() == Some(b'.') {
                    self.bump();
                    SyntaxKind::Range
                } else {
                    SyntaxKind::Dot
                }
            }
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxKind::Eq
                } else {
                    SyntaxKind::Equals
                }
            }
            b':' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxKind::Walrus
                } else {
                    SyntaxKind::Colon
                }
            }
            b'<' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxKind::Le
                } else {
                    SyntaxKind::Lt
                }
            }
            b'>' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    SyntaxKind::Ge
                } else {
                    SyntaxKind::Gt
                }
            }
            b'!' if self.peek() == Some(b'=') => {
                self.bump();
                SyntaxKind::Ne
            }
            b'|' if self.peek() == Some(b'>') => {
                self.bump();
                SyntaxKind::Pipe
            }
            b'*' => SyntaxKind::Mul,
            b'/' => SyntaxKind::Div,
            b'^' => SyntaxKind::Exp,
            b'%' => SyntaxKind::Mod,
            _ => unreachable!()
        };

        self.bump();
        kind
    }

    fn number(&mut self) -> SyntaxKind {
        self.bump_while(Self::is_number);

        match self.curr {
            Some(b'.') if Self::is_number(self.peek()) => {
                self.bump();
                self.bump_while(Self::is_number);
            }
            Some(b'x' | b'X') if self.slice() == "0" => {
                self.bump();
                self.bump_while(Self::is_hex);
            }
            Some(b'b' | b'B') if self.slice() == "0" => {
                self.bump();
                self.bump_while(Self::is_bin);
            }
            _ => {}
        }

        SyntaxKind::Number
    }

    fn string(&mut self) -> SyntaxKind {
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

        self.value(string, SyntaxKind::String)
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

    fn comment(&mut self) -> SyntaxKind {
        self.bump_while(|ch| !matches!(ch, Some(b'\n') | None));
        SyntaxKind::Comment
    }

    fn space(&mut self) -> SyntaxKind {
        self.bump_while(|ch| matches!(ch, Some(b' ' | b'\t' | b'\n' | b'\r')));
        SyntaxKind::Space
    }

    fn bump_while<P>(&mut self, pred: P)
    where
        P: Fn(Option<u8>) -> bool,
    {
        while pred(self.curr) { self.bump(); }
    }

    fn error(&mut self, value: impl Into<String>) -> SyntaxKind {
        self.value = Some(value.into());
        SyntaxKind::Error
    }

    fn value(&mut self, value: impl Into<String>, kind: SyntaxKind) -> SyntaxKind {
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

        assert_syntax_kind!(lexer, SyntaxKind::Ident, "variable");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Ident, "True");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Ident, "CONST");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Ident, "_");
    }

    #[test]
    fn keywords() {
        let mut lexer = Lexer::new("and or is not if then else yield break where import");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::And);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Or);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Is);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Not);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::If);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Then);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Else);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Yield);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Break);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Where);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Import);
    }

    #[test]
    fn punctuation() {
        let mut lexer = Lexer::new(". , ; ( ) [ ] { }");
        assert_syntax_kind!(lexer, SyntaxKind::Dot);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Comma);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Semi);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::LParen);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::RParen);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::LBracket);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::RBracket);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::LBrace);
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::RBrace);
    }

    #[test]
    fn operators() {
        let mut lexer = Lexer::new(". -> : := = == != + - * / % ^ > < >= <= |> ..");
        assert_syntax_kind!(lexer, SyntaxKind::Dot, ".");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Arrow, "->");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Colon, ":");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Walrus, ":=");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Equals, "=");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Eq, "==");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Ne, "!=");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Add, "+");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Sub, "-");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Mul, "*");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Div, "/");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Mod, "%");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Exp, "^");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Gt, ">");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Lt, "<");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Ge, ">=");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Le, "<=");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Pipe, "|>");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Range, "..");
        lexer.next(); // space
    }

    #[test]
    fn number() {
        let mut lexer = Lexer::new("42 3.14 0xABCDEF 0b0101");

        assert_syntax_kind!(lexer, SyntaxKind::Number, "42");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Number, "3.14");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Number, "0xABCDEF");
        lexer.next(); // space
        assert_syntax_kind!(lexer, SyntaxKind::Number, "0b0101");
        lexer.next(); // space
    }

    #[test]
    fn simple_string() {
        let mut lexer = Lexer::new(r#""Hello, World""#);

        assert_syntax_kind!(lexer, SyntaxKind::String, "Hello, World");
    }

    #[test]
    fn nested_string() {
        let mut lexer = Lexer::new(r#""code = \"n = 42\"""#);

        assert_syntax_kind!(lexer, SyntaxKind::String, r#"code = "n = 42""#);
    }

    #[test]
    fn empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");

        assert_syntax_kind!(lexer, SyntaxKind::Space);
    }

    #[test]
    fn unterminated_string() {
        let mut lexer = Lexer::new(r#""Hello"#);

        assert_syntax_kind!(lexer, SyntaxKind::Error, "unterminated string");
    }

    #[test]
    fn unbalanced_quotes() {
        let mut lexer = Lexer::new(r#""Hello"""#);

        assert_syntax_kind!(lexer, SyntaxKind::String, "Hello");
        assert_syntax_kind!(lexer, SyntaxKind::Error, "unterminated string");
    }

    #[test]
    fn invalid_escaped_string() {
        let mut lexer = Lexer::new(r#""escape \a""#);

        assert_syntax_kind!(lexer, SyntaxKind::Error, "invalid escape character");
    }

    #[test]
    fn unexpected_character() {
        let mut lexer = Lexer::new("😀");

        assert_syntax_kind!(lexer, SyntaxKind::Error, "unexpected character");
    }

    #[test]
    fn lookahead() {
        let mut lexer = Lexer::new("a 1 c");

        assert_syntax_kind!(lexer, SyntaxKind::Ident, "a");

        assert_eq!(lexer.lookahead(0), SyntaxKind::Number);
        assert_eq!(lexer.lookahead(1), SyntaxKind::Ident);

        assert_syntax_kind!(lexer, SyntaxKind::Space);
        assert_syntax_kind!(lexer, SyntaxKind::Number, "1");
        assert_syntax_kind!(lexer, SyntaxKind::Space);
        assert_syntax_kind!(lexer, SyntaxKind::Ident, "c");
    }
}
