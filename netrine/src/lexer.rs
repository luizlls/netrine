use crate::source::{Source, Span};

use crate::token::{Token, TokenKind};

#[derive(Debug, Clone)]
struct Lexer<'src> {
    source: &'src Source<'src>,
    bytes: &'src [u8],
    curr: u8,
    peek: u8,
    index: usize,
    start: usize,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src Source) -> Lexer<'src> {
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
        if idx < self.bytes.len() { self.bytes[idx] } else { b'\0' }
    }

    fn slice(&self) -> &'src str {
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

    fn kind(&mut self) -> TokenKind {
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
            _ => TokenKind::UnexpectedCharacter,
        };
        self.bump();
        kind
    }

    fn ident(&mut self) -> TokenKind {
        self.bump_while(|chr| matches!(chr, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
        self.bump_while(|chr| chr == b'\'');

        let value = self.slice();

        match value {
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            _ => {
                if value == "_" {
                    TokenKind::Underscore
                } else {
                    TokenKind::Identifier
                }
            }
        }
    }

    #[rustfmt::skip]
    fn is_symbol(&self, chr: u8) -> bool {
        matches!(
            chr,
            b'=' | b':' | b'.' | b'|' | b'<' | b'>' | b'!' | b'+' | b'-' | b'*' | b'/' | b'^' | b'%'
        )
    }

    fn operator(&mut self) -> TokenKind {
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
        kind
    }

    fn number(&mut self) -> TokenKind {
        self.bump_while(|chr| chr.is_ascii_digit());

        match self.curr {
            b'.' if self.peek.is_ascii_digit() => {
                self.bump();
                self.bump_while(|chr| chr.is_ascii_digit());

                return TokenKind::Number;
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

        TokenKind::Integer
    }

    fn string(&mut self) -> TokenKind {
        loop {
            match self.bump() {
                b'\n' | 0 => {
                    return TokenKind::UnterminatedString;
                }
                b'"' => {
                    self.bump();
                    break;
                }
                b'\\' => match self.bump() {
                    b'\\' | b'n' | b'r' | b't' | b'"' | b'0' => {}
                    b'u' => todo!("validate escaped unicode"),
                    b'x' => todo!("validate escaped binary"),
                    _ => {
                        return TokenKind::UnexpectedCharacter;
                    }
                },
                _ => {}
            }
        }

        TokenKind::String
    }

    fn newline(&mut self) -> TokenKind {
        while self.curr == b'\n' {
            self.bump();
            self.trivia();
        }
        TokenKind::EOL
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

    fn token(&mut self) -> Token {
        Token {
            kind: self.kind(),
            span: self.span(),
        }
    }

    fn value(&self, span: Span) -> &str {
        &self.source.content[span.range()]
    }
}

#[derive(Debug, Clone)]
pub struct Tokens<'src> {
    lexer: Lexer<'src>,
    prev: Option<Token>,
    peek: Token,
    token: Token,
}

impl<'src> Tokens<'src> {
    pub fn new(source: &'src Source) -> Tokens<'src> {
        Tokens {
            lexer: Lexer::new(source),
            prev: None,
            peek: Token::default(),
            token: Token::default(),
        }
        .init()
    }

    fn init(mut self) -> Tokens<'src> {
        self.bump();
        self.bump();
        self
    }

    pub fn bump(&mut self) {
        self.prev = Some(self.token);
        self.token = self.peek;
        self.peek = self.lexer.token();
    }

    #[inline]
    pub fn token(&self) -> Token {
        self.token
    }

    #[inline]
    pub fn prev(&self) -> Option<Token> {
        self.prev
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

impl<'s> Iterator for Tokens<'s> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.token;
        self.bump();
        if token.kind != TokenKind::EOF { Some(token) } else { None }
    }
}

pub fn tokens<'s>(source: &'s Source) -> Tokens<'s> {
    Tokens::new(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_token {
        ($tokens: expr, $kind: expr) => {{
            assert_eq!($kind, $tokens.token().kind);
            $tokens.bump();
        }};
        ($tokens: expr, $kind: expr, $value: expr) => {{
            assert_eq!($kind, $tokens.token().kind);
            assert_eq!($value, $tokens.value($tokens.token()));
            $tokens.bump();
        }};
    }

    #[test]
    fn identifier() {
        let source = Source::new("<test>".to_string(), "ident test_1 True CONST _ ___");
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::Identifier, "ident");
        assert_token!(tokens, TokenKind::Identifier, "test_1");
        assert_token!(tokens, TokenKind::Identifier, "True");
        assert_token!(tokens, TokenKind::Identifier, "CONST");
        assert_token!(tokens, TokenKind::Underscore, "_");
        assert_token!(tokens, TokenKind::Identifier, "___");
    }

    #[test]
    fn keywords() {
        let source = Source::new("<test>".to_string(), "and or not");
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::And);
        assert_token!(tokens, TokenKind::Or);
        assert_token!(tokens, TokenKind::Not);
    }

    #[test]
    fn punctuation() {
        let source = Source::new("<test>".to_string(), ". , ; ( ) [ ] { }");
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::Dot);
        assert_token!(tokens, TokenKind::Comma);
        assert_token!(tokens, TokenKind::Semi);
        assert_token!(tokens, TokenKind::LParen);
        assert_token!(tokens, TokenKind::RParen);
        assert_token!(tokens, TokenKind::LBracket);
        assert_token!(tokens, TokenKind::RBracket);
        assert_token!(tokens, TokenKind::LBrace);
        assert_token!(tokens, TokenKind::RBrace);
    }

    #[test]
    fn operators() {
        let source = Source::new("<test>".to_string(), ". : => = == != + - * / % ^ > >= < <= ..");
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::Dot, ".");
        assert_token!(tokens, TokenKind::Colon, ":");
        assert_token!(tokens, TokenKind::Arrow, "=>");
        assert_token!(tokens, TokenKind::Equals, "=");
        assert_token!(tokens, TokenKind::EqEq, "==");
        assert_token!(tokens, TokenKind::NoEq, "!=");
        assert_token!(tokens, TokenKind::Plus, "+");
        assert_token!(tokens, TokenKind::Minus, "-");
        assert_token!(tokens, TokenKind::Star, "*");
        assert_token!(tokens, TokenKind::Slash, "/");
        assert_token!(tokens, TokenKind::Mod, "%");
        assert_token!(tokens, TokenKind::Caret, "^");
        assert_token!(tokens, TokenKind::Gt, ">");
        assert_token!(tokens, TokenKind::GtEq, ">=");
        assert_token!(tokens, TokenKind::Lt, "<");
        assert_token!(tokens, TokenKind::LtEq, "<=");
        assert_token!(tokens, TokenKind::Dots, "..");
    }

    #[test]
    fn number() {
        let source = Source::new("<test>".to_string(), "42 3.14 0xABCDEF 0b0101");
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::Integer, "42");
        assert_token!(tokens, TokenKind::Number, "3.14");
        assert_token!(tokens, TokenKind::Integer, "0xABCDEF");
        assert_token!(tokens, TokenKind::Integer, "0b0101");
    }

    #[test]
    fn simple_string() {
        let source = Source::new("<test>".to_string(), r#""Hello, World""#);
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::String, "\"Hello, World\"");
    }

    #[test]
    fn nested_string() {
        let source = Source::new("<test>".to_string(), r#""code = \"n = 42\"""#);
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::String, "\"code = \\\"n = 42\\\"\"");
    }

    #[test]
    fn empty_lines() {
        let source = Source::new("<test>".to_string(), "\n\n\n");
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::EOL);
        assert_token!(tokens, TokenKind::EOF);
    }

    #[test]
    fn unterminated_string() {
        let source = Source::new("<test>".to_string(), r#""Hello"#);
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::UnterminatedString);
    }

    #[test]
    fn unbalanced_quotes() {
        let source = Source::new("<test>".to_string(), r#""Hello"""#);
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::String, "\"Hello\"");
        assert_token!(tokens, TokenKind::UnterminatedString);
    }

    #[test]
    fn invalid_escaped_string() {
        let source = Source::new("<test>".to_string(), r#""escape \a""#);
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::UnexpectedCharacter);
    }

    #[test]
    fn unexpected_character() {
        let source = Source::new("<test>".to_string(), "😀 = 10");
        let mut tokens = tokens(&source);

        assert_token!(tokens, TokenKind::UnexpectedCharacter);
    }

    #[test]
    fn peek_prev() {
        let source = Source::new("<test>".to_string(), "text 3.14 _");
        let mut tokens = tokens(&source);

        assert_eq!(tokens.token().kind, TokenKind::Identifier);
        assert_eq!(tokens.value(tokens.token()), "text");

        assert_eq!(tokens.peek().kind, TokenKind::Number);
        assert_eq!(tokens.value(tokens.peek()), "3.14");

        tokens.bump();

        assert_eq!(tokens.token().kind, TokenKind::Number);
        assert_eq!(tokens.value(tokens.token()), "3.14");

        assert_eq!(tokens.peek().kind, TokenKind::Underscore);
        assert_eq!(tokens.value(tokens.peek()), "_");

        assert_eq!(tokens.prev().unwrap().kind, TokenKind::Identifier);
        assert_eq!(tokens.value(tokens.prev().unwrap()), "text");
    }
}
