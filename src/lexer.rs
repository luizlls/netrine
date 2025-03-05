use crate::source::Span;

use super::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer<'l> {
    source: &'l str,
    bytes: &'l [u8],
    curr: u8,
    peek: u8,
    index: usize,
    start: usize,
}

impl<'l> Lexer<'l> {
    pub fn new(source: &'l str) -> Lexer<'l> {
        let mut lexer = Lexer {
            source,
            bytes: source.as_bytes(),
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

    fn slice(&self) -> &str {
        &self.source[self.start..self.index]
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

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = kind(self);
        if kind != TokenKind::EOF {
            Some(Token {
                kind,
                span: self.span(),
            })
        } else {
            None
        }
    }
}

fn kind(l: &mut Lexer) -> TokenKind {
    l.trivia();
    l.align();

    let kind = match l.curr {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
            return ident(l);
        }
        b'0'..=b'9' => {
            return number(l);
        }
        b'"' => {
            return string(l);
        }
        b'\n' => {
            return newline(l);
        }
        _ if is_symbol(l.curr) => {
            return operator(l);
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
    l.bump();
    kind
}

fn ident(l: &mut Lexer) -> TokenKind {
    l.bump_while(|chr| matches!(chr, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
    l.bump_while(|chr| chr == b'\'');

    let value = l.slice();

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

fn is_symbol(chr: u8) -> bool {
    matches!(
        chr,
        b'=' | b':' | b'.' | b'|' | b'<' | b'>' | b'!' | b'+' | b'-' | b'*' | b'/' | b'^' | b'%'
    )
}

fn operator(l: &mut Lexer) -> TokenKind {
    let kind = match l.curr {
        b'.' if l.peek == b'.' => {
            l.bump();
            TokenKind::Dots
        }
        b'.' => TokenKind::Dot,
        b'=' if l.peek == b'=' => {
            l.bump();
            TokenKind::EqEq
        }
        b'=' if l.peek == b'>' => {
            l.bump();
            TokenKind::Arrow
        }
        b'=' => TokenKind::Equals,
        b':' => TokenKind::Colon,
        b'<' if l.peek == b'=' => {
            l.bump();
            TokenKind::LtEq
        }
        b'<' => TokenKind::Lt,
        b'>' if l.peek == b'=' => {
            l.bump();
            TokenKind::GtEq
        }
        b'>' => TokenKind::Gt,
        b'!' if l.peek == b'=' => {
            l.bump();
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

    l.bump();
    kind
}

fn number(l: &mut Lexer) -> TokenKind {
    l.bump_while(|chr| chr.is_ascii_digit());

    match l.curr {
        b'.' if l.peek.is_ascii_digit() => {
            l.bump();
            l.bump_while(|chr| chr.is_ascii_digit());
        }
        b'b' | b'B' if l.slice() == "0" => {
            l.bump();
            l.bump_while(|chr| matches!(chr, b'0' | b'1'));
        }
        b'x' | b'X' if l.slice() == "0" => {
            l.bump();
            l.bump_while(|chr| chr.is_ascii_hexdigit());
        }
        _ => return TokenKind::Integer,
    }

    TokenKind::Number
}

fn string(l: &mut Lexer) -> TokenKind {
    loop {
        match l.bump() {
            b'\n' | 0 => {
                return TokenKind::UnterminatedString;
            }
            b'"' => {
                l.bump();
                break;
            }
            b'\\' => match l.bump() {
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

fn newline(l: &mut Lexer) -> TokenKind {
    while l.curr == b'\n' {
        l.bump();
        l.trivia();
    }
    TokenKind::NewLine
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_token {
        ($lexer: expr, $kind: expr) => {{
            assert_eq!($kind, kind(&mut $lexer));
        }};
        ($lexer: expr, $kind: expr, $value: expr) => {{
            assert_eq!($kind, kind(&mut $lexer));
            assert_eq!($value, $lexer.slice());
        }};
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("ident True CONST _ ___");

        assert_token!(lexer, TokenKind::Identifier, "ident");
        assert_token!(lexer, TokenKind::Identifier, "True");
        assert_token!(lexer, TokenKind::Identifier, "CONST");
        assert_token!(lexer, TokenKind::Underscore, "_");
        assert_token!(lexer, TokenKind::Identifier, "___");
    }

    #[test]
    fn keywords() {
        let mut lexer = Lexer::new("and or not");

        assert_token!(lexer, TokenKind::And);
        assert_token!(lexer, TokenKind::Or);
        assert_token!(lexer, TokenKind::Not);
    }

    #[test]
    fn punctuation() {
        let mut lexer = Lexer::new(". , ; ( ) [ ] { }");
        assert_token!(lexer, TokenKind::Dot);
        assert_token!(lexer, TokenKind::Comma);
        assert_token!(lexer, TokenKind::Semi);
        assert_token!(lexer, TokenKind::LParen);
        assert_token!(lexer, TokenKind::RParen);
        assert_token!(lexer, TokenKind::LBracket);
        assert_token!(lexer, TokenKind::RBracket);
        assert_token!(lexer, TokenKind::LBrace);
        assert_token!(lexer, TokenKind::RBrace);
    }

    #[test]
    fn operators() {
        let mut lexer = Lexer::new(". : => = == != + - * / % ^ > >= < <= ..");
        assert_token!(lexer, TokenKind::Dot, ".");
        assert_token!(lexer, TokenKind::Colon, ":");
        assert_token!(lexer, TokenKind::Arrow, "=>");
        assert_token!(lexer, TokenKind::Equals, "=");
        assert_token!(lexer, TokenKind::EqEq, "==");
        assert_token!(lexer, TokenKind::NoEq, "!=");
        assert_token!(lexer, TokenKind::Plus, "+");
        assert_token!(lexer, TokenKind::Minus, "-");
        assert_token!(lexer, TokenKind::Star, "*");
        assert_token!(lexer, TokenKind::Slash, "/");
        assert_token!(lexer, TokenKind::Mod, "%");
        assert_token!(lexer, TokenKind::Caret, "^");
        assert_token!(lexer, TokenKind::Gt, ">");
        assert_token!(lexer, TokenKind::GtEq, ">=");
        assert_token!(lexer, TokenKind::Lt, "<");
        assert_token!(lexer, TokenKind::LtEq, "<=");
        assert_token!(lexer, TokenKind::Dots, "..");
    }

    #[test]
    fn number() {
        let mut lexer = Lexer::new("42 3.14 0xABCDEF 0b0101");

        assert_token!(lexer, TokenKind::Integer, "42");
        assert_token!(lexer, TokenKind::Number, "3.14");
        assert_token!(lexer, TokenKind::Number, "0xABCDEF");
        assert_token!(lexer, TokenKind::Number, "0b0101");
    }

    #[test]
    fn simple_string() {
        let mut lexer = Lexer::new(r#""Hello, World""#);

        assert_token!(lexer, TokenKind::String, "\"Hello, World\"");
    }

    #[test]
    fn nested_string() {
        let mut lexer = Lexer::new(r#""code = \"n = 42\"""#);

        assert_token!(lexer, TokenKind::String, "\"code = \\\"n = 42\\\"\"");
    }

    #[test]
    fn empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");

        assert_token!(lexer, TokenKind::NewLine);
        assert_token!(lexer, TokenKind::EOF);
    }

    #[test]
    fn unterminated_string() {
        let mut lexer = Lexer::new(r#""Hello"#);

        assert_token!(lexer, TokenKind::UnterminatedString);
    }

    #[test]
    fn unbalanced_quotes() {
        let mut lexer = Lexer::new(r#""Hello"""#);

        assert_token!(lexer, TokenKind::String, "\"Hello\"");
        assert_token!(lexer, TokenKind::UnterminatedString);
    }

    #[test]
    fn invalid_escaped_string() {
        let mut lexer = Lexer::new(r#""escape \a""#);

        assert_token!(lexer, TokenKind::UnexpectedCharacter);
    }

    #[test]
    fn unexpected_character() {
        let mut lexer = Lexer::new("😀 = 10");

        assert_token!(lexer, TokenKind::UnexpectedCharacter);
    }
}
