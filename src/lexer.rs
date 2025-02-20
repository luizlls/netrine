use crate::source::Span;

use super::token::Token;

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
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = kind(self);
        if kind != Token::EOF {
            Some((kind, self.span()))
        } else {
            None
        }
    }
}

fn kind(l: &mut Lexer) -> Token {
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
        b'(' => Token::LParen,
        b')' => Token::RParen,
        b'{' => Token::LBrace,
        b'}' => Token::RBrace,
        b'[' => Token::LBracket,
        b']' => Token::RBracket,
        b';' => Token::Semi,
        b',' => Token::Comma,
        0 => Token::EOF,
        _ => Token::UnexpectedCharacter,
    };
    l.bump();
    kind
}

fn ident(l: &mut Lexer) -> Token {
    l.bump_while(|chr| matches!(chr, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
    l.bump_while(|chr| chr == b'\'');

    let value = l.slice();

    match value {
        "and" => Token::And,
        "or" => Token::Or,
        "not" => Token::Not,
        _ => {
            if value == "_" {
                Token::Underscore
            } else {
                Token::Identifier
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

fn operator(l: &mut Lexer) -> Token {
    let kind = match l.curr {
        b'.' if l.peek == b'.' => {
            l.bump();
            Token::Dots
        }
        b'.' => Token::Dot,
        b'=' if l.peek == b'=' => {
            l.bump();
            Token::EqEq
        }
        b'=' if l.peek == b'>' => {
            l.bump();
            Token::Arrow
        }
        b'=' => Token::Equals,
        b':' => Token::Colon,
        b'<' if l.peek == b'=' => {
            l.bump();
            Token::LtEq
        }
        b'<' => Token::Lt,
        b'>' if l.peek == b'=' => {
            l.bump();
            Token::GtEq
        }
        b'>' => Token::Gt,
        b'!' if l.peek == b'=' => {
            l.bump();
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

    l.bump();
    kind
}

fn number(l: &mut Lexer) -> Token {
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
        _ => return Token::Integer,
    }

    Token::Number
}

fn string(l: &mut Lexer) -> Token {
    loop {
        match l.bump() {
            b'\n' | 0 => {
                return Token::UnterminatedString;
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
                    return Token::UnexpectedCharacter;
                }
            },
            _ => {}
        }
    }

    Token::String
}

fn newline(l: &mut Lexer) -> Token {
    while l.curr == b'\n' {
        l.bump();
        l.trivia();
    }
    Token::NewLine
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

        assert_token!(lexer, Token::Identifier, "ident");
        assert_token!(lexer, Token::Identifier, "True");
        assert_token!(lexer, Token::Identifier, "CONST");
        assert_token!(lexer, Token::Underscore, "_");
        assert_token!(lexer, Token::Identifier, "___");
    }

    #[test]
    fn keywords() {
        let mut lexer = Lexer::new("and or not");

        assert_token!(lexer, Token::And);
        assert_token!(lexer, Token::Or);
        assert_token!(lexer, Token::Not);
    }

    #[test]
    fn punctuation() {
        let mut lexer = Lexer::new(". , ; ( ) [ ] { }");
        assert_token!(lexer, Token::Dot);
        assert_token!(lexer, Token::Comma);
        assert_token!(lexer, Token::Semi);
        assert_token!(lexer, Token::LParen);
        assert_token!(lexer, Token::RParen);
        assert_token!(lexer, Token::LBracket);
        assert_token!(lexer, Token::RBracket);
        assert_token!(lexer, Token::LBrace);
        assert_token!(lexer, Token::RBrace);
    }

    #[test]
    fn operators() {
        let mut lexer = Lexer::new(". : => = == != + - * / % ^ > >= < <= ..");
        assert_token!(lexer, Token::Dot, ".");
        assert_token!(lexer, Token::Colon, ":");
        assert_token!(lexer, Token::Arrow, "=>");
        assert_token!(lexer, Token::Equals, "=");
        assert_token!(lexer, Token::EqEq, "==");
        assert_token!(lexer, Token::NoEq, "!=");
        assert_token!(lexer, Token::Plus, "+");
        assert_token!(lexer, Token::Minus, "-");
        assert_token!(lexer, Token::Star, "*");
        assert_token!(lexer, Token::Slash, "/");
        assert_token!(lexer, Token::Mod, "%");
        assert_token!(lexer, Token::Caret, "^");
        assert_token!(lexer, Token::Gt, ">");
        assert_token!(lexer, Token::GtEq, ">=");
        assert_token!(lexer, Token::Lt, "<");
        assert_token!(lexer, Token::LtEq, "<=");
        assert_token!(lexer, Token::Dots, "..");
    }

    #[test]
    fn number() {
        let mut lexer = Lexer::new("42 3.14 0xABCDEF 0b0101");

        assert_token!(lexer, Token::Integer, "42");
        assert_token!(lexer, Token::Number, "3.14");
        assert_token!(lexer, Token::Number, "0xABCDEF");
        assert_token!(lexer, Token::Number, "0b0101");
    }

    #[test]
    fn simple_string() {
        let mut lexer = Lexer::new(r#""Hello, World""#);

        assert_token!(lexer, Token::String, "\"Hello, World\"");
    }

    #[test]
    fn nested_string() {
        let mut lexer = Lexer::new(r#""code = \"n = 42\"""#);

        assert_token!(lexer, Token::String, "\"code = \\\"n = 42\\\"\"");
    }

    #[test]
    fn empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");

        assert_token!(lexer, Token::NewLine);
        assert_token!(lexer, Token::EOF);
    }

    #[test]
    fn unterminated_string() {
        let mut lexer = Lexer::new(r#""Hello"#);

        assert_token!(lexer, Token::UnterminatedString);
    }

    #[test]
    fn unbalanced_quotes() {
        let mut lexer = Lexer::new(r#""Hello"""#);

        assert_token!(lexer, Token::String, "\"Hello\"");
        assert_token!(lexer, Token::UnterminatedString);
    }

    #[test]
    fn invalid_escaped_string() {
        let mut lexer = Lexer::new(r#""escape \a""#);

        assert_token!(lexer, Token::UnexpectedCharacter);
    }

    #[test]
    fn unexpected_character() {
        let mut lexer = Lexer::new("😀 = 10");

        assert_token!(lexer, Token::UnexpectedCharacter);
    }
}
