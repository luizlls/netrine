use crate::span::Span;

use super::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    raw: &'src [u8],
    curr: u8,
    peek: u8,
    index: usize,
    start: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Lexer {
        let mut lexer = Lexer {
            src,
            raw: src.as_bytes(),
            curr: 0,
            peek: 0,
            index: 0,
            start: 0,
        };
        lexer.curr = lexer.nth(0);
        lexer.peek = lexer.nth(1);
        lexer
    }

    fn align(&mut self) {
        self.start = self.index;
    }

    fn bump(&mut self) -> u8 {
        self.index += 1;
        self.curr = self.peek;
        self.peek = self.nth(self.index + 1);
        self.curr
    }

    fn nth(&self, idx: usize) -> u8 {
        if self.raw.len() > idx { self.raw[idx] } else { b'\0' }
    }

    fn slice(&self) -> &str {
        &self.src[self.start .. self.index]
    }

    fn span(&self) -> Span {
        Span(self.start as u32, self.index as u32)
    }

    fn bump_while<P>(&mut self, pred: P)
    where
        P: Fn(u8) -> bool,
    {
        while pred(self.curr) { self.bump(); }
    }
}

pub fn token(l: &mut Lexer) -> Token {
    let kind = kind(l);
    let span = l.span();
    Token { kind, span }
}

fn kind(l: &mut Lexer) -> TokenKind {
    trivia(l);
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
        b'/' if l.peek == b'/' => {
            return comment(l);
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
    l.bump_while(|chr| chr.is_ascii_alphanumeric() || chr == b'_');
    l.bump_while(|chr| chr == b'\'');

    match l.slice() {
        "and" => TokenKind::And,
        "or" => TokenKind::Or,
        "not" => TokenKind::Not,
        "is" => TokenKind::Is,
        "where" => TokenKind::Where,
        "import" => TokenKind::Import,
        _ => TokenKind::Ident,
    }
}

fn is_symbol(chr: u8) -> bool {
    matches!(chr, b'=' | b':' | b'.' | b'|' | b'<' | b'>' | b'!' | b'+' | b'-' | b'*' | b'/' | b'^' | b'%')
}

fn operator(l: &mut Lexer) -> TokenKind {
    let kind = match l.curr {
        b'.' if l.peek == b'.' => {
            l.bump();
            TokenKind::DotDot
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
        b':' if l.peek == b'=' => {
            l.bump();
            TokenKind::Walrus
        }
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
        b'|' if l.peek == b'>' => {
            l.bump();
            TokenKind::Pipe
        }
        b'+' => TokenKind::Plus,
        b'-' => TokenKind::Minus,
        b'*' => TokenKind::Star,
        b'/' => TokenKind::Slash,
        b'^' => TokenKind::Caret,
        b'%' => TokenKind::Mod,
        _ => unreachable!()
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
        _ => {}
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
            b'\\' => {
                match l.bump() {
                    b'\\' | b'n' | b'r' | b't' | b'"' | b'0' => {}, 
                    b'u' => todo!("validate escaped unicode"),
                    b'x' => todo!("validate escaped binary"),
                    _ => {
                        return TokenKind::UnexpectedCharacter;
                    }
                }
            }
            _ => {}
        }
    }

    TokenKind::String
}

fn newline(l: &mut Lexer) -> TokenKind {
    trivia(l);
    TokenKind::EOL
}

fn comment(l: &mut Lexer) -> TokenKind {
    l.bump_while(|chr| chr != b'\n' && chr != 0);
    kind(l)
}

fn trivia(l: &mut Lexer) {
    l.bump_while(|chr| chr.is_ascii_whitespace());
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
        let mut lexer = Lexer::new("variable True CONST _");

        assert_token!(lexer, TokenKind::Ident, "variable");
        assert_token!(lexer, TokenKind::Ident, "True");
        assert_token!(lexer, TokenKind::Ident, "CONST");
        assert_token!(lexer, TokenKind::Ident, "_");
    }

    #[test]
    fn keywords() {
        let mut lexer = Lexer::new("and or not is function if else when case where import");

        assert_token!(lexer, TokenKind::And);
        assert_token!(lexer, TokenKind::Or);
        assert_token!(lexer, TokenKind::Not);
        assert_token!(lexer, TokenKind::Is);
        assert_token!(lexer, TokenKind::Where);
        assert_token!(lexer, TokenKind::Import);
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
        let mut lexer = Lexer::new(". => : = := |> == != + - * / % ^ > >= < <= ..");
        assert_token!(lexer, TokenKind::Dot, ".");
        assert_token!(lexer, TokenKind::Arrow, "=>");
        assert_token!(lexer, TokenKind::Colon, ":");
        assert_token!(lexer, TokenKind::Equals, "=");
        assert_token!(lexer, TokenKind::Walrus, ":=");
        assert_token!(lexer, TokenKind::Pipe, "|>");
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
        assert_token!(lexer, TokenKind::DotDot, "..");
    }

    #[test]
    fn number() {
        let mut lexer = Lexer::new("42 3.14 0xABCDEF 0b0101");

        assert_token!(lexer, TokenKind::Number, "42");
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
