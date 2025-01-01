use crate::span::Span;

use super::token::{
    Token,
    TokenKind::{self, *},
};

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
    pub fn new(src: &'src str) -> Lexer<'src> {
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
        if self.raw.len() > idx {
            self.raw[idx]
        } else {
            b'\0'
        }
    }

    fn slice(&self) -> &str {
        &self.src[self.start..self.index]
    }

    fn span(&self) -> Span {
        Span {
            lo: self.start as u32,
            hi: self.index as u32,
        }
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
        b'(' => LParen,
        b')' => RParen,
        b'{' => LBrace,
        b'}' => RBrace,
        b'[' => LBracket,
        b']' => RBracket,
        b';' => Semi,
        b',' => Comma,
        0 => EOF,
        _ => UnexpectedCharacter,
    };
    l.bump();
    kind
}

fn ident(l: &mut Lexer) -> TokenKind {
    l.bump_while(|chr| matches!(chr, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
    l.bump_while(|chr| chr == b'\'');

    let value = l.slice();

    match value {
        "and" => And,
        "or" => Or,
        "not" => Not,
        "is" => Is,
        "mut" => Mut,
        "if" => If,
        "else" => Else,
        "case" => Case,
        "import" => Import,
        "where" => Where,
        _ => {
            if value.as_bytes().iter().all(|&chr| chr == b'_') {
                Underscore
            } else {
                Identifier
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
            Dots
        }
        b'.' => Dot,
        b'=' if l.peek == b'=' => {
            l.bump();
            EqEq
        }
        b'=' if l.peek == b'>' => {
            l.bump();
            Arrow
        }
        b'=' => Equals,
        b':' => Colon,
        b'<' if l.peek == b'=' => {
            l.bump();
            LtEq
        }
        b'<' => Lt,
        b'>' if l.peek == b'=' => {
            l.bump();
            GtEq
        }
        b'>' => Gt,
        b'!' if l.peek == b'=' => {
            l.bump();
            NoEq
        }
        b'+' => Plus,
        b'-' => Minus,
        b'*' => Star,
        b'/' => Slash,
        b'^' => Caret,
        b'%' => Mod,
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
        _ => return Integer,
    }

    Number
}

fn string(l: &mut Lexer) -> TokenKind {
    loop {
        match l.bump() {
            b'\n' | 0 => {
                return UnterminatedString;
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
                    return UnexpectedCharacter;
                }
            },
            _ => {}
        }
    }

    String
}

fn newline(l: &mut Lexer) -> TokenKind {
    while l.curr == b'\n' {
        l.bump();
        l.trivia();
    }
    NewLine
}

pub fn tokens(src: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(src);
    let mut tokens = vec![];

    while let kind = kind(&mut lexer) && kind != EOF {
        tokens.push(Token {
            kind,
            span: lexer.span(),
        });
    }

    tokens
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

        assert_token!(lexer, Identifier, "ident");
        assert_token!(lexer, Identifier, "True");
        assert_token!(lexer, Identifier, "CONST");
        assert_token!(lexer, Underscore, "_");
        assert_token!(lexer, Underscore, "___");
    }

    #[test]
    fn keywords() {
        let mut lexer = Lexer::new("and or not is mut if else case import where");

        assert_token!(lexer, And);
        assert_token!(lexer, Or);
        assert_token!(lexer, Not);
        assert_token!(lexer, Is);
        assert_token!(lexer, Mut);
        assert_token!(lexer, If);
        assert_token!(lexer, Else);
        assert_token!(lexer, Case);
        assert_token!(lexer, Import);
        assert_token!(lexer, Where);
    }

    #[test]
    fn punctuation() {
        let mut lexer = Lexer::new(". , ; ( ) [ ] { }");
        assert_token!(lexer, Dot);
        assert_token!(lexer, Comma);
        assert_token!(lexer, Semi);
        assert_token!(lexer, LParen);
        assert_token!(lexer, RParen);
        assert_token!(lexer, LBracket);
        assert_token!(lexer, RBracket);
        assert_token!(lexer, LBrace);
        assert_token!(lexer, RBrace);
    }

    #[test]
    fn operators() {
        let mut lexer = Lexer::new(". : => = == != + - * / % ^ > >= < <= ..");
        assert_token!(lexer, Dot, ".");
        assert_token!(lexer, Colon, ":");
        assert_token!(lexer, Arrow, "=>");
        assert_token!(lexer, Equals, "=");
        assert_token!(lexer, EqEq, "==");
        assert_token!(lexer, NoEq, "!=");
        assert_token!(lexer, Plus, "+");
        assert_token!(lexer, Minus, "-");
        assert_token!(lexer, Star, "*");
        assert_token!(lexer, Slash, "/");
        assert_token!(lexer, Mod, "%");
        assert_token!(lexer, Caret, "^");
        assert_token!(lexer, Gt, ">");
        assert_token!(lexer, GtEq, ">=");
        assert_token!(lexer, Lt, "<");
        assert_token!(lexer, LtEq, "<=");
        assert_token!(lexer, Dots, "..");
    }

    #[test]
    fn number() {
        let mut lexer = Lexer::new("42 3.14 0xABCDEF 0b0101");

        assert_token!(lexer, Integer, "42");
        assert_token!(lexer, Number, "3.14");
        assert_token!(lexer, Number, "0xABCDEF");
        assert_token!(lexer, Number, "0b0101");
    }

    #[test]
    fn simple_string() {
        let mut lexer = Lexer::new(r#""Hello, World""#);

        assert_token!(lexer, String, "\"Hello, World\"");
    }

    #[test]
    fn nested_string() {
        let mut lexer = Lexer::new(r#""code = \"n = 42\"""#);

        assert_token!(lexer, String, "\"code = \\\"n = 42\\\"\"");
    }

    #[test]
    fn empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");

        assert_token!(lexer, NewLine);
        assert_token!(lexer, EOF);
    }

    #[test]
    fn unterminated_string() {
        let mut lexer = Lexer::new(r#""Hello"#);

        assert_token!(lexer, UnterminatedString);
    }

    #[test]
    fn unbalanced_quotes() {
        let mut lexer = Lexer::new(r#""Hello"""#);

        assert_token!(lexer, String, "\"Hello\"");
        assert_token!(lexer, UnterminatedString);
    }

    #[test]
    fn invalid_escaped_string() {
        let mut lexer = Lexer::new(r#""escape \a""#);

        assert_token!(lexer, UnexpectedCharacter);
    }

    #[test]
    fn unexpected_character() {
        let mut lexer = Lexer::new("😀 = 10");

        assert_token!(lexer, UnexpectedCharacter);
    }
}
