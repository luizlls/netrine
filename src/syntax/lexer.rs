use crate::source::{Source, Span};

use super::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer<'l> {
    source: &'l Source,
    bytes: &'l [u8],
    curr: u8,
    peek: u8,
    index: usize,
    start: usize,
}

impl<'l> Lexer<'l> {
    fn new(source: &'l Source) -> Lexer<'l> {
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
}

pub fn tokens(source: &Source) -> Vec<Token> {
    let mut lexer = Lexer::new(source);
    let mut tokens = vec![];

    while let kind = lexer.kind() && kind != TokenKind::EOF {
        tokens.push(Token {
            kind,
            span: lexer.span(),
        });
    }

    tokens
}
