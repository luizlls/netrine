use std::str::Chars;

use super::token::*;
use crate::error::{err, NetrineError, Result};
use crate::span::Span;

const SYMBOLS: &str = ".!:=+-<>*/%|";

#[derive(Debug, Clone, Copy, PartialEq)]
enum Mode {
    Regular,
    String,
    Template,
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: Chars<'src>,
    curr : Option<char>,
    peek : Option<char>,
    start: usize,
    offset: usize,
    mode: Mode,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Lexer {
        let mut lexer = Lexer {
            src,
            chars: src.chars(),
            curr: None,
            peek: None,
            start: 0,
            offset: 0,
            mode: Mode::Regular,
        };

        lexer.bump();
        lexer.bump();
        lexer.offset = 0;

        lexer
    }

    pub fn value(&self) -> &str {
        &self.src[(self.start as usize)..(self.offset as usize)]
    }

    pub fn next(&mut self) -> Result<Token> {
        self.next_token().map(|kind| {
            Token {
                kind,
                span: self.span(),
            }
        })
    }

    fn align(&mut self) {
        self.start = self.offset;
    }

    fn bump(&mut self) {
        let length = self.curr.map_or(0, char::len_utf8);
        self.offset += length;
        self.curr = self.peek;
        self.peek = self.chars.next();
    }

    fn span(&self) -> Span {
        Span(self.start as u32, self.offset as u32)
    }

    fn token(&mut self, kind: TokenKind) -> Result<TokenKind> {
        self.bump();
        Ok(kind)
    }

    fn next_token(&mut self) -> Result<TokenKind> {
        if self.mode == Mode::String {
            return self.next_string(false);
        }

        self.align();

        if let Some(chr) = self.curr {
            match chr {
                ' ' | '\t' | '\r' => {
                    self.space()
                }
                '\n' => {
                    self.lines()
                }
                'a'..='z' | '_' => {
                    self.lower()
                }
                'A'..='Z' => {
                    self.upper()
                }
                '0'..='9' => {
                    self.number(false)
                }
                '+' | '-' if self.is_number(self.peek) => {
                    self.number(true)
                }
                '"' => {
                    self.next_string(true)
                }
                '(' => {
                    self.token(TokenKind::LParen)
                }
                ')' => {
                    self.token(TokenKind::RParen)
                }
                '[' => {
                    self.token(TokenKind::LBracket)
                }
                ']' => {
                    self.token(TokenKind::RBracket)
                }
                '{' => {
                    self.token(TokenKind::LBrace)
                }
                '}' if self.mode == Mode::Template => {
                    self.mode = Mode::String;
                    self.token(TokenKind::RBrace)
                }
                '}' => {
                    self.token(TokenKind::RBrace)
                }
                ',' => {
                    self.token(TokenKind::Comma)
                }
                ';' => {
                    self.token(TokenKind::Semi)
                }
                '/' if self.peek == Some('/') => {
                    self.comment()
                }
                _ if self.is_symbol(self.curr) => {
                    self.operator()
                }
                _ => {
                    err!(self.span(), "unexpected character")
                }
            }
        } else {
            Ok(TokenKind::Eof)
        }
    }

    fn operator(&mut self) -> Result<TokenKind> {
        match self.curr.unwrap() {
            '=' => {
                if self.peek == Some('=') {
                    self.bump();
                    self.token(TokenKind::Op(OpKind::EqEq))
                } else {
                    self.token(TokenKind::Equals)
                }
            }
            '.' => {
                self.token(TokenKind::Dot)
            }
            ':' => {
                self.token(TokenKind::Colon)
            }
            '+' => {
                self.token(TokenKind::Op(OpKind::Plus))
            }
            '-' => {
                self.token(TokenKind::Op(OpKind::Minus))
            }
            '*' => {
                self.token(TokenKind::Op(OpKind::Star))
            }
            '/' => {
                self.token(TokenKind::Op(OpKind::Slash))
            }
            '%' => {
                self.token(TokenKind::Op(OpKind::Mod))
            }
            '<' => {
                if self.curr == Some('=') {
                    self.bump();
                    self.token(TokenKind::Op(OpKind::LeEq))
                } else {
                    self.token(TokenKind::Op(OpKind::Lt))
                }
            }
            '>' => {
                if self.curr == Some('=') {
                    self.bump();
                    self.token(TokenKind::Op(OpKind::GtEq))
                } else {
                    self.token(TokenKind::Op(OpKind::Gt))
                }
            }
            '!' => {
                if self.peek == Some('=') {
                    self.bump();
                    self.token(TokenKind::Op(OpKind::BangEq))
                } else {
                    err!(self.span(), "invalid operator")
                }
            }
            '|' => {
                if self.peek == Some('>') {
                    self.bump();
                    self.token(TokenKind::Op(OpKind::Pipe))
                } else {
                    err!(self.span(), "invalid operator")
                }
            }
            _ => {
                err!(self.span(), "unexpected character")
            }
        }
    }

    fn next_string(&mut self, start: bool) -> Result<TokenKind> {
        self.align();

        loop {
            match self.curr {
                Some('"') if self.mode == Mode::Regular => {
                    self.bump();
                    self.mode = Mode::String;
                }
                Some('"') => {
                    self.mode = Mode::Regular;
                    self.bump();
                    return if start {
                        Ok(TokenKind::String)
                    } else {
                        Ok(TokenKind::StringEnd)
                    };
                }
                Some('{') if self.peek == Some('{') => {
                    self.bump();
                    self.bump();
                }
                Some('{') => {
                    self.mode = Mode::Template;
                    return if start {
                        Ok(TokenKind::StringStart)
                    } else {
                        Ok(TokenKind::StringSlice)
                    };
                }
                Some('\n') | None => {
                    return err!(self.span(), "unterminated string")
                }
                Some('\\') => {
                    self.bump();

                    match self.curr {
                        Some('n' | 'r' | 't' | 'v' | 'a' | 'b' | '"' | '0' | '\\') => {
                            self.bump();
                        }
                        Some('u') => {
                            todo!("Validate escaped unicode")
                        }
                        Some('x') => {
                            todo!("Validate escaped binary")
                        }
                        _ => {
                            return err!(self.span(), "invalid escape character")
                        }
                    }
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn is_alpha(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
    }

    fn is_number(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('0'..='9'))
    }

    fn is_symbol(&self, chr: Option<char>) -> bool {
        chr.map_or(false, |c| SYMBOLS.contains(c))
    }

    fn is_space(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('\r' | '\t' | ' '))
    }

    fn lower(&mut self) -> Result<TokenKind> {
        self.bump_while(Self::is_alpha);
        self.bump_while(|_, ch| matches!(ch, Some('\'' | '?' | '!')));

        if let Some(keyword) = get_keyword(self.value()) {
            Ok(keyword)
        } else {
            Ok(TokenKind::Id(IdKind::Lower))
        }
    }

    fn upper(&mut self) -> Result<TokenKind> {
        self.bump_while(Self::is_alpha);
        Ok(TokenKind::Id(IdKind::Upper))
    }

    fn number(&mut self, prefix: bool) -> Result<TokenKind> {
        if prefix {
            self.bump();
        }

        self.bump_while(Self::is_number);

        if self.curr == Some('.') {
            self.bump();
            self.bump_while(Self::is_number);
        }

        Ok(TokenKind::Number)
    }

    fn lines(&mut self) -> Result<TokenKind> {
        while self.curr == Some('\n') {
            self.bump();
        }
        self.next_token()
    }

    fn space(&mut self) -> Result<TokenKind> {
        self.bump_while(Self::is_space);
        self.next_token()
    }

    fn comment(&mut self) -> Result<TokenKind> {
        self.bump_while(|_, ch| !matches!(ch, Some('\n') | None));
        self.next_token()
    }

    fn bump_while<P>(&mut self, pred: P)
    where
        P: Fn(&Self, Option<char>) -> bool,
    {
        while pred(self, self.curr) { self.bump(); }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("variable = 1");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Id(IdKind::Lower));
        assert_eq!(lexer.value(), "variable");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Equals);

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Number);
        assert_eq!(lexer.value(), "1");
    }

    #[test]
    fn test_variant() {
        let mut lexer = Lexer::new("True or False");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Id(IdKind::Upper));
        assert_eq!(lexer.value(), "True");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Or));

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Id(IdKind::Upper));
        assert_eq!(lexer.value(), "False");
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("and or not is");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::And));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Or));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Not));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Is));
    }

    #[test]
    fn test_operator() {
        let mut lexer = Lexer::new(": . .. = == != + - * / % > < >= <= |>");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Colon);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Dot);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Dot);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Dot);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Equals);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::EqEq));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::BangEq));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Plus));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Minus));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Star));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Slash));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Mod));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Gt));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Lt));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::GtEq));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::LeEq));
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Op(OpKind::Pipe));
    }

    #[test]
    fn test_integer() {
        let mut lexer = Lexer::new("42");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Number);
        assert_eq!(lexer.value(), "42");
    }

    #[test]
    fn test_float() {
        let mut lexer = Lexer::new("3.14519");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Number);
        assert_eq!(lexer.value(), "3.14519");
    }

    #[test]
    fn test_simple_string() {
        let mut lexer = Lexer::new("\"Hello, World\"");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::String);
        assert_eq!(lexer.value(), "\"Hello, World\"");
    }

    #[test]
    fn test_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#);

        assert_eq!(lexer.next_token().unwrap(), TokenKind::String);
        assert_eq!(lexer.value(), "\"src = \\\"y = 42\\\"\"");
    }

    #[test]
    fn test_string_template() {
        let mut lexer = Lexer::new(r#""Hello, {name}""#);

        assert_eq!(lexer.next_token().unwrap(), TokenKind::StringStart);
        assert_eq!(lexer.value(), r#""Hello, "#);
        assert_eq!(lexer.mode, Mode::Template);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::LBrace);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Id(IdKind::Lower));
        assert_eq!(lexer.value(), "name");
        assert_eq!(lexer.next_token().unwrap(), TokenKind::RBrace);
        let _ = lexer.next_token(); // closing "
        assert_eq!(lexer.mode, Mode::Regular);
    }

    #[test]
    fn test_empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");
        assert_eq!(lexer.next_token().unwrap(), TokenKind::NewLine);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Eof);
    }
}
