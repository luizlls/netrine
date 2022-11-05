use crate::span::Span;

use super::token::*;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Mode {
    Regular,
    String,
    Template,
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    raw: &'src [u8],
    curr: Option<u8>,
    start: usize,
    index: usize,
    mode: Mode,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Lexer {
        let mut lexer = Lexer {
            src,
            raw: src.as_bytes(),
            curr: None,
            start: 0,
            index: 0,
            mode: Mode::Regular,
        };

        lexer.curr = lexer.raw.first().cloned();
        lexer
    }

    fn align(&mut self) {
        self.start = self.index;
    }

    fn bump(&mut self) {
        self.index += 1;
        self.curr = self.raw.get(self.index).cloned();
    }
    
    fn peek(&self) -> Option<u8> {
        self.raw.get(self.index + 1).cloned()
    }

    fn span(&self) -> Span {
        Span(self.start as u32, self.index as u32)
    }

    fn value(&self) -> &str {
        &self.src[self.span().range()]
    }

    fn single(&mut self, kind: TokenKind) -> TokenKind {
        self.bump();
        kind
    }

    fn token(&mut self) -> TokenKind {
        self.align();

        if self.mode == Mode::String {
            return self.string(false);
        }

        match self.curr.unwrap() {
            b' ' | b'\t' | b'\r' => {
                self.space()
            }
            b'\n' => {
                self.lines()
            }
            b'a'..=b'z' | 
            b'A'..=b'Z' |
            b'_' => {
                self.ident()
            }
            b'0'..=b'9' => {
                self.number(false)
            }
            b'+' | b'-' if self.is_number(self.peek()) => {
                self.number(true)
            }
            b'"' => {
                self.string(true)
            }
            b'(' => {
                self.single(TokenKind::LParen)
            }
            b')' => {
                self.single(TokenKind::RParen)
            }
            b'[' => {
                self.single(TokenKind::LBracket)
            }
            b']' => {
                self.single(TokenKind::RBracket)
            }
            b'{' => {
                self.single(TokenKind::LBrace)
            }
            b'}' if self.mode == Mode::Template => {
                self.mode = Mode::String;
                self.single(TokenKind::RBrace)
            }
            b'}' => {
                self.single(TokenKind::RBrace)
            }
            b',' => {
                self.single(TokenKind::Comma)
            }
            b';' => {
                self.single(TokenKind::Semi)
            }
            b'/' if self.peek() == Some(b'/') => {
                self.comment()
            }
            _ if self.is_symbol(self.curr) => {
                self.operator()
            }
            _ => {
                TokenKind::Error(TokenErrorKind::UnexpectedCharacter)
            }
        }
    }

    fn operator(&mut self) -> TokenKind {
        match self.curr.unwrap() {
            b'.' => {
                self.single(TokenKind::Dot)
            }
            b':' => {
                self.single(TokenKind::Colon)
            }
            b'+' => {
                self.single(TokenKind::Plus)
            }
            b'-' => {
                self.single(TokenKind::Minus)
            }
            b'*' => {
                self.single(TokenKind::Star)
            }
            b'/' => {
                self.single(TokenKind::Slash)
            }
            b'%' => {
                self.single(TokenKind::Mod)
            }
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    self.single(TokenKind::EqEq)
                } else {
                    self.single(TokenKind::Equals)
                }
            }
            b'<' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    self.single(TokenKind::LtEq)
                } else {
                    self.single(TokenKind::Lt)
                }
            }
            b'>' => {
                if self.peek() == Some(b'=') {
                    self.bump();
                    self.single(TokenKind::GtEq)
                } else {
                    self.single(TokenKind::Gt)
                }
            }
            b'!' if self.peek() == Some(b'=') => {
                self.bump();
                self.single(TokenKind::NoEq)
            }
            b'|' if self.peek() == Some(b'>') => {
                self.bump();
                self.single(TokenKind::Pipe)
            }
            _ => {
                TokenKind::Error(TokenErrorKind::UnexpectedCharacter)
            }
        }
    }

    fn string(&mut self, start: bool) -> TokenKind {
        while let Some(chr) = self.curr {
            match chr {
                b'"' if self.mode == Mode::Regular => {
                    self.bump();
                    self.mode = Mode::String;
                }
                b'"' => {
                    self.mode = Mode::Regular;
                    self.bump();
                    return if start {
                        TokenKind::String
                    } else {
                        TokenKind::StringEnd
                    };
                }
                b'{' if self.peek() == Some(b'{') => {
                    self.bump();
                    self.bump();
                }
                b'{' => {
                    self.mode = Mode::Template;
                    return if start {
                        TokenKind::StringStart
                    } else {
                        TokenKind::StringSlice
                    };
                }
                b'\n' => {
                    break;
                }
                b'\\' => {
                    self.bump();

                    match self.curr {
                        Some(b'n' | b'r' | b't' | b'v' | b'a' | b'b' | b'"' | b'0' | b'\\') => {
                            self.bump();
                        }
                        Some(b'u') => {
                            todo!("Validate escaped unicode")
                        }
                        Some(b'x') => {
                            todo!("Validate escaped binary")
                        }
                        _ => {
                            return TokenKind::Error(TokenErrorKind::UnexpectedCharacter);
                        }
                    }
                }
                _ => {
                    self.bump();
                }
            }
        }

        TokenKind::Error(TokenErrorKind::UnterminatedString)
    }

    fn is_alpha(&self, chr: Option<u8>) -> bool {
        matches!(chr, Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'))
    }

    fn is_number(&self, chr: Option<u8>) -> bool {
        matches!(chr, Some(b'0'..=b'9'))
    }

    fn is_symbol(&self, chr: Option<u8>) -> bool {
        matches!(chr, Some(b'.' | b':' | b'!' | b'=' | b'<' | b'>' |
                           b'+' | b'-' | b'*' | b'/' | b'%' | b'|'))
    }

    fn is_space(&self, chr: Option<u8>) -> bool {
        matches!(chr, Some(b'\r' | b'\t' | b' '))
    }

    fn ident(&mut self) -> TokenKind {
        self.bump_while(Self::is_alpha);
        self.bump_while(|_, ch| matches!(ch, Some(b'\'' | b'?' | b'!')));

        match self.value() {
            "and" => TokenKind::And,
            "or"  => TokenKind::Or,
            "is"  => TokenKind::Is,
            "not" => TokenKind::Not,
            _ => TokenKind::Ident,
        }
    }

    fn number(&mut self, prefix: bool) -> TokenKind {
        if prefix {
            self.bump();
        }

        self.bump_while(Self::is_number);

        if self.curr == Some(b'.') {
            self.bump();
            self.bump_while(Self::is_number);
        }

        TokenKind::Number
    }

    fn lines(&mut self) -> TokenKind {
        while self.curr == Some(b'\n') {
            self.bump();
        }
        self.single(TokenKind::NewLine)
    }

    fn space(&mut self) -> TokenKind {
        self.bump_while(Self::is_space);
        self.token()
    }

    fn comment(&mut self) -> TokenKind {
        self.bump_while(|_, ch| !matches!(ch, Some(b'\n') | None));
        self.token()
    }

    fn bump_while<P>(&mut self, pred: P)
    where
        P: Fn(&Self, Option<u8>) -> bool,
    {
        while pred(self, self.curr) { self.bump(); }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr.is_some() {
            Some(Token {
                kind: self.token(),
                span: self.span(),
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("variable = 1");

        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.value(), "variable");

        assert_eq!(lexer.token(), TokenKind::Equals);

        assert_eq!(lexer.token(), TokenKind::Number);
        assert_eq!(lexer.value(), "1");
    }

    #[test]
    fn test_variant() {
        let mut lexer = Lexer::new("True or False");

        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.value(), "True");

        assert_eq!(lexer.token(), TokenKind::Or);

        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.value(), "False");
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("and or not is");

        assert_eq!(lexer.token(), TokenKind::And);
        assert_eq!(lexer.token(), TokenKind::Or);
        assert_eq!(lexer.token(), TokenKind::Not);
        assert_eq!(lexer.token(), TokenKind::Is);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new(": . .. = == != + - * / % > < >= <= |>");

        assert_eq!(lexer.token(), TokenKind::Colon);
        assert_eq!(lexer.token(), TokenKind::Dot);
        assert_eq!(lexer.token(), TokenKind::Dot);
        assert_eq!(lexer.token(), TokenKind::Dot);
        assert_eq!(lexer.token(), TokenKind::Equals);
        assert_eq!(lexer.token(), TokenKind::EqEq);
        assert_eq!(lexer.token(), TokenKind::NoEq);
        assert_eq!(lexer.token(), TokenKind::Plus);
        assert_eq!(lexer.token(), TokenKind::Minus);
        assert_eq!(lexer.token(), TokenKind::Star);
        assert_eq!(lexer.token(), TokenKind::Slash);
        assert_eq!(lexer.token(), TokenKind::Mod);
        assert_eq!(lexer.token(), TokenKind::Gt);
        assert_eq!(lexer.token(), TokenKind::Lt);
        assert_eq!(lexer.token(), TokenKind::GtEq);
        assert_eq!(lexer.token(), TokenKind::LtEq);
        assert_eq!(lexer.token(), TokenKind::Pipe);
    }

    #[test]
    fn test_integer() {
        let mut lexer = Lexer::new("42");

        assert_eq!(lexer.token(), TokenKind::Number);
        assert_eq!(lexer.value(), "42");
    }

    #[test]
    fn test_float() {
        let mut lexer = Lexer::new("3.14519");

        assert_eq!(lexer.token(), TokenKind::Number);
        assert_eq!(lexer.value(), "3.14519");
    }

    #[test]
    fn test_simple_string() {
        let mut lexer = Lexer::new("\"Hello, World\"");

        assert_eq!(lexer.token(), TokenKind::String);
        assert_eq!(lexer.value(), "\"Hello, World\"");
    }

    #[test]
    fn test_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#);

        assert_eq!(lexer.token(), TokenKind::String);
        assert_eq!(lexer.value(), "\"src = \\\"y = 42\\\"\"");
    }

    #[test]
    fn test_string_template() {
        let mut lexer = Lexer::new(r#""Hello, {name}""#);

        assert_eq!(lexer.token(), TokenKind::StringStart);
        assert_eq!(lexer.value(), r#""Hello, "#);
        assert_eq!(lexer.mode, Mode::Template);
        assert_eq!(lexer.token(), TokenKind::LBrace);
        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.value(), "name");
        assert_eq!(lexer.token(), TokenKind::RBrace);
        let _ = lexer.token(); // closing "
        assert_eq!(lexer.mode, Mode::Regular);
    }

    #[test]
    fn test_empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");
        assert_eq!(lexer.token(), TokenKind::NewLine);
        assert_eq!(lexer.token(), TokenKind::EOF);
    }
}
