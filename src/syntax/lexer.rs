use crate::span::Span;
use super::token::*;

const AND: &[u8] = "and".as_bytes();
const OR : &[u8] = "or".as_bytes();
const NOT: &[u8] = "not".as_bytes();
const IS : &[u8] = "is".as_bytes();

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Context {
    Default,
    String,
    Template,
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src [u8],
    curr: Option<u8>,
    start: usize,
    index: usize,
    context: Context,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src [u8]) -> Lexer {
        let mut lexer = Lexer {
            src,
            curr: None,
            start: 0,
            index: 0,
            context: Context::Default,
        };

        lexer.curr = lexer.src.first().cloned();
        lexer
    }

    fn align(&mut self) {
        self.start = self.index;
    }

    fn bump(&mut self) {
        self.index += 1;
        self.curr = self.src.get(self.index).cloned();
    }
    
    fn peek(&self) -> Option<u8> {
        self.src.get(self.index + 1).cloned()
    }

    fn slice(&self) -> &[u8] {
        &self.src[self.start .. self.index]
    }

    fn span(&self) -> Span {
        Span(self.start as u32, self.index as u32)
    }

    fn single(&mut self, kind: TokenKind) -> TokenKind {
        self.bump();
        kind
    }

    fn token(&mut self) -> TokenKind {
        self.align();

        if self.context == Context::String {
            return self.string(false);
        }

        if let Some(curr) = self.curr {
            match curr {
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
                b'}' if self.context == Context::Template => {
                    self.context = Context::String;
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
        } else {
            TokenKind::EOF
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
            b'^' => {
                self.single(TokenKind::Caret)
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
                b'"' if self.context == Context::Default => {
                    self.bump();
                    self.context = Context::String;
                }
                b'"' => {
                    self.context = Context::Default;
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
                    self.context = Context::Template;
                    return if start {
                        TokenKind::StringStart
                    } else {
                        TokenKind::StringPart
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
                           b'+' | b'-' | b'*' | b'/' | b'^' | b'%' | b'|'))
    }

    fn is_space(&self, chr: Option<u8>) -> bool {
        matches!(chr, Some(b'\r' | b'\t' | b' '))
    }

    fn ident(&mut self) -> TokenKind {
        self.bump_while(Self::is_alpha);
        self.bump_while(|_, ch| matches!(ch, Some(b'\''))); // single quote / "prime"

        match self.slice() {
            AND => TokenKind::And,
            OR  => TokenKind::Or,
            IS  => TokenKind::Is,
            NOT => TokenKind::Not,
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
        let token = Token {
            kind: self.token(),
            span: self.span(),
        };

        if token.is(TokenKind::EOF) {
            None
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("variable = 1".as_bytes());

        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.slice(), "variable".as_bytes());

        assert_eq!(lexer.token(), TokenKind::Equals);

        assert_eq!(lexer.token(), TokenKind::Number);
        assert_eq!(lexer.slice(), "1".as_bytes());
    }

    #[test]
    fn test_variant() {
        let mut lexer = Lexer::new("True or False".as_bytes());

        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.slice(), "True".as_bytes());

        assert_eq!(lexer.token(), TokenKind::Or);

        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.slice(), "False".as_bytes());
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("and or not is".as_bytes());

        assert_eq!(lexer.token(), TokenKind::And);
        assert_eq!(lexer.token(), TokenKind::Or);
        assert_eq!(lexer.token(), TokenKind::Not);
        assert_eq!(lexer.token(), TokenKind::Is);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new(": . .. = == != + - * / % ^ > < >= <= |>".as_bytes());

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
        assert_eq!(lexer.token(), TokenKind::Caret);
        assert_eq!(lexer.token(), TokenKind::Gt);
        assert_eq!(lexer.token(), TokenKind::Lt);
        assert_eq!(lexer.token(), TokenKind::GtEq);
        assert_eq!(lexer.token(), TokenKind::LtEq);
        assert_eq!(lexer.token(), TokenKind::Pipe);
    }

    #[test]
    fn test_integer() {
        let mut lexer = Lexer::new("42".as_bytes());

        assert_eq!(lexer.token(), TokenKind::Number);
        assert_eq!(lexer.slice(), "42".as_bytes());
    }

    #[test]
    fn test_float() {
        let mut lexer = Lexer::new("3.14519".as_bytes());

        assert_eq!(lexer.token(), TokenKind::Number);
        assert_eq!(lexer.slice(), "3.14519".as_bytes());
    }

    #[test]
    fn test_simple_string() {
        let mut lexer = Lexer::new("\"Hello, World\"".as_bytes());

        assert_eq!(lexer.token(), TokenKind::String);
        assert_eq!(lexer.slice(), "\"Hello, World\"".as_bytes());
    }

    #[test]
    fn test_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#.as_bytes());

        assert_eq!(lexer.token(), TokenKind::String);
        assert_eq!(lexer.slice(), "\"src = \\\"y = 42\\\"\"".as_bytes());
    }

    #[test]
    fn test_string_template() {
        let mut lexer = Lexer::new(r#""Hello, {name}""#.as_bytes());

        assert_eq!(lexer.token(), TokenKind::StringStart);
        assert_eq!(lexer.slice(), r#""Hello, "#.as_bytes());
        assert_eq!(lexer.context, Context::Template);
        assert_eq!(lexer.token(), TokenKind::LBrace);
        assert_eq!(lexer.token(), TokenKind::Ident);
        assert_eq!(lexer.slice(), "name".as_bytes());
        assert_eq!(lexer.token(), TokenKind::RBrace);
        let _ = lexer.token(); // closing "
        assert_eq!(lexer.context, Context::Default);
    }

    #[test]
    fn test_empty_lines() {
        let mut lexer = Lexer::new("\n\n\n".as_bytes());

        assert_eq!(lexer.token(), TokenKind::NewLine);
        assert_eq!(lexer.token(), TokenKind::EOF);
    }
}
