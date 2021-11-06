use std::str::Chars;

use super::token::*;
use crate::Span;

const SYMBOLS: &str = ".!:=+-<>*/%|";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Regular,

    String,

    Template
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: Chars<'src>,
    curr: Option<char>,
    peek: Option<char>,
    start: usize,
    offset: usize,
    line: u32,
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
            line: 1,
            mode: Mode::Regular,
        };
        lexer.bump();
        lexer.bump();
        lexer.offset = 0;

        lexer
    }

    fn span(&self) -> Span {
        Span::new(self.line, self.start as u32, self.offset as u32)
    }

    fn value(&self) -> &str {
        &self.src[(self.start as usize) .. (self.offset as usize)]
    }

    fn align(&mut self) {
        self.start = self.offset;
    }

    fn bump(&mut self) {
        let length = self.curr.map(char::len_utf8).unwrap_or(0);
        self.offset += length;
        self.curr = self.peek;
        self.peek = self.chars.next();
    }

    fn line(&mut self) -> Option<TokenKind> {
        self.line += 1;
        self.bump();
        self.next_token()
    }

    fn next_token(&mut self) -> Option<TokenKind> {
        if self.mode == Mode::String {
            return self.template(false);
        }

        self.align();

        match self.curr {
            Some('a'..='z') => {
                Some(self.lower())
            }
            Some('A'..='Z') => {
                Some(self.upper())
            }
            Some('_') if self.is_alpha(self.peek) => {
                Some(self.lower())
            }
            Some('0'..='9') => {
                Some(self.number(false))
            }
            Some('+' | '-') if self.is_number(self.peek) => {
                Some(self.number(true))
            }
            Some('"') => {
                self.template(true)
            }
            Some('(') => {
                self.single(TokenKind::LParen)
            }
            Some(')') => {
                self.single(TokenKind::RParen)
            }
            Some('[') => {
                self.single(TokenKind::LBracket)
            }
            Some(']') => {
                self.single(TokenKind::RBracket)
            }
            Some('{') => {
                self.single(TokenKind::LBrace)
            }
            Some('}') if self.mode == Mode::Template => {
                self.mode = Mode::String;
                self.single(TokenKind::RBrace)
            }
            Some('}') => {
                self.single(TokenKind::RBrace)
            }
            Some(',') => {
                self.single(TokenKind::Comma)
            }
            Some(';') => {
                self.single(TokenKind::Semi)
            }
            Some('#') => {
                self.single(TokenKind::Hash)
            }
            Some('/') if self.peek == Some('/') => {
                self.comment()
            }
            Some(' ' | '\t' | '\r') => {
                self.space()
            }
            Some('\n') => {
                self.line()
            }
            Some(_) if self.is_symbol(self.curr) => {
                self.operator()
            }
            None => {
                None
            }
            Some(_) => {
                Some(TokenKind::Error(TokenError::InvalidCharacter))
            }
        }
    }

    fn operator(&mut self) -> Option<TokenKind> {
        match self.curr {
            Some('.') => {
                if self.peek == Some('.') {
                    self.bump();
                    self.single(TokenKind::Range)
                } else {
                    self.single(TokenKind::Dot)
                }
            }
            Some('=') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.single(TokenKind::Eq)
                } else {
                    self.single(TokenKind::Equals)
                }
            }
            Some(':') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.single(TokenKind::Walrus)
                } else {
                    self.single(TokenKind::Colon)
                }
            }
            Some('<') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.single(TokenKind::Le)
                } else {
                    self.single(TokenKind::Lt)
                }
            }
            Some('>') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.single(TokenKind::Ge)
                } else {
                    self.single(TokenKind::Gt)
                }
            }
            Some('!') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.single(TokenKind::Ne)
                } else {
                    Some(TokenKind::Error(TokenError::InvalidOperator))
                }
            }
            Some('|') => {
                if self.peek == Some('>') {
                    self.bump();
                    self.single(TokenKind::Thread)
                } else {
                    self.single(TokenKind::Pipe)
                }
            }
            Some('+') => {
                self.single(TokenKind::Add)
            }
            Some('-') => {
                self.single(TokenKind::Sub)
            }
            Some('*') => {
                self.single(TokenKind::Mul)
            }
            Some('/') => {
                self.single(TokenKind::Div)
            }
            Some('%') => {
                self.single(TokenKind::Rem)
            }
            _ => None
        }
    }

    fn template(&mut self, start: bool) -> Option<TokenKind> {
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
                        Some(TokenKind::String)
                    } else {
                        Some(TokenKind::StringEnd)
                    };
                }
                Some('{') if self.peek == Some('{') => {
                    self.bump();
                    self.bump();
                }
                Some('{') => {
                    self.mode = Mode::Template;
                    return if start {
                        Some(TokenKind::StringStart)
                    } else {
                        Some(TokenKind::StringSlice)
                    };
                }
                Some('\n') |
                None => {
                    return Some(TokenKind::Error(TokenError::UnterminatedString));
                }
                Some('\\') => {
                    self.bump();

                    match self.curr {
                        Some('n') | Some('r')
                      | Some('t') | Some('v')
                      | Some('a') | Some('b')
                      | Some('"') | Some('0')
                      | Some('\\') => {
                            self.bump();
                        }
                        Some('u') => {
                            todo!("Validate escaped unicode")
                        }
                        Some('x') => {
                            todo!("Validate escaped binary")
                        }
                        _ => {
                            return Some(TokenKind::Error(TokenError::InvalidEscapeCharacter));
                        }
                    }
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn single(&mut self, token: TokenKind) -> Option<TokenKind> {
        self.bump();
        Some(token)
    }

    fn is_alpha(&self, chr: Option<char>) -> bool {
        matches!(chr,
            Some('a'..='z') |
            Some('A'..='Z') |
            Some('0'..='9') |
            Some('_'))
    }

    fn is_number(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('0'..='9'))
    }

    fn is_symbol(&self, chr: Option<char>) -> bool {
        if let Some(chr) = chr {
            SYMBOLS.contains(chr)
        } else {
            false
        }
    }

    fn is_space(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('\r') | Some('\t') | Some(' '))
    }

    fn lower(&mut self) -> TokenKind {
        while self.is_alpha(self.curr) { self.bump(); }

        match self.curr {
            Some('?')
          | Some('!') => {
                self.bump();
            }
            Some('\'') => {
                while self.curr == Some('\'') {
                    self.bump();
                }
            }
            _ => {}
        }

        if let Some(keyword) = get_keyword(self.value()) {
            keyword
        } else {
            TokenKind::Lower
        }
    }

    fn upper(&mut self) -> TokenKind {
        while self.is_alpha(self.curr) { self.bump(); }
        TokenKind::Upper
    }

    fn number(&mut self, prefix: bool) -> TokenKind {
        if prefix {
            self.bump();
        }

        let mut float = false;

        while self.is_number(self.curr) {
            self.bump();

            if self.curr == Some('.') && !float && self.is_number(self.peek) {
                float = true;
                self.bump();
            }
        }

        TokenKind::Number
    }

    fn space(&mut self) -> Option<TokenKind> {
        while self.is_space(self.curr) {
            self.bump();
        }

        self.next_token()
    }

    fn comment(&mut self) -> Option<TokenKind> {
        loop {
            match self.curr {
                Some('\n')
              | None => {
                    break;
                }
                _ => {
                    self.bump();
                }
            }
        }

        self.next_token()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.next_token()
            .map(|kind| {
                Token { kind, span: self.span() }
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("variable = 1");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lower);
        assert_eq!(lexer.value(), "variable");
    }

    #[test]
    fn test_variant() {
        let mut lexer = Lexer::new("True or False");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Upper);
        assert_eq!(lexer.value(), "True");
        
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Or);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Upper);
        assert_eq!(lexer.value(), "False");
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("case if then else it and or not");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Case);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::If);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Then);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Else);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::It);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::And);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Or);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Not);
    }

    #[test]
    fn test_operator() {
       let mut lexer = Lexer::new(": . .. | |> = := == != + - * / % > < >= <=");

       assert_eq!(lexer.next().unwrap().kind, TokenKind::Colon);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Dot);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Range);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Pipe);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Thread);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Equals);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Walrus);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Eq);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Ne);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Add);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Sub);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Mul);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Div);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Rem);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Gt);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Lt);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Ge);
       assert_eq!(lexer.next().unwrap().kind, TokenKind::Le);
    }

    #[test]
    fn test_integer() {
        let mut lexer = Lexer::new("42");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Number);
        assert_eq!(lexer.value(), "42");
    }

    #[test]
    fn test_float() {
        let mut lexer = Lexer::new("3.14519");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Number);
        assert_eq!(lexer.value(), "3.14519");
    }

    #[test]
    fn test_simple_string() {
        let mut lexer = Lexer::new("\"Hello, World\"");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String);
        assert_eq!(lexer.value(), "\"Hello, World\"");
    }

    #[test]
    fn test_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::String);
        assert_eq!(lexer.value(), "\"src = \\\"y = 42\\\"\"");
    }

    #[test]
    fn test_string_template() {
        let mut lexer = Lexer::new(r#""Hello, {name}""#);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::StringStart);
        assert_eq!(lexer.value(), r#""Hello, "#);
        assert_eq!(lexer.mode, Mode::Template);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::LBrace);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lower);
        assert_eq!(lexer.value(), "name");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::RBrace);
        lexer.next(); // closing "
        assert_eq!(lexer.mode, Mode::Regular);
    }
}
