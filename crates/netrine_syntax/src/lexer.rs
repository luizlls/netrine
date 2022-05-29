use std::str::Chars;

use super::token::*;
use netrine_core::{NetrineError, Result, Span};

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

    pub fn value(&self) -> &str {
        &self.src[(self.start as usize)..(self.offset as usize)]
    }

    pub fn next(&mut self) -> Result<Token> {
        self.next_token().map(|kind| Token {
            kind,
            span: self.span(),
        })
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

    fn line(&mut self) -> Result<TokenKind> {
        while self.curr == Some('\n') {
            self.bump();
            self.line += 1;
        }

        self.next_token()
    }

    fn span(&self) -> Span {
        Span::new(self.line, self.start as u32, self.offset as u32)
    }

    fn next_token(&mut self) -> Result<TokenKind> {
        match self.mode {
            Mode::String => {
                return self.next_string(false);
            }
            _ => {}
        }

        self.align();

        if let Some(chr) = self.curr {
            match chr {
                ' ' | '\t' | '\r' => {
                    self.space()
                }
                '\n' => {
                    self.line()
                }
                'a'..='z' => {
                    self.lower()
                }
                'A'..='Z' => {
                    self.upper()
                }
                '_' if self.is_alpha(self.peek) => {
                    self.lower()
                }
                '0'..='9' => {
                    self.number(false)
                }
                '+' | '-' if self.is_number(self.peek) => {
                    self.number(true)
                }
                '"' => self.next_string(true),
                '(' => {
                    self.bump();
                    Ok(TokenKind::LParen)
                }
                ')' => {
                    self.bump();
                    Ok(TokenKind::RParen)
                }
                '[' => {
                    self.bump();
                    Ok(TokenKind::LBracket)
                }
                ']' => {
                    self.bump();
                    Ok(TokenKind::RBracket)
                }
                '{' => {
                    self.bump();
                    Ok(TokenKind::LBrace)
                }
                '}' if self.mode == Mode::Template => {
                    self.bump();
                    self.mode = Mode::String;
                    Ok(TokenKind::RBrace)
                }
                '}' => {
                    self.bump();
                    Ok(TokenKind::RBrace)
                }
                ',' => {
                    self.bump();
                    Ok(TokenKind::Comma)
                }
                ';' => {
                    self.bump();
                    Ok(TokenKind::Semi)
                }
                '/' if self.peek == Some('/') => {
                    self.comment()
                }
                _ if self.is_symbol(self.curr) => {
                    self.operator()
                }
                _ => Err(NetrineError::error(
                    self.span(),
                    "unexpected character".into(),
                )),
            }
        } else {
            Ok(TokenKind::EOF)
        }
    }

    fn operator(&mut self) -> Result<TokenKind> {
        if let Some(chr) = self.curr {
            match chr {
                '.' => {
                    if self.peek == Some('.') {
                        self.bump();
                        self.bump();
                        Ok(TokenKind::Range)
                    } else {
                        self.bump();
                        Ok(TokenKind::Dot)
                    }
                }
                '=' => {
                    if self.peek == Some('=') {
                        self.bump();
                        self.bump();
                        Ok(TokenKind::Eq)
                    } else {
                        self.bump();
                        Ok(TokenKind::Equals)
                    }
                }
                ':' => {
                    self.bump();
                    Ok(TokenKind::Colon)
                }
                '<' => {
                    if self.peek == Some('=') {
                        self.bump();
                        self.bump();
                        Ok(TokenKind::Le)
                    } else {
                        self.bump();
                        Ok(TokenKind::Lt)
                    }
                }
                '>' => {
                    if self.peek == Some('=') {
                        self.bump();
                        self.bump();
                        Ok(TokenKind::Ge)
                    } else {
                        self.bump();
                        Ok(TokenKind::Gt)
                    }
                }
                '!' => {
                    if self.peek == Some('=') {
                        self.bump();
                        self.bump();
                        Ok(TokenKind::Ne)
                    } else {
                        Err(NetrineError::error(self.span(), "invalid operator".into()))
                    }
                }
                '|' => {
                    if self.peek == Some('>') {
                        self.bump();
                        self.bump();
                        Ok(TokenKind::Pipe)
                    } else {
                        Err(NetrineError::error(self.span(), "invalid operator".into()))
                    }
                }
                '+' => {
                    self.bump();
                    Ok(TokenKind::Add)
                }
                '-' => {
                    self.bump();
                    Ok(TokenKind::Sub)
                }
                '*' => {
                    self.bump();
                    Ok(TokenKind::Mul)
                }
                '/' => {
                    self.bump();
                    Ok(TokenKind::Div)
                }
                '%' => {
                    self.bump();
                    Ok(TokenKind::Rem)
                }
                _ => Err(NetrineError::error(self.span(), "unexpected character".into())),
            }
        } else {
            Ok(TokenKind::EOF)
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
                    return Err(NetrineError::error(
                        self.span(),
                        "unterminated string".into(),
                    ))
                }
                Some('\\') => {
                    self.bump();

                    match self.curr {
                        Some('n') | Some('r') | Some('t') | Some('v') | Some('a') | Some('b')
                        | Some('"') | Some('0') | Some('\\') => {
                            self.bump();
                        }
                        Some('u') => {
                            todo!("Validate escaped unicode")
                        }
                        Some('x') => {
                            todo!("Validate escaped binary")
                        }
                        _ => {
                            return Err(NetrineError::error(
                                self.span(),
                                "invalid escape character".into(),
                            ))
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
        matches!(
            chr,
            Some('a'..='z') | Some('A'..='Z') | Some('0'..='9') | Some('_')
        )
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

    fn lower(&mut self) -> Result<TokenKind> {
        self.bump_while(Self::is_alpha);

        self.bump_while(|_, ch| matches!(ch, Some('\'' | '?' | '!')));

        if let Some(keyword) = get_keyword(self.value()) {
            Ok(keyword)
        } else {
            Ok(TokenKind::Lower)
        }
    }

    fn upper(&mut self) -> Result<TokenKind> {
        self.bump_while(Self::is_alpha);
        Ok(TokenKind::Upper)
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
        while pred(&self, self.curr) {
            self.bump()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("variable = 1");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Lower);
        assert_eq!(lexer.indent(), 0);
        assert_eq!(lexer.value(), "variable");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Equals);
        assert_eq!(lexer.indent(), 9);

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Number);
        assert_eq!(lexer.indent(), 11);
        assert_eq!(lexer.value(), "1");
    }

    #[test]
    fn test_variant() {
        let mut lexer = Lexer::new("True or False");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Upper);
        assert_eq!(lexer.value(), "True");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Or);

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Upper);
        assert_eq!(lexer.value(), "False");
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("if then else and or not");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::If);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Then);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Else);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::And);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Or);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Not);
    }

    #[test]
    fn test_operator() {
        let mut lexer = Lexer::new(": . .. |> = == != + - * / % > < >= <=");

        assert_eq!(lexer.next_token().unwrap(), TokenKind::Colon);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Dot);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Range);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Pipe);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Equals);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Eq);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Ne);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Add);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Sub);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Mul);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Div);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Rem);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Gt);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Lt);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Ge);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Le);
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
        assert_eq!(lexer.next_token().unwrap(), TokenKind::Lower);
        assert_eq!(lexer.value(), "name");
        assert_eq!(lexer.next_token().unwrap(), TokenKind::RBrace);
        let _ = lexer.next_token(); // closing "
        assert_eq!(lexer.mode, Mode::Regular);
    }

    #[test]
    fn test_empty_lines() {
        let mut lexer = Lexer::new("\n\n\n");
        assert_eq!(lexer.next_token().unwrap(), TokenKind::NewLine);
        assert_eq!(lexer.next_token().unwrap(), TokenKind::EOF);
    }
}
