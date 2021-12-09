use std::str::Chars;

use super::token::*;
use crate::Span;
use crate::error::{NetrineError, Result};

const SYMBOLS: &str = ".!:=+-<>*/%|";

#[derive(Debug, Clone, Copy, PartialEq)]
enum Mode {
    Regular,
    String,
    Template,
    NewLine,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum IndentStyle {
    Unset,
    Tabs,
    Spaces,
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: Chars<'src>,
    curr: Option<char>,
    peek: Option<char>,
    start: usize,
    offset: usize,
    indent: usize,
    line: u32,
    mode: Mode,
    indent_level: Vec<usize>,
    indent_style: IndentStyle,
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
            indent: 0,
            line: 1,
            mode: Mode::Regular,
            indent_level: vec![0],
            indent_style: IndentStyle::Unset,
        };
        lexer.bump();
        lexer.bump();
        lexer.offset = 0;
        lexer.indent = 0;

        lexer
    }

    pub fn span(&self) -> Span {
        Span::new(self.line, self.start as u32, self.offset as u32)
    }

    pub fn value(&self) -> &str {
        &self.src[(self.start as usize) .. (self.offset as usize)]
    }

    pub fn indent(&self) -> usize {
        self.indent
    }

    fn align(&mut self) {
        self.start = self.offset;
    }

    fn bump(&mut self) {
        let length = self.curr.map(char::len_utf8).unwrap_or(0);
        self.offset += length;
        self.indent += 1;
        self.curr = self.peek;
        self.peek = self.chars.next();
    }

    fn line(&mut self) -> Result<Token> {
        self.mode = Mode::NewLine;

        while self.curr == Some('\n') {
            self.bump();
            self.line += 1;
        }

        self.indent = 0;

        Ok(Token::NewLine)
    }

    fn next_token(&mut self) -> Result<Token> {
        match self.mode {
            Mode::NewLine => {
                return self.next_indent();
            }
            Mode::String => {
                return self.next_string(false);
            }
            _ => {}
        }

        self.align();

        match self.curr {
            Some(' ' | '\t' | '\r') => {
                self.space()
            }
            Some('\n') => {
                self.line()
            }
            Some('a'..='z') => {
                Ok(self.lower())
            }
            Some('A'..='Z') => {
                Ok(self.upper())
            }
            Some('_') if self.is_alpha(self.peek) => {
                Ok(self.lower())
            }
            Some('0'..='9') => {
                Ok(self.number(false))
            }
            Some('+' | '-') if self.is_number(self.peek) => {
                Ok(self.number(true))
            }
            Some('"') => {
                self.next_string(true)
            }
            Some('(') => {
                self.one(Token::LParen)
            }
            Some(')') => {
                self.one(Token::RParen)
            }
            Some('[') => {
                self.one(Token::LBracket)
            }
            Some(']') => {
                self.one(Token::RBracket)
            }
            Some('{') => {
                self.one(Token::LBrace)
            }
            Some('}') if self.mode == Mode::Template => {
                self.mode = Mode::String;
                self.one(Token::RBrace)
            }
            Some('}') => {
                self.one(Token::RBrace)
            }
            Some(',') => {
                self.one(Token::Comma)
            }
            Some(';') => {
                self.one(Token::Semi)
            }
            Some('_') => {
                self.one(Token::Anything)
            }
            Some('/') if self.peek == Some('/') => {
                self.comment()
            }
            Some(_) if self.is_symbol(self.curr) => {
                self.operator()
            }
            None => {
                Ok(Token::EOF)
            }
            Some(_) => {
                Err(NetrineError::error(
                    self.span(),
                    "unexpected character".into()))
            }
        }
    }

    fn operator(&mut self) -> Result<Token> {
        match self.curr {
            Some('.') => {
                if self.peek == Some('.') {
                    self.bump();
                    self.one(Token::Range)
                } else {
                    self.one(Token::Dot)
                }
            }
            Some('=') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.one(Token::Eq)
                } else if self.peek == Some('>') {
                    self.bump();
                    self.one(Token::Arrow)
                } else {
                    self.one(Token::Equals)
                }
            }
            Some(':') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.one(Token::Walrus)
                } else {
                    self.one(Token::Colon)
                }
            }
            Some('<') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.one(Token::Le)
                } else {
                    self.one(Token::Lt)
                }
            }
            Some('>') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.one(Token::Ge)
                } else {
                    self.one(Token::Gt)
                }
            }
            Some('!') => {
                if self.peek == Some('=') {
                    self.bump();
                    self.one(Token::Ne)
                } else {
                    Err(NetrineError::error(
                        self.span(),
                        "invalid operator".into()))
                }
            }
            Some('|') => {
                if self.peek == Some('>') {
                    self.bump();
                    self.one(Token::Pipe)
                } else {
                    Err(NetrineError::error(
                        self.span(),
                        "invalid operator".into()))
                }
            }
            Some('+') => {
                self.one(Token::Add)
            }
            Some('-') => {
                self.one(Token::Sub)
            }
            Some('*') => {
                self.one(Token::Mul)
            }
            Some('/') => {
                self.one(Token::Div)
            }
            Some('%') => {
                self.one(Token::Rem)
            }
            _ => Err(NetrineError::error(
                    self.span(),
                    "unexpected character".into()))
        }
    }

    fn next_indent(&mut self) -> Result<Token> {
        let new_level = self.count_indent_level()?;
        let top_level = self.indent_level.pop().unwrap();

        if new_level > top_level {
            Ok(Token::Indent)
        } else if new_level < top_level {
            Ok(Token::Dedent)
        } else {
            self.mode = Mode::Regular;
            self.next_token()
        }
    }

    fn count_indent_level(&mut self) -> Result<usize> {
        if self.indent_style == IndentStyle::Unset {
            if self.curr == Some(' ') {
                self.indent_style = IndentStyle::Spaces;
            } else {
                self.indent_style = IndentStyle::Tabs;
            }
        }

        while self.is_indent(self.curr) {
            match self.indent_style {
                IndentStyle::Spaces if self.curr == Some(' ') => {
                    self.bump();
                }
                IndentStyle::Tabs if self.curr == Some('\t') => {
                    self.bump();
                }
                _ => return Err(NetrineError::error(
                                self.span(),
                                "mixed spaces and tabs".into()))
            }
        }

        Ok(self.indent())
    }

    fn next_string(&mut self, start: bool) -> Result<Token> {
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
                        Ok(Token::String)
                    } else {
                        Ok(Token::StringEnd)
                    };
                }
                Some('{') if self.peek == Some('{') => {
                    self.bump();
                    self.bump();
                }
                Some('{') => {
                    self.mode = Mode::Template;
                    return if start {
                        Ok(Token::StringStart)
                    } else {
                        Ok(Token::StringSlice)
                    };
                }
                Some('\n') |
                None => {
                    return Err(NetrineError::error(
                                self.span(),
                                "unterminated string".into()))
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
                            return Err(NetrineError::error(
                                        self.span(),
                                        "invalid escape character".into()))
                        }
                    }
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn one(&mut self, token: Token) -> Result<Token> {
        self.bump();
        Ok(token)
    }

    fn is_alpha(&self, chr: Option<char>) -> bool {
        matches!(
            chr,
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

    fn is_indent(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('\t') | Some(' '))
    }

    fn is_space(&self, chr: Option<char>) -> bool {
        matches!(chr, Some('\r') | Some('\t') | Some(' '))
    }

    fn lower(&mut self) -> Token {
        self.bump_while(Self::is_alpha);

        self.bump_while(|_, ch| {
            !matches!(ch, Some('\'' | '?' | '!'))
        });

        if let Some(keyword) = get_keyword(self.value()) {
            keyword
        } else {
            Token::Lower
        }
    }

    fn upper(&mut self) -> Token {
        self.bump_while(Self::is_alpha);
        Token::Upper
    }

    fn number(&mut self, prefix: bool) -> Token {
        if prefix {
            self.bump();
        }

        self.bump_while(Self::is_number);

        if self.curr == Some('.') {
            self.bump();
            self.bump_while(Self::is_number);
        }

        Token::Number
    }

    fn space(&mut self) -> Result<Token> {
        self.bump_while(Self::is_space);
        self.next_token()
    }

    fn comment(&mut self) -> Result<Token> {
        self.bump_while(|_, ch| {
            !matches!(ch, Some('\n') | None)
        });
        self.next_token()
    }

    fn bump_while<P>(&mut self, pred: P)
        where P: Fn(&Self, Option<char>) -> bool
    {
        while !pred(&self, self.curr) {
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

        assert_eq!(lexer.next_token().unwrap(), Token::Lower);
        assert_eq!(lexer.value(), "variable");
    }

    #[test]
    fn test_variant() {
        let mut lexer = Lexer::new("True or False");

        assert_eq!(lexer.next_token().unwrap(), Token::Upper);
        assert_eq!(lexer.value(), "True");
        
        assert_eq!(lexer.next_token().unwrap(), Token::Or);

        assert_eq!(lexer.next_token().unwrap(), Token::Upper);
        assert_eq!(lexer.value(), "False");
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("function if then else and or not");

        assert_eq!(lexer.next_token().unwrap(), Token::Fn);
        assert_eq!(lexer.next_token().unwrap(), Token::If);
        assert_eq!(lexer.next_token().unwrap(), Token::Then);
        assert_eq!(lexer.next_token().unwrap(), Token::Else);
        assert_eq!(lexer.next_token().unwrap(), Token::And);
        assert_eq!(lexer.next_token().unwrap(), Token::Or);
        assert_eq!(lexer.next_token().unwrap(), Token::Not);
    }

    #[test]
    fn test_operator() {
       let mut lexer = Lexer::new(": . .. |> => = := == != + - * / % > < >= <=");

       assert_eq!(lexer.next_token().unwrap(), Token::Colon);
       assert_eq!(lexer.next_token().unwrap(), Token::Dot);
       assert_eq!(lexer.next_token().unwrap(), Token::Range);
       assert_eq!(lexer.next_token().unwrap(), Token::Pipe);
       assert_eq!(lexer.next_token().unwrap(), Token::Arrow);
       assert_eq!(lexer.next_token().unwrap(), Token::Equals);
       assert_eq!(lexer.next_token().unwrap(), Token::Walrus);
       assert_eq!(lexer.next_token().unwrap(), Token::Eq);
       assert_eq!(lexer.next_token().unwrap(), Token::Ne);
       assert_eq!(lexer.next_token().unwrap(), Token::Add);
       assert_eq!(lexer.next_token().unwrap(), Token::Sub);
       assert_eq!(lexer.next_token().unwrap(), Token::Mul);
       assert_eq!(lexer.next_token().unwrap(), Token::Div);
       assert_eq!(lexer.next_token().unwrap(), Token::Rem);
       assert_eq!(lexer.next_token().unwrap(), Token::Gt);
       assert_eq!(lexer.next_token().unwrap(), Token::Lt);
       assert_eq!(lexer.next_token().unwrap(), Token::Ge);
       assert_eq!(lexer.next_token().unwrap(), Token::Le);
    }

    #[test]
    fn test_integer() {
        let mut lexer = Lexer::new("42");

        assert_eq!(lexer.next_token().unwrap(), Token::Number);
        assert_eq!(lexer.value(), "42");
    }

    #[test]
    fn test_float() {
        let mut lexer = Lexer::new("3.14519");

        assert_eq!(lexer.next_token().unwrap(), Token::Number);
        assert_eq!(lexer.value(), "3.14519");
    }

    #[test]
    fn test_simple_string() {
        let mut lexer = Lexer::new("\"Hello, World\"");

        assert_eq!(lexer.next_token().unwrap(), Token::String);
        assert_eq!(lexer.value(), "\"Hello, World\"");
    }

    #[test]
    fn test_complex_string() {
        let mut lexer = Lexer::new(r#""src = \"y = 42\"""#);

        assert_eq!(lexer.next_token().unwrap(), Token::String);
        assert_eq!(lexer.value(), "\"src = \\\"y = 42\\\"\"");
    }

    #[test]
    fn test_string_template() {
        let mut lexer = Lexer::new(r#""Hello, {name}""#);

        assert_eq!(lexer.next_token().unwrap(), Token::StringStart);
        assert_eq!(lexer.value(), r#""Hello, "#);
        assert_eq!(lexer.mode, Mode::Template);
        assert_eq!(lexer.next_token().unwrap(), Token::LBrace);
        assert_eq!(lexer.next_token().unwrap(), Token::Lower);
        assert_eq!(lexer.value(), "name");
        assert_eq!(lexer.next_token().unwrap(), Token::RBrace);
        let _ = lexer.next_token(); // closing "
        assert_eq!(lexer.mode, Mode::Regular);
    }
}
