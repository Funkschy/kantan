use std::str::CharIndices;
use std::{error, fmt};

use super::token::*;
use super::Scanner;

type CharPos = usize;
type Scanned<'input> = Result<Spanned<Token<'input>>, LexError>;

#[derive(Debug, Eq, PartialEq)]
pub struct Span {
    pub start: CharPos,
    pub end: CharPos,
}

impl Span {
    fn new(start: CharPos, end: CharPos) -> Self {
        Span { start, end }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

impl<T> Spanned<T> {
    fn new(start: CharPos, end: CharPos, node: T) -> Self {
        let span = Span { start, end };

        Spanned { span, node }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct InputPos {
    pos: CharPos,
    value: char,
}

impl InputPos {
    fn new_opt(value: Option<(CharPos, char)>) -> Option<Self> {
        let (pos, value) = value?;

        Some(InputPos { pos, value })
    }
}

pub struct LexError {
    ch: Span,
    cause: Option<String>,
}

impl LexError {
    fn new(ch: Span) -> Self {
        LexError { ch, cause: None }
    }

    fn with_cause(ch: Span, cause: &str) -> Self {
        LexError {
            ch,
            cause: Some(cause.to_owned()),
        }
    }

    fn as_str(&self) -> &str {
        "failed to lex token"
    }

    fn as_string(&self) -> String {
        let msg = self.as_str();

        if let Some(ref reason) = self.cause {
            format!("{}, because: {}", msg, reason)
        } else {
            msg.to_string()
        }
    }
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl error::Error for LexError {
    fn description(&self) -> &str {
        self.as_str()
    }
}

pub struct Lexer<'input> {
    src: &'input str,
    chars: CharIndices<'input>,
    current: Option<InputPos>,
}

impl<'input> Lexer<'input> {
    pub fn new(src: &'input str) -> Self {
        let mut chars = src.char_indices();

        Lexer {
            src,
            current: InputPos::new_opt(chars.next()),
            chars,
        }
    }
}

impl<'input> Lexer<'input> {
    fn pos(&self) -> CharPos {
        if let Some(InputPos { pos, .. }) = self.current {
            return pos;
        }

        self.src.len()
    }

    fn slice(&self, start: CharPos, end: CharPos) -> &'input str {
        let end = if end > self.src.len() {
            self.src.len()
        } else {
            end
        };

        &self.src[start..end]
    }

    fn spanned<T>(&self, start: CharPos, t: T) -> Spanned<T> {
        Spanned::new(start, self.pos() - 1, t)
    }
}

impl<'input> Scanner<'input> for Lexer<'input> {
    fn source(&self) -> &'input str {
        &self.src
    }
}

macro_rules! consume_single {
    ($self:ident, $start:ident, $token:expr) => {{
        $self.advance();
        Ok($self.spanned($start, $token))
    }};
}

impl<'input> Lexer<'input> {
    fn advance(&mut self) -> Option<InputPos> {
        let curr = self.current?;
        self.current = InputPos::new_opt(self.chars.next());
        Some(curr)
    }

    fn read_while<P>(&mut self, predicate: P) -> &'input str
    where
        P: Fn(char) -> bool,
    {
        let start = self.pos();

        while let Some(InputPos { value, .. }) = self.current {
            if predicate(value) {
                self.advance();
            } else {
                break;
            }
        }

        self.slice(start, self.pos())
    }

    fn skip_whitespace(&mut self) {
        self.read_while(|c| c.is_whitespace());
    }

    fn scan_ident(&mut self) -> Scanned<'input> {
        let start = self.pos();
        let slice = self.read_while(|c| c.is_alphanumeric() || c == '_');
        if let Some(keyword) = self.check_keyword(start, slice) {
            return Ok(keyword);
        }
        Ok(self.spanned(start, Token::Ident(slice)))
    }

    fn check_keyword(
        &mut self,
        start: CharPos,
        slice: &'input str,
    ) -> Option<Spanned<Token<'input>>> {
        match slice {
            "i32" => Some(self.spanned(start, Token::TypeIdent(TypeIdent::I32))),
            "let" => Some(self.spanned(start, Token::Let)),
            _ => None,
        }
    }

    fn scan_dec_num(&mut self) -> Scanned<'input> {
        let start = self.pos();
        let slice = self.read_while(|c| c.is_digit(10));

        let int: i64 = slice.parse().map_err(|err: std::num::ParseIntError| {
            LexError::with_cause(Span::new(start, self.pos()), &format!("{}", err))
        })?;

        Ok(self.spanned(start, Token::DecLit(int)))
    }

    fn scan_token(&mut self) -> Option<Scanned<'input>> {
        self.skip_whitespace();
        let start = self.pos();

        let ch = self.current.map(|InputPos { value, .. }| value)?;

        let scanned: Scanned = match ch {
            '=' => consume_single!(self, start, Token::Equals),
            '+' => consume_single!(self, start, Token::Plus),
            '-' => consume_single!(self, start, Token::Minus),
            '*' => consume_single!(self, start, Token::Star),
            '/' => consume_single!(self, start, Token::Slash),
            ':' => consume_single!(self, start, Token::Colon),
            ';' => consume_single!(self, start, Token::Semi),
            '(' => consume_single!(self, start, Token::LParen),
            ')' => consume_single!(self, start, Token::RParen),
            c if c.is_alphabetic() => self.scan_ident(),
            c if c.is_digit(10) => self.scan_dec_num(),
            _ => {
                self.advance();
                Err(LexError::new(Span::new(start, start)))
            }
        };

        Some(scanned)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Scanned<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slice_returns_correct_substring() {
        let source = "hello world";
        let lexer = Lexer::new(source);

        let slice = lexer.slice(0, source.len());
        assert_eq!("hello world", slice);

        let source = "こんにちは";
        let mut lexer = Lexer::new(source);

        let slice = lexer.slice(0, source.len());
        assert_eq!("こんにちは", slice);

        lexer.advance();
        let pos = lexer.pos();
        let slice = lexer.slice(pos, source.len());

        assert_eq!("んにちは", slice);

        lexer.advance();
        let pos = lexer.pos();
        let slice = lexer.slice(pos, 1000);

        assert_eq!("にちは", slice);
    }

    #[test]
    fn pos_after_advance_equals_sizeof_char() {
        let source = "こんにちは";
        let mut lexer = Lexer::new(source);

        let pos = lexer.pos();
        assert_eq!(0, pos);

        lexer.advance();
        let pos = lexer.pos();
        let c = 'こ';
        assert_eq!(c.len_utf8(), pos);
    }

    #[test]
    fn test_read_while() {
        let source = "hello1 world";
        let mut lexer = Lexer::new(source);

        let slice = lexer.read_while(|c| c.is_alphabetic());
        assert_eq!("hello", slice);

        let source = "こんにちは";
        let mut lexer = Lexer::new(source);

        let slice = lexer.read_while(|c| c.is_alphabetic());
        assert_eq!("こんにちは", slice);

        let source = "hello1 world";
        let mut lexer = Lexer::new(source);

        let slice = lexer.read_while(|c| !c.is_digit(10));
        assert_eq!("hello", slice);
    }

    #[test]
    fn test_scan_unicode_identifier() {
        let source = "こんにちは";
        let mut lexer = Lexer::new(source);

        let ident = lexer.scan_token().unwrap().unwrap();
        let expected = Spanned::new(0, source.len() - 1, Token::Ident("こんにちは"));

        assert_eq!(expected, ident);
    }

    #[test]
    fn test_scan_declaration() {
        let source = "let test: i32 = (42);";
        let lexer = Lexer::new(source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        let expected = vec![
            Spanned::new(0, 2, Token::Let),
            Spanned::new(4, 7, Token::Ident("test")),
            Spanned::new(8, 8, Token::Colon),
            Spanned::new(10, 12, Token::TypeIdent(TypeIdent::I32)),
            Spanned::new(14, 14, Token::Equals),
            Spanned::new(16, 16, Token::LParen),
            Spanned::new(17, 18, Token::DecLit(42)),
            Spanned::new(19, 19, Token::RParen),
            Spanned::new(20, 20, Token::Semi),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_scan_illegal_char_should_return_some_err() {
        let source = "i32 `";
        let lexer = Lexer::new(source);

        let tokens: Vec<Scanned> = lexer.skip(1).map(|e| e).collect();
        let backtick = tokens.get(0);

        if let Some(Err(err)) = backtick {
            assert_eq!("failed to lex token", format!("{}", err));
        } else {
            panic!("Token should be some error");
        }
    }

    #[test]
    fn test_scan_operators() {
        let source = "=*+/-";
        let lexer = Lexer::new(source);

        let tokens: Vec<Token> = lexer.map(|e| e.unwrap().node).collect();
        let expected = vec![
            Token::Equals,
            Token::Star,
            Token::Plus,
            Token::Slash,
            Token::Minus,
        ];

        assert_eq!(expected, tokens);
    }
}
