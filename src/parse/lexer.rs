use std::str::CharIndices;

use super::error::*;
use super::token::*;
use super::*;
use crate::types::Type;

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

pub struct Lexer<'input> {
    src: &'input str,
    chars: CharIndices<'input>,
    current: Option<InputPos>,
    prev: Option<char>,
}

impl<'input> Lexer<'input> {
    pub fn new(src: &'input str) -> Self {
        let mut chars = src.char_indices();

        Lexer {
            src,
            current: InputPos::new_opt(chars.next()),
            chars,
            prev: None,
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
        Spanned::new(start, self.pos() - self.prev.map_or(0, |p| p.len_utf8()), t)
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

macro_rules! consume_double {
    ($self:ident, $start:ident, $single_tok:expr, $double_tok:expr) => {{
        let InputPos { value: tok, .. } = $self.current.unwrap();
        $self.advance();
        if let Some(InputPos { value: new, .. }) = $self.current {
            if new == tok {
                $self.advance();
                Ok($self.spanned($start, $double_tok))
            } else {
                Ok($self.spanned($start, $single_tok))
            }
        } else {
            Ok($self.spanned($start, $single_tok))
        }
    }};
}

impl<'input> Lexer<'input> {
    fn advance(&mut self) -> Option<InputPos> {
        let curr = self.current?;
        if self.pos() > 0 {
            self.prev = Some({
                let InputPos { value: prev, .. } = curr;
                prev
            });
        }
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

        if !slice.is_ascii() {
            Err(self.spanned(
                start,
                ParseError::LexError(LexError::with_cause(
                    "Non ascii identifiers are currently not supported",
                )),
            ))
        } else {
            Ok(self.spanned(start, Token::Ident(slice)))
        }
    }

    fn check_keyword(
        &mut self,
        start: CharPos,
        slice: &'input str,
    ) -> Option<Spanned<Token<'input>>> {
        match slice {
            "i32" => Some(self.spanned(start, Token::TypeIdent(Type::I32))),
            "let" => Some(self.spanned(start, Token::Let)),
            "fn" => Some(self.spanned(start, Token::Fn)),
            "if" => Some(self.spanned(start, Token::If)),
            _ => None,
        }
    }

    fn scan_dec_num(&mut self) -> Scanned<'input> {
        let start = self.pos();
        let slice = self.read_while(|c| c.is_digit(10));

        let int: i64 = slice.parse().map_err(|err: std::num::ParseIntError| {
            Spanned::new(start, self.pos(), LexError::with_cause(&err.to_string()))
        })?;

        Ok(self.spanned(start, Token::DecLit(int)))
    }

    fn scan_string(&mut self) -> Scanned<'input> {
        self.advance();
        let start = self.pos();
        let slice = self.read_while(|c| c != '"');

        // consume last '"'
        if self.advance().is_none() {
            let pos = self.pos();
            Err(Spanned::new(
                pos,
                pos,
                LexError::with_cause("Reached end of file while reading string"),
            ))?;
        }

        let mut spanned = self.spanned(start, Token::StringLit(slice));
        // remove trailing "
        if spanned.span.end - spanned.span.start > 0 {
            spanned.span.end -= 1;
        }
        Ok(spanned)
    }

    fn scan_token(&mut self) -> Option<Scanned<'input>> {
        self.skip_whitespace();
        let start = self.pos();

        let ch = self.current.map(|InputPos { value, .. }| value)?;

        let scanned: Scanned = match ch {
            '=' => consume_double!(self, start, Token::Equals, Token::EqualsEquals),
            '+' => consume_single!(self, start, Token::Plus),
            '-' => consume_single!(self, start, Token::Minus),
            '*' => consume_single!(self, start, Token::Star),
            '/' => consume_single!(self, start, Token::Slash),
            ':' => consume_single!(self, start, Token::Colon),
            ';' => consume_single!(self, start, Token::Semi),
            '(' => consume_single!(self, start, Token::LParen),
            ')' => consume_single!(self, start, Token::RParen),
            '{' => consume_single!(self, start, Token::LBrace),
            '}' => consume_single!(self, start, Token::RBrace),
            '"' => self.scan_string(),
            c if c.is_alphabetic() => self.scan_ident(),
            c if c.is_digit(10) => self.scan_dec_num(),
            _ => {
                self.advance();
                let span = Span::new(start, start);
                Err(Spanned {
                    span,
                    node: ParseError::LexError(LexError::new()),
                })
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
    fn test_scan_string_should_return_correct_string_without_parens() {
        let source = r#""hello world""#;
        let mut lexer = Lexer::new(&source);

        let s = lexer.scan_token();
        assert_eq!(
            Some(Ok(Spanned::new(1, 11, Token::StringLit("hello world")))),
            s
        );

        assert_eq!(None, lexer.scan_token());
    }

    #[test]
    fn test_scan_string_decl() {
        let source = r#"let s = "";"#;
        let lexer = Lexer::new(&source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        assert_eq!(
            vec![
                Spanned::new(0, 2, Token::Let),
                Spanned::new(4, 4, Token::Ident("s")),
                Spanned::new(6, 6, Token::Equals),
                Spanned::new(9, 9, Token::StringLit("")),
                Spanned::new(10, 10, Token::Semi),
            ],
            tokens
        );
    }

    #[test]
    fn test_scan_empty_string_should_return_correct_string_without_parens() {
        let source = r#""""#;
        let mut lexer = Lexer::new(&source);

        let s = lexer.scan_token();
        assert_eq!(Some(Ok(Spanned::new(1, 1, Token::StringLit("")))), s);
    }

    #[test]
    fn test_scan_string_should_return_lexerror_when_reaching_eof() {
        let source = r#""hello world"#;
        let mut lexer = Lexer::new(&source);

        let s = lexer.scan_token();
        assert_eq!(
            Some(Err(Spanned::new(
                12,
                12,
                ParseError::from(LexError::with_cause(
                    "Reached end of file while reading string"
                ))
            ))),
            s
        );
    }

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
    fn test_scan_non_ascii_identifier_should_return_error() {
        let source = "let こんにちは";
        let mut lexer = Lexer::new(source);

        lexer.scan_ident().unwrap();
        let ident = lexer.scan_token().unwrap();

        assert_eq!(
            Err(Spanned::new(
                4,
                source.len() - 'は'.len_utf8(),
                ParseError::LexError(LexError::with_cause(
                    "Non ascii identifiers are currently not supported"
                ))
            )),
            ident
        );

        assert_eq!(None, lexer.scan_token());
    }

    #[test]
    fn test_scan_unicode_string_literal() {
        let source = r#""こんにちは""#;
        let mut lexer = Lexer::new(source);

        let ident = lexer.scan_token().unwrap().unwrap();
        let expected = Spanned::new(1, source.len() - 2, Token::StringLit("こんにちは"));

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
            Spanned::new(10, 12, Token::TypeIdent(Type::I32)),
            Spanned::new(14, 14, Token::Equals),
            Spanned::new(16, 16, Token::LParen),
            Spanned::new(17, 18, Token::DecLit(42)),
            Spanned::new(19, 19, Token::RParen),
            Spanned::new(20, 20, Token::Semi),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_scan_double_equals() {
        let source = "= == =";
        let lexer = Lexer::new(source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        let expected = vec![
            Spanned::new(0, 1, Token::Equals),
            Spanned::new(2, 3, Token::EqualsEquals),
            Spanned::new(5, 5, Token::Equals),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_scan_illegal_char_should_return_some_err() {
        let source = "i32 `";
        let lexer = Lexer::new(source);

        let tokens: Vec<Scanned> = lexer.skip(1).map(|e| e).collect();
        let backtick = tokens.get(0);

        if let Some(Err(Spanned { node: err, .. })) = backtick {
            assert_eq!("Failed to lex token", err.to_string());
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

    #[test]
    fn test_scan_number_in_multiple_parens() {
        let source = "(((42)))";
        let lexer = Lexer::new(source);

        let tokens: Vec<Token> = lexer.map(|e| e.unwrap().node).collect();
        let expected = vec![
            Token::LParen,
            Token::LParen,
            Token::LParen,
            Token::DecLit(42),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];

        assert_eq!(expected, tokens);
    }
}
