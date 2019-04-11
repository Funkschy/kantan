use std::{iter::Peekable, str::CharIndices};

use super::{error::*, token::*, *};
use crate::{types::*, Source};

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

pub struct Lexer<'src> {
    source: &'src Source,
    src: &'src str,
    chars: Peekable<CharIndices<'src>>,
    current: Option<InputPos>,
    prev: Option<char>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src Source) -> Self {
        let src = &source.code;
        let mut chars = src.char_indices().peekable();

        Lexer {
            source,
            src,
            current: InputPos::new_opt(chars.next()),
            chars,
            prev: None,
        }
    }
}

impl<'src> Lexer<'src> {
    fn pos(&self) -> CharPos {
        if let Some(InputPos { pos, .. }) = self.current {
            return pos;
        }

        self.src.len()
    }

    fn slice(&self, start: CharPos, end: CharPos) -> &'src str {
        let end = if end > self.src.len() {
            self.src.len()
        } else {
            end
        };

        &self.src[start..end]
    }

    fn spanned<T>(&self, start: CharPos, t: T) -> Spanned<T> {
        Spanned::new(start, self.pos() - self.prev.map_or(0, char::len_utf8), t)
    }
}

impl<'src> Scanner<'src> for Lexer<'src> {
    fn source(&self) -> &'src Source {
        &self.source
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
    ($self:ident, $start:ident, $next_char:expr, $single_tok:expr, $double_tok:expr) => {{
        $self.advance();
        if let Some(InputPos { value: new, .. }) = $self.current {
            if new == $next_char {
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

impl<'src> Lexer<'src> {
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

    fn read_while<P>(&mut self, predicate: P) -> &'src str
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
        self.read_while(char::is_whitespace);
    }

    fn scan_ident(&mut self) -> Scanned<'src> {
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

    fn check_keyword(&mut self, start: CharPos, slice: &'src str) -> Option<Spanned<Token<'src>>> {
        Some(self.spanned(
            start,
            match slice {
                "i32" => Token::TypeIdent(Simple::I32),
                "f32" => Token::TypeIdent(Simple::F32),
                "string" => Token::TypeIdent(Simple::String),
                "bool" => Token::TypeIdent(Simple::Bool),
                "void" => Token::TypeIdent(Simple::Void),
                "return" => Token::Return,
                "let" => Token::Let,
                "def" => Token::Def,
                "if" => Token::If,
                "else" => Token::Else,
                "import" => Token::Import,
                "extern" => Token::Extern,
                "while" => Token::While,
                "type" => Token::Type,
                "struct" => Token::Struct,
                "null" => Token::NullLit,
                "new" => Token::New,
                "delete" => Token::Delete,
                "sizeof" => Token::Sizeof,
                _ => return None,
            },
        ))
    }

    fn scan_num(&mut self) -> Scanned<'src> {
        let start = self.pos();
        let slice = self.read_while(|c| c.is_digit(10));

        if let Some(InputPos { value: '.', .. }) = self.current {
            if let Some((_, peek)) = self.chars.peek() {
                if peek.is_digit(10) {
                    // consume '.'
                    self.advance();
                    self.read_while(|c| c.is_digit(10));
                    let slice = self.slice(start, self.pos());
                    return Ok(self.spanned(start, Token::FloatLit(slice)));
                }
            }
        }

        Ok(self.spanned(start, Token::DecLit(slice)))
    }

    fn scan_string(&mut self) -> Scanned<'src> {
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

    fn scan_token(&mut self) -> Option<Scanned<'src>> {
        self.skip_whitespace();
        let start = self.pos();

        let ch = self.current.map(|InputPos { value, .. }| value)?;

        let scanned: Scanned = match ch {
            '=' => consume_double!(self, start, Token::Equals, Token::EqualsEquals),
            '!' => consume_double!(self, start, '=', Token::Bang, Token::BangEquals),
            '<' => consume_double!(self, start, '=', Token::Smaller, Token::SmallerEquals),
            '>' => consume_double!(self, start, '=', Token::Greater, Token::GreaterEquals),
            '&' => consume_double!(self, start, Token::Ampersand, Token::AmpersandAmpersand),
            '+' => consume_single!(self, start, Token::Plus),
            '-' => consume_single!(self, start, Token::Minus),
            '*' => consume_single!(self, start, Token::Star),
            '/' => {
                if let Some((_, '/')) = self.chars.peek() {
                    self.read_while(|c| c != '\n');
                    return self.scan_token();
                }
                consume_single!(self, start, Token::Slash)
            }
            ':' => consume_single!(self, start, Token::Colon),
            ';' => consume_single!(self, start, Token::Semi),
            '(' => consume_single!(self, start, Token::LParen),
            ')' => consume_single!(self, start, Token::RParen),
            '{' => consume_single!(self, start, Token::LBrace),
            '}' => consume_single!(self, start, Token::RBrace),
            '.' => {
                let dots = self.read_while(|c| c == '.');
                Ok(self.spanned(
                    start,
                    match dots.len() {
                        1 => Token::Dot,
                        3 => Token::TripleDot,
                        _ => {
                            return Some(Err(Spanned::new(
                                start,
                                self.pos() - 1,
                                ParseError::LexError(LexError::with_cause("too many '.'")),
                            )));
                        }
                    },
                ))
            }
            ',' => consume_single!(self, start, Token::Comma),
            '"' => self.scan_string(),
            c if c.is_alphabetic() => self.scan_ident(),
            c if c.is_digit(10) => self.scan_num(),
            _ => {
                self.advance();
                let span = Span::new(start, start);
                Err(Spanned {
                    span,
                    node: ParseError::LexError(LexError::default()),
                })
            }
        };

        Some(scanned)
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Scanned<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_double_dot_returns_error() {
        let source = Source::new("main", "..");
        let mut lexer = Lexer::new(&source);

        let tok = lexer.next().unwrap();
        assert!(tok.is_err());
        let err = tok.unwrap_err();
        assert_eq!(
            "Failed to lex token, because: too many '.'",
            err.node.to_string()
        );
    }

    #[test]
    fn test_scan_triple_dot() {
        let source = Source::new("main", ". ...");
        let lexer = Lexer::new(&source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        assert_eq!(
            vec![
                Spanned::new(0, 1, Token::Dot),
                Spanned::new(2, 4, Token::TripleDot),
            ],
            tokens
        );
    }

    #[test]
    fn test_float_lit_is_scanned_correctly() {
        let source = Source::new("main", ".1. 1.0 1");

        let lexer = Lexer::new(&source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        assert_eq!(
            vec![
                Spanned::new(0, 1, Token::Dot),
                Spanned::new(1, 1, Token::DecLit("1")),
                Spanned::new(2, 2, Token::Dot),
                Spanned::new(4, 6, Token::FloatLit("1.0")),
                Spanned::new(8, 8, Token::DecLit("1")),
            ],
            tokens
        );
    }

    #[test]
    fn test_comments_are_ignored() {
        let source = Source::new(
            "main",
            "1 + //
            2",
        );

        let lexer = Lexer::new(&source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        assert_eq!(
            vec![
                Spanned::new(0, 1, Token::DecLit("1")),
                Spanned::new(2, 2, Token::Plus),
                Spanned::new(19, 19, Token::DecLit("2")),
            ],
            tokens
        );
    }

    #[test]
    fn test_scan_smaller_and_smaller_equals() {
        let source = Source::new("main", "1 <= 2 <=2< =3");
        let lexer = Lexer::new(&source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        assert_eq!(
            vec![
                Spanned::new(0, 1, Token::DecLit("1")),
                Spanned::new(2, 3, Token::SmallerEquals),
                Spanned::new(5, 5, Token::DecLit("2")),
                Spanned::new(7, 8, Token::SmallerEquals),
                Spanned::new(9, 9, Token::DecLit("2")),
                Spanned::new(10, 10, Token::Smaller),
                Spanned::new(12, 12, Token::Equals),
                Spanned::new(13, 13, Token::DecLit("3")),
            ],
            tokens
        );
    }

    #[test]
    fn test_scan_string_should_return_correct_string_without_parens() {
        let source = Source::new("main", r#""hello world""#);
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
        let source = Source::new("main", r#"let s = "";"#);
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
        let source = Source::new("main", r#""""#);
        let mut lexer = Lexer::new(&source);

        let s = lexer.scan_token();
        assert_eq!(Some(Ok(Spanned::new(1, 1, Token::StringLit("")))), s);
    }

    #[test]
    fn test_scan_string_should_return_lexerror_when_reaching_eof() {
        let source = Source::new("main", r#""hello world"#);
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
        let source = Source::new("main", "hello world");
        let lexer = Lexer::new(&source);

        let slice = lexer.slice(0, source.code.len());
        assert_eq!("hello world", slice);

        let source = Source::new("main", "こんにちは");
        let mut lexer = Lexer::new(&source);

        let slice = lexer.slice(0, source.code.len());
        assert_eq!("こんにちは", slice);

        lexer.advance();
        let pos = lexer.pos();
        let slice = lexer.slice(pos, source.code.len());

        assert_eq!("んにちは", slice);

        lexer.advance();
        let pos = lexer.pos();
        let slice = lexer.slice(pos, 1000);

        assert_eq!("にちは", slice);
    }

    #[test]
    fn pos_after_advance_equals_sizeof_char() {
        let source = Source::new("main", "こんにちは");
        let mut lexer = Lexer::new(&source);

        let pos = lexer.pos();
        assert_eq!(0, pos);

        lexer.advance();
        let pos = lexer.pos();
        let c = 'こ';
        assert_eq!(c.len_utf8(), pos);
    }

    #[test]
    fn test_read_while() {
        let source = Source::new("main", "hello1 world");
        let mut lexer = Lexer::new(&source);

        let slice = lexer.read_while(|c| c.is_alphabetic());
        assert_eq!("hello", slice);

        let source = Source::new("main", "こんにちは");
        let mut lexer = Lexer::new(&source);

        let slice = lexer.read_while(|c| c.is_alphabetic());
        assert_eq!("こんにちは", slice);

        let source = Source::new("main", "hello1 world");
        let mut lexer = Lexer::new(&source);

        let slice = lexer.read_while(|c| !c.is_digit(10));
        assert_eq!("hello", slice);
    }

    #[test]
    fn test_scan_non_ascii_identifier_should_return_error() {
        let source = Source::new("main", "let こんにちは");
        let mut lexer = Lexer::new(&source);

        lexer.scan_ident().unwrap();
        let ident = lexer.scan_token().unwrap();

        assert_eq!(
            Err(Spanned::new(
                4,
                source.code.len() - 'は'.len_utf8(),
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
        let source = Source::new("main", r#""こんにちは""#);
        let mut lexer = Lexer::new(&source);

        let ident = lexer.scan_token().unwrap().unwrap();
        let expected = Spanned::new(
            1,
            source.code.len() - 2,
            Token::StringLit("こんにちは"),
        );

        assert_eq!(expected, ident);
    }

    #[test]
    fn test_scan_declaration() {
        let source = Source::new("main", "let test: i32 = (42);");
        let lexer = Lexer::new(&source);

        let tokens: Vec<Spanned<Token>> = lexer.map(|e| e.unwrap()).collect();
        let expected = vec![
            Spanned::new(0, 2, Token::Let),
            Spanned::new(4, 7, Token::Ident("test")),
            Spanned::new(8, 8, Token::Colon),
            Spanned::new(10, 12, Token::TypeIdent(Simple::I32)),
            Spanned::new(14, 14, Token::Equals),
            Spanned::new(16, 16, Token::LParen),
            Spanned::new(17, 18, Token::DecLit("42")),
            Spanned::new(19, 19, Token::RParen),
            Spanned::new(20, 20, Token::Semi),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_scan_double_equals() {
        let source = Source::new("main", "= == =");
        let lexer = Lexer::new(&source);

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
        let source = Source::new("main", "i32 `");
        let lexer = Lexer::new(&source);

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
        let source = Source::new("main", "=*+/-");
        let lexer = Lexer::new(&source);

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
        let source = Source::new("main", "(((42)))");
        let lexer = Lexer::new(&source);

        let tokens: Vec<Token> = lexer.map(|e| e.unwrap().node).collect();
        let expected = vec![
            Token::LParen,
            Token::LParen,
            Token::LParen,
            Token::DecLit("42"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ];

        assert_eq!(expected, tokens);
    }
}
