use std::error;
use std::fmt;
use std::str::CharIndices;

type CharPos = usize;
type Scanned<'input> = Result<Spanned<Token<'input>>, LexError<'input>>;

pub struct Span {
    pub start: CharPos,
    pub end: CharPos,
}

pub struct Spanned<T> {
    pub spanned: Span,
    pub node: T,
}

pub enum Token<'input> {
    Ident(&'input str),
    DecLit(i64),
}

#[derive(Copy, Clone)]
pub struct InputPos {
    pos: CharPos,
    value: char,
}

impl InputPos {
    pub fn new(value: Option<(CharPos, char)>) -> Option<Self> {
        let (pos, value) = value?;

        Some(InputPos { pos: pos, value })
    }
}

pub struct LexError<'input> {
    tok: Spanned<Token<'input>>,
    cause: Option<&'input error::Error>,
}

impl<'input> LexError<'input> {
    fn as_str(&self) -> &str {
        "error while reading token"
    }
}

impl<'input> fmt::Debug for LexError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'input> fmt::Display for LexError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'input> error::Error for LexError<'input> {
    fn description(&self) -> &str {
        self.as_str()
    }

    fn cause(&self) -> Option<&error::Error> {
        None
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
            current: InputPos::new(chars.next()),
            chars,
        }
    }
}

impl<'input> Lexer<'input> {
    fn pos(&self) -> CharPos {
        if let Some(InputPos { pos, .. }) = self.current {
            return pos;
        }

        CharPos::from(self.src.len())
    }

    fn slice(&self, start: CharPos, end: CharPos) -> &'input str {
        let end = if end > self.src.len() {
            self.src.len()
        } else {
            end
        };

        &self.src[start..end]
    }
}

impl<'input> Lexer<'input> {
    fn advance(&mut self) -> Option<InputPos> {
        let curr = self.current?;
        self.current = InputPos::new(self.chars.next());
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

    fn scan_token(&mut self) -> Option<Scanned<'input>> {
        unimplemented!()
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
    use super::Lexer;

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
}
