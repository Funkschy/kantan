pub(crate) mod ast;
mod error;
pub(crate) mod lexer;
pub(crate) mod parser;
mod token;

use self::error::{LexError, ParseError};
use self::token::Token;

type Scanned<'input> = Result<Spanned<Token<'input>>, Spanned<ParseError<'input>>>;
type CharPos = usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

impl<'a> From<Spanned<LexError>> for Spanned<ParseError<'a>> {
    fn from(err: Spanned<LexError>) -> Self {
        let span = err.span;
        let parse_err = ParseError::from(err.node);
        Spanned {
            span,
            node: parse_err,
        }
    }
}

pub trait Scanner<'input>: Iterator<Item = Scanned<'input>> {
    fn source(&self) -> &'input str;
}

#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::parser::Parser;

    #[test]
    fn test_parser_new_should_return_parser() {
        let source = "let test: i32 = 0;";
        let lexer = Lexer::new(&source);
        let parser = Parser::new(lexer);

        assert_eq!(source, parser.source);
    }
}
