use std::hash;

pub(crate) mod ast;
mod error;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod token;

use self::{
    error::{LexError, ParseError},
    token::Token,
};
use crate::Source;

type Scanned<'src> = Result<Spanned<Token<'src>>, Spanned<ParseError<'src>>>;
type CharPos = usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub start: CharPos,
    pub end: CharPos,
}

impl Span {
    pub fn new(start: CharPos, end: CharPos) -> Self {
        Span { start, end }
    }
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

impl<T> Spanned<T> {
    pub fn new(start: CharPos, end: CharPos, node: T) -> Self {
        let span = Span { start, end };

        Spanned { span, node }
    }

    pub fn from_span(span: Span, node: T) -> Self {
        Self { span, node }
    }
}

impl<'a> From<Spanned<LexError>> for Spanned<ParseError<'a>> {
    fn from(err: Spanned<LexError>) -> Self {
        let span = err.span;
        let parse_err = ParseError::from(err.node);
        Self {
            span,
            node: parse_err,
        }
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            span: self.span,
            node: self.node.clone(),
        }
    }
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T: hash::Hash> hash::Hash for Spanned<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node && self.span == other.span
    }
}

impl<T: PartialEq> Eq for Spanned<T> {}

pub trait Scanner<'src>: Iterator<Item = Scanned<'src>> {
    fn source(&self) -> &'src Source;
}

#[cfg(test)]
mod tests {
    use super::{lexer::Lexer, parser::Parser};
    use crate::Source;

    #[test]
    fn test_parser_new_should_return_parser() {
        let source = Source::new("main", "let test: i32 = 0;");
        let lexer = Lexer::new(&source);
        let parser = Parser::new(lexer);

        assert_eq!(source, *parser.source);
    }
}
