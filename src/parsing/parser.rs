use super::Scanner;
use std::iter::Peekable;

pub struct Parser<'input, I>
where
    I: Scanner<'input>,
{
    pub source: &'input str,
    scanner: Peekable<I>,
}

impl<'input, I> Parser<'input, I>
where
    I: Scanner<'input>,
{
    pub fn new(scanner: I) -> Self {
        let source = scanner.source();
        let peekable = scanner.peekable();

        Parser {
            source,
            scanner: peekable,
        }
    }
}
