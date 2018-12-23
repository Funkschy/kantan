#[allow(dead_code)]
mod lexer;
mod parser;
mod token;

use self::lexer::*;
use self::token::Token;

pub trait Scanner<'input>: Iterator<Item = Result<Spanned<Token<'input>>, LexError>> {
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
