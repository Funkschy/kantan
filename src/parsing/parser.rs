use super::ast::Expr;
use super::error::LexError;
use super::token::*;
use super::*;
use std::iter::Peekable;

type ParseResult<'input> = Result<Expr<'input>, ParseError<'input>>;

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

    fn make_lex_err(span: Span, cause: &str) -> Scanned<'input> {
        Err(ParseError::LexError(LexError::with_cause(span, cause)))
    }

    fn make_prefix_err(_token: &Spanned<Token<'input>>, cause: &str) -> ParseResult<'input> {
        Err(ParseError::PrefixError(cause.to_owned()))
    }

    fn make_infix_err(_token: &Spanned<Token<'input>>, cause: &str) -> ParseResult<'input> {
        Err(ParseError::InfixError(cause.to_owned()))
    }

    fn make_consume_err(token: Token<'input>) -> Scanned<'input> {
        Err(ParseError::ConsumeError(token))
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Scanner<'input>,
{
    pub fn expression(&mut self) -> ParseResult<'input> {
        self.parse_expression(Precedence::None)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<'input> {
        let token = self.advance()?;
        let mut left = self.prefix(&token)?;

        while self.next_higher_precedence(precedence) {
            let token = self.advance()?;
            left = self.infix(&token, left)?;
        }

        Ok(left)
    }

    fn advance(&mut self) -> Scanned<'input> {
        self.scanner.next().unwrap_or_else(|| {
            let len = self.source.len();
            let span = Span::new(len, len);
            Self::make_lex_err(span, "Unexpected end of file")
        })
    }

    fn consume(&mut self, expected: Token) -> Scanned<'input> {
        let next = self.advance()?;
        if next.node == expected {
            Ok(next)
        } else {
            Self::make_consume_err(next.node)
        }
    }

    fn next_higher_precedence(&mut self, precedence: Precedence) -> bool {
        self.scanner.peek().map_or(false, |scanned| {
            if let Ok(spanned) = scanned {
                spanned.node.precedence() > precedence
            } else {
                false
            }
        })
    }

    fn infix(&mut self, token: &Spanned<Token<'input>>, left: Expr<'input>) -> ParseResult<'input> {
        let tok = token.node;
        match tok {
            Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                let right = self.parse_expression(tok.precedence())?;
                Ok(Expr::Binary(Box::new(left), tok, Box::new(right)))
            }
            _ => Self::make_infix_err(token, "Invalid Token in infix rule"),
        }
    }

    fn prefix(&mut self, token: &Spanned<Token<'input>>) -> ParseResult<'input> {
        match token.node {
            Token::DecLit(lit) => Ok(Expr::DecLit(lit)),
            Token::LParen => {
                let expr = self.expression()?;
                self.consume(Token::RParen)?;
                Ok(expr)
            }
            _ => Self::make_prefix_err(token, "Invalid Token in prefix rule"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn test_parsing_number_in_parentheses_should_just_return_number() {
        let source = "((((42))))";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression().unwrap();
        assert_eq!(Expr::DecLit(42), expr);
    }
}
