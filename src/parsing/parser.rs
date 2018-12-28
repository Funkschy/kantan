use super::ast::*;
use super::error::LexError;
use super::token::*;
use super::*;
use std::iter::Peekable;

type ExprResult<'input> = Result<Expr<'input>, ParseError<'input>>;
type StmtResult<'input> = Result<Stmt<'input>, ParseError<'input>>;

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

    fn make_prefix_err(_token: &Spanned<Token<'input>>, cause: &str) -> ExprResult<'input> {
        Err(ParseError::PrefixError(cause.to_owned()))
    }

    fn make_infix_err(_token: &Spanned<Token<'input>>, cause: &str) -> ExprResult<'input> {
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
    pub fn parse(&mut self) -> Result<Program<'input>, ParseError<'input>> {
        let mut top_lvl_decls = vec![];

        while self.scanner.peek().is_some() {
            top_lvl_decls.push(self.top_lvl_decl()?);
        }

        Ok(Program(top_lvl_decls))
    }

    fn top_lvl_decl(&mut self) -> StmtResult<'input> {
        self.consume(Token::Fn)?;
        let name = self.consume_ident()?;
        let params = self.param_list()?;
        let body = self.block()?;
        Ok(Stmt::FnDecl { name, params, body })
    }

    // TODO parse parameters
    fn param_list(&mut self) -> Result<ParamList<'input>, ParseError<'input>> {
        self.consume(Token::LParen)?;
        self.consume(Token::RParen)?;
        Ok(ParamList(vec![]))
    }

    fn statement(&mut self) -> StmtResult<'input> {
        let expr = self.expression()?;
        self.consume(Token::Semi)?;
        Ok(Stmt::Expr(expr))
    }

    fn block(&mut self) -> Result<Block<'input>, ParseError<'input>> {
        self.consume(Token::LBrace)?;
        let mut stmts = vec![];

        while !self.peek_eq(Token::RBrace) {
            stmts.push(self.statement()?);
        }
        self.consume(Token::RBrace)?;

        Ok(Block(stmts))
    }

    pub fn expression(&mut self) -> ExprResult<'input> {
        self.parse_expression(Precedence::None)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ExprResult<'input> {
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

    fn peek_eq(&mut self, expected: Token<'input>) -> bool {
        self.scanner.peek().map_or(false, |peek| match peek {
            Ok(Spanned { node, .. }) => *node == expected,
            _ => false,
        })
    }

    fn consume_ident(&mut self) -> Result<&'input str, ParseError<'input>> {
        let next = self.advance()?;
        if let Token::Ident(ident) = next.node {
            Ok(ident)
        } else {
            Err(ParseError::ConsumeError(next.node))
        }
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

    fn infix(&mut self, token: &Spanned<Token<'input>>, left: Expr<'input>) -> ExprResult<'input> {
        let tok = token.node;
        match tok {
            Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                let right = self.parse_expression(tok.precedence())?;
                Ok(Expr::Binary(Box::new(left), tok, Box::new(right)))
            }
            _ => Self::make_infix_err(token, "Invalid Token in infix rule"),
        }
    }

    fn prefix(&mut self, token: &Spanned<Token<'input>>) -> ExprResult<'input> {
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
    fn test_parse_with_empty_main_returns_empty_fn_decl() {
        let source = "fn main() {}";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse().unwrap();
        assert_eq!(
            Program(vec![Stmt::FnDecl {
                name: "main",
                params: ParamList(vec![]),
                body: Block(vec![])
            }]),
            prg
        );
    }

    #[test]
    fn test_parsing_number_in_parentheses_should_just_return_number() {
        let source = "((((42))))";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression().unwrap();
        assert_eq!(Expr::DecLit(42), expr);
    }

    #[test]
    fn test_parsing_binary_operations_should_have_correct_precedence() {
        let source = "1 + 2 * 3";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression().unwrap();
        assert_eq!(
            Expr::Binary(
                Box::new(Expr::DecLit(1)),
                Token::Plus,
                Box::new(Expr::Binary(
                    Box::new(Expr::DecLit(2)),
                    Token::Star,
                    Box::new(Expr::DecLit(3))
                ))
            ),
            expr
        );

        let source = "2 * 3 + 1";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression().unwrap();
        assert_eq!(
            Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::DecLit(2)),
                    Token::Star,
                    Box::new(Expr::DecLit(3))
                )),
                Token::Plus,
                Box::new(Expr::DecLit(1)),
            ),
            expr
        );

        let source = "(2 + 3) * 1";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression().unwrap();
        assert_eq!(
            Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::DecLit(2)),
                    Token::Plus,
                    Box::new(Expr::DecLit(3))
                )),
                Token::Star,
                Box::new(Expr::DecLit(1)),
            ),
            expr
        );

        let source = "1 + (2 * 3)";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression().unwrap();
        assert_eq!(
            Expr::Binary(
                Box::new(Expr::DecLit(1)),
                Token::Plus,
                Box::new(Expr::Binary(
                    Box::new(Expr::DecLit(2)),
                    Token::Star,
                    Box::new(Expr::DecLit(3))
                ))
            ),
            expr
        );
;
    }
}
