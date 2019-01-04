use std::iter::Peekable;

use super::{ast::*, error::LexError, token::*, *};

type ExprResult<'input> = Result<Spanned<Expr<'input>>, Spanned<ParseError<'input>>>;
type StmtResult<'input> = Result<Stmt<'input>, Spanned<ParseError<'input>>>;

fn as_err_stmt<'input>(err: Spanned<ParseError<'input>>) -> Stmt<'input> {
    Stmt::Expr(Spanned::new(
        err.span.start,
        err.span.end,
        Expr::Error(err.node),
    ))
}

pub struct Parser<'input, I>
where
    I: Scanner<'input>,
{
    pub(crate) source: &'input str,
    pub(crate) err_count: usize,
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
            err_count: 0,
        }
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Scanner<'input>,
{
    pub fn parse(&mut self) -> Program<'input> {
        let mut top_lvl_decls = vec![];
        while self.scanner.peek().is_some() {
            let decl = self.top_lvl_decl();
            if let Ok(decl) = decl {
                top_lvl_decls.push(decl);
            } else if let Err(err) = decl {
                top_lvl_decls.push(as_err_stmt(err));
            }
        }

        Program(top_lvl_decls)
    }

    fn top_lvl_decl(&mut self) -> StmtResult<'input> {
        // TODO: error collection
        self.consume(Token::Fn)?;
        let name = self.consume_ident()?;
        let params = self.param_list()?;
        let body = self.block()?;
        Ok(Stmt::FnDecl { name, params, body })
    }

    // TODO parse parameters
    fn param_list(&mut self) -> Result<ParamList<'input>, Spanned<ParseError<'input>>> {
        self.consume(Token::LParen)?;
        self.consume(Token::RParen)?;
        Ok(ParamList(vec![]))
    }

    fn block(&mut self) -> Result<Block<'input>, Spanned<ParseError<'input>>> {
        self.consume(Token::LBrace)?;
        let mut stmts = vec![];

        while !self.at_end() && !self.peek_eq(Token::RBrace) {
            let stmt = self.statement();
            if let Ok(stmt) = stmt {
                stmts.push(stmt);
            } else if let Err(err) = stmt {
                stmts.push(as_err_stmt(err));
            }
            self.consume(Token::Semi)?;
        }

        self.consume(Token::RBrace)?;
        Ok(Block(stmts))
    }

    fn statement(&mut self) -> StmtResult<'input> {
        let stmt = if self.peek_eq(Token::Let) {
            self.let_decl()?
        } else {
            let expr = self.expression();
            Stmt::Expr(expr)
        };

        Ok(stmt)
    }

    fn let_decl(&mut self) -> StmtResult<'input> {
        self.consume(Token::Let)?;

        let ident = self.consume_ident();
        if ident.is_err() {
            self.err_count += 1;
            self.sync();
        }
        let name = ident?;

        self.consume(Token::Equals)?;
        let value = self.expression();
        Ok(Stmt::VarDecl { name, value })
    }

    pub fn expression(&mut self) -> Spanned<Expr<'input>> {
        let as_err_stmt =
            |err: Spanned<ParseError<'input>>| Spanned::from_span(err.span, Expr::Error(err.node));

        self.parse_expression(Precedence::None)
            .unwrap_or_else(|err| {
                self.sync();
                self.err_count += 1;
                as_err_stmt(err)
            })
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
            self.make_lex_err(span, "Unexpected end of file")
        })
    }

    fn peek_eq(&mut self, expected: Token<'input>) -> bool {
        self.scanner.peek().map_or(false, |peek| match peek {
            Ok(Spanned { node, .. }) => *node == expected,
            _ => false,
        })
    }

    fn consume_ident(&mut self) -> Result<Spanned<&'input str>, Spanned<ParseError<'input>>> {
        let next = self.advance()?;
        if let Spanned {
            span,
            node: Token::Ident(ident),
        } = next
        {
            Ok(Spanned::from_span(span, ident))
        } else {
            // TODO: better error
            Err(self.make_consume_err(&next, Token::Ident("")).unwrap_err())
        }
    }

    fn consume(&mut self, expected: Token<'input>) -> Scanned<'input> {
        let next = self.advance()?;
        if next.node == expected {
            Ok(next)
        } else {
            self.make_consume_err(&next, expected)
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

    fn infix(
        &mut self,
        token: &Spanned<Token<'input>>,
        left: Spanned<Expr<'input>>,
    ) -> ExprResult<'input> {
        let tok = token.node;
        match tok {
            Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                let right = self.parse_expression(tok.precedence())?;
                Ok(Spanned::new(
                    left.span.start,
                    right.span.end,
                    Expr::Binary(Box::new(left.node), *token, Box::new(right.node)),
                ))
            }
            Token::Equals => {
                if let Expr::Ident(name) = left.node {
                    let value = Box::new(self.expression());
                    Ok(Spanned::new(
                        left.span.start,
                        (*value).span.end,
                        Expr::Assign {
                            name,
                            eq: *token,
                            value,
                        },
                    ))
                } else {
                    self.make_infix_err(token)
                }
            }
            _ => self.make_infix_err(token),
        }
    }

    fn prefix(&mut self, token: &Spanned<Token<'input>>) -> ExprResult<'input> {
        let ok_spanned = |expr| Ok(Spanned::from_span(token.span, expr));

        match token.node {
            Token::DecLit(lit) => ok_spanned(Expr::DecLit(lit)),
            Token::StringLit(lit) => ok_spanned(Expr::StringLit(lit)),
            Token::LParen => {
                let mut expr = self.expression();
                self.consume(Token::RParen)?;
                expr.span.start -= 1;
                expr.span.end += 1;
                Ok(expr)
            }
            Token::Minus => {
                let next = self.expression();
                Ok(Spanned::new(
                    token.span.start,
                    next.span.end,
                    Expr::Negate(Box::new(next.node)),
                ))
            }
            Token::Ident(ref name) => ok_spanned(Expr::Ident(name)),
            _ => self.make_prefix_err(token),
        }
    }

    fn make_lex_err(&mut self, span: Span, cause: &str) -> Scanned<'input> {
        self.err_count += 1;
        Err(Spanned {
            span,
            node: ParseError::LexError(LexError::with_cause(cause)),
        })
    }

    fn make_prefix_err(&mut self, token: &Spanned<Token<'input>>) -> ExprResult<'input> {
        self.err_count += 1;
        let s = format!("Invalid token in prefix rule: '{}'", token.node);
        Err(Spanned {
            span: token.span,
            node: ParseError::PrefixError(s),
        })
    }

    fn make_infix_err(&mut self, token: &Spanned<Token<'input>>) -> ExprResult<'input> {
        self.err_count += 1;
        let s = format!("Invalid token in infix rule: '{}'", token.node);
        Err(Spanned {
            span: token.span,
            node: ParseError::InfixError(s),
        })
    }

    fn make_consume_err(
        &mut self,
        actual: &Spanned<Token<'input>>,
        expected: Token<'input>,
    ) -> Scanned<'input> {
        self.err_count += 1;
        Err(Spanned {
            span: actual.span,
            node: ParseError::ConsumeError {
                actual: actual.node,
                expected,
            },
        })
    }

    fn sync(&mut self) {
        self.scanner.next();

        'outer: while let Some(next) = self.scanner.peek() {
            match next {
                Ok(Spanned { node, .. }) if *node == Token::Semi => {
                    break 'outer;
                }
                _ => {}
            }

            if let Some(Ok(Spanned { node, .. })) = self.scanner.next() {
                match node {
                    Token::Let | Token::Fn => break 'outer,
                    _ => {}
                }
            }
        }
    }

    fn at_end(&mut self) -> bool {
        self.scanner.peek().is_none()
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::Lexer;
    use super::*;

    #[test]
    fn test_parse_with_one_error_should_have_err_count_of_one() {
        let source = "fn err() { 1 ++ 2; 3 + 4; }";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![Stmt::FnDecl {
                name: Spanned::new(3, 5, "err"),
                params: ParamList(vec![]),
                body: Block(vec![
                    Stmt::Expr(Spanned {
                        node: Expr::Error(ParseError::PrefixError(
                            "Invalid token in prefix rule: '+'".to_owned()
                        ),),
                        span: Span::new(14, 14)
                    }),
                    Stmt::Expr(Spanned {
                        node: Expr::Binary(
                            Box::new(Expr::DecLit(3)),
                            Spanned::new(21, 21, Token::Plus),
                            Box::new(Expr::DecLit(4))
                        ),
                        span: Span::new(19, 23)
                    })
                ])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_let_with_value_should_return_var_decl_stmt() {
        let source = "fn main() { let var = 5; }";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![Stmt::FnDecl {
                name: Spanned::new(3, 6, "main"),
                params: ParamList(vec![]),
                body: Block(vec![Stmt::VarDecl {
                    name: Spanned::new(16, 18, "var"),
                    value: Spanned::new(22, 22, Expr::DecLit(5))
                }])
            }]),
            prg
        );

        assert_eq!(0, parser.err_count);
    }

    #[test]
    fn test_parse_with_empty_main_returns_empty_fn_decl() {
        let source = "fn main() {}";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![Stmt::FnDecl {
                name: Spanned::new(3, 6, "main"),
                params: ParamList(vec![]),
                body: Block(vec![])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_with_one_stmt_in_main() {
        let source = "fn main() {1 + 1;}";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![Stmt::FnDecl {
                name: Spanned::new(3, 6, "main"),
                params: ParamList(vec![]),
                body: Block(vec![Stmt::Expr(Spanned::new(
                    11,
                    15,
                    Expr::Binary(
                        Box::new(Expr::DecLit(1)),
                        Spanned::new(13, 13, Token::Plus),
                        Box::new(Expr::DecLit(1))
                    )
                ))])
            }]),
            prg
        );

        assert_eq!(0, parser.err_count);
    }

    #[test]
    fn test_parsing_number_in_parentheses_should_just_return_number() {
        let source = "((((42))))";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression().node;
        assert_eq!(Expr::DecLit(42), expr);
    }

    #[test]
    fn test_parsing_binary_operations_should_have_correct_precedence() {
        let source = "1 + 2 * 3";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression();
        assert_eq!(
            Spanned {
                node: Expr::Binary(
                    Box::new(Expr::DecLit(1)),
                    Spanned::new(2, 2, Token::Plus),
                    Box::new(Expr::Binary(
                        Box::new(Expr::DecLit(2)),
                        Spanned::new(6, 6, Token::Star),
                        Box::new(Expr::DecLit(3))
                    ))
                ),
                span: Span::new(0, 8)
            },
            expr
        );

        let source = "2 * 3 + 1";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression();
        assert_eq!(
            Spanned {
                node: Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::DecLit(2)),
                        Spanned::new(2, 2, Token::Star),
                        Box::new(Expr::DecLit(3))
                    )),
                    Spanned::new(6, 6, Token::Plus),
                    Box::new(Expr::DecLit(1)),
                ),
                span: Span::new(0, 8)
            },
            expr
        );

        let source = "(2 + 3) * 1";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression();
        assert_eq!(
            Spanned {
                node: Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::DecLit(2)),
                        Spanned::new(3, 3, Token::Plus),
                        Box::new(Expr::DecLit(3))
                    )),
                    Spanned::new(8, 8, Token::Star),
                    Box::new(Expr::DecLit(1)),
                ),
                span: Span::new(0, 10)
            },
            expr
        );

        let source = "1 + (2 * 3)";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression();
        assert_eq!(
            Spanned {
                node: Expr::Binary(
                    Box::new(Expr::DecLit(1)),
                    Spanned::new(2, 2, Token::Plus),
                    Box::new(Expr::Binary(
                        Box::new(Expr::DecLit(2)),
                        Spanned::new(7, 7, Token::Star),
                        Box::new(Expr::DecLit(3))
                    ))
                ),
                span: Span::new(0, 10)
            },
            expr
        );
;
    }
}
