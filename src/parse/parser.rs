use std::{cell::Cell, iter::Peekable};

use super::{ast::*, error::LexError, token::*, *};
use crate::types::Type;

type ExprResult<'input> = Result<Spanned<Expr<'input>>, Spanned<ParseError<'input>>>;
type StmtResult<'input> = Result<Stmt<'input>, Spanned<ParseError<'input>>>;
type TopLvlResult<'input> = Result<TopLvl<'input>, Spanned<ParseError<'input>>>;

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
            let is_extern = self.peek_eq(Token::Extern);

            let decl = self.top_lvl_decl(is_extern);
            if let Ok(decl) = decl {
                top_lvl_decls.push(decl);
            } else if let Err(err) = decl {
                top_lvl_decls.push(TopLvl::Error(err));
                // TODO: fix sync
                self.sync();
            }
        }

        Program(top_lvl_decls)
    }

    fn top_lvl_decl(&mut self, is_extern: bool) -> TopLvlResult<'input> {
        // TODO: error collection

        if self.peek_eq(Token::Import) {
            return self.import();
        }

        if is_extern {
            self.consume(Token::Extern)?;
        }

        self.consume(Token::Fn)?;
        let name = self.consume_ident()?;
        let params = self.param_list()?;

        self.consume(Token::Colon)?;
        let ret_type = self.consume_type()?;

        let body = if !is_extern {
            self.block()?
        } else {
            self.consume(Token::Semi)?;
            Block::default()
        };

        Ok(TopLvl::FnDecl {
            name,
            params,
            body,
            ret_type,
            is_extern,
        })
    }

    fn import(&mut self) -> TopLvlResult<'input> {
        self.consume(Token::Import)?;
        let name = self.consume_ident()?;

        Ok(TopLvl::Import { name })
    }

    // TODO: parse parameters
    fn param_list(&mut self) -> Result<ParamList<'input>, Spanned<ParseError<'input>>> {
        self.consume(Token::LParen)?;

        if self.peek_eq(Token::RParen) {
            self.consume(Token::RParen)?;
            return Ok(ParamList(vec![]));
        }

        let mut params = vec![];

        while !self.peek_eq(Token::RParen) {
            let ident = self.consume_ident()?;
            self.consume(Token::Colon)?;
            // TODO: user defined types
            let ty = self.consume_type()?;
            params.push(Param::new(ident, ty.node));
        }

        self.consume(Token::RParen)?;
        Ok(ParamList(params))
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
        }

        self.consume(Token::RBrace)?;
        Ok(Block(stmts))
    }

    fn statement(&mut self) -> StmtResult<'input> {
        let stmt = if self.peek_eq(Token::Let) {
            let decl = self.let_decl();
            self.consume(Token::Semi)?;
            decl?
        } else if self.peek_eq(Token::If) {
            self.if_stmt()?
        } else if self.peek_eq(Token::Return) {
            self.return_stmt()?
        } else {
            let expr = self.expression();
            self.consume(Token::Semi)?;
            Stmt::Expr(expr)
        };

        Ok(stmt)
    }

    fn return_stmt(&mut self) -> StmtResult<'input> {
        self.consume(Token::Return)?;

        let ret = Ok(Stmt::Return(if self.peek_eq(Token::Semi) {
            None
        } else {
            Some(self.expression())
        }));

        self.consume(Token::Semi)?;
        ret
    }

    fn if_stmt(&mut self) -> StmtResult<'input> {
        self.consume(Token::If)?;

        let condition = self.expression();
        let then_block = self.block()?;

        let else_branch = if self.peek_eq(Token::Else) {
            self.consume(Token::Else)?;

            let else_branch = if self.peek_eq(Token::If) {
                let else_if = self.if_stmt()?;
                Else::IfStmt(else_if)
            } else {
                let block = self.block()?;
                Else::Block(block)
            };

            Some(Box::new(else_branch))
        } else {
            None
        };

        // TODO: else branch
        Ok(Stmt::If {
            condition,
            then_block,
            else_branch,
        })
    }

    fn let_decl(&mut self) -> StmtResult<'input> {
        self.consume(Token::Let)?;

        let ident = self.consume_ident();
        if ident.is_err() {
            self.err_count += 1;
            self.sync();
        }
        let name = ident?;

        let ty = if let Ok(true) = self.match_tok(Token::Colon) {
            Some(self.consume_type()?)
        } else {
            None
        };

        let ty = Cell::new(ty);

        let eq = self.consume(Token::Equals)?;

        let value = self.expression();
        Ok(Stmt::VarDecl {
            name,
            value,
            eq,
            ty,
        })
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

    fn eof(&mut self) -> Scanned<'input> {
        let len = self.source.len();
        let span = Span::new(len, len);
        self.make_lex_err(span, "Unexpected end of file")
    }

    fn advance(&mut self) -> Scanned<'input> {
        self.scanner.next().unwrap_or_else(|| self.eof())
    }

    fn match_tok(&mut self, expected: Token<'input>) -> Result<bool, Spanned<ParseError<'input>>> {
        if self.peek_eq(expected) {
            self.consume(expected)?;
            return Ok(true);
        }

        Ok(false)
    }

    fn peek_eq(&mut self, expected: Token<'input>) -> bool {
        self.scanner.peek().map_or(false, |peek| match peek {
            Ok(Spanned { node, .. }) => *node == expected,
            _ => false,
        })
    }

    fn consume_ident(&mut self) -> Result<Spanned<&'input str>, Spanned<ParseError<'input>>> {
        if let Some(peek) = self.scanner.peek().cloned() {
            return match peek {
                Ok(peek) => {
                    if let Spanned {
                        node: Token::Ident(ident),
                        span,
                    } = peek
                    {
                        self.advance()?;
                        return Ok(Spanned::from_span(span, ident));
                    } else {
                        let tok = Spanned::clone(&peek);
                        return Err(self
                            .make_consume_err(&tok, "Identifier".to_owned())
                            .unwrap_err());
                    }
                }
                Err(err) => Err(err),
            };
        }

        Err(self.eof().unwrap_err())
    }

    fn consume_type(&mut self) -> Result<Spanned<Type>, Spanned<ParseError<'input>>> {
        if let Some(peek) = self.scanner.peek().cloned() {
            return match peek {
                Ok(peek) => {
                    if let Spanned {
                        node: Token::TypeIdent(ty),
                        span,
                    } = peek
                    {
                        self.advance()?;
                        return Ok(Spanned::from_span(span, ty));
                    } else {
                        let tok = Spanned::clone(&peek);
                        return Err(self.make_consume_err(&tok, "Type".to_owned()).unwrap_err());
                    }
                }
                Err(err) => Err(err),
            };
        }

        Err(self.eof().unwrap_err())
    }

    fn consume(&mut self, expected: Token<'input>) -> Scanned<'input> {
        if let Some(peek) = self.scanner.peek() {
            if let Ok(peek) = peek {
                if peek.node == expected {
                    let next = self.advance()?;
                    return Ok(next);
                } else {
                    let tok = Spanned::clone(peek);
                    return self.make_consume_err(&tok, expected.to_string());
                }
            } else {
                return peek.clone();
            }
        }

        self.eof()
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
            Token::EqualsEquals
            | Token::SmallerEquals
            | Token::Smaller
            | Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash => {
                let right = self.parse_expression(tok.precedence())?;
                let right_span = right.span;
                let left_span = left.span;

                let expr = match tok {
                    Token::EqualsEquals | Token::SmallerEquals | Token::Smaller => {
                        Expr::BoolBinary(Box::new(left), *token, Box::new(right))
                    }
                    _ => Expr::Binary(
                        Box::new(left),
                        *token,
                        Box::new(Spanned::from_span(right.span, right.node)),
                    ),
                };
                Ok(Spanned::new(left_span.start, right_span.end, expr))
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
            Token::Dot => {
                let ident = self.consume_ident()?;
                Ok(Spanned::new(
                    left.span.start,
                    ident.span.end,
                    Expr::Access {
                        left: Box::new(left),
                        identifier: ident,
                    },
                ))
            }
            Token::LParen => {
                let valid = match left.node {
                    Expr::Ident(_) | Expr::Access { .. } => true,
                    _ => false,
                };

                if valid {
                    let arg_list = self.arg_list()?;
                    let end = self.consume(Token::RParen)?.span.end;

                    Ok(Spanned::new(
                        left.span.start,
                        end,
                        Expr::Call {
                            callee: Box::new(left),
                            args: arg_list,
                        },
                    ))
                } else {
                    // TODO: Implement meaningful error
                    unimplemented!("Implement meaningful error");
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
                    Expr::Negate(*token, Box::new(next)),
                ))
            }
            Token::Ident(ref name) => ok_spanned(Expr::Ident(name)),
            _ => self.make_prefix_err(token),
        }
    }

    fn arg_list(&mut self) -> Result<ArgList<'input>, Spanned<ParseError<'input>>> {
        let mut args = vec![];

        while !self.at_end() && !self.peek_eq(Token::RParen) {
            args.push(self.expression());
        }

        Ok(ArgList(args))
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
        expected: String,
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

        'outer: while let Some(peek) = self.scanner.peek() {
            match peek {
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
    fn test_parse_import_access() {
        let source = "fn main(): void { test.func(); }";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FnDecl {
                name: Spanned::new(3, 6, "main"),
                params: ParamList(vec![]),
                ret_type: Spanned::new(11, 14, Type::Void),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    18,
                    28,
                    Expr::Call {
                        callee: Box::new(Spanned::new(
                            18,
                            26,
                            Expr::Access {
                                left: Box::new(Spanned::new(18, 21, Expr::Ident("test"))),
                                identifier: Spanned::new(23, 26, "func")
                            }
                        )),
                        args: ArgList(vec![])
                    }
                ))])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_import() {
        let source = "import test\nfn main(): void {}";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![
                TopLvl::Import {
                    name: Spanned::new(7, 10, "test")
                },
                TopLvl::FnDecl {
                    name: Spanned::new(15, 18, "main"),
                    is_extern: false,
                    ret_type: Spanned::new(23, 26, Type::Void),
                    params: ParamList(vec![]),
                    body: Block(vec![])
                }
            ]),
            prg
        );
    }

    #[test]
    fn test_parse_access_through_identifier() {
        let source = "fn main(): void { test.fun(); }";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FnDecl {
                name: Spanned::new(3, 6, "main"),
                ret_type: Spanned::new(11, 14, Type::Void),
                is_extern: false,
                params: ParamList(vec![]),
                body: Block(vec![Stmt::Expr(Spanned::new(
                    18,
                    27,
                    Expr::Call {
                        callee: Box::new(Spanned::new(
                            18,
                            25,
                            Expr::Access {
                                left: Box::new(Spanned::new(18, 21, Expr::Ident("test"))),
                                identifier: Spanned::new(23, 25, "fun")
                            }
                        )),
                        args: ArgList(vec![])
                    }
                ))])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_call_without_args_should_return_call_expr_with_empty_args() {
        let source = "fn main(): void { test(); }";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FnDecl {
                name: Spanned::new(3, 6, "main"),
                ret_type: Spanned::new(11, 14, Type::Void),
                is_extern: false,
                params: ParamList(vec![]),
                body: Block(vec![Stmt::Expr(Spanned::new(
                    18,
                    23,
                    Expr::Call {
                        callee: Box::new(Spanned::new(18, 21, Expr::Ident("test"))),
                        args: ArgList(vec![])
                    }
                ))])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_with_one_error_should_have_err_count_of_one() {
        let source = "fn err(): void { 1 ++ 2; 3 + 4; }";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FnDecl {
                name: Spanned::new(3, 5, "err"),
                ret_type: Spanned::new(10, 13, Type::Void),
                is_extern: false,
                params: ParamList(vec![]),
                body: Block(vec![
                    Stmt::Expr(Spanned {
                        node: Expr::Error(ParseError::PrefixError(
                            "Invalid token in prefix rule: '+'".to_owned()
                        ),),
                        span: Span::new(20, 20)
                    }),
                    Stmt::Expr(Spanned {
                        node: Expr::Binary(
                            Box::new(Spanned::new(25, 25, Expr::DecLit("3"))),
                            Spanned::new(27, 27, Token::Plus),
                            Box::new(Spanned::new(29, 29, Expr::DecLit("4")))
                        ),
                        span: Span::new(25, 29)
                    })
                ])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_let_with_value_should_return_var_decl_stmt() {
        let source = "fn main(): void { let var = 5; }";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FnDecl {
                name: Spanned::new(3, 6, "main"),
                ret_type: Spanned::new(11, 14, Type::Void),
                is_extern: false,
                params: ParamList(vec![]),
                body: Block(vec![Stmt::VarDecl {
                    name: Spanned::new(22, 24, "var"),
                    value: Spanned::new(28, 28, Expr::DecLit("5")),
                    eq: Spanned::new(26, 26, Token::Equals),
                    ty: Cell::new(None)
                }])
            }]),
            prg
        );

        assert_eq!(0, parser.err_count);
    }

    #[test]
    fn test_parse_with_empty_main_returns_empty_fn_decl() {
        let source = "fn main(): void {}";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FnDecl {
                name: Spanned::new(3, 6, "main"),
                is_extern: false,
                ret_type: Spanned::new(11, 14, Type::Void),
                params: ParamList(vec![]),
                body: Block(vec![])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_with_one_stmt_in_main() {
        let source = "fn main(): void {1 + 1;}";
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FnDecl {
                name: Spanned::new(3, 6, "main"),
                ret_type: Spanned::new(11, 14, Type::Void),
                params: ParamList(vec![]),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    17,
                    21,
                    Expr::Binary(
                        Box::new(Spanned::new(17, 17, Expr::DecLit("1"))),
                        Spanned::new(19, 19, Token::Plus),
                        Box::new(Spanned::new(21, 21, Expr::DecLit("1")))
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
        assert_eq!(Expr::DecLit("42"), expr);
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
                    Box::new(Spanned::new(0, 1, Expr::DecLit("1"))),
                    Spanned::new(2, 2, Token::Plus),
                    Box::new(Spanned::new(
                        4,
                        8,
                        Expr::Binary(
                            Box::new(Spanned::new(4, 4, Expr::DecLit("2"))),
                            Spanned::new(6, 6, Token::Star),
                            Box::new(Spanned::new(8, 8, Expr::DecLit("3")))
                        )
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
                    Box::new(Spanned::new(
                        0,
                        4,
                        Expr::Binary(
                            Box::new(Spanned::new(0, 1, Expr::DecLit("2"))),
                            Spanned::new(2, 2, Token::Star),
                            Box::new(Spanned::new(4, 4, Expr::DecLit("3")))
                        )
                    )),
                    Spanned::new(6, 6, Token::Plus),
                    Box::new(Spanned::new(8, 8, Expr::DecLit("1"))),
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
                    Box::new(Spanned::new(
                        0,
                        6,
                        Expr::Binary(
                            Box::new(Spanned::new(1, 1, Expr::DecLit("2"))),
                            Spanned::new(3, 3, Token::Plus),
                            Box::new(Spanned::new(5, 5, Expr::DecLit("3")))
                        )
                    )),
                    Spanned::new(8, 8, Token::Star),
                    Box::new(Spanned::new(10, 10, Expr::DecLit("1"))),
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
                    Box::new(Spanned::new(0, 1, Expr::DecLit("1"))),
                    Spanned::new(2, 2, Token::Plus),
                    Box::new(Spanned::new(
                        4,
                        10,
                        Expr::Binary(
                            Box::new(Spanned::new(5, 5, Expr::DecLit("2"))),
                            Spanned::new(7, 7, Token::Star),
                            Box::new(Spanned::new(9, 9, Expr::DecLit("3")))
                        )
                    ))
                ),
                span: Span::new(0, 10)
            },
            expr
        );
;
    }
}
