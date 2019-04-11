use std::{
    cell::{Cell, RefCell},
    iter::Peekable,
};

use super::{ast::*, error::LexError, token::*, *};
use crate::{types::*, Source};

type ParseResult<'src, T> = Result<T, Spanned<ParseError<'src>>>;
type ExprResult<'src> = ParseResult<'src, Spanned<Expr<'src>>>;
type StmtResult<'src> = ParseResult<'src, Stmt<'src>>;
type TopLvlResult<'src> = ParseResult<'src, TopLvl<'src>>;

fn as_err_stmt<'src>(err: Spanned<ParseError<'src>>) -> Stmt<'src> {
    Stmt::Expr(Spanned::new(
        err.span.start,
        err.span.end,
        Expr::new(ExprKind::Error(err.node)),
    ))
}

pub struct Parser<'src, I>
where
    I: Scanner<'src>,
{
    pub(crate) source: &'src Source,
    pub(crate) err_count: usize,
    scanner: Peekable<I>,
}

impl<'src, I> Parser<'src, I>
where
    I: Scanner<'src>,
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

impl<'src, I> Parser<'src, I>
where
    I: Scanner<'src>,
{
    pub fn parse(&mut self) -> Program<'src> {
        let mut top_lvl_decls = vec![];

        while self.scanner.peek().is_some() {
            let decl = self.top_lvl_decl();
            if let Ok(decl) = decl {
                top_lvl_decls.push(decl);
            } else if let Err(err) = decl {
                top_lvl_decls.push(TopLvl::Error(err));
                self.err_count += 1;
                while !(self.peek_eq(Token::Def) || self.peek_eq(Token::Type) || self.at_end()) {
                    self.advance().unwrap();
                }
            }
        }

        Program(top_lvl_decls)
    }

    fn top_lvl_decl(&mut self) -> TopLvlResult<'src> {
        if self.peek_eq(Token::Import) {
            return self.import();
        }

        if self.peek_eq(Token::Type) {
            return self.type_definition();
        }

        let is_extern = self.peek_eq(Token::Extern);
        if is_extern {
            self.consume(Token::Extern)?;
        }

        self.consume(Token::Def)?;
        let name = self.consume_ident()?;
        let params = self.def_param_list(is_extern)?;

        self.consume(Token::Colon)?;
        let ret_type = self.consume_type()?;

        let body = if !is_extern {
            self.block()?.node
        } else {
            self.consume(Token::Semi)?;
            Block::default()
        };

        Ok(TopLvl::FuncDecl {
            name,
            params,
            body,
            ret_type,
            is_extern,
        })
    }

    fn type_definition(&mut self) -> TopLvlResult<'src> {
        self.consume(Token::Type)?;
        let name = self.consume_ident()?;

        self.consume(Token::Struct)?;
        self.consume(Token::LBrace)?;

        let mut fields = Vec::new();

        if !self.at_end() && !self.peek_eq(Token::RBrace) {
            loop {
                let field_name = self.consume_ident()?;
                self.consume(Token::Colon)?;
                let field_type = self.consume_type()?;
                fields.push((field_name, field_type));

                if self.at_end() || self.peek_eq(Token::RBrace) {
                    break;
                }

                self.consume(Token::Comma)?;
            }
        }

        self.consume(Token::RBrace)?;
        Ok(TopLvl::TypeDef(TypeDef::StructDef { name, fields }))
    }

    fn import(&mut self) -> TopLvlResult<'src> {
        self.consume(Token::Import)?;
        let name = self.consume_ident()?;

        Ok(TopLvl::Import { name })
    }

    fn def_param_list(&mut self, is_extern: bool) -> ParseResult<'src, ParamList<'src>> {
        self.delim_param_list(Token::LParen, Token::RParen, true, is_extern)
    }

    fn closure_param_list(&mut self) -> ParseResult<'src, ParamList<'src>> {
        self.delim_param_list(Token::Pipe, Token::Pipe, false, false)
    }

    fn delim_param_list(
        &mut self,
        ldelim: Token<'src>,
        rdelim: Token<'src>,
        consume_start: bool,
        is_extern: bool,
    ) -> ParseResult<'src, ParamList<'src>> {
        if consume_start {
            self.consume(ldelim)?;
        }
        let mut varargs = false;

        if self.match_tok(rdelim.clone())? {
            return Ok(ParamList::default());
        }

        let mut params = vec![];

        while !self.peek_eq_ref(&rdelim) {
            if self.peek_eq(Token::TripleDot) {
                if !is_extern {
                    // TODO: replace with custom error
                    panic!("Varargs are currently only supported in extern functions");
                }

                let triple_dot = self.consume(Token::TripleDot)?;
                varargs = true;
                let spanned = Spanned::from_span(triple_dot.span, "...");
                params.push(Param::new(
                    spanned,
                    Spanned::from_span(triple_dot.span, Type::Simple(Simple::Varargs)),
                ));
                // ... has to be the last param
                // TODO: replace consume error with special error
                break;
            }

            let ident = self.consume_ident()?;
            self.consume(Token::Colon)?;
            // TODO: user defined types
            let ty = self.consume_type()?;
            params.push(Param::new(ident, ty));

            if !self.peek_eq_ref(&rdelim) {
                self.consume(Token::Comma)?;
            }
        }

        self.consume(rdelim)?;
        Ok(ParamList { varargs, params })
    }

    fn block(&mut self) -> ParseResult<'src, Spanned<Block<'src>>> {
        let start = self.consume(Token::LBrace)?.span.start;
        let mut stmts = vec![];

        while !self.at_end() && !self.peek_eq(Token::RBrace) {
            let stmt = self.statement();
            if let Ok(stmt) = stmt {
                stmts.push(stmt);
            } else if let Err(err) = stmt {
                self.err_count += 1;
                self.sync();
                stmts.push(as_err_stmt(err));
            }
        }

        // Unexpected end of file
        let end = if !self.at_end() {
            self.consume(Token::RBrace)?.span.end
        } else {
            self.source.code.len()
        };

        Ok(Spanned::new(start, end, Block(stmts)))
    }

    fn statement(&mut self) -> StmtResult<'src> {
        if let Some(Ok(Spanned { node, .. })) = self.scanner.peek() {
            // Check different statement types
            match node {
                Token::Let => {
                    let decl = self.let_decl()?;
                    self.consume(Token::Semi)?;
                    return Ok(decl);
                }
                Token::If => return Ok(self.if_stmt()?),
                Token::Return => return Ok(self.return_stmt()?),
                Token::While => return Ok(self.while_stmt()?),
                Token::Delete => return Ok(self.delete_stmt()?),
                _ => {}
            }
        }

        // If no statement rule applies, assume an expression
        let expr = self.expression(false)?;
        self.consume(Token::Semi)?;
        Ok(Stmt::Expr(expr))
    }

    fn delete_stmt(&mut self) -> StmtResult<'src> {
        self.consume(Token::Delete)?;
        let expr = self.expression(false)?;
        self.consume(Token::Semi)?;

        Ok(Stmt::Delete(Box::new(expr)))
    }

    fn while_stmt(&mut self) -> StmtResult<'src> {
        self.consume(Token::While)?;
        let condition = self.expression(true)?;
        let body = self.block()?.node;

        Ok(Stmt::While { condition, body })
    }

    fn return_stmt(&mut self) -> StmtResult<'src> {
        self.consume(Token::Return)?;

        let ret = Ok(Stmt::Return(if self.peek_eq(Token::Semi) {
            None
        } else {
            Some(self.expression(false)?)
        }));

        self.consume(Token::Semi)?;
        ret
    }

    fn if_stmt(&mut self) -> StmtResult<'src> {
        self.consume(Token::If)?;

        let condition = self.expression(true)?;
        let then_block = self.block()?.node;

        let else_branch = if self.peek_eq(Token::Else) {
            self.consume(Token::Else)?;

            let else_branch = if self.peek_eq(Token::If) {
                let else_if = Box::new(self.if_stmt()?);
                Else::IfStmt(else_if)
            } else {
                let block = self.block()?.node;
                Else::Block(block)
            };

            Some(Box::new(else_branch))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_block,
            else_branch,
        })
    }

    fn let_decl(&mut self) -> StmtResult<'src> {
        self.consume(Token::Let)?;

        let name = self.consume_ident()?;
        let ty = if let Ok(true) = self.match_tok(Token::Colon) {
            Some(self.consume_type()?)
        } else {
            None
        };

        let ty = RefCell::new(ty);

        let eq = self.consume(Token::Equals)?;

        let value = self.expression(false)?;
        Ok(Stmt::VarDecl(Box::new(VarDecl {
            name,
            value,
            eq,
            ty,
        })))
    }

    pub fn expression(&mut self, no_struct: bool) -> ExprResult<'src> {
        let mut left = self.parse_expression(Precedence::Assign, no_struct)?;
        while self.peek_eq(Token::Equals) {
            let eq = self.consume(Token::Equals)?;
            let value = Box::new(self.parse_expression(Precedence::Assign, no_struct)?);
            left = Spanned::new(
                left.span.start,
                value.span.end,
                Expr::new(ExprKind::Assign {
                    left: Box::new(left),
                    eq,
                    value,
                }),
            );
        }

        Ok(left)
    }

    fn parse_expression(&mut self, precedence: Precedence, no_struct: bool) -> ExprResult<'src> {
        let token = self.advance()?;
        let mut left = self.prefix(&token, no_struct)?;

        while self.next_higher_precedence(precedence, no_struct) {
            let token = self.advance()?;
            left = self.infix(&token, left, no_struct)?;
        }

        Ok(left)
    }

    fn eof(&mut self) -> Scanned<'src> {
        let len = self.source.code.len();
        let span = Span::new(len, len);
        self.make_lex_err(span, "Unexpected end of file")
    }

    fn advance(&mut self) -> Scanned<'src> {
        self.scanner.next().unwrap_or_else(|| self.eof())
    }

    fn match_tok(&mut self, expected: Token<'src>) -> ParseResult<'src, bool> {
        if self.peek_eq_ref(&expected) {
            self.consume(expected)?;
            return Ok(true);
        }

        Ok(false)
    }

    fn peek_eq(&mut self, expected: Token<'src>) -> bool {
        self.scanner.peek().map_or(false, |peek| match peek {
            Ok(Spanned { node, .. }) => *node == expected,
            _ => false,
        })
    }

    fn peek_eq_ref(&mut self, expected: &Token<'src>) -> bool {
        self.scanner.peek().map_or(false, |peek| match peek {
            Ok(Spanned { node, .. }) => *node == *expected,
            _ => false,
        })
    }

    fn consume_ident(&mut self) -> ParseResult<'src, Spanned<&'src str>> {
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

    /// Parses a user ident of type name or package.name
    fn user_ident(&mut self, expr: &Spanned<Expr<'src>>) -> ParseResult<'src, UserIdent<'src>> {
        Ok(match expr.node.kind() {
            ExprKind::Ident(n) => UserIdent::new(&self.source.name, n),
            ExprKind::Access { left, identifier } => {
                if let ExprKind::Ident(left) = left.node.kind() {
                    UserIdent::new(left, identifier.node)
                } else {
                    return Err(Spanned::from_span(
                        left.span,
                        ParseError::InternalError(
                            "The left side of this expression has to be an Identifier",
                        ),
                    ));
                }
            }
            _ => {
                return Err(Spanned::from_span(
                    expr.span,
                    ParseError::InternalError(
                        "The expression for a user_ident has to be an Ident or an Access",
                    ),
                ));
            }
        })
    }

    fn consume_type(&mut self) -> ParseResult<'src, Spanned<Type<'src>>> {
        if let Some(peek) = self.scanner.peek().cloned() {
            return match peek {
                Ok(peek) => {
                    match peek {
                        Spanned {
                            node: Token::TypeIdent(ty),
                            span,
                        } => {
                            self.advance()?;
                            Ok(Spanned::from_span(span, Type::Simple(ty)))
                        }
                        Spanned {
                            node: Token::Ident(_),
                            ..
                        } => {
                            let expr = self.expression(true)?;
                            let ident = self.user_ident(&expr)?;

                            Ok(Spanned::from_span(
                                expr.span,
                                Type::Simple(Simple::UserType(ident)),
                            ))
                        }
                        Spanned {
                            node: Token::Star, ..
                        } => {
                            let mut counter = 1;
                            let start = self.advance()?.span.start;
                            // count *s
                            while self.match_tok(Token::Star)? {
                                counter += 1;
                            }

                            let ty = self.consume_type()?;
                            let (inner, end) = if let Type::Simple(s) = ty.node {
                                (s, ty.span.end)
                            } else {
                                return Err(Spanned::from_span(
                                    ty.span,
                                    ParseError::InternalError(
                                        "Unreachable code reached in the consumption of a pointer type",
                                    ),
                                ));
                            };

                            Ok(Spanned::new(
                                start,
                                end,
                                Type::Pointer(Pointer::new(counter, inner)),
                            ))
                        }
                        _ => {
                            let tok = Spanned::clone(&peek);
                            Err(self.make_consume_err(&tok, "Type".to_owned()).unwrap_err())
                        }
                    }
                }
                Err(err) => Err(err),
            };
        }

        Err(self.eof().unwrap_err())
    }

    fn consume(&mut self, expected: Token<'src>) -> Scanned<'src> {
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

    fn next_higher_precedence(&mut self, precedence: Precedence, no_struct: bool) -> bool {
        self.scanner.peek().map_or(false, |scanned| {
            if let Ok(spanned) = scanned {
                if let Token::LBrace = spanned.node {
                    return !no_struct && spanned.node.precedence() > precedence;
                }
                spanned.node.precedence() > precedence
            } else {
                false
            }
        })
    }

    fn infix(
        &mut self,
        token: &Spanned<Token<'src>>,
        left: Spanned<Expr<'src>>,
        no_struct: bool,
    ) -> ExprResult<'src> {
        let tok = &token.node;
        match tok {
            Token::EqualsEquals
            | Token::BangEquals
            | Token::SmallerEquals
            | Token::GreaterEquals
            | Token::Smaller
            | Token::Greater
            | Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash => {
                let right = self.parse_expression(tok.precedence(), no_struct)?;
                let right_span = right.span;
                let left_span = left.span;

                let expr = match tok {
                    Token::EqualsEquals
                    | Token::BangEquals
                    | Token::GreaterEquals
                    | Token::Greater
                    | Token::SmallerEquals
                    | Token::Smaller => {
                        ExprKind::BoolBinary(Box::new(left), token.clone(), Box::new(right))
                    }
                    _ => ExprKind::Binary(
                        Box::new(left),
                        token.clone(),
                        Box::new(Spanned::from_span(right.span, right.node)),
                    ),
                };
                Ok(Spanned::new(
                    left_span.start,
                    right_span.end,
                    Expr::new(expr),
                ))
            }
            Token::Dot => {
                let ident = self.consume_ident()?;
                Ok(Spanned::new(
                    left.span.start,
                    ident.span.end,
                    Expr::new(ExprKind::Access {
                        left: Box::new(left),
                        identifier: ident,
                    }),
                ))
            }
            Token::LBrace => {
                let init_list = self.init_list()?;
                let brace = self.consume(Token::RBrace)?;

                let span = Span::new(token.span.start, brace.span.end);
                let ident = self.user_ident(&left)?;

                Ok(Spanned::from_span(
                    span,
                    Expr::new(ExprKind::StructInit {
                        identifier: Spanned::from_span(left.span, ident),
                        fields: init_list,
                    }),
                ))
            }
            Token::LParen => {
                let ident = self.user_ident(&left)?;

                let arg_list = self.arg_list()?;
                let end = self.consume(Token::RParen)?.span.end;
                let span = Span::new(left.span.start, end);

                Ok(Spanned::from_span(
                    span,
                    Expr::new(ExprKind::Call {
                        callee: Spanned::from_span(left.span, ident),
                        args: arg_list,
                    }),
                ))
            }
            _ => self.make_infix_err(token),
        }
    }

    fn prefix(&mut self, token: &Spanned<Token<'src>>, no_struct: bool) -> ExprResult<'src> {
        let ok_spanned = |kind| Ok(Spanned::from_span(token.span, Expr::new(kind)));

        match token.node {
            Token::NullLit => ok_spanned(ExprKind::NullLit),
            Token::DecLit(lit) => ok_spanned(ExprKind::DecLit(lit)),
            Token::FloatLit(lit) => ok_spanned(ExprKind::FloatLit(lit)),
            Token::StringLit(lit) => ok_spanned(ExprKind::StringLit(lit)),
            Token::Sizeof => {
                // TODO: support sizeof expr
                let ty = self.consume_type()?;
                let size_of = ExprKind::SizeOf(ty.node);
                Ok(Spanned::from_span(ty.span, Expr::new(size_of)))
            }
            Token::New => {
                let expr = self.expression(no_struct)?;
                let new = ExprKind::New(Box::new(Spanned::from_span(expr.span, expr.node)));
                Ok(Spanned::from_span(expr.span, Expr::new(new)))
            }
            Token::LParen => {
                // ignore no_struct, because it's in parentheses
                let mut expr = self.expression(false)?;
                self.consume(Token::RParen)?;
                expr.span.start -= 1;
                expr.span.end += 1;
                Ok(expr)
            }
            Token::Minus => {
                let next = self.expression(no_struct)?;

                Ok(Spanned::new(
                    token.span.start,
                    next.span.end,
                    Expr::new(ExprKind::Negate(token.clone(), Box::new(next))),
                ))
            }
            Token::Ampersand => {
                // TODO: change false Precedence
                let next = self.parse_expression(Precedence::Sum, no_struct)?;

                Ok(Spanned::new(
                    token.span.start,
                    next.span.end,
                    Expr::new(ExprKind::Ref(token.clone(), Box::new(next))),
                ))
            }
            Token::Star => {
                // TODO: change false Precedence
                let next = self.parse_expression(Precedence::Sum, no_struct)?;

                Ok(Spanned::new(
                    token.span.start,
                    next.span.end,
                    Expr::new(ExprKind::Deref(token.clone(), Box::new(next))),
                ))
            }
            Token::Ident(ref name) => {
                if !no_struct && self.match_tok(Token::LBrace)? {
                    let init_list = self.init_list()?;
                    let brace = self.consume(Token::RBrace)?;
                    Ok(Spanned::new(
                        token.span.start,
                        brace.span.end,
                        Expr::new(ExprKind::StructInit {
                            identifier: Spanned::from_span(
                                token.span,
                                UserIdent::new(&self.source.name, name),
                            ),
                            fields: init_list,
                        }),
                    ))
                } else {
                    ok_spanned(ExprKind::Ident(name))
                }
            }
            Token::Pipe => {
                let params = self.closure_param_list()?;

                let (body, end) = if self.peek_eq(Token::LBrace) {
                    let block = self.block()?;
                    let end = block.span.end;
                    (Box::new(ClosureBody::Block(block.node)), end)
                } else {
                    let expr = self.expression(false)?;
                    let end = expr.span.end;
                    (Box::new(ClosureBody::Expr(expr)), end)
                };

                let closure = Expr::new(ExprKind::Closure(params, body, Cell::new(0)));
                Ok(Spanned::new(token.span.start, end, closure))
            }
            _ => self.make_prefix_err(token),
        }
    }

    fn init_list(&mut self) -> ParseResult<'src, InitList<'src>> {
        let mut inits = vec![];

        while !self.at_end() && !self.peek_eq(Token::RBrace) {
            let ident = self.consume_ident()?;
            self.consume(Token::Colon)?;
            let expr = self.expression(false)?;
            inits.push((ident, expr));
            if !self.peek_eq(Token::RBrace) {
                self.consume(Token::Comma)?;
            }
        }

        Ok(InitList(inits))
    }

    fn arg_list(&mut self) -> ParseResult<'src, ArgList<'src>> {
        let mut args = vec![];

        while !self.at_end() && !self.peek_eq(Token::RParen) {
            args.push(self.expression(false)?);
            if !self.peek_eq(Token::RParen) {
                self.consume(Token::Comma)?;
            }
        }

        Ok(ArgList(args))
    }

    fn make_lex_err(&mut self, span: Span, cause: &str) -> Scanned<'src> {
        self.err_count += 1;
        Err(Spanned {
            span,
            node: ParseError::LexError(LexError::with_cause(cause)),
        })
    }

    fn make_prefix_err(&mut self, token: &Spanned<Token<'src>>) -> ExprResult<'src> {
        self.err_count += 1;
        let s = format!("Invalid token in prefix rule: '{}'", token.node);
        Err(Spanned {
            span: token.span,
            node: ParseError::PrefixError(s),
        })
    }

    fn make_infix_err(&mut self, token: &Spanned<Token<'src>>) -> ExprResult<'src> {
        self.err_count += 1;
        let s = format!("Invalid token in infix rule: '{}'", token.node);
        Err(Spanned {
            span: token.span,
            node: ParseError::InfixError(s),
        })
    }

    fn make_consume_err(
        &mut self,
        actual: &Spanned<Token<'src>>,
        expected: String,
    ) -> Scanned<'src> {
        self.err_count += 1;
        Err(Spanned {
            span: actual.span,
            node: ParseError::ConsumeError {
                actual: actual.node.clone(),
                expected,
            },
        })
    }

    fn sync(&mut self) {
        let mut previous = self.advance();

        while let Some(Ok(peek)) = self.scanner.peek() {
            if let Ok(Spanned {
                node: Token::Semi, ..
            }) = previous
            {
                break;
            }

            match peek.node {
                Token::Type | Token::Def | Token::If | Token::Let | Token::Return => return,
                _ => {}
            }

            previous = self.advance();
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
    fn test_parse_closure_one_param() {
        let source = Source::new("main", "|x: i32| x + 1");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression(false).unwrap();
        assert_eq!(
            Spanned::new(
                0,
                13,
                Expr::new(ExprKind::Closure(
                    ParamList {
                        varargs: false,
                        params: vec![Param::new(
                            Spanned::new(1, 1, "x"),
                            Spanned::new(4, 6, Type::Simple(Simple::I32))
                        )]
                    },
                    Box::new(ClosureBody::Expr(Spanned::new(
                        9,
                        13,
                        Expr::new(ExprKind::Binary(
                            Box::new(Spanned::new(9, 9, Expr::new(ExprKind::Ident("x")))),
                            Spanned::new(11, 11, Token::Plus),
                            Box::new(Spanned::new(13, 13, Expr::new(ExprKind::DecLit("1")))),
                        ))
                    ))),
                    Cell::new(0)
                ),)
            ),
            expr
        );
    }

    #[test]
    fn test_parse_closure_no_params() {
        let source = Source::new("main", "|| 1");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression(false).unwrap();
        assert_eq!(
            Spanned::new(
                0,
                3,
                Expr::new(ExprKind::Closure(
                    ParamList {
                        varargs: false,
                        params: vec![]
                    },
                    Box::new(ClosureBody::Expr(Spanned::new(
                        3,
                        3,
                        Expr::new(ExprKind::DecLit("1"))
                    ))),
                    Cell::new(0)
                ))
            ),
            expr
        );
    }

    #[test]
    fn test_consume_type_pointer_to_pointer() {
        let source = Source::new("main", "**Person");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let ty = parser.consume_type().unwrap();
        assert_eq!(
            Spanned::new(
                0,
                7,
                Type::Pointer(Pointer::new(
                    2,
                    Simple::UserType(UserIdent::new("main", "Person"))
                ))
            ),
            ty
        );
    }

    #[test]
    fn test_consume_type_pointer() {
        let source = Source::new("main", "*i32");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let ty = parser.consume_type().unwrap();
        assert_eq!(
            Spanned::new(0, 3, Type::Pointer(Pointer::new(1, Simple::I32))),
            ty
        );
    }

    #[test]
    fn test_parse_assignment_chain() {
        let source = Source::new("main", "x = y = 0");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        parser.expression(false).unwrap();
    }

    #[test]
    fn test_parse_return_struct() {
        let source = Source::new(
            "main",
            "type Test struct {test: i32, second: bool} def main(): Test { }",
        );
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![
                TopLvl::TypeDef(TypeDef::StructDef {
                    name: Spanned::new(5, 8, "Test"),
                    fields: vec![
                        (
                            Spanned::new(18, 21, "test"),
                            Spanned::new(24, 26, Type::Simple(Simple::I32))
                        ),
                        (
                            Spanned::new(29, 34, "second"),
                            Spanned::new(37, 40, Type::Simple(Simple::Bool))
                        ),
                    ]
                }),
                TopLvl::FuncDecl {
                    name: Spanned::new(47, 50, "main"),
                    params: ParamList::default(),
                    ret_type: Spanned::new(
                        55,
                        58,
                        Type::Simple(Simple::UserType(UserIdent::new("main", "Test")))
                    ),
                    is_extern: false,
                    body: Block(vec![])
                }
            ]),
            prg
        );
    }

    #[test]
    fn test_parse_struct_definition() {
        let source = Source::new(
            "main",
            "type Test struct {test: i32, second: bool} def main(): void { }",
        );
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![
                TopLvl::TypeDef(TypeDef::StructDef {
                    name: Spanned::new(5, 8, "Test"),
                    fields: vec![
                        (
                            Spanned::new(18, 21, "test"),
                            Spanned::new(24, 26, Type::Simple(Simple::I32))
                        ),
                        (
                            Spanned::new(29, 34, "second"),
                            Spanned::new(37, 40, Type::Simple(Simple::Bool))
                        ),
                    ]
                }),
                TopLvl::FuncDecl {
                    name: Spanned::new(47, 50, "main"),
                    params: ParamList::default(),
                    ret_type: Spanned::new(55, 58, Type::Simple(Simple::Void)),
                    is_extern: false,
                    body: Block(vec![])
                }
            ]),
            prg
        );
    }

    #[test]
    fn test_parse_empty_struct_definition() {
        let source = Source::new("main", "type Test struct {} def main(): void { }");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![
                TopLvl::TypeDef(TypeDef::StructDef {
                    name: Spanned::new(5, 8, "Test"),
                    fields: Vec::new()
                }),
                TopLvl::FuncDecl {
                    name: Spanned::new(24, 27, "main"),
                    params: ParamList::default(),
                    ret_type: Spanned::new(32, 35, Type::Simple(Simple::Void)),
                    is_extern: false,
                    body: Block(vec![])
                }
            ]),
            prg
        );
    }

    #[test]
    fn test_parse_while_statement() {
        let source = Source::new("main", "def main(): void { while 1 == 1 { test(); } }");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 7, "main"),
                params: ParamList::default(),
                ret_type: Spanned::new(12, 15, Type::Simple(Simple::Void)),
                is_extern: false,
                body: Block(vec![Stmt::While {
                    condition: Spanned::new(
                        25,
                        30,
                        Expr::new(ExprKind::BoolBinary(
                            Box::new(Spanned::new(25, 25, Expr::new(ExprKind::DecLit("1")))),
                            Spanned::new(27, 28, Token::EqualsEquals),
                            Box::new(Spanned::new(30, 30, Expr::new(ExprKind::DecLit("1")))),
                        ))
                    ),
                    body: Block(vec![Stmt::Expr(Spanned::new(
                        34,
                        39,
                        Expr::new(ExprKind::Call {
                            callee: Spanned::new(34, 37, UserIdent::new("main", "test")),
                            args: ArgList(vec![])
                        })
                    ))])
                }])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_import_access() {
        let source = Source::new("main", "def main(): void { test.func(); }");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 7, "main"),
                params: ParamList::default(),
                ret_type: Spanned::new(12, 15, Type::Simple(Simple::Void)),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    19,
                    29,
                    Expr::new(ExprKind::Call {
                        callee: Spanned::new(19, 27, UserIdent::new("test", "func")),
                        args: ArgList(vec![])
                    })
                ))])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_import() {
        let source = Source::new("main", "import test\ndef main(): void {}");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![
                TopLvl::Import {
                    name: Spanned::new(7, 10, "test")
                },
                TopLvl::FuncDecl {
                    name: Spanned::new(16, 19, "main"),
                    is_extern: false,
                    ret_type: Spanned::new(24, 27, Type::Simple(Simple::Void)),
                    params: ParamList::default(),
                    body: Block(vec![])
                }
            ]),
            prg
        );
    }

    #[test]
    fn test_parse_access_through_identifier() {
        let source = Source::new("main", "def main(): void { test.fun(); }");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 7, "main"),
                ret_type: Spanned::new(12, 15, Type::Simple(Simple::Void)),
                is_extern: false,
                params: ParamList::default(),
                body: Block(vec![Stmt::Expr(Spanned::new(
                    19,
                    28,
                    Expr::new(ExprKind::Call {
                        callee: Spanned::new(19, 26, UserIdent::new("test", "fun")),
                        args: ArgList(vec![])
                    })
                ))])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_call_without_args_should_return_call_expr_with_empty_args() {
        let source = Source::new("main", "def main(): void { test(); }");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 7, "main"),
                ret_type: Spanned::new(12, 15, Type::Simple(Simple::Void)),
                is_extern: false,
                params: ParamList::default(),
                body: Block(vec![Stmt::Expr(Spanned::new(
                    19,
                    24,
                    Expr::new(ExprKind::Call {
                        callee: Spanned::new(19, 22, UserIdent::new("main", "test")),
                        args: ArgList(vec![])
                    })
                ))])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_with_one_error_should_have_err_count_of_one() {
        let source = Source::new("main", "def err(): void { 1 ++ 2; 3 + 4; }");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 6, "err"),
                ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
                is_extern: false,
                params: ParamList::default(),
                body: Block(vec![
                    Stmt::Expr(Spanned {
                        node: Expr::new(ExprKind::Error(ParseError::PrefixError(
                            "Invalid token in prefix rule: '+'".to_owned()
                        ),)),
                        span: Span::new(21, 21)
                    }),
                    Stmt::Expr(Spanned {
                        node: Expr::new(ExprKind::Binary(
                            Box::new(Spanned::new(26, 26, Expr::new(ExprKind::DecLit("3")))),
                            Spanned::new(28, 28, Token::Plus),
                            Box::new(Spanned::new(30, 30, Expr::new(ExprKind::DecLit("4"))))
                        )),
                        span: Span::new(26, 30)
                    })
                ])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_let_with_value_should_return_var_decl_stmt() {
        let source = Source::new("main", "def main(): void { let var = 5; }");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 7, "main"),
                ret_type: Spanned::new(12, 15, Type::Simple(Simple::Void)),
                is_extern: false,
                params: ParamList::default(),
                body: Block(vec![Stmt::VarDecl(Box::new(VarDecl {
                    name: Spanned::new(23, 25, "var"),
                    value: Spanned::new(29, 29, Expr::new(ExprKind::DecLit("5"))),
                    eq: Spanned::new(27, 27, Token::Equals),
                    ty: RefCell::new(None)
                }))])
            }]),
            prg
        );

        assert_eq!(0, parser.err_count);
    }

    #[test]
    fn test_parse_with_empty_main_returns_empty_fn_decl() {
        let source = Source::new("main", "def main(): void {}");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 7, "main"),
                is_extern: false,
                ret_type: Spanned::new(12, 15, Type::Simple(Simple::Void)),
                params: ParamList::default(),
                body: Block(vec![])
            }]),
            prg
        );
    }

    #[test]
    fn test_parse_with_one_stmt_in_main() {
        let source = Source::new("main", "def main(): void {1 + 1;}");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let prg = parser.parse();
        assert_eq!(
            Program(vec![TopLvl::FuncDecl {
                name: Spanned::new(4, 7, "main"),
                ret_type: Spanned::new(12, 15, Type::Simple(Simple::Void)),
                params: ParamList::default(),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    18,
                    22,
                    Expr::new(ExprKind::Binary(
                        Box::new(Spanned::new(18, 18, Expr::new(ExprKind::DecLit("1")))),
                        Spanned::new(20, 20, Token::Plus),
                        Box::new(Spanned::new(22, 22, Expr::new(ExprKind::DecLit("1"))))
                    ))
                ))])
            }]),
            prg
        );

        assert_eq!(0, parser.err_count);
    }

    #[test]
    fn test_parsing_number_in_parentheses_should_just_return_number() {
        let source = Source::new("main", "((((42))))");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression(false).unwrap().node;
        assert_eq!(Expr::new(ExprKind::DecLit("42")), expr);
    }

    #[test]
    fn test_parsing_binary_operations_should_have_correct_precedence() {
        let source = Source::new("main", "1 + 2 * 3");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression(false).unwrap();
        assert_eq!(
            Spanned {
                node: Expr::new(ExprKind::Binary(
                    Box::new(Spanned::new(0, 1, Expr::new(ExprKind::DecLit("1")))),
                    Spanned::new(2, 2, Token::Plus),
                    Box::new(Spanned::new(
                        4,
                        8,
                        Expr::new(ExprKind::Binary(
                            Box::new(Spanned::new(4, 4, Expr::new(ExprKind::DecLit("2")))),
                            Spanned::new(6, 6, Token::Star),
                            Box::new(Spanned::new(8, 8, Expr::new(ExprKind::DecLit("3"))))
                        ))
                    ))
                )),
                span: Span::new(0, 8)
            },
            expr
        );

        let source = Source::new("main", "2 * 3 + 1");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression(false).unwrap();
        assert_eq!(
            Spanned {
                node: Expr::new(ExprKind::Binary(
                    Box::new(Spanned::new(
                        0,
                        4,
                        Expr::new(ExprKind::Binary(
                            Box::new(Spanned::new(0, 1, Expr::new(ExprKind::DecLit("2")))),
                            Spanned::new(2, 2, Token::Star),
                            Box::new(Spanned::new(4, 4, Expr::new(ExprKind::DecLit("3"))))
                        ))
                    )),
                    Spanned::new(6, 6, Token::Plus),
                    Box::new(Spanned::new(8, 8, Expr::new(ExprKind::DecLit("1")))),
                )),
                span: Span::new(0, 8)
            },
            expr
        );

        let source = Source::new("main", "(2 + 3) * 1");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression(false).unwrap();
        assert_eq!(
            Spanned {
                node: Expr::new(ExprKind::Binary(
                    Box::new(Spanned::new(
                        0,
                        6,
                        Expr::new(ExprKind::Binary(
                            Box::new(Spanned::new(1, 1, Expr::new(ExprKind::DecLit("2")))),
                            Spanned::new(3, 3, Token::Plus),
                            Box::new(Spanned::new(5, 5, Expr::new(ExprKind::DecLit("3"))))
                        ))
                    )),
                    Spanned::new(8, 8, Token::Star),
                    Box::new(Spanned::new(10, 10, Expr::new(ExprKind::DecLit("1")))),
                )),
                span: Span::new(0, 10)
            },
            expr
        );

        let source = Source::new("main", "1 + (2 * 3)");
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let expr = parser.expression(false).unwrap();
        assert_eq!(
            Spanned {
                node: Expr::new(ExprKind::Binary(
                    Box::new(Spanned::new(0, 1, Expr::new(ExprKind::DecLit("1")))),
                    Spanned::new(2, 2, Token::Plus),
                    Box::new(Spanned::new(
                        4,
                        10,
                        Expr::new(ExprKind::Binary(
                            Box::new(Spanned::new(5, 5, Expr::new(ExprKind::DecLit("2")))),
                            Spanned::new(7, 7, Token::Star),
                            Box::new(Spanned::new(9, 9, Expr::new(ExprKind::DecLit("3"))))
                        ))
                    ))
                )),
                span: Span::new(0, 10)
            },
            expr
        );
;
    }
}
