use super::error::ParseError;
use super::token::Token;
use super::Spanned;
use crate::types::Type;

#[derive(Debug, Eq, PartialEq)]
pub struct Program<'input>(pub Vec<Stmt<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt<'input> {
    FnDecl {
        name: &'input str,
        params: ParamList<'input>,
        body: Block<'input>,
    },
    VarDecl {
        name: &'input str,
        value: Expr<'input>,
    },
    Expr(Expr<'input>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block<'input>(pub Vec<Stmt<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct ParamList<'input>(pub Vec<Param<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct Param<'input>(pub &'input str, pub Type);

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'input> {
    Error(Spanned<ParseError<'input>>),
    DecLit(i64),
    Negate(Box<Expr<'input>>),
    Binary(Box<Expr<'input>>, Token<'input>, Box<Expr<'input>>),
}
