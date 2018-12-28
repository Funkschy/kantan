use super::error::ParseError;
use super::token::{Token, TypeIdent};

#[derive(Debug, Eq, PartialEq)]
pub struct Program<'input>(pub Vec<Stmt<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub enum Stmt<'input> {
    FnDecl {
        name: &'input str,
        params: ParamList<'input>,
        body: Block<'input>,
    },
    Expr(Expr<'input>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block<'input>(pub Vec<Stmt<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct ParamList<'input>(pub Vec<Param<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct Param<'input>(&'input str, TypeIdent);

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'input> {
    Error(ParseError<'input>),
    DecLit(i64),
    Binary(Box<Expr<'input>>, Token<'input>, Box<Expr<'input>>),
}
