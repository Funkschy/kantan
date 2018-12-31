use std::fmt;

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
    StringLit(&'input str),
    Negate(Box<Expr<'input>>),
    Binary(Box<Expr<'input>>, Token<'input>, Box<Expr<'input>>),
}

impl<'input> fmt::Display for Expr<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Expr::Error(err) => err.to_string(),
            Expr::DecLit(lit) => lit.to_string(),
            Expr::StringLit(lit) => lit.to_string(),
            Expr::Negate(expr) => expr.to_string(),
            Expr::Binary(l, op, r) => format!("{} {} {}", l, op, r),
        };

        write!(f, "{}", s)
    }
}
