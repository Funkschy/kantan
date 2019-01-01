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
        value: Spanned<Expr<'input>>,
    },
    Expr(Spanned<Expr<'input>>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block<'input>(pub Vec<Stmt<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct ParamList<'input>(pub Vec<Param<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct Param<'input>(pub &'input str, pub Type);

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'input> {
    Error(ParseError<'input>),
    DecLit(i64),
    StringLit(&'input str),
    Negate(Box<Expr<'input>>),
    Binary(Box<Expr<'input>>, Spanned<Token<'input>>, Box<Expr<'input>>),
    Ident(&'input str),
}

impl<'input> fmt::Display for Expr<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Error(err) => write!(f, "{}", err),
            Expr::DecLit(lit) => write!(f, "{}", lit),
            Expr::StringLit(lit) => write!(f, "{}", lit),
            Expr::Negate(expr) => write!(f, "{}", expr),
            Expr::Binary(l, op, r) => write!(f, "{}", format!("{} {} {}", l, op.node, r)),
            Expr::Ident(name) => write!(f, "{}", name),
        }
    }
}
