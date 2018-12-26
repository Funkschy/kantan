use super::error::ParseError;
use super::token::Token;

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'input> {
    Error(ParseError<'input>),
    DecLit(i64),
    Binary(Box<Expr<'input>>, Token<'input>, Box<Expr<'input>>),
}
