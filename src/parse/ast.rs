use std::fmt;

use super::error::ParseError;
use super::token::Token;
use super::Spanned;
use crate::types::Type;

#[derive(Debug, Eq, PartialEq)]
pub struct Program<'input>(pub Vec<TopLvl<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub enum TopLvl<'input> {
    FnDecl {
        name: Spanned<&'input str>,
        params: ParamList<'input>,
        body: Block<'input>,
        ret_type: Spanned<Type>,
    },
    Import {
        name: Spanned<&'input str>,
    },
    Error(Spanned<ParseError<'input>>),
}

// TODO: refactor Spanned<&'input str> to identifier
#[derive(Debug, Eq, PartialEq)]
pub enum Stmt<'input> {
    VarDecl {
        name: Spanned<&'input str>,
        value: Spanned<Expr<'input>>,
        eq: Spanned<Token<'input>>,
        ty: Option<Spanned<Type>>,
    },
    If {
        condition: Spanned<Expr<'input>>,
        then_block: Block<'input>,
        else_branch: Option<Box<Else<'input>>>,
    },
    Return(Option<Spanned<Expr<'input>>>),
    Expr(Spanned<Expr<'input>>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Else<'input> {
    IfStmt(Stmt<'input>),
    Block(Block<'input>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block<'input>(pub Vec<Stmt<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct ParamList<'input>(pub Vec<Param<'input>>);

#[derive(Debug, Eq, PartialEq)]
pub struct Param<'input>(pub Spanned<&'input str>, pub Type);

impl<'input> Param<'input> {
    pub fn new(ident: Spanned<&'input str>, ty: Type) -> Self {
        Param(ident, ty)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct ArgList<'input>(pub Vec<Spanned<Expr<'input>>>);

impl<'input> fmt::Display for ArgList<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strings: Vec<String> = self
            .0
            .iter()
            .map(|Spanned { node, .. }| node.to_string())
            .collect();

        write!(f, "{}", strings.join(", "))
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Expr<'input> {
    Error(ParseError<'input>),
    DecLit(&'input str),
    StringLit(&'input str),
    Negate(Spanned<Token<'input>>, Box<Spanned<Expr<'input>>>),
    Binary(
        Box<Spanned<Expr<'input>>>,
        Spanned<Token<'input>>,
        Box<Spanned<Expr<'input>>>,
    ),
    BoolBinary(
        Box<Spanned<Expr<'input>>>,
        Spanned<Token<'input>>,
        Box<Spanned<Expr<'input>>>,
    ),
    Ident(&'input str),
    Assign {
        name: &'input str,
        eq: Spanned<Token<'input>>,
        value: Box<Spanned<Expr<'input>>>,
    },
    Call {
        callee: Box<Spanned<Expr<'input>>>,
        args: ArgList<'input>,
    },
    Access {
        left: Box<Spanned<Expr<'input>>>,
        identifier: Spanned<&'input str>,
    },
}

impl<'input> fmt::Display for Expr<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;

        match self {
            Error(err) => write!(f, "{}", err),
            DecLit(lit) => write!(f, "{}", lit),
            StringLit(lit) => write!(f, "{}", lit),
            Negate(_, expr) => write!(f, "-{}", expr.node),
            Binary(l, op, r) => write!(f, "{}", format!("{} {} {}", l.node, op.node, r.node)),
            BoolBinary(l, op, r) => write!(f, "{}", format!("{} {} {}", l.node, op.node, r.node)),
            Ident(name) => write!(f, "{}", name),
            Assign { name, value, .. } => write!(f, "{} = {}", name, value.node),
            Call { callee, args } => write!(f, "{}({})", callee.node, args),
            Access { left, identifier } => write!(f, "{}.{}", left.node, identifier.node),
        }
    }
}
