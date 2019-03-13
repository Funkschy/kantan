use std::fmt;

use super::tac::{Expression, Label};
use crate::types::Type;

#[derive(PartialEq, Debug, Clone)]
pub enum Address<'input> {
    Empty,
    Name(String),
    Const(Constant<'input>),
    Temp(TempVar),
    Global(Label),
}

impl<'input> fmt::Display for Address<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Address::*;

        let s = match self {
            Empty => "empty".to_owned(),
            Name(n) => n.to_string(),
            Const(c) => c.to_string(),
            Temp(t) => t.to_string(),
            Global(l) => l.to_string(),
        };

        write!(f, "{}", s)
    }
}

impl<'input> Into<Expression<'input>> for Address<'input> {
    fn into(self) -> Expression<'input> {
        Expression::Copy(self)
    }
}

impl<'input> From<&String> for Address<'input> {
    fn from(value: &String) -> Self {
        Address::Name(value.clone())
    }
}

impl<'input> Address<'input> {
    pub fn new_const(ty: Type<'input>, literal: &'input str) -> Self {
        Address::Const(Constant::new(ty, literal))
    }

    pub fn new_global_ref(label: Label) -> Self {
        Address::Global(label)
    }

    pub fn new_copy_name(name: String) -> Self {
        Address::Name(name)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TempVar(usize);

impl From<usize> for TempVar {
    fn from(value: usize) -> Self {
        TempVar(value)
    }
}

impl fmt::Display for TempVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "_t{}", self.0)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Constant<'input> {
    pub ty: Type<'input>,
    pub literal: &'input str,
}

impl<'input> fmt::Display for Constant<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl<'input> Constant<'input> {
    pub fn new(ty: Type<'input>, literal: &'input str) -> Self {
        Constant { ty, literal }
    }
}
