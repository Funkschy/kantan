use std::fmt;

use super::tac::{Expression, Label};
use crate::types::Type;

#[derive(PartialEq, Debug, Clone)]
pub enum Address<'src> {
    Empty,
    Name(String),
    Const(Constant<'src>),
    Temp(TempVar),
    Global(Label),
}

impl<'src> fmt::Display for Address<'src> {
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

impl<'src> Into<Expression<'src>> for Address<'src> {
    fn into(self) -> Expression<'src> {
        Expression::Copy(self)
    }
}

impl<'src> From<&String> for Address<'src> {
    fn from(value: &String) -> Self {
        Address::Name(value.clone())
    }
}

impl<'src> Address<'src> {
    pub fn new_const(ty: Type<'src>, literal: &'src str) -> Self {
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
pub struct Constant<'src> {
    pub ty: Type<'src>,
    pub literal: &'src str,
}

impl<'src> fmt::Display for Constant<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl<'src> Constant<'src> {
    pub fn new(ty: Type<'src>, literal: &'src str) -> Self {
        Constant { ty, literal }
    }
}
