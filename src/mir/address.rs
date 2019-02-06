use std::fmt;

use super::tac::{Expression, Label};
use crate::types::Type;

#[derive(PartialEq, Debug, Clone)]
pub enum Address<'input> {
    Name(&'input str),
    Const(Constant<'input>),
    CompConst(CompilerConstant),
    Temp(TempVar),
    Global(Label),
}

impl<'input> fmt::Display for Address<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Address::*;

        let s = match self {
            Name(n) => n.to_string(),
            Const(c) => c.to_string(),
            CompConst(c) => c.to_string(),
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

impl<'input> From<&'input str> for Address<'input> {
    fn from(value: &'input str) -> Self {
        Address::Name(value)
    }
}

impl<'input> Address<'input> {
    pub fn new_const(ty: Type, literal: &'input str) -> Self {
        Address::Const(Constant::new(ty, literal))
    }

    pub fn new_global_ref(label: Label) -> Self {
        Address::Global(label)
    }

    pub fn new_copy_name(name: &'input str) -> Self {
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
    ty: Type,
    literal: &'input str,
}

impl<'input> fmt::Display for Constant<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl<'input> Constant<'input> {
    pub fn new(ty: Type, literal: &'input str) -> Self {
        Constant { ty, literal }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct CompilerConstant {
    ty: Type,
    pub literal: String,
}

impl<'input> fmt::Display for CompilerConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl<'input> CompilerConstant {
    pub fn new(ty: Type, literal: String) -> Self {
        CompilerConstant { ty, literal }
    }
}
