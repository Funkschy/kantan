use std::fmt;

use crate::types::Type;

pub struct Label(String);

pub enum Expression<'input> {
    Binary(Address<'input>, BinaryType, Address<'input>),
    Unary(UnaryType, Address<'input>),
    /// The value of the second address is copied to the first
    Copy(Address<'input>, Address<'input>),
    Jmp(Label),
}

pub enum UnaryType {
    I32Negate,
}

pub enum BinaryType {
    I32Add,
}

pub enum Address<'input> {
    Name(&'input str),
    Const(Constant<'input>),
    Temp(TempVar),
}

pub struct TempVar(usize);

impl fmt::Display for TempVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "_t{}", self.0)
    }
}

pub struct Constant<'input> {
    ty: Type,
    literal: &'input str,
}
