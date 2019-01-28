use std::fmt;

use crate::types::Type;

pub struct Func<'input> {
    label: Label,
    params: Vec<Type>,
    ret: Type,
    block: InstructionBlock<'input>,
}

impl<'input> Func<'input> {
    pub fn new(
        label: Label,
        params: Vec<Type>,
        ret: Type,
        block: InstructionBlock<'input>,
    ) -> Self {
        Func {
            label,
            params,
            ret,
            block,
        }
    }
}

pub struct InstructionBlock<'input>(Vec<Instruction<'input>>);

pub struct Label(String);

impl Label {
    pub fn new(number: usize) -> Self {
        Label(format!(".L{}", number))
    }
}

impl From<String> for Label {
    #[inline]
    fn from(value: String) -> Self {
        Label(value)
    }
}

pub enum Instruction<'input> {
    /// x = <expr>
    Assignment(Address<'input>, Expression<'input>),
    /// goto l
    Jmp(Label),
    /// if x goto l
    JmpIf(Address<'input>, Label),
    /// return x
    Return(Address<'input>),
}

/// An expression is always on the right side of an assignment instruction
/// In the comments, this assignment is denoted as 'x = '
pub enum Expression<'input> {
    /// x = y op z
    Binary(Address<'input>, BinaryType, Address<'input>),
    /// x = op y
    Unary(UnaryType, Address<'input>),
    /// x = y
    Copy(Address<'input>),
    /// x = &y
    Ref(Address<'input>),
    /// x = *y
    DeRef(Address<'input>),
    /// x = call f (y, z)
    Call(Func<'input>, Vec<Address<'input>>),
}

pub enum UnaryType {
    I32Negate,
}

pub enum BinaryType {
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
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
