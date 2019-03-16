use std::{borrow::Borrow, fmt};

use crate::{parse::token::Token, types::Type};

use super::address::Address;

#[derive(PartialEq, Debug)]
pub struct BasicBlock<'src> {
    pub instructions: Vec<Instruction<'src>>,
    pub terminator: Instruction<'src>,
}

impl<'src> Default for BasicBlock<'src> {
    fn default() -> Self {
        BasicBlock {
            instructions: vec![],
            terminator: Instruction::Nop,
        }
    }
}

#[derive(Debug, Default)]
pub struct InstructionBlock<'src>(pub Vec<Instruction<'src>>);

impl<'src> InstructionBlock<'src> {
    pub fn push(&mut self, instr: Instruction<'src>) {
        self.0.push(instr);
    }

    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0);
    }

    pub fn last(&self) -> Option<&Instruction<'src>> {
        self.0.last()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Label(String);

impl<'src> fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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

impl From<&str> for Label {
    #[inline]
    fn from(value: &str) -> Self {
        Label(value.to_owned())
    }
}

impl Borrow<str> for Label {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl<'src> Into<Instruction<'src>> for Label {
    fn into(self) -> Instruction<'src> {
        Instruction::Label(self)
    }
}

#[derive(PartialEq, Debug)]
pub enum Instruction<'src> {
    /// let x: i32
    Decl(Address<'src>, Type<'src>),
    /// x = <expr>
    Assignment(Address<'src>, Expression<'src>),
    /// goto l
    Jmp(Label),
    /// if x goto l0 else goto l1
    JmpIf(Address<'src>, Label, Label),
    /// return x
    Return(Option<Address<'src>>),
    /// .L0:
    Label(Label),
    /// No operation
    Nop,
}

impl<'src> fmt::Display for Instruction<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;

        let s = match self {
            Decl(name, ty) => format!("let {}: {};", name, ty),
            Assignment(a, e) => format!("{} = {};", a, e),
            Jmp(l) => format!("goto {};", l),
            JmpIf(a, l0, l1) => format!("if {} goto {} else goto {};", a, l0, l1),
            Return(Some(a)) => format!("return {};", a),
            Return(None) => "return;".to_string(),
            Label(l) => format!("{}:", l),
            Nop => "nop".to_owned(),
        };

        write!(f, "{}", s)
    }
}

/// An expression is always on the right side of an assignment instruction
/// In the comments, this assignment is denoted as 'x = '
#[derive(PartialEq, Debug, Clone)]
pub enum Expression<'src> {
    /// x = y op z
    Binary(Address<'src>, BinaryType, Address<'src>),
    /// x = op y
    Unary(UnaryType, Address<'src>),
    /// x = y
    Copy(Address<'src>),
    /// x = call f (y, z)
    Call(Label, Vec<Address<'src>>, Type<'src>),
    /// Gets a pointer to the Xth element of a struct or array
    /// x = base + offset
    StructGep(Address<'src>, u32),
    /// x = test { 41, "test" }
    StructInit(&'src str, Vec<Address<'src>>),
}

impl<'src> fmt::Display for Expression<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;

        let s = match self {
            Binary(l, op, r) => format!("{} {} {}", l, op, r),
            Unary(op, a) => format!("{} {}", op, a),
            Copy(a) => format!("{}", a),
            StructGep(a, offset) => format!("structgep {} offset {}", a, offset),
            StructInit(ident, values) => format!(
                "{} {{ {} }}",
                ident,
                values
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Call(f, args, _) => {
                let args: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                let args = args.join(", ");
                format!("call {}({})", f, args)
            }
        };

        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryType {
    BoolNegate,
    I32Negate,
}

impl fmt::Display for UnaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnaryType::*;

        let s = match self {
            I32Negate => "-",
            BoolNegate => "!",
        };

        write!(f, "{}", s)
    }
}

impl<'a> From<&Token<'a>> for Option<UnaryType> {
    fn from(value: &Token) -> Self {
        match value {
            Token::Minus => Some(UnaryType::I32Negate),
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum BinaryType {
    I16(IntBinaryType),
    I32(IntBinaryType),
}

impl fmt::Display for BinaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryType::*;

        let s = match self {
            I16(bt) | I32(bt) => bt.to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum IntBinaryType {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Smaller,
    SmallerEq,
}

impl<'a> From<&Token<'a>> for Option<IntBinaryType> {
    fn from(value: &Token) -> Self {
        use IntBinaryType::*;

        match value {
            Token::Plus => Some(Add),
            Token::Minus => Some(Sub),
            Token::Star => Some(Mul),
            Token::Slash => Some(Div),
            Token::EqualsEquals => Some(Eq),
            Token::Smaller => Some(Smaller),
            Token::SmallerEquals => Some(SmallerEq),
            _ => None,
        }
    }
}

impl fmt::Display for IntBinaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use IntBinaryType::*;

        let s = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Eq => "==",
            Smaller => "<",
            SmallerEq => "<=",
        };

        write!(f, "{}", s)
    }
}
