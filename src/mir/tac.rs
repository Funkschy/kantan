use std::{borrow::Borrow, fmt};

use crate::{parse::token::Token, types::Type};

use super::address::{Address, CompilerConstant};

#[derive(PartialEq, Debug)]
pub struct BasicBlock<'input> {
    pub instructions: Vec<Instruction<'input>>,
    pub terminator: Instruction<'input>,
}

impl<'input> Default for BasicBlock<'input> {
    fn default() -> Self {
        BasicBlock {
            instructions: vec![],
            terminator: Instruction::Nop,
        }
    }
}

#[derive(Debug, Default)]
pub struct InstructionBlock<'input>(pub Vec<Instruction<'input>>);

impl<'input> InstructionBlock<'input> {
    pub fn push(&mut self, instr: Instruction<'input>) {
        self.0.push(instr);
    }

    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0);
    }

    pub fn last(&self) -> Option<&Instruction<'input>> {
        self.0.last()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Label(String);

impl<'input> fmt::Display for Label {
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

impl<'input> Into<Instruction<'input>> for Label {
    fn into(self) -> Instruction<'input> {
        Instruction::Label(self)
    }
}

#[derive(PartialEq, Debug)]
pub enum Instruction<'input> {
    /// let x: i32
    Decl(Address<'input>, Type<'input>),
    /// x = <expr>
    Assignment(Address<'input>, Expression<'input>),
    /// goto l
    Jmp(Label),
    /// if x goto l0 else goto l1
    JmpIf(Address<'input>, Label, Label),
    /// return x
    Return(Option<Address<'input>>),
    /// .L0:
    Label(Label),
    /// No operation
    Nop,
}

impl<'input> fmt::Display for Instruction<'input> {
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
    Call(Label, Vec<Address<'input>>),
    /// Gets a pointer to the Xth element of a struct or array
    /// x = base + offset
    StructGep(Address<'input>, u32),
}

impl<'input> fmt::Display for Expression<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;

        let s = match self {
            Binary(l, op, r) => format!("{} {} {}", l, op, r),
            Unary(op, a) => format!("{} {}", op, a),
            Copy(a) => format!("{}", a),
            Ref(a) => format!("ref {}", a),
            DeRef(a) => format!("deref {}", a),
            StructGep(a, offset) => format!("structgep {} offset {}", a, offset),
            Call(f, args) => {
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

macro_rules! boolean {
    ($e:expr) => {
        CompilerConstant::new(Type::Bool, ($e).to_string())
    };
}

macro_rules! integer {
    ($int_type:expr, $e:expr) => {
        CompilerConstant::new($int_type, ($e).to_string())
    };
}

impl IntBinaryType {
    pub fn execute(self, ty: Type, left: i128, right: i128) -> CompilerConstant {
        use IntBinaryType::*;

        match self {
            Add => integer!(ty, left + right),
            Sub => integer!(ty, left - right),
            Mul => integer!(ty, left * right),
            Div => integer!(ty, left / right),
            Eq => boolean!(left == right),
            Smaller => boolean!(left < right),
            SmallerEq => boolean!(left <= right),
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
