use std::{borrow::Borrow, fmt};

use crate::{parse::token::Token, types::*};

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
    Assignment(Address<'src>, Box<Expression<'src>>),
    /// goto l
    Jmp(Label),
    /// if x goto l0 else goto l1
    JmpIf(Address<'src>, Label, Label),
    /// return x
    Return(Option<Address<'src>>),
    /// .L0:
    Label(Label),
    /// frees heap memory
    Delete(Address<'src>),
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
            Delete(a) => format!("delete({})", a),
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
    Call {
        ident: UserIdent<'src>,
        args: Vec<Address<'src>>,
        ret_type: Type<'src>,
        varargs: bool,
    },
    CallFuncPtr {
        ident: Address<'src>,
        args: Vec<Address<'src>>,
        ret_type: Type<'src>,
    },
    /// Gets a pointer to the Xth element of a struct or array
    /// x = base + offset
    StructGep(Address<'src>, u32),
    /// x = test { 41, "test" }
    StructInit(UserIdent<'src>, Vec<Address<'src>>),
    /// allocates the value of its address on the heap
    /// x = new 5
    New(Address<'src>, Type<'src>),
    /// x = sizeof(ty)
    SizeOf(Type<'src>),
    /// x = param #y
    GetParam(u32),
}

impl<'src> fmt::Display for Expression<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;

        let s = match self {
            Binary(l, op, r) => format!("{} {} {}", l, op, r),
            Unary(op, a) => format!("{} {}", op, a),
            Copy(a) => format!("{}", a),
            New(a, ty) => format!("new(sizeof({}), {})", ty, a),
            SizeOf(ty) => format!("sizeof({})", ty),
            StructGep(a, offset) => format!("structgep {} offset {}", a, offset),
            GetParam(i) => format!("param #{}", i),
            StructInit(ident, values) => format!(
                "{} {{ {} }}",
                ident,
                values
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Call {
                ident: f,
                args,
                varargs,
                ..
            } => {
                let args: Vec<String> = args.iter().map(std::string::ToString::to_string).collect();
                let args = args.join(", ");
                let varargs = if *varargs { "varargs " } else { "" };
                format!("{}call {}({})", varargs, f, args)
            }
            CallFuncPtr { ident, args, .. } => {
                let args: Vec<String> = args.iter().map(std::string::ToString::to_string).collect();
                let args = args.join(", ");
                format!("call_ptr {}({})", ident, args)
            }
        };

        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum UnaryType {
    BoolNegate,
    I32Negate,
    Deref,
}

impl fmt::Display for UnaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnaryType::*;

        let s = match self {
            I32Negate => "-",
            BoolNegate => "!",
            Deref => "*",
        };

        write!(f, "{}", s)
    }
}

impl<'a> From<&Token<'a>> for Option<UnaryType> {
    fn from(value: &Token) -> Self {
        match value {
            Token::Minus => Some(UnaryType::I32Negate),
            Token::Star => Some(UnaryType::Deref),
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum BinaryType {
    Ptr(PtrBinaryType),
    I16(NumBinaryType),
    I32(NumBinaryType),
    F32(NumBinaryType),
}

impl fmt::Display for BinaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryType::*;

        let s = match self {
            I16(bt) | I32(bt) | F32(bt) => bt.to_string(),
            Ptr(bt) => bt.to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum PtrBinaryType {
    Add,
    Sub,
}

impl fmt::Display for PtrBinaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PtrBinaryType::*;

        let s = match self {
            Add => "+",
            Sub => "-",
        };

        write!(f, "{}", s)
    }
}

impl<'a> From<&Token<'a>> for Option<PtrBinaryType> {
    fn from(value: &Token) -> Self {
        use PtrBinaryType::*;

        match value {
            Token::Plus => Some(Add),
            Token::Minus => Some(Sub),
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum NumBinaryType {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Smaller,
    SmallerEq,
    Greater,
    GreaterEq,
}

impl<'a> From<&Token<'a>> for Option<NumBinaryType> {
    fn from(value: &Token) -> Self {
        use NumBinaryType::*;

        match value {
            Token::Plus => Some(Add),
            Token::Minus => Some(Sub),
            Token::Star => Some(Mul),
            Token::Slash => Some(Div),
            Token::EqualsEquals => Some(Eq),
            Token::BangEquals => Some(Neq),
            Token::Smaller => Some(Smaller),
            Token::SmallerEquals => Some(SmallerEq),
            Token::Greater => Some(Greater),
            Token::GreaterEquals => Some(GreaterEq),
            _ => None,
        }
    }
}

impl fmt::Display for NumBinaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use NumBinaryType::*;

        let s = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Eq => "==",
            Neq => "!=",
            Smaller => "<",
            SmallerEq => "<=",
            Greater => ">",
            GreaterEq => ">=",
        };

        write!(f, "{}", s)
    }
}
