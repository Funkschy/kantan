use std::fmt;

use crate::types::Type;

#[derive(Debug)]
pub struct Func<'input> {
    label: Label,
    params: Vec<(&'input str, Type)>,
    ret: Type,
    block: InstructionBlock<'input>,
}

impl<'input> Func<'input> {
    pub fn new(
        label: Label,
        params: Vec<(&'input str, Type)>,
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

impl<'input> fmt::Display for Func<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|(n, t)| format!("{}: t_{}", n, t))
            .collect::<Vec<String>>()
            .join(", ");

        let instructions = self
            .block
            .0
            .iter()
            .map(|i| format!("\t{}", i))
            .collect::<Vec<String>>()
            .join("\n");

        write!(
            f,
            "fn {}({}): {} {{\n{}\n}}",
            self.label, params, self.ret, instructions
        )
    }
}

#[derive(Debug, Default)]
pub struct InstructionBlock<'input>(Vec<Instruction<'input>>);

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

impl<'input> Into<Instruction<'input>> for Label {
    fn into(self) -> Instruction<'input> {
        Instruction::Label(self)
    }
}

#[derive(Debug)]
pub enum Instruction<'input> {
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
}

impl<'input> fmt::Display for Instruction<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;

        let s = match self {
            Assignment(a, e) => format!("{} = {};", a, e),
            Jmp(l) => format!("goto {};", l),
            JmpIf(a, l0, l1) => format!("if {} goto {} else goto {};", a, l0, l1),
            Return(Some(a)) => format!("return {};", a),
            Return(None) => "return;".to_string(),
            Label(l) => format!("{}:", l),
        };

        write!(f, "{}", s)
    }
}

/// An expression is always on the right side of an assignment instruction
/// In the comments, this assignment is denoted as 'x = '
#[derive(Debug)]
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
    /// empty
    Empty,
}

impl<'input> Expression<'input> {
    pub fn is_empty(&self) -> bool {
        if let Expression::Empty = self {
            return true;
        }

        false
    }
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
            Call(f, args) => {
                let args: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                let args = args.join(", ");
                format!("call {}({})", f, args)
            }
            Empty => "empty".to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum BinaryType {
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32Eq,
    I32Smaller,
    I32SmallerEq,
}

impl fmt::Display for BinaryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryType::*;

        let s = match self {
            I32Add => "+",
            I32Sub => "-",
            I32Mul => "*",
            I32Div => "/",
            I32Eq => "==",
            I32Smaller => "<",
            I32SmallerEq => "<=",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub enum Address<'input> {
    Name(&'input str),
    Const(Constant<'input>),
    Temp(TempVar),
    Global(Label),
}

impl<'input> fmt::Display for Address<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Address::*;

        let s = match self {
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
