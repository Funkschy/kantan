use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type<'input> {
    Simple(Simple<'input>),
    Pointer(Pointer<'input>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Simple<'input> {
    I32,
    Bool,
    String,
    Void,
    Varargs,
    UserType(&'input str),
}

impl<'input> fmt::Display for Simple<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Simple::I32 => "i32",
            Simple::String => "string",
            Simple::Bool => "bool",
            Simple::Void => "void",
            Simple::Varargs => "...",
            Simple::UserType(name) => name,
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Pointer<'input> {
    /// the number of references: 1 = *, 2 = **
    pub number: usize,
    pub ty: Simple<'input>,
}

impl<'input> Pointer<'input> {
    pub fn new(number: usize, ty: Simple<'input>) -> Self {
        Pointer { number, ty }
    }
}

impl<'input> fmt::Display for Pointer<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", "*".repeat(self.number), self.ty.to_string())
    }
}

impl<'input> fmt::Display for Type<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Type::Simple(s) => s.to_string(),
            Type::Pointer(p) => p.to_string(),
        };

        write!(f, "{}", s)
    }
}
