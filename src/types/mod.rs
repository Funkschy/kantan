use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type<'src> {
    Simple(Simple<'src>),
    Pointer(Pointer<'src>),
}

impl<'src> Type<'src> {
    pub fn is_ptr(&self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Simple<'src> {
    I32,
    Bool,
    String,
    Void,
    Varargs,
    UserType(&'src str),
}

impl<'src> fmt::Display for Simple<'src> {
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
pub struct Pointer<'src> {
    /// the number of references: 1 = *, 2 = **
    pub number: usize,
    pub ty: Simple<'src>,
}

impl<'src> Pointer<'src> {
    pub fn new(number: usize, ty: Simple<'src>) -> Self {
        Pointer { number, ty }
    }
}

impl<'src> fmt::Display for Pointer<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", "*".repeat(self.number), self.ty.to_string())
    }
}

impl<'src> fmt::Display for Type<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Type::Simple(s) => s.to_string(),
            Type::Pointer(p) => p.to_string(),
        };

        write!(f, "{}", s)
    }
}
