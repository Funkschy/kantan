use std::{cell::Cell, fmt, hash};

#[derive(Debug, Eq, Clone, PartialEq)]
pub struct UserIdent<'src> {
    file: Cell<&'src str>,
    name: &'src str,
}

impl<'src> UserIdent<'src> {
    pub fn new(file: &'src str, name: &'src str) -> Self {
        UserIdent {
            file: Cell::new(file),
            name,
        }
    }

    #[inline]
    pub fn name(&self) -> &'src str {
        self.name
    }

    #[inline]
    pub fn set_module(&self, value: &'src str) {
        self.file.set(value);
    }

    #[inline]
    pub fn module(&self) -> &'src str {
        self.file.get()
    }
}

impl<'src> fmt::Display for UserIdent<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.module(), self.name())
    }
}

impl<'src> hash::Hash for UserIdent<'src> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.module().hash(state);
        self.name().hash(state);
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type<'src> {
    Simple(Simple<'src>),
    Pointer(Pointer<'src>),
}

impl<'src> Type<'src> {
    #[inline]
    pub fn is_ptr(&self) -> bool {
        match self {
            Type::Pointer(_) | Type::Simple(Simple::String) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        match self {
            Type::Simple(Simple::I32) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        match self {
            Type::Simple(Simple::F32) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_num(&self) -> bool {
        self.is_int() || self.is_float()
    }

    #[inline]
    pub fn simple(&self) -> &Simple<'src> {
        match self {
            Type::Simple(s) => &s,
            Type::Pointer(p) => &p.ty,
        }
    }

    /// Checks if you can do arithmetic operations (+, -, *, ...) on this type
    #[inline]
    pub fn arithmetic(&self) -> bool {
        if self.is_ptr() {
            true
        } else if let Type::Simple(s) = self {
            s.arithmetic()
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Simple<'src> {
    I32,
    F32,
    Bool,
    String,
    Void,
    Varargs,
    UserType(UserIdent<'src>),
}

impl<'src> Simple<'src> {
    pub fn arithmetic(&self) -> bool {
        match self {
            Simple::I32 | Simple::F32 => true,
            _ => false,
        }
    }
}

impl<'src> fmt::Display for Simple<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Simple::I32 => "i32",
            Simple::F32 => "f32",
            Simple::String => "string",
            Simple::Bool => "bool",
            Simple::Void => "void",
            Simple::Varargs => "...",
            Simple::UserType(name) => return write!(f, "{}", name),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
