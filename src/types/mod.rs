use std::fmt;

#[derive(Debug, Eq, Copy, Clone, PartialEq, Hash)]
pub struct UserIdent<'src> {
    file: &'src str,
    name: &'src str,
}

impl<'src> UserIdent<'src> {
    pub fn new(file: &'src str, name: &'src str) -> Self {
        UserIdent { file, name }
    }

    #[inline(always)]
    pub fn name(&self) -> &'src str {
        self.name
    }

    #[inline(always)]
    pub fn module(&self) -> &'src str {
        self.file
    }
}

impl<'src> fmt::Display for UserIdent<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.file, self.name)
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
            Type::Pointer(_) => true,
            Type::Simple(Simple::String) => true,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Simple<'src> {
    I32,
    F32,
    Bool,
    String,
    Void,
    Varargs,
    UserType(UserIdent<'src>),
    Closure(ClosureType<'src>),
}

#[derive(Debug, Clone, Eq)]
pub struct ClosureType<'src> {
    pub params: Vec<(&'src str, Type<'src>)>,
    pub ret_ty: Box<Type<'src>>,
    pub index: usize,
}

impl<'src> ClosureType<'src> {
    pub fn new(
        params: Vec<(&'src str, Type<'src>)>,
        ret_ty: Box<Type<'src>>,
        index: usize,
    ) -> Self {
        ClosureType {
            params,
            ret_ty,
            index,
        }
    }
}

impl<'src> PartialEq for ClosureType<'src> {
    fn eq(&self, other: &Self) -> bool {
        self.ret_ty == other.ret_ty && self.params == other.params
    }
}

impl<'src> Simple<'src> {
    pub fn arithmetic(&self) -> bool {
        match self {
            Simple::I32 => true,
            Simple::F32 => true,
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
            Simple::Closure(ClosureType { params, ret_ty, .. }) => {
                return write!(
                    f,
                    "({}) -> {}",
                    params
                        .iter()
                        .map(|(_, p)| p)
                        .map(std::string::ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret_ty
                );
            }
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
