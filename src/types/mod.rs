use std::{fmt, hash};

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
    pub fn is_closure(&self) -> bool {
        if let Type::Simple(Simple::Closure(..)) = self {
            return true;
        }
        false
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

pub type Module<'src> = &'src str;
pub type TypeIndex = usize;

#[derive(Debug, Clone, Eq)]
pub struct ClosureParamList<'src>(pub Vec<(&'src str, Type<'src>)>);

impl<'src> ClosureParamList<'src> {
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &(&'src str, Type<'src>)> {
        self.0.iter()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'src> PartialEq for ClosureParamList<'src> {
    fn eq(&self, other: &Self) -> bool {
        if other.0.len() == self.0.len() {
            return self.0.iter().zip(other.0.iter()).all(|(s, o)| s.1 == o.1);
        }

        false
    }
}

impl<'src> hash::Hash for ClosureParamList<'src> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        for t in self.0.iter().map(|(_, t)| t) {
            t.hash(state);
        }
    }
}

impl<'src> From<Vec<(&'src str, Type<'src>)>> for ClosureParamList<'src> {
    fn from(value: Vec<(&'src str, Type<'src>)>) -> Self {
        ClosureParamList(value)
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
    // (module, type index)
    Closure(Module<'src>, TypeIndex),
    Function(Box<ClosureType<'src>>),
    UserType(UserIdent<'src>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ClosureType<'src> {
    pub params: ClosureParamList<'src>,
    pub ret_ty: Type<'src>,
}

// TODO: implement display
impl<'src> ClosureType<'src> {
    pub fn new(params: Vec<(&'src str, Type<'src>)>, ret_ty: Type<'src>) -> Self {
        ClosureType {
            params: params.into(),
            ret_ty,
        }
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
            Simple::Closure(module, type_idx) => {
                return write!(f, "{}._closure_{}", module, type_idx);
            }
            // TODO: implement display
            Simple::Function(cls_ty) => return write!(f, "{:?}", cls_ty),
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
