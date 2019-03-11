use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type<'input> {
    I32,
    Bool,
    String,
    Void,
    Varargs,
    UserType(&'input str),
}

impl<'input> fmt::Display for Type<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Type::I32 => "i32",
            Type::String => "string",
            Type::Bool => "bool",
            Type::Void => "void",
            Type::Varargs => "...",
            Type::UserType(name) => name,
        };

        write!(f, "{}", s)
    }
}
