use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    I32,
    Bool,
    String,
    Void,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Type::I32 => "i32",
            Type::String => "string",
            Type::Bool => "bool",
            Type::Void => "void",
        };

        write!(f, "{}", s)
    }
}
