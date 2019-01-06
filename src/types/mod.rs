use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Type {
    I32,
    Bool,
    String,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Type::I32 => "i32",
            Type::String => "string",
            Type::Bool => "bool",
        };

        write!(f, "{}", s)
    }
}
