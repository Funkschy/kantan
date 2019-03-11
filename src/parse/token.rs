use std::fmt;

use crate::types::Type;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Token<'input> {
    Ident(&'input str),
    DecLit(&'input str),
    StringLit(&'input str),
    TypeIdent(Type<'input>),

    // Keywords
    Let,
    Fn,
    If,
    Else,
    Import,
    Return,
    Extern,
    While,
    Type,
    Struct,

    // Operators
    Equals, // =
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /

    Smaller, // <

    TripleDot, // ...
    Colon,     // :
    Semi,      // ;
    Dot,       // .
    Comma,     // ,

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    // double Operators
    EqualsEquals,  // ==
    SmallerEquals, // <=
}

#[repr(u8)]
#[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub enum Precedence {
    None = 0,
    Assign = 1,
    Equality = 2,
    Comparison = 3,
    Sum = 4,
    Product = 5,
    Call = 6,
}

impl<'input> Token<'input> {
    pub fn precedence(&self) -> Precedence {
        match self {
            Token::Equals => Precedence::Assign,
            Token::EqualsEquals => Precedence::Equality,
            // TODO: add greater
            Token::Smaller | Token::SmallerEquals => Precedence::Comparison,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Star | Token::Slash => Precedence::Product,
            Token::LParen | Token::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(ref name) => write!(f, "{}", name),
            Token::StringLit(ref lit) => write!(f, "{}", lit),
            Token::DecLit(lit) => write!(f, "{}", lit.to_string()),
            Token::TypeIdent(ty) => write!(f, "{}", ty.to_string()),
            // Keywords
            Token::Type => write!(f, "type"),
            Token::Struct => write!(f, "struct"),
            Token::Let => write!(f, "let"),
            Token::Fn => write!(f, "fn"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Import => write!(f, "import"),
            Token::Return => write!(f, "return"),
            Token::Extern => write!(f, "extern"),
            Token::While => write!(f, "while"),

            // Operators
            Token::Equals => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),

            Token::Smaller => write!(f, "<"),

            Token::TripleDot => write!(f, "..."),
            Token::Colon => write!(f, ":"),
            Token::Semi => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::Comma => write!(f, ","),

            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),

            // double Operators
            Token::EqualsEquals => write!(f, "=="),
            Token::SmallerEquals => write!(f, "<="),
        }
    }
}
