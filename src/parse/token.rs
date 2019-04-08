use std::fmt;

use crate::types::Simple;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token<'src> {
    NullLit,
    Ident(&'src str),
    DecLit(&'src str),
    FloatLit(&'src str),
    StringLit(&'src str),
    TypeIdent(Simple<'src>),

    // Keywords
    Let,
    Def,
    If,
    Else,
    Import,
    Return,
    Extern,
    While,
    Type,
    Struct,
    New,
    Delete,
    Sizeof,

    // Operators
    Bang,   // !
    Equals, // =
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /

    Smaller,   // <
    Greater,   // >
    Ampersand, // &

    TripleDot, // ...
    Colon,     // :
    Semi,      // ;
    Dot,       // .
    Comma,     // ,

    Pipe,   // |
    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    // double Operators
    EqualsEquals,       // ==
    BangEquals,         // !=
    SmallerEquals,      // <=
    GreaterEquals,      // >=
    AmpersandAmpersand, // &&
}

#[repr(u8)]
#[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub enum Precedence {
    None = 0,
    Assign = 1,
    And = 2,
    Equality = 3,
    Comparison = 4,
    Sum = 5,
    Product = 6,
    Unary = 7,
    Call = 8,
}

impl<'src> Token<'src> {
    pub fn precedence(&self) -> Precedence {
        match self {
            Token::Equals => Precedence::Assign,
            Token::AmpersandAmpersand => Precedence::And,
            Token::EqualsEquals | Token::BangEquals => Precedence::Equality,
            Token::Greater | Token::GreaterEquals | Token::Smaller | Token::SmallerEquals => {
                Precedence::Comparison
            }
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Star | Token::Slash => Precedence::Product,
            Token::LParen | Token::Dot | Token::LBrace => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::NullLit => write!(f, "null"),
            Token::Ident(ref name) => write!(f, "{}", name),
            Token::StringLit(ref lit) => write!(f, "{}", lit),
            Token::DecLit(lit) => write!(f, "{}", lit.to_string()),
            Token::FloatLit(lit) => write!(f, "{}", lit.to_string()),
            Token::TypeIdent(ty) => write!(f, "{}", ty.to_string()),
            // Keywords
            Token::Type => write!(f, "type"),
            Token::Struct => write!(f, "struct"),
            Token::Let => write!(f, "let"),
            Token::Def => write!(f, "def"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Import => write!(f, "import"),
            Token::Return => write!(f, "return"),
            Token::Extern => write!(f, "extern"),
            Token::While => write!(f, "while"),
            Token::New => write!(f, "new"),
            Token::Delete => write!(f, "delete"),
            Token::Sizeof => write!(f, "sizeof"),

            // Operators
            Token::Bang => write!(f, "!"),
            Token::Equals => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),

            Token::Smaller => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::Ampersand => write!(f, "&"),

            Token::TripleDot => write!(f, "..."),
            Token::Colon => write!(f, ":"),
            Token::Semi => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::Comma => write!(f, ","),

            Token::Pipe => write!(f, "|"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),

            // double Operators
            Token::EqualsEquals => write!(f, "=="),
            Token::BangEquals => write!(f, "!="),
            Token::SmallerEquals => write!(f, "<="),
            Token::GreaterEquals => write!(f, ">="),
            Token::AmpersandAmpersand => write!(f, "&&"),
        }
    }
}
