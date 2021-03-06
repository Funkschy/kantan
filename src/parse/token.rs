use std::fmt;

use crate::types::Simple;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token<'src> {
    NullLit,
    Ident(&'src str),
    DecLit(&'src str),
    FloatLit(&'src str),
    StringLit(&'src str),
    Char(&'src str),
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
    As,

    // Operators
    Bang,    // !
    Equals,  // =
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    Smaller,   // <
    Greater,   // >
    Ampersand, // &
    Pipe,      // |

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
    EqualsEquals,       // ==
    BangEquals,         // !=
    SmallerEquals,      // <=
    GreaterEquals,      // >=
    AmpersandAmpersand, // &&
    PipePipe,           // ||
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
            Token::AmpersandAmpersand | Token::PipePipe => Precedence::And,
            Token::EqualsEquals | Token::BangEquals => Precedence::Equality,
            Token::Greater | Token::GreaterEquals | Token::Smaller | Token::SmallerEquals => {
                Precedence::Comparison
            }
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Star | Token::Slash | Token::Percent | Token::As => Precedence::Product,
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
            Token::Char(c) => write!(f, "'{}'", c),
            Token::DecLit(lit) | Token::FloatLit(lit) => write!(f, "{}", lit),
            Token::TypeIdent(ty) => write!(f, "{}", ty),
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
            Token::As => write!(f, "as"),

            // Operators
            Token::Bang => write!(f, "!"),
            Token::Equals => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),

            Token::Smaller => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::Ampersand => write!(f, "&"),
            Token::Pipe => write!(f, "|"),

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
            Token::BangEquals => write!(f, "!="),
            Token::SmallerEquals => write!(f, "<="),
            Token::GreaterEquals => write!(f, ">="),
            Token::AmpersandAmpersand => write!(f, "&&"),
            Token::PipePipe => write!(f, "||"),
        }
    }
}
