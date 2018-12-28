#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Token<'input> {
    Ident(&'input str),
    DecLit(i64),

    // Keywords
    Let,
    Fn,
    TypeIdent(TypeIdent),

    // Operators
    Equals, // =
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /

    Colon, // :
    Semi,  // ;

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TypeIdent {
    I32,
}

#[repr(u8)]
#[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub enum Precedence {
    None = 0,
    Sum = 1,
    Product = 2,
}

impl<'input> Token<'input> {
    pub fn precedence(&self) -> Precedence {
        match self {
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Star | Token::Slash => Precedence::Product,
            _ => Precedence::None,
        }
    }
}
