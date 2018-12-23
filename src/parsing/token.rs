#[derive(Debug, Eq, PartialEq)]
pub enum Token<'input> {
    Ident(&'input str),
    DecLit(i64),

    // Keywords
    Let,
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
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeIdent {
    I32,
}
