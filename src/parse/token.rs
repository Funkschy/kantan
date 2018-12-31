use std::fmt;

use crate::types::Type;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Token<'input> {
    Ident(&'input str),
    DecLit(i64),
    StringLit(&'input str),

    // Keywords
    Let,
    Fn,
    TypeIdent(Type),

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

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // for performance reasons, split into block, which needs allocation,
        // and block, which doesn't

        let mut needs_alloc = false;

        // no allocation needed
        let s = match self {
            Token::Ident(ref name) => name,
            Token::StringLit(ref lit) => lit,
            // Keywords
            Token::Let => "let",
            Token::Fn => "fn",
            // Operators
            Token::Equals => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",

            Token::Colon => ":",
            Token::Semi => ";",

            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            _ => {
                needs_alloc = true;
                ""
            }
        };

        if needs_alloc {
            let s = match self {
                Token::DecLit(lit) => lit.to_string(),
                Token::TypeIdent(ty) => ty.to_string(),
                // guarantee that all cases are checked eventually
                Token::Ident(_)
                | Token::StringLit(_)
                | Token::Let
                | Token::Fn
                | Token::Equals
                | Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Colon
                | Token::Semi
                | Token::LParen
                | Token::RParen
                | Token::LBrace
                | Token::RBrace => unreachable!(),
            };

            return write!(f, "{}", s);
        }

        write!(f, "{}", s)
    }
}
