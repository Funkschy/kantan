use super::token::Token;
use crate::REPO_URL;
use std::{error, fmt};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParseError<'src> {
    LexError(LexError),
    PrefixError(String),
    InfixError(String),
    InternalError(&'static str),
    ConsumeError {
        actual: Token<'src>,
        expected: String,
    },
}

impl<'src> fmt::Display for ParseError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::LexError(err) => write!(f, "{}", err.as_string()),
            ParseError::PrefixError(err) => write!(f, "{}", err),
            ParseError::InfixError(err) => write!(f, "{}", err),
            ParseError::InternalError(err) => write!(
                f,
                "An internal error has occured: {}\nPlease report this at {}/issues",
                err, REPO_URL
            ),
            ParseError::ConsumeError { expected, actual } => {
                write!(f, "Expected {}, but got {}", expected, actual)
            }
        }
    }
}

impl<'src> From<LexError> for ParseError<'src> {
    fn from(value: LexError) -> Self {
        ParseError::LexError(value)
    }
}

#[derive(Default, Eq, PartialEq, Clone, Hash)]
pub struct LexError {
    cause: Option<String>,
}

impl LexError {
    pub fn with_cause(cause: &str) -> Self {
        LexError {
            cause: Some(cause.to_owned()),
        }
    }

    fn as_string(&self) -> String {
        let msg = "Failed to lex token";

        if let Some(ref reason) = self.cause {
            format!("{}, because: {}", msg, reason)
        } else {
            msg.to_string()
        }
    }
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl error::Error for LexError {}
