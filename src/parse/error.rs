use super::token::Token;
use super::Span;
use std::error;
use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError<'input> {
    LexError(LexError),
    PrefixError(String),
    InfixError(String),
    ConsumeError {
        actual: Token<'input>,
        expected: Token<'input>,
    },
}

impl<'input> fmt::Display for ParseError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::LexError(err) => write!(f, "{}", err.as_string()),
            ParseError::PrefixError(err) => write!(f, "{}", err),
            ParseError::InfixError(err) => write!(f, "{}", err),
            ParseError::ConsumeError { expected, actual } => {
                write!(f, "Expected {:?}, but got {:?}", expected, actual)
            }
        }
    }
}

impl<'input> From<LexError> for ParseError<'input> {
    fn from(value: LexError) -> Self {
        ParseError::LexError(value)
    }
}

#[derive(Eq, PartialEq)]
pub struct LexError {
    pub(crate) span: Span,
    cause: Option<String>,
}

impl LexError {
    pub fn new(span: Span) -> Self {
        LexError { span, cause: None }
    }

    pub fn with_cause(span: Span, cause: &str) -> Self {
        LexError {
            span,
            cause: Some(cause.to_owned()),
        }
    }

    fn as_string(&self) -> String {
        let msg = format!(
            "[{}:{}] Failed to lex token",
            self.span.start, self.span.end
        );

        if let Some(ref reason) = self.cause {
            format!("{}, because: {}", msg, reason)
        } else {
            msg.to_string()
        }
    }
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

impl error::Error for LexError {}
