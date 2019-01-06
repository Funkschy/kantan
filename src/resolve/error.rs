use std::fmt;

use super::types::Type;
use crate::{
    err_to_string, find_line_index, format_error, parse::token::Token, Source, Span, Spanned,
};

#[derive(Debug, Eq, PartialEq)]
pub struct ResolveError<'input> {
    pub source: &'input Source<'input>,
    pub error: ResolveErrorType<'input>,
}

impl<'input> fmt::Display for ResolveError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let binoperr_to_string = |&BinaryOperationError {
                                      expr_span,
                                      token,
                                      left_type,
                                      right_type,
                                  }| {
            format!(
                "{} - not allowed",
                format_error(
                    self.source,
                    expr_span,
                    token.span,
                    &format!(
                        "binary operation '{}' cannot be applied to '{}' and '{}'",
                        token.node, left_type, right_type
                    )
                )
            )
        };

        let s = match self.error {
            ResolveErrorType::IllegalAssignment(AssignmentError {
                name,
                definition_span,
                ref bin_op_err,
            }) => {
                let (line_nr, _) = find_line_index(self.source, definition_span.start);
                let reason = format!(
                    "{} - '{}' was defined as '{}' here",
                    err_to_string(self.source, definition_span, definition_span, line_nr, true),
                    name,
                    bin_op_err.left_type
                );
                format!("{}\n\nreason:\n{}", binoperr_to_string(bin_op_err), reason)
            }
            ResolveErrorType::NotDefined(DefinitionError {
                expr_span,
                name,
                name_span,
            }) => format_error(
                self.source,
                expr_span,
                name_span,
                &format!("'{}' not in scope", name),
            ),
            ResolveErrorType::IllegalOperation(ref err) => binoperr_to_string(err),
            ResolveErrorType::IllegalType(IllegalTypeError {
                expr_span,
                expected_type,
                actual_type,
                name,
            }) => format_error(
                self.source,
                expr_span,
                expr_span,
                &format!(
                    "{} must be of type '{}', but the supplied type was '{}'",
                    name, expected_type, actual_type
                ),
            ),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ResolveErrorType<'input> {
    IllegalAssignment(AssignmentError<'input>),
    NotDefined(DefinitionError<'input>),
    IllegalOperation(BinaryOperationError<'input>),
    IllegalType(IllegalTypeError),
}

#[derive(Debug, Eq, PartialEq)]
pub struct IllegalTypeError {
    pub expr_span: Span,
    pub expected_type: Type,
    pub actual_type: Type,
    // e.g. "If condition" or "while condition"
    pub name: &'static str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AssignmentError<'input> {
    pub name: &'input str,
    pub definition_span: Span,
    pub bin_op_err: BinaryOperationError<'input>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BinaryOperationError<'input> {
    pub token: Spanned<Token<'input>>,
    pub expr_span: Span,
    pub left_type: Type,
    pub right_type: Type,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinitionError<'input> {
    pub expr_span: Span,
    pub name: &'input str,
    pub name_span: Span,
}

impl<'input> DefinitionError<'input> {
    pub fn new(name: &'input str, expr_span: Span) -> Self {
        DefinitionError {
            name,
            expr_span,
            name_span: Span::new(expr_span.start, expr_span.start + name.len() - 1),
        }
    }
}
