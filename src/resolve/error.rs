use std::fmt;

use super::types::Type;
use crate::{err_to_string, find_line_index, format_error, Source, Span};

#[derive(Debug, Eq, PartialEq)]
pub struct ResolveError<'input> {
    pub source: &'input Source,
    pub error: ResolveErrorType<'input>,
    pub err_span: Span,
    pub expr_span: Span,
}

impl<'input> ResolveError<'input> {
    fn err_token(&self) -> &'input str {
        &self.source.code[self.err_span.start..=self.err_span.end]
    }
}

impl<'input> fmt::Display for ResolveError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let binoperr_to_string = |err: &BinaryOperationError| {
            format!(
                "{} - not allowed",
                format_error(
                    self.source,
                    self.expr_span,
                    self.err_span,
                    &format!(
                        "binary operation '{}' cannot be applied to '{}' and '{}'",
                        self.err_token(),
                        err.left_type,
                        err.right_type
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
            ResolveErrorType::NotDefined(DefinitionError { name }) => format_error(
                self.source,
                self.expr_span,
                self.err_span,
                &format!("'{}' not in scope", name),
            ),
            ResolveErrorType::IllegalOperation(ref err) => binoperr_to_string(err),
            ResolveErrorType::IllegalType(IllegalTypeError {
                expected_type,
                actual_type,
                name,
            }) => format_error(
                self.source,
                self.expr_span,
                self.err_span,
                &format!(
                    "{} must be of type '{}', but the supplied type was '{}'",
                    name, expected_type, actual_type
                ),
            ),
            ResolveErrorType::SelfImport(_) => format_error(
                self.source,
                self.expr_span,
                self.err_span,
                "cannot import self",
            ),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ResolveErrorType<'input> {
    IllegalAssignment(AssignmentError<'input>),
    NotDefined(DefinitionError<'input>),
    IllegalOperation(BinaryOperationError),
    IllegalType(IllegalTypeError),
    SelfImport(SelfImportError),
}

#[derive(Debug, Eq, PartialEq)]
pub struct SelfImportError;

#[derive(Debug, Eq, PartialEq)]
pub struct IllegalTypeError {
    pub expected_type: Type,
    pub actual_type: Type,
    // e.g. "If condition" or "while condition"
    pub name: &'static str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AssignmentError<'input> {
    pub name: &'input str,
    pub definition_span: Span,
    pub bin_op_err: BinaryOperationError,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BinaryOperationError {
    pub left_type: Type,
    pub right_type: Type,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinitionError<'input> {
    pub name: &'input str,
}
