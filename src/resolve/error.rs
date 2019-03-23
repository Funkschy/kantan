use std::fmt;

use super::types::Type;
use crate::{err_to_string, find_line_index, format_error, Source, Span};

#[derive(Debug, Eq, PartialEq)]
pub struct ResolveError<'src> {
    pub source: &'src Source,
    pub error: ResolveErrorType<'src>,
    pub err_span: Span,
    pub expr_span: Span,
}

impl<'src> ResolveError<'src> {
    fn err_token(&self) -> &'src str {
        &self.source.code[self.err_span.start..=self.err_span.end]
    }

    fn fmt_err(&self, msg: &str) -> String {
        format_error(self.source, self.expr_span, self.err_span, msg)
    }
}

impl<'src> fmt::Display for ResolveError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let binoperr_to_string = |err: &BinaryOperationError| {
            format!(
                "{} - not allowed",
                self.fmt_err(&format!(
                    "binary operation '{}' cannot be applied to '{}' and '{}'",
                    self.err_token(),
                    err.left_type,
                    err.right_type
                ))
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
            ResolveErrorType::NotDefined(DefinitionError { name }) => {
                self.fmt_err(&format!("'{}' not in scope", name))
            }
            ResolveErrorType::IllegalOperation(ref err) => binoperr_to_string(err),
            ResolveErrorType::IllegalType(IllegalTypeError {
                expected_type,
                actual_type,
                name,
            }) => self.fmt_err(&format!(
                "{} must be of type '{}', but the supplied type was '{}'",
                name, expected_type, actual_type
            )),
            ResolveErrorType::SelfImport(_) => self.fmt_err("cannot import self"),
            ResolveErrorType::NoSuchField(StructFieldError {
                struct_name,
                field_name,
            }) => self.fmt_err(&format!(
                "'{}' has no field named '{}'",
                struct_name, field_name
            )),
            ResolveErrorType::Inference(_) => self.fmt_err("type cannot be inferred"),
            ResolveErrorType::Deref(NonPtrError(ty)) => {
                self.fmt_err(&format!("{} cannot be dereferenced", ty))
            }
            ResolveErrorType::Delete(NonPtrError(ty)) => {
                self.fmt_err(&format!("{} cannot be deleted. Only Pointers can", ty))
            }
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ResolveErrorType<'src> {
    IllegalAssignment(AssignmentError<'src>),
    NotDefined(DefinitionError<'src>),
    IllegalOperation(BinaryOperationError<'src>),
    IllegalType(IllegalTypeError<'src>),
    NoSuchField(StructFieldError<'src>),
    SelfImport(SelfImportError),
    Inference(TypeInferenceError),
    Deref(NonPtrError<'src>),
    Delete(NonPtrError<'src>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeInferenceError;

#[derive(Debug, Eq, PartialEq)]
pub struct SelfImportError;

#[derive(Debug, Eq, PartialEq)]
pub struct NonPtrError<'src>(pub Type<'src>);

#[derive(Debug, Eq, PartialEq)]
pub struct IllegalTypeError<'src> {
    pub expected_type: Type<'src>,
    pub actual_type: Type<'src>,
    // e.g. "If condition" or "while condition"
    pub name: &'static str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AssignmentError<'src> {
    pub name: &'src str,
    pub definition_span: Span,
    pub bin_op_err: BinaryOperationError<'src>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BinaryOperationError<'src> {
    pub left_type: Type<'src>,
    pub right_type: Type<'src>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinitionError<'src> {
    pub name: &'src str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StructFieldError<'src> {
    pub struct_name: &'src str,
    pub field_name: &'src str,
}
