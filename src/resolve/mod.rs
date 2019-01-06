use self::symbol::*;
use super::*;
use crate::{
    parse::{ast::*, token::Token, Span, Spanned},
    types::Type,
    Source,
};

mod error;
#[allow(dead_code)]
mod symbol;

use self::error::*;

pub(crate) struct Resolver<'input> {
    source: &'input Source<'input>,
    sym_table: SymbolTable<'input>,
}

impl<'input> Resolver<'input> {
    pub fn new(source: &'input Source<'input>) -> Self {
        Resolver {
            source,
            sym_table: SymbolTable::new(),
        }
    }
}

impl<'input> Resolver<'input> {
    pub fn resolve(&mut self, prg: Program<'input>) -> Vec<ResolveError<'input>> {
        let mut errors = vec![];
        for stmt in prg.0 {
            self.resolve_stmt(&stmt, &mut errors);
        }
        errors
    }

    fn resolve_stmt(&mut self, stmt: &Stmt<'input>, errors: &mut Vec<ResolveError<'input>>) {
        match stmt {
            Stmt::VarDecl {
                name: Spanned { node: name, span },
                ref value,
            } => {
                match self.resolve_expr(&Spanned::from_span(value.span, &value.node)) {
                    Err(msg) => errors.push(msg),
                    Ok(expr) => self.sym_table.bind(name, *span, expr, false),
                };
            }
            Stmt::FnDecl { params, body, .. } => {
                self.sym_table.scope_enter();

                for p in &params.0 {
                    self.sym_table.bind(p.0.node, p.0.span, p.1, true);
                }

                for stmt in &body.0 {
                    self.resolve_stmt(&stmt, errors);
                }

                self.sym_table.scope_exit();
            }
            Stmt::Expr(ref expr) => {
                if let Err(msg) = self.resolve_expr(&Spanned::from_span(expr.span, &expr.node)) {
                    errors.push(msg);
                }
            }
        };
    }

    fn resolve_expr(
        &mut self,
        expr: &Spanned<&Expr<'input>>,
    ) -> Result<Type, ResolveError<'input>> {
        let span = expr.span;

        match &expr.node {
            Expr::Error(_) => {
                unreachable!("If errors occur during parsing, the program should not be resolved")
            }
            Expr::DecLit(_) => Ok(Type::I32),
            Expr::StringLit(_) => Ok(Type::String),
            Expr::Negate(expr) => self.resolve_expr(&Spanned::from_span(span, expr)),
            Expr::Binary(l, op, r) => {
                let left = self.resolve_expr(&Spanned::from_span(span, l))?;
                let right = self.resolve_expr(&Spanned::from_span(span, r))?;
                self.compare_types(op, expr.span, left, right)
            }
            Expr::BoolBinary(l, op, r) => {
                let left = self.resolve_expr(&Spanned::from_span(span, l))?;
                let right = self.resolve_expr(&Spanned::from_span(span, r))?;
                self.compare_types(op, expr.span, left, right)?;
                Ok(Type::Bool)
            }
            Expr::Assign { name, eq, value } => {
                // Lookup variable in defined scopes
                let (ty, sym_span) = {
                    let Spanned {
                        span,
                        node: Symbol { ty, .. },
                    } = self
                        .sym_table
                        .lookup(name)
                        .ok_or_else(|| self.not_defined_error(span, name))?;
                    (*ty, *span)
                };

                let &Spanned {
                    span: value_span,
                    ref node,
                } = &**value;

                // get type of right expression
                let val_type = self.resolve_expr(&Spanned::from_span(value_span, node))?;
                // check if type of right expression is the same as that of
                // the declared variable
                self.compare_types(eq, span, ty, val_type).map_err(
                    |ResolveError { error, .. }| {
                        if let ResolveErrorType::IllegalOperation(err) = error {
                            self.error(ResolveErrorType::IllegalAssignment(AssignmentError {
                                name,
                                definition_span: sym_span,
                                bin_op_err: err,
                            }))
                        } else {
                            panic!("Invalid Error Type");
                        }
                    },
                )
            }
            Expr::Ident(name) => self
                .sym_table
                .lookup(name)
                .ok_or_else(|| self.not_defined_error(span, name))
                .map(|sym| sym.node.ty),
        }
    }
}

impl<'input> Resolver<'input> {
    fn error(&self, err: ResolveErrorType<'input>) -> ResolveError<'input> {
        ResolveError {
            source: self.source,
            error: err,
        }
    }

    fn not_defined_error(&self, span: Span, name: &'input str) -> ResolveError<'input> {
        self.error(ResolveErrorType::NotDefined(DefinitionError::new(
            name, span,
        )))
    }

    fn compare_types(
        &self,
        op: &Spanned<Token<'input>>,
        span: Span,
        first: Type,
        second: Type,
    ) -> Result<Type, ResolveError<'input>> {
        if first != second {
            Err(
                self.error(ResolveErrorType::IllegalOperation(BinaryOperationError {
                    token: *op,
                    expr_span: span,
                    left_type: first,
                    right_type: second,
                })),
            )
        } else {
            Ok(first)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_without_errors_should_return_empty_vec() {
        let source = Source::new("test", "fn main() { let x = 10; }");

        let ast = Program(vec![Stmt::FnDecl {
            name: Spanned::new(3, 6, "main"),
            params: ParamList(vec![]),
            body: Block(vec![Stmt::VarDecl {
                name: Spanned::new(16, 16, "x"),
                value: Spanned::new(20, 22, Expr::DecLit(10)),
            }]),
        }]);

        let mut resolver = Resolver::new(&source);
        let errors = resolver.resolve(ast);

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);
    }

    #[test]
    fn test_resolve_with_assign_error_should_return_correct_error() {
        let source = Source::new("test", r#"fn main() { let x = 10; x = ""; }"#);

        let ast = Program(vec![Stmt::FnDecl {
            name: Spanned::new(3, 6, "main"),
            params: ParamList(vec![]),
            body: Block(vec![
                Stmt::VarDecl {
                    name: Spanned::new(16, 16, "x"),
                    value: Spanned::new(20, 22, Expr::DecLit(10)),
                },
                Stmt::Expr(Spanned::new(
                    24,
                    29,
                    Expr::Assign {
                        name: "x",
                        eq: Spanned::new(26, 26, Token::Equals),
                        value: Box::new(Spanned::new(29, 29, Expr::StringLit(""))),
                    },
                )),
            ]),
        }]);

        let mut resolver = Resolver::new(&source);
        let errors = resolver.resolve(ast);

        assert_eq!(1, errors.len());

        if let [err] = errors.as_slice() {
            let err = err.to_string();
            assert!(err.contains("operation '=' cannot be applied to 'i32' and 'string'"));
            assert!(err.contains("'x' was defined as 'i32' here"));
        } else {
            panic!("Expected one error");
        }
    }
}
