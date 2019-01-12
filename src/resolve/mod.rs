use std::collections::HashMap;

use crate::{
    parse::{ast::*, Span, Spanned},
    types::Type,
    Source,
};

use self::error::*;
use self::symbol::*;

use super::*;

mod error;
#[allow(dead_code)]
mod symbol;

pub(crate) struct Resolver<'input> {
    source: &'input Source,
    sym_table: SymbolTable<'input>,
    functions: HashMap<String, Type>,
}

impl<'input> Resolver<'input> {
    pub fn new(source: &'input Source) -> Self {
        Resolver {
            source,
            sym_table: SymbolTable::new(),
            functions: HashMap::new(),
        }
    }
}

impl<'input> Resolver<'input> {
    pub fn resolve(&mut self, prg: &Program<'input>) -> Vec<ResolveError<'input>> {
        let mut errors = vec![];

        for stmt in &prg.0 {
            self.resolve_top_lvl(&stmt, &mut errors);
        }

        for stmt in &prg.0 {
            self.resolve_stmt(&stmt, &mut errors);
        }

        errors
    }

    fn resolve_top_lvl(&mut self, stmt: &Stmt<'input>, _errors: &mut Vec<ResolveError<'input>>) {
        match stmt {
            Stmt::FnDecl { name, .. } => {
                self.functions.insert(name.node.to_owned(), Type::Void);
            }
            Stmt::Import { .. } => {
                // TODO: implement import resolving
            }
            _ => panic!("Invalid top level declaration"),
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt<'input>, errors: &mut Vec<ResolveError<'input>>) {
        match stmt {
            Stmt::VarDecl {
                name: Spanned { node: name, span },
                ref value,
                eq,
                ty: var_type,
            } => {
                match self.resolve_expr(&Spanned::from_span(value.span, &value.node)) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => {
                        if let Some(var_type) = var_type {
                            let expr_span = Span::new(span.start, value.span.end);
                            // If a type was provided, check if it's one of the builtin types
                            if let Err(err) = self
                                .compare_types(eq.span, expr_span, var_type.node, ty)
                                .map(|_| self.sym_table.bind(name, *span, ty, false))
                                .map_err(
                                    |ResolveError {
                                         error,
                                         err_span,
                                         expr_span,
                                         ..
                                     }| {
                                        self.illegal_op_to_illegal_assignment(
                                            err_span, expr_span, name, error, *span,
                                        )
                                    },
                                )
                            {
                                errors.push(err);
                            }
                        } else {
                            self.sym_table.bind(name, *span, ty, false)
                        }
                    }
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
            // TODO: else_branch
            Stmt::If {
                condition:
                    Spanned {
                        span,
                        node: condition,
                    },
                then_block,
                ..
            } => {
                match self.resolve_expr(&Spanned::from_span(*span, &condition)) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => {
                        if ty != Type::Bool {
                            errors.push(self.type_error(
                                *span,
                                *span,
                                "if condition",
                                Type::Bool,
                                ty,
                            ))
                        }
                    }
                }

                for stmt in &then_block.0 {
                    self.resolve_stmt(&stmt, errors);
                }
            }
            Stmt::Expr(ref expr) => {
                if let Err(msg) = self.resolve_expr(&Spanned::from_span(expr.span, &expr.node)) {
                    errors.push(msg);
                }
            }
            Stmt::Import { .. } => { /*Imports are handled in resolve_top_lvl()*/ }
        };
    }

    // TODO: change to &Spanned<Expr<'input>>
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
                let right_span = r.span;
                let right_expr = &r.node;
                let right = self.resolve_expr(&Spanned::from_span(right_span, right_expr))?;
                self.compare_types(op.span, expr.span, left, right)
            }
            Expr::BoolBinary(l, op, r) => {
                let left = self.resolve_expr(&Spanned::from_span(span, l))?;
                let right_span = r.span;
                let right_expr = &r.node;
                let right = self.resolve_expr(&Spanned::from_span(right_span, right_expr))?;
                self.compare_types(op.span, expr.span, left, right)?;
                Ok(Type::Bool)
            }
            Expr::Access { left, .. } => {
                // TODO: check if ident is package name -> import
                self.resolve_expr(&Spanned::from_span(left.span, &left.node))?;
                // TODO: check type of field/import
                Ok(Type::Void)
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
                        .ok_or_else(|| self.not_defined_error(span, span, name))?;
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
                self.compare_types(eq.span, span, ty, val_type).map_err(
                    |ResolveError {
                         error,
                         err_span,
                         expr_span,
                         ..
                     }| {
                        self.illegal_op_to_illegal_assignment(
                            err_span, expr_span, name, error, sym_span,
                        )
                    },
                )
            }
            Expr::Ident(name) => self
                .sym_table
                .lookup(name)
                .ok_or_else(|| self.not_defined_error(span, span, name))
                .map(|sym| sym.node.ty),

            Expr::Call { callee, args } => {
                for Spanned { node: expr, span } in &args.0 {
                    self.resolve_expr(&Spanned::from_span(*span, &expr))?;
                }

                let callee_name = self.source.slice(callee.span);

                let func_type = self
                    .functions
                    .get(callee_name)
                    .ok_or_else(|| self.not_defined_error(callee.span, span, callee_name))?;

                Ok(*func_type)
            }
        }
    }
}

impl<'input> Resolver<'input> {
    fn error(
        &self,
        err_span: Span,
        expr_span: Span,
        err: ResolveErrorType<'input>,
    ) -> ResolveError<'input> {
        ResolveError {
            source: self.source,
            error: err,
            err_span,
            expr_span,
        }
    }

    fn illegal_op_to_illegal_assignment(
        &self,
        err_span: Span,
        expr_span: Span,
        name: &'input str,
        error: ResolveErrorType<'input>,
        def_span: Span,
    ) -> ResolveError<'input> {
        if let ResolveErrorType::IllegalOperation(err) = error {
            self.error(
                err_span,
                expr_span,
                ResolveErrorType::IllegalAssignment(AssignmentError {
                    name,
                    definition_span: def_span,
                    bin_op_err: err,
                }),
            )
        } else {
            panic!("Invalid Error Type");
        }
    }

    fn type_error(
        &self,
        err_span: Span,
        expr_span: Span,
        name: &'static str,
        expected_type: Type,
        actual_type: Type,
    ) -> ResolveError<'input> {
        self.error(
            err_span,
            expr_span,
            ResolveErrorType::IllegalType(IllegalTypeError {
                expected_type,
                actual_type,
                name,
            }),
        )
    }

    fn not_defined_error(
        &self,
        err_span: Span,
        expr_span: Span,
        name: &'input str,
    ) -> ResolveError<'input> {
        self.error(
            err_span,
            expr_span,
            ResolveErrorType::NotDefined(DefinitionError { name }),
        )
    }

    fn compare_types(
        &self,
        err_span: Span,
        expr_span: Span,
        first: Type,
        second: Type,
    ) -> Result<Type, ResolveError<'input>> {
        if first != second {
            Err(self.error(
                err_span,
                expr_span,
                ResolveErrorType::IllegalOperation(BinaryOperationError {
                    left_type: first,
                    right_type: second,
                }),
            ))
        } else {
            Ok(first)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::token::Token;

    #[test]
    fn test_functions_are_found_without_forward_decl() {
        let source = Source::new("test", "fn main() { test(); } fn test() {}");

        let ast = Program(vec![
            Stmt::FnDecl {
                name: Spanned::new(3, 6, "main"),
                params: ParamList(vec![]),
                body: Block(vec![Stmt::Expr(Spanned::new(
                    12,
                    17,
                    Expr::Call {
                        callee: Box::new(Spanned::new(12, 15, Expr::Ident("test"))),
                        args: ArgList(vec![]),
                    },
                ))]),
            },
            Stmt::FnDecl {
                name: Spanned::new(25, 28, "test"),
                params: ParamList(vec![]),
                body: Block(vec![]),
            },
        ]);

        let mut resolver = Resolver::new(&source);
        let errors = resolver.resolve(&ast);

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);
    }

    #[test]
    fn test_resolve_without_errors_should_return_empty_vec() {
        let source = Source::new("test", "fn main() { let x = 10; }");

        let ast = Program(vec![Stmt::FnDecl {
            name: Spanned::new(3, 6, "main"),
            params: ParamList(vec![]),
            body: Block(vec![Stmt::VarDecl {
                name: Spanned::new(16, 16, "x"),
                value: Spanned::new(20, 22, Expr::DecLit(10)),
                eq: Spanned::new(18, 18, Token::Equals),
                ty: None,
            }]),
        }]);

        let mut resolver = Resolver::new(&source);
        let errors = resolver.resolve(&ast);

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
                    eq: Spanned::new(18, 18, Token::Equals),
                    ty: None,
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
        let errors = resolver.resolve(&ast);

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
