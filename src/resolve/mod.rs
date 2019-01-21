use std::collections::{HashMap, HashSet};

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
pub mod symbol;

pub type PrgMap<'input> = HashMap<&'input str, (&'input Source, &'input Program<'input>)>;
pub type TypeMap<'input, 'ast> = HashMap<(Span, &'ast Expr<'input>), Type>;

pub(crate) struct Resolver<'input, 'ast> {
    current_name: &'input str,
    programs: PrgMap<'input>,
    resolved: HashSet<&'input str>,
    // use tuple, because Spanned::hash only considers node
    pub expr_types: TypeMap<'input, 'ast>,
    sym_table: SymbolTable<'input>,
    functions: HashMap<String, Type>,
}

impl<'input, 'ast> Resolver<'input, 'ast> {
    pub fn new(main_file: &'input str, programs: PrgMap<'input>) -> Self {
        Resolver {
            current_name: main_file,
            programs,
            expr_types: HashMap::new(),
            resolved: HashSet::new(),
            sym_table: SymbolTable::new(),
            functions: HashMap::new(),
        }
    }
}

impl<'input, 'ast> Resolver<'input, 'ast> {
    pub fn resolve(&mut self) -> Vec<ResolveError<'input>> {
        let (_, prg) = self.programs[self.current_name];
        self.resolve_prg(prg, None)
    }

    fn resolve_prg(
        &mut self,
        prg: &'ast Program<'input>,
        prefix: Option<&str>,
    ) -> Vec<ResolveError<'input>> {
        let mut errors = vec![];

        for top_lvl in &prg.0 {
            self.declare_top_lvl(&top_lvl, &mut errors, prefix);
        }

        for top_lvl in &prg.0 {
            self.resolve_top_lvl(&top_lvl, &mut errors);
        }

        errors
    }

    fn declare_top_lvl(
        &mut self,
        top_lvl: &TopLvl<'input>,
        errors: &mut Vec<ResolveError<'input>>,
        prefix: Option<&str>,
    ) {
        match top_lvl {
            TopLvl::FnDecl { name, .. } => {
                let name = prefix
                    .map(|prefix| format!("{}.{}", prefix, name.node))
                    .unwrap_or_else(|| name.node.to_owned());
                self.functions.insert(name, Type::Void);
            }
            TopLvl::Import { name, .. } => {
                if name.node == self.current_name {
                    errors.push(ResolveError {
                        source: self.current_source(),
                        error: ResolveErrorType::SelfImport(SelfImportError),
                        err_span: name.span,
                        expr_span: name.span,
                    });
                } else if !self.resolved.contains(name.node) {
                    if let Some((_, prg)) = self.programs.get(name.node) {
                        let current_name = self.current_name;
                        self.current_name = name.node;
                        self.resolved.insert(name.node);
                        self.resolve_prg(prg, Some(name.node));
                        self.current_name = current_name;
                    }
                } else {
                    errors.push(self.not_defined_error(name.span, name.span, name.node));
                }
            }
            _ => panic!("Invalid top level declaration"),
        }
    }

    fn resolve_top_lvl(
        &mut self,
        top_lvl: &'ast TopLvl<'input>,
        errors: &mut Vec<ResolveError<'input>>,
    ) {
        if let TopLvl::FnDecl { params, body, .. } = top_lvl {
            self.sym_table.scope_enter();

            for p in &params.0 {
                self.sym_table.bind(p.0.node, p.0.span, p.1, true);
            }

            for stmt in &body.0 {
                self.resolve_stmt(&stmt, errors);
            }

            self.sym_table.scope_exit();
        }
    }

    fn resolve_stmt(&mut self, stmt: &'ast Stmt<'input>, errors: &mut Vec<ResolveError<'input>>) {
        match stmt {
            Stmt::VarDecl {
                name,
                ref value,
                eq,
                ty: var_type,
            } => {
                match self.resolve_expr(value.span, &value.node) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => {
                        if let Some(var_type) = var_type {
                            let expr_span = Span::new(name.span.start, value.span.end);
                            // If a type was provided, check if it's one of the builtin types
                            if let Err(err) = self
                                .compare_types(eq.span, expr_span, var_type.node, ty)
                                .map(|_| self.sym_table.bind(name.node, name.span, ty, false))
                                .map_err(
                                    |ResolveError {
                                         error,
                                         err_span,
                                         expr_span,
                                         ..
                                     }| {
                                        self.illegal_op_to_illegal_assignment(
                                            err_span, expr_span, name.node, error, name.span,
                                        )
                                    },
                                )
                            {
                                errors.push(err);
                            }
                        } else {
                            self.sym_table.bind(name.node, name.span, ty, false)
                        }
                    }
                };
            }
            Stmt::If {
                condition,
                then_block,
                ..
            } => {
                match self.resolve_expr(condition.span, &condition.node) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => {
                        if ty != Type::Bool {
                            errors.push(self.type_error(
                                condition.span,
                                condition.span,
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
                if let Err(msg) = self.resolve_expr(expr.span, &expr.node) {
                    errors.push(msg);
                }
            }
        };
    }

    fn resolve_expr(
        &mut self,
        span: Span,
        expr: &'ast Expr<'input>,
    ) -> Result<Type, ResolveError<'input>> {
        let ty = match &expr {
            Expr::Error(_) => {
                unreachable!("If errors occur during parsing, the program should not be resolved")
            }
            Expr::DecLit(_) => Ok(Type::I32),
            Expr::StringLit(_) => Ok(Type::String),
            Expr::Negate(op, expr) => {
                let ty = self.resolve_expr(expr.span, &expr.node)?;
                // TODO: unary operation error
                self.compare_types(op.span, span, ty, Type::I32)
            }
            Expr::Binary(l, op, r) => {
                let left = self.resolve_expr(l.span, &l.node)?;
                let right = self.resolve_expr(r.span, &r.node)?;
                self.compare_types(op.span, span, left, right)
            }
            Expr::BoolBinary(l, op, r) => {
                let left = self.resolve_expr(l.span, &l.node)?;
                let right = self.resolve_expr(r.span, &r.node)?;
                self.compare_types(op.span, span, left, right)?;
                Ok(Type::Bool)
            }
            Expr::Access { left, .. } => {
                self.resolve_expr(left.span, &left.node)?;
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
                let val_type = self.resolve_expr(value_span, node)?;
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
                    self.resolve_expr(*span, &expr)?;
                }

                let callee_name = self.current_source().slice(callee.span);

                let func_type = self
                    .functions
                    .get(callee_name)
                    .ok_or_else(|| self.not_defined_error(callee.span, span, callee_name))?;

                Ok(*func_type)
            }
        };

        if let Ok(ty) = ty {
            self.expr_types.insert((span, expr), ty);
        }

        ty
    }
}

impl<'input, 'ast> Resolver<'input, 'ast> {
    fn current_source(&self) -> &'input Source {
        let (src, _) = self.programs[self.current_name];
        src
    }

    fn error(
        &self,
        err_span: Span,
        expr_span: Span,
        err: ResolveErrorType<'input>,
    ) -> ResolveError<'input> {
        ResolveError {
            source: self.current_source(),
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
    fn test_resolve_should_find_imported_fn() {
        let main_src = Source::new("main", "import test\nfn main() {test.func()}");
        let test_src = Source::new("test", "fn func() {}");

        let main_ast = Program(vec![
            TopLvl::Import {
                name: Spanned::new(7, 10, "test"),
            },
            TopLvl::FnDecl {
                name: Spanned::new(16, 19, "main"),
                params: ParamList(vec![]),
                body: Block(vec![Stmt::Expr(Spanned::new(
                    25,
                    28,
                    Expr::Call {
                        callee: Box::new(Spanned::new(
                            23,
                            31,
                            Expr::Access {
                                left: Box::new(Spanned::new(23, 26, Expr::Ident("test"))),
                                identifier: Spanned::new(28, 31, "func"),
                            },
                        )),
                        args: ArgList(vec![]),
                    },
                ))]),
            },
        ]);

        let test_ast = Program(vec![TopLvl::FnDecl {
            name: Spanned::new(3, 6, "func"),
            params: ParamList(vec![]),
            body: Block(vec![]),
        }]);

        let mut map = HashMap::new();
        map.insert("main", (&main_src, &main_ast));
        map.insert("test", (&test_src, &test_ast));

        let mut resolver = Resolver::new("main", map);
        let errors = resolver.resolve();

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);
    }

    #[test]
    fn test_functions_are_found_without_forward_decl() {
        let source = Source::new("test", "fn main() { test(); } fn test() {}");

        let ast = Program(vec![
            TopLvl::FnDecl {
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
            TopLvl::FnDecl {
                name: Spanned::new(25, 28, "test"),
                params: ParamList(vec![]),
                body: Block(vec![]),
            },
        ]);

        let mut map = HashMap::new();
        map.insert("test", (&source, &ast));

        let mut resolver = Resolver::new("test", map);
        let errors = resolver.resolve();

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);
    }

    #[test]
    fn test_resolve_without_errors_should_return_empty_vec() {
        let source = Source::new("test", "fn main() { let x = 10; }");

        let ast = Program(vec![TopLvl::FnDecl {
            name: Spanned::new(3, 6, "main"),
            params: ParamList(vec![]),
            body: Block(vec![Stmt::VarDecl {
                name: Spanned::new(16, 16, "x"),
                value: Spanned::new(20, 22, Expr::DecLit("10")),
                eq: Spanned::new(18, 18, Token::Equals),
                ty: None,
            }]),
        }]);

        let mut map = HashMap::new();
        map.insert("test", (&source, &ast));

        let mut resolver = Resolver::new("test", map);
        let errors = resolver.resolve();

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);
    }

    #[test]
    fn test_resolve_with_assign_error_should_return_correct_error() {
        let source = Source::new("test", r#"fn main() { let x = 10; x = ""; }"#);

        let ast = Program(vec![TopLvl::FnDecl {
            name: Spanned::new(3, 6, "main"),
            params: ParamList(vec![]),
            body: Block(vec![
                Stmt::VarDecl {
                    name: Spanned::new(16, 16, "x"),
                    value: Spanned::new(20, 22, Expr::DecLit("10")),
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

        let mut map = HashMap::new();
        map.insert("test", (&source, &ast));

        let mut resolver = Resolver::new("test", map);
        let errors = resolver.resolve();

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
