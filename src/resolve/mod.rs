use self::symbol::*;
use super::*;
use crate::{
    parse::{ast::*, token::Token, Span, Spanned},
    types::Type,
};

#[allow(dead_code)]
mod symbol;

pub(crate) struct Resolver<'input> {
    source: &'input str,
    sym_table: SymbolTable<'input>,
}

impl<'input> Resolver<'input> {
    pub fn new(source: &'input str) -> Self {
        Resolver {
            source,
            sym_table: SymbolTable::new(),
        }
    }
}

impl<'input> Resolver<'input> {
    pub fn resolve(&mut self, prg: Program<'input>) -> Vec<String> {
        let mut errors: Vec<String> = vec![];
        for stmt in prg.0 {
            self.resolve_stmt(&stmt, &mut errors);
        }
        errors
    }

    fn resolve_stmt(&mut self, stmt: &Stmt<'input>, errors: &mut Vec<String>) {
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

    fn resolve_expr(&mut self, expr: &Spanned<&Expr<'input>>) -> Result<Type, String> {
        let span = expr.span;

        match &expr.node {
            Expr::Error(ref err) => Err(err.to_string()),
            Expr::DecLit(_) => Ok(Type::I32),
            Expr::StringLit(_) => Ok(Type::String),
            Expr::Negate(expr) => self.resolve_expr(&Spanned::from_span(span, expr)),
            Expr::Binary(l, op, r) => {
                let left = self.resolve_expr(&Spanned::from_span(span, l))?;
                let right = self.resolve_expr(&Spanned::from_span(span, r))?;
                self.compare_types(op, expr.span, left, right)
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

                let &Spanned { span, ref node } = &**value;
                // get type of right expression
                let val_type = self.resolve_expr(&Spanned::from_span(span, node))?;
                // check if type of right expression is the same as that of
                // the declared variable
                self.compare_types(eq, span, ty, val_type).map_err(|err| {
                    format!(
                        "{}\n\nreason:\n{}",
                        err,
                        self.defined_with_other_type_error(sym_span, name, ty)
                    )
                })
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
    fn not_defined_error(&self, span: Span, name: &'input str) -> String {
        format_error(
            self.source,
            span,
            Span::new(span.start, span.start + name.len() - 1),
            &format!("'{}' not in scope", name),
        )
    }

    fn defined_with_other_type_error(&self, name_span: Span, name: &str, ty: Type) -> String {
        let (line_nr, _) = find_line_index(self.source, name_span.start);
        format!(
            "{} - {} was defined as {} here",
            err_to_string(self.source, name_span, name_span, line_nr, true),
            name,
            ty
        )
    }

    fn compare_types(
        &self,
        op: &Spanned<Token<'input>>,
        span: Span,
        first: Type,
        second: Type,
    ) -> Result<Type, String> {
        if first != second {
            Err(format!(
                "{} - not allowed",
                format_error(
                    self.source,
                    span,
                    op.span,
                    &format!(
                        "Binary operation '{}' cannot be applied to '{}' and '{}'",
                        op.node, first, second,
                    ),
                )
            ))
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
        let source = "fn main() { let x = 10; }";

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

        let expected: Vec<String> = vec![];
        assert_eq!(expected, errors);
    }
}
