use self::symbol::SymbolTable;
use super::format_error;
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
    pub fn resolve(&mut self, prg: Program<'input>) {
        for stmt in prg.0 {
            if let Err(msg) = self.resolve_stmt(&stmt) {
                println!("{}", msg);
            }
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt<'input>) -> Result<(), String> {
        match stmt {
            Stmt::VarDecl { name, ref value } => {
                let expr = self.resolve_expr(&Spanned::from_span(value.span, &value.node))?;
                self.sym_table.bind(name, expr, false);
            }
            Stmt::FnDecl { params, body, .. } => {
                self.sym_table.scope_enter();

                for stmt in &body.0 {
                    self.resolve_stmt(&stmt)?;
                }

                for p in &params.0 {
                    self.sym_table.bind(p.0, p.1, true);
                }

                self.sym_table.scope_exit();
            }
            Stmt::Expr(ref expr) => {
                self.resolve_expr(&Spanned::from_span(expr.span, &expr.node))?;
            }
        };

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Spanned<&Expr<'input>>) -> Result<Type, String> {
        let span = expr.span;

        match &expr.node {
            Expr::Error(ref err) => Err(err.to_string()),
            Expr::DecLit(_) => Ok(Type::I32),
            Expr::StringLit(_) => Ok(Type::String),
            Expr::Negate(expr) => self.resolve_expr(&Spanned::from_span(span, &*expr)),
            Expr::Binary(l, op, r) => {
                let left = self.resolve_expr(&Spanned::from_span(span, &*l))?;
                let right = self.resolve_expr(&Spanned::from_span(span, &*r))?;
                self.compare_types(op, expr.span, left, right)
            }
            Expr::Ident(ref name) => self
                .sym_table
                .lookup(name)
                .ok_or_else(|| {
                    format_error(
                        self.source,
                        span,
                        &format!("'{}' not defined in scope", name),
                    )
                })
                .map(|sym| sym.ty),
        }
    }
}

impl<'input> Resolver<'input> {
    fn compare_types(
        &self,
        op: &Token<'input>,
        span: Span,
        first: Type,
        second: Type,
    ) -> Result<Type, String> {
        if first != second {
            Err(format_error(
                self.source,
                span,
                &format!(
                    "Binary operation '{}' cannot be applied to '{}' and '{}'",
                    op, first, second,
                ),
            ))
        } else {
            Ok(first)
        }
    }
}
