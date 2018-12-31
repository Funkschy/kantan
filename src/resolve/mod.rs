use self::symbol::SymbolTable;
use crate::parse::ast::*;
use crate::types::Type;

#[allow(dead_code)]
mod symbol;

pub(crate) struct Resolver<'input> {
    sym_table: SymbolTable<'input>,
}

impl<'input> Resolver<'input> {
    pub fn new() -> Self {
        Resolver {
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
                let expr = self.resolve_expr(value)?;
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
            Stmt::Expr(expr) => {
                self.resolve_expr(expr)?;
            }
        };

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr<'input>) -> Result<Type, String> {
        match expr {
            Expr::Error(ref err) => Err(err.to_string()),
            Expr::DecLit(_) => Ok(Type::I32),
            Expr::StringLit(_) => Ok(Type::String),
            Expr::Negate(expr) => self.resolve_expr(expr),
            Expr::Binary(l, _, r) => {
                let left = self.resolve_expr(l)?;
                let right = self.resolve_expr(r)?;
                Self::compare_types(&expr, left, right)
            }
        }
    }

    fn compare_types(expr: &Expr, first: Type, second: Type) -> Result<Type, String> {
        if first != second {
            Err(format!(
                "Error in expression: '{}', Type: '{}' not compatible with Type: '{}'",
                expr, first, second
            ))
        } else {
            Ok(first)
        }
    }
}
