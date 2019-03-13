use std::collections::HashMap;

use crate::types::*;
use crate::{Span, Spanned};

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolKind {
    Global,
    Local,
    Param,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Symbol<'input> {
    pub ty: Type<'input>,
    pub kind: SymbolKind,
    pub name: &'input str,
}

impl<'input> Symbol<'input> {
    fn new(name: &'input str, ty: Type<'input>, kind: SymbolKind) -> Self {
        Symbol { ty, kind, name }
    }
}

#[derive(Debug)]
pub struct SymbolTable<'input> {
    scopes: Vec<HashMap<&'input str, Spanned<Symbol<'input>>>>,
}

impl<'input> SymbolTable<'input> {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }
}

impl<'input> SymbolTable<'input> {
    pub fn scope_enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn scope_exit(&mut self) {
        if self.in_global_scope() {
            panic!("Cannot exit out of global scope");
        }
        self.scopes.pop();
    }

    pub fn bind(&mut self, name: &'input str, span: Span, ty: Type<'input>, is_param: bool) {
        let global = self.in_global_scope();
        let scope = self.scopes.last_mut().unwrap();

        let kind = if global {
            SymbolKind::Global
        } else if is_param {
            SymbolKind::Param
        } else {
            SymbolKind::Local
        };

        let symbol = Spanned::from_span(span, Symbol::new(name, ty, kind));
        scope.insert(name, symbol);
    }
}

impl<'input> SymbolTable<'input> {
    fn in_global_scope(&self) -> bool {
        self.scopes.len() <= 1
    }

    pub fn lookup(&self, name: &'input str) -> Option<&Spanned<Symbol<'input>>> {
        for scope in self.scopes.iter().rev() {
            let symbol = scope.get(name);
            if symbol.is_some() {
                return symbol;
            }
        }

        None
    }

    pub fn lookup_current(&self, name: &'input str) -> Option<&Spanned<Symbol>> {
        let last_scope = self.scopes.last().unwrap();
        last_scope.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_should_return_symbol_from_outer_scope_if_not_found() {
        let mut sym_table = SymbolTable::new();
        assert_eq!(1, sym_table.scopes.len());

        sym_table.bind("x", Span::new(1, 1), Type::Simple(Simple::I32), false);
        sym_table.scope_enter();

        assert_eq!(
            Some(&Spanned::new(
                1,
                1,
                Symbol::new("x", Type::Simple(Simple::I32), SymbolKind::Global),
            )),
            sym_table.lookup("x")
        );
        assert_eq!(None, sym_table.lookup_current("x"));

        sym_table.scope_exit();
        assert_eq!(
            Some(&Spanned::new(
                1,
                1,
                Symbol::new("x", Type::Simple(Simple::I32), SymbolKind::Global),
            )),
            sym_table.lookup("x")
        );
        assert_eq!(
            Some(&Spanned::new(
                1,
                1,
                Symbol::new("x", Type::Simple(Simple::I32), SymbolKind::Global),
            )),
            sym_table.lookup_current("x")
        );
    }

    #[test]
    #[should_panic(expected = "Cannot exit out of global scope")]
    fn test_scope_exit_should_panic_when_trying_to_exit_global_scope() {
        let mut sym_table = SymbolTable::new();
        sym_table.scope_exit();
    }
}
