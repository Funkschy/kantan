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
pub struct Symbol<'src> {
    pub ty: Type<'src>,
    pub kind: SymbolKind,
    pub name: &'src str,
}

impl<'src> Symbol<'src> {
    fn new(name: &'src str, ty: Type<'src>, kind: SymbolKind) -> Self {
        Symbol { ty, kind, name }
    }
}

#[derive(Debug)]
pub struct SymbolTable<'src> {
    scopes: Vec<HashMap<&'src str, Spanned<Symbol<'src>>>>,
}

impl<'src> SymbolTable<'src> {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }
}

impl<'src> SymbolTable<'src> {
    pub fn scope_enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn scope_exit(&mut self) {
        if self.in_global_scope() {
            panic!("Cannot exit out of global scope");
        }
        self.scopes.pop();
    }

    pub fn bind(&mut self, name: &'src str, span: Span, ty: Type<'src>, is_param: bool) {
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

impl<'src> SymbolTable<'src> {
    fn in_global_scope(&self) -> bool {
        self.scopes.len() <= 1
    }

    pub fn lookup(&self, name: &'src str) -> Option<(&Spanned<Symbol<'src>>, usize)> {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            let symbol = scope.get(name);
            if let Some(symbol) = symbol {
                return Some((symbol, i));
            }
        }

        None
    }

    #[inline]
    pub fn num_scopes(&self) -> usize {
        self.scopes.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<'src> SymbolTable<'src> {
        pub fn lookup_current(&self, name: &'src str) -> Option<&Spanned<Symbol>> {
            let last_scope = self.scopes.last().unwrap();
            last_scope.get(name)
        }
    }

    #[test]
    fn test_lookup_should_return_symbol_from_outer_scope_if_not_found() {
        let mut sym_table = SymbolTable::new();
        assert_eq!(1, sym_table.scopes.len());

        sym_table.bind("x", Span::new(1, 1), Type::Simple(Simple::I32), false);
        sym_table.scope_enter();

        assert_eq!(
            Some((
                &Spanned::new(
                    1,
                    1,
                    Symbol::new("x", Type::Simple(Simple::I32), SymbolKind::Global),
                ),
                0
            )),
            sym_table.lookup("x")
        );
        assert_eq!(None, sym_table.lookup_current("x"));

        sym_table.bind("x", Span::new(1, 1), Type::Simple(Simple::I32), false);

        assert_eq!(
            Some((
                &Spanned::new(
                    1,
                    1,
                    Symbol::new("x", Type::Simple(Simple::I32), SymbolKind::Local),
                ),
                1
            )),
            sym_table.lookup("x")
        );

        sym_table.scope_exit();
        assert_eq!(
            Some((
                &Spanned::new(
                    1,
                    1,
                    Symbol::new("x", Type::Simple(Simple::I32), SymbolKind::Global),
                ),
                0
            )),
            sym_table.lookup("x")
        );
        assert_eq!(
            Some(&Spanned::new(
                1,
                1,
                Symbol::new("x", Type::Simple(Simple::I32), SymbolKind::Global),
            ),),
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
