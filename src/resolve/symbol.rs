use std::collections::HashMap;

use crate::types::Type;

#[derive(Debug, Eq, PartialEq)]
enum SymbolKind {
    Global,
    Local,
    Param,
}

#[derive(Debug, Eq, PartialEq)]
struct Symbol<'input> {
    ty: Type,
    kind: SymbolKind,
    name: &'input str,
}

impl<'input> Symbol<'input> {
    fn new(name: &'input str, ty: Type, kind: SymbolKind) -> Self {
        Symbol { ty, kind, name }
    }
}

struct SymbolTable<'input> {
    scopes: Vec<HashMap<&'input str, Symbol<'input>>>,
}

impl<'input> SymbolTable<'input> {
    fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }
}

impl<'input> SymbolTable<'input> {
    fn scope_enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn scope_exit(&mut self) {
        if self.scopes.len() <= 1 {
            panic!("Cannot exit out of global scope");
        }
        self.scopes.pop();
    }

    fn bind(&mut self, name: &'input str, ty: Type, is_param: bool) {
        let scope_len = self.scopes.len();
        let scope = self.scopes.last_mut().unwrap();

        let kind = if scope_len == 1 {
            SymbolKind::Global
        } else if is_param {
            SymbolKind::Param
        } else {
            SymbolKind::Local
        };

        let symbol = Symbol::new(name, ty, kind);
        scope.insert(name, symbol);
    }
}

impl<'input> SymbolTable<'input> {
    fn lookup(&self, name: &'input str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            let symbol = scope.get(name);
            if symbol.is_some() {
                return symbol;
            }
        }

        None
    }

    fn lookup_current(&self, name: &'input str) -> Option<&Symbol> {
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

        sym_table.bind("x", Type::I32, false);
        sym_table.scope_enter();

        assert_eq!(
            Some(&Symbol::new("x", Type::I32, SymbolKind::Global)),
            sym_table.lookup("x")
        );
        assert_eq!(None, sym_table.lookup_current("x"));

        sym_table.scope_exit();
        assert_eq!(
            Some(&Symbol::new("x", Type::I32, SymbolKind::Global)),
            sym_table.lookup("x")
        );
        assert_eq!(
            Some(&Symbol::new("x", Type::I32, SymbolKind::Global)),
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