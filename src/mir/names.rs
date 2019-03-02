use std::collections::HashMap;

/// Binds the original name of a variable to a new one, to allow scopes.
/// {
///     let x = 5;
///     {
///         let x = "test";
///     }
/// }
///
/// In the outer scope, nametable.lookup("x") will return x.
/// In the inner scope, is will return "x1"
#[derive(Debug)]
pub struct NameTable<'input> {
    scopes: Vec<HashMap<&'input str, String>>,
}

impl<'input> NameTable<'input> {
    pub fn new() -> Self {
        NameTable {
            scopes: vec![HashMap::new()],
        }
    }
}

impl<'input> NameTable<'input> {
    pub fn scope_enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn scope_exit(&mut self) {
        if self.in_global_scope() {
            panic!("Cannot exit out of global scope");
        }
        self.scopes.pop();
    }

    pub fn bind(&mut self, name: &'input str) {
        // count all occurences of name in all active scopes
        let num = self
            .scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.get(name))
            .count();

        let scope = self.scopes.last_mut().unwrap();
        scope.insert(name, format!("{}{}", name, num));
    }
}

impl<'input> NameTable<'input> {
    fn in_global_scope(&self) -> bool {
        self.scopes.len() <= 1
    }

    pub fn lookup(&self, name: &'input str) -> &String {
        for scope in self.scopes.iter().rev() {
            let n = scope.get(name);
            if let Some(n) = n {
                return n;
            }
        }

        panic!("Name {} could not be resolved", name);
    }
}
