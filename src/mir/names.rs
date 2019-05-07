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
/// In the inner scope, is will return "x_1"
#[derive(Debug)]
pub struct NameTable<'src> {
    scopes: Vec<HashMap<&'src str, String>>,
    total: HashMap<&'src str, usize>,
}

impl<'src> NameTable<'src> {
    pub fn new() -> Self {
        NameTable {
            scopes: vec![HashMap::new()],
            total: HashMap::new(),
        }
    }
}

impl<'src> NameTable<'src> {
    pub fn scope_enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn scope_exit(&mut self) {
        if self.in_global_scope() {
            panic!("Cannot exit out of global scope");
        }
        self.scopes.pop();
    }

    pub fn bind(&mut self, name: &'src str) {
        let num = *self.total.entry(name).or_insert(0);

        let scope = self.scopes.last_mut().unwrap();
        if num != 0 {
            scope.insert(name, format!("{}_{}", name, num));
        } else {
            scope.insert(name, name.to_string());
        }
        self.total.insert(name, num + 1);
    }
}

impl<'src> NameTable<'src> {
    fn in_global_scope(&self) -> bool {
        self.num_scopes() <= 1
    }

    pub fn lookup(&self, name: &'src str) -> &String {
        for scope in self.scopes.iter().rev() {
            let n = scope.get(name);
            if let Some(n) = n {
                return n;
            }
        }

        panic!("Name {} could not be resolved", name);
    }

    #[inline(always)]
    pub fn num_scopes(&self) -> usize {
        self.scopes.len()
    }
}
