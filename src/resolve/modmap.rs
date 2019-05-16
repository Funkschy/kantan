use std::collections::HashMap;

use crate::types::UserIdent;
use crate::{FuncDef, FunctionMap, UserTypeDefinition, UserTypeMap};

#[derive(Debug, Default)]
struct Module<'src> {
    user_types: UserTypeMap<'src>,
    functions: FunctionMap<'src>,
}

type ModuleName<'src> = &'src str;

#[derive(Debug, Default)]
pub struct ModMap<'src> {
    modules: HashMap<ModuleName<'src>, Module<'src>>,
}

impl<'src> ModMap<'src> {
    pub fn create(&mut self, module: ModuleName<'src>) {
        self.modules.insert(module, Module::default());
    }

    pub fn define_function(
        &mut self,
        module: ModuleName<'src>,
        name: &'src str,
        def: FuncDef<'src>,
    ) {
        self.modules
            .entry(module)
            .or_insert_with(Module::default)
            .functions
            .insert(name, def);
    }

    pub fn define_type(
        &mut self,
        module: ModuleName<'src>,
        name: &'src str,
        def: UserTypeDefinition<'src>,
    ) {
        self.modules
            .entry(module)
            .or_insert_with(Module::default)
            .user_types
            .insert(name, def);
    }
}

impl<'src> ModMap<'src> {
    fn and_then<'a, T, F>(&'a self, module: ModuleName, f: F) -> Option<T>
    where
        F: FnOnce(&'a Module<'src>) -> Option<T>,
    {
        self.modules.get(module).and_then(f)
    }

    pub fn function_defined(&self, module: ModuleName, name: &str) -> bool {
        self.and_then(module, |m| Some(m.functions.contains_key(name)))
            .unwrap_or(false)
    }

    pub fn type_defined(&self, module: ModuleName, ident: &UserIdent) -> bool {
        self.and_then(module, |m| Some(m.user_types.contains_key(ident.name())))
            .unwrap_or(false)
    }

    pub fn get_function(&self, module: ModuleName, name: &str) -> Option<&FuncDef<'src>> {
        self.and_then(module, |m| m.functions.get(name))
    }

    pub fn get_user_type(&self, ident: &UserIdent) -> Option<&UserTypeDefinition<'src>> {
        self.and_then(ident.module(), |m| m.user_types.get(ident.name()))
    }

    pub fn iter_types<'a>(
        &'a self,
    ) -> impl Iterator<Item = (ModuleName<'src>, &'a UserTypeDefinition<'src>)> {
        self.modules
            .iter()
            .flat_map(|(mod_name, m)| m.user_types.iter().map(move |(_, t)| (*mod_name, t)))
    }

    pub fn iter_functions<'a>(
        &'a self,
    ) -> impl Iterator<Item = (ModuleName<'src>, &'a FuncDef<'src>)> {
        self.modules
            .iter()
            .flat_map(|(mod_name, m)| m.functions.iter().map(move |(_, t)| (*mod_name, t)))
    }

    pub fn move_iter_types(
        self,
    ) -> impl Iterator<Item = (ModuleName<'src>, UserTypeDefinition<'src>)> {
        self.modules.into_iter().flat_map(move |(mod_name, m)| {
            m.user_types.into_iter().map(move |(_, t)| (mod_name, t))
        })
    }
}
