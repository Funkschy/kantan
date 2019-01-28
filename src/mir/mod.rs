use super::{parse::ast::*, resolve::TypeMap, types::Type};

mod tac;

use tac::*;

pub struct Tac<'ast, 'input> {
    types: TypeMap<'input, 'ast>,
    functions: Vec<Func<'input>>,
}

impl<'ast, 'input> Tac<'ast, 'input> {
    pub fn add_function(&mut self, name: String, params: Vec<Type>, body: Block<'input>) {
        let block = self.create_block(body.0);
        let f = Func::new(name.into(), params, Type::Void, block);

        self.functions.push(f);
    }

    fn create_block(&self, statements: Vec<Stmt<'input>>) -> InstructionBlock<'input> {
        unimplemented!()
    }

    fn expression(&self, expression: Expr<'input>) -> Instruction<'input> {
        unimplemented!()
    }
}
