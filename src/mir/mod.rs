use std::collections::HashMap;

use super::{parse::ast::*, parse::token::Token, resolve::TypeMap, types::Type};
use tac::*;

mod tac;

#[derive(Debug)]
pub struct Tac<'ast, 'input> {
    types: &'ast TypeMap<'input, 'ast>,
    pub(crate) functions: Vec<Func<'input>>,
    literals: HashMap<Label, &'input str>,
    temp_count: usize,
    label_count: usize,
}

impl<'ast, 'input> Tac<'ast, 'input> {
    pub fn new(types: &'ast TypeMap<'input, 'ast>) -> Self {
        Tac {
            types,
            functions: vec![],
            literals: HashMap::new(),
            temp_count: 0,
            label_count: 0,
        }
    }
}

impl<'ast, 'input> Tac<'ast, 'input> {
    pub fn add_function(
        &mut self,
        name: String,
        params: Vec<(&'input str, Type)>,
        body: Block<'input>,
    ) {
        let block = self.create_block(body.0);
        let f = Func::new(name.into(), params, Type::Void, block);

        self.functions.push(f);
    }

    fn create_block(&mut self, statements: Vec<Stmt<'input>>) -> InstructionBlock<'input> {
        let mut block = InstructionBlock::default();

        for s in statements {
            let _ = match s {
                Stmt::Expr(e) => self.expr_instr(e.node, &mut block),
                Stmt::VarDecl { name, value, .. } => {
                    let expr: Expression = if let Some(rval) = self.rvalue(&value.node) {
                        rval.into()
                    } else {
                        self.expr(value.node, &mut block)
                    };

                    let address = name.node.into();
                    self.assign(address, expr, &mut block)
                }
                _ => continue,
            };
        }

        block
    }

    fn expr(
        &mut self,
        expr: Expr<'input>,
        block: &mut InstructionBlock<'input>,
    ) -> Expression<'input> {
        match expr {
            Expr::Binary(l, op, r) | Expr::BoolBinary(l, op, r) => {
                let left = self.expr_instr(l.node, block);
                let right = self.expr_instr(r.node, block);

                let bin_type = match op.node {
                    Token::Plus => BinaryType::I32Add,
                    Token::Minus => BinaryType::I32Sub,
                    Token::Star => BinaryType::I32Mul,
                    Token::Slash => BinaryType::I32Div,
                    Token::EqualsEquals => BinaryType::I32Eq,
                    _ => unimplemented!(),
                };

                Expression::Binary(left, bin_type, right)
            }
            Expr::Call { callee, args } => {
                let args: Vec<Address> = args
                    .into_iter()
                    .map(|a| self.expr_instr(a.node, block))
                    .collect();

                let label = callee.node.to_string().into();

                Expression::Call(label, args)
            }
            _ => unimplemented!(),
        }
    }

    fn expr_instr(
        &mut self,
        expr: Expr<'input>,
        block: &mut InstructionBlock<'input>,
    ) -> Address<'input> {
        if let Some(rval) = self.rvalue(&expr) {
            return rval;
        }

        let e = self.expr(expr, block);
        let temp = self.temp();

        self.assign(temp, e, block)
    }

    fn assign(
        &mut self,
        address: Address<'input>,
        expression: Expression<'input>,
        block: &mut InstructionBlock<'input>,
    ) -> Address<'input> {
        let assign = Instruction::Assignment(address.clone(), expression);
        block.push(assign);
        address
    }

    fn rvalue(&mut self, expr: &Expr<'input>) -> Option<Address<'input>> {
        Some(match expr {
            Expr::DecLit(lit) => Address::new_const(Type::I32, lit),
            Expr::StringLit(lit) => Address::new_global_ref(self.string_lit(lit)),
            Expr::Ident(ident) => Address::new_copy_name(ident),
            _ => return None,
        })
    }

    fn string_lit(&mut self, lit: &'input str) -> Label {
        let label = Label::new(self.label_count);
        self.label_count += 1;
        self.literals.insert(label.clone(), lit);
        label
    }

    fn temp(&mut self) -> Address<'input> {
        let temp = self.temp_count.into();
        self.temp_count += 1;

        Address::Temp(temp)
    }
}
