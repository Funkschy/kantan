//! The middle intermediate representation.
//! This IR is very similar to LLVM-IR, but can also be compiled to Assembly directly.

use std::collections::HashMap;

use super::{parse::ast::*, parse::token::Token, resolve::TypeMap, types::Type, Spanned};
use blockmap::BlockMap;
use tac::*;

mod blockmap;
mod tac;

#[derive(Debug)]
pub struct Tac<'input, 'ast> {
    types: &'ast TypeMap<'input, 'ast>,
    pub(crate) functions: Vec<Func<'input>>,
    literals: HashMap<Label, &'input str>,
    temp_count: usize,
    label_count: usize,
}

impl<'input, 'ast> Tac<'input, 'ast> {
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

impl<'input, 'ast> Tac<'input, 'ast> {
    pub fn add_function(
        &mut self,
        name: String,
        params: Vec<(&'input str, Type)>,
        body: &Block<'input>,
        ret_type: Type,
    ) {
        let mut block = self.create_block(&body.0);
        let add_ret = match block.last() {
            Some(Instruction::Return(_)) => false,
            _ => true,
        };

        if add_ret {
            block.push(Instruction::Return(None));
        }

        let f = Func::new(
            name.into(),
            params,
            ret_type,
            BlockMap::from_instructions(block),
        );

        self.functions.push(f);
    }

    fn create_block(&mut self, statements: &[Stmt<'input>]) -> InstructionBlock<'input> {
        let mut block = InstructionBlock::default();

        for s in statements {
            match s {
                Stmt::Expr(e) => {
                    self.expr_instr(&e.node, &mut block);
                }
                Stmt::VarDecl { name, value, .. } => {
                    let expr = if let Some(rval) = self.rvalue(&value.node) {
                        rval.into()
                    } else {
                        self.expr(&value.node, &mut block)
                    };

                    let address = name.node.into();
                    self.assign(address, expr, &mut block);
                }
                Stmt::Return(e) => {
                    let ret = if let Some(Spanned { node, .. }) = e {
                        let address = self.expr_instr(node, &mut block).unwrap();
                        Instruction::Return(Some(address))
                    } else {
                        Instruction::Return(None)
                    };
                    block.push(ret);
                }
                Stmt::If {
                    condition,
                    then_block,
                    else_branch,
                } => {
                    let end_label = self.label();
                    self.if_branch(
                        &condition.node,
                        then_block,
                        else_branch,
                        &mut block,
                        end_label.clone(),
                    );
                    block.push(Instruction::Label(end_label));
                }
            };
        }

        block
    }

    fn if_branch(
        &mut self,
        condition: &Expr<'input>,
        then_block: &Block<'input>,
        else_branch: &Option<Box<Else<'input>>>,
        block: &mut InstructionBlock<'input>,
        end_label: Label,
    ) {
        let msg = "unexpected empty expression";
        let condition = self.expr_instr(condition, block).expect(msg);

        let mut then_block = self.create_block(&then_block.0);
        let then_label = self.label();

        let else_label = if else_branch.is_some() {
            self.label()
        } else {
            end_label.clone()
        };

        let instr = Instruction::JmpIf(condition, then_label.clone(), else_label.clone());
        block.push(instr);
        block.push(then_label.into());
        block.append(&mut then_block);
        block.push(Instruction::Jmp(end_label.clone()));

        if let Some(else_branch) = else_branch {
            block.push(else_label.into());

            match else_branch.as_ref() {
                Else::IfStmt(s) => {
                    if let Stmt::If {
                        condition,
                        then_block,
                        else_branch,
                    } = s
                    {
                        self.if_branch(
                            &condition.node,
                            &then_block,
                            &else_branch,
                            block,
                            end_label.clone(),
                        );
                    } else {
                        panic!("Only if statement allowed here");
                    }
                }
                Else::Block(b) => {
                    let mut b = self.create_block(&b.0);
                    block.append(&mut b);
                }
            }
        }
    }

    fn expr(
        &mut self,
        expr: &Expr<'input>,
        block: &mut InstructionBlock<'input>,
    ) -> Expression<'input> {
        match expr {
            Expr::Binary(l, op, r) | Expr::BoolBinary(l, op, r) => {
                let msg = "unexpected empty expression";
                let left = self.expr_instr(&l.node, block).expect(msg);
                let right = self.expr_instr(&r.node, block).expect(msg);

                let bin_type = match op.node {
                    Token::Plus => BinaryType::I32Add,
                    Token::Minus => BinaryType::I32Sub,
                    Token::Star => BinaryType::I32Mul,
                    Token::Slash => BinaryType::I32Div,
                    Token::EqualsEquals => BinaryType::I32Eq,
                    Token::Smaller => BinaryType::I32Smaller,
                    Token::SmallerEquals => BinaryType::I32SmallerEq,
                    _ => unimplemented!(),
                };

                Expression::Binary(left, bin_type, right)
            }
            Expr::Call { callee, args } => {
                let args: Vec<Address> = args
                    .0
                    .iter()
                    .filter_map(|a| self.expr_instr(&a.node, block))
                    .collect();

                let label = callee.node.to_string().into();

                Expression::Call(label, args)
            }
            Expr::Assign { name, value, .. } => {
                let expr = if let Some(rval) = self.rvalue(&value.node) {
                    rval.into()
                } else {
                    self.expr(&value.node, block)
                };

                let address = (*name).into();
                self.assign(address, expr, block);
                Expression::Empty
            }
            _ => unimplemented!(),
        }
    }

    fn expr_instr(
        &mut self,
        expr: &Expr<'input>,
        block: &mut InstructionBlock<'input>,
    ) -> Option<Address<'input>> {
        let rval = self.rvalue(&expr);
        if rval.is_some() {
            return rval;
        }

        let e = self.expr(expr, block);
        if e.is_empty() {
            return None;
        }

        let temp = self.temp();

        Some(self.assign(temp, e, block))
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

    fn label(&mut self) -> Label {
        let label = Label::new(self.label_count);
        self.label_count += 1;
        label
    }
}
