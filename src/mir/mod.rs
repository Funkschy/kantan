//! The middle intermediate representation.
//! This IR is very similar to LLVM-IR, but can also be compiled to Assembly directly.

use std::collections::HashMap;

use super::{parse::ast::*, resolve::TypeMap, types::Type, Spanned};
use address::{Address, Argument};
use blockmap::BlockMap;
use func::Func;
use tac::*;

pub(crate) mod address;
mod blockmap;
pub(crate) mod func;
pub(crate) mod tac;

#[derive(Debug)]
pub struct Tac<'input, 'ast> {
    types: &'ast TypeMap<'input, 'ast>,
    pub(crate) functions: Vec<Func<'input>>,
    pub(crate) literals: HashMap<Label, &'input str>,
    temp_count: usize,
    label_count: usize,
    current_params: Option<Vec<&'input str>>,
}

impl<'input, 'ast> Tac<'input, 'ast> {
    pub fn new(types: &'ast TypeMap<'input, 'ast>) -> Self {
        Tac {
            types,
            functions: vec![],
            literals: HashMap::new(),
            temp_count: 0,
            label_count: 0,
            current_params: None,
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
        is_extern: bool,
    ) {
        self.current_params = Some(params.iter().map(|(n, _)| *n).collect());

        let f = if !is_extern {
            let mut block = self.create_block(&body.0);
            let add_ret = match block.last() {
                Some(Instruction::Return(_)) => false,
                _ => true,
            };

            if add_ret {
                block.push(Instruction::Return(None));
            }

            Func::new(
                name.into(),
                params,
                ret_type,
                BlockMap::from_instructions(block),
                false,
            )
        } else {
            Func::new(name.into(), params, ret_type, BlockMap::default(), true)
        };

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

                // TODO: find correct dec size
                let bin_type = Option::from(&op.node).map(BinaryType::I32).unwrap();

                let left = self.expr_instr(&l.node, block).expect(msg);
                let right = self.expr_instr(&r.node, block).expect(msg);

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
            Expr::Negate(op, expr) => {
                // TODO: find correct dec size
                let u_type = Option::from(&op.node).unwrap();
                let expr = self.expr_instr(&expr.node, block).unwrap();

                Expression::Unary(u_type, expr)
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
            Expr::Ident(ident) => {
                if let Some(arg) = self.find_param(ident) {
                    Address::new_arg(arg)
                } else {
                    Address::new_copy_name(ident)
                }
            }
            _ => return None,
        })
    }

    fn find_param(&self, ident: &str) -> Option<Argument> {
        self.current_params
            .as_ref()
            .and_then(|params| params.iter().position(|p| *p == ident))
            .map(Argument::from)
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

#[cfg(test)]
mod tests {
    use super::{address::*, *};
    use crate::{compile, Source};

    use std::io::Cursor;

    #[test]
    fn test_mir_for_sequence_of_instructions_should_have_no_jumps() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            fn main(): void {
                let x = 0;
                let y = 2;

                let z = x * y + 2;
                x = z;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);

        let mut bb = BasicBlock::default();
        bb.instructions = vec![
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Assignment(
                Address::Name("y"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x"),
                    BinaryType::I32(IntBinaryType::Mul),
                    Address::Name("y"),
                ),
            ),
            Instruction::Assignment(
                Address::Name("z"),
                Expression::Binary(
                    Address::Temp(TempVar::from(0)),
                    BinaryType::I32(IntBinaryType::Add),
                    Address::Const(Constant::new(Type::I32, "2")),
                ),
            ),
            Instruction::Assignment(Address::Name("x"), Expression::Copy(Address::Name("z"))),
        ];

        bb.terminator = Instruction::Return(None);
        bm.blocks = vec![bb];

        let expected = vec![Func::new(
            Label::from("main"),
            vec![],
            Type::Void,
            bm,
            false,
        )];

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_with_if_should_continue_after_if_in_both_cases() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            fn main(): i32 {
                let x = 0;
                if x == 0 {
                    x = 2;
                }
                return x;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);
        bm.mappings.insert(Label::new(0), 2);
        bm.mappings.insert(Label::new(1), 1);

        let mut bb1 = BasicBlock::default();
        bb1.instructions = vec![
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x"),
                    BinaryType::I32(IntBinaryType::Eq),
                    Address::Const(Constant::new(Type::I32, "0")),
                ),
            ),
        ];
        bb1.terminator = Instruction::JmpIf(
            Address::Temp(TempVar::from(0)),
            Label::new(1),
            Label::new(0),
        );

        let mut bb2 = BasicBlock::default();
        bb2.instructions = vec![
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![Instruction::Label(Label::new(0))];
        bb3.terminator = Instruction::Return(Some(Address::Name("x")));

        bm.blocks = vec![bb1, bb2, bb3];

        let expected = vec![Func::new(
            Label::from("main".to_string()),
            vec![],
            Type::I32,
            bm,
            false,
        )];

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_with_if_else_should_continue_after_if_in_both_cases() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            fn main(): i32 {
                let x = 0;
                if x == 0 {
                    x = 2;
                } else {
                    x = 3;
                }
                return x;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);
        bm.mappings.insert(Label::new(0), 3);
        bm.mappings.insert(Label::new(1), 1);
        bm.mappings.insert(Label::new(2), 2);

        let mut bb1 = BasicBlock::default();
        bb1.instructions = vec![
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x"),
                    BinaryType::I32(IntBinaryType::Eq),
                    Address::Const(Constant::new(Type::I32, "0")),
                ),
            ),
        ];
        bb1.terminator = Instruction::JmpIf(
            Address::Temp(TempVar::from(0)),
            Label::new(1),
            Label::new(2),
        );

        let mut bb2 = BasicBlock::default();
        bb2.instructions = vec![
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "3"))),
            ),
        ];
        bb3.terminator = Instruction::Jmp(Label::new(0));

        let mut bb4 = BasicBlock::default();
        bb4.instructions = vec![Instruction::Label(Label::new(0))];
        bb4.terminator = Instruction::Return(Some(Address::Name("x")));

        bm.blocks = vec![bb1, bb2, bb3, bb4];

        let expected = vec![Func::new(
            Label::from("main".to_string()),
            vec![],
            Type::I32,
            bm,
            false,
        )];

        assert_eq!(expected, funcs);
    }
}
