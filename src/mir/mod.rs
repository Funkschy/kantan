//! The middle intermediate representation.
//! This IR is very similar to LLVM-IR, but can also be compiled to Assembly directly.

use std::{collections::HashMap, mem};

use super::{
    parse::ast::*,
    resolve::{symbol::SymbolTable, ResolveResult},
    types::Type,
    Spanned, UserTypeMap,
};
use address::{Address, Constant};
use blockmap::BlockMap;
use func::Func;
use names::NameTable;
use tac::*;

pub(crate) mod address;
mod blockmap;
pub(crate) mod func;
mod names;
pub(crate) mod tac;

#[derive(Debug)]
pub struct Tac<'input> {
    pub(crate) functions: Vec<Func<'input>>,
    pub(crate) literals: HashMap<Label, &'input str>,
    pub(crate) types: UserTypeMap<'input>,
    symbols: SymbolTable<'input>,
    names: NameTable<'input>,
    temp_count: usize,
    label_count: usize,
    current_params: Option<Vec<(&'input str, Type<'input>)>>,
}

impl<'input> Tac<'input> {
    pub fn new(resolve_result: ResolveResult<'input>) -> Self {
        Tac {
            functions: vec![],
            literals: HashMap::new(),
            names: NameTable::new(),
            symbols: resolve_result.symbols,
            types: resolve_result.user_types,
            temp_count: 0,
            label_count: 0,
            current_params: None,
        }
    }
}

impl<'input> Tac<'input> {
    pub fn add_function(
        &mut self,
        name: String,
        params: Vec<(&'input str, Type<'input>)>,
        body: &Block<'input>,
        ret_type: Type<'input>,
        is_extern: bool,
        is_varargs: bool,
    ) {
        // reset scopes
        self.names = NameTable::new();
        self.current_params = Some(params);

        let f = if !is_extern {
            let mut block = self.create_block(&body.0);
            let add_ret = match block.last() {
                Some(Instruction::Return(_)) => false,
                _ => true,
            };

            let main_func = name == "main";

            if add_ret {
                let ret = if main_func {
                    Some(Address::Const(Constant::new(Type::I32, "0")))
                } else {
                    None
                };

                block.push(Instruction::Return(ret));
            }

            // the main function has to return an int
            let ret_type = if main_func { Type::I32 } else { ret_type };

            // move params out of current_params and replace with None
            let mut moved_params = None;
            mem::swap(&mut self.current_params, &mut moved_params);

            Func::new(
                name.into(),
                moved_params.unwrap(),
                ret_type,
                BlockMap::from_instructions(block),
                is_extern,
                is_varargs,
            )
        } else {
            // move params out of current_params and replace with None
            let mut moved_params = None;
            mem::swap(&mut self.current_params, &mut moved_params);

            Func::new(
                name.into(),
                moved_params.unwrap(),
                ret_type,
                BlockMap::default(),
                is_extern,
                is_varargs,
            )
        };

        self.functions.push(f);
    }

    fn create_block(&mut self, statements: &[Stmt<'input>]) -> InstructionBlock<'input> {
        let mut block = InstructionBlock::default();
        self.names.scope_enter();

        for s in statements {
            match s {
                Stmt::Expr(e) => {
                    self.expr_instr(&e.node, &mut block);
                }
                Stmt::VarDecl {
                    name, value, ty, ..
                } => {
                    let expr = if let Some(rval) = self.address_expr(&value.node) {
                        rval.into()
                    } else {
                        self.expr(&value.node, &mut block)
                    };

                    self.names.bind(name.node);
                    let address: Address = self.names.lookup(name.node).into();
                    // Unwrapping is safe, because the typechecker inserted the type
                    let ty = ty.get().unwrap().node;
                    block.push(Instruction::Decl(address.clone(), ty));

                    self.assign(address, expr, &mut block);
                }
                Stmt::Return(e) => {
                    let ret = if let Some(Spanned { node, .. }) = e {
                        // If the return value is a struct initilizer, we need to first declare it
                        // as a variabel, because the llvm codegenerator expects that the target of
                        // the struct memcpy was already alloca'd
                        let address = if let ExprKind::StructInit { .. } = node.kind() {
                            let address = self.temp();
                            let ty = node.ty().unwrap();
                            block.push(Instruction::Decl(address.clone(), ty.clone()));
                            let expr = self.expr(node, &mut block);
                            let address = self.assign(address, expr, &mut block);
                            // Wrap in Name, so context will generate a load instruction
                            Address::new_copy_name(address.to_string())
                        } else {
                            self.expr_instr(node, &mut block)
                        };
                        Instruction::Return(Some(address))
                    } else {
                        Instruction::Return(None)
                    };
                    block.push(ret);
                }
                Stmt::While { condition, body } => {
                    let end_label = self.label();
                    self.while_loop(&condition.node, body, &mut block, end_label.clone());
                    block.push(Instruction::Label(end_label));
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

        self.names.scope_exit();

        block
    }

    fn while_loop(
        &mut self,
        condition: &Expr<'input>,
        body: &Block<'input>,
        block: &mut InstructionBlock<'input>,
        end_label: Label,
    ) {
        let condition_label = self.label();
        block.push(condition_label.clone().into());

        let condition = self.expr_instr(condition, block);
        let mut body = self.create_block(&body.0);
        let body_label = self.label();

        let instr = Instruction::JmpIf(condition, body_label.clone(), end_label);
        block.push(instr);
        block.push(body_label.into());
        block.append(&mut body);
        block.push(Instruction::Jmp(condition_label));
    }

    fn if_branch(
        &mut self,
        condition: &Expr<'input>,
        then_block: &Block<'input>,
        else_branch: &Option<Box<Else<'input>>>,
        block: &mut InstructionBlock<'input>,
        end_label: Label,
    ) {
        let condition = self.expr_instr(condition, block);

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
                    } = s.as_ref()
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
        match expr.kind() {
            ExprKind::Binary(l, op, r) | ExprKind::BoolBinary(l, op, r) => {
                // TODO: find correct dec size
                let bin_type = Option::from(&op.node).map(BinaryType::I32).unwrap();

                let left = self.expr_instr(&l.node, block);
                let right = self.expr_instr(&r.node, block);

                Expression::Binary(left, bin_type, right)
            }
            ExprKind::Call { callee, args } => {
                let args: Vec<Address> = args
                    .0
                    .iter()
                    .map(|a| self.expr_instr(&a.node, block))
                    .collect();

                let label = callee.node.to_string().into();

                Expression::Call(label, args)
            }
            ExprKind::Assign { left, value, .. } => {
                let expr = if let Some(rval) = self.address_expr(&value.node) {
                    rval.into()
                } else {
                    self.expr(&value.node, block)
                };

                let address = if let ExprKind::Ident(name) = left.node.kind() {
                    self.names.lookup(name).into()
                } else {
                    self.expr_instr(&left.as_ref().node, block)
                };
                Expression::Copy(self.assign(address, expr.clone(), block))
            }
            ExprKind::Negate(op, expr) => {
                // TODO: find correct dec size
                let u_type = Option::from(&op.node).unwrap();
                let address = self.expr_instr(&expr.node, block);

                Expression::Unary(u_type, address)
            }
            ExprKind::Access { left, identifier } => {
                let address = self.expr_instr(&left.node, block);
                if let Some(Type::UserType(ty)) = left.node.ty() {
                    // the index of the field inside the struct
                    let idx = self.types[ty].fields[identifier.node].0;
                    Expression::StructGep(address, idx)
                } else {
                    // The resolver should insert the type information
                    unreachable!("No type information for '{}' available", left.node);
                }
            }
            ExprKind::StructInit { identifier, fields } => {
                let values = fields
                    .0
                    .iter()
                    .map(|(_, e)| self.expr_instr(&e.node, block))
                    .collect();
                Expression::StructInit(identifier.node, values)
            }
            _ => unimplemented!(),
        }
    }

    /// Splits an expression and returns an address to the result
    fn expr_instr(
        &mut self,
        expr: &Expr<'input>,
        block: &mut InstructionBlock<'input>,
    ) -> Address<'input> {
        let rval = self.address_expr(&expr);
        if let Some(rval) = rval {
            return rval;
        }

        let e = self.expr(expr, block);
        if let Expression::Copy(a) = e {
            return a;
        }

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

    fn address_expr(&mut self, expr: &Expr<'input>) -> Option<Address<'input>> {
        Some(match expr.kind() {
            ExprKind::DecLit(lit) => Address::new_const(Type::I32, lit),
            ExprKind::StringLit(lit) => Address::new_global_ref(self.string_lit(lit)),
            ExprKind::Ident(ident) => {
                if let Some(arg) = self.find_param(ident) {
                    Address::Name(arg)
                } else {
                    // Address::Name
                    self.names.lookup(ident).into()
                }
            }
            _ => return None,
        })
    }

    fn find_param(&self, ident: &str) -> Option<String> {
        self.current_params.as_ref().and_then(|params| {
            params.iter().find_map(|(name, _)| {
                if *name == ident {
                    return Some(name.to_string());
                }
                None
            })
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
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x0".to_string()), Type::I32),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Decl(Address::Name("y0".to_string()), Type::I32),
            Instruction::Assignment(
                Address::Name("y0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(IntBinaryType::Mul),
                    Address::Name("y0".to_string()),
                ),
            ),
            Instruction::Decl(Address::Name("z0".to_string()), Type::I32),
            Instruction::Assignment(
                Address::Name("z0".to_string()),
                Expression::Binary(
                    Address::Temp(TempVar::from(0)),
                    BinaryType::I32(IntBinaryType::Add),
                    Address::Const(Constant::new(Type::I32, "2")),
                ),
            ),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Name("z0".to_string())),
            ),
        ];

        bb.terminator = Instruction::Return(Some(Address::Const(Constant::new(Type::I32, "0"))));
        bm.blocks = vec![bb];

        let expected = vec![Func::new(
            Label::from("main"),
            vec![],
            Type::I32,
            bm,
            false,
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
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x0".to_string()), Type::I32),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x0".to_string()),
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
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![Instruction::Label(Label::new(0))];
        bb3.terminator = Instruction::Return(Some(Address::Name("x0".to_string())));

        bm.blocks = vec![bb1, bb2, bb3];

        let expected = vec![Func::new(
            Label::from("main".to_string()),
            vec![],
            Type::I32,
            bm,
            false,
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
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x0".to_string()), Type::I32),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x0".to_string()),
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
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "3"))),
            ),
        ];
        bb3.terminator = Instruction::Jmp(Label::new(0));

        let mut bb4 = BasicBlock::default();
        bb4.instructions = vec![Instruction::Label(Label::new(0))];
        bb4.terminator = Instruction::Return(Some(Address::Name("x0".to_string())));

        bm.blocks = vec![bb1, bb2, bb3, bb4];

        let expected = vec![Func::new(
            Label::from("main".to_string()),
            vec![],
            Type::I32,
            bm,
            false,
            false,
        )];

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_while_has_correct_jumps() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            fn main(): void {
                let x = 0;
                while x < 10 {
                    x = x + 1;
                }
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);
        bm.mappings.insert(Label::new(1), 1);
        bm.mappings.insert(Label::new(2), 2);
        bm.mappings.insert(Label::new(0), 3);

        // init block
        let mut bb1 = BasicBlock::default();
        bb1.instructions = vec![
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x0".to_string()), Type::I32),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
        ];
        bb1.terminator = Instruction::Jmp(Label::new(1));

        let mut bb2 = BasicBlock::default();
        bb2.instructions = vec![
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(IntBinaryType::Smaller),
                    Address::Const(Constant::new(Type::I32, "10")),
                ),
            ),
        ];
        bb2.terminator = Instruction::JmpIf(
            Address::Temp(TempVar::from(0)),
            Label::new(2),
            Label::new(0),
        );

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(IntBinaryType::Add),
                    Address::Const(Constant::new(Type::I32, "1")),
                ),
            ),
        ];
        bb3.terminator = Instruction::Jmp(Label::new(1));

        let mut bb4 = BasicBlock::default();
        bb4.instructions = vec![Instruction::Label(Label::new(0))];
        bb4.terminator = Instruction::Return(Some(Address::Const(Constant::new(Type::I32, "0"))));

        bm.blocks = vec![bb1, bb2, bb3, bb4];

        let expected = vec![Func::new(
            Label::from("main".to_string()),
            vec![],
            Type::I32,
            bm,
            false,
            false,
        )];

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_void_return_can_be_omitted() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            fn test(): void {

            }

            fn main(): void {

            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut main_bm = BlockMap::default();
        main_bm
            .mappings
            .insert(Label::from(".entry0".to_string()), 0);

        let mut test_bm = BlockMap::default();
        test_bm
            .mappings
            .insert(Label::from(".entry0".to_string()), 0);

        let mut main_bb = BasicBlock::default();
        main_bb.instructions = vec![Label::from(".entry0".to_string()).into()];
        main_bb.terminator =
            Instruction::Return(Some(Address::Const(Constant::new(Type::I32, "0"))));

        let mut test_bb = BasicBlock::default();
        test_bb.instructions = vec![Label::from(".entry0".to_string()).into()];
        test_bb.terminator = Instruction::Return(None);

        main_bm.blocks = vec![main_bb];
        test_bm.blocks = vec![test_bb];

        let expected = vec![
            Func::new(
                Label::from("test"),
                vec![],
                Type::Void,
                test_bm,
                false,
                false,
            ),
            Func::new(
                Label::from("main"),
                vec![],
                Type::I32,
                main_bm,
                false,
                false,
            ),
        ];

        assert_eq!(expected, funcs);
    }
}
