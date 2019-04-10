//! The middle intermediate representation.
//! This IR is very similar to LLVM-IR, but can also be compiled to Assembly directly.

use std::collections::HashMap;

use super::{
    parse::ast::*,
    resolve::{symbol::SymbolTable, ModFuncMap, ModTypeMap, ResolveResult},
    types::*,
    Spanned,
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

pub struct FunctionHead<'src> {
    name: String,
    params: Vec<(&'src str, Type<'src>)>,
    ret_type: Type<'src>,
    is_extern: bool,
    is_varargs: bool,
}

impl<'src> FunctionHead<'src> {
    pub fn new(
        name: String,
        params: Vec<(&'src str, Type<'src>)>,
        ret_type: Type<'src>,
        is_extern: bool,
        is_varargs: bool,
    ) -> Self {
        FunctionHead {
            name,
            params,
            ret_type,
            is_extern,
            is_varargs,
        }
    }
}

pub enum FunctionBody<'src, 'ast> {
    Block(&'ast Block<'src>),
    Expr(&'ast Expr<'src>),
}

impl<'src, 'ast> From<&'ast ClosureBody<'src>> for FunctionBody<'src, 'ast> {
    fn from(value: &'ast ClosureBody<'src>) -> Self {
        match value {
            ClosureBody::Block(b) => FunctionBody::Block(b),
            ClosureBody::Expr(e) => FunctionBody::Expr(&e.node),
        }
    }
}

#[derive(Debug)]
pub struct Tac<'src, 'ast> {
    pub(crate) functions: HashMap<&'src str, HashMap<String, Func<'src>>>,
    pub(crate) literals: HashMap<Label, &'src str>,
    pub(crate) types: &'ast ModTypeMap<'src>,
    mod_funcs: &'ast ModFuncMap<'src>,
    symbols: &'ast SymbolTable<'src>,
    names: NameTable<'src>,
    temp_count: usize,
    label_count: usize,
}

impl<'src, 'ast> Tac<'src, 'ast> {
    pub fn new(resolve_result: &'ast ResolveResult<'src, 'ast>) -> Self {
        let functions = resolve_result
            .mod_functions
            .iter()
            .map(|(mod_name, _)| (*mod_name, HashMap::new()))
            .collect();

        Tac {
            functions,
            literals: HashMap::new(),
            names: NameTable::new(),
            mod_funcs: &resolve_result.mod_functions,
            symbols: &resolve_result.symbols,
            types: &resolve_result.mod_user_types,
            temp_count: 0,
            label_count: 0,
        }
    }
}

impl<'src, 'ast> Tac<'src, 'ast> {
    pub fn add_function(
        &mut self,
        module: &'src str,
        head: FunctionHead<'src>,
        body: FunctionBody<'src, 'ast>,
    ) {
        // reset scopes
        self.names = NameTable::new();

        let f = if !head.is_extern {
            let mut block = InstructionBlock::default();
            self.fill_params(&mut block, &head.params);
            if let FunctionBody::Block(b) = body {
                block = self.fill_block(&b.0, block);
            }

            let add_ret = match block.last() {
                Some(Instruction::Return(_)) => false,
                _ => true,
            };

            let main_func = head.name == "main";

            if add_ret {
                let ret = if main_func {
                    Some(Address::Const(Constant::new(
                        Type::Simple(Simple::I32),
                        "0",
                    )))
                } else {
                    None
                };

                block.push(Instruction::Return(ret));
            }

            // the main function has to return an int
            let ret_type = if main_func {
                Type::Simple(Simple::I32)
            } else {
                head.ret_type
            };

            Func::new(
                head.name.clone(),
                head.params,
                ret_type,
                BlockMap::from_instructions(block),
                head.is_extern,
                head.is_varargs,
            )
        } else {
            Func::new(
                head.name.clone(),
                head.params,
                head.ret_type,
                BlockMap::default(),
                head.is_extern,
                head.is_varargs,
            )
        };

        self.functions.get_mut(module).unwrap().insert(head.name, f);
    }

    fn fill_params(
        &mut self,
        block: &mut InstructionBlock<'src>,
        params: &[(&'src str, Type<'src>)],
    ) {
        for (i, (n, t)) in params.iter().enumerate() {
            self.names.bind(n);
            let address: Address = self.names.lookup(n).into();
            block.push(Instruction::Decl(address.clone(), t.clone()));
            self.assign(address, Expression::GetParam(i as u32), block);
        }
    }

    fn create_block(&mut self, statements: &[Stmt<'src>]) -> InstructionBlock<'src> {
        let block = InstructionBlock::default();
        self.fill_block(statements, block)
    }

    fn fill_block(
        &mut self,
        statements: &[Stmt<'src>],
        mut block: InstructionBlock<'src>,
    ) -> InstructionBlock<'src> {
        self.names.scope_enter();

        for s in statements {
            match s {
                Stmt::Expr(e) => {
                    self.expr_instr(true, &e.node, &mut block);
                }
                Stmt::VarDecl(decl) => {
                    let VarDecl {
                        name,
                        value,
                        ref ty,
                        ..
                    } = decl.as_ref();

                    let expr = if let Some(rval) = self.address_expr(&value.node) {
                        rval.into()
                    } else {
                        self.expr(true, &value.node, &mut block)
                    };

                    self.names.bind(name.node);
                    let address: Address = self.names.lookup(name.node).into();
                    // Unwrapping is safe, because the typechecker inserted the type
                    let ty = ty.borrow().clone().unwrap().node;
                    block.push(Instruction::Decl(address.clone(), ty));

                    self.assign(address, expr, &mut block);
                }
                Stmt::Return(e) => {
                    let ret = if let Some(Spanned { node, .. }) = e {
                        // If the return value is a struct initilizer, we need to first declare it
                        // as a variable, because the llvm codegenerator expects that the target of
                        // the struct memcpy was already alloca'd
                        let address = self.expr_instr(true, node, &mut block);
                        Instruction::Return(Some(address))
                    } else {
                        Instruction::Return(None)
                    };
                    block.push(ret);
                }
                Stmt::Delete(expr) => {
                    let address = self.expr_instr(true, &expr.node, &mut block);
                    let address = self.temp_assign(Expression::Copy(address), &mut block);
                    block.push(Instruction::Delete(address));
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
        condition: &Expr<'src>,
        body: &Block<'src>,
        block: &mut InstructionBlock<'src>,
        end_label: Label,
    ) {
        let condition_label = self.label();
        block.push(condition_label.clone().into());

        let condition = self.expr_instr(true, condition, block);
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
        condition: &Expr<'src>,
        then_block: &Block<'src>,
        else_branch: &Option<Box<Else<'src>>>,
        block: &mut InstructionBlock<'src>,
        end_label: Label,
    ) {
        let condition = self.expr_instr(true, condition, block);

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
        rhs: bool,
        expr: &Expr<'src>,
        block: &mut InstructionBlock<'src>,
    ) -> Expression<'src> {
        match expr.kind() {
            ExprKind::Binary(l, op, r) => {
                let lty = l.node.clone_ty().unwrap();
                let rty = r.node.clone_ty().unwrap();

                let left = self.expr_instr(true, &l.node, block);
                let right = self.expr_instr(true, &r.node, block);

                // TODO: find correct dec size
                if lty.is_ptr() || rty.is_ptr() {
                    let bin_type = Option::from(&op.node).map(BinaryType::Ptr).unwrap();
                    // the pointer has to be on the left side of the Expression
                    if lty.is_ptr() {
                        Expression::Binary(left, bin_type, right)
                    } else {
                        Expression::Binary(right, bin_type, left)
                    }
                } else {
                    let bin_type = Option::from(&op.node)
                        .map(match lty {
                            Type::Simple(Simple::I32) => BinaryType::I32,
                            Type::Simple(Simple::F32) => BinaryType::F32,
                            // TODO: is this even reachable? Maybe internal error
                            _ => unimplemented!(),
                        })
                        .unwrap();
                    Expression::Binary(left, bin_type, right)
                }
            }
            ExprKind::BoolBinary(l, op, r) => {
                let bin_type = Option::from(&op.node).map(BinaryType::I32).unwrap();

                let left = self.expr_instr(true, &l.node, block);
                let right = self.expr_instr(true, &r.node, block);

                Expression::Binary(left, bin_type, right)
            }
            ExprKind::Call { callee, args } => {
                let args: Vec<Address> = args
                    .0
                    .iter()
                    .map(|a| self.expr_instr(true, &a.node, block))
                    .collect();

                let varargs = self.mod_funcs[callee.node.module()][callee.node.name()].varargs;

                let ret_type = expr.clone_ty().unwrap();
                Expression::Call {
                    ident: callee.node,
                    args,
                    ret_type,
                    varargs,
                }
            }
            ExprKind::Assign { left, value, .. } => {
                let expr = if let Some(rval) = self.address_expr(&value.node) {
                    rval.into()
                } else {
                    self.expr(true, &value.node, block)
                };

                let address = if let ExprKind::Ident(name) = left.node.kind() {
                    self.names.lookup(name).into()
                } else {
                    let e = self.expr(false, &left.node, block);
                    // remove the deref/copy, so that the value can be assigned
                    if let Expression::Copy(a) = &e {
                        a.clone()
                    } else {
                        self.temp_assign(e, block)
                    }
                };
                Expression::Copy(self.assign(address, expr.clone(), block))
            }
            ExprKind::Negate(op, expr) => {
                // TODO: find correct dec size
                let u_type = Option::from(&op.node).unwrap();
                let address = self.expr_instr(true, &expr.node, block);

                Expression::Unary(u_type, address)
            }
            ExprKind::Ref(_, expr) => {
                let address = self.expr_instr(true, &expr.node, block);
                let ref_address = Address::Ref(address.to_string());
                Expression::Copy(ref_address)
            }
            ExprKind::Deref(_, expr) => {
                let u_type = UnaryType::Deref;
                let mut address = self.expr_instr(rhs, &expr.node, block);

                // Expressions on the right side of an assignment have to be derefed twice
                if rhs {
                    address = self.temp_assign(Expression::Unary(u_type, address), block);
                }
                Expression::Unary(u_type, address)
            }
            ExprKind::Access { left, identifier } => {
                use super::types::Simple::UserType;

                let (ty_name, address) = match left.node.clone_ty().unwrap() {
                    Type::Simple(UserType(ty_name)) => {
                        (ty_name, self.expr_instr(rhs, &left.node, block))
                    }
                    Type::Pointer(Pointer {
                        number,
                        ty: UserType(ty_name),
                    }) => {
                        let mut address = self.expr_instr(rhs, &left.node, block);
                        for _ in 0..number {
                            address = self.temp_assign(Expression::Copy(address), block);
                        }
                        (ty_name, address)
                    }
                    _ => unreachable!("Invalid type: {}", left.node.clone_ty().unwrap()),
                };

                // the index of the field inside the struct
                let ty = &self.types[ty_name.module()][ty_name.name()].fields[identifier.node];

                let idx = ty.0;
                let address = self.temp_assign(Expression::StructGep(address, idx), block);
                // Deref by default. This copy has to be removed if the value
                // should be assigned
                Expression::Copy(address)
            }
            ExprKind::StructInit { identifier, fields } => {
                let values = fields
                    .0
                    .iter()
                    .map(|(_, e)| self.expr_instr(true, &e.node, block))
                    .collect();
                Expression::StructInit(identifier.node, values)
            }
            ExprKind::SizeOf(ty) => Expression::SizeOf(ty.clone()),
            ExprKind::New(expr) => {
                let ty = expr.node.clone_ty().unwrap();

                let address = if let ExprKind::Ident(name) = expr.node.kind() {
                    self.names.lookup(name).into()
                } else if let Some(a) = self.address_expr(&expr.node) {
                    let temp = self.temp();
                    block.push(Instruction::Decl(temp.clone(), ty.clone()));
                    self.assign(temp, Expression::Copy(a), block)
                } else {
                    let temp = self.temp();
                    block.push(Instruction::Decl(temp.clone(), ty.clone()));

                    let e = self.expr(true, &expr.node, block);
                    if let Expression::Copy(a) = &e {
                        a.clone()
                    } else {
                        self.assign(temp, e, block)
                    }
                };
                Expression::New(address, ty)
            }
            _ => unimplemented!("{:?}", expr),
        }
    }

    /// Splits an expression and returns an address to the result
    fn expr_instr(
        &mut self,
        rhs: bool,
        expr: &Expr<'src>,
        block: &mut InstructionBlock<'src>,
    ) -> Address<'src> {
        let rval = self.address_expr(&expr);
        if let Some(rval) = rval {
            return rval;
        }

        let e = self.expr(rhs, expr, block);

        if let Expression::Copy(a) = e {
            return a;
        }

        // void functions should be assigned to an empty address
        if let Expression::Call {
            ret_type: Type::Simple(Simple::Void),
            ..
        } = e
        {
            return self.assign(Address::Empty, e, block);
        }

        let temp = self.temp();
        let ty = expr.clone_ty().unwrap();
        block.push(Instruction::Decl(temp.clone(), ty));
        self.assign(temp, e, block)
    }

    fn assign(
        &mut self,
        address: Address<'src>,
        expression: Expression<'src>,
        block: &mut InstructionBlock<'src>,
    ) -> Address<'src> {
        let assign = Instruction::Assignment(address.clone(), Box::new(expression));
        block.push(assign);
        address
    }

    fn handle_ident(&mut self, ident: &'src str) -> Address<'src> {
        self.names.lookup(ident).into()
    }

    fn address_expr(&mut self, expr: &Expr<'src>) -> Option<Address<'src>> {
        Some(match expr.kind() {
            ExprKind::NullLit => Address::Null(expr.clone_ty().unwrap()),
            ExprKind::DecLit(lit) => Address::new_const(Type::Simple(Simple::I32), lit),
            ExprKind::FloatLit(lit) => Address::new_const(Type::Simple(Simple::F32), lit),
            ExprKind::StringLit(lit) => Address::new_global_ref(self.string_lit(lit)),
            ExprKind::Ident(ident) => self.handle_ident(ident),
            _ => return None,
        })
    }

    fn string_lit(&mut self, lit: &'src str) -> Label {
        let label = Label::new(self.label_count);
        self.label_count += 1;
        self.literals.insert(label.clone(), lit);
        label
    }

    fn temp_assign(
        &mut self,
        expression: Expression<'src>,
        block: &mut InstructionBlock<'src>,
    ) -> Address<'src> {
        let temp = self.temp();
        self.assign(temp, expression, block)
    }

    fn temp(&mut self) -> Address<'src> {
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
            def main(): void {
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
            Instruction::Decl(Address::Name("x0".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Name("y0".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Name("z0".to_string()), Type::Simple(Simple::I32)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
            Instruction::Assignment(
                Address::Name("y0".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "2",
                )))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(NumBinaryType::Mul),
                    Address::Name("y0".to_string()),
                )),
            ),
            Instruction::Assignment(
                Address::Name("z0".to_string()),
                Box::new(Expression::Binary(
                    Address::Temp(TempVar::from(0)),
                    BinaryType::I32(NumBinaryType::Add),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "2")),
                )),
            ),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Box::new(Expression::Copy(Address::Name("z0".to_string()))),
            ),
        ];

        bb.terminator = Instruction::Return(Some(Address::Const(Constant::new(
            Type::Simple(Simple::I32),
            "0",
        ))));
        bm.blocks = vec![bb];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_with_if_should_continue_after_if_in_both_cases() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def main(): i32 {
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
            Instruction::Decl(Address::Name("x0".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::Bool)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(NumBinaryType::Eq),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                )),
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
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "2",
                )))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![Instruction::Label(Label::new(0))];
        bb3.terminator = Instruction::Return(Some(Address::Name("x0".to_string())));

        bm.blocks = vec![bb1, bb2, bb3];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_with_if_else_should_continue_after_if_in_both_cases() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def main(): i32 {
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
            Instruction::Decl(Address::Name("x0".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::Bool)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(NumBinaryType::Eq),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                )),
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
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "2",
                )))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "3",
                )))),
            ),
        ];
        bb3.terminator = Instruction::Jmp(Label::new(0));

        let mut bb4 = BasicBlock::default();
        bb4.instructions = vec![Instruction::Label(Label::new(0))];
        bb4.terminator = Instruction::Return(Some(Address::Name("x0".to_string())));

        bm.blocks = vec![bb1, bb2, bb3, bb4];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_while_has_correct_jumps() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def main(): void {
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
            Instruction::Decl(Address::Name("x0".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::Bool)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x0".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
        ];
        bb1.terminator = Instruction::Jmp(Label::new(1));

        let mut bb2 = BasicBlock::default();
        bb2.instructions = vec![
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(NumBinaryType::Smaller),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "10")),
                )),
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
                Box::new(Expression::Binary(
                    Address::Name("x0".to_string()),
                    BinaryType::I32(NumBinaryType::Add),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "1")),
                )),
            ),
        ];
        bb3.terminator = Instruction::Jmp(Label::new(1));

        let mut bb4 = BasicBlock::default();
        bb4.instructions = vec![Instruction::Label(Label::new(0))];
        bb4.terminator = Instruction::Return(Some(Address::Const(Constant::new(
            Type::Simple(Simple::I32),
            "0",
        ))));

        bm.blocks = vec![bb1, bb2, bb3, bb4];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_void_return_can_be_omitted() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def test(): void {

            }

            def main(): void {

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
        main_bb.instructions = vec![Label::from(".entry0".to_string()).into(), Instruction::Nop];
        main_bb.terminator = Instruction::Return(Some(Address::Const(Constant::new(
            Type::Simple(Simple::I32),
            "0",
        ))));

        let mut test_bb = BasicBlock::default();
        test_bb.instructions = vec![Label::from(".entry0".to_string()).into(), Instruction::Nop];
        test_bb.terminator = Instruction::Return(None);

        main_bm.blocks = vec![main_bb];
        test_bm.blocks = vec![test_bb];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "test".to_owned(),
            Func::new(
                "test".to_owned(),
                vec![],
                Type::Simple(Simple::Void),
                test_bm,
                false,
                false,
            ),
        );
        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                main_bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }
}
