use std::collections::HashMap;

use super::{
    address::{Address, Constant},
    blockmap::BlockMap,
    func::Func,
    names::NameTable,
    tac::*,
    FunctionHead,
};
use crate::{
    parse::{ast::*, token::Token},
    resolve::{modmap::ModMap, symbol::SymbolTable, ResolveResult},
    types::*,
};

#[derive(Debug)]
pub struct MirBuilder<'src> {
    pub(crate) functions: HashMap<&'src str, HashMap<String, Func<'src>>>,
    pub(crate) literals: HashMap<Label, &'src str>,
    pub(crate) definitions: ModMap<'src>,
    symbols: SymbolTable<'src>,
    names: NameTable<'src>,
    temp_count: usize,
    label_count: usize,
}

impl<'src> MirBuilder<'src> {
    pub fn new(resolve_result: ResolveResult<'src>) -> Self {
        let functions = resolve_result
            .definitions
            .iter_functions()
            .map(|(mod_name, _)| (mod_name, HashMap::new()))
            .collect();

        MirBuilder {
            functions,
            literals: HashMap::new(),
            names: NameTable::new(),
            definitions: resolve_result.definitions,
            symbols: resolve_result.symbols,
            temp_count: 0,
            label_count: 0,
        }
    }
}

impl<'src> MirBuilder<'src> {
    pub fn add_function(
        &mut self,
        module: &'src str,
        head: FunctionHead<'src>,
        body: &Block<'src>,
    ) {
        // reset scopes
        self.names = NameTable::new();

        let main_func = head.name.as_str() == "main";

        let (ret_type, block_map) = if head.is_extern {
            (head.ret_type, BlockMap::default())
        } else {
            let mut block = InstructionBlock::default();
            self.fill_params(&mut block, &head.params);
            block = self.fill_block(&body.0, main_func, block);

            // the main function has to return an int
            let ret_type = if main_func {
                Type::Simple(Simple::I32)
            } else {
                head.ret_type
            };

            Self::add_ret(main_func, &mut block);

            (ret_type, BlockMap::from_instructions(block))
        };

        let f = Func::new(
            head.name.clone(),
            head.params,
            ret_type,
            block_map,
            head.is_extern,
            head.is_varargs,
        );

        self.functions.get_mut(module).unwrap().insert(head.name, f);
    }

    fn add_ret(main_func: bool, block: &mut InstructionBlock<'src>) {
        let add_ret = match block.last() {
            Some(Instruction::Return(_)) => false,
            _ => true,
        };

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
    }

    fn fill_params(
        &mut self,
        block: &mut InstructionBlock<'src>,
        params: &[(&'src str, Type<'src>)],
    ) {
        for (i, (n, t)) in params.iter().enumerate() {
            self.bind_param(i as u32, n, t, block);
        }
    }

    #[inline]
    fn bind_param(
        &mut self,
        index: u32,
        name: &'src str,
        ty: &Type<'src>,
        block: &mut InstructionBlock<'src>,
    ) {
        self.names.bind(name);
        let address = self.lookup_ident(name);
        block.push(Instruction::Decl(address.clone(), ty.clone()));
        self.assign(address, Expression::GetParam(index), block);
    }

    fn create_block(
        &mut self,
        main_func: bool,
        statements: &[Stmt<'src>],
    ) -> InstructionBlock<'src> {
        let block = InstructionBlock::default();
        self.fill_block(statements, main_func, block)
    }

    fn fill_block(
        &mut self,
        statements: &[Stmt<'src>],
        main_func: bool,
        mut block: InstructionBlock<'src>,
    ) -> InstructionBlock<'src> {
        self.names.scope_enter();

        for s in statements {
            self.stmt(s, main_func, &mut block);
        }

        self.names.scope_exit();

        block
    }

    fn var_decl(
        &mut self,
        name: &'src str,
        ty: Type<'src>,
        value: Expression<'src>,
        block: &mut InstructionBlock<'src>,
    ) -> Address<'src> {
        self.names.bind(name);
        let address: Address = self.lookup_ident(name);

        block.push(Instruction::Decl(address.clone(), ty));
        self.assign(address, value, block)
    }

    fn stmt(&mut self, stmt: &Stmt<'src>, main_func: bool, block: &mut InstructionBlock<'src>) {
        match stmt {
            Stmt::Expr(e) => {
                self.expr_instr(false, &e.node, block);
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
                    self.expr(true, &value.node, block)
                };

                // Unwrapping is safe, because the typechecker inserted the type
                let ty = ty.borrow().clone().unwrap().node;
                self.var_decl(name.node, ty, expr, block);
            }
            Stmt::Return(Some(e)) => self.return_stmt(Some(&e.node), main_func, block),
            Stmt::Return(None) => self.return_stmt(None, main_func, block),
            Stmt::Delete(expr) => {
                let address = self.expr_instr(true, &expr.node, block);
                let address = self.temp_assign(Expression::Copy(address), block);
                block.push(Instruction::Delete(address));
            }
            Stmt::While(while_stmt) => {
                let WhileStmt { condition, body } = while_stmt.as_ref();

                let end_label = self.label();
                self.while_loop(&condition.node, body, main_func, block, end_label.clone());
                block.push(Instruction::Label(end_label));
            }
            Stmt::If(if_stmt) => {
                let IfStmt {
                    condition,
                    then_block,
                    else_branch,
                } = if_stmt.as_ref();

                let end_label = self.label();
                self.if_branch(
                    &condition.node,
                    then_block,
                    else_branch,
                    main_func,
                    block,
                    end_label.clone(),
                );
                block.push(Instruction::Label(end_label));
            }
        };
    }

    fn return_stmt(
        &mut self,
        e: Option<&Expr<'src>>,
        main_func: bool,
        block: &mut InstructionBlock<'src>,
    ) {
        let ret = if let Some(node) = e {
            // If the return value is a struct initilizer, we need to first declare it
            // as a variable, because the llvm codegenerator expects that the target of
            // the struct memcpy was already alloca'd
            let address = self.expr_instr(true, node, block);
            Instruction::Return(Some(address))
        } else if main_func {
            Instruction::Return(Some(Address::Const(Constant::new(
                Type::Simple(Simple::I32),
                "0",
            ))))
        } else {
            Instruction::Return(None)
        };
        block.push(ret);
    }

    fn while_loop(
        &mut self,
        condition: &Expr<'src>,
        body: &Block<'src>,
        main_func: bool,
        block: &mut InstructionBlock<'src>,
        end_label: Label,
    ) {
        let condition_label = self.label();
        block.push(condition_label.clone().into());

        let condition = self.expr_instr(true, condition, block);
        let mut body = self.create_block(main_func, &body.0);
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
        main_func: bool,
        block: &mut InstructionBlock<'src>,
        end_label: Label,
    ) {
        let condition = self.expr_instr(true, condition, block);

        let mut then_block = self.create_block(main_func, &then_block.0);
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
                    if let Stmt::If(if_stmt) = s.as_ref() {
                        let IfStmt {
                            condition,
                            then_block,
                            else_branch,
                        } = if_stmt.as_ref();

                        self.if_branch(
                            &condition.node,
                            &then_block,
                            &else_branch,
                            main_func,
                            block,
                            end_label.clone(),
                        );
                    } else {
                        panic!("Only if statement allowed here");
                    }
                }
                Else::Block(b) => {
                    let mut b = self.create_block(main_func, &b.0);
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
                if lty.is_ptr() && rty.is_ptr() {
                    let bin_type = BinaryType::Ptr(match op.node {
                        Token::Plus => PtrBinaryType::AddPointers,
                        Token::Minus => PtrBinaryType::SubPointers,
                        _ => unimplemented!("Should be unreachable, but probably isn't"),
                    });

                    Expression::Binary(left, bin_type, right)
                } else if lty.is_ptr() || rty.is_ptr() {
                    let bin_type = Option::from(&op.node).map(BinaryType::Ptr).unwrap();
                    // the pointer has to be on the left side of the Expression
                    if lty.is_ptr() {
                        Expression::Binary(left, bin_type, right)
                    } else {
                        Expression::Binary(right, bin_type, left)
                    }
                } else {
                    let bin_type = BinaryType::from_tok_and_type(&op.node, &lty);
                    Expression::Binary(left, bin_type, right)
                }
            }
            ExprKind::BoolBinary(l, op, r) => {
                let bin_type =
                    BinaryType::from_tok_and_type(&op.node, (&l.node.ty()).as_ref().unwrap());

                let left = self.expr_instr(true, &l.node, block);
                let right = self.expr_instr(true, &r.node, block);

                Expression::Binary(left, bin_type, right)
            }
            ExprKind::Call {
                callee,
                args,
                module,
            } => {
                let args: Vec<Address> = args
                    .0
                    .iter()
                    .map(|a| self.expr_instr(true, &a.node, block))
                    .collect();

                let ret_type = expr.clone_ty().unwrap();

                if let ExprKind::Ident(ident) = callee.node.kind() {
                    let varargs = self
                        .definitions
                        .get_function(module, ident)
                        .map_or(false, |f| f.varargs);

                    Expression::Call {
                        ident: UserIdent::new(module, ident),
                        args,
                        ret_type,
                        varargs,
                    }
                } else {
                    unreachable!(
                        "the type checker should not allow calling arbitrary expressions right now"
                    )
                }
            }
            ExprKind::Assign { left, value, .. } => {
                let expr = if let Some(rval) = self.address_expr(&value.node) {
                    rval.into()
                } else {
                    self.expr(true, &value.node, block)
                };

                let address = if let ExprKind::Ident(name) = left.node.kind() {
                    self.lookup_ident(name)
                } else {
                    let e = self.expr(false, &left.node, block);
                    self.get_expression_address(e, block)
                };
                Expression::Copy(self.assign(address, expr, block))
            }
            ExprKind::Negate(op, expr) | ExprKind::BoolNegate(op, expr) => {
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
                use crate::types::Simple::UserType;

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
                let (idx, _) =
                    self.definitions.get_user_type(&ty_name).unwrap().fields[identifier.node];

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
                    self.lookup_ident(name)
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
            ExprKind::Cast(expr, _, ty) => {
                let address = self.expr_instr(rhs, &expr.node, block);
                Expression::BitCast(address, ty.node.clone())
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

    #[inline]
    fn get_expression_address(
        &mut self,
        e: Expression<'src>,
        block: &mut InstructionBlock<'src>,
    ) -> Address<'src> {
        // remove the deref/copy, so that the value can be assigned
        if let Expression::Copy(a) = &e {
            a.clone()
        } else {
            self.temp_assign(e, block)
        }
    }

    #[inline]
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

    #[inline]
    fn lookup_ident(&mut self, ident: &'src str) -> Address<'src> {
        self.names.lookup(ident).into()
    }

    fn address_expr(&mut self, expr: &Expr<'src>) -> Option<Address<'src>> {
        Some(match expr.kind() {
            ExprKind::NullLit => Address::Null(expr.clone_ty().unwrap()),
            ExprKind::DecLit(lit) => Address::new_const(Type::Simple(Simple::I32), lit),
            ExprKind::Char(c) => Address::new_const(Type::Simple(Simple::Char), c),
            ExprKind::FloatLit(lit) => Address::new_const(Type::Simple(Simple::F32), lit),
            ExprKind::StringLit(lit) => Address::new_global_ref(self.string_lit(lit)),
            ExprKind::Ident(ident) => self.lookup_ident(ident),
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

    #[inline]
    fn temp(&mut self) -> Address<'src> {
        let temp = self.temp_count.into();
        self.temp_count += 1;

        Address::Temp(temp)
    }

    #[inline]
    fn label(&mut self) -> Label {
        let label = Label::new(self.label_count);
        self.label_count += 1;
        label
    }
}
