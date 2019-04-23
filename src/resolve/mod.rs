use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
};

use crate::{
    parse::{
        ast::*,
        token::{Precedence, Token},
        Span, Spanned,
    },
    types::*,
    Source, UserTypeDefinition, UserTypeMap,
};

use self::error::*;
use self::symbol::*;

use super::*;

mod error;
pub mod symbol;

/// modname -> typename -> typedef
pub type ModMap<'src, T> = HashMap<&'src str, T>;

pub type ModTypeMap<'src> = ModMap<'src, UserTypeMap<'src>>;
pub type ModFuncMap<'src> = ModMap<'src, FunctionMap<'src>>;
pub type ModClosureMap<'src> = ModMap<'src, ClosureDefinitions<'src>>;
pub type ModCompilerTypeMap<'src> = ModMap<'src, Vec<CompilerType<'src>>>;

/// Types constructed by the compiler
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilerType<'src> {
    pub type_idx: usize,
    pub free_vars: HashMap<FreeVarKey<'src>, FreeVarValue>,
    pub closure_type: ClosureType<'src>,
}

impl<'src> fmt::Display for CompilerType<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let vars = self
            .free_vars
            .iter()
            .map(|(k, _)| format!("{}: {}", k.name, k.ty))
            .collect::<Vec<_>>()
            .join(", ");

        write!(
            f,
            "_internal_.{} {{ ({:?}), {} }}",
            self.type_idx, self.closure_type, vars
        )
    }
}

impl<'src> CompilerType<'src> {
    pub fn new(
        type_idx: usize,
        closure_type: ClosureType<'src>,
        free_vars: HashMap<FreeVarKey<'src>, FreeVarValue>,
    ) -> Self {
        CompilerType {
            type_idx,
            closure_type,
            free_vars,
        }
    }

    pub fn len(&self) -> usize {
        self.free_vars.len()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FreeVarKey<'src> {
    pub name: &'src str,
    pub ty: Type<'src>,
}

impl<'src> FreeVarKey<'src> {
    pub fn new(name: &'src str, ty: Type<'src>) -> Self {
        FreeVarKey { name, ty }
    }
}

impl<'src> fmt::Display for FreeVarKey<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FreeVarValue {
    // used to keep the order intact inside the hashset
    pub index: usize,
    pub scope: usize,
}

impl FreeVarValue {
    pub fn new(index: usize, scope: usize) -> Self {
        FreeVarValue { index, scope }
    }
}

#[derive(Debug)]
struct ResolveClosureCtx<'src> {
    scope_start: usize,
    is_inner: bool,
    free_vars: HashMap<FreeVarKey<'src>, FreeVarValue>,
}

impl<'src> ResolveClosureCtx<'src> {
    pub fn new(scope_start: usize, is_inner: bool) -> Self {
        ResolveClosureCtx {
            scope_start,
            is_inner,
            free_vars: HashMap::new(),
        }
    }
}

pub struct ResolveResult<'src> {
    pub symbols: SymbolTable<'src>,
    pub mod_user_types: ModTypeMap<'src>,
    pub mod_functions: ModFuncMap<'src>,
    pub mod_closures: ModClosureMap<'src>,
    pub mod_compiler_types: ModCompilerTypeMap<'src>,
}

pub(crate) struct Resolver<'src, 'ast> {
    current_name: &'src str,
    programs: &'ast PrgMap<'src>,
    resolved: HashSet<&'src str>,
    pub(crate) sym_table: SymbolTable<'src>,
    // TODO: refactor into one map
    mod_functions: ModFuncMap<'src>,
    mod_user_types: ModTypeMap<'src>,
    mod_closures: ModClosureMap<'src>,
    mod_compiler_types: ModCompilerTypeMap<'src>,
    current_func_def: FunctionDefinition<'src>,
}

impl<'src, 'ast> Resolver<'src, 'ast> {
    pub fn new(main_file: &'src str, programs: &'ast PrgMap<'src>) -> Self {
        let mut resolved = HashSet::new();
        resolved.insert(main_file);

        Resolver {
            current_name: main_file,
            programs,
            resolved,
            sym_table: SymbolTable::new(),
            mod_functions: HashMap::new(),
            mod_user_types: HashMap::new(),
            mod_closures: HashMap::new(),
            mod_compiler_types: HashMap::new(),
            current_func_def: FunctionDefinition::default(),
        }
    }

    pub fn get_result(self) -> ResolveResult<'src> {
        ResolveResult {
            symbols: self.sym_table,
            mod_user_types: self.mod_user_types,
            mod_functions: self.mod_functions,
            mod_closures: self.mod_closures,
            mod_compiler_types: self.mod_compiler_types,
        }
    }
}

impl<'src, 'ast> Resolver<'src, 'ast> {
    pub fn resolve(&mut self) -> Vec<ResolveError<'src>> {
        let mut errors = vec![];
        self.resolve_prg(&mut errors);

        // TODO: check for recursive type defs
        // Check if every field of a user defined type in every struct is defined
        for (_, mod_user_types) in self.mod_user_types.iter() {
            for (_, type_def) in mod_user_types.iter() {
                for (_, (_, ty)) in type_def.fields.iter() {
                    self.check_user_type_defined(ty, &mut errors);
                }
            }
        }

        for (_, mod_functions) in self.mod_functions.iter() {
            for (_, func_def) in mod_functions.iter() {
                self.check_user_type_defined(&func_def.ret_type, &mut errors);
                for p_ty in func_def.params.iter() {
                    self.check_user_type_defined(p_ty, &mut errors);
                }
            }
        }

        errors
    }

    fn resolve_prg(&mut self, errors: &mut Vec<ResolveError<'src>>) {
        let name = self.current_name;

        self.mod_user_types.insert(name, HashMap::new());
        self.mod_functions.insert(name, HashMap::new());

        let (_, prg) = self.programs.get(name).unwrap();

        for top_lvl in prg.0.iter() {
            self.declare_top_lvl(&top_lvl, errors);
        }

        for top_lvl in prg.0.iter() {
            self.resolve_top_lvl(top_lvl, errors);
        }
    }

    fn check_user_type_defined(
        &self,
        ty: &Spanned<Type<'src>>,
        errors: &mut Vec<ResolveError<'src>>,
    ) {
        if let Simple::UserType(type_name) = ty.node.simple() {
            let module = type_name.module();
            if !self.mod_user_types.contains_key(module) {
                let err = self.not_defined_error(ty.span, ty.span, type_name.name());
                errors.push(err);
            }
        }
    }

    fn declare_top_lvl(&mut self, top_lvl: &TopLvl<'src>, errors: &mut Vec<ResolveError<'src>>) {
        match top_lvl {
            TopLvl::FuncDecl {
                name,
                ret_type,
                params,
                ..
            } => {
                let mod_name = self.current_name;

                if self.mod_functions[mod_name].contains_key(&name.node) {
                    // TODO: replace with proper error
                    panic!("Duplicate function '{}'", name.node);
                }

                let varargs = params.varargs;

                let func_params = params
                    .params
                    .iter()
                    .map(|Param(_, ty)| ty.clone())
                    .collect();

                let func_def = FunctionDefinition {
                    name: name.node,
                    ret_type: ret_type.clone(),
                    params: func_params,
                    varargs,
                };

                // TODO: error for invalid module
                self.mod_functions
                    .get_mut(mod_name)
                    .unwrap()
                    .insert(name.node, func_def);
            }
            TopLvl::Import { name, .. } => {
                if name.node == self.current_name {
                    errors.push(ResolveError {
                        source: self.current_source(),
                        error: ResolveErrorType::SelfImport(SelfImportError),
                        err_span: name.span,
                        expr_span: name.span,
                    });
                } else if !self.resolved.contains(name.node) {
                    if self.programs.contains_key(name.node) {
                        let current_name = self.current_name;
                        self.current_name = name.node;
                        self.resolved.insert(name.node);
                        self.resolve_prg(errors);
                        self.current_name = current_name;
                    } else {
                        errors.push(self.not_defined_error(name.span, name.span, name.node));
                    }
                }
            }
            TopLvl::TypeDef(TypeDef::StructDef { name, fields }) => {
                // TODO: check for duplicate typedefs
                let mod_name = self.current_name;

                let fields = fields
                    .iter()
                    .enumerate()
                    .map(|(i, (Spanned { node, .. }, ty))| (*node, (i as u32, ty.clone())))
                    .collect();

                let def = UserTypeDefinition {
                    name: name.node,
                    fields,
                };

                // TODO: error for invalid module
                self.mod_user_types
                    .get_mut(mod_name)
                    .unwrap()
                    .insert(name.node, def);
            }
            TopLvl::Error(err) => {
                panic!("Invalid top level declaration {:#?}\n{}", top_lvl, err.node)
            }
        }
    }

    fn bind_param(&mut self, p: &Param<'src>) {
        // TODO: refactor
        self.sym_table
            .bind(p.0.node, p.0.span, p.1.node.clone(), true);
    }

    fn resolve_top_lvl(
        &mut self,
        top_lvl: &'ast TopLvl<'src>,
        errors: &mut Vec<ResolveError<'src>>,
    ) {
        if let TopLvl::FuncDecl {
            name,
            params,
            ref body,
            is_extern,
            ..
        } = top_lvl
        {
            let mut closure_ctx = ResolveClosureCtx::new(0, false);
            self.current_func_def = self.mod_functions[self.current_name][name.node].clone();
            self.sym_table.scope_enter();

            for p in params.params.iter() {
                self.bind_param(p);
            }

            if !*is_extern {
                for stmt in body.0.iter() {
                    self.resolve_stmt(stmt, errors, &mut closure_ctx);
                }
            }

            self.sym_table.scope_exit();
        }
    }

    fn resolve_stmt(
        &mut self,
        stmt: &'ast Stmt<'src>,
        errors: &mut Vec<ResolveError<'src>>,
        closure_ctx: &mut ResolveClosureCtx<'src>,
    ) {
        match stmt {
            Stmt::VarDecl(decl) => {
                let VarDecl {
                    name,
                    ref value,
                    eq,
                    ty: var_type,
                } = decl.as_ref();

                let var_type_clone = var_type.borrow().clone();
                let expected = if let Some(ref ty) = var_type_clone {
                    Some(&ty.node)
                } else {
                    None
                };

                match self.resolve_expr(value.span, &value.node, expected, closure_ctx) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => {
                        if let Some(var_type) = var_type_clone {
                            let expr_span = Span::new(name.span.start, value.span.end);
                            // If a type was provided, check if it's one of the builtin types
                            if let Err(err) = self
                                .compare_binary_types(
                                    eq.span,
                                    expr_span,
                                    &eq.node,
                                    &var_type.borrow().node,
                                    &ty,
                                )
                                .map(|_| self.sym_table.bind(name.node, name.span, ty, false))
                                .map_err(
                                    |ResolveError {
                                         error,
                                         err_span,
                                         expr_span,
                                         ..
                                     }| {
                                        self.illegal_op_to_illegal_assignment(
                                            err_span, expr_span, name.node, error, name.span,
                                        )
                                    },
                                )
                            {
                                errors.push(err);
                            }
                        } else {
                            let insert_ty = Some(Spanned::new(0, 0, ty.clone()));
                            var_type.replace(insert_ty);
                            self.sym_table.bind(name.node, name.span, ty, false)
                        }
                    }
                };
            }
            Stmt::If(if_stmt) => {
                let IfStmt {
                    condition,
                    then_block,
                    else_branch,
                } = if_stmt.as_ref();
                match self.resolve_expr(condition.span, &condition.node, None, closure_ctx) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => self.expect_bool(ty, "if condition", condition.span, errors),
                }

                // TODO: refactor to method
                self.sym_table.scope_enter();
                for stmt in &then_block.0 {
                    self.resolve_stmt(&stmt, errors, closure_ctx);
                }
                self.sym_table.scope_exit();

                if let Some(else_branch) = else_branch {
                    match else_branch.as_ref() {
                        Else::IfStmt(s) => {
                            if let Stmt::If { .. } = s.as_ref() {
                                self.resolve_stmt(s, errors, closure_ctx);
                            } else {
                                panic!("Only if statement allowed here");
                            }
                        }
                        Else::Block(b) => {
                            self.sym_table.scope_enter();
                            for stmt in &b.0 {
                                self.resolve_stmt(&stmt, errors, closure_ctx);
                            }
                            self.sym_table.scope_exit();
                        }
                    }
                }
            }
            Stmt::While { condition, body } => {
                match self.resolve_expr(condition.span, &condition.node, None, closure_ctx) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => self.expect_bool(ty, "while condition", condition.span, errors),
                }

                // TODO: refactor to method
                self.sym_table.scope_enter();
                for stmt in &body.0 {
                    self.resolve_stmt(&stmt, errors, closure_ctx);
                }
                self.sym_table.scope_exit();
            }
            Stmt::Return(expr) => {
                let return_type = self.current_func_def.ret_type.node.clone();

                if let Some(expr) = expr {
                    if return_type == Type::Simple(Simple::Void) {
                        // TODO Error for return in void
                        unimplemented!("Error for return in void");
                    }

                    let resolved =
                        self.resolve_expr(expr.span, &expr.node, Some(&return_type), closure_ctx);

                    if let Err(msg) = resolved {
                        errors.push(msg);
                    } else if let Ok(res) = resolved {
                        if let Err(err) = self.compare_types(
                            expr.span,
                            expr.span,
                            &return_type,
                            &res,
                            "return value",
                        ) {
                            errors.push(err);
                        }
                    } else {
                        // create an error, because the type could not be inferred, but the
                        // function also does not return a pointer, so null cannot be casted
                        let err = self.type_inference_error(expr.span);
                        errors.push(err);
                    }
                } else if return_type != Type::Simple(Simple::Void) {
                    // TODO Error for no return in non void
                    unimplemented!("Error for no return in non void")
                }
            }
            Stmt::Delete(expr) => {
                let ty = self.resolve_expr(expr.span, &expr.node, None, closure_ctx);
                if let Err(msg) = ty {
                    errors.push(msg);
                } else if let Ok(ty) = ty {
                    if !ty.is_ptr() {
                        let err = self.error(
                            expr.span,
                            expr.span,
                            ResolveErrorType::Delete(NonPtrError(ty)),
                        );
                        errors.push(err);
                    }
                }
            }
            Stmt::Expr(ref expr) => {
                if let Err(msg) = self.resolve_expr(expr.span, &expr.node, None, closure_ctx) {
                    errors.push(msg);
                }
            }
        };
    }

    fn resolve_expr(
        &mut self,
        span: Span,
        expr: &'ast Expr<'src>,
        expected: Option<&Type<'src>>,
        closure_ctx: &mut ResolveClosureCtx<'src>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        let mut queue = expr.sub_exprs();
        let mut exprs = Vec::with_capacity(queue.len());

        // fill with all sub-sub... expressions
        // basically a BFS
        while !queue.is_empty() {
            let e = queue.pop().unwrap();
            let mut subs = e.node.sub_exprs();
            exprs.push(e);
            queue.append(&mut subs);
        }

        // check child expressions
        for Spanned { node: expr, span } in exprs.iter().rev() {
            let opt_ty = self.check_expr(*span, expr, closure_ctx)?;
            // if the type is there already, fill it in
            if let Some(ty) = opt_ty {
                expr.set_ty(ty);
            }
        }

        // check actual expression
        let opt_ty = self.check_expr(span, expr, closure_ctx)?;
        let ty = self.ty_unwrap(span, &opt_ty, expected)?;
        // Insert type information
        expr.set_ty(ty.clone());
        Ok(ty)
    }

    fn check_expr(
        &mut self,
        span: Span,
        expr: &'ast Expr<'src>,
        closure_ctx: &mut ResolveClosureCtx<'src>,
    ) -> Result<Option<Type<'src>>, ResolveError<'src>> {
        match expr.kind() {
            ExprKind::Error(_) => {
                unreachable!("If errors occur during parsing, the program should not be resolved")
            }
            ExprKind::NullLit => Ok(None),
            ExprKind::DecLit(_) => Ok(Some(Type::Simple(Simple::I32))),
            ExprKind::FloatLit(_) => Ok(Some(Type::Simple(Simple::F32))),
            ExprKind::SizeOf(_) => Ok(Some(Type::Simple(Simple::I32))),
            ExprKind::StringLit(_) => Ok(Some(Type::Simple(Simple::String))),
            ExprKind::Ident(name) => self
                .handle_ident(span, name, closure_ctx)
                .map(|t| Some(t.node.clone())),

            ExprKind::New(expr) => {
                let ty = self.resolve_type(expr, None)?;

                if let Type::Simple(ty) = ty {
                    Ok(Some(Type::Pointer(Pointer::new(1, ty))))
                } else {
                    // TODO: Implement proper error handling
                    panic!("Implement proper error handling");
                }
            }
            ExprKind::Negate(op, expr) => {
                let ty = self.resolve_type(expr, None)?;
                // TODO: unary operation error
                Some(self.compare_binary_types(
                    op.span,
                    span,
                    &op.node,
                    &ty,
                    &Type::Simple(Simple::I32),
                ))
                .transpose()
            }
            ExprKind::Ref(_, expr) => {
                if expr.node.is_r_value() {
                    unimplemented!("TODO: add error for &rvalue")
                }

                let ty = self.resolve_type(expr, None)?;
                Ok(Some(match ty {
                    Type::Pointer(mut ptr) => {
                        ptr.number += 1;
                        Type::Pointer(ptr)
                    }
                    Type::Simple(s) => Type::Pointer(Pointer::new(1, s)),
                }))
            }
            ExprKind::Deref(op, expr) => {
                let ty = self.resolve_type(expr, None)?;
                if let Type::Pointer(mut ptr) = ty {
                    Ok(Some(if ptr.number > 1 {
                        ptr.number -= 1;
                        Type::Pointer(ptr)
                    } else {
                        Type::Simple(ptr.ty)
                    }))
                } else {
                    Err(self.error(op.span, span, ResolveErrorType::Deref(NonPtrError(ty))))
                }
            }
            ExprKind::Binary(l, op, r) => {
                let left = self.resolve_type(l, None)?;
                let right = self.resolve_type(r, None)?;

                let wrong_type = Self::check_type_predicate(&left, &right, Type::arithmetic);

                if let Some(wrong) = wrong_type {
                    return Err(self.error(
                        op.span,
                        span,
                        ResolveErrorType::NotArithmetic(ArithmeticError::new(
                            wrong.clone(),
                            op.node.clone(),
                        )),
                    ));
                }

                Some(self.compare_binary_types(op.span, span, &op.node, &left, &right)).transpose()
            }
            ExprKind::BoolBinary(l, op, r) => {
                let left = self.resolve_type(l, None);
                let right = self.resolve_type(r, None);

                let (left, right) = if let Err(ResolveError {
                    error: ResolveErrorType::Inference(TypeInferenceError),
                    ..
                }) = left
                {
                    // if left could not be resolved, but right could, try to resolve left again,
                    // but with the expected type of right
                    let right = right?;
                    let left = self.resolve_type(l, Some(&right))?;
                    (left, right)
                } else if let Err(ResolveError {
                    error: ResolveErrorType::Inference(TypeInferenceError),
                    ..
                }) = right
                {
                    // the same as above, but inverted
                    let left = left?;
                    let right = self.resolve_type(r, Some(&left))?;
                    (left, right)
                } else {
                    (left?, right?)
                };

                self.compare_binary_types(op.span, span, &op.node, &left, &right)?;
                Ok(Some(Type::Simple(Simple::Bool)))
            }
            // currently only field access
            ExprKind::Access { left, identifier } => {
                let left_ty = self.resolve_type(left, None)?;
                match left_ty {
                    // if the type is either a struct or a pointer to (pointer to ...) a struct
                    Type::Simple(Simple::UserType(type_name))
                    | Type::Pointer(Pointer {
                        ty: Simple::UserType(type_name),
                        ..
                    }) => {
                        let user_type = self.get_user_type(&Spanned::from_span(span, type_name))?;
                        let field_type = self.get_field(&user_type, identifier)?;
                        Ok(Some(field_type))
                    }
                    _ => {
                        // TODO: replace with custom error
                        panic!("Cannot access field of primitive type: {:?}", left_ty);
                    }
                }
            }
            ExprKind::StructInit { identifier, fields } => {
                for (name, value) in fields.0.iter() {
                    let user_type = self.get_user_type(identifier)?;
                    let field_type = self.get_field(&user_type, name)?;

                    let val_type = self.resolve_type(value, Some(&field_type))?;
                    self.compare_types(value.span, span, &field_type, &val_type, "struct literal")?
                }
                Ok(Some(Type::Simple(Simple::UserType(identifier.node))))
            }
            ExprKind::Assign { left, eq, value } => {
                if let ExprKind::Ident(name) = left.node.kind() {
                    // Lookup variable in defined scopes
                    let Spanned {
                        span: sym_span,
                        node: ty,
                    } = self.handle_ident(span, name, closure_ctx)?;

                    // get type of right expression
                    let val_type = self.resolve_type(value, Some(&ty))?;
                    // check if type of right expression is the same as that of
                    // the declared variable
                    Some(
                        self.compare_assignment(eq.span, span, ty.clone(), val_type)
                            .map_err(
                                // convert error into illegal assignment error
                                |ResolveError {
                                     error,
                                     err_span,
                                     expr_span,
                                     ..
                                 }| {
                                    self.illegal_op_to_illegal_assignment(
                                        err_span, expr_span, name, error, sym_span,
                                    )
                                },
                            ),
                    )
                    .transpose()
                } else {
                    let ty = self.resolve_type(left, None)?;

                    // get type of right expression
                    // the type of left is the expected type for the right expr
                    let val_type = self.resolve_type(value, Some(&ty))?;

                    Some(self.compare_assignment(eq.span, span, ty, val_type)).transpose()
                }
            }
            ExprKind::Call {
                callee, // can never be ExprKind::Access
                args,
                module,
            } => {
                let func = self.get_function(module, callee);

                match func {
                    // normal function call
                    Some(Ok(func_type)) => self.call_function(func_type, args, span),
                    // callee is ident, but not in top level functions
                    Some(_) => {
                        let cls_ty = self.get_closure(callee, closure_ctx)?;
                        if let Type::Simple(Simple::CompilerType(module, type_idx)) = cls_ty.node {
                            callee.node.set_ty(cls_ty.node.clone());

                            let comp_type = &self.mod_compiler_types[module][*type_idx];
                            let ct = &comp_type.closure_type;
                            self.call_closure(ct, args, span)?;
                            Ok(Some(ct.ret_ty.clone()))
                        } else if let Type::Simple(Simple::Function(closure)) = cls_ty.node {
                            callee.node.set_ty(cls_ty.node.clone());

                            self.call_closure(closure, args, span)?;
                            Ok(Some(closure.ret_ty.clone()))
                        } else {
                            Err(self.call_non_function_error(
                                callee.span,
                                span,
                                cls_ty.node.clone(),
                            ))
                        }
                    }
                    // callee is an arbitrary expression
                    None => {
                        let cls_ty = self.resolve_type(callee, None)?;
                        if let Type::Simple(Simple::CompilerType(module, type_idx)) = cls_ty {
                            callee.node.set_ty(cls_ty.clone());

                            let comp_type = &self.mod_compiler_types[module][type_idx];
                            let ct = &comp_type.closure_type;
                            self.call_closure(ct, args, span)?;
                            Ok(Some(ct.ret_ty.clone()))
                        } else if let Type::Simple(Simple::Function(ref closure)) = cls_ty {
                            callee.node.set_ty(cls_ty.clone());

                            self.call_closure(closure, args, span)?;
                            Ok(Some(closure.ret_ty.clone()))
                        } else {
                            Err(self.call_non_function_error(callee.span, span, cls_ty.clone()))
                        }
                    }
                }
            }
            ExprKind::Closure(params, body) => match body.as_ref() {
                ClosureBody::Expr(e) => {
                    let mut errors = Vec::new();
                    let mut param_types = Vec::with_capacity(params.params.len());
                    self.sym_table.scope_enter();

                    for p in params.params.iter() {
                        self.check_user_type_defined(&p.1, &mut errors);
                        if !errors.is_empty() {
                            return Err(errors.swap_remove(0));
                        }
                        self.bind_param(p);
                        param_types.push(p.1.node.clone());
                    }

                    let mut cls_ctx = ResolveClosureCtx::new(self.sym_table.num_scopes() - 1, true);
                    let ret_ty = self.resolve_expr(e.span, &e.node, None, &mut cls_ctx)?;

                    self.sym_table.scope_exit();

                    let current_scope = self.sym_table.num_scopes();
                    let current_module = self.current_name;

                    // vars of cls_ctx that are not free variables of outer scope
                    let diff = cls_ctx
                        .free_vars
                        .iter()
                        .map(|(_, v)| v.scope)
                        .filter(|s| *s >= current_scope)
                        .count();

                    // merge envs
                    for (k, mut v) in cls_ctx.free_vars {
                        if v.scope < current_scope {
                            v.index -= diff;
                            closure_ctx.free_vars.insert(k, v);
                        }
                    }

                    let free_vars = closure_ctx.free_vars.clone();

                    // create closure definition
                    let cls_type = ClosureType::new(param_types, ret_ty);
                    let cls_defs = self
                        .mod_closures
                        .entry(current_module)
                        .or_insert_with(ClosureDefinitions::default);
                    cls_defs.push(cls_type.clone());

                    // create type for environment
                    let comp_types = self
                        .mod_compiler_types
                        .entry(current_module)
                        .or_insert_with(Vec::new);
                    let type_idx = comp_types.len();
                    comp_types.push(CompilerType::new(type_idx, cls_type, free_vars));

                    let ty = Type::Simple(Simple::CompilerType(current_module, type_idx));
                    Ok(Some(ty))
                }
                ClosureBody::Block(..) => {
                    unimplemented!("TODO: implement block closure type res (remember to set type)")
                }
            },
        }
    }
}

impl<'src, 'ast> Resolver<'src, 'ast> {
    fn call_closure(
        &self,
        cls_type: &ClosureType<'src>,
        args: &ArgList<'src>,
        expr_span: Span,
    ) -> Result<(), ResolveError<'src>> {
        // resolve arguments
        let mut arg_types: Vec<(Span, Type)> = Vec::with_capacity(args.0.len());
        if cls_type.params.len() != args.0.len() {
            // TODO: emit custom error
            panic!(
                "Expected {} arguments, but got {}!",
                cls_type.params.len(),
                args.0.len()
            );
        }

        let iter = args.0.iter().zip(cls_type.params.iter());

        for (arg, param_type) in iter {
            let ty = self.resolve_type(arg, Some(&param_type))?;
            arg_types.push((arg.span, ty));
        }

        let arg_error = cls_type
            .params
            .iter()
            .zip(arg_types.iter())
            .filter_map(|(p, (arg_span, a))| {
                // compare arguments to expected parameters
                if let Err(err) = self.compare_types(*arg_span, expr_span, &p, a, "argument") {
                    return Some(err);
                }
                None
            })
            // only evaluate the first argument (this should probably be changed)
            .take(1)
            .next();

        if let Some(err) = arg_error {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn call_function(
        &self,
        func_type: &FunctionDefinition<'src>,
        args: &ArgList<'src>,
        expr_span: Span,
    ) -> Result<Option<Type<'src>>, ResolveError<'src>> {
        let varargs = func_type.varargs;
        let no_vararg_passed = func_type.params.len() == args.0.len() + 1;

        // don't check number of arguments for variadic functions
        if !varargs && func_type.params.len() != args.0.len() {
            // TODO: emit custom error
            panic!(
                "Expected {} arguments, but got {}!",
                func_type.params.len(),
                args.0.len()
            );
        }

        // resolve arguments
        let mut arg_types: Vec<(Span, Type)> = Vec::with_capacity(args.0.len());
        let mut params: Vec<Spanned<Type<'src>>>;

        let iter = if !varargs {
            args.0.iter().zip(func_type.params.iter())
        } else if no_vararg_passed {
            // the function is varargs, but no value was passed for the variadic parameter,
            // therefore skip last param
            params = func_type
                .params
                .iter()
                .take(func_type.params.len() - 1)
                .cloned()
                .collect();

            args.0.iter().zip(params.iter())
        } else {
            // if the function is varargs, the number of arguments does not correspond to
            // the number of params, so simply zipping them would cut of some args.
            // Thats why the difference between params and args is filled with void
            // pointers
            let mut type_params = func_type.params.clone();
            params = Vec::with_capacity(args.0.len());
            let diff = args.0.len() - type_params.len();

            let varargs_span = func_type.params.last().unwrap().span;

            params.append(&mut type_params);
            for _ in 0..diff {
                // fill difference with void pointers
                params.push(Spanned::from_span(
                    varargs_span,
                    Type::Pointer(Pointer::new(1, Simple::Void)),
                ));
            }

            args.0.iter().zip(params.iter())
        };

        for (arg, param_type) in iter {
            let ty = self.resolve_type(arg, Some(&param_type.node))?;
            arg_types.push((arg.span, ty));
        }

        let arg_error = func_type
            .params
            .iter()
            .zip(arg_types.iter())
            .filter_map(|(p, (arg_span, a))| {
                // compare arguments to expected parameters
                if let Err(err) = self.compare_types(*arg_span, expr_span, &p.node, a, "argument") {
                    return Some(err);
                }
                None
            })
            // only evaluate the first argument (this should probably be changed)
            .take(1)
            .next();

        if let Some(err) = arg_error {
            Err(err)
        } else {
            Ok(Some(func_type.ret_type.node.clone()))
        }
    }
}

impl<'src, 'ast> Resolver<'src, 'ast> {
    fn current_source(&self) -> &'src Source {
        let (src, _) = self.programs[self.current_name];
        src
    }

    fn handle_ident(
        &self,
        span: Span,
        name: &'src str,
        ctx: &mut ResolveClosureCtx<'src>,
    ) -> Result<Spanned<&Type<'src>>, ResolveError<'src>> {
        self.sym_table
            .lookup(name)
            .map(|(sym, i)| {
                if i < ctx.scope_start {
                    let idx = ctx.free_vars.len(); // is ignored for cmp
                    let key = FreeVarKey::new(name, sym.node.ty.clone());
                    let value = FreeVarValue::new(idx, i);
                    ctx.free_vars.insert(key, value);
                }

                sym
            })
            .ok_or_else(|| self.not_defined_error(span, span, name))
            .map(|sym| Spanned::from_span(sym.span, &sym.node.ty))
    }

    fn check_type_predicate<'a, F>(
        first: &'a Type<'src>,
        second: &'a Type<'src>,
        pred: F,
    ) -> Option<&'a Type<'src>>
    where
        F: Fn(&Type<'src>) -> bool,
    {
        if !pred(&first) {
            Some(first)
        } else if !pred(&second) {
            Some(second)
        } else {
            None
        }
    }

    fn ty_unwrap(
        &self,
        span: Span,
        ty: &Option<Type<'src>>,
        expected: Option<&Type<'src>>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        if let Some(ty) = ty {
            return Ok(ty.clone());
        } else if let Some(expected) = expected {
            // the ty is null, but since a pointer is expected, we can "cast" it
            if let Type::Pointer(_) = expected {
                return Ok(expected.clone());
            }
        }
        Err(self.type_inference_error(span))
    }

    fn resolve_type(
        &self,
        expr: &Spanned<Expr<'src>>,
        expected: Option<&Type<'src>>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        let ty = self.ty_unwrap(expr.span, &expr.node.ty(), expected);
        if let Ok(ref ty) = ty {
            expr.node.set_ty(ty.clone());
        }
        ty
    }

    fn type_inference_error(&self, span: Span) -> ResolveError<'src> {
        self.error(span, span, ResolveErrorType::Inference(TypeInferenceError))
    }

    fn expect_bool(
        &self,
        ty: Type<'src>,
        name: &'static str,
        span: Span,
        errors: &mut Vec<ResolveError<'src>>,
    ) {
        if ty != Type::Simple(Simple::Bool) {
            errors.push(self.type_error(span, span, name, Type::Simple(Simple::Bool), ty))
        }
    }

    fn get_closure(
        &self,
        callee: &Spanned<Expr<'src>>,
        closure_ctx: &mut ResolveClosureCtx<'src>,
    ) -> Result<Spanned<&Type<'src>>, ResolveError<'src>> {
        if let ExprKind::Ident(ident) = callee.node.kind() {
            return self.handle_ident(callee.span, ident, closure_ctx);
        }

        unreachable!("callee should always be an ident if this is called")
    }

    fn get_function(
        &self,
        module: &'src str,
        ident: &Spanned<Expr<'src>>,
    ) -> Option<Result<&FunctionDefinition<'src>, ResolveError<'src>>> {
        if let ExprKind::Ident(name) = ident.node.kind() {
            let res = self
                .mod_functions
                .get(module)
                .and_then(|funcs| funcs.get(name))
                .ok_or_else(|| self.not_defined_error(ident.span, ident.span, name));
            Some(res)
        } else {
            None
        }
    }

    fn get_user_type(
        &self,
        ident: &Spanned<UserIdent<'src>>,
    ) -> Result<&UserTypeDefinition<'src>, ResolveError<'src>> {
        self.mod_user_types
            .get(ident.node.module())
            .and_then(|types| types.get(ident.node.name()))
            .ok_or_else(|| self.not_defined_error(ident.span, ident.span, ident.node.name()))
    }

    fn get_field(
        &self,
        user_type: &UserTypeDefinition<'src>,
        name: &Spanned<&'src str>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        user_type
            .fields
            .get(name.node)
            .ok_or_else(|| self.no_such_field_error(user_type, name))
            .map(|ty| ty.1.node.clone())
    }

    fn error(
        &self,
        err_span: Span,
        expr_span: Span,
        err: ResolveErrorType<'src>,
    ) -> ResolveError<'src> {
        ResolveError {
            source: self.current_source(),
            error: err,
            err_span,
            expr_span,
        }
    }

    fn no_such_field_error(
        &self,
        user_type: &UserTypeDefinition<'src>,
        name: &Spanned<&'src str>,
    ) -> ResolveError<'src> {
        self.error(
            name.span,
            name.span,
            ResolveErrorType::NoSuchField(StructFieldError {
                struct_name: user_type.name,
                field_name: name.node,
            }),
        )
    }

    fn illegal_op_to_illegal_assignment(
        &self,
        err_span: Span,
        expr_span: Span,
        name: &'src str,
        error: ResolveErrorType<'src>,
        def_span: Span,
    ) -> ResolveError<'src> {
        if let ResolveErrorType::IllegalOperation(err) = error {
            self.error(
                err_span,
                expr_span,
                ResolveErrorType::IllegalAssignment(Box::new(AssignmentError {
                    name,
                    definition_span: def_span,
                    bin_op_err: err,
                })),
            )
        } else {
            panic!("Invalid Error Type");
        }
    }

    fn call_non_function_error(
        &self,
        err_span: Span,
        expr_span: Span,
        actual_type: Type<'src>,
    ) -> ResolveError<'src> {
        self.error(
            err_span,
            expr_span,
            ResolveErrorType::CallNonFunction(NonFunctionError(actual_type)),
        )
    }

    fn type_error(
        &self,
        err_span: Span,
        expr_span: Span,
        name: &'static str,
        expected_type: Type<'src>,
        actual_type: Type<'src>,
    ) -> ResolveError<'src> {
        self.error(
            err_span,
            expr_span,
            ResolveErrorType::IllegalType(IllegalTypeError {
                expected_type,
                actual_type,
                name,
            }),
        )
    }

    fn not_defined_error(
        &self,
        err_span: Span,
        expr_span: Span,
        name: &'src str,
    ) -> ResolveError<'src> {
        self.error(
            err_span,
            expr_span,
            ResolveErrorType::NotDefined(DefinitionError { name }),
        )
    }

    fn compare_types(
        &self,
        err_span: Span,
        expr_span: Span,
        expected: &Type<'src>,
        actual: &Type<'src>,
        name: &'static str,
    ) -> Result<(), ResolveError<'src>> {
        if expected != actual
            // varargs disables type checking
            && *expected != Type::Simple(Simple::Varargs)
            && *actual != Type::Simple(Simple::Varargs)
        {
            return Err(self.error(
                err_span,
                expr_span,
                ResolveErrorType::IllegalType(IllegalTypeError {
                    expected_type: expected.clone(),
                    actual_type: actual.clone(),
                    name,
                }),
            ));
        }

        Ok(())
    }

    fn compare_assignment(
        &self,
        err_span: Span,
        expr_span: Span,
        first: Type<'src>,
        second: Type<'src>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        if first != second {
            Err(self.error(
                err_span,
                expr_span,
                ResolveErrorType::IllegalOperation(BinaryOperationError {
                    left_type: first,
                    right_type: second,
                }),
            ))
        } else {
            Ok(first)
        }
    }

    fn allowed_binary<'a>(
        op: &Token<'src>,
        first: &'a Type<'src>,
        second: &'a Type<'src>,
    ) -> (bool, &'a Type<'src>) {
        // TODO: better error message here (probably best, to just return a result)
        if first.is_closure() || second.is_closure() {
            return (false, first);
        }

        if first == second {
            let prec = op.precedence();
            // Hacky solution, because we want to allow ==, <, !=, ... for pointers,
            // but not +, *, ... . So we use the precedence, to determine, if
            // this is an arithmetic expression
            if prec == Precedence::Sum || prec == Precedence::Product {
                return (first.is_num() && second.is_num(), first);
            }
            return (true, first);
        }

        let first_ptr = first.is_ptr();
        let second_int = second.is_int();

        if let Token::Plus = op {
            // return the pointer type as the expression type
            if first_ptr && second_int {
                return (true, first);
            } else if first.is_int() && second.is_ptr() {
                return (true, second);
            }
        } else if let Token::Minus = op {
            // int - ptr is not allowed
            return (first_ptr && second_int, first);
        }

        (false, first)
    }

    fn compare_binary_types<'a>(
        &self,
        err_span: Span,
        expr_span: Span,
        op: &Token<'src>,
        first: &'a Type<'src>,
        second: &'a Type<'src>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        let (allowed, ty) = Self::allowed_binary(op, first, second);

        if !allowed {
            Err(self.error(
                err_span,
                expr_span,
                ResolveErrorType::IllegalOperation(BinaryOperationError {
                    left_type: first.clone(),
                    right_type: second.clone(),
                }),
            ))
        } else {
            Ok(ty.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::token::Token;
    use std::cell::RefCell;

    #[test]
    fn test_resolve_should_find_imported_fn() {
        let main_src = Source::new("main", "import test\nfn main(): void {test.func()}");
        let test_src = Source::new("test", "fn func(): void {}");

        let main_ast = Program(vec![
            TopLvl::Import {
                name: Spanned::new(7, 10, "test"),
            },
            TopLvl::FuncDecl {
                name: Spanned::new(16, 19, "main"),
                ret_type: Spanned::new(24, 27, Type::Simple(Simple::Void)),
                params: ParamList::default(),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    32,
                    34,
                    Expr::new(ExprKind::Call {
                        module: "test",
                        callee: Box::new(Spanned::new(29, 37, Expr::new(ExprKind::Ident("func")))),
                        args: ArgList(vec![]),
                    }),
                ))]),
            },
        ]);

        let test_ast = Program(vec![TopLvl::FuncDecl {
            name: Spanned::new(3, 6, "func"),
            ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
            is_extern: false,
            params: ParamList::default(),
            body: Block(vec![]),
        }]);

        let mut map = HashMap::new();
        map.insert("main", (&main_src, main_ast));
        map.insert("test", (&test_src, test_ast));

        let mut resolver = Resolver::new("main", &map);
        let errors = resolver.resolve();

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);
    }

    #[test]
    fn test_functions_are_found_without_forward_decl() {
        let source = Source::new("test", "fn main(): void { test(); } fn test(): void {}");

        let ast = Program(vec![
            TopLvl::FuncDecl {
                name: Spanned::new(3, 6, "main"),
                ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
                params: ParamList::default(),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    18,
                    23,
                    Expr::new(ExprKind::Call {
                        module: "test",
                        callee: Box::new(Spanned::new(18, 21, Expr::new(ExprKind::Ident("test")))),
                        args: ArgList(vec![]),
                    }),
                ))]),
            },
            TopLvl::FuncDecl {
                name: Spanned::new(25, 28, "test"),
                ret_type: Spanned::new(39, 42, Type::Simple(Simple::Void)),
                params: ParamList::default(),
                is_extern: false,
                body: Block(vec![]),
            },
        ]);

        let mut map = HashMap::new();
        map.insert("test", (&source, ast));

        let mut resolver = Resolver::new("test", &map);
        let errors = resolver.resolve();

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);
    }

    #[test]
    fn test_resolve_without_errors_should_return_empty_vec() {
        let source = Source::new("test", "fn main(): void { let x = 10; }");

        let ast = Program(vec![TopLvl::FuncDecl {
            name: Spanned::new(3, 6, "main"),
            ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
            params: ParamList::default(),
            is_extern: false,
            body: Block(vec![Stmt::VarDecl(Box::new(VarDecl {
                name: Spanned::new(22, 22, "x"),
                value: Spanned::new(26, 28, Expr::new(ExprKind::DecLit("10"))),
                eq: Spanned::new(24, 24, Token::Equals),
                ty: RefCell::new(None),
            }))]),
        }]);

        let mut map = HashMap::new();
        map.insert("test", (&source, ast));

        let mut resolver = Resolver::new("test", &map);
        let errors = resolver.resolve();

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);

        if let TopLvl::FuncDecl { body, .. } = &(&map["test"].1).0[0] {
            if let Stmt::VarDecl(decl) = &body.0[0] {
                assert_eq!(
                    decl.value.node.ty().clone(),
                    Some(Type::Simple(Simple::I32))
                );
                return;
            }
        }

        panic!("Type not inserted");
    }

    #[test]
    fn test_resolve_with_assign_error_should_return_correct_error() {
        let source = Source::new("test", r#"fn main(): void { let x = 10; x = ""; }"#);

        let ast = Program(vec![TopLvl::FuncDecl {
            name: Spanned::new(3, 6, "main"),
            ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
            params: ParamList::default(),
            is_extern: false,
            body: Block(vec![
                Stmt::VarDecl(Box::new(VarDecl {
                    name: Spanned::new(22, 22, "x"),
                    value: Spanned::new(26, 28, Expr::new(ExprKind::DecLit("10"))),
                    eq: Spanned::new(24, 24, Token::Equals),
                    ty: RefCell::new(None),
                })),
                Stmt::Expr(Spanned::new(
                    30,
                    35,
                    Expr::new(ExprKind::Assign {
                        left: Box::new(Spanned::new(30, 30, Expr::new(ExprKind::Ident("x")))),
                        eq: Spanned::new(32, 32, Token::Equals),
                        value: Box::new(Spanned::new(35, 35, Expr::new(ExprKind::StringLit("")))),
                    }),
                )),
            ]),
        }]);

        let mut map = HashMap::new();
        map.insert("test", (&source, ast));

        let mut resolver = Resolver::new("test", &map);
        let errors = resolver.resolve();

        assert_eq!(1, errors.len());

        if let [err] = errors.as_slice() {
            let err = err.to_string();
            assert!(err.contains("operation '=' cannot be applied to 'i32' and 'string'"));
            assert!(err.contains("'x' was defined as 'i32' here"));
        } else {
            panic!("Expected one error");
        }
    }
}
