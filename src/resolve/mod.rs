use std::collections::{HashMap, HashSet};

use crate::{
    parse::{ast::*, Span, Spanned},
    types::*,
    Source, UserTypeDefinition, UserTypeMap,
};

use self::error::*;
use self::symbol::*;

use super::*;

mod error;
#[allow(dead_code)]
pub mod symbol;

#[derive(Debug, Clone)]
struct FunctionDefinition<'src> {
    ret_type: Spanned<Type<'src>>,
    params: Vec<Spanned<Type<'src>>>,
    varargs: bool,
}

pub struct ResolveResult<'src> {
    pub symbols: SymbolTable<'src>,
    pub user_types: UserTypeMap<'src>,
}

pub(crate) struct Resolver<'src, 'ast> {
    current_name: &'src str,
    programs: &'ast PrgMap<'src>,
    resolved: HashSet<&'src str>,
    pub(crate) sym_table: SymbolTable<'src>,
    functions: HashMap<String, FunctionDefinition<'src>>,
    user_types: UserTypeMap<'src>,
    current_func_ret_type: Spanned<Type<'src>>,
}

impl<'src, 'ast> Resolver<'src, 'ast> {
    pub fn new(main_file: &'src str, programs: &'ast PrgMap<'src>) -> Self {
        Resolver {
            current_name: main_file,
            programs,
            resolved: HashSet::new(),
            sym_table: SymbolTable::new(),
            functions: HashMap::new(),
            user_types: HashMap::new(),
            current_func_ret_type: Spanned::new(0, 0, Type::Simple(Simple::Void)),
        }
    }

    pub fn get_result(self) -> ResolveResult<'src> {
        ResolveResult {
            symbols: self.sym_table,
            user_types: self.user_types,
        }
    }
}

impl<'src, 'ast> Resolver<'src, 'ast> {
    pub fn resolve(&mut self) -> Vec<ResolveError<'src>> {
        self.resolve_prg(None)
    }

    fn resolve_prg(&mut self, prefix: Option<&str>) -> Vec<ResolveError<'src>> {
        let name = &self.current_name;
        let (_, prg) = self.programs.get(name).unwrap();
        let mut errors = vec![];

        for top_lvl in &prg.0 {
            self.declare_top_lvl(&top_lvl, &mut errors, prefix);
        }

        // TODO: check for recursive type defs

        // Check if every field of a user defined type in every struct is defined
        for (_, type_def) in self.user_types.iter() {
            for (_, (_, ty)) in type_def.fields.iter() {
                self.check_user_type_defined(ty, &mut errors);
            }
        }

        for (_, func_def) in self.functions.iter() {
            self.check_user_type_defined(&func_def.ret_type, &mut errors);
            for p_ty in func_def.params.iter() {
                self.check_user_type_defined(p_ty, &mut errors);
            }
        }

        for top_lvl in &prg.0 {
            self.resolve_top_lvl(&top_lvl, &mut errors);
        }

        errors
    }

    fn check_user_type_defined(
        &self,
        ty: &Spanned<Type<'src>>,
        errors: &mut Vec<ResolveError<'src>>,
    ) {
        if let Simple::UserType(type_name) = ty.node.simple() {
            if !self.user_types.contains_key(type_name.to_owned()) {
                let err = self.not_defined_error(ty.span, ty.span, type_name);
                errors.push(err);
            }
        }
    }

    fn declare_top_lvl(
        &mut self,
        top_lvl: &TopLvl<'src>,
        errors: &mut Vec<ResolveError<'src>>,
        prefix: Option<&str>,
    ) {
        match top_lvl {
            TopLvl::FnDecl {
                name,
                ret_type,
                params,
                ..
            } => {
                let name = prefix
                    .map(|prefix| format!("{}.{}", prefix, name.node))
                    .unwrap_or_else(|| name.node.to_owned());

                if self.functions.contains_key(&name) {
                    // TODO: replace with proper error
                    panic!("Duplicate function '{}'", name);
                }

                let varargs = params.varargs;

                let func_params = params.params.iter().map(|Param(_, ty)| *ty).collect();
                let func_def = FunctionDefinition {
                    ret_type: *ret_type,
                    params: func_params,
                    varargs,
                };

                self.functions.insert(name, func_def);
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
                        self.resolve_prg(Some(name.node));
                        self.current_name = current_name;
                    } else {
                        errors.push(self.not_defined_error(name.span, name.span, name.node));
                    }
                }
            }
            TopLvl::TypeDef(TypeDef::StructDef { name, fields }) => {
                let full_name = prefix
                    .map(|prefix| format!("{}.{}", prefix, name.node))
                    .unwrap_or_else(|| name.node.to_owned());

                let fields = fields
                    .iter()
                    .enumerate()
                    .map(|(i, (Spanned { node, .. }, ty))| (*node, (i as u32, *ty)))
                    .collect();

                let def = UserTypeDefinition {
                    name: name.node,
                    fields,
                };

                self.user_types.insert(full_name, def);
            }
            TopLvl::Error(err) => {
                panic!("Invalid top level declaration {:#?}\n{}", top_lvl, err.node)
            }
        }
    }

    fn resolve_top_lvl(&mut self, top_lvl: &TopLvl<'src>, errors: &mut Vec<ResolveError<'src>>) {
        if let TopLvl::FnDecl {
            params,
            ref body,
            ret_type,
            is_extern,
            ..
        } = top_lvl
        {
            self.current_func_ret_type = *ret_type;
            self.sym_table.scope_enter();

            for p in &params.params {
                self.sym_table.bind(p.0.node, p.0.span, p.1.node, true);
            }

            if !*is_extern {
                for stmt in &body.0 {
                    self.resolve_stmt(&stmt, errors);
                }
            }

            self.sym_table.scope_exit();
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt<'src>, errors: &mut Vec<ResolveError<'src>>) {
        match stmt {
            Stmt::VarDecl(decl) => {
                let VarDecl {
                    name,
                    ref value,
                    eq,
                    ty: ref var_type,
                } = decl.as_ref();

                let expected = if let Some(ty) = var_type.get() {
                    Some(ty.node)
                } else {
                    None
                };

                match self.resolve_expr(value.span, &value.node, expected) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => {
                        if let Some(var_type) = var_type.get() {
                            let expr_span = Span::new(name.span.start, value.span.end);
                            // If a type was provided, check if it's one of the builtin types
                            if let Err(err) = self
                                .compare_binary_types(
                                    eq.span,
                                    expr_span,
                                    var_type.borrow().node,
                                    ty,
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
                            let insert_ty = Some(Spanned::new(0, 0, ty));
                            var_type.set(insert_ty);
                            self.sym_table.bind(name.node, name.span, ty, false)
                        }
                    }
                };
            }
            Stmt::If {
                condition,
                then_block,
                else_branch,
            } => {
                match self.resolve_expr(condition.span, &condition.node, None) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => self.expect_bool(ty, "if condition", condition.span, errors),
                }

                // TODO: refactor to method
                self.sym_table.scope_enter();
                for stmt in &then_block.0 {
                    self.resolve_stmt(&stmt, errors);
                }
                self.sym_table.scope_exit();

                if let Some(else_branch) = else_branch {
                    match else_branch.as_ref() {
                        Else::IfStmt(s) => {
                            if let Stmt::If { .. } = s.as_ref() {
                                self.resolve_stmt(s, errors);
                            } else {
                                panic!("Only if statement allowed here");
                            }
                        }
                        Else::Block(b) => {
                            self.sym_table.scope_enter();
                            for stmt in &b.0 {
                                self.resolve_stmt(&stmt, errors);
                            }
                            self.sym_table.scope_exit();
                        }
                    }
                }
            }
            Stmt::While { condition, body } => {
                match self.resolve_expr(condition.span, &condition.node, None) {
                    Err(msg) => errors.push(msg),
                    Ok(ty) => self.expect_bool(ty, "while condition", condition.span, errors),
                }

                // TODO: refactor to method
                self.sym_table.scope_enter();
                for stmt in &body.0 {
                    self.resolve_stmt(&stmt, errors);
                }
                self.sym_table.scope_exit();
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let return_type = self.current_func_ret_type.node;

                    if return_type == Type::Simple(Simple::Void) {
                        // TODO Error for return in void
                        unimplemented!("Error for return in void");
                    }

                    let resolved = self.resolve_expr(expr.span, &expr.node, Some(return_type));

                    if let Err(msg) = resolved {
                        errors.push(msg);
                    } else if let Ok(res) = resolved {
                        if let Err(err) = self.compare_types(
                            expr.span,
                            expr.span,
                            self.current_func_ret_type.node,
                            res,
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
                } else if self.current_func_ret_type.node != Type::Simple(Simple::Void) {
                    // TODO Error for no return in non void
                    unimplemented!("Error for no return in non void")
                }
            }
            Stmt::Delete(expr) => {
                let ty = self.resolve_expr(expr.span, &expr.node, None);
                if let Err(msg) = ty {
                    errors.push(msg);
                } else if let Ok(ty) = ty {
                    match ty {
                        Type::Pointer(_) => {}
                        _ => {
                            let err = self.error(
                                expr.span,
                                expr.span,
                                ResolveErrorType::Delete(NonPtrError(ty)),
                            );
                            errors.push(err);
                        }
                    }
                }
            }
            Stmt::Expr(ref expr) => {
                if let Err(msg) = self.resolve_expr(expr.span, &expr.node, None) {
                    errors.push(msg);
                }
            }
        };
    }

    fn resolve_expr(
        &mut self,
        span: Span,
        expr: &Expr<'src>,
        expected: Option<Type<'src>>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        let mut queue = expr.sub_exprs();
        let mut exprs = Vec::with_capacity(queue.len());

        // fill with all sub-sub... expressions
        // basically a BFS
        while !queue.is_empty() {
            let e = queue.pop().unwrap();
            let mut subs = e.sub_exprs();
            exprs.push(e);
            queue.append(&mut subs);
        }

        // check child expressions
        for expr in exprs.iter().rev() {
            let opt_ty = self.check_expr(span, expr)?;
            // if the type is there already, fill it in
            if let Some(ty) = opt_ty {
                expr.set_ty(ty);
            }
        }

        // check actual expression
        let opt_ty = self.check_expr(span, expr)?;
        let ty = self.ty_unwrap(span, opt_ty, expected)?;
        // Insert type information
        expr.set_ty(ty);
        Ok(ty)
    }

    fn check_expr(
        &mut self,
        span: Span,
        expr: &Expr<'src>,
    ) -> Result<Option<Type<'src>>, ResolveError<'src>> {
        match expr.kind() {
            ExprKind::Error(_) => {
                unreachable!("If errors occur during parsing, the program should not be resolved")
            }
            ExprKind::NullLit => Ok(None),
            ExprKind::New(expr) => {
                let ty = self.resolve_type(expr, None)?;

                if let Type::Simple(ty) = ty {
                    Ok(Some(Type::Pointer(Pointer::new(1, ty))))
                } else {
                    // TODO: Implement proper error handling
                    panic!("Implement proper error handling");
                }
            }
            ExprKind::DecLit(_) => Ok(Some(Type::Simple(Simple::I32))),
            ExprKind::StringLit(_) => Ok(Some(Type::Simple(Simple::String))),
            ExprKind::Negate(op, expr) => {
                let ty = self.resolve_type(expr, None)?;
                // TODO: unary operation error
                Some(self.compare_binary_types(op.span, span, ty, Type::Simple(Simple::I32)))
                    .transpose()
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

                Some(self.compare_binary_types(op.span, span, left, right)).transpose()
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
                    let left = self.resolve_type(l, Some(right))?;
                    (left, right)
                } else if let Err(ResolveError {
                    error: ResolveErrorType::Inference(TypeInferenceError),
                    ..
                }) = right
                {
                    // the same as above, but inverted
                    let left = left?;
                    let right = self.resolve_type(r, Some(left))?;
                    (left, right)
                } else {
                    (left?, right?)
                };

                self.compare_binary_types(op.span, span, left, right)?;
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
                        let user_type = self.get_user_type(Spanned::from_span(span, type_name))?;
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
                    let user_type = self.get_user_type(*identifier)?;
                    let field_type = self.get_field(&user_type, name)?;

                    let val_type = self.resolve_type(value, Some(field_type))?;
                    self.compare_types(value.span, span, field_type, val_type, "struct literal")?
                }
                Ok(Some(Type::Simple(Simple::UserType(identifier.node))))
            }
            ExprKind::Assign { left, eq, value } => {
                if let ExprKind::Ident(name) = left.node.kind() {
                    // Lookup variable in defined scopes
                    let (ty, sym_span) = {
                        let Spanned {
                            span,
                            node: Symbol { ty, .. },
                        } = self
                            .sym_table
                            .lookup(name)
                            .ok_or_else(|| self.not_defined_error(span, span, name))?;
                        (*ty, *span)
                    };

                    // get type of right expression
                    let val_type = self.resolve_type(value, Some(ty))?;
                    // check if type of right expression is the same as that of
                    // the declared variable
                    Some(
                        self.compare_binary_types(eq.span, span, ty, val_type)
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
                    let val_type = self.resolve_type(value, Some(ty))?;

                    Some(self.compare_binary_types(eq.span, span, ty, val_type)).transpose()
                }
            }
            ExprKind::Ident(name) => self
                .sym_table
                .lookup(name)
                .ok_or_else(|| self.not_defined_error(span, span, name))
                .map(|sym| Some(sym.node.ty)),

            ExprKind::Call { callee, args } => {
                // TODO: replace with proper resolution to enable UFCS
                let callee_name = self.current_source().slice(callee.span);
                &self.functions;

                let func_type = self
                    .functions
                    .get(callee_name)
                    .ok_or_else(|| self.not_defined_error(callee.span, span, callee_name))?
                    .clone();

                let varargs = func_type.varargs;

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

                let mut params;

                let iter = if !varargs {
                    args.0.iter().zip(func_type.params.iter())
                } else {
                    // if the function is varargs, the number of arguments does not correspond to
                    // the number of params, so simply zipping them would cut of some args.
                    // Thats why the difference between params and args is filled with void
                    // pointers
                    let mut type_params = func_type.params.clone();
                    // TODO: fix underflow
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
                    let ty = self.resolve_type(arg, Some(param_type.node))?;
                    arg_types.push((arg.span, ty));
                }

                let arg_error = func_type
                    .params
                    .iter()
                    .zip(arg_types.iter())
                    .filter_map(|(p, (arg_span, a))| {
                        // compare arguments to expected parameters
                        if let Err(err) =
                            self.compare_types(*arg_span, span, p.node, *a, "argument")
                        {
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
                    Ok(Some(func_type.ret_type.node))
                }
            }
        }
    }
}

impl<'src, 'ast> Resolver<'src, 'ast> {
    fn current_source(&self) -> &'src Source {
        let (src, _) = self.programs[self.current_name];
        src
    }

    fn ty_unwrap(
        &self,
        span: Span,
        ty: Option<Type<'src>>,
        expected: Option<Type<'src>>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        if let Some(ty) = ty {
            return Ok(ty);
        } else if let Some(expected) = expected {
            // the ty is null, but since a pointer is expected, we can "cast" it
            if let Type::Pointer(_) = expected {
                return Ok(expected);
            }
        }
        Err(self.type_inference_error(span))
    }

    fn resolve_type(
        &self,
        expr: &Spanned<Expr<'src>>,
        expected: Option<Type<'src>>,
    ) -> Result<Type<'src>, ResolveError<'src>> {
        let ty = self.ty_unwrap(expr.span, expr.node.ty(), expected);
        if let Ok(ty) = ty {
            expr.node.set_ty(ty);
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

    fn get_user_type(
        &self,
        name: Spanned<&'src str>,
    ) -> Result<&UserTypeDefinition<'src>, ResolveError<'src>> {
        self.user_types
            .get(name.node)
            .ok_or_else(|| self.not_defined_error(name.span, name.span, name.node))
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
            .map(|ty| ty.1.node)
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
                ResolveErrorType::IllegalAssignment(AssignmentError {
                    name,
                    definition_span: def_span,
                    bin_op_err: err,
                }),
            )
        } else {
            panic!("Invalid Error Type");
        }
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
        expected: Type<'src>,
        actual: Type<'src>,
        name: &'static str,
    ) -> Result<(), ResolveError<'src>> {
        if expected != actual
            // varargs disables type checking
            && expected != Type::Simple(Simple::Varargs)
            && actual != Type::Simple(Simple::Varargs)
        {
            return Err(self.error(
                err_span,
                expr_span,
                ResolveErrorType::IllegalType(IllegalTypeError {
                    expected_type: expected,
                    actual_type: actual,
                    name,
                }),
            ));
        }

        Ok(())
    }

    fn compare_binary_types(
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::token::Token;
    use std::cell::Cell;

    #[test]
    fn test_resolve_should_find_imported_fn() {
        let main_src = Source::new("main", "import test\nfn main(): void {test.func()}");
        let test_src = Source::new("test", "fn func(): void {}");

        let main_ast = Program(vec![
            TopLvl::Import {
                name: Spanned::new(7, 10, "test"),
            },
            TopLvl::FnDecl {
                name: Spanned::new(16, 19, "main"),
                ret_type: Spanned::new(24, 27, Type::Simple(Simple::Void)),
                params: ParamList::default(),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    32,
                    34,
                    Expr::new(ExprKind::Call {
                        callee: Box::new(Spanned::new(
                            29,
                            37,
                            Expr::new(ExprKind::Access {
                                left: Box::new(Spanned::new(
                                    29,
                                    32,
                                    Expr::new(ExprKind::Ident("test")),
                                )),
                                identifier: Spanned::new(34, 37, "func"),
                            }),
                        )),
                        args: ArgList(vec![]),
                    }),
                ))]),
            },
        ]);

        let test_ast = Program(vec![TopLvl::FnDecl {
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
            TopLvl::FnDecl {
                name: Spanned::new(3, 6, "main"),
                ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
                params: ParamList::default(),
                is_extern: false,
                body: Block(vec![Stmt::Expr(Spanned::new(
                    18,
                    23,
                    Expr::new(ExprKind::Call {
                        callee: Box::new(Spanned::new(18, 21, Expr::new(ExprKind::Ident("test")))),
                        args: ArgList(vec![]),
                    }),
                ))]),
            },
            TopLvl::FnDecl {
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

        let ast = Program(vec![TopLvl::FnDecl {
            name: Spanned::new(3, 6, "main"),
            ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
            params: ParamList::default(),
            is_extern: false,
            body: Block(vec![Stmt::VarDecl(Box::new(VarDecl {
                name: Spanned::new(22, 22, "x"),
                value: Spanned::new(26, 28, Expr::new(ExprKind::DecLit("10"))),
                eq: Spanned::new(24, 24, Token::Equals),
                ty: Cell::new(None),
            }))]),
        }]);

        let mut map = HashMap::new();
        map.insert("test", (&source, ast));

        let mut resolver = Resolver::new("test", &map);
        let errors = resolver.resolve();

        let expected: Vec<ResolveError> = vec![];
        assert_eq!(expected, errors);

        if let TopLvl::FnDecl { body, .. } = &(&map["test"].1).0[0] {
            if let Stmt::VarDecl(decl) = &body.0[0] {
                assert_eq!(decl.value.node.ty(), Some(Type::Simple(Simple::I32)));
                return;
            }
        }

        panic!("Type not inserted");
    }

    #[test]
    fn test_resolve_with_assign_error_should_return_correct_error() {
        let source = Source::new("test", r#"fn main(): void { let x = 10; x = ""; }"#);

        let ast = Program(vec![TopLvl::FnDecl {
            name: Spanned::new(3, 6, "main"),
            ret_type: Spanned::new(11, 14, Type::Simple(Simple::Void)),
            params: ParamList::default(),
            is_extern: false,
            body: Block(vec![
                Stmt::VarDecl(Box::new(VarDecl {
                    name: Spanned::new(22, 22, "x"),
                    value: Spanned::new(26, 28, Expr::new(ExprKind::DecLit("10"))),
                    eq: Spanned::new(24, 24, Token::Equals),
                    ty: Cell::new(None),
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
