use std::{borrow::Borrow, collections::HashMap, ffi::CString, mem, ptr};

use llvm_sys::{
    analysis::*, core::*, prelude::*, LLVMIntPredicate, LLVMLinkage, LLVMRealPredicate,
    LLVMUnnamedAddr,
};

use crate::{
    mir::{address::*, tac::*, ModTypeMap},
    types::*,
    Mir, UserTypeDefinition,
};

const ADDRESS_SPACE: u32 = 0;

enum Intrinsic {
    MemCpy = 0,
    MemSet = 1,
}

type LLVMFuncDef = (LLVMValueRef, LLVMTypeRef);

pub struct KantanLLVMContext<'src, 'mir> {
    // TODO: we probably want separate Modules
    module: LLVMModuleRef,
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    name_table: HashMap<String, LLVMValueRef>,
    current_function: Option<LLVMValueRef>,
    globals: HashMap<Label, LLVMValueRef>,
    blocks: HashMap<Label, LLVMBasicBlockRef>,
    extern_functions: HashMap<&'mir str, LLVMFuncDef>,
    functions: HashMap<&'src str, HashMap<&'mir str, LLVMFuncDef>>,
    user_types: HashMap<&'src str, HashMap<&'src str, LLVMTypeRef>>,
    // TODO: make hashmap to save memory
    strings: Vec<CString>,
    intrinsics: Vec<LLVMValueRef>,
}

impl<'src, 'mir> KantanLLVMContext<'src, 'mir> {
    pub fn new(name: &str, mir: &'mir Mir<'src>) -> Self {
        unsafe {
            let name = CString::new(name).unwrap().into_raw();

            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(name, context);
            let builder = LLVMCreateBuilderInContext(context);

            let name_table = HashMap::new();
            let functions = HashMap::new();
            let extern_functions = HashMap::new();
            let globals = HashMap::new();
            let blocks = HashMap::new();

            let user_types = HashMap::new();

            let mut ctx = KantanLLVMContext {
                context,
                builder,
                module,
                name_table,
                functions,
                extern_functions,
                current_function: None,
                globals,
                blocks,
                user_types,
                strings: vec![CString::from_raw(name)],
                intrinsics: Vec::new(),
            };

            ctx.add_intrinsics();
            ctx.add_user_types(&mir.types);

            // Function definitions need to be evaluated first
            for (file, functions) in &mir.functions {
                let mut llvm_funcs = HashMap::new();

                for (_, function) in functions.iter() {
                    let ret_type = ctx.convert(&function.ret);

                    let mut params = ctx.convert_params(function.is_varargs, &function.params);
                    let func_type = ctx.func_type(function.is_varargs, ret_type, &mut params);

                    let f = ctx.llvm_add_func(func_type, &function.name, function.is_extern);
                    let func_def = (f, func_type);
                    let name = function.name.as_ref();
                    llvm_funcs.insert(name, func_def);

                    if function.is_extern {
                        ctx.extern_functions.insert(name, func_def);
                    }
                }

                ctx.functions.insert(file, llvm_funcs);
            }

            ctx
        }
    }

    unsafe fn add_user_types(&mut self, types: &ModTypeMap<'src>) {
        for (m, typemap) in types.iter() {
            let mut structs: HashMap<&'src str, _> = HashMap::new();
            for typedef in typemap.iter() {
                let n = typedef.name;
                // forward declaration of types
                let s = LLVMStructCreateNamed(self.context, self.cstring(n));
                structs.insert(n, s);
            }
            self.user_types.insert(m, structs);
        }

        for (m, typemap) in types.iter() {
            for typedef in typemap.iter() {
                self.add_llvm_struct(m, typedef);
            }
        }
    }

    unsafe fn add_intrinsics(&mut self) {
        let mut memcpy_params = vec![
            LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE),
            LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE),
            LLVMInt64TypeInContext(self.context),
            LLVMInt1TypeInContext(self.context),
        ];

        let memcpy_type = LLVMFunctionType(
            LLVMVoidTypeInContext(self.context),
            memcpy_params.as_mut_ptr(),
            memcpy_params.len() as u32,
            false as i32,
        );

        let memcpy_func = LLVMAddFunction(
            self.module,
            self.cstring("llvm.memcpy.p0i8.p0i8.i64"),
            memcpy_type,
        );

        let mut memset_params = vec![
            LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE),
            LLVMInt8TypeInContext(self.context),
            LLVMInt64TypeInContext(self.context),
            LLVMInt1TypeInContext(self.context),
        ];

        let memset_type = LLVMFunctionType(
            LLVMVoidTypeInContext(self.context),
            memset_params.as_mut_ptr(),
            memset_params.len() as u32,
            false as i32,
        );

        let memset_func = LLVMAddFunction(
            self.module,
            self.cstring("llvm.memset.p0i8.i64"),
            memset_type,
        );

        self.intrinsics.extend(&[memcpy_func, memset_func]);
    }

    unsafe fn add_llvm_struct(&mut self, module: &str, def: &UserTypeDefinition<'src>) {
        let mut fields = vec![ptr::null_mut(); def.fields.len()];

        for (i, ty) in def.fields.values() {
            fields[*i as usize] = self.convert(&ty.node);
        }

        let s = self.user_types[module][def.name];
        LLVMStructSetBody(s, fields.as_mut_ptr(), fields.len() as u32, false as i32);
    }
}

impl<'src, 'mir> KantanLLVMContext<'src, 'mir> {
    pub fn verify_module(&self) -> Result<(), *mut i8> {
        unsafe {
            let mut error = ptr::null_mut();
            if LLVMVerifyModule(
                self.module,
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                &mut error,
            ) != 0
            {
                return Err(error);
            }
            LLVMDisposeMessage(error);
        }

        Ok(())
    }

    pub fn dump_module(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }

    #[inline]
    pub fn module(&self) -> LLVMModuleRef {
        self.module
    }

    #[inline]
    fn get_intrinsic(&self, intrinsic: Intrinsic) -> LLVMValueRef {
        self.intrinsics[intrinsic as usize]
    }

    #[inline]
    unsafe fn llvm_bool(&self, value: bool) -> LLVMValueRef {
        LLVMConstInt(
            LLVMInt1TypeInContext(self.context),
            value as u64,
            true as i32,
        )
    }

    #[inline]
    fn get_user_type(&self, user_ty: &UserIdent) -> LLVMTypeRef {
        self.user_types[user_ty.module()][user_ty.name()]
    }

    #[inline]
    unsafe fn get_byte_ptr(&self) -> LLVMTypeRef {
        LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE)
    }

    unsafe fn convert(&mut self, ty: &Type<'src>) -> LLVMTypeRef {
        match ty {
            Type::Simple(ty) => match ty {
                Simple::Char => LLVMInt8TypeInContext(self.context),
                Simple::I32 => LLVMInt32TypeInContext(self.context),
                Simple::F32 => LLVMFloatTypeInContext(self.context),
                Simple::Bool => LLVMInt1TypeInContext(self.context),
                Simple::Void => LLVMVoidTypeInContext(self.context),
                Simple::String => self.get_byte_ptr(),
                Simple::UserType(user_ty) => self.get_user_type(&user_ty),
                // varargs is just handled as a type for convenience
                Simple::Varargs => panic!("Varargs is not a real type"),
            },
            Type::Pointer(ptr) => {
                let mut ty = if ptr.ty == Simple::Void {
                    // LLVM doesn't have a void pointer, so we use *i8 instead
                    LLVMInt8TypeInContext(self.context)
                } else {
                    self.convert(&Type::Simple(ptr.ty.clone()))
                };

                for _ in 0..ptr.number {
                    ty = LLVMPointerType(ty, ADDRESS_SPACE);
                }
                ty
            }
        }
    }
}

impl<'src, 'mir> KantanLLVMContext<'src, 'mir> {
    unsafe fn cstring(&mut self, string: &str) -> *mut i8 {
        let cstr = CString::new(string).unwrap().into_raw();
        self.strings.push(CString::from_raw(cstr));
        cstr as *mut i8
    }

    unsafe fn convert_params(
        &mut self,
        varargs: bool,
        params: &[(&str, Type<'src>)],
    ) -> Vec<LLVMTypeRef> {
        if varargs {
            Vec::new()
        } else {
            params.iter().map(|(_, t)| self.convert(t)).collect()
        }
    }

    unsafe fn func_type(
        &mut self,
        varargs: bool,
        ret: LLVMTypeRef,
        params: &mut [LLVMTypeRef],
    ) -> LLVMTypeRef {
        LLVMFunctionType(
            ret,
            params.as_mut_ptr(),
            params.len() as u32,
            varargs as i32,
        )
    }

    pub fn generate(&mut self, mir: &'mir Mir<'src>) {
        unsafe {
            for (label, string) in &mir.global_strings {
                self.add_global_string(label, string);
            }

            for (file, functions) in &mir.functions {
                for (_, function) in functions.iter() {
                    self.name_table.clear();

                    if function.is_extern {
                        continue;
                    }

                    let name: &str = function.name.as_ref();
                    let f = self.functions[*file][name];
                    self.current_function = Some(f.0);

                    let mut bbs = Vec::with_capacity(function.blocks.blocks.len());

                    // generate basic blocks
                    for b in function.blocks.blocks.iter() {
                        if let Instruction::Label(label) = &b.instructions[0] {
                            let name: &str = label.borrow();
                            let bb_ref = self.add_bb(f.0, name);
                            self.blocks.insert(label.clone(), bb_ref);
                            bbs.push(bb_ref);
                        } else {
                            panic!("No label");
                        }
                    }

                    // generate actual instructions
                    for (i, b) in function.blocks.blocks.iter().enumerate() {
                        LLVMPositionBuilderAtEnd(self.builder, bbs[i]);

                        for inst in &b.instructions {
                            self.translate_mir_instr(inst);
                        }
                        self.translate_mir_instr(&b.terminator);
                    }
                }
            }
        }
    }

    unsafe fn global_string_name(&mut self) -> *mut i8 {
        let num = self.globals.len();
        let s = format!(".str{}", num);
        self.cstring(&s)
    }

    unsafe fn add_global_string(&mut self, label: &Label, string: &str) {
        // TODO: find better solution
        let string = string.replace("\\n", "\n");
        let string = string.replace("\\0", "\0");

        let length = string.len() as u32;
        let name = self.global_string_name();
        let string = self.cstring(&string);

        let glob_str = LLVMAddGlobal(
            self.module,
            LLVMArrayType(LLVMInt8TypeInContext(self.context), length + 1),
            name,
        );
        LLVMSetGlobalConstant(glob_str, true as i32);
        LLVMSetLinkage(glob_str, LLVMLinkage::LLVMLinkerPrivateLinkage);
        LLVMSetUnnamedAddress(glob_str, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);

        let const_string = LLVMConstStringInContext(self.context, string, length, false as i32);
        LLVMSetInitializer(glob_str, const_string);

        self.globals.insert(label.clone(), glob_str);
    }

    unsafe fn llvm_add_func<T: Borrow<str>>(
        &mut self,
        func_type: LLVMTypeRef,
        name: &T,
        is_extern: bool,
    ) -> LLVMValueRef {
        let n = name.borrow();

        // TODO: find better solution
        let real_name = if is_extern {
            n.split('.').last().unwrap()
        } else {
            n
        };

        // don't declare extern functions multiple times
        if is_extern {
            if let Some((prev_def, _)) = self.extern_functions.get(real_name) {
                return *prev_def;
            }
        }

        let real_name = self.cstring(real_name);

        LLVMAddFunction(self.module, real_name, func_type)
    }

    unsafe fn add_bb(&mut self, f: LLVMValueRef, name: &str) -> LLVMBasicBlockRef {
        let name = self.cstring(name);
        LLVMAppendBasicBlockInContext(self.context, f, name)
    }

    unsafe fn translate_mir_instr(&mut self, instr: &Instruction<'src>) {
        match instr {
            Instruction::Return(Some(a)) => {
                LLVMBuildRet(self.builder, self.translate_mir_address(a));
            }
            Instruction::Return(None) => {
                LLVMBuildRetVoid(self.builder);
            }
            Instruction::Delete(a) => {
                let value = self.name_table[&a.to_string()];
                LLVMBuildFree(self.builder, value);
            }
            Instruction::Decl(a, ty) => {
                let ty = self.convert(ty);
                let n = a.to_string();

                let stack = LLVMBuildAlloca(self.builder, ty, self.cstring(&n));

                self.name_table.insert(n, stack);
            }
            Instruction::Assignment(a, e) => {
                let n = a.to_string();

                if let Expression::StructInit(identifier, values) = e.as_ref() {
                    let struct_ty = self.get_user_type(identifier);
                    let new_struct = self.create_struct(struct_ty, values);

                    let var = self.name_table[&n];
                    self.build_memcpy(var, new_struct, struct_ty);
                    return;
                }

                let expr = self.translate_mir_expr(e, &n);

                if *a == Address::Empty {
                    // Only void calls generate empty addresses. The typechecker does not allow them to be
                    // assigned or used in any way, but the mir generates assign instructions for every
                    // expression, so we return here, to make sure that no actual assign is generated
                    // for the result of a void function call
                    return;
                }

                if let Some(ptr) = self.name_table.get(&n) {
                    LLVMBuildStore(self.builder, expr, *ptr);
                } else {
                    self.name_table.insert(n, expr);
                }
            }
            Instruction::Jmp(l) => {
                let bb_ref = self.blocks[l];
                LLVMBuildBr(self.builder, bb_ref);
            }
            Instruction::JmpIf(a, then_label, else_label) => {
                let cond = self.translate_mir_address(a);
                let then_bb_ref = self.blocks[then_label];
                let else_bb_ref = self.blocks[else_label];
                LLVMBuildCondBr(self.builder, cond, then_bb_ref, else_bb_ref);
            }
            Instruction::Label(_) | Instruction::Nop => {}
        }
    }

    unsafe fn translate_mir_address(&mut self, a: &Address<'src>) -> LLVMValueRef {
        match a {
            Address::Empty => unreachable!(),
            Address::Null(ty) => LLVMConstNull(self.convert(ty)),
            Address::Name(n) => LLVMBuildLoad(self.builder, self.name_table[n], self.cstring(&n)),
            Address::Ref(r) => self.name_table[r],
            Address::Temp(t) => LLVMBuildLoad(
                self.builder,
                self.name_table[&t.to_string()],
                self.cstring(&t.to_string()),
            ),
            Address::Const(c) => {
                if c.ty.is_int() {
                    LLVMConstInt(self.convert(&c.ty), c.literal.parse().unwrap(), true as i32)
                } else if c.ty.is_char() {
                    LLVMConstInt(
                        self.convert(&c.ty),
                        c.literal.chars().next().unwrap() as u64,
                        false as i32,
                    )
                } else {
                    LLVMConstRealOfString(self.convert(&c.ty), self.cstring(c.literal))
                }
            }
            Address::Global(g) => {
                let string: LLVMValueRef = self.globals[g];

                let gep = LLVMBuildGEP(
                    self.builder,
                    string,
                    ptr::null_mut(),
                    0,
                    self.cstring("geptmp"),
                );

                // TODO: not only strings
                let ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE);
                LLVMBuildPointerCast(self.builder, gep, ptr, self.cstring("tmpstring"))
            }
        }
    }

    unsafe fn build_memcpy(
        &mut self,
        dest: LLVMValueRef,
        src: LLVMValueRef,
        ty: LLVMTypeRef,
    ) -> LLVMValueRef {
        let dest = self.build_i8_ptr_cast(dest, "dest");
        let src = self.build_i8_ptr_cast(src, "src");
        let size = LLVMSizeOf(ty);

        let memcpy = self.get_intrinsic(Intrinsic::MemCpy);
        let mut memcpy_args = vec![dest, src, size, self.llvm_bool(false)];

        LLVMBuildCall(
            self.builder,
            memcpy,
            memcpy_args.as_mut_ptr(),
            memcpy_args.len() as u32,
            self.cstring(""),
        )
    }

    unsafe fn translate_mir_expr(&mut self, e: &Expression<'src>, name: &str) -> LLVMValueRef {
        match e {
            Expression::GetParam(i) => LLVMGetParam(self.current_function.unwrap(), *i),
            Expression::BitCast(a, ty) => {
                let val = self.translate_mir_address(a);
                let dest_ty = self.convert(ty);
                LLVMBuildBitCast(self.builder, val, dest_ty, self.cstring(name))
            }
            Expression::SizeOf(ty) => {
                let ty = self.convert(ty);
                // TODO: remove when i64 is supported
                LLVMBuildIntCast(
                    self.builder,
                    LLVMSizeOf(ty),
                    LLVMInt32TypeInContext(self.context),
                    self.cstring(name),
                )
            }
            Expression::New(a, ty) => {
                let ty = self.convert(ty);
                let value = self.name_table[&a.to_string()];
                let malloc = LLVMBuildMalloc(self.builder, ty, self.cstring(name));
                self.build_memcpy(malloc, value, ty);
                malloc
            }
            Expression::Copy(a) => self.translate_mir_address(a),
            Expression::Binary(l, ty, r) => {
                let left = self.translate_mir_address(l);
                let right = self.translate_mir_address(r);

                match ty {
                    BinaryType::Int(ty) => self.int_binary(left, right, *ty, name),
                    BinaryType::F32(ty) => self.float_binary(left, right, *ty, name),
                    BinaryType::Ptr(PtrBinaryType::AddPointers)
                    | BinaryType::Ptr(PtrBinaryType::SubPointers) => {
                        let l_int = self.cstring("lint");
                        let r_int = self.cstring("rint");

                        let ty = if let BinaryType::Ptr(PtrBinaryType::AddPointers) = ty {
                            NumBinaryType::Add
                        } else {
                            NumBinaryType::Sub
                        };

                        self.int_binary(
                            LLVMBuildPtrToInt(
                                self.builder,
                                left,
                                LLVMInt32TypeInContext(self.context),
                                l_int,
                            ),
                            LLVMBuildPtrToInt(
                                self.builder,
                                right,
                                LLVMInt32TypeInContext(self.context),
                                r_int,
                            ),
                            ty,
                            name,
                        )
                    }
                    BinaryType::Ptr(ty) => {
                        let mut right = match ty {
                            PtrBinaryType::Add => vec![right],
                            PtrBinaryType::Sub => {
                                vec![LLVMBuildNeg(self.builder, right, self.cstring("offset"))]
                            }
                            _ => unreachable!(),
                        };

                        LLVMBuildInBoundsGEP(
                            self.builder,
                            left,
                            right.as_mut_ptr(),
                            1,
                            self.cstring(name),
                        )
                    }
                }
            }
            Expression::Unary(uop, a) => {
                let a = self.translate_mir_address(a);
                let n = self.cstring(name);

                match uop {
                    UnaryType::I32Negate => LLVMBuildNeg(self.builder, a, n),
                    UnaryType::BoolNegate => LLVMBuildNot(self.builder, a, n),
                    UnaryType::Deref => a,
                }
            }
            Expression::Call {
                ident,
                args,
                ret_type,
                varargs,
            } => {
                let mut n = self.cstring(name);
                let num_args = args.len() as u32;

                let float_type = LLVMFloatTypeInContext(self.context);

                let mut args: Vec<LLVMValueRef> =
                    args.iter().map(|a| self.translate_mir_address(a)).collect();

                for a in &mut args {
                    // If a float is passed as an argument to a variadic function,
                    // it has to be promoted to a double implicitly
                    if *varargs && LLVMTypeOf(*a) == float_type {
                        // convert float to double
                        let double = LLVMBuildFPExt(
                            self.builder,
                            *a,
                            LLVMDoubleTypeInContext(self.context),
                            self.cstring("promoted"),
                        );

                        // replace float with double
                        mem::replace(a, double);
                    }
                }

                let f = self.functions[ident.module()][ident.name()];
                if *ret_type == Type::Simple(Simple::Void) {
                    // void functions can't have a name
                    n = self.cstring("");
                }

                LLVMBuildCall(self.builder, f.0, args.as_mut_ptr(), num_args, n)
            }
            Expression::StructGep(a, idx) => {
                let address = match a {
                    Address::Name(n) => self.name_table[n],
                    Address::Temp(t) => self.name_table[&t.to_string()],
                    Address::Global(g) => self.globals[g],
                    _ => unreachable!("{} is invalid here", a),
                };
                LLVMBuildStructGEP(self.builder, address, *idx, self.cstring("ptr"))
            }
            Expression::StructInit(identifier, values) => {
                let struct_ty = self.get_user_type(identifier);
                self.create_struct(struct_ty, values)
            }
        }
    }

    unsafe fn build_i8_ptr_cast(&mut self, value: LLVMValueRef, name: &str) -> LLVMValueRef {
        let byte_ty = LLVMInt8TypeInContext(self.context);
        let ptr_ty = LLVMPointerType(byte_ty, ADDRESS_SPACE);
        LLVMBuildBitCast(self.builder, value, ptr_ty, self.cstring(name))
    }

    unsafe fn struct_init(
        &mut self,
        struct_val: LLVMValueRef,
        struct_ty: LLVMTypeRef,
        values: &[Address<'src>],
    ) {
        let memset = self.get_intrinsic(Intrinsic::MemSet);
        let mut args = vec![
            self.build_i8_ptr_cast(struct_val, "i8structptr"),
            LLVMConstInt(LLVMInt8TypeInContext(self.context), 0 as u64, true as i32),
            LLVMSizeOf(struct_ty),
            self.llvm_bool(false),
        ];

        LLVMBuildCall(
            self.builder,
            memset,
            args.as_mut_ptr(),
            args.len() as u32,
            self.cstring(""),
        );

        for (i, value) in values.iter().enumerate() {
            let a = self.translate_mir_address(value);
            let ptr = LLVMBuildStructGEP(
                self.builder,
                struct_val,
                i as u32,
                self.cstring(&format!("value.{}", i)),
            );
            LLVMBuildStore(self.builder, a, ptr);
        }
    }

    unsafe fn create_struct(
        &mut self,
        struct_ty: LLVMTypeRef,
        values: &[Address<'src>],
    ) -> LLVMValueRef {
        let struct_alloca = LLVMBuildAlloca(self.builder, struct_ty, self.cstring("structtmp"));
        self.struct_init(struct_alloca, struct_ty, values);
        struct_alloca
    }

    unsafe fn float_binary(
        &mut self,
        left: LLVMValueRef,
        right: LLVMValueRef,
        ty: NumBinaryType,
        name: &str,
    ) -> LLVMValueRef {
        let n = self.cstring(name);

        match ty {
            NumBinaryType::Add => LLVMBuildFAdd(self.builder, left, right, n),
            NumBinaryType::Sub => LLVMBuildFSub(self.builder, left, right, n),
            NumBinaryType::Mul => LLVMBuildFMul(self.builder, left, right, n),
            // TODO: signed vs unsigned
            NumBinaryType::Div => LLVMBuildFDiv(self.builder, left, right, n),
            _ => {
                let real_pred = match ty {
                    NumBinaryType::Eq => LLVMRealPredicate::LLVMRealOEQ,
                    NumBinaryType::Neq => LLVMRealPredicate::LLVMRealONE,
                    NumBinaryType::Smaller => LLVMRealPredicate::LLVMRealOLT,
                    NumBinaryType::SmallerEq => LLVMRealPredicate::LLVMRealOLE,
                    NumBinaryType::Greater => LLVMRealPredicate::LLVMRealOGT,
                    NumBinaryType::GreaterEq => LLVMRealPredicate::LLVMRealOGE,
                    _ => unreachable!(),
                };
                LLVMBuildFCmp(self.builder, real_pred, left, right, n)
            }
        }
    }

    unsafe fn int_binary(
        &mut self,
        left: LLVMValueRef,
        right: LLVMValueRef,
        ty: NumBinaryType,
        name: &str,
    ) -> LLVMValueRef {
        let n = self.cstring(name);

        match ty {
            NumBinaryType::Add => LLVMBuildAdd(self.builder, left, right, n),
            NumBinaryType::Sub => LLVMBuildSub(self.builder, left, right, n),
            NumBinaryType::Mul => LLVMBuildMul(self.builder, left, right, n),
            NumBinaryType::And => LLVMBuildAnd(self.builder, left, right, n),
            NumBinaryType::Or => LLVMBuildOr(self.builder, left, right, n),
            // TODO: not actually modulo, but remainder
            NumBinaryType::Mod => LLVMBuildSRem(self.builder, left, right, n),
            // TODO: signed vs unsigned
            NumBinaryType::Div => LLVMBuildSDiv(self.builder, left, right, n),
            _ => {
                let int_pred = match ty {
                    NumBinaryType::Eq => LLVMIntPredicate::LLVMIntEQ,
                    NumBinaryType::Neq => LLVMIntPredicate::LLVMIntNE,
                    NumBinaryType::Smaller => LLVMIntPredicate::LLVMIntSLT,
                    NumBinaryType::SmallerEq => LLVMIntPredicate::LLVMIntSLE,
                    NumBinaryType::Greater => LLVMIntPredicate::LLVMIntSGT,
                    NumBinaryType::GreaterEq => LLVMIntPredicate::LLVMIntSGE,
                    _ => unreachable!(),
                };
                LLVMBuildICmp(self.builder, int_pred, left, right, n)
            }
        }
    }
}

impl<'src, 'mir> Drop for KantanLLVMContext<'src, 'mir> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
