use std::{borrow::Borrow, collections::HashMap, ffi::CString, ptr};

use llvm_sys::{analysis::*, core::*, prelude::*, LLVMIntPredicate, LLVMLinkage, LLVMUnnamedAddr};

use crate::{
    mir::{address::*, tac::*},
    types::Type,
    Mir, UserTypeDefinition, UserTypeMap,
};

const ADDRESS_SPACE: u32 = 0;

enum Intrinsic {
    MemCpy = 0,
}

pub struct KantanLLVMContext {
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    // TODO: we probably want separate Modules
    module: LLVMModuleRef,
    temp_var_counter: usize,
    name_table: HashMap<String, LLVMValueRef>,
    functions: HashMap<String, (LLVMValueRef, LLVMTypeRef)>,
    current_function: Option<LLVMValueRef>,
    globals: HashMap<Label, LLVMValueRef>,
    blocks: HashMap<Label, LLVMBasicBlockRef>,
    user_types: HashMap<String, LLVMTypeRef>,
    // TODO: make hashmap to save memory
    strings: Vec<CString>,
    intrinsics: Vec<LLVMValueRef>,
}

impl KantanLLVMContext {
    pub fn new(name: &str, types: &UserTypeMap) -> Self {
        unsafe {
            let name = CString::new(name).unwrap().into_raw();

            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(name, context);
            let builder = LLVMCreateBuilderInContext(context);

            let name_table = HashMap::new();
            let functions = HashMap::new();
            let globals = HashMap::new();
            let blocks = HashMap::new();

            let user_types = HashMap::new();

            let mut ctx = KantanLLVMContext {
                context,
                builder,
                module,
                temp_var_counter: 0,
                name_table,
                functions,
                current_function: None,
                globals,
                blocks,
                user_types,
                strings: vec![CString::from_raw(name)],
                intrinsics: Vec::new(),
            };

            ctx.add_intrinsics();

            let user_types = types
                .iter()
                .map(|(n, ty)| (n.clone(), ctx.create_llvm_struct(n, ty)))
                .collect();

            ctx.user_types = user_types;

            ctx
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

        self.intrinsics.push(memcpy_func);
    }

    unsafe fn create_llvm_struct(&mut self, name: &str, def: &UserTypeDefinition) -> LLVMTypeRef {
        let mut fields = vec![ptr::null_mut(); def.fields.len()];

        for (_, (i, ty)) in def.fields.iter() {
            fields[*i as usize] = self.convert(ty.node);
        }

        let s = LLVMStructCreateNamed(self.context, self.cstring(name));
        LLVMStructSetBody(s, fields.as_mut_ptr(), fields.len() as u32, false as i32);
        s
    }
}

impl KantanLLVMContext {
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

    #[inline(always)]
    pub fn module(&self) -> LLVMModuleRef {
        self.module
    }

    fn get_intrinsic(&self, intrinsic: Intrinsic) -> LLVMValueRef {
        self.intrinsics[intrinsic as usize]
    }

    #[inline(always)]
    unsafe fn llvm_bool(&self, value: bool) -> LLVMValueRef {
        LLVMConstInt(
            LLVMInt1TypeInContext(self.context),
            value as u64,
            true as i32,
        )
    }

    unsafe fn convert(&self, ty: Type) -> LLVMTypeRef {
        match ty {
            Type::I32 => LLVMInt32TypeInContext(self.context),
            Type::Bool => LLVMInt8TypeInContext(self.context),
            Type::Void => LLVMVoidTypeInContext(self.context),
            Type::String => LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE),
            Type::UserType(name) => self.user_types[name],
        }
    }
}

// TODO: run "memory to register promotion" pass
impl KantanLLVMContext {
    unsafe fn cstring(&mut self, string: &str) -> *mut i8 {
        let cstr = CString::new(string).unwrap().into_raw();
        self.strings.push(CString::from_raw(cstr));
        cstr
    }

    unsafe fn func_type(&mut self, ret: LLVMTypeRef, mut params: Vec<LLVMTypeRef>) -> LLVMTypeRef {
        LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, 0)
    }

    pub fn generate(&mut self, mir: &Mir) {
        unsafe {
            for (label, string) in &mir.global_strings {
                self.add_global_string(label, string);
            }

            let mut llvm_funcs = Vec::with_capacity(mir.functions.len());

            // Function definitions need to be evaluated first
            for function in &mir.functions {
                let ret_type = self.convert(function.ret);

                let params: Vec<LLVMTypeRef> = function
                    .params
                    .iter()
                    .map(|(_, t)| self.convert(*t))
                    .collect();

                let func_type = self.func_type(ret_type, params);
                let f = self.add_func(func_type, &function.label, function.is_extern);
                llvm_funcs.push(f);
            }

            for (i, function) in mir.functions.iter().enumerate() {
                if function.is_extern {
                    continue;
                }

                let f = llvm_funcs[i];
                self.current_function = Some(f);

                let mut bbs = Vec::with_capacity(function.blocks.blocks.len());

                // generate basic blocks
                for b in function.blocks.blocks.iter() {
                    if let Instruction::Label(label) = &b.instructions[0] {
                        let name: &str = label.borrow();
                        let bb_ref = self.add_bb(f, name);
                        self.blocks.insert(label.clone(), bb_ref);
                        bbs.push(bb_ref);
                    } else {
                        panic!("No label");
                    }
                }

                // allocate arguments on stack
                LLVMPositionBuilderAtEnd(self.builder, bbs[0]);
                for (i, (name, ty)) in function.params.iter().enumerate() {
                    let n = self.cstring(name);
                    dbg!(name);
                    let stack_arg = LLVMBuildAlloca(self.builder, self.convert(*ty), n);
                    LLVMBuildStore(self.builder, LLVMGetParam(f, i as u32), stack_arg);
                    self.name_table.insert(name.to_string(), stack_arg);
                }

                // generate actual instructions
                for (j, b) in function.blocks.blocks.iter().enumerate() {
                    LLVMPositionBuilderAtEnd(self.builder, bbs[j]);

                    for inst in &b.instructions {
                        self.translate_mir_instr(inst);
                    }
                    self.translate_mir_instr(&b.terminator);
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
        let length = string.len() as u32;
        let name = self.global_string_name();
        let string = self.cstring(string);

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

    unsafe fn add_func<T: Borrow<str>>(
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

        let real_name = self.cstring(real_name);

        let f = LLVMAddFunction(self.module, real_name, func_type);
        self.functions.insert(n.to_owned(), (f, func_type));
        f
    }

    unsafe fn add_bb(&mut self, f: LLVMValueRef, name: &str) -> LLVMBasicBlockRef {
        let name = self.cstring(name);
        LLVMAppendBasicBlockInContext(self.context, f, name)
    }

    unsafe fn translate_mir_instr(&mut self, instr: &Instruction) {
        match instr {
            Instruction::Return(Some(a)) => {
                LLVMBuildRet(self.builder, self.translate_mir_address(a));
            }
            Instruction::Return(None) => {
                LLVMBuildRetVoid(self.builder);
            }
            Instruction::Decl(a, ty) => {
                let ty = self.convert(*ty);
                let n = a.to_string();

                let stack = LLVMBuildAlloca(self.builder, ty, self.cstring(&n));

                self.name_table.insert(n, stack);
            }
            Instruction::Assignment(a, e) => {
                let n = a.to_string();
                let expr = self.translate_mir_expr(e, &n);

                // translate_mir_expr allocas a new struct, which needs to be memcpyed
                if let Expression::StructInit(identifier, _) = e {
                    let a = self.name_table[&n];
                    let ptr_ty =
                        LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE);

                    let dest = LLVMBuildBitCast(self.builder, a, ptr_ty, self.cstring("dest"));
                    let src = LLVMBuildBitCast(self.builder, expr, ptr_ty, self.cstring("src"));
                    let size = LLVMSizeOf(self.user_types[identifier.to_owned()]);

                    let memcpy = self.get_intrinsic(Intrinsic::MemCpy);
                    let mut memcpy_args = vec![dest, src, size, self.llvm_bool(false)];

                    LLVMBuildCall(
                        self.builder,
                        memcpy,
                        memcpy_args.as_mut_ptr(),
                        memcpy_args.len() as u32,
                        self.cstring(""),
                    );
                } else if let Some(ptr) = self.name_table.get(&n) {
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
            Instruction::Label(_) => {}
            _ => unimplemented!("{}", instr),
        }
    }

    unsafe fn translate_mir_address(&mut self, a: &Address) -> LLVMValueRef {
        match a {
            Address::Name(n) => {
                LLVMBuildLoad(self.builder, self.name_table[n], self.cstring("tmp"))
            }
            Address::Temp(t) => self.name_table[&t.to_string()],
            // TODO: other types
            Address::Const(c) => LLVMConstInt(self.convert(c.ty), c.literal.parse().unwrap(), 1),
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

    unsafe fn translate_mir_expr(&mut self, e: &Expression, name: &str) -> LLVMValueRef {
        match e {
            Expression::Copy(a) => self.translate_mir_address(a),
            Expression::Binary(l, ty, r) => {
                let left = self.translate_mir_address(l);
                let right = self.translate_mir_address(r);
                match ty {
                    BinaryType::I16(ty) | BinaryType::I32(ty) => {
                        self.int_binary(left, right, *ty, name)
                    }
                }
            }
            Expression::Unary(uop, a) => {
                let a = self.translate_mir_address(a);
                let n = self.cstring(name);

                match uop {
                    UnaryType::I32Negate => LLVMBuildNeg(self.builder, a, n),
                    UnaryType::BoolNegate => LLVMBuildNot(self.builder, a, n),
                }
            }
            Expression::Call(label, args) => {
                let n = self.cstring(name);
                let num_args = args.len() as u32;

                let mut args: Vec<LLVMValueRef> =
                    args.iter().map(|a| self.translate_mir_address(a)).collect();

                let (f, f_type) = self.functions[&label.to_string()];
                let name = if LLVMGetReturnType(f_type) != LLVMVoidTypeInContext(self.context) {
                    n
                } else {
                    // void functions can't have a name
                    self.cstring("")
                };
                LLVMBuildCall(self.builder, f, args.as_mut_ptr(), num_args, name)
            }
            Expression::StructGep(a, idx) => {
                let address = match a {
                    Address::Name(n) => self.name_table[n],
                    Address::Temp(t) => self.name_table[&t.to_string()],
                    Address::Global(g) => self.globals[g],
                    _ => unreachable!("{} is invalid here", a),
                };
                let gep = LLVMBuildStructGEP(self.builder, address, *idx, self.cstring("ptr"));
                LLVMBuildLoad(self.builder, gep, self.cstring("value"))
            }
            Expression::StructInit(identifier, values) => {
                let struct_ty = self.user_types[identifier.to_owned()];
                let struct_alloca = LLVMBuildAlloca(self.builder, struct_ty, self.cstring("tmp"));
                for (i, value) in values.iter().enumerate() {
                    let a = self.translate_mir_address(value);
                    let ptr = LLVMBuildStructGEP(
                        self.builder,
                        struct_alloca,
                        i as u32,
                        self.cstring(&format!("value.{}", i)),
                    );
                    LLVMBuildStore(self.builder, a, ptr);
                }
                struct_alloca
            }
            _ => unimplemented!(),
        }
    }

    unsafe fn int_binary(
        &mut self,
        left: LLVMValueRef,
        right: LLVMValueRef,
        ty: IntBinaryType,
        name: &str,
    ) -> LLVMValueRef {
        let n = self.cstring(name);

        match ty {
            IntBinaryType::Add => LLVMBuildAdd(self.builder, left, right, n),
            IntBinaryType::Sub => LLVMBuildSub(self.builder, left, right, n),
            IntBinaryType::Mul => LLVMBuildMul(self.builder, left, right, n),
            // TODO: signed vs unsigned
            IntBinaryType::Div => LLVMBuildSDiv(self.builder, left, right, n),
            IntBinaryType::Eq => {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntEQ, left, right, n)
            }
            IntBinaryType::Smaller => {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntSLT, left, right, n)
            }
            IntBinaryType::SmallerEq => {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntSLE, left, right, n)
            }
        }
    }
}

impl Drop for KantanLLVMContext {
    fn drop(&mut self) {
        // TODO: remove
        println!("Dropping the bass");
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
