use std::{borrow::Borrow, collections::HashMap, ffi::CString, ptr};

use llvm_sys::{analysis::*, core::*, prelude::*, LLVMIntPredicate, LLVMLinkage, LLVMUnnamedAddr};

use crate::{
    mir::{address::*, tac::*},
    types::Type,
    Mir,
};

const ADDRESS_SPACE: u32 = 0;

pub struct KantanLLVMContext {
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    // TODO: we probably want separate Modules
    module: LLVMModuleRef,
    temp_var_counter: usize,
    // TODO: scopes
    name_table: HashMap<String, LLVMValueRef>,
    functions: HashMap<String, LLVMValueRef>,
    current_function: Option<LLVMValueRef>,
    globals: HashMap<Label, LLVMValueRef>,
    blocks: HashMap<Label, LLVMBasicBlockRef>,
    // TODO: make hashmap to save memory
    strings: Vec<CString>,
}

impl KantanLLVMContext {
    pub fn new(name: &str) -> Self {
        unsafe {
            let name = CString::new(name).unwrap().into_raw();

            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(name, context);
            let builder = LLVMCreateBuilderInContext(context);

            let name_table = HashMap::new();
            let functions = HashMap::new();
            let globals = HashMap::new();
            let blocks = HashMap::new();

            KantanLLVMContext {
                context,
                builder,
                module,
                temp_var_counter: 0,
                name_table,
                functions,
                globals,
                blocks,
                current_function: None,
                strings: vec![CString::from_raw(name)],
            }
        }
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

    unsafe fn convert(&self, ty: Type) -> LLVMTypeRef {
        match ty {
            Type::I32 => LLVMInt32TypeInContext(self.context),
            Type::Bool => LLVMInt8TypeInContext(self.context),
            Type::Void => LLVMVoidTypeInContext(self.context),
            Type::String => LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE),
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

    // TODO: params
    unsafe fn func_type(&mut self, ret: LLVMTypeRef, mut params: Vec<LLVMTypeRef>) -> LLVMTypeRef {
        LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, 0)
    }

    pub fn generate(&mut self, mir: &Mir) {
        unsafe {
            for (label, string) in &mir.globals {
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
                bbs.push(self.add_bb(f, "entry"));

                for b in function.blocks.blocks.iter().skip(1) {
                    if let Instruction::Label(label) = &b.instructions[0] {
                        let name: &str = label.borrow();
                        let bb_ref = self.add_bb(f, name);
                        self.blocks.insert(label.clone(), bb_ref);
                        bbs.push(bb_ref);
                    } else {
                        panic!("No label");
                    }
                }

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
        self.functions.insert(n.to_owned(), f);
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
            Instruction::Label(_) => {}
            _ => unimplemented!("{}", instr),
        }
    }

    unsafe fn translate_mir_address(&mut self, a: &Address) -> LLVMValueRef {
        match a {
            Address::Name(n) => LLVMBuildLoad(
                self.builder,
                self.name_table[n.to_owned()],
                self.cstring("tmp"),
            ),
            Address::Temp(t) => self.name_table[&t.to_string()],
            Address::Arg(arg) => LLVMGetParam(self.current_function.unwrap(), arg.into()),
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

                let ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), ADDRESS_SPACE);
                LLVMBuildPointerCast(self.builder, gep, ptr, self.cstring("tmpstring"))
            }
            _ => unimplemented!(),
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
                let n = CString::new(name).unwrap().into_raw();

                let op = match uop {
                    UnaryType::I32Negate => LLVMBuildNeg(self.builder, a, n),
                    UnaryType::BoolNegate => LLVMBuildNot(self.builder, a, n),
                };

                self.strings.push(CString::from_raw(n));
                op
            }
            Expression::Call(label, args) => {
                let n = CString::new(name).unwrap().into_raw();
                let num_args = args.len() as u32;

                let mut args: Vec<LLVMValueRef> =
                    args.iter().map(|a| self.translate_mir_address(a)).collect();

                let f = self.functions[&label.to_string()];
                let op = LLVMBuildCall(self.builder, f, args.as_mut_ptr(), num_args, n);

                self.strings.push(CString::from_raw(n));
                op
            }
            Expression::Empty => panic!("Unexpected empty expression"),
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
        let n = CString::new(name).unwrap().into_raw();

        let op = match ty {
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
        };

        self.strings.push(CString::from_raw(n));
        op
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
