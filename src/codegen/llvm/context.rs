use std::{borrow::Borrow, collections::HashMap, ffi::CString, ptr};

use llvm_sys::{analysis::*, core::*, prelude::*, LLVMIntPredicate};

use crate::{
    mir::{address::*, func::Func, tac::*},
    types::Type,
};

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

            KantanLLVMContext {
                context,
                builder,
                module,
                temp_var_counter: 0,
                name_table,
                functions,
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
            _ => unimplemented!("TODO: implement strings"),
        }
    }
}

// TODO: run "memory to register promotion" pass
impl KantanLLVMContext {
    // TODO: params
    unsafe fn func_type(&mut self, ret: LLVMTypeRef, mut params: Vec<LLVMTypeRef>) -> LLVMTypeRef {
        LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, 0)
    }

    pub fn generate(&mut self, function: &Func) {
        unsafe {
            let ret_type = self.convert(function.ret);
            // TODO: params

            let params: Vec<LLVMTypeRef> = function
                .params
                .iter()
                .map(|(_, t)| self.convert(*t))
                .collect();

            let func_type = self.func_type(ret_type, params);

            let f = self.add_func(func_type, &function.label, function.is_extern);

            if !function.is_extern {
                self.add_entry_bb(f);

                for b in &function.blocks.blocks {
                    for inst in &b.instructions {
                        self.translate_mir_instr(inst);
                    }
                    self.translate_mir_instr(&b.terminator);
                }
            }
        }
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
            n.split('.').skip(1).nth(0).unwrap()
        } else {
            n
        };

        let real_name = CString::new(real_name).unwrap().into_raw();

        let f = LLVMAddFunction(self.module, real_name, func_type);
        self.current_function = Some(f);
        self.functions.insert(n.to_owned(), f);
        self.strings.push(CString::from_raw(real_name));
        f
    }

    unsafe fn add_entry_bb(&mut self, f: LLVMValueRef) {
        let entry = CString::new("entry").unwrap().into_raw();
        let bb = LLVMAppendBasicBlockInContext(self.context, f, entry);
        LLVMPositionBuilderAtEnd(self.builder, bb);
        self.strings.push(CString::from_raw(entry));
    }

    unsafe fn translate_mir_instr(&mut self, instr: &Instruction) {
        match instr {
            Instruction::Return(Some(a)) => {
                LLVMBuildRet(self.builder, self.translate_mir_address(a));
            }
            Instruction::Return(None) => {
                LLVMBuildRetVoid(self.builder);
            }
            Instruction::Assignment(a, e) => {
                let n = a.to_string();
                let expr = self.translate_mir_expr(e, &n);
                self.name_table.insert(n.to_owned(), expr);
            }
            _ => unimplemented!(),
        }
    }

    unsafe fn translate_mir_address(&mut self, a: &Address) -> LLVMValueRef {
        match a {
            Address::Name(n) => self.name_table[n.to_owned()],
            Address::Temp(t) => self.name_table[&t.to_string()],
            Address::Arg(arg) => LLVMGetParam(self.current_function.unwrap(), arg.into()),
            // TODO: other types
            Address::Const(c) => LLVMConstInt(self.convert(c.ty), c.literal.parse().unwrap(), 1),
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
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}
