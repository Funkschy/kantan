use std::{borrow::Borrow, collections::HashMap, ffi::CString, ptr};

use llvm_sys::{analysis::*, core::*, prelude::*};

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

            KantanLLVMContext {
                context,
                builder,
                module,
                temp_var_counter: 0,
                name_table,
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

    // TODO: params
    unsafe fn func_type(&self, ret: LLVMTypeRef, _params: &[(&str, Type)]) -> LLVMTypeRef {
        LLVMFunctionType(ret, ptr::null_mut(), 0, 0)
    }
}

// TODO: run "memory to register promotion" pass
impl KantanLLVMContext {
    pub fn generate(&mut self, function: &Func) {
        unsafe {
            let ret_type = self.convert(function.ret);
            // TODO: params
            let func_type = self.func_type(ret_type, &[]);

            let f = self.add_func(func_type, &function.label);
            self.add_entry_bb(f);

            for b in &function.blocks.blocks {
                for inst in &b.instructions {
                    self.translate_mir_instr(inst);
                }
                self.translate_mir_instr(&b.terminator);
            }
        }
    }

    unsafe fn add_func<T: Borrow<str>>(
        &mut self,
        func_type: LLVMTypeRef,
        name: &T,
    ) -> LLVMValueRef {
        let name = CString::new(name.borrow()).unwrap().into_raw();
        let f = LLVMAddFunction(self.module, name, func_type);
        self.strings.push(CString::from_raw(name));
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
            Instruction::Assignment(a, e) => match a {
                Address::Name(n) => {
                    self.translate_mir_expr(e, n);
                }
                Address::Temp(t) => {
                    self.translate_mir_expr(e, &t.to_string());
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    unsafe fn translate_mir_address(&mut self, a: &Address) -> LLVMValueRef {
        match a {
            Address::Name(n) => self.name_table[n.to_owned()],
            Address::Temp(t) => self.name_table[&t.to_string()],
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
                    BinaryType::I32(ty) => match ty {
                        IntBinaryType::Add => {
                            let n = CString::new(name).unwrap().into_raw();
                            let add = LLVMBuildAdd(self.builder, left, right, n);
                            self.strings.push(CString::from_raw(n));
                            self.name_table.insert(name.to_owned(), add);
                            add
                        }
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    unsafe fn temp_var(&mut self) -> *mut i8 {
        let cstr = CString::new(format!("tmp{}", self.temp_var_counter))
            .unwrap()
            .into_raw();
        self.temp_var_counter += 1;
        self.strings.push(CString::from_raw(cstr));
        cstr
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
