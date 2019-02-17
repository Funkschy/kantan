use std::{borrow::Borrow, ffi::CString, ptr};

use llvm_sys::{analysis::*, core::*, prelude::*};

use crate::{mir::func::Func, types::Type};

pub struct KantanLLVMContext {
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    // TODO: we probably want separate Modules
    module: LLVMModuleRef,
    strings: Vec<CString>,
}

impl KantanLLVMContext {
    pub fn new(name: &str) -> Self {
        unsafe {
            let name = CString::new(name).unwrap().into_raw();

            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(name, context);
            let builder = LLVMCreateBuilderInContext(context);

            KantanLLVMContext {
                context,
                builder,
                module,
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

impl KantanLLVMContext {
    pub fn generate(&mut self, function: &Func) {
        unsafe {
            let ret_type = self.convert(function.ret);
            // TODO: params
            let func_type = self.func_type(ret_type, &[]);

            self.add_func(func_type, &function.label);
        }
    }

    unsafe fn add_func<T: Borrow<str>>(&mut self, func_type: LLVMTypeRef, name: &T) {
        let name = CString::new(name.borrow()).unwrap().into_raw();
        LLVMAddFunction(self.module, name, func_type);
        self.strings.push(CString::from_raw(name));
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
