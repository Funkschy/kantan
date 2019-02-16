use std::ffi::CString;

use llvm_sys::{core::*, prelude::*};

pub struct KantanLLVMContext {
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    // TODO: we probably want separate Modules
    module: LLVMModuleRef,
    name: *mut i8,
}

impl KantanLLVMContext {
    pub fn new(name: &str) -> Self {
        unsafe {
            let name = CString::new(name).unwrap().into_raw();

            let context = LLVMContextCreate();
            let builder = LLVMCreateBuilderInContext(context);
            let module = LLVMModuleCreateWithNameInContext(name, context);

            KantanLLVMContext {
                context,
                builder,
                module,
                name,
            }
        }
    }
}

impl Drop for KantanLLVMContext {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.module);
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.context);
            CString::from_raw(self.name);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_context() {
        let ctx = KantanLLVMContext::new("test");
    }
}
