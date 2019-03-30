use super::target::CodeGenOptLevel;

use llvm_sys::{
    core::*,
    prelude::*,
    transforms::{pass_manager_builder::*, scalar::*, util::*},
};

pub fn optimize_module(module: LLVMModuleRef, opt_level: CodeGenOptLevel) -> LLVMModuleRef {
    unsafe {
        // Per clang and rustc, we want to use both kinds.
        let fpm = LLVMCreateFunctionPassManagerForModule(module);
        let mpm = LLVMCreatePassManager();

        let pmb = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderSetOptLevel(pmb, opt_level.into());

        // Magic threshold from Clang for -O2
        LLVMPassManagerBuilderUseInlinerWithThreshold(pmb, 225);
        LLVMPassManagerBuilderPopulateModulePassManager(pmb, mpm);
        LLVMPassManagerBuilderPopulateFunctionPassManager(pmb, fpm);
        LLVMPassManagerBuilderDispose(pmb);

        LLVMInitializeFunctionPassManager(fpm);
        LLVMAddPromoteMemoryToRegisterPass(fpm);
        LLVMAddDeadStoreEliminationPass(fpm);

        // Iterate over functions, running the FPM over each
        let mut func = LLVMGetFirstFunction(module);
        while func.is_null() {
            LLVMRunFunctionPassManager(fpm, func);
            func = LLVMGetNextFunction(func);
        }
        LLVMFinalizeFunctionPassManager(fpm);

        // Run the MPM over the module
        LLVMRunPassManager(mpm, module);

        // Clean up managers
        LLVMDisposePassManager(fpm);
        LLVMDisposePassManager(mpm);

        module
    }
}
