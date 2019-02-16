use crate::mir::func::Func;

mod context;
mod target;

use context::*;
use target::*;

pub fn emit_to_file(_functions: &[Func], filename: &str) {
    let ctx = KantanLLVMContext::new("main");

    let arch = ArchType::X86_64;
    let vendor = VendorType::PC;
    let os = OsType::GnuLinux;

    ctx.verify_module().unwrap();
    ctx.dump_module();

    let target = Target::new(TargetTriple::new(arch, vendor, os)).unwrap();
    let tm = TargetMachine::new(target, CpuType::Generic, CodeGenOptLevel::OptNone);

    tm.emit_to_file(ctx.module(), filename, true).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_to_file() {
        emit_to_file(&vec![], "target/test.asm");
    }
}
