use crate::mir::func::Func;

mod context;
mod target;

use context::*;
use target::*;

pub fn emit_to_file(functions: &[Func], filename: &str) {
    let mut ctx = KantanLLVMContext::new("main");

    for function in functions {
        ctx.generate(function);
    }

    let arch = ArchType::X86_64;
    let vendor = VendorType::PC;
    let os = OsType::GnuLinux;

    ctx.verify_module().unwrap();
    // TODO: remove
    ctx.dump_module();

    let target = Target::new(TargetTriple::new(arch, vendor, os)).unwrap();
    let tm = TargetMachine::new(target, CpuType::Generic, CodeGenOptLevel::OptNone);

    tm.emit_to_file(ctx.module(), filename, true).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{compile, Source};
    use std::io::Cursor;

    #[test]
    fn test_emit_to_file() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            fn main(): i32 {
                let x = 0;
                if x == 0 {
                    x = 2;
                } else {
                    x = 3;
                }
                return x;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap();

        emit_to_file(&funcs, "target/test.asm");
        assert!(cursor.into_inner().is_empty());
    }
}
