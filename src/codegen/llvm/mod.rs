use crate::mir::func::Func;

mod target;
mod wrapper;

use target::*;
use wrapper::*;

pub fn emit_to_file(functions: &[Func], filename: &str) {
    let ctx = KantanLLVMContext::new("main");

    let arch = ArchType::X86_64;
    let vendor = VendorType::PC;
    let os = OsType::Linux;

    // let target = Target::new(TargetTriple::new(arch, vendor, os)).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_emit_to_file() {
        emit_to_file(&vec![], "");
    }
}
