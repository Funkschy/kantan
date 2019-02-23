use std::{ffi::CString, io::Write};

use crate::{mir::func::Func, print_error};

mod context;
mod target;

use context::*;
use target::*;

pub fn emit_to_file<W: Write>(functions: &[Func], filename: &str, err_writer: &mut W) {
    let mut ctx = KantanLLVMContext::new("main");

    for function in functions {
        ctx.generate(function);
    }

    let arch = ArchType::X86_64;
    let vendor = VendorType::PC;
    let os = OsType::GnuLinux;

    if let Err(msg) = ctx.verify_module() {
        unsafe {
            let cstr = CString::from_raw(msg);
            let msg = cstr.to_str().unwrap();
            print_error(msg, err_writer).unwrap();
        }
        return;
    }

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
                let x = 20 + 0;
                let y = x + 20 + 2;
                return y;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap();

        println!(
            "{}",
            funcs
                .iter()
                .map(|f| f.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        );

        println!("----------");

        emit_to_file(&funcs, "target/test.asm", &mut cursor);
        let inner = cursor.into_inner();
        let len = inner.len();

        if !inner.is_empty() {
            eprintln!("{}", String::from_utf8(inner).unwrap());
        }

        assert_eq!(0, len);
    }
}
