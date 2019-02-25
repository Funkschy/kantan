use std::{ffi::CString, io::Write};

use crate::{mir::func::Func, print_error};

mod context;
mod target;

use context::*;
use target::*;

pub fn emit_to_file<W: Write>(functions: &[Func], filename: &str, err_writer: &mut W) {
    let mut ctx = KantanLLVMContext::new("main");
    ctx.generate(functions);

    let arch = ArchType::X86_64;
    let vendor = VendorType::PC;
    let os = OsType::GnuLinux;

    if let Err(msg) = ctx.verify_module() {
        unsafe {
            let cstr = CString::from_raw(msg);
            let msg = cstr.to_str().unwrap();
            print_error(msg, err_writer).unwrap();
            ctx.dump_module();
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
    use crate::{compile, stdlib, Source};
    use std::io::Cursor;

    #[test]
    fn test_emit_to_file() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            import io

            fn f(i: i32): i32 {
                let x = 20 + 2;
                let y = x + 20 + 2;
                let z = y;
                let v = -i;
                return z + v;
            }

            fn main(): i32 {
                io.putchar(65);
                return f(2);
            }
        ";

        let io = stdlib().remove(0);
        let sources = vec![Source::new("main", source), io];
        let compile_result = compile(&sources, &mut cursor);

        if let Err(err) = compile_result {
            println!("{}", err);
            println!("{}", String::from_utf8(cursor.into_inner()).unwrap());
            panic!("Compilation error");
        }

        let funcs = compile_result.unwrap();

        println!(
            "{}",
            funcs
                .iter()
                .map(|f| f.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        );

        println!("----------");

        emit_to_file(&funcs, "target/test.s", &mut cursor);
        let inner = cursor.into_inner();
        let len = inner.len();

        if !inner.is_empty() {
            eprintln!("{}", String::from_utf8(inner).unwrap());
        }

        assert_eq!(0, len);
    }
}
