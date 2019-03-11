use std::{ffi::CString, io::Write};

use crate::{print_error, Mir};

mod context;
mod target;

use context::*;
use target::*;

pub fn emit_to_file<W: Write>(mir: &Mir, filename: &str, err_writer: &mut W) {
    let mut ctx = KantanLLVMContext::new("main", &mir.types);
    ctx.generate(&mir);

    let arch = ArchType::X86_64;
    let vendor = VendorType::PC;
    let os = OsType::GnuLinux;

    // TODO: remove
    ctx.dump_module();

    if let Err(msg) = ctx.verify_module() {
        unsafe {
            let cstr = CString::from_raw(msg);
            let msg = cstr.to_str().unwrap();
            print_error(msg, err_writer).unwrap();
            ctx.dump_module();
        }
        panic!("Error while verifying");
    }

    let target = Target::new(TargetTriple::new(arch, vendor, os)).unwrap();
    let tm = TargetMachine::new(target, CpuType::Generic, CodeGenOptLevel::OptNone);

    tm.emit_to_file(ctx.module(), filename, true).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{compile, stdlib, Source};
    use std::io::Cursor;

    fn call_emit_to_file(source: &str) {
        let mut cursor = Cursor::new(Vec::default());

        let io = stdlib().remove(0);
        let sources = vec![Source::new("main", source), io];
        let compile_result = compile(&sources, &mut cursor);

        if let Err(err) = compile_result {
            println!("{}", err);
            println!("{}", String::from_utf8(cursor.into_inner()).unwrap());
            panic!("Compilation error");
        }

        let mir = compile_result.unwrap();
        println!("{}", mir);

        println!("----------");

        emit_to_file(&mir, "target/test.s", &mut cursor);
        let inner = cursor.into_inner();
        let len = inner.len();

        if !inner.is_empty() {
            eprintln!("{}", String::from_utf8(inner).unwrap());
        }

        assert_eq!(0, len);
    }

    #[test]
    fn test_control_flow() {
        let source = r#"
            import io

            fn f(i: i32): i32 {
                let x = 20 + 2;
                let y = x + 20 + 2;
                let z = y;
                let v = -i;
                return z + v;
            }

            fn main(): i32 {
                let s = "correct";
                let x = 0;
                if f(2) == 44 {
                    s = "Hello World";
                    let x = 5;
                } else if f(2) == 43 {
                    s = "Wrong result";
                }
                io.puts(s);

                if f(3) == 41 {
                    s = "correct";
                    let x = 42;
                } else {
                    s = "test";
                    let x = "test";
                }
                let y = x;
                while x < 2 {
                    io.puts(s);
                    x = x + 1;
                }
                return x;
            }
        "#;

        call_emit_to_file(source);
    }

    #[test]
    fn test_struct_init_return() {
        let source = r#"
            import io

            type Person struct {
                name: string,
                age: i32
            }

            fn make_peter(age: i32): Person {
                return Person { name: "Peter", age: age };
            }

            fn main(): i32 {
                let p = make_peter(20);
                io.printf("%s\n", p.name);
                p.name = "Hans";
                io.printf("%s\n", p.name);
                return p.age;
            }
        "#;

        call_emit_to_file(source);
    }
}
