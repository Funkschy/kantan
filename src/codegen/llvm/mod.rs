use std::{ffi::CString, io::Write};

use crate::{print_error, Mir};

mod context;
mod pass;
mod target;

use context::*;
use pass::*;
use target::*;

pub use target::CodeGenOptLevel;

#[derive(Clone, Copy, PartialEq)]
pub enum OutputType {
    Asm,
    LLVMIR,
    Object,
}

impl OutputType {
    pub fn convert(value: &str) -> Option<Self> {
        Some(match value {
            "asm" => OutputType::Asm,
            "obj" => OutputType::Object,
            "llvm-ir" => OutputType::LLVMIR,
            _ => return None,
        })
    }
}

pub struct CodeGenArgs<'a, W: Write> {
    filename: &'a str,
    err_writer: &'a mut W,
    output: OutputType,
    opt_lvl: CodeGenOptLevel,
}

impl<'a, W: Write> CodeGenArgs<'a, W> {
    pub fn new(
        filename: &'a str,
        err_writer: &'a mut W,
        output: OutputType,
        opt_lvl: CodeGenOptLevel,
    ) -> Self {
        CodeGenArgs {
            filename,
            err_writer,
            output,
            opt_lvl,
        }
    }
}

pub fn emit_to_file<'a, W: Write>(mir: &Mir, args: CodeGenArgs<'a, W>) {
    let mut ctx = KantanLLVMContext::new("main", &mir);
    ctx.generate(&mir);

    let arch = ArchType::X86_64;
    let vendor = VendorType::PC;
    let os = OsType::GnuLinux;

    if let Err(msg) = ctx.verify_module() {
        unsafe {
            let cstr = CString::from_raw(msg);
            let msg = cstr.to_str().unwrap();
            print_error(msg, args.err_writer).unwrap();
            ctx.dump_module();
        }
        panic!("Error while verifying");
    }

    let module = if args.opt_lvl > CodeGenOptLevel::OptNone {
        optimize_module(ctx.module(), args.opt_lvl)
    } else {
        ctx.module()
    };

    // TODO: remove
    ctx.dump_module();

    let target = Target::new(TargetTriple::new(arch, vendor, os)).unwrap();
    let tm = TargetMachine::new(target, CpuType::Generic, args.opt_lvl);

    let asm = args.output == OutputType::Asm;
    tm.emit_to_file(module, args.filename, asm).unwrap();
}
