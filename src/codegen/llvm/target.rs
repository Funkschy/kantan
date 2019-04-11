use std::{convert::TryFrom, ffi::CString, ptr};

use llvm_sys::{core::LLVMDisposeMessage, prelude::*, target::*, target_machine::*};

pub enum ArchType {
    X86_64,
}

impl ArchType {
    pub fn register(&self) {
        match self {
            ArchType::X86_64 => Self::register_x86(),
        }
    }

    fn register_x86() {
        unsafe {
            LLVMInitializeX86TargetInfo();
            LLVMInitializeX86Target();
            LLVMInitializeX86TargetMC();
            LLVMInitializeX86AsmPrinter();
        }
    }
}

impl Into<&'static str> for ArchType {
    fn into(self) -> &'static str {
        match self {
            ArchType::X86_64 => "x86_64",
        }
    }
}

pub enum VendorType {
    PC,
}

impl Into<&'static str> for VendorType {
    fn into(self) -> &'static str {
        match self {
            VendorType::PC => "pc",
        }
    }
}

pub enum OsType {
    GnuLinux,
}

impl Into<&'static str> for OsType {
    fn into(self) -> &'static str {
        match self {
            OsType::GnuLinux => "linux-gnu",
        }
    }
}

pub enum CpuType {
    Generic,
}

impl Into<&'static str> for CpuType {
    fn into(self) -> &'static str {
        match self {
            CpuType::Generic => "generic",
        }
    }
}

pub struct TargetTriple {
    arch: ArchType,
    vendor: VendorType,
    os: OsType,
}

impl TargetTriple {
    pub fn new(arch: ArchType, vendor: VendorType, os: OsType) -> Self {
        TargetTriple { arch, vendor, os }
    }
}

impl Into<String> for TargetTriple {
    fn into(self) -> String {
        let arch: &str = self.arch.into();
        let vendor: &str = self.vendor.into();
        let os: &str = self.os.into();

        format!("{}-{}-{}", arch, vendor, os)
    }
}

impl Into<Vec<u8>> for TargetTriple {
    fn into(self) -> Vec<u8> {
        let s: String = self.into();
        s.into()
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub enum CodeGenOptLevel {
    OptNone,
    OptLess,
    OptDefault,
    OptAggressive,
}

impl CodeGenOptLevel {
    pub fn convert(value: &str) -> Option<Self> {
        Some(match value {
            "0" => CodeGenOptLevel::OptNone,
            "1" => CodeGenOptLevel::OptLess,
            "2" => CodeGenOptLevel::OptDefault,
            "3" => CodeGenOptLevel::OptAggressive,
            _ => return None,
        })
    }
}

impl Into<LLVMCodeGenOptLevel> for CodeGenOptLevel {
    fn into(self) -> LLVMCodeGenOptLevel {
        use LLVMCodeGenOptLevel::*;

        match self {
            CodeGenOptLevel::OptNone => LLVMCodeGenLevelNone,
            CodeGenOptLevel::OptLess => LLVMCodeGenLevelLess,
            CodeGenOptLevel::OptDefault => LLVMCodeGenLevelDefault,
            CodeGenOptLevel::OptAggressive => LLVMCodeGenLevelAggressive,
        }
    }
}

impl Into<u32> for CodeGenOptLevel {
    fn into(self) -> u32 {
        match self {
            CodeGenOptLevel::OptNone => 0,
            CodeGenOptLevel::OptLess => 1,
            CodeGenOptLevel::OptDefault => 2,
            CodeGenOptLevel::OptAggressive => 3,
        }
    }
}

pub struct Target {
    target_ref: LLVMTargetRef,
    triple: *mut i8,
}

impl TryFrom<TargetTriple> for Target {
    type Error = *mut i8;

    fn try_from(triple: TargetTriple) -> Result<Self, *mut i8> {
        unsafe {
            let mut target = ptr::null_mut();
            let mut error = ptr::null_mut();

            triple.arch.register();

            let cstring = CString::new(triple).unwrap();
            let triple = cstring.into_raw();

            if LLVMGetTargetFromTriple(triple, &mut target, &mut error) != 0 {
                LLVMDisposeMessage(triple);
                return Err(error);
            }

            Ok(Target {
                target_ref: target,
                triple,
            })
        }
    }
}

impl Drop for Target {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeMessage(self.triple);
        }
    }
}

pub struct TargetMachine {
    tm_ref: LLVMTargetMachineRef,
    target: Target,
    features: *mut i8,
    cpu: *mut i8,
}

impl TargetMachine {
    pub fn new(target: Target, cpu: CpuType, level: CodeGenOptLevel) -> Self {
        unsafe {
            let features = CString::new("").unwrap().into_raw();

            let cpu_string: &str = cpu.into();
            let cpu = CString::new(cpu_string).unwrap().into_raw();

            let machine = LLVMCreateTargetMachine(
                target.target_ref,
                target.triple,
                cpu,
                features,
                level.into(),
                // TODO: find out what this does and act accordingly
                LLVMRelocMode::LLVMRelocPIC,
                LLVMCodeModel::LLVMCodeModelDefault,
            );

            if machine.is_null() {
                panic!("TargetMachine is null");
            }

            TargetMachine {
                tm_ref: machine,
                target,
                features,
                cpu,
            }
        }
    }

    pub fn emit_to_file(
        self,
        module: LLVMModuleRef,
        filename: &str,
        asm: bool,
    ) -> Result<(), *mut i8> {
        let ty = if asm {
            LLVMCodeGenFileType::LLVMAssemblyFile
        } else {
            LLVMCodeGenFileType::LLVMObjectFile
        };

        unsafe {
            let mut error = ptr::null_mut();
            let filename = CString::new(filename).unwrap().into_raw();
            if LLVMTargetMachineEmitToFile(self.tm_ref, module, filename, ty, &mut error) != 0 {
                LLVMDisposeMessage(filename);
                return Err(error);
            }

            LLVMDisposeMessage(filename);
        }

        Ok(())
    }
}

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetMachine(self.tm_ref);
            LLVMDisposeMessage(self.features);
            LLVMDisposeMessage(self.cpu);
        }
    }
}
