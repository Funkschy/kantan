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

pub struct Triple {
    arch: ArchType,
    vendor: VendorType,
    os: OsType,
}

impl Triple {
    pub fn new(arch: ArchType, vendor: VendorType, os: OsType) -> Self {
        Self { arch, vendor, os }
    }
}

impl Into<String> for Triple {
    fn into(self) -> String {
        let arch: &str = self.arch.into();
        let vendor: &str = self.vendor.into();
        let os: &str = self.os.into();

        format!("{}-{}-{}", arch, vendor, os)
    }
}

impl Into<Vec<u8>> for Triple {
    fn into(self) -> Vec<u8> {
        let s: String = self.into();
        s.into()
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub enum CodeGenOptLevel {
    None,
    Less,
    Default,
    Aggressive,
}

impl CodeGenOptLevel {
    pub fn convert(value: &str) -> Option<Self> {
        Some(match value {
            "0" => CodeGenOptLevel::None,
            "1" => CodeGenOptLevel::Less,
            "2" => CodeGenOptLevel::Default,
            "3" => CodeGenOptLevel::Aggressive,
            _ => return None,
        })
    }
}

impl Into<LLVMCodeGenOptLevel> for CodeGenOptLevel {
    fn into(self) -> LLVMCodeGenOptLevel {
        match self {
            CodeGenOptLevel::None => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
            CodeGenOptLevel::Less => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
            CodeGenOptLevel::Default => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            CodeGenOptLevel::Aggressive => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
        }
    }
}

impl Into<u32> for CodeGenOptLevel {
    fn into(self) -> u32 {
        match self {
            CodeGenOptLevel::None => 0,
            CodeGenOptLevel::Less => 1,
            CodeGenOptLevel::Default => 2,
            CodeGenOptLevel::Aggressive => 3,
        }
    }
}

pub struct Target {
    target_ref: LLVMTargetRef,
    triple: *mut i8,
}

impl TryFrom<Triple> for Target {
    type Error = *mut i8;

    fn try_from(triple: Triple) -> Result<Self, *mut i8> {
        unsafe {
            let mut target = ptr::null_mut();
            let mut error = ptr::null_mut();

            triple.arch.register();

            let triple = CString::new(triple).unwrap().into_raw();

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
    features: *mut i8,
    cpu: *mut i8,
}

impl TargetMachine {
    pub fn new(target: Target, cpu_type: CpuType, level: CodeGenOptLevel) -> Self {
        unsafe {
            let features = CString::new("").unwrap().into_raw();

            let cpu_string: &str = cpu_type.into();
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
