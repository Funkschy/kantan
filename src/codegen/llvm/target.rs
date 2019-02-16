use std::{ffi::CString, ptr};

use llvm_sys::{core::LLVMDisposeMessage, target_machine::*};

pub enum ArchType {
    X86_64,
}

impl Into<&'static str> for ArchType {
    fn into(self) -> &'static str {
        match self {
            ArchType::X86_64 => "x86",
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
    Linux,
}

impl Into<&'static str> for OsType {
    fn into(self) -> &'static str {
        match self {
            OsType::Linux => "Linux",
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

pub struct Target {
    target_ref: Box<LLVMTarget>,
}

impl Target {
    pub fn new(triple: TargetTriple) -> Result<Self, CString> {
        unsafe {
            let target = ptr::null_mut();
            let mut error = ptr::null_mut();

            let cstr = CString::new(triple).unwrap().into_raw() as *mut _;

            if LLVMGetTargetFromTriple(cstr, target, &mut error as *mut *mut _) != 0 {
                LLVMDisposeMessage(cstr);
                return Err(CString::from_raw(error));
            }

            LLVMDisposeMessage(cstr);

            Ok(Target {
                target_ref: Box::from_raw(*target),
            })
        }
    }
}
