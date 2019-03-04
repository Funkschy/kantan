use std::fmt;

use super::{blockmap::BlockMap, tac::*};
use crate::types::Type;

#[derive(PartialEq, Debug)]
pub struct Func<'input> {
    pub(crate) label: Label,
    pub(crate) params: Vec<(&'input str, Type)>,
    pub(crate) ret: Type,
    pub(crate) blocks: BlockMap<'input>,
    pub(crate) is_extern: bool,
}

impl<'input> Func<'input> {
    pub fn new(
        label: Label,
        params: Vec<(&'input str, Type)>,
        ret: Type,
        blocks: BlockMap<'input>,
        is_extern: bool,
    ) -> Self {
        Func {
            label,
            params,
            ret,
            blocks,
            is_extern,
        }
    }
}

impl<'input> fmt::Display for Func<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, t))| format!("_arg{}: {}", i, t))
            .collect::<Vec<String>>()
            .join(", ");

        if self.is_extern {
            return write!(f, "extern fn {}({}): {};", self.label, params, self.ret);
        }

        let format = |inst| {
            if let &Instruction::Label(..) = inst {
                format!("   {}", inst)
            } else {
                format!("       {}", inst)
            }
        };

        let instructions = self
            .blocks
            .blocks
            .iter()
            .map(|b| (b.instructions.iter(), &b.terminator))
            .flat_map(|(is, t)| {
                let mut instrs = is.map(format).collect::<Vec<String>>();
                instrs.push(format(t));
                instrs
            })
            .collect::<Vec<String>>()
            .join("\n");

        write!(
            f,
            "fn {}({}): {} {{\n{}\n}}",
            self.label, params, self.ret, instructions
        )
    }
}
