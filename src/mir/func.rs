use std::fmt;

use super::{blockmap::BlockMap, tac::*};
use crate::types::Type;

#[derive(PartialEq, Debug)]
pub struct Func<'input> {
    pub(crate) label: Label,
    pub(crate) params: Vec<(&'input str, Type)>,
    pub(crate) ret: Type,
    pub(crate) blocks: BlockMap<'input>,
}

impl<'input> Func<'input> {
    pub fn new(
        label: Label,
        params: Vec<(&'input str, Type)>,
        ret: Type,
        blocks: BlockMap<'input>,
    ) -> Self {
        Func {
            label,
            params,
            ret,
            blocks,
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

        let instructions = self
            .blocks
            .blocks
            .iter()
            .map(|b| (b.instructions.iter(), &b.terminator))
            .flat_map(|(is, t)| {
                let mut instrs = is.map(|i| format!("\t{}", i)).collect::<Vec<String>>();
                instrs.push(format!("\t{}", t));
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
