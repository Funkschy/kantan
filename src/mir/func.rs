use std::fmt;

use super::{blockmap::BlockMap, tac::*};
use crate::types::*;

#[derive(PartialEq, Debug)]
pub struct Func<'src> {
    pub(crate) name: String,
    pub(crate) params: Vec<(&'src str, Type<'src>)>,
    pub(crate) ret: Type<'src>,
    pub(crate) blocks: BlockMap<'src>,
    pub(crate) is_extern: bool,
    pub(crate) is_varargs: bool,
}

impl<'src> Func<'src> {
    pub fn new(
        name: String,
        params: Vec<(&'src str, Type<'src>)>,
        ret: Type<'src>,
        blocks: BlockMap<'src>,
        is_extern: bool,
        is_varargs: bool,
    ) -> Self {
        let mut params = params;

        if is_varargs {
            if let Some((_, ty)) = params.last() {
                if *ty == Type::Simple(Simple::Varargs) {
                    // If this is a variadic function, remove the last argument,
                    // because otherwise the codegenerator will try to allocate
                    // stack storage for the variadic argument
                    // TODO: instead handle Varargs like array argument
                    params.pop();
                }
            }
        }

        Func {
            name,
            params,
            ret,
            blocks,
            is_extern,
            is_varargs,
        }
    }
}

impl<'src> fmt::Display for Func<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|(n, t)| format!("{}: {}", n, t))
            .collect::<Vec<String>>()
            .join(", ");

        if self.is_extern {
            return write!(f, "extern def {}({}): {};", self.name, params, self.ret);
        }

        let format = |inst: &Instruction| {
            if let Instruction::Label(..) = *inst {
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
            "{}({}): {} {{\n{}\n}}",
            self.name, params, self.ret, instructions
        )
    }
}
