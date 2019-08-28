//! The middle intermediate representation.
//! This IR is very similar to LLVM-IR

pub mod address;
mod blockmap;
pub mod builder;
pub mod func;
mod names;
pub mod tac;

use std::{collections::HashMap, fmt};

pub use self::builder::MirBuilder;
use super::types::*;
use crate::{mir::tac::Label, MirFuncMap, UserTypeDefinition};

pub struct FunctionHead<'src> {
    name: String,
    params: Vec<(&'src str, Type<'src>)>,
    ret_type: Type<'src>,
    is_extern: bool,
    is_varargs: bool,
}

impl<'src> FunctionHead<'src> {
    pub fn new(
        name: String,
        params: Vec<(&'src str, Type<'src>)>,
        ret_type: Type<'src>,
        is_extern: bool,
        is_varargs: bool,
    ) -> Self {
        FunctionHead {
            name,
            params,
            ret_type,
            is_extern,
            is_varargs,
        }
    }
}

pub type ModTypeMap<'src> = HashMap<&'src str, Vec<UserTypeDefinition<'src>>>;

#[derive(Debug)]
pub struct Mir<'src> {
    pub global_strings: HashMap<Label, &'src str>,
    pub functions: HashMap<&'src str, MirFuncMap<'src>>,
    pub types: ModTypeMap<'src>,
}

impl<'src> fmt::Display for Mir<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = self
            .types
            .iter()
            .flat_map(|(m, types)| {
                types
                    .iter()
                    .map(|v| format!("{}.{}", m, v))
                    .collect::<Vec<String>>()
            })
            .collect::<Vec<String>>()
            .join("\n");

        let global_strings = self
            .global_strings
            .iter()
            .map(|(k, v)| format!("{} {}", k, v))
            .collect::<Vec<String>>()
            .join("\n");

        let funcs = self
            .functions
            .iter()
            .flat_map(|(m, funcs)| {
                funcs
                    .iter()
                    .map(|(_, v)| format!("{}.{}", m, v))
                    .collect::<Vec<String>>()
            })
            .collect::<Vec<String>>()
            .join("\n\n");

        write!(f, "{}\n{}\n{}", types, global_strings, funcs)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        compile,
        mir::{address::*, blockmap::BlockMap, func::Func, tac::*, *},
        Source,
    };

    use std::{collections::HashMap, io::Cursor};

    #[test]
    fn test_mir_for_sequence_of_instructions_should_have_no_jumps() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def main(): void {
                let x = 0;
                let y = 2;

                let z = x * y + 2;
                x = z;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);

        let mut bb = BasicBlock::default();
        bb.instructions = vec![
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Name("y".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Name("z".to_string()), Type::Simple(Simple::I32)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
            Instruction::Assignment(
                Address::Name("y".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "2",
                )))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x".to_string()),
                    BinaryType::Int(NumBinaryType::Mul),
                    Address::Name("y".to_string()),
                )),
            ),
            Instruction::Assignment(
                Address::Name("z".to_string()),
                Box::new(Expression::Binary(
                    Address::Temp(TempVar::from(0)),
                    BinaryType::Int(NumBinaryType::Add),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "2")),
                )),
            ),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Name("z".to_string()))),
            ),
        ];

        bb.terminator = Instruction::Return(Some(Address::Const(Constant::new(
            Type::Simple(Simple::I32),
            "0",
        ))));
        bm.blocks = vec![bb];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_with_if_should_continue_after_if_in_both_cases() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def main(): i32 {
                let x = 0;
                if x == 0 {
                    x = 2;
                }
                return x;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);
        bm.mappings.insert(Label::new(0), 2);
        bm.mappings.insert(Label::new(1), 1);

        let mut bb1 = BasicBlock::default();
        bb1.instructions = vec![
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::Bool)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x".to_string()),
                    BinaryType::Int(NumBinaryType::Eq),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                )),
            ),
        ];
        bb1.terminator = Instruction::JmpIf(
            Address::Temp(TempVar::from(0)),
            Label::new(1),
            Label::new(0),
        );

        let mut bb2 = BasicBlock::default();
        bb2.instructions = vec![
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "2",
                )))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![Instruction::Label(Label::new(0))];
        bb3.terminator = Instruction::Return(Some(Address::Name("x".to_string())));

        bm.blocks = vec![bb1, bb2, bb3];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_with_if_else_should_continue_after_if_in_both_cases() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def main(): i32 {
                let x = 0;
                if x == 0 {
                    x = 2;
                } else {
                    x = 3;
                }
                return x;
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);
        bm.mappings.insert(Label::new(0), 3);
        bm.mappings.insert(Label::new(1), 1);
        bm.mappings.insert(Label::new(2), 2);

        let mut bb1 = BasicBlock::default();
        bb1.instructions = vec![
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::Bool)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x".to_string()),
                    BinaryType::Int(NumBinaryType::Eq),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                )),
            ),
        ];
        bb1.terminator = Instruction::JmpIf(
            Address::Temp(TempVar::from(0)),
            Label::new(1),
            Label::new(2),
        );

        let mut bb2 = BasicBlock::default();
        bb2.instructions = vec![
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "2",
                )))),
            ),
        ];
        bb2.terminator = Instruction::Jmp(Label::new(0));

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "3",
                )))),
            ),
        ];
        bb3.terminator = Instruction::Jmp(Label::new(0));

        let mut bb4 = BasicBlock::default();
        bb4.instructions = vec![Instruction::Label(Label::new(0))];
        bb4.terminator = Instruction::Return(Some(Address::Name("x".to_string())));

        bm.blocks = vec![bb1, bb2, bb3, bb4];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_mir_while_has_correct_jumps() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def main(): void {
                let x = 0;
                while x < 10 {
                    x = x + 1;
                }
            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut bm = BlockMap::default();

        bm.mappings.insert(Label::from(".entry0".to_string()), 0);
        bm.mappings.insert(Label::new(1), 1);
        bm.mappings.insert(Label::new(2), 2);
        bm.mappings.insert(Label::new(0), 3);

        // init block
        let mut bb1 = BasicBlock::default();
        bb1.instructions = vec![
            Label::from(".entry0".to_string()).into(),
            Instruction::Decl(Address::Name("x".to_string()), Type::Simple(Simple::I32)),
            Instruction::Decl(Address::Temp(TempVar::from(0)), Type::Simple(Simple::Bool)),
            Instruction::Nop,
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "0",
                )))),
            ),
        ];
        bb1.terminator = Instruction::Jmp(Label::new(1));

        let mut bb2 = BasicBlock::default();
        bb2.instructions = vec![
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x".to_string()),
                    BinaryType::Int(NumBinaryType::Smaller),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "10")),
                )),
            ),
        ];
        bb2.terminator = Instruction::JmpIf(
            Address::Temp(TempVar::from(0)),
            Label::new(2),
            Label::new(0),
        );

        let mut bb3 = BasicBlock::default();
        bb3.instructions = vec![
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Binary(
                    Address::Name("x".to_string()),
                    BinaryType::Int(NumBinaryType::Add),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "1")),
                )),
            ),
        ];
        bb3.terminator = Instruction::Jmp(Label::new(1));

        let mut bb4 = BasicBlock::default();
        bb4.instructions = vec![Instruction::Label(Label::new(0))];
        bb4.terminator = Instruction::Return(Some(Address::Const(Constant::new(
            Type::Simple(Simple::I32),
            "0",
        ))));

        bm.blocks = vec![bb1, bb2, bb3, bb4];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }

    #[test]
    fn test_void_return_can_be_omitted() {
        let mut cursor = Cursor::new(Vec::default());

        let source = "
            def test(): void {

            }

            def main(): void {

            }
        ";

        let sources = vec![Source::new("main", source)];
        let funcs = compile(&sources, &mut cursor).unwrap().functions;

        let mut main_bm = BlockMap::default();
        main_bm
            .mappings
            .insert(Label::from(".entry0".to_string()), 0);

        let mut test_bm = BlockMap::default();
        test_bm
            .mappings
            .insert(Label::from(".entry0".to_string()), 0);

        let mut main_bb = BasicBlock::default();
        main_bb.instructions = vec![Label::from(".entry0".to_string()).into(), Instruction::Nop];
        main_bb.terminator = Instruction::Return(Some(Address::Const(Constant::new(
            Type::Simple(Simple::I32),
            "0",
        ))));

        let mut test_bb = BasicBlock::default();
        test_bb.instructions = vec![Label::from(".entry0".to_string()).into(), Instruction::Nop];
        test_bb.terminator = Instruction::Return(None);

        main_bm.blocks = vec![main_bb];
        test_bm.blocks = vec![test_bb];

        let mut expected = HashMap::new();
        let mut expected_funcs = HashMap::new();

        expected_funcs.insert(
            "test".to_owned(),
            Func::new(
                "test".to_owned(),
                vec![],
                Type::Simple(Simple::Void),
                test_bm,
                false,
                false,
            ),
        );
        expected_funcs.insert(
            "main".to_owned(),
            Func::new(
                "main".to_owned(),
                vec![],
                Type::Simple(Simple::I32),
                main_bm,
                false,
                false,
            ),
        );
        expected.insert("main", expected_funcs);

        assert_eq!(expected, funcs);
    }
}
