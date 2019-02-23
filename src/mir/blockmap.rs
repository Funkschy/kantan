use std::collections::HashMap;

use super::tac::*;

#[derive(Default, Debug, PartialEq)]
pub struct BlockMap<'input> {
    pub mappings: HashMap<Label, usize>,
    pub blocks: Vec<BasicBlock<'input>>,
}

impl<'input> BlockMap<'input> {
    pub fn from_instructions(block: InstructionBlock<'input>) -> Self {
        let mut mappings = HashMap::new();
        let mut blocks = vec![];
        let mut bb = BasicBlock::default();
        let mut label = Label::from(BlockMap::entry_name());
        let mut label_count = 1;

        for instr in block.0 {
            match instr {
                Instruction::Assignment(..) | Instruction::Nop => {
                    bb.instructions.push(instr);
                }
                Instruction::Label(ref l) => {
                    if !bb.instructions.is_empty() {
                        // Terminate last block with goto
                        let term = Instruction::Jmp(l.clone());
                        bb.terminator = term;
                        let idx = blocks.len();
                        blocks.push(bb);
                        mappings.insert(label, idx);
                        // begin new block with this label
                        label = l.clone();
                        bb = BasicBlock::default();
                        bb.instructions.push(instr);
                    } else {
                        label = l.clone();
                        bb.instructions.push(instr);
                    }
                }
                // Terminator instructions
                Instruction::Jmp(..) | Instruction::JmpIf(..) | Instruction::Return(..) => {
                    bb.terminator = instr;
                    let idx = blocks.len();
                    blocks.push(bb);
                    mappings.insert(label, idx);
                    label = Label::from(format!(".entry{}", label_count));
                    label_count += 1;
                    bb = BasicBlock::default();
                }
            }
        }

        BlockMap { mappings, blocks }
    }

    pub fn entry_name() -> String {
        ".entry0".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::{super::address::*, *};
    use crate::types::Type;

    #[test]
    fn test_from_instructions_only_basics_should_return_map_with_one_block() {
        // .test:
        // x = 4;
        // y = 2 + x * 3;
        // x = 42;
        // return;

        let instructions = vec![
            Instruction::Label(Label::from(".test".to_string())),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "4"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x"),
                    BinaryType::I32(IntBinaryType::Mul),
                    Address::Const(Constant::new(Type::I32, "3")),
                ),
            ),
            Instruction::Assignment(
                Address::Name("y"),
                Expression::Binary(
                    Address::Temp(TempVar::from(0)),
                    BinaryType::I32(IntBinaryType::Add),
                    Address::Const(Constant::new(Type::I32, "3")),
                ),
            ),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "42"))),
            ),
            Instruction::Return(None),
        ];

        let result = BlockMap::from_instructions(InstructionBlock(instructions));
        let mut expected = HashMap::new();
        expected.insert(Label::from(".test".to_string()), 0);

        assert_eq!(expected, result.mappings);
        assert_eq!(1, result.blocks.len());

        let expected = vec![BasicBlock {
            instructions: vec![
                Instruction::Label(Label::from(".test".to_string())),
                Instruction::Assignment(
                    Address::Name("x"),
                    Expression::Copy(Address::Const(Constant::new(Type::I32, "4"))),
                ),
                Instruction::Assignment(
                    Address::Temp(TempVar::from(0)),
                    Expression::Binary(
                        Address::Name("x"),
                        BinaryType::I32(IntBinaryType::Mul),
                        Address::Const(Constant::new(Type::I32, "3")),
                    ),
                ),
                Instruction::Assignment(
                    Address::Name("y"),
                    Expression::Binary(
                        Address::Temp(TempVar::from(0)),
                        BinaryType::I32(IntBinaryType::Add),
                        Address::Const(Constant::new(Type::I32, "3")),
                    ),
                ),
                Instruction::Assignment(
                    Address::Name("x"),
                    Expression::Copy(Address::Const(Constant::new(Type::I32, "42"))),
                ),
            ],
            terminator: Instruction::Return(None),
        }];

        assert_eq!(expected, result.blocks);
    }

    #[test]
    fn test_from_instructions_one_if_should_return_three_basic_blocks() {
        // x = 0;
        // if x == 0 {
        //     x = 1;
        // }
        // return x;

        let instructions = vec![
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x"),
                    BinaryType::I32(IntBinaryType::Eq),
                    Address::Const(Constant::new(Type::I32, "0")),
                ),
            ),
            Instruction::JmpIf(
                Address::Temp(TempVar::from(0)),
                Label::new(1),
                Label::new(0),
            ),
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "1"))),
            ),
            Instruction::Jmp(Label::new(0)),
            Instruction::Label(Label::new(0)),
            Instruction::Return(Some(Address::Name("x"))),
        ];

        let result = BlockMap::from_instructions(InstructionBlock(instructions));
        let mut expected = HashMap::new();
        expected.insert(Label::from(".entry0".to_string()), 0);
        expected.insert(Label::from(".L1".to_string()), 1);
        expected.insert(Label::from(".L0".to_string()), 2);

        assert_eq!(expected, result.mappings);
        assert_eq!(3, result.blocks.len());

        let expected = vec![
            BasicBlock {
                instructions: vec![
                    Instruction::Assignment(
                        Address::Name("x"),
                        Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
                    ),
                    Instruction::Assignment(
                        Address::Temp(TempVar::from(0)),
                        Expression::Binary(
                            Address::Name("x"),
                            BinaryType::I32(IntBinaryType::Eq),
                            Address::Const(Constant::new(Type::I32, "0")),
                        ),
                    ),
                ],
                terminator: Instruction::JmpIf(
                    Address::Temp(TempVar::from(0)),
                    Label::new(1),
                    Label::new(0),
                ),
            },
            BasicBlock {
                instructions: vec![
                    Instruction::Label(Label::new(1)),
                    Instruction::Assignment(
                        Address::Name("x"),
                        Expression::Copy(Address::Const(Constant::new(Type::I32, "1"))),
                    ),
                ],
                terminator: Instruction::Jmp(Label::new(0)),
            },
            BasicBlock {
                instructions: vec![Instruction::Label(Label::new(0))],
                terminator: Instruction::Return(Some(Address::Name("x"))),
            },
        ];

        assert_eq!(expected, result.blocks);
    }

    #[test]
    fn test_from_instructions_if_else_should_return_four_basic_blocks() {
        // x = 0;
        // if x == 0 {
        //     x = 1;
        // } else {
        //     x = 2;
        // }
        // return x;

        let instructions = vec![
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Expression::Binary(
                    Address::Name("x"),
                    BinaryType::I32(IntBinaryType::Eq),
                    Address::Const(Constant::new(Type::I32, "0")),
                ),
            ),
            Instruction::JmpIf(
                Address::Temp(TempVar::from(0)),
                Label::new(1),
                Label::new(2),
            ),
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "1"))),
            ),
            Instruction::Jmp(Label::new(0)),
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x"),
                Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
            ),
            Instruction::Jmp(Label::new(0)),
            Instruction::Label(Label::new(0)),
            Instruction::Return(Some(Address::Name("x"))),
        ];

        let result = BlockMap::from_instructions(InstructionBlock(instructions));
        let mut expected = HashMap::new();
        expected.insert(Label::from(".entry0".to_string()), 0);
        expected.insert(Label::from(".L1".to_string()), 1);
        expected.insert(Label::from(".L2".to_string()), 2);
        expected.insert(Label::from(".L0".to_string()), 3);

        assert_eq!(expected, result.mappings);
        assert_eq!(4, result.blocks.len());

        let expected = vec![
            BasicBlock {
                instructions: vec![
                    Instruction::Assignment(
                        Address::Name("x"),
                        Expression::Copy(Address::Const(Constant::new(Type::I32, "0"))),
                    ),
                    Instruction::Assignment(
                        Address::Temp(TempVar::from(0)),
                        Expression::Binary(
                            Address::Name("x"),
                            BinaryType::I32(IntBinaryType::Eq),
                            Address::Const(Constant::new(Type::I32, "0")),
                        ),
                    ),
                ],
                terminator: Instruction::JmpIf(
                    Address::Temp(TempVar::from(0)),
                    Label::new(1),
                    Label::new(2),
                ),
            },
            BasicBlock {
                instructions: vec![
                    Instruction::Label(Label::new(1)),
                    Instruction::Assignment(
                        Address::Name("x"),
                        Expression::Copy(Address::Const(Constant::new(Type::I32, "1"))),
                    ),
                ],
                terminator: Instruction::Jmp(Label::new(0)),
            },
            BasicBlock {
                instructions: vec![
                    Instruction::Label(Label::new(2)),
                    Instruction::Assignment(
                        Address::Name("x"),
                        Expression::Copy(Address::Const(Constant::new(Type::I32, "2"))),
                    ),
                ],
                terminator: Instruction::Jmp(Label::new(0)),
            },
            BasicBlock {
                instructions: vec![Instruction::Label(Label::new(0))],
                terminator: Instruction::Return(Some(Address::Name("x"))),
            },
        ];

        assert_eq!(expected, result.blocks);
    }
}
