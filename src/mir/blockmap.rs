use std::collections::HashMap;

use super::tac::*;

#[derive(Default, Debug, PartialEq)]
pub struct BlockMap<'src> {
    pub mappings: HashMap<Label, usize>,
    pub blocks: Vec<BasicBlock<'src>>,
}

impl<'src> BlockMap<'src> {
    pub fn from_instructions(block: InstructionBlock<'src>) -> Self {
        let mut mappings = HashMap::new();
        let mut blocks = vec![];
        let mut vardecls = vec![];
        let mut bb = BasicBlock::default();
        let mut label = Label::from(BlockMap::entry_name());
        let mut label_count = 1;

        vardecls.push(label.clone().into());
        bb.instructions.push(Instruction::Nop);

        for instr in block.0 {
            match instr {
                Instruction::Assignment(..) | Instruction::Delete(..) | Instruction::Nop => {
                    bb.instructions.push(instr);
                }
                Instruction::Decl(..) => {
                    vardecls.push(instr);
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
                    // If the current block is empty, this means, that the last
                    // Instruction already terminated the block. Normally, there
                    // should already be a label Instruction inside
                    if bb.instructions.is_empty() {
                        // Continue, to ensure, that there are no Blocks, which
                        // consist only of a terminator
                        continue;
                    }
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

        let first = &mut blocks[0].instructions;
        vardecls.append(first);
        blocks[0].instructions = vardecls;

        BlockMap { mappings, blocks }
    }

    pub fn entry_name() -> String {
        ".entry0".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::{super::address::*, *};
    use crate::types::*;

    #[test]
    fn test_from_instructions_only_basics_should_return_map_with_one_block() {
        // x = 4;
        // y = 2 + x * 3;
        // x = 42;
        // return;

        let instructions = vec![
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "4",
                )))),
            ),
            Instruction::Assignment(
                Address::Temp(TempVar::from(0)),
                Box::new(Expression::Binary(
                    Address::Name("x".to_string()),
                    BinaryType::I32(NumBinaryType::Mul),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "3")),
                )),
            ),
            Instruction::Assignment(
                Address::Name("y".to_string()),
                Box::new(Expression::Binary(
                    Address::Temp(TempVar::from(0)),
                    BinaryType::I32(NumBinaryType::Add),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "3")),
                )),
            ),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "42",
                )))),
            ),
            Instruction::Return(None),
        ];

        let result = BlockMap::from_instructions(InstructionBlock(instructions));
        let mut expected = HashMap::new();
        expected.insert(Label::from(".entry0".to_string()), 0);

        assert_eq!(expected, result.mappings);
        assert_eq!(1, result.blocks.len());

        let expected = vec![BasicBlock {
            instructions: vec![
                Instruction::Label(Label::from(".entry0".to_string())),
                Instruction::Nop,
                Instruction::Assignment(
                    Address::Name("x".to_string()),
                    Box::new(Expression::Copy(Address::Const(Constant::new(
                        Type::Simple(Simple::I32),
                        "4",
                    )))),
                ),
                Instruction::Assignment(
                    Address::Temp(TempVar::from(0)),
                    Box::new(Expression::Binary(
                        Address::Name("x".to_string()),
                        BinaryType::I32(NumBinaryType::Mul),
                        Address::Const(Constant::new(Type::Simple(Simple::I32), "3")),
                    )),
                ),
                Instruction::Assignment(
                    Address::Name("y".to_string()),
                    Box::new(Expression::Binary(
                        Address::Temp(TempVar::from(0)),
                        BinaryType::I32(NumBinaryType::Add),
                        Address::Const(Constant::new(Type::Simple(Simple::I32), "3")),
                    )),
                ),
                Instruction::Assignment(
                    Address::Name("x".to_string()),
                    Box::new(Expression::Copy(Address::Const(Constant::new(
                        Type::Simple(Simple::I32),
                        "42",
                    )))),
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
                    BinaryType::I32(NumBinaryType::Eq),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                )),
            ),
            Instruction::JmpIf(
                Address::Temp(TempVar::from(0)),
                Label::new(1),
                Label::new(0),
            ),
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "1",
                )))),
            ),
            Instruction::Jmp(Label::new(0)),
            Instruction::Label(Label::new(0)),
            Instruction::Return(Some(Address::Name("x".to_string()))),
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
                    Instruction::Label(Label::from(".entry0".to_string())),
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
                            BinaryType::I32(NumBinaryType::Eq),
                            Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                        )),
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
                        Address::Name("x".to_string()),
                        Box::new(Expression::Copy(Address::Const(Constant::new(
                            Type::Simple(Simple::I32),
                            "1",
                        )))),
                    ),
                ],
                terminator: Instruction::Jmp(Label::new(0)),
            },
            BasicBlock {
                instructions: vec![Instruction::Label(Label::new(0))],
                terminator: Instruction::Return(Some(Address::Name("x".to_string()))),
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
                    BinaryType::I32(NumBinaryType::Eq),
                    Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                )),
            ),
            Instruction::JmpIf(
                Address::Temp(TempVar::from(0)),
                Label::new(1),
                Label::new(2),
            ),
            Instruction::Label(Label::new(1)),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "1",
                )))),
            ),
            Instruction::Jmp(Label::new(0)),
            Instruction::Label(Label::new(2)),
            Instruction::Assignment(
                Address::Name("x".to_string()),
                Box::new(Expression::Copy(Address::Const(Constant::new(
                    Type::Simple(Simple::I32),
                    "2",
                )))),
            ),
            Instruction::Jmp(Label::new(0)),
            Instruction::Label(Label::new(0)),
            Instruction::Return(Some(Address::Name("x".to_string()))),
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
                    Instruction::Label(Label::from(".entry0".to_string())),
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
                            BinaryType::I32(NumBinaryType::Eq),
                            Address::Const(Constant::new(Type::Simple(Simple::I32), "0")),
                        )),
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
                        Address::Name("x".to_string()),
                        Box::new(Expression::Copy(Address::Const(Constant::new(
                            Type::Simple(Simple::I32),
                            "1",
                        )))),
                    ),
                ],
                terminator: Instruction::Jmp(Label::new(0)),
            },
            BasicBlock {
                instructions: vec![
                    Instruction::Label(Label::new(2)),
                    Instruction::Assignment(
                        Address::Name("x".to_string()),
                        Box::new(Expression::Copy(Address::Const(Constant::new(
                            Type::Simple(Simple::I32),
                            "2",
                        )))),
                    ),
                ],
                terminator: Instruction::Jmp(Label::new(0)),
            },
            BasicBlock {
                instructions: vec![Instruction::Label(Label::new(0))],
                terminator: Instruction::Return(Some(Address::Name("x".to_string()))),
            },
        ];

        assert_eq!(expected, result.blocks);
    }
}
