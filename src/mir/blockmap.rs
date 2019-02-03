use std::collections::HashMap;

use super::tac::*;

#[derive(Debug)]
pub struct BlockMap<'input> {
    mappings: HashMap<Label, usize>,
    pub blocks: Vec<BasicBlock<'input>>,
}

impl<'input> BlockMap<'input> {
    pub fn from_instructions(block: InstructionBlock<'input>) -> Self {
        let mut mappings = HashMap::new();
        let mut blocks = vec![];
        let mut bb = BasicBlock::new();
        let mut label = Label::from(".entry0:".to_string());
        let mut label_count = 1;

        for instr in block.0 {
            match instr {
                Instruction::Assignment(..) | Instruction::Nop => {
                    bb.instructions.push(instr);
                }
                Instruction::Label(ref l) => {
                    if bb.instructions.len() > 0 {
                        // Terminate last block with goto
                        let term = Instruction::Jmp(l.clone());
                        bb.terminator = term;
                        let idx = blocks.len();
                        blocks.push(bb);
                        mappings.insert(label, idx);
                        // begin new block with this label
                        label = l.clone();
                        bb = BasicBlock::new();
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
                    bb = BasicBlock::new();
                }
            }
        }

        BlockMap { mappings, blocks }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
                    BinaryType::I32Mul,
                    Address::Const(Constant::new(Type::I32, "3")),
                ),
            ),
            Instruction::Assignment(
                Address::Name("y"),
                Expression::Binary(
                    Address::Temp(TempVar::from(0)),
                    BinaryType::I32Add,
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
                        BinaryType::I32Mul,
                        Address::Const(Constant::new(Type::I32, "3")),
                    ),
                ),
                Instruction::Assignment(
                    Address::Name("y"),
                    Expression::Binary(
                        Address::Temp(TempVar::from(0)),
                        BinaryType::I32Add,
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
}
