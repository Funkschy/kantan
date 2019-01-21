use super::dag::Dag;
use crate::{
    parse::ast::{Block, Expr, Stmt},
    resolve::TypeMap,
    Spanned,
};

pub struct Cfg {
    blocks: Vec<BasicBlock>,
}

#[derive(Debug, PartialEq)]
pub struct BasicBlock {
    label: Label,
    statements: Vec<Statement>,
    terminator: Terminator,
}

impl<'input> BasicBlock {
    pub fn from_block(mut block_num: usize, block: &Block<'input>, types: &TypeMap) -> Vec<Self> {
        let mut blocks = vec![];

        BasicBlock::bb_from_block(block, &mut blocks, &mut block_num, types);
        blocks
    }

    fn bb_from_block(
        block: &Block,
        blocks: &mut Vec<BasicBlock>,
        block_num: &mut usize,
        types: &TypeMap,
    ) -> usize {
        let label = Label::new(*block_num);
        *block_num += 1;

        let mut current_block = BasicBlock {
            label,
            statements: vec![],
            terminator: Terminator::Exit,
        };

        for stmt in &block.0 {
            match stmt {
                Stmt::VarDecl {
                    name: Spanned { node: name, .. },
                    value,
                    eq,
                    ..
                } => {
                    let value = Expr::Assign {
                        name,
                        eq: *eq,
                        value: Box::new(value.clone()),
                    };

                    let dag = Dag::construct(&value, types);
                    current_block.statements.push(Statement(dag));
                }
                Stmt::Expr(value) => {
                    let dag = Dag::construct(&value.node, types);
                    current_block.statements.push(Statement(dag));
                }
                // TODO: else branch
                Stmt::If {
                    condition,
                    then_block,
                    ..
                } => {
                    let condition = Dag::construct(&condition.node, types);
                    let bb_idx = BasicBlock::bb_from_block(&then_block, blocks, block_num, types);
                    let then_label = blocks[bb_idx].label.clone();

                    let else_label = Label::new(*block_num);

                    current_block.terminator = Terminator::If {
                        condition,
                        then_label,
                        else_label: else_label.clone(),
                    };

                    blocks.push(current_block);

                    current_block = BasicBlock {
                        label: else_label,
                        statements: vec![],
                        terminator: Terminator::Exit,
                    };

                    *block_num += 1;
                }
            }
        }

        let idx = blocks.len();
        blocks.push(current_block);

        idx
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Label(String);

impl Label {
    fn new(block_num: usize) -> Self {
        Label(format!("L{}", block_num))
    }
}

#[derive(Debug, PartialEq)]
pub struct Statement(Dag);

#[derive(Debug, PartialEq)]
pub enum Terminator {
    If {
        condition: Dag,
        then_label: Label,
        else_label: Label,
    },
    GoTo(Label),
    Exit,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{
        mir::dag::*,
        parse::{ast::*, lexer::Lexer, parser::Parser},
        resolve::Resolver,
        Source,
    };

    #[test]
    fn test_from_block_should_return_bb_of_consecutive_statements() {
        let source = Source::new("main", "fn main() { let x = 2 + 3; let y = 2 * x + 2; }");
        let lexer = Lexer::new(&source.code);
        let mut parser = Parser::new(lexer);

        let mut programs = HashMap::new();
        let prg = parser.parse();
        programs.insert("main", (&source, &prg));

        let mut resolver = Resolver::new("main", programs);
        assert_eq!(0, resolver.resolve().len());

        let types = resolver.expr_types;

        if let Some(TopLvl::FnDecl { body, .. }) = prg.0.first() {
            let basic_blocks = BasicBlock::from_block(0, &body, &types);
            assert_eq!(1, basic_blocks.len());

            use crate::mir::dag::DagNode::*;

            let expected = vec![BasicBlock {
                label: Label("L0".to_owned()),
                statements: vec![
                    Statement(Dag {
                        nodes: vec![
                            Value(ValueDagNode {
                                ty: ValueType::Name,
                                value: "x".to_owned(),
                            }),
                            Value(ValueDagNode {
                                ty: ValueType::I32,
                                value: "2".to_owned(),
                            }),
                            Value(ValueDagNode {
                                ty: ValueType::I32,
                                value: "3".to_owned(),
                            }),
                            Binary(BinaryDagNode {
                                ty: BinType::I32Add,
                                left: DagIndex(1),
                                right: DagIndex(2),
                            }),
                            Binary(BinaryDagNode {
                                ty: BinType::Assign,
                                left: DagIndex(0),
                                right: DagIndex(3),
                            }),
                        ],
                    }),
                    Statement(Dag {
                        nodes: vec![
                            Value(ValueDagNode {
                                ty: ValueType::Name,
                                value: "y".to_owned(),
                            }),
                            Value(ValueDagNode {
                                ty: ValueType::I32,
                                value: "2".to_owned(),
                            }),
                            Value(ValueDagNode {
                                ty: ValueType::Name,
                                value: "x".to_owned(),
                            }),
                            Binary(BinaryDagNode {
                                ty: BinType::I32Mul,
                                left: DagIndex(1),
                                right: DagIndex(2),
                            }),
                            Binary(BinaryDagNode {
                                ty: BinType::I32Add,
                                left: DagIndex(3),
                                right: DagIndex(1),
                            }),
                            Binary(BinaryDagNode {
                                ty: BinType::Assign,
                                left: DagIndex(0),
                                right: DagIndex(4),
                            }),
                        ],
                    }),
                ],

                terminator: Terminator::Exit,
            }];

            assert_eq!(expected, basic_blocks);
        } else {
            panic!("No function body");
        }
    }
}
