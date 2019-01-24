use super::dag::Dag;
use crate::{
    parse::ast::{Else, Expr, Stmt},
    resolve::TypeMap,
    types::Type,
};

pub struct Cfg<'ast, 'input> {
    pub(crate) blocks: Vec<BasicBlock>,
    temp_counter: usize,
    label_counter: usize,
    types: &'ast TypeMap<'ast, 'input>,
}

impl<'ast, 'input> Cfg<'ast, 'input> {
    pub fn function(
        name: &str,
        _params: Vec<Type>,
        body: Vec<Stmt<'input>>,
        types: &'ast TypeMap<'ast, 'input>,
    ) -> Self {
        let mut cfg = Cfg {
            blocks: vec![],
            temp_counter: 0,
            label_counter: 0,
            types,
        };

        let label = Label::from(name);
        cfg.block_label(label, body);
        cfg
    }

    fn handle_if(
        &mut self,
        condition: &Expr<'input>,
        then_block: Vec<Stmt<'input>>,
        else_label: Label,
    ) -> Terminator {
        let condition = Dag::construct_assign(self.temp(), condition, self.types);
        self.block(then_block);

        let then_label = self
            .blocks
            .last()
            .map(|bb| bb.label.clone())
            .expect("Empty blocklist");

        Terminator::If {
            condition,
            then_label,
            else_label,
        }
    }

    fn block(&mut self, body: Vec<Stmt<'input>>) {
        let label = self.label();
        self.block_label(label, body)
    }

    fn block_label(&mut self, label: Label, mut body: Vec<Stmt<'input>>) {
        let mut bb = BasicBlock::new(label);

        while !body.is_empty() {
            let s = body.remove(0);
            match s {
                Stmt::Expr(val) => self.expr(&val.node, &mut bb.statements),
                Stmt::VarDecl { name, value, .. } => {
                    self.assign_dag(&name.node, &value.node, &mut bb.statements)
                }
                Stmt::If {
                    condition,
                    then_block,
                    else_branch,
                } => {
                    let mut else_statements = if let Some(else_branch) = else_branch {
                        match *else_branch {
                            Else::IfStmt(if_stmt) => vec![if_stmt],
                            Else::Block(block) => block.0,
                        }
                    } else {
                        vec![]
                    };

                    let else_label = self.label();
                    bb.terminator =
                        self.handle_if(&condition.node, then_block.0, else_label.clone());

                    // push block until if
                    self.blocks.push(bb);

                    // begin new block for else
                    bb = BasicBlock::new(else_label);
                    body.append(&mut else_statements);
                }
            }
        }

        self.blocks.push(bb);
    }

    fn label(&mut self) -> Label {
        let num = self.label_counter;
        self.label_counter += 1;
        Label(format!("_L{}", num))
    }

    fn temp(&mut self) -> String {
        let num = self.temp_counter;
        self.temp_counter += 1;
        format!("_t{}", num)
    }

    #[inline]
    fn expr(&self, expr: &Expr<'input>, statements: &mut Vec<Statement>) {
        statements.push(Statement(Dag::construct(expr, self.types)));
    }

    #[inline]
    fn assign_dag(&self, name: &str, value: &'ast Expr<'input>, statements: &mut Vec<Statement>) {
        statements.push(Statement(Dag::construct_assign(
            name.to_owned(),
            value,
            self.types,
        )));
    }
}

#[derive(Debug, PartialEq)]
pub struct BasicBlock {
    label: Label,
    statements: Vec<Statement>,
    terminator: Terminator,
}

impl BasicBlock {
    pub fn new(label: Label) -> Self {
        BasicBlock {
            label,
            statements: vec![],
            terminator: Terminator::Return,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Label(String);

impl Label {
    fn new(block_num: usize) -> Self {
        Label(format!("L{}", block_num))
    }
}

impl From<&str> for Label {
    fn from(value: &str) -> Label {
        Label(value.to_owned())
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
    Return,
}
