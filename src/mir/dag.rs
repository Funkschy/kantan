use crate::{
    parse::{ast::Expr, token::Token},
    resolve::TypeMap,
    types::Type,
    Span,
};

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct DagIndex(pub usize);

#[derive(Default, Debug, Eq, PartialEq)]
pub struct Dag {
    pub nodes: Vec<DagNode>,
}

impl<'input, 'ast> Dag {
    pub(crate) fn construct(expr: &'ast Expr<'input>, types: &TypeMap<'ast, 'input>) -> Self {
        let mut dag = Dag::default();
        dag.visit(expr, types);
        dag
    }

    fn push(&mut self, node: DagNode) -> DagIndex {
        let pos = self.nodes.iter().position(|n| *n == node);

        // reuse node if possible
        let idx = if let Some(idx) = pos {
            idx
        } else {
            let idx = self.nodes.len();
            self.nodes.push(node);
            idx
        };

        DagIndex(idx)
    }

    fn get_type(types: &TypeMap<'ast, 'input>, key: &(Span, &Expr<'input>)) -> Type {
        *types
            .get(key)
            .unwrap_or_else(|| panic!("No type for {:?}", key))
    }

    fn visit(&mut self, expr: &'ast Expr<'input>, types: &TypeMap<'ast, 'input>) -> DagIndex {
        use crate::parse::ast::Expr::*;

        let node = match expr {
            Error(_) => unreachable!(),
            DecLit(lit) => DagNode::value(ValueType::I32, lit),
            StringLit(lit) => DagNode::value(ValueType::StringLit, lit),
            Ident(ident) => DagNode::value(ValueType::Name, ident),
            Negate(_, expr) => {
                let ty = Dag::get_type(types, &(expr.span, &expr.node));

                let value = self.visit(&expr.node, types);

                if ty == Type::I32 {
                    DagNode::unary(UnaryType::I32Negate, value)
                } else {
                    unreachable!()
                }
            }
            Binary(l, op, r) | BoolBinary(l, op, r) => {
                let l_ty = Dag::get_type(types, &(l.span, &l.node));

                let l_val = self.visit(&l.node, types);

                let r_ty = Dag::get_type(types, &(r.span, &r.node));

                let r_val = self.visit(&r.node, types);

                assert_eq!(l_ty, r_ty);

                let ty = match op.node {
                    Token::Plus => BinType::I32Add,
                    Token::Minus => BinType::I32Sub,
                    Token::Star => BinType::I32Mul,
                    Token::Slash => BinType::I32Div,
                    _ => unimplemented!(),
                };

                DagNode::binary(ty, l_val, r_val)
            }
            Assign { name, value, .. } => {
                let name_idx = self.push(DagNode::value(ValueType::Name, name));
                let value_idx = self.visit(&value.node, types);

                DagNode::binary(BinType::Assign, name_idx, value_idx)
            }
            Call { callee, args } => {
                let left = self.visit(&callee.node, types);

                for (i, arg) in args.0.iter().enumerate() {
                    let value_idx = self.visit(&arg.node, types);
                    let name = format!("_{}_arg_{}", callee.node, i);
                    let name_idx = self.push(DagNode::value_string(ValueType::Name, name));

                    self.push(DagNode::binary(BinType::ArgAssign, name_idx, value_idx));
                }

                DagNode::unary(UnaryType::Call, left)
            }
            // TODO: implement
            Access { .. } => unimplemented!(),
        };

        self.push(node)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum DagNode {
    Value(ValueDagNode),
    Binary(BinaryDagNode),
    Unary(UnaryDagNode),
}

impl DagNode {
    pub fn value(ty: ValueType, value: &str) -> Self {
        DagNode::Value(ValueDagNode {
            ty,
            value: value.to_owned(),
        })
    }

    pub fn value_string(ty: ValueType, value: String) -> Self {
        DagNode::Value(ValueDagNode { ty, value })
    }

    pub fn unary(ty: UnaryType, value: DagIndex) -> Self {
        DagNode::Unary(UnaryDagNode { ty, value })
    }

    pub fn binary(ty: BinType, left: DagIndex, right: DagIndex) -> Self {
        DagNode::Binary(BinaryDagNode { ty, left, right })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueDagNode {
    pub ty: ValueType,
    pub value: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ValueType {
    Name,
    I32,
    StringLit,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryDagNode {
    pub ty: UnaryType,
    pub value: DagIndex,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryType {
    I32Negate,
    Call,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryDagNode {
    pub ty: BinType,
    pub left: DagIndex,
    pub right: DagIndex,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinType {
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32Eq,
    Assign,
    // Temporary variable for a function call argument
    ArgAssign,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{
        parse::{ast::Expr, token::Token},
        types::Type,
        Span, Spanned,
    };

    #[test]
    fn test_construct_int_negation() {
        let dec = Expr::DecLit("42");

        let expr = Expr::Negate(
            Spanned::new(1, 1, Token::Minus),
            Box::new(Spanned::new(3, 4, dec.clone())),
        );

        let mut types = HashMap::new();
        types.insert((Span::new(3, 4), &dec), Type::I32);

        let dag = Dag::construct(&expr, &types);
        let expected = Dag {
            nodes: vec![
                DagNode::Value(ValueDagNode {
                    ty: ValueType::I32,
                    value: "42".to_owned(),
                }),
                DagNode::Unary(UnaryDagNode {
                    ty: UnaryType::I32Negate,
                    value: DagIndex(0),
                }),
            ],
        };

        assert_eq!(expected, dag);
    }

    #[test]
    fn test_construct_i32_add_should_reuse_node() {
        let left = Expr::DecLit("22");
        let right = Expr::DecLit("22");

        let expr = Expr::Binary(
            Box::new(Spanned::new(0, 2, left.clone())),
            Spanned::new(3, 3, Token::Plus),
            Box::new(Spanned::new(4, 5, right.clone())),
        );

        let mut types = HashMap::new();
        types.insert((Span::new(0, 2), &left), Type::I32);
        types.insert((Span::new(4, 5), &right), Type::I32);

        let dag = Dag::construct(&expr, &types);
        let expected = Dag {
            nodes: vec![
                DagNode::Value(ValueDagNode {
                    ty: ValueType::I32,
                    value: "22".to_owned(),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::I32Add,
                    left: DagIndex(0),
                    right: DagIndex(0),
                }),
            ],
        };

        assert_eq!(expected, dag);
    }

    #[test]
    fn test_construct_i32_add_should_only_reuse_equal_nodes() {
        // name=22/(42-22)

        let left = Expr::DecLit("22");
        let mid = Expr::DecLit("42");
        let right = Expr::DecLit("22");

        let r_bin = Expr::Binary(
            Box::new(Spanned::new(8, 9, mid.clone())),
            Spanned::new(10, 10, Token::Slash),
            Box::new(Spanned::new(11, 12, right.clone())),
        );

        let value = Expr::Binary(
            Box::new(Spanned::new(5, 6, left.clone())),
            Spanned::new(7, 7, Token::Minus),
            Box::new(Spanned::new(8, 13, r_bin.clone())),
        );

        let expr = Expr::Assign {
            name: "name",
            eq: Spanned::new(4, 4, Token::Equals),
            value: Box::new(Spanned::new(5, 13, value.clone())),
        };

        let mut types = HashMap::new();

        types.insert((Span::new(5, 6), &left), Type::I32);
        types.insert((Span::new(8, 9), &mid), Type::I32);
        types.insert((Span::new(11, 12), &right), Type::I32);
        types.insert((Span::new(8, 13), &r_bin), Type::I32);
        types.insert((Span::new(5, 13), &value), Type::I32);

        let dag = Dag::construct(&expr, &types);
        let expected = Dag {
            nodes: vec![
                DagNode::Value(ValueDagNode {
                    ty: ValueType::Name,
                    value: "name".to_owned(),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::I32,
                    value: "22".to_owned(),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::I32,
                    value: "42".to_owned(),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::I32Div,
                    left: DagIndex(2),
                    right: DagIndex(1),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::I32Sub,
                    left: DagIndex(1),
                    right: DagIndex(3),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::Assign,
                    left: DagIndex(0),
                    right: DagIndex(4),
                }),
            ],
        };

        assert_eq!(expected, dag);
    }

    #[test]
    fn test_construct_of_call_should_push_all_values_before_calling() {
        // func(3 + 5, 3 + 5, 42);
        //        a      b

        use crate::parse::ast::ArgList;

        let a_left = Expr::DecLit("3");
        let a_right = Expr::DecLit("5");
        let a_val = Expr::Binary(
            Box::new(Spanned::new(5, 5, a_left.clone())),
            Spanned::new(7, 7, Token::Plus),
            Box::new(Spanned::new(9, 9, a_right.clone())),
        );

        let b_left = Expr::DecLit("3");
        let b_right = Expr::DecLit("5");
        let b_val = Expr::Binary(
            Box::new(Spanned::new(12, 12, b_left.clone())),
            Spanned::new(14, 14, Token::Plus),
            Box::new(Spanned::new(16, 16, b_right.clone())),
        );

        let func_call = Expr::Call {
            callee: Box::new(Spanned::new(29, 32, Expr::Ident("func"))),
            args: ArgList(vec![
                Spanned::new(5, 9, a_val.clone()),
                Spanned::new(12, 16, b_val.clone()),
                Spanned::new(19, 19, Expr::DecLit("42")),
            ]),
        };

        let mut types = HashMap::new();

        types.insert((Span::new(5, 5), &a_left), Type::I32);
        types.insert((Span::new(9, 9), &a_right), Type::I32);
        types.insert((Span::new(5, 9), &a_val), Type::I32);

        types.insert((Span::new(12, 12), &b_left), Type::I32);
        types.insert((Span::new(16, 16), &b_right), Type::I32);
        types.insert((Span::new(12, 16), &b_val), Type::I32);

        let dag = Dag::construct(&func_call, &types);

        let expected = Dag {
            nodes: vec![
                DagNode::Value(ValueDagNode {
                    ty: ValueType::Name,
                    value: "func".to_owned(),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::I32,
                    value: "3".to_owned(),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::I32,
                    value: "5".to_owned(),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::I32Add,
                    left: DagIndex(1),
                    right: DagIndex(2),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::Name,
                    value: "_func_arg_0".to_owned(),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::ArgAssign,
                    left: DagIndex(4),
                    right: DagIndex(3),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::Name,
                    value: "_func_arg_1".to_owned(),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::ArgAssign,
                    left: DagIndex(6),
                    right: DagIndex(3),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::I32,
                    value: "42".to_owned(),
                }),
                DagNode::Value(ValueDagNode {
                    ty: ValueType::Name,
                    value: "_func_arg_2".to_owned(),
                }),
                DagNode::Binary(BinaryDagNode {
                    ty: BinType::ArgAssign,
                    left: DagIndex(9),
                    right: DagIndex(8),
                }),
                DagNode::Unary(UnaryDagNode {
                    ty: UnaryType::Call,
                    value: DagIndex(0),
                }),
            ],
        };

        assert_eq!(expected, dag);
    }
}
