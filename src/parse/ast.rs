use std::{
    cell::{Cell, Ref, RefCell},
    fmt,
};

use super::{error::ParseError, token::Token, Spanned};
use crate::types::{Type, UserIdent};

#[derive(Debug, PartialEq)]
pub struct Program<'src>(pub Vec<TopLvl<'src>>);

#[derive(Debug, PartialEq)]
pub enum TopLvl<'src> {
    FuncDecl {
        name: Spanned<&'src str>,
        params: ParamList<'src>,
        body: Block<'src>,
        ret_type: Spanned<Type<'src>>,
        is_extern: bool,
    },
    Import {
        name: Spanned<&'src str>,
    },
    TypeDef(TypeDef<'src>),
    Error(Spanned<ParseError<'src>>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeDef<'src> {
    StructDef {
        name: Spanned<&'src str>,
        fields: Vec<(Spanned<&'src str>, Spanned<Type<'src>>)>,
    },
}

// TODO: refactor Spanned<&'src str> to identifier
#[derive(Debug, PartialEq)]
pub enum Stmt<'src> {
    VarDecl(Box<VarDecl<'src>>),
    If(Box<IfStmt<'src>>),
    While {
        condition: Spanned<Expr<'src>>,
        body: Block<'src>,
    },
    Return(Option<Spanned<Expr<'src>>>),
    Delete(Box<Spanned<Expr<'src>>>),
    Expr(Spanned<Expr<'src>>),
}

#[derive(Debug, PartialEq)]
pub struct IfStmt<'src> {
    pub condition: Spanned<Expr<'src>>,
    pub then_block: Block<'src>,
    pub else_branch: Option<Box<Else<'src>>>,
}

#[derive(Debug, PartialEq)]
pub struct VarDecl<'src> {
    pub name: Spanned<&'src str>,
    pub value: Spanned<Expr<'src>>,
    pub eq: Spanned<Token<'src>>,
    // is filled in by resolver if necessary
    pub ty: RefCell<Option<Spanned<Type<'src>>>>,
}

#[derive(Debug, PartialEq)]
pub enum Else<'src> {
    IfStmt(Box<Stmt<'src>>),
    Block(Block<'src>),
}

#[derive(Default, Debug, PartialEq)]
pub struct Block<'src>(pub Vec<Stmt<'src>>);

#[derive(Debug, Eq, PartialEq, Default)]
pub struct ParamList<'src> {
    pub varargs: bool,
    pub params: Vec<Param<'src>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Param<'src>(pub Spanned<&'src str>, pub Spanned<Type<'src>>);

impl<'src> Param<'src> {
    pub fn new(ident: Spanned<&'src str>, ty: Spanned<Type<'src>>) -> Self {
        Param(ident, ty)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ArgList<'src>(pub Vec<Spanned<Expr<'src>>>);

impl<'src> fmt::Display for ArgList<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strings: Vec<String> = self
            .0
            .iter()
            .map(|Spanned { node, .. }| node.to_string())
            .collect();

        write!(f, "{}", strings.join(", "))
    }
}

#[derive(Debug)]
pub struct Expr<'src> {
    // is filled in by resolver if necessary
    ty: RefCell<Option<Type<'src>>>,
    kind: ExprKind<'src>,
}

impl<'src> Expr<'src> {
    pub fn new(kind: ExprKind<'src>) -> Self {
        Expr {
            ty: RefCell::new(None),
            kind,
        }
    }

    pub fn is_err(&self) -> bool {
        if let ExprKind::Error(..) = self.kind {
            return true;
        }
        false
    }

    #[inline]
    pub fn ty(&self) -> Ref<Option<Type<'src>>> {
        self.ty.borrow()
    }

    #[inline]
    pub fn clone_ty(&self) -> Option<Type<'src>> {
        self.ty.borrow().clone()
    }

    #[inline]
    pub fn kind(&self) -> &ExprKind<'src> {
        &self.kind
    }

    /// This method is used by the resolver to insert type information into
    /// the Expression
    pub fn set_ty(&self, ty: Type<'src>) {
        self.ty.replace(Some(ty));
    }
}

impl<'src> PartialEq for Expr<'src> {
    fn eq(&self, other: &Self) -> bool {
        self.kind().eq(other.kind())
    }
}

#[derive(Debug, PartialEq)]
pub struct InitList<'src>(pub Vec<(Spanned<&'src str>, Spanned<Expr<'src>>)>);

#[derive(Debug, PartialEq)]
pub enum ExprKind<'src> {
    Error(ParseError<'src>),
    NullLit,
    DecLit(&'src str),
    FloatLit(&'src str),
    StringLit(&'src str),
    Ref(Spanned<Token<'src>>, Box<Spanned<Expr<'src>>>),
    Deref(Spanned<Token<'src>>, Box<Spanned<Expr<'src>>>),
    Negate(Spanned<Token<'src>>, Box<Spanned<Expr<'src>>>),
    Binary(
        Box<Spanned<Expr<'src>>>,
        Spanned<Token<'src>>,
        Box<Spanned<Expr<'src>>>,
    ),
    BoolBinary(
        Box<Spanned<Expr<'src>>>,
        Spanned<Token<'src>>,
        Box<Spanned<Expr<'src>>>,
    ),
    Ident(&'src str),
    New(Box<Spanned<Expr<'src>>>),
    Assign {
        left: Box<Spanned<Expr<'src>>>,
        eq: Spanned<Token<'src>>,
        value: Box<Spanned<Expr<'src>>>,
    },
    Call {
        module: Cell<&'src str>,
        callee: Box<Spanned<Expr<'src>>>,
        args: ArgList<'src>,
    },
    Access {
        left: Box<Spanned<Expr<'src>>>,
        identifier: Spanned<&'src str>,
    },
    StructInit {
        identifier: Spanned<UserIdent<'src>>,
        fields: InitList<'src>,
    },
    SizeOf(Type<'src>),
}

impl<'src> Expr<'src> {
    /// Gets the sub expressions of this expression
    pub fn sub_exprs(&self) -> Vec<&Spanned<Expr<'src>>> {
        use self::ExprKind::*;

        match self.kind() {
            Error(_) => panic!(),
            NullLit | SizeOf(_) | DecLit(_) | FloatLit(_) | StringLit(_) | Ident(_) => vec![],
            New(expr) | Negate(_, expr) | Deref(_, expr) | Ref(_, expr) => vec![expr],
            Binary(l, _, r) => vec![l, r],
            BoolBinary(l, _, r) => vec![l, r],
            Assign { left, value, .. } => vec![left, value],
            Call { args, callee, .. } => {
                if let ExprKind::Ident(_) = callee.node.kind() {
                    args.0.iter().collect()
                } else {
                    let mut vec = Vec::with_capacity(args.0.len() + 1);

                    vec.push(callee.as_ref());
                    for a in &args.0 {
                        vec.push(a);
                    }

                    vec
                }
            }
            Access { left, .. } => vec![left],
            StructInit { fields, .. } => fields.0.iter().map(|(_, e)| e).collect(),
        }
    }

    pub fn is_r_value(&self) -> bool {
        use self::ExprKind::*;

        match self.kind() {
            Ident(_) | Access { .. } => false,
            _ => true,
        }
    }

    #[inline]
    pub fn is_l_value(&self) -> bool {
        !self.is_r_value()
    }
}

impl<'src> fmt::Display for Expr<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ExprKind::*;

        match self.kind() {
            Error(err) => write!(f, "{}", err),
            DecLit(lit) | FloatLit(lit) | StringLit(lit) => write!(f, "{}", lit),
            NullLit => write!(f, "null"),
            SizeOf(ty) => write!(f, "sizeof({})", ty),
            New(expr) => write!(f, "new {}", expr.node),
            Negate(_, expr) => write!(f, "-{}", expr.node),
            Deref(_, expr) => write!(f, "*{}", expr.node),
            Ref(_, expr) => write!(f, "&{}", expr.node),
            Binary(l, op, r) | BoolBinary(l, op, r) => {
                write!(f, "{}", format!("{} {} {}", l.node, op.node, r.node))
            }
            Ident(name) => write!(f, "{}", name),
            Assign { left, value, .. } => write!(f, "{} = {}", left.node, value.node),
            Call { callee, args, .. } => write!(f, "{}({})", callee.node, args),
            Access { left, identifier } => write!(f, "{}.{}", left.node, identifier.node),
            StructInit { identifier, fields } => write!(
                f,
                "{} {{ {} }}",
                identifier.node,
                fields
                    .0
                    .iter()
                    .map(|(n, e)| format!("{}: {}", n.node, e.node))
                    .collect::<Vec<String>>()
                    .join(",\n")
            ),
        }
    }
}
