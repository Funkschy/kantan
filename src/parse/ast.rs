use std::{cell::Cell, fmt, hash};

use super::{error::ParseError, token::Token, Spanned};
use crate::types::{Type, UserIdent};

#[derive(Debug, Eq, PartialEq)]
pub struct Program<'src>(pub Vec<TopLvl<'src>>);

#[derive(Debug, Eq, PartialEq)]
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
#[derive(Debug, Eq, PartialEq)]
pub enum Stmt<'src> {
    VarDecl(Box<VarDecl<'src>>),
    If {
        condition: Spanned<Expr<'src>>,
        then_block: Block<'src>,
        else_branch: Option<Box<Else<'src>>>,
    },
    While {
        condition: Spanned<Expr<'src>>,
        body: Block<'src>,
    },
    Return(Option<Spanned<Expr<'src>>>),
    Delete(Box<Spanned<Expr<'src>>>),
    Expr(Spanned<Expr<'src>>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct VarDecl<'src> {
    pub name: Spanned<&'src str>,
    pub value: Spanned<Expr<'src>>,
    pub eq: Spanned<Token<'src>>,
    // is filled in by resolver if necessary
    pub ty: Cell<Option<Spanned<Type<'src>>>>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Else<'src> {
    IfStmt(Box<Stmt<'src>>),
    Block(Block<'src>),
}

#[derive(Default, Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq, Hash)]
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

#[derive(Debug, Eq)]
pub struct Expr<'src> {
    // is filled in by resolver if necessary
    ty: Cell<Option<Type<'src>>>,
    kind: ExprKind<'src>,
}

impl<'src> Expr<'src> {
    pub fn new(kind: ExprKind<'src>) -> Self {
        Expr {
            ty: Cell::new(None),
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
    pub fn kind(&self) -> &ExprKind<'src> {
        &self.kind
    }

    #[inline]
    pub fn ty(&self) -> Option<Type<'src>> {
        self.ty.get()
    }

    /// This method is used by the resolver to insert type information into
    /// the Expression
    pub fn set_ty(&self, ty: Type<'src>) {
        self.ty.set(Some(ty))
    }
}

impl<'src> hash::Hash for Expr<'src> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl<'src> PartialEq for Expr<'src> {
    fn eq(&self, other: &Self) -> bool {
        self.kind().eq(other.kind())
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct InitList<'src>(pub Vec<(Spanned<&'src str>, Spanned<Expr<'src>>)>);

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum ExprKind<'src> {
    Error(ParseError<'src>),
    NullLit,
    DecLit(&'src str),
    FloatLit(&'src str),
    StringLit(&'src str),
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
        // TODO: handle function pointers
        callee: Spanned<UserIdent<'src>>,
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
            NullLit => vec![],
            SizeOf(_) => vec![],
            DecLit(_) => vec![],
            FloatLit(_) => vec![],
            StringLit(_) => vec![],
            New(expr) => vec![expr],
            Negate(_, expr) => vec![expr],
            Deref(_, expr) => vec![expr],
            Binary(l, _, r) => vec![l, r],
            BoolBinary(l, _, r) => vec![l, r],
            Ident(_) => vec![],
            Assign { left, value, .. } => vec![left, value],
            Call { args, .. } => args.0.iter().collect(),
            Access { left, .. } => vec![left],
            StructInit { fields, .. } => fields.0.iter().map(|(_, e)| e).collect(),
        }
    }
}

impl<'src> fmt::Display for Expr<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ExprKind::*;

        match self.kind() {
            Error(err) => write!(f, "{}", err),
            NullLit => write!(f, "null"),
            SizeOf(ty) => write!(f, "sizeof({})", ty),
            DecLit(lit) => write!(f, "{}", lit),
            FloatLit(lit) => write!(f, "{}", lit),
            StringLit(lit) => write!(f, "{}", lit),
            New(expr) => write!(f, "new {}", expr.node),
            Negate(_, expr) => write!(f, "-{}", expr.node),
            Deref(_, expr) => write!(f, "*{}", expr.node),
            Binary(l, op, r) => write!(f, "{}", format!("{} {} {}", l.node, op.node, r.node)),
            BoolBinary(l, op, r) => write!(f, "{}", format!("{} {} {}", l.node, op.node, r.node)),
            Ident(name) => write!(f, "{}", name),
            Assign { left, value, .. } => write!(f, "{} = {}", left.node, value.node),
            Call { callee, args } => write!(f, "{}({})", callee.node, args),
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
