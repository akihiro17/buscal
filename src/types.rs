use std::collections::HashMap;

use nom_locate::LocatedSpan;

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
    Ident(Span<'src>),
    NumLiteral(i64),
    StrLiteral(String),
    ExitStatus(u8),
    FnInvoke(Span<'src>, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Eq(Box<Expression<'src>>, Box<Expression<'src>>),
    NotEq(Box<Expression<'src>>, Box<Expression<'src>>),
    And(Box<Expression<'src>>, Box<Expression<'src>>),
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'a> {
    pub(crate) expr: ExprEnum<'a>,
    pub(crate) span: Span<'a>,
}

impl<'a> Expression<'a> {
    pub fn new(expr: ExprEnum<'a>, span: Span<'a>) -> Self {
        Self { expr, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef {
        span: Span<'src>,
        name: Span<'src>,
        td: TypeDecl,
        ex: Expression<'src>,
    },
    VarAssign {
        span: Span<'src>,
        name: Span<'src>,
        ex: Expression<'src>,
    },
    FnDef {
        name: Span<'src>,
        args: Vec<(Span<'src>, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
    },
    Return(Expression<'src>),
    ReturnWithStatus(Expression<'src>, Expression<'src>),
    If(Expression<'src>, Statements<'src>, Option<Statements<'src>>),
    For {
        span: Span<'src>,
        name: Span<'src>,
        from: Expression<'src>,
        to: Expression<'src>,
        stmts: Statements<'src>,
    },
}

pub type Statements<'a> = Vec<Statement<'a>>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
    Any,
    I64,
    Str,
    ExitStatus,
    Command,
}

pub enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
}

impl<'src> FnDef<'src> {
    pub fn args(&self) -> Vec<(&'src str, TypeDecl)> {
        match self {
            Self::User(user) => user.args.iter().map(|arg| (&**arg.0, arg.1)).collect(),
            Self::Native(code) => code.args.clone(),
        }
    }

    pub fn ret_type(&self) -> TypeDecl {
        match self {
            Self::User(user) => user.ret_type,
            Self::Native(native) => native.ret_type,
        }
    }
}

pub struct UserFn<'src> {
    pub args: Vec<(Span<'src>, TypeDecl)>,
    pub ret_type: TypeDecl,
    pub stmts: Statements<'src>,
}

pub struct NativeFn<'src> {
    pub args: Vec<(&'src str, TypeDecl)>,
    pub ret_type: TypeDecl,
}
