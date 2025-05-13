use crate::{typedecl::TypeDecl, Span};

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
    Ident(Span<'src>),
    NameRef(Span<'src>),
    IndexedArray(Span<'src>, Box<Expression<'src>>),
    NumLiteral(i64),
    StrLiteral(String),
    PatternLiteral(String),
    Array(Expressions<'src>),
    ExitStatus(u8),
    FnInvoke {
        span: Span<'src>,
        exprs: Vec<Expression<'src>>,
        name: String,
    },
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Eq(Box<Expression<'src>>, Box<Expression<'src>>),
    NotEq(Box<Expression<'src>>, Box<Expression<'src>>),
    And(Box<Expression<'src>>, Box<Expression<'src>>),
    Or(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Negation(Box<Expression<'src>>),
}

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

pub type Expressions<'a> = Vec<Expression<'a>>;
