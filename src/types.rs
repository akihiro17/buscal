#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(i64),
    StrLiteral(String),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, TypeDecl, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    FnDef {
        name: &'src str,
        args: Vec<(&'src str, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
    },
    Return(Expression<'src>),
    If(Expression<'src>, Statements<'src>, Option<Statements<'src>>),
}

pub type Statements<'a> = Vec<Statement<'a>>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
    Any,
    I64,
    Str,
    ExitStatus,
}
