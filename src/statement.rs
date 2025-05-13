use crate::{calc_offset, expression::Expression, typedecl::TypeDecl, Span};

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
        ex: Box<Expression<'src>>,
    },
    FnDef {
        name: Span<'src>,
        args: Vec<(Span<'src>, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
    },
    Return(Expression<'src>),
    Fail(Expression<'src>),
    If(Expression<'src>, Statements<'src>, Option<Statements<'src>>),
    Case(
        Expression<'src>,
        Vec<(Expression<'src>, Vec<Statement<'src>>)>,
    ),
    For {
        span: Span<'src>,
        name: Span<'src>,
        from: Expression<'src>,
        to: Expression<'src>,
        stmts: Statements<'src>,
    },
    Loop {
        span: Span<'src>,
        stmts: Statements<'src>,
    },
    Continue(Span<'src>),
    Break(Span<'src>),
    NewLine(Span<'src>),
    Comment {
        span: Span<'src>,
        comment: String,
    },
}

pub type Statements<'a> = Vec<Statement<'a>>;

impl<'src> Statement<'src> {
    pub fn span(&self) -> Option<Span<'src>> {
        use Statement::*;
        Some(match self {
            Expression(ex) => ex.span,
            VarDef { span, .. } => *span,
            VarAssign { span, .. } => *span,
            For { span, .. } => *span,
            Loop { span, stmts } => *span,
            FnDef { name, stmts, .. } => calc_offset(*name, stmts.span()),
            Return(ex) => ex.span,
            Fail(ex) => ex.span,
            If(ex, _, _) => ex.span,
            Case(ex, _) => ex.span,
            NewLine(span) => *span,
            Continue(span) => *span,
            Break(span) => *span,
            Comment { span, comment: _ } => *span,
        })
    }
}

pub trait GetSpan<'a> {
    fn span(&self) -> Span<'a>;
}

impl<'a> GetSpan<'a> for Statements<'a> {
    fn span(&self) -> Span<'a> {
        self.iter().find_map(|stmt| stmt.span()).unwrap()
    }
}
