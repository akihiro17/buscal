use std::collections::HashMap;

use log::info;
use nom::{InputTake, Offset};
use nom_locate::LocatedSpan;

use crate::{
    standard::{self, functions},
    types::{ExprEnum, Expression, FnDef, Span, Statement, Statements, TypeDecl, UserFn},
};

impl<'src> Statement<'src> {
    fn span(&self) -> Option<Span<'src>> {
        use Statement::*;
        Some(match self {
            Expression(ex) => ex.span,
            VarDef { span, .. } => *span,
            VarAssign { span, .. } => *span,
            For { span, .. } => *span,
            FnDef { name, stmts, .. } => calc_offset(*name, stmts.span()),
            Return(ex) => ex.span,
            ReturnWithStatus(ex, _) => ex.span,
            If(ex, _, _) => ex.span,
        })
    }
}

trait GetSpan<'a> {
    fn span(&self) -> Span<'a>;
}

impl<'a> GetSpan<'a> for Statements<'a> {
    fn span(&self) -> Span<'a> {
        self.iter().find_map(|stmt| stmt.span()).unwrap()
    }
}

pub fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut Context<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::VarDef { name, td, ex, .. } => {
                let init_type = tc_expr(ex, ctx)?;
                if let TypeDecl::Command = init_type {
                    return Err(TypeCheckError::new(
                        format!("{:?} cannot be assigned to var", init_type),
                        ex.span,
                    ));
                }
                let init_type = tc_coerce_type(&init_type, td, ex.span)?;
                ctx.vars.insert(**name, init_type);
            }
            Statement::VarAssign { span, name, ex } => {
                let init_type = tc_expr(ex, ctx)?;
                if let TypeDecl::Command = init_type {
                    return Err(TypeCheckError::new(
                        format!("{:?} cannot be assigned to var", init_type),
                        ex.span,
                    ));
                }
                let var = ctx.vars.get(**name).expect("[VarAssign]Variable not found");
                tc_coerce_type(&init_type, var, ex.span)?;
            }
            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
            } => {
                // Function declaration needs to be added first to allow recursive calls
                ctx.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        ret_type: *ret_type,
                        stmts: stmts.clone(),
                    }),
                );
                let mut subctx = Context::push_stack(ctx);
                for (arg, ty) in args.iter() {
                    subctx.vars.insert(arg, *ty);
                }
                let last_stmt = type_check(stmts, &mut subctx)?;
                tc_coerce_type(&last_stmt, &ret_type, stmts.span())?;
            }
            Statement::Expression(e) => {
                res = tc_expr(&e, ctx)?;
            }
            Statement::Return(e) => {
                return tc_expr(e, ctx);
            }
            Statement::ReturnWithStatus(e1, e2) => {
                tc_coerce_type(&tc_expr(e2, ctx)?, &TypeDecl::ExitStatus, e2.span)?;

                return tc_expr(e1, ctx);
            }
            Statement::If(cond, _, _) => {
                tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::ExitStatus, cond.span)?;
            }
            Statement::For {
                span,
                name,
                from,
                to,
                stmts,
            } => {
                tc_coerce_type(&tc_expr(from, ctx)?, &TypeDecl::I64, from.span)?;
                tc_coerce_type(&tc_expr(to, ctx)?, &TypeDecl::I64, to.span)?;
                ctx.vars.insert(name, TypeDecl::I64);

                res = type_check(stmts, ctx)?;
            }
        }
    }
    Ok(res)
}

#[derive(Debug)]
pub struct TypeCheckError<'src> {
    msg: String,
    span: Span<'src>,
}

impl<'src> std::fmt::Display for TypeCheckError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\nlocation: {}:{}: {}",
            self.msg,
            self.span.location_line(),
            self.span.get_utf8_column(),
            self.span.fragment()
        )
    }
}

impl<'src> TypeCheckError<'src> {
    fn new(msg: String, span: Span<'src>) -> Self {
        Self { msg, span }
    }
}

pub struct Context<'src, 'ctx> {
    /// Variables table for type checking.
    pub vars: HashMap<&'src str, TypeDecl>,
    /// Function names are owned strings because it can be either from source or native.
    pub funcs: HashMap<String, FnDef<'src>>,
    pub super_context: Option<&'ctx Context<'src, 'ctx>>,
}

impl<'src, 'ctx> Context<'src, 'ctx> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: functions(),
            super_context: None,
        }
    }

    fn get_var(&self, name: &str) -> Option<TypeDecl> {
        if let Some(val) = self.vars.get(name) {
            Some(val.clone())
        } else {
            None
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
        if let Some(val) = self.funcs.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(name)
        } else {
            None
        }
    }

    fn push_stack(super_ctx: &'ctx Self) -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            super_context: Some(super_ctx),
        }
    }
}

fn tc_coerce_type<'src>(
    value: &TypeDecl,
    target: &TypeDecl,
    span: Span<'src>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (I64, I64) => I64,
        (Str, Str) => Str,
        (Command, Command) => Command,
        (ExitStatus, ExitStatus) => ExitStatus,
        _ => {
            return Err(TypeCheckError::new(
                format!("{:?} cannot be assigned to {:?}", value, target),
                span,
            ))
        }
    })
}

fn tc_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut Context<'src, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    binary_op_type(&lhst, &rhst, op).map_err(|_| {
        TypeCheckError::new(
            format!(
                "Operation {op} between incompatible type: {:?} and {:?}",
                lhst, rhst,
            ),
            lhs.span,
        )
    })
}

fn binary_op_type(lhs: &TypeDecl, rhs: &TypeDecl, op: &str) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    if op == "Eq" || op == "NotEq" {
        Ok(match (lhs, rhs) {
            (I64, I64) => ExitStatus,
            (Str, Str) => ExitStatus,
            _ => return Err(()),
        })
    } else if op == "And" {
        Ok(match (lhs, rhs) {
            (ExitStatus, ExitStatus) => ExitStatus,
            _ => return Err(()),
        })
    } else {
        Ok(match (lhs, rhs) {
            (Any, _) => Any,
            (_, Any) => Any,
            (I64, I64) => I64,
            (Str, Str) => Str,
            _ => return Err(()),
        })
    }
}

fn tc_expr<'src>(
    e: &Expression<'src>,
    ctx: &mut Context<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use ExprEnum::*;
    Ok(match &e.expr {
        NumLiteral(_val) => TypeDecl::I64,
        StrLiteral(_val) => TypeDecl::Str,
        ExitStatus(_val) => TypeDecl::ExitStatus,
        Ident(str) => {
            if let Some(val) = ctx.get_var(str) {
                return Ok(val);
            }

            let mut c = ctx.super_context;
            loop {
                match c {
                    Some(s) => {
                        if let Some(val) = s.get_var(str) {
                            return Ok(val);
                        }
                        c = s.super_context;
                    }
                    None => break,
                }
            }

            return Err(TypeCheckError::new(
                format!("[tc expr]Variable {:?} not found in scope", str),
                e.span,
            ));
        }
        FnInvoke(str, args) => {
            let args_ty = args
                .iter()
                .map(|v| Ok((tc_expr(v, ctx)?, v.span)))
                .collect::<Result<Vec<_>, _>>()?;
            let func = ctx.get_fn(**str).ok_or_else(|| {
                TypeCheckError::new(format!("function {} is not defined", str), *str)
            })?;
            let args_decl = func.args();
            for ((arg_ty, arg_span), decl) in args_ty.iter().zip(args_decl.iter()) {
                tc_coerce_type(&arg_ty, &decl.1, *arg_span)?;
            }
            func.ret_type()
        }
        Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Add")?,
        Eq(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Eq")?,
        NotEq(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "NotEq")?,
        And(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "And")?,
        _ => {
            panic!("not implemented")
        }
    })
}

/// Calculate offset between the start positions of the input spans and return a span between them.
///
/// Note: `i` shall start earlier than `r`, otherwise wrapping would occur.
pub fn calc_offset<'a>(i: Span<'a>, r: Span<'a>) -> Span<'a> {
    i.take(i.offset(&r))
}
