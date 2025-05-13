use std::{
    collections::{HashMap, HashSet},
    ops::Index,
};

use crate::{
    expression::*,
    function::{FnDef, UserFn},
    standard,
    statement::{GetSpan, Statement},
    typedecl::{ArrayTypeDecl, RefTypeDecl, TypeDecl},
    Span,
};

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Eq,
    NotEq,
    And,
    Or,
    Lt,
    Gt,
}

pub fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut Context<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::NewLine(_) => {
                // nothing
            }
            Statement::VarDef { name, td, ex, .. } => {
                let init_type = tc_expr(ex, ctx)?;
                let init_type = tc_coerce_type(&init_type, td, ex.span)?;
                ctx.insert_var(**name, init_type);
            }
            Statement::VarAssign { span, name, ex } => {
                let res = tc_expr(&ex, ctx)?;
                if let Some(val) = ctx.get_var(name) {
                    tc_coerce_type(&res, &val, *span)?;
                }
            }
            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
            } => {
                // Function declaration needs to be added first to allow recursive calls
                ctx.insert_func(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        ret_type: (*ret_type).clone(),
                        stmts: stmts.clone(),
                    }),
                );
                let mut subctx = Context::push_stack(ctx);
                for (arg, ty) in args.iter() {
                    if matches!(ty, TypeDecl::Any | TypeDecl::ExitStatus) {
                        return Err(TypeCheckError::new(
                            format!("[any] is not implemented as an argument"),
                            *name,
                        ));
                    }
                    subctx.insert_var(arg, (*ty).clone());
                }

                // for void function
                let ret_type_void = matches!(ret_type, TypeDecl::Void);
                let mut last_statement_is_return = false;
                for s in stmts.iter().rev() {
                    if matches!(s, Statement::NewLine(_)) {
                        continue;
                    }

                    if matches!(s, Statement::Return(_)) {
                        last_statement_is_return = true;
                    }

                    break;
                }

                if ret_type_void && !last_statement_is_return {
                    type_check(stmts, &mut subctx)?;
                } else {
                    let last_stmt = type_check(stmts, &mut subctx)?;
                    tc_coerce_type(&last_stmt, &ret_type, stmts.span())?;
                }
            }
            Statement::Expression(e) => {
                res = tc_expr(&e, ctx)?;
            }
            Statement::Return(e) => {
                return tc_expr(e, ctx);
            }
            Statement::Fail(e) => {
                return tc_expr(e, ctx);
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
                ctx.insert_var(name, TypeDecl::I64);

                res = type_check(stmts, ctx)?;
            }
            Statement::Loop { span, stmts } => {
                res = type_check(stmts, ctx)?;
            }
            Statement::Continue(span) => {
                // nothing
            }
            Statement::Break(span) => {
                // nothing
            }
            Statement::Comment { span, comment } => {
                // nothing
            }
            Statement::Case(ex, cases) => {
                tc_coerce_type(&tc_expr(ex, ctx)?, &TypeDecl::Str, ex.span)?;

                for (pattern, stmts) in cases.iter() {
                    tc_coerce_type(&tc_expr(pattern, ctx)?, &TypeDecl::Pattern, pattern.span)?;
                    res = type_check(stmts, ctx)?;
                }
            }
        }
    }

    Ok(res)
}

#[derive(Debug)]
pub struct TypeCheckError<'src> {
    pub msg: String,
    pub span: Span<'src>,
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
    vars: HashMap<&'src str, TypeDecl>,
    /// Function names are owned strings because it can be either from source or native.
    funcs: HashMap<String, FnDef<'src>>,
    super_context: Option<&'ctx Context<'src, 'ctx>>,
}

impl<'src, 'ctx> Context<'src, 'ctx> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: standard::functions(),
            super_context: None,
        }
    }

    fn get_var(&self, name: &str) -> Option<&TypeDecl> {
        let mut next_context = Some(self);
        while let Some(context) = next_context {
            if let Some(val) = context.vars.get(name) {
                return Some(val);
            }
            next_context = context.super_context;
        }

        None
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
        if let Some(val) = self.funcs.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(&name)
        } else {
            None
        }
    }

    fn insert_var(&mut self, name: &'src str, td: TypeDecl) {
        self.vars.insert(name, td);
    }

    fn insert_func(&mut self, name: String, def: FnDef<'src>) {
        self.funcs.insert(name, def);
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
        (ExitStatus, ExitStatus) => ExitStatus,
        (Pattern, Pattern) => Pattern,
        // (Void, _) => Void,
        // (_, Void) => Void,
        (Array(ArrayTypeDecl::I64), Array(ArrayTypeDecl::I64)) => Array(ArrayTypeDecl::I64),
        (Array(ArrayTypeDecl::I64), Array(ArrayTypeDecl::Any)) => Array(ArrayTypeDecl::I64),
        (Array(ArrayTypeDecl::Str), Array(ArrayTypeDecl::Str)) => Array(ArrayTypeDecl::Str),
        (Array(ArrayTypeDecl::Str), Array(ArrayTypeDecl::Any)) => Array(ArrayTypeDecl::Str),
        (Ref(RefTypeDecl::I64), Ref(RefTypeDecl::I64)) => Ref(RefTypeDecl::I64),
        (Ref(RefTypeDecl::Str), Ref(RefTypeDecl::Str)) => Ref(RefTypeDecl::Str),
        (
            Ref(RefTypeDecl::Array(ArrayTypeDecl::I64)),
            Ref(RefTypeDecl::Array(ArrayTypeDecl::I64)),
        ) => Ref(RefTypeDecl::Array(ArrayTypeDecl::I64)),
        (
            Ref(RefTypeDecl::Array(ArrayTypeDecl::Str)),
            Ref(RefTypeDecl::Array(ArrayTypeDecl::Str)),
        ) => Ref(RefTypeDecl::Array(ArrayTypeDecl::Str)),
        (
            Ref(RefTypeDecl::Array(ArrayTypeDecl::Str)),
            Ref(RefTypeDecl::Array(ArrayTypeDecl::Any)),
        ) => Ref(RefTypeDecl::Array(ArrayTypeDecl::Str)),
        (
            Ref(RefTypeDecl::Array(ArrayTypeDecl::I64)),
            Ref(RefTypeDecl::Array(ArrayTypeDecl::Any)),
        ) => Ref(RefTypeDecl::Array(ArrayTypeDecl::I64)),
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
    op: &BinaryOp,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    binary_op_type(&lhst, &rhst, op).map_err(|_| {
        TypeCheckError::new(
            format!(
                "Operation {:?} between incompatible type: {:?} and {:?}",
                op, lhst, rhst,
            ),
            lhs.span,
        )
    })
}

fn binary_op_type(lhs: &TypeDecl, rhs: &TypeDecl, op: &BinaryOp) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    match op {
        BinaryOp::Eq | BinaryOp::NotEq => Ok(match (lhs, rhs) {
            (I64, I64) => ExitStatus,
            (Str, Str) => ExitStatus,
            _ => return Err(()),
        }),
        BinaryOp::And | BinaryOp::Or => Ok(match (lhs, rhs) {
            (ExitStatus, ExitStatus) => ExitStatus,
            _ => return Err(()),
        }),
        BinaryOp::Lt | BinaryOp::Gt => Ok(match (lhs, rhs) {
            (I64, I64) => ExitStatus,
            _ => return Err(()),
        }),
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => Ok(match (lhs, rhs) {
            (Any, _) => Any,
            (_, Any) => Any,
            (I64, I64) => I64,
            (Str, Str) => Str,
            (Array(ArrayTypeDecl::I64), Array(ArrayTypeDecl::I64)) => Array(ArrayTypeDecl::I64),
            (Array(ArrayTypeDecl::Str), Array(ArrayTypeDecl::Str)) => Array(ArrayTypeDecl::Str),
            _ => return Err(()),
        }),
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
        PatternLiteral(_) => TypeDecl::Pattern,
        Ident(str) => {
            if let Some(val) = ctx.get_var(**str) {
                return Ok((*val).clone());
            }

            return Err(TypeCheckError::new(
                format!("Variable {:?} not found in scope", **str),
                e.span,
            ));
        }
        NameRef(str) => {
            if let Some(val) = ctx.get_var(**str) {
                let ref_type = match val {
                    TypeDecl::I64 => TypeDecl::Ref(RefTypeDecl::I64),
                    TypeDecl::Str => TypeDecl::Ref(RefTypeDecl::Str),
                    TypeDecl::Array(ArrayTypeDecl::I64) => {
                        TypeDecl::Ref(RefTypeDecl::Array(ArrayTypeDecl::I64))
                    }
                    TypeDecl::Array(ArrayTypeDecl::Str) => {
                        TypeDecl::Ref(RefTypeDecl::Array(ArrayTypeDecl::Str))
                    }
                    _ => {
                        return Err(TypeCheckError::new(
                            format!("Reference to variable {:?} is not allowed", **str),
                            e.span,
                        ));
                    }
                };

                return Ok(ref_type);
            }

            return Err(TypeCheckError::new(
                format!("Variable {:?} not found in scope", **str),
                e.span,
            ));
        }
        IndexedArray(str, index) => {
            tc_coerce_type(&tc_expr(&index, ctx)?, &TypeDecl::I64, index.span)?;

            if let Some(val) = ctx.get_var(str) {
                match val {
                    TypeDecl::Array(ArrayTypeDecl::I64) => return Ok(TypeDecl::I64),
                    TypeDecl::Array(ArrayTypeDecl::Str) => return Ok(TypeDecl::Str),
                    _ => {
                        return Err(TypeCheckError::new(
                            format!(
                                "indexed access is not allowed. Variable: {:?} type: {:?}",
                                **str, val
                            ),
                            e.span,
                        ));
                    }
                }
            }

            return Err(TypeCheckError::new(
                format!("Variable {:?} not found in scope", **str),
                e.span,
            ));
        }
        FnInvoke {
            span: str,
            exprs: args,
            name: func_name,
        } => {
            let args_ty = args
                .iter()
                .map(|v| Ok((tc_expr(v, ctx)?, v.span)))
                .collect::<Result<Vec<_>, _>>()?;
            let func = ctx.get_fn(func_name).ok_or_else(|| {
                TypeCheckError::new(format!("function {} is not defined", **str), *str)
            })?;

            if func_name != "format" && args.len() != func.args().len() {
                return Err(TypeCheckError::new(
                    format!(
                        "Function {} expects {} arguments, but {} were provided",
                        *func_name,
                        func.args().len(),
                        args.len()
                    ),
                    *str,
                ));
            }

            let args_decl = func.args();
            for ((arg_ty, arg_span), decl) in args_ty.iter().zip(args_decl.iter()) {
                tc_coerce_type(&arg_ty, &decl.1, *arg_span)?;
            }

            func.ret_type()
        }
        Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::Add)?,
        Sub(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::Sub)?,
        Mul(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::Mul)?,
        Eq(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::Eq)?,
        NotEq(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::NotEq)?,
        And(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::And)?,
        Or(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::Or)?,
        Lt(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::Lt)?,
        Gt(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, &BinaryOp::Gt)?,
        Negation(ex) => {
            let td = tc_expr(ex, ctx)?;
            tc_coerce_type(&TypeDecl::ExitStatus, &td, ex.span)?
        }

        Array(exprs) => {
            let mut type_decls = HashSet::new();
            for ex in exprs.iter() {
                let td = tc_expr(&ex, ctx)?;
                type_decls.insert(td);
            }

            if type_decls.len() > 1 {
                return Err(TypeCheckError::new(
                    format!("The types of elements in an array must be same"),
                    e.span,
                ));
            } else if type_decls.is_empty() {
                return Ok(TypeDecl::Any);
            }

            let s = type_decls.into_iter().collect::<Vec<TypeDecl>>();
            let array_type_decl = s.first().unwrap();
            match array_type_decl {
                TypeDecl::I64 => return Ok(TypeDecl::Array(ArrayTypeDecl::I64)),
                TypeDecl::Str => return Ok(TypeDecl::Array(ArrayTypeDecl::Str)),
                _ => {
                    return Err(TypeCheckError::new(
                        format!("The types of elements in an array must be I64 or Str"),
                        e.span,
                    ));
                }
            }
        }
        _ => {
            panic!("{:?} not implemented", e)
        }
    })
}
