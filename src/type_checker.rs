use std::collections::HashMap;

use log::info;

use crate::{
    standard::functions,
    types::{Expression, FnDef, Statement, TypeDecl, UserFn},
};

pub fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut Context<'src>,
) -> Result<TypeDecl, TypeCheckError> {
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::VarDef(var, type_, init_expr) => {
                let init_type = tc_expr(init_expr, ctx)?;
                if let TypeDecl::Command = init_type {
                    return Err(TypeCheckError::new(format!(
                        "{:?} cannot be assigned to var",
                        init_type
                    )));
                }
                let init_type = tc_coerce_type(&init_type, type_)?;
                ctx.vars.insert(*var, init_type);
            }
            Statement::VarAssign(var, expr) => {
                let init_type = tc_expr(expr, ctx)?;
                if let TypeDecl::Command = init_type {
                    return Err(TypeCheckError::new(format!(
                        "{:?} cannot be assigned to var",
                        init_type
                    )));
                }
                let var = ctx.vars.get(*var).expect("[VarAssign]Variable not found");
                tc_coerce_type(&init_type, var)?;
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
                tc_coerce_type(&last_stmt, &ret_type)?;
            }
            Statement::Expression(e) => {
                res = tc_expr(&e, ctx)?;
            }
            Statement::Return(e) => {
                return tc_expr(e, ctx);
            }
            Statement::If(cond, _, _) => {
                tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::ExitStatus)?;
            }
        }
    }
    Ok(res)
}

#[derive(Debug)]
pub struct TypeCheckError {
    msg: String,
}

impl<'src> std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg,)
    }
}

impl TypeCheckError {
    fn new(msg: String) -> Self {
        Self { msg }
    }
}

pub struct Context<'src> {
    /// Variables table for type checking.
    vars: HashMap<&'src str, TypeDecl>,
    /// Function names are owned strings because it can be either from source or native.
    funcs: HashMap<String, FnDef<'src>>,
    super_context: Option<&'src Context<'src>>,
}

impl<'src> Context<'src> {
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

    fn push_stack(super_ctx: &'src Self) -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            super_context: Some(super_ctx),
        }
    }
}

fn tc_coerce_type<'src>(value: &TypeDecl, target: &TypeDecl) -> Result<TypeDecl, TypeCheckError> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (I64, I64) => I64,
        (Str, Str) => Str,
        (Command, Command) => Command,
        (ExitStatus, ExitStatus) => ExitStatus,
        _ => {
            return Err(TypeCheckError::new(format!(
                "{:?} cannot be assigned to {:?}",
                value, target
            )))
        }
    })
}

fn tc_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut Context<'src>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError> {
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    binary_op_type(&lhst, &rhst).map_err(|_| {
        TypeCheckError::new(format!(
            "Operation {op} between incompatible type: {:?} and {:?}",
            lhst, rhst,
        ))
    })
}

fn binary_op_type(lhs: &TypeDecl, rhs: &TypeDecl) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    Ok(match (lhs, rhs) {
        (Any, _) => Any,
        (_, Any) => Any,
        (I64, I64) => I64,
        (Str, Str) => Str,
        _ => return Err(()),
    })
}

fn tc_expr<'src>(
    e: &Expression<'src>,
    ctx: &mut Context<'src>,
) -> Result<TypeDecl, TypeCheckError> {
    use Expression::*;
    Ok(match &e {
        NumLiteral(_val) => TypeDecl::I64,
        StrLiteral(_val) => TypeDecl::Str,
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

            return Err(TypeCheckError::new(format!(
                "[tc expr]Variable {:?} not found in scope",
                str
            )));
        }
        FnInvoke(str, args) => {
            let args_ty = args
                .iter()
                .map(|v| tc_expr(v, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let func = ctx
                .get_fn(*str)
                .ok_or_else(|| TypeCheckError::new(format!("function {} is not defined", str)))?;
            let args_decl = func.args();
            for (arg_ty, decl) in args_ty.iter().zip(args_decl.iter()) {
                tc_coerce_type(&arg_ty, &decl.1)?;
            }
            func.ret_type()
        }
        Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "Add")?,
        _ => {
            panic!("not implemented")
        }
    })
}
