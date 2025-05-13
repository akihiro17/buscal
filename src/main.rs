use core::panic;
use std::{collections::HashMap, ffi::FromBytesUntilNulError, io::Read, sync::WaitTimeoutResult};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric1, char, line_ending, multispace0, multispace1, none_of,
        not_line_ending, space0,
    },
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult, Parser,
};

mod parser;
mod types;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    I64,
    Str,
    Ident,
    ExitStatus,
    None,
}

struct Info {
    string: String,
    value: Value,
}

enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
}
struct UserFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    stmts: Statements<'src>,
}

struct NativeFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
}

impl<'src> FnDef<'src> {
    fn args(&self) -> Vec<(&'src str, TypeDecl)> {
        match self {
            Self::User(user) => user.args.clone(),
            Self::Native(native) => return native.args.clone(),
        }
    }

    fn ret_type(&self) -> TypeDecl {
        match self {
            Self::User(user) => user.ret_type,
            Self::Native(native) => native.ret_type,
        }
    }
}

type Variables = HashMap<String, Value>;
type Functions<'src> = HashMap<String, FnDef<'src>>;
struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    fn new() -> Self {
        Self {
            vars: Variables::new(),
            funcs: standard_functions(),
            uplevel: None,
        }
    }

    fn push_stack(uplevel: &'src Self) -> Self {
        Self {
            vars: Variables::new(),
            funcs: Functions::new(),
            uplevel: Some(uplevel),
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
        let mut next_frame = Some(self);
        while let Some(frame) = next_frame {
            if let Some(func) = frame.funcs.get(name) {
                return Some(func);
            }
            next_frame = frame.uplevel;
        }
        None
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
    Any,
    I64,
    Str,
    ExitStatus,
}

fn tc_coerce_type<'src>(value: &TypeDecl, target: &TypeDecl) -> Result<TypeDecl, TypeCheckError> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (I64, I64) => I64,
        (Str, Str) => Str,
        _ => {
            return Err(TypeCheckError::new(format!(
                "{:?} cannot be assigned to {:?}",
                value, target
            )))
        }
    })
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

fn standard_functions<'src>() -> Functions<'src> {
    let mut funcs = Functions::new();
    funcs.insert(
        "echo".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Any,
        }),
    );
    funcs.insert(
        "command".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Str)],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs
}

pub struct TypeCheckContext<'src> {
    /// Variables table for type checking.
    vars: HashMap<&'src str, TypeDecl>,
    /// Function names are owned strings because it can be either from source or native.
    funcs: HashMap<String, FnDef<'src>>,
    super_context: Option<&'src TypeCheckContext<'src>>,
}

impl<'src> TypeCheckContext<'src> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: standard_functions(),
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

fn tc_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src>,
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
    ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
    use Expression::*;
    Ok(match &e {
        NumLiteral(_val) => TypeDecl::I64,
        StrLiteral(_val) => TypeDecl::Str,
        Ident(str) => ctx
            .get_var(str)
            .ok_or_else(|| TypeCheckError::new(format!("Variable {:?} not found in scope", str)))?,
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

fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut TypeCheckContext<'src>,
) -> Result<TypeDecl, TypeCheckError> {
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::VarDef(var, type_, init_expr) => {
                let init_type = tc_expr(init_expr, ctx)?;
                let init_type = tc_coerce_type(&init_type, type_)?;
                ctx.vars.insert(*var, init_type);
            }
            Statement::VarAssign(var, expr) => {
                let init_type = tc_expr(expr, ctx)?;
                let var = ctx.vars.get(*var).expect("Variable not found");
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
                let mut subctx = TypeCheckContext::push_stack(ctx);
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

fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read from stdin");
    }
    let parsed_statements = match statements_finish(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            eprintln!("Parse error: {e:?}");
            return;
        }
    };

    let mut tc_ctx = TypeCheckContext::new();

    if let Err(err) = type_check(&parsed_statements, &mut tc_ctx) {
        println!("Type check error: {err}");
        return;
    }
    println!("Type check OK");

    let mut frame = StackFrame::new();
    let mut lines = vec![];
    lines.push("#!/usr/bin/env bash".to_string());
    eval_stmts(&parsed_statements, &mut frame, &mut lines, 0);
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
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
enum Statement<'src> {
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

type Statements<'a> = Vec<Statement<'a>>;

fn depth_space(depth: usize) -> String {
    "  ".repeat(depth)
}

fn eval_stmts<'src>(
    stmts: &[Statement<'src>],
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) {
    for statement in stmts {
        match statement {
            Statement::Expression(expr) => {
                lines.push(format!(
                    "{}{}",
                    depth_space(depth),
                    convert(expr, frame).string
                ));
            }
            Statement::VarDef(name, td, expr) => {
                let result = convert(expr, frame);
                frame.vars.insert((*name).to_string(), result.value);
                lines.push(format!("{}{}={}", depth_space(depth), name, result.string));
            }
            Statement::VarAssign(name, expr) => {
                if !frame.vars.contains_key(*name) {
                    panic!("Variable is not defined");
                }
                let result = convert(expr, frame);
                frame.vars.insert((*name).to_string(), result.value);
                lines.push(format!(
                    "{}{}=\"{}\"",
                    depth_space(depth),
                    name,
                    result.string
                ));
            }
            Statement::FnDef {
                name,
                ret_type,
                args,
                stmts,
            } => {
                frame.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        ret_type: ret_type.clone(),
                        stmts: stmts.clone(),
                    }),
                );
                lines.push(format!("{}function {}() {{", depth_space(depth), name));

                let mut new_lines = vec![];
                let mut new_frame = StackFrame::push_stack(frame);

                for (i, (arg, td)) in args.iter().enumerate() {
                    lines.push(format!(
                        "{}local {}=\"${}\"",
                        depth_space(depth + 1),
                        *arg,
                        i + 1
                    ));
                }
                for (arg, td) in args {
                    match td {
                        TypeDecl::I64 => {
                            new_frame.vars.insert((*arg).to_string(), Value::I64);
                        }
                        TypeDecl::Str => {
                            new_frame.vars.insert((*arg).to_string(), Value::Str);
                        }
                        _ => {
                            panic!("not implemented")
                        }
                    }
                }
                eval_stmts(&stmts, &mut new_frame, &mut new_lines, depth + 1);
                for line in new_lines {
                    lines.push(line);
                }
                lines.push(format!("}}"));
            }
            Statement::Return(expr) => {
                let result = convert(expr, frame);
                lines.push(format!("{}echo \"{}\"", depth_space(depth), result.string));
            }
            Statement::If(cond, t_case, f_case) => {
                let cond_exp = convert(cond, frame);
                let mut new_lines = vec![];
                let mut new_frame = StackFrame::push_stack(frame);
                lines.push(format!(
                    "{}if {}; then",
                    depth_space(depth),
                    cond_exp.string
                ));
                println!("t_case: {:?}", t_case);
                eval_stmts(t_case, &mut new_frame, &mut new_lines, depth + 1);
                for line in new_lines {
                    lines.push(line);
                }
                lines.push(format!("{}fi", depth_space(depth)));
                // TODO: f_case
            }
            _ => {
                println!("{:?}", statement);
                panic!("not impletemented")
            }
        }
    }

    for line in lines {
        println!("{}", line);
    }
}

fn convert<'src>(expr: &Expression, frame: &mut StackFrame<'src>) -> Info {
    use Expression::*;
    match expr {
        Ident(id) => {
            let v = frame
                .vars
                .get(*id)
                .cloned()
                .expect(&format!("{} not found", id));
            Info {
                string: format!("${{{}}}", *id),
                value: v,
            }
        }
        NumLiteral(n) => Info {
            string: n.to_string(),
            value: Value::I64,
        },
        StrLiteral(str) => Info {
            string: format!("\"{}\"", str),
            value: Value::Str,
        },
        FnInvoke(name, args) => {
            if let Some(func) = frame.get_fn(*name) {
                match func {
                    FnDef::User(_) => {
                        let ret_type = func.ret_type().clone();
                        let new_args: Vec<Info> =
                            args.iter().map(|arg| convert(arg, frame)).collect();

                        let test = new_args
                            .iter()
                            .map(|v| v.string.clone())
                            .collect::<Vec<String>>()
                            .join(" ");
                        let str = match ret_type {
                            TypeDecl::Any => {
                                format!("{} {}", name, test)
                            }
                            _ => {
                                format!("$({} {})", name, test)
                            }
                        };
                        Info {
                            string: str,
                            value: Value::I64,
                        }
                    }
                    FnDef::Native(native) => match *name {
                        "echo" => {
                            let new_args: Vec<Info> =
                                args.iter().map(|arg| convert(arg, frame)).collect();

                            let test = new_args
                                .iter()
                                .map(|v| v.string.clone())
                                .collect::<Vec<String>>()
                                .join(" ");
                            let str = format!("{} {}", name, test);
                            Info {
                                string: str,
                                value: Value::ExitStatus,
                            }
                        }
                        "command" => {
                            let new_args: Vec<Info> =
                                args.iter().map(|arg| convert(arg, frame)).collect();

                            let test = new_args
                                .iter()
                                .map(|v| v.string.clone())
                                .collect::<Vec<String>>()
                                .join(" ");

                            let str = format!("$({})", test);
                            Info {
                                string: str,
                                value: Value::Str,
                            }
                        }
                        _ => {
                            panic!("not implemented")
                        }
                    },
                }
            } else {
                panic!("Unknown function {name:?}")
            }
        }
        Add(lhs, rhs) => {
            let left = convert(lhs, frame);
            let right = convert(rhs, frame);
            match (left.value, right.value) {
                (Value::I64, Value::I64) => Info {
                    string: format!("$(({} + {}))", left.string, right.string),
                    value: Value::I64,
                },
                (Value::Str, Value::Str) => Info {
                    string: format!("\"{}{}\"", left.string, right.string),
                    value: Value::Str,
                },
                _ => {
                    panic!("not implemented")
                }
            }
        }
        _ => {
            panic!("expr not implemented")
        }
    }
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((
        var_def,
        fn_def_statement,
        if_statement,
        terminated(
            alt((var_assign, return_statement, expr_statement)),
            char(';'),
        ),
    ))(i)
}

fn statements(i: &str) -> IResult<&str, Statements> {
    let (i, stmts) = many0(statement)(i)?;
    let (i, _) = opt(char(';'))(i)?;
    Ok((i, stmts))
}

fn statements_finish(i: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

fn factor(i: &str) -> IResult<&str, Expression> {
    alt((str_literal, number, func_call, ident, parens))(i)
}

fn func_call(i: &str) -> IResult<&str, Expression> {
    let (r, ident) = space_delimited(identifier)(i)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, space_delimited(opt(tag(","))))),
        tag(")"),
    ))(r)?;
    Ok((r, Expression::FnInvoke(ident, args)))
}

fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, res) = space_delimited(identifier)(input)?;
    Ok((r, Expression::Ident(res)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;
    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn str_literal(i: &str) -> IResult<&str, Expression> {
    let (r0, _) = preceded(multispace0, char('\"'))(i)?;
    let (r, val) = many0(none_of("\""))(r0)?;
    let (r, _) = terminated(char('"'), multispace0)(r)?;
    Ok((
        r,
        Expression::StrLiteral(
            val.iter()
                .collect::<String>()
                .replace("\\\\", "\\")
                .replace("\\n", "\n"),
        ),
    ))
}

fn parens(i: &str) -> IResult<&str, Expression> {
    space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn term(i: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(i)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!("Multiplicative expression should have '*' or '/' operator"),
        },
    )(i)
}

fn expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => {
                panic!("Additive expression should have '+' or '-' operator")
            }
        },
    )(i)
}

fn open_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(char('{'))(i)?;
    Ok((i, ()))
}

fn close_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(char('}'))(i)?;
    Ok((i, ()))
}

// fn var_def(i: &str) -> IResult<&str, Statement> {
//     let (i, _) = delimited(multispace0, tag("var"), multispace1)(i)?;
//     let (i, name) = space_delimited(identifier)(i)?;
//     let (i, _) = space_delimited(char('='))(i)?;
//     let (i, expr) = space_delimited(expr)(i)?;
//     Ok((i, Statement::VarDef(name, expr)))
// }

fn var_def(i: &str) -> IResult<&str, Statement> {
    let (i, _) = delimited(multispace0, tag("var"), multispace1)(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char(':'))(i)?;
    let (i, td) = type_decl(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(char(';'))(i)?;
    Ok((i, Statement::VarDef(name, td, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::VarAssign(name, expr)))
}

fn if_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, cond) = expr(i)?;
    // let (i, t_case) = delimited(open_brace, expr, close_brace)(i)?;
    let (i, t_case) = delimited(parser::open_brace, statements, close_brace)(i)?;
    let (i, f_case) = opt(preceded(space_delimited(tag("else")), statements))(i)?;

    Ok((i, Statement::If(cond, t_case, f_case)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, res) = expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn fn_def_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("fn"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("("))(i)?;
    let (i, args) = separated_list0(char(','), space_delimited(argument))(i)?;
    let (i, _) = space_delimited(tag(")"))(i)?;
    let (i, _) = space_delimited(tag("->"))(i)?;
    let (i, ret_type) = type_decl(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::FnDef {
            name,
            args,
            ret_type,
            stmts,
        },
    ))
}

fn return_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("return"))(i)?;
    let (i, ex) = space_delimited(expr)(i)?;
    Ok((i, Statement::Return(ex)))
}

fn argument(i: &str) -> IResult<&str, (&str, TypeDecl)> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = char(':')(i)?;
    let (i, td) = type_decl(i)?;

    Ok((i, (ident, td)))
}

fn type_decl(i: &str) -> IResult<&str, TypeDecl> {
    let (i, td) = space_delimited(identifier)(i)?;
    Ok((
        i,
        match td {
            "i64" => TypeDecl::I64,
            "str" => TypeDecl::Str,
            _ => {
                panic!("Type annotation has unknown type: {td}")
            }
        },
    ))
}
