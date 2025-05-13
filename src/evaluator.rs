use core::panic;
use std::{
    collections::{HashMap, VecDeque},
    mem::swap,
    result,
};

use log::info;
use nom::Err;

use crate::{
    expression::{ExprEnum, Expression},
    function::{FnDef, Functions, UserFn},
    standard::{self},
    statement::{Statement, Statements},
    typedecl::{ArrayTypeDecl, TypeDecl},
    Span,
};

type Variables = HashMap<String, TypeDecl>;

#[derive(Debug)]
struct Info {
    string: String,
    unquoted_string: String,
    name: Option<String>,
    td: TypeDecl,
}

fn depth_space(depth: usize) -> String {
    "  ".repeat(depth)
}

pub struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

impl<'src> StackFrame<'src> {
    pub fn new() -> Self {
        Self {
            vars: Variables::new(),
            funcs: standard::functions(),
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

    fn get_vars(&self, name: &str) -> Option<&TypeDecl> {
        let mut next_frame = Some(self);
        while let Some(frame) = next_frame {
            if let Some(var) = frame.vars.get(name) {
                return Some(var);
            }
            next_frame = frame.uplevel;
        }
        None
    }
}

#[derive(Debug)]
pub struct Error<'src> {
    msg: String,
    pub span: Span<'src>,
}

impl<'src> std::fmt::Display for Error<'src> {
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

pub fn eval_stmts<'src>(
    stmts: &[Statement<'src>],
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    for statement in stmts {
        info!("statement: {:?}", statement);
        match statement {
            Statement::NewLine(_) => {
                lines.push(String::from("\n"));
            }
            Statement::Expression(expr) => {
                lines.push(format!(
                    "{}{}",
                    depth_space(depth),
                    eval_expression(expr, frame, depth)?.string
                ));
            }
            Statement::VarDef { span, name, td, ex } => {
                eval_var_def_statement(name, td, ex, frame, lines, depth)?;
            }
            Statement::VarAssign { span, name, ex } => {
                eval_var_assign_statement(name, ex, frame, lines, depth)?;
            }
            Statement::FnDef {
                name,
                ret_type,
                args,
                stmts,
            } => {
                eval_fn_def_statement(name, args, ret_type, stmts, frame, lines, depth)?;
            }
            Statement::Return(expr) => {
                eval_return_statement(expr, frame, lines, depth)?;
            }
            Statement::Fail(expr) => {
                eval_fail_statement(expr, frame, lines, depth)?;
            }
            Statement::If(cond, t_case, f_case) => {
                eval_if_statement(cond, t_case, f_case, frame, lines, depth)?;
            }
            Statement::For {
                span,
                name,
                from,
                to,
                stmts,
            } => {
                eval_for_statement(name, from, to, stmts, frame, lines, depth)?;
            }
            Statement::Loop { span, stmts } => {
                eval_loop_statement(span, stmts, frame, lines, depth)?;
            }
            Statement::Continue(span) => {
                lines.push(format!("{}continue", depth_space(depth)));
            }
            Statement::Break(span) => {
                lines.push(format!("{}break", depth_space(depth)));
            }
            Statement::Comment { span, comment } => {
                lines.push(format!("{}#{}", depth_space(depth), comment));
            }
            Statement::Case(ex, cases) => {
                eval_case_statement(ex, cases, frame, lines, depth)?;
            }
        }
    }

    Ok(())
}

fn eval_if_statement<'src>(
    cond: &Expression<'src>,
    t_case: &Statements<'src>,
    f_case: &Option<Statements<'src>>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    let cond_exp = eval_expression(cond, frame, depth)?;
    lines.push(format!(
        "{}if {}; then",
        depth_space(depth),
        cond_exp.string
    ));

    let mut if_statement_lines = vec![];
    let mut sub_frame = StackFrame::push_stack(frame);
    match eval_stmts(t_case, &mut sub_frame, &mut if_statement_lines, depth + 1) {
        Ok(_) => {}
        Err(err) => {
            return Err(Error {
                msg: format!("Error in if statement: {}", err),
                span: cond.span,
            });
        }
    }
    for line in if_statement_lines {
        lines.push(line);
    }

    // else
    if let Some(f_stmts) = f_case {
        let mut f_stmts_lines = vec![];
        let mut sub_frame = StackFrame::push_stack(frame);
        lines.push(format!("{}else", depth_space(depth)));
        match eval_stmts(&f_stmts, &mut sub_frame, &mut f_stmts_lines, depth + 1) {
            Ok(_) => {}
            Err(err) => {
                return Err(Error {
                    msg: format!("Error in else statement: {}", err),
                    span: cond.span,
                });
            }
        }
        for line in f_stmts_lines {
            lines.push(line);
        }
    }

    lines.push(format!("{}fi", depth_space(depth)));

    Ok(())
}

fn eval_for_statement<'src>(
    name: &&'src str,
    from: &Expression<'src>,
    to: &Expression<'src>,
    stmts: &Statements<'src>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    let from_exp = eval_expression(from, frame, depth)?;
    let to_exp = eval_expression(to, frame, depth)?;
    lines.push(format!(
        "{}for {} in $(seq {} {}); do",
        depth_space(depth),
        name,
        from_exp.string,
        to_exp.string
    ));

    let mut for_statement_lines = vec![];
    let mut sub_frame = StackFrame::push_stack(frame);
    sub_frame.vars.insert((*name).to_string(), TypeDecl::I64);
    match eval_stmts(stmts, &mut sub_frame, &mut for_statement_lines, depth + 1) {
        Ok(_) => {}
        Err(err) => {
            return Err(Error {
                msg: format!("Error in for statement: {}", err),
                span: from.span,
            });
        }
    }
    for line in for_statement_lines {
        lines.push(line);
    }
    lines.push(format!("{}done", depth_space(depth)));

    Ok(())
}

fn eval_var_def_statement<'src>(
    name: &Span<'src>,
    td: &TypeDecl,
    ex: &Expression<'src>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    let result = eval_expression(ex, frame, depth)?;
    match &result.td {
        TypeDecl::Array(_) => match &ex.expr {
            ExprEnum::FnInvoke {
                span,
                exprs,
                name: func_name,
            } => {
                if let Some(func) = frame.get_fn(func_name) {
                    match func {
                        // Native関数は関数呼び出しではなくinlineに展開されるので、
                        // User定義の関数呼び出しと区別する
                        FnDef::Native(_) => {
                            lines.push(format!(
                                "{}{}=({})",
                                depth_space(depth),
                                name,
                                result.string
                            ));
                        }
                        FnDef::User(user) => {
                            lines.push(format!(
                                "{}mapfile -t {} <<< \"{}\"",
                                depth_space(depth),
                                name,
                                result.string
                            ));
                        }
                    }
                } else {
                    panic!("function {} is not defined", func_name);
                }
            }
            ExprEnum::Ident(_) => {
                lines.push(format!(
                    "{}{}=({})",
                    depth_space(depth),
                    name,
                    result.string,
                ));
            }
            _ => {
                lines.push(format!(
                    "{}{}=({})",
                    depth_space(depth),
                    name,
                    result.string
                ));
            }
        },
        _ => {
            lines.push(format!("{}{}={}", depth_space(depth), name, result.string,));
        }
    }

    frame.vars.insert((*name).to_string(), (*td).clone());

    Ok(())
}

fn eval_var_assign_statement<'src>(
    name: &Span<'src>,
    ex: &Expression<'src>,
    frame: &StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    frame
        .get_vars(**name)
        .unwrap_or_else(|| panic!("variable {} is not defined", name));

    let result = eval_expression(ex, frame, depth)?;

    match &result.td {
        // Arrayの文字列をもらって、Arrayとして構築する
        TypeDecl::Array(_) => {
            lines.push(format!(
                "{}{}=({})",
                depth_space(depth),
                name,
                result.string
            ));
        }
        _ => {
            lines.push(format!("{}{}={}", depth_space(depth), name, result.string));
        }
    };

    Ok(())
}

fn eval_return_statement<'src>(
    expr: &Expression<'src>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    let ret = eval_expression(expr, frame, depth)?;

    if matches!(ret.td, TypeDecl::ExitStatus) {
        lines.push(format!("{}return {}", depth_space(depth), ret.string));
    } else {
        if matches!(ret.td, TypeDecl::Array(_)) {
            lines.push(format!("{}for i in {};", depth_space(depth), ret.string,));
            lines.push(format!("{}do", depth_space(depth)));
            lines.push(format!("{}echo \"$i\"", depth_space(depth + 1)));
            lines.push(format!("{}done", depth_space(depth)));
        } else {
            lines.push(format!("{}echo {}", depth_space(depth), ret.string));
        }

        lines.push(format!("{}return 0", depth_space(depth),));
    }

    Ok(())
}

fn eval_fail_statement<'src>(
    expr: &Expression<'src>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    let ret = eval_expression(expr, frame, depth)?;

    if matches!(ret.td, TypeDecl::ExitStatus) {
        lines.push(format!("{}return {}", depth_space(depth), ret.string));
    } else {
        if matches!(ret.td, TypeDecl::Array(_)) {
            lines.push(format!("{}for i in {};", depth_space(depth), ret.string,));
            lines.push(format!("{}do", depth_space(depth)));
            lines.push(format!("{}echo \"$i\"", depth_space(depth + 1)));
            lines.push(format!("{}done", depth_space(depth)));
        } else {
            lines.push(format!("{}echo {}", depth_space(depth), ret.string));
        }

        lines.push(format!("{}return 1", depth_space(depth),));
    }

    Ok(())
}

fn eval_loop_statement<'src>(
    span: &Span<'src>,
    stmts: &Statements<'src>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    lines.push("while true".to_string());
    lines.push("do".to_string());
    let mut loop_statement_lines = vec![];
    let mut sub_frame = StackFrame::push_stack(frame);
    match eval_stmts(stmts, &mut sub_frame, &mut loop_statement_lines, depth + 1) {
        Ok(_) => {}
        Err(err) => {
            return Err(Error {
                msg: format!("Error in loop statement: {}", err),
                span: *span,
            });
        }
    }
    for line in loop_statement_lines {
        lines.push(line);
    }
    lines.push(format!("{}done", depth_space(depth)));

    Ok(())
}

fn eval_case_statement<'src>(
    ex: &Expression<'src>,
    cases: &Vec<(Expression<'src>, Statements<'src>)>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
    let res = eval_expression(ex, frame, depth)?;
    lines.push(format!("{}case {} in", depth_space(depth), res.string));

    for (pattern, stmts) in cases {
        let pattern_exp = eval_expression(pattern, frame, depth)?;
        lines.push(format!("{}{})", depth_space(depth + 1), pattern_exp.string));

        let mut case_statement_lines = vec![];
        let mut sub_frame = StackFrame::push_stack(frame);
        match eval_stmts(stmts, &mut sub_frame, &mut case_statement_lines, depth + 2) {
            Ok(_) => {}
            Err(err) => {
                return Err(Error {
                    msg: format!("Error in case statement: {}", err),
                    span: pattern.span,
                });
            }
        }
        for line in case_statement_lines {
            lines.push(line);
        }
        lines.push(format!("{};; ", depth_space(depth + 2)));
    }

    lines.push(format!("{}esac", depth_space(depth)));

    Ok(())
}

fn eval_fn_def_statement<'src>(
    name: &Span<'src>,
    args: &Vec<(Span<'src>, TypeDecl)>,
    ret_type: &TypeDecl,
    stmts: &Statements<'src>,
    frame: &mut StackFrame<'src>,
    lines: &mut Vec<String>,
    depth: usize,
) -> Result<(), Error<'src>> {
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
    let mut sub_frame = StackFrame::push_stack(frame);

    for (i, (arg, td)) in args.iter().enumerate() {
        sub_frame.vars.insert((*arg).to_string(), (*td).clone());
        match td {
            TypeDecl::Array(_) => {
                lines.push(format!(
                    "{}local -n {}=\"${}\"",
                    depth_space(depth + 1),
                    *arg,
                    i + 1
                ));
            }
            _ => {
                lines.push(format!(
                    "{}local {}=\"${}\"",
                    depth_space(depth + 1),
                    *arg,
                    i + 1
                ));
            }
        }
    }
    match eval_stmts(&stmts, &mut sub_frame, &mut new_lines, depth + 1) {
        Ok(_) => {}
        Err(err) => {
            return Err(Error {
                msg: format!("Error in function {} definition: {}", name, err),
                span: *name,
            });
        }
    }
    for line in new_lines {
        lines.push(line);
    }
    lines.push(format!("}}"));

    Ok(())
}

fn eval_fn_invoke<'src>(
    name: &Span<'src>,
    args: &Vec<Expression<'src>>,
    function_name: &String,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    if let Some(func) = frame.get_fn(&function_name) {
        match func {
            FnDef::User(_) => eval_user_function(name, args, func.ret_type(), frame, depth),
            FnDef::Native(_native) => {
                eval_native_function(name, args, func.ret_type(), function_name, frame, depth)
            }
        }
    } else {
        Err(Error {
            msg: format!("Unknown function {name:?}"),
            span: *name,
        })
    }
}

fn eval_user_function<'src>(
    name: &Span<'src>,
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: Vec<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<Vec<_>, _>>()?;

    // User定義の関数におけるArray型の引数はnamerefで定義するので、
    // 直接配列を渡したりせずに、Array型の変数を渡す必要がある
    if let Some(arg) = args_exprs
        .iter()
        .find_map(|a| (matches!(a.td, TypeDecl::Array(_)) && a.name.is_none()).then_some(a))
    {
        return Err(Error {msg: format!("Array type argument must be passed as a variable name when calling the user defined function"), span: *name});
    }

    let test = args_exprs
        .into_iter()
        .map(|v| match v.td {
            // Arrayは変数の名前を渡す
            TypeDecl::Array(_) => v.name.unwrap(),
            _ => v.string,
        })
        .collect::<Vec<String>>()
        .join(" ");

    let str = match ret_type {
        TypeDecl::ExitStatus | TypeDecl::Void => {
            format!("{} {}", name, test)
        }
        _ => {
            format!("$({} {})", name, test)
        }
    };

    Ok(Info {
        string: format!("{}", str),
        unquoted_string: str.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_echo<'src>(
    name: &Span<'src>,
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: Vec<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<Vec<_>, _>>()?;

    let test = args_exprs
        .into_iter()
        .map(|v| v.string)
        .collect::<Vec<String>>()
        .join(" ");
    let str = format!("{} {}", name, test);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_capture<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let test = args_exprs
        .into_iter()
        .map(|v| v.string)
        .collect::<Vec<String>>()
        .join(" ");

    let str = format!("$({})", test);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_capture2<'src>(
    span: &Span<'src>,
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let ident = args_exprs.pop_front().unwrap();

    let test = args_exprs
        .into_iter()
        .map(|v| v.string)
        .collect::<Vec<String>>()
        .join(" ");

    match &ident.name {
        Some(name) => {
            let str = format!("{}=$({})", name, test);
            Ok(Info {
                string: str.clone(),
                unquoted_string: str.clone(),
                name: None,
                td: ret_type,
            })
        }
        None => Err(Error {
            msg: format!(
                "the first arg of capture2 must be variable name.\n{:?} not found",
                ident.string
            ),
            span: *span,
        }),
    }
}

fn eval_native_execute<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: Vec<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<Vec<_>, _>>()?;

    let test = args_exprs
        .into_iter()
        .map(|v| v.string)
        .collect::<Vec<String>>()
        .join(" ");

    let str = format!("{}", test);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_args<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: Vec<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<Vec<_>, _>>()?;

    if args_exprs.len() != 1 {
        panic!("args accept 1 argument");
    }

    let i = args_exprs.first().unwrap();

    let str = format!("${}", i.string);

    Ok(Info {
        string: format!("{}", str.clone()),
        unquoted_string: str,
        name: None,
        td: ret_type,
    })
}

fn eval_native_expanded_args<'src>(ret_type: TypeDecl, depth: usize) -> Result<Info, Error<'src>> {
    let str = format!("{}\"$@\"", depth_space(depth));
    Ok(Info {
        string: str.clone(),
        unquoted_string: str,
        name: None,
        td: ret_type,
    })
}

fn eval_native_format<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    // TODO: "{{}"のようなものを弾く
    let ret = parse_format_string(&args, frame, true, depth)?;

    Ok(Info {
        string: format!("{}", ret),
        unquoted_string: format!("{}", ret),
        name: None,
        td: ret_type,
    })
}

fn eval_native_sleep<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: Vec<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<Vec<_>, _>>()?;

    let second = args_exprs.first().unwrap();
    let str = format!("sleep {}", second.string);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_len<'src>(
    span: &Span<'src>,
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let array = args_exprs.pop_front().unwrap();
    if array.name.is_none() {
        return Err(Error {
            msg: format!("len function requires an array variable name as the first argument"),
            span: *span,
        });
    }

    match &array.name {
        Some(_name) => {
            let str = format!("${{#{}[@]}}", array.name.unwrap());

            Ok(Info {
                string: str.clone(),
                unquoted_string: str.clone(),
                name: None,
                td: ret_type,
            })
        }
        None => Err(Error {
            msg: format!("must pass an array as variable {:?}", array.string),
            span: *span,
        }),
    }
}

fn eval_native_exit_status<'src>(ret_type: TypeDecl) -> Result<Info, Error<'src>> {
    Ok(Info {
        string: "$?".to_string(),
        unquoted_string: "$?".to_string(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_split<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let ident = args_exprs.pop_front().unwrap();

    let array = args_exprs.pop_front().unwrap();
    let str = format!("mapfile -t {} <<< {}", ident.name.unwrap(), array.string);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_number_of_args<'src>(ret_type: TypeDecl) -> Result<Info, Error<'src>> {
    Ok(Info {
        string: "$#".to_string(),
        unquoted_string: "$#".to_string(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_exit<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: Vec<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<Vec<_>, _>>()?;

    let second = args_exprs.first().unwrap();
    let str = format!("exit {}", second.string);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_join<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let ident = args_exprs.pop_front().unwrap();
    let sep = args_exprs.pop_front().unwrap();

    if ident.name.is_none() {
        panic!("join function requires a variable name as the first argument");
    }

    let ident_name = ident.name.as_ref().unwrap();
    let str = format!(
        "$(printf \"{}%s\" \"${{{}[@]}}\")",
        sep.unquoted_string, ident_name
    );

    Ok(Info {
        string: format!("\"{}\"", str),
        unquoted_string: str,
        name: None,
        td: ret_type,
    })
}

fn eval_native_is_file<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let path = args_exprs.pop_front().unwrap();
    let str = format!("[ -f {} ]", path.string);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str,
        name: None,
        td: ret_type,
    })
}

fn eval_native_read<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let message = args_exprs.pop_front().unwrap();
    let ident = args_exprs.pop_front().unwrap();
    let ident_name = ident.name.as_ref().unwrap_or_else(|| {
        panic!("read function requires a variable name as the second argument");
    });
    let str = format!("read -r -p {} {}", message.string, ident_name);

    Ok(Info {
        string: str.clone(),
        unquoted_string: str,
        name: None,
        td: ret_type,
    })
}

fn eval_native_unquoted_str<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let args_exprs: Vec<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<Vec<_>, _>>()?;

    if args_exprs.len() != 1 {
        panic!("unquoted_str requires 1 argument");
    }

    let arg = args_exprs.first().unwrap();

    Ok(Info {
        string: arg.unquoted_string.clone(),
        unquoted_string: arg.unquoted_string.clone(),
        name: None,
        td: ret_type,
    })
}

fn eval_native_substr<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let ident = args_exprs.pop_front().unwrap();
    let index = args_exprs.pop_front().unwrap();

    if ident.name.is_none() {
        panic!("substr function requires a variable name as the first argument");
    }
    let ident_name = ident.name.as_ref().unwrap();

    let str = format!("${{{}:{}}}", ident_name, index.string);

    Ok(Info {
        string: format!("\"{}\"", str),
        unquoted_string: str,
        name: None,
        td: ret_type,
    })
}

fn eval_native_strlen<'src>(
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let ident = args_exprs.pop_front().unwrap();
    let name = ident.name.as_ref().unwrap_or_else(|| {
        panic!("strlen function requires a variable name as the first argument")
    });

    let str = format!("${{#{}}}", name);

    Ok(Info {
        string: format!("{}", str),
        unquoted_string: str,
        name: None,
        td: ret_type,
    })
}

fn eval_native_function<'src>(
    name: &Span<'src>,
    args: &Vec<Expression<'src>>,
    ret_type: TypeDecl,
    function_name: &String,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    match function_name.as_str() {
        "echo" => eval_native_echo(name, args, ret_type, frame, depth),
        "capture" => eval_native_capture(args, ret_type, frame, depth),
        "capture2" => eval_native_capture2(name, args, ret_type, frame, depth),
        "execute" => eval_native_execute(args, ret_type, frame, depth),
        "args" => eval_native_args(args, ret_type, frame, depth),
        "expanded_args" => eval_native_expanded_args(ret_type, depth),
        "format" => eval_native_format(args, ret_type, frame, depth),
        "sleep" => eval_native_sleep(args, ret_type, frame, depth),
        "len" => eval_native_len(name, args, ret_type, frame, depth),
        "exit_status" => eval_native_exit_status(ret_type),
        "split" => eval_native_split(args, ret_type, frame, depth),
        "number_of_args" => eval_native_number_of_args(ret_type),
        "exit" => eval_native_exit(args, ret_type, frame, depth),
        "join" => eval_native_join(args, ret_type, frame, depth),
        "is_file" => eval_native_is_file(args, ret_type, frame, depth),
        "read" => eval_native_read(args, ret_type, frame, depth),
        "unquoted_str" => eval_native_unquoted_str(args, ret_type, frame, depth),
        "substr" => eval_native_substr(args, ret_type, frame, depth),
        "strlen" => eval_native_strlen(args, ret_type, frame, depth),
        _ => Err(Error {
            msg: format!("native function {} not implemented", name),
            span: *name,
        }),
    }
}

fn eval_ident<'src>(id: &Span<'src>, frame: &StackFrame<'src>) -> Result<Info, Error<'src>> {
    if let Some(var) = frame.get_vars(**id) {
        match var {
            TypeDecl::Array(_) => Ok(Info {
                string: format!("\"${{{}[@]}}\"", *id),
                unquoted_string: format!("${{{}[@]}}", *id),
                name: Some((*id).to_string()),
                td: var.clone(),
            }),
            TypeDecl::Pattern => Ok(Info {
                string: format!("${{{}}}", *id),
                unquoted_string: format!("${{{}}}", *id),
                name: Some((*id).to_string()),
                td: var.clone(),
            }),
            _ => Ok(Info {
                string: format!("\"${{{}}}\"", *id),
                unquoted_string: format!("${{{}}}", *id),
                name: Some((*id).to_string()),
                td: var.clone(),
            }),
        }
    } else {
        Err(Error {
            msg: format!("variable {} not found", *id),
            span: *id,
        })
    }
}

fn eval_indexed_array<'src>(
    id: &Span<'src>,
    index_expr: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let index = eval_expression(&index_expr, frame, depth)?;

    let var_type_decl = frame
        .get_vars(**id)
        .unwrap_or_else(|| panic!("Variable: {:?} not found", **id));

    let value_type = match var_type_decl {
        TypeDecl::Array(ArrayTypeDecl::I64) => TypeDecl::I64,
        TypeDecl::Array(ArrayTypeDecl::Str) => TypeDecl::Str,
        _ => Err(Error {
            msg: format!(
                "indexed access is not allowed. Variable: {:?} {:?}",
                **id, var_type_decl
            ),
            span: *id,
        })?,
    };

    Ok(Info {
        string: format!("\"${{{}[{}]}}\"", *id, index.string),
        unquoted_string: format!("${{{}[{}]}}", *id, index.string),
        name: None,
        td: value_type,
    })
}

fn eval_add_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (&left.td, &right.td) {
        (TypeDecl::I64, TypeDecl::I64) => Ok(Info {
            string: format!("$(({} + {}))", left.string, right.string),
            unquoted_string: format!("$(({} + {}))", left.string, right.string),
            name: None,
            td: TypeDecl::I64,
        }),
        (TypeDecl::Str, TypeDecl::Str) => Ok(Info {
            string: format!("\"{}{}\"", left.unquoted_string, right.unquoted_string),
            unquoted_string: format!("{}{}", left.unquoted_string, right.unquoted_string),
            name: None,
            td: TypeDecl::Str,
        }),
        (TypeDecl::Array(ArrayTypeDecl::I64), TypeDecl::Array(ArrayTypeDecl::I64)) => Ok(Info {
            string: format!("{} {}", left.string, right.string),
            unquoted_string: format!("{}{}", left.string, right.string),
            name: None,
            td: TypeDecl::Array(ArrayTypeDecl::I64),
        }),
        (TypeDecl::Array(ArrayTypeDecl::Str), TypeDecl::Array(ArrayTypeDecl::Str)) => Ok(Info {
            string: format!("{} {}", left.string, right.string),
            unquoted_string: format!("{}{}", left.string, right.string),
            name: None,
            td: TypeDecl::Array(ArrayTypeDecl::Str),
        }),
        _ => Err(Error {
            msg: format!("{:?} + {:?} is not implemented", left.td, right.td),
            span: lhs.span,
        }),
    }
}

fn eval_sub_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (&left.td, &right.td) {
        (TypeDecl::I64, TypeDecl::I64) => Ok(Info {
            string: format!("$(({} - {}))", left.string, right.string),
            unquoted_string: format!("$(({} - {}))", left.string, right.string),
            name: None,
            td: TypeDecl::I64,
        }),
        _ => Err(Error {
            msg: format!("{:?} - {:?} is not implemented", left.td, right.td),
            span: lhs.span,
        }),
    }
}

fn eval_mul_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (&left.td, &right.td) {
        (TypeDecl::I64, TypeDecl::I64) => Ok(Info {
            string: format!("$(({} * {}))", left.string, right.string),
            unquoted_string: format!("$(({} * {}))", left.string, right.string),
            name: None,
            td: TypeDecl::I64,
        }),
        _ => Err(Error {
            msg: format!("{:?} * {:?} is not implemented", left.td, right.td),
            span: lhs.span,
        }),
    }
}

fn eval_eq_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (left.td, right.td) {
        (TypeDecl::I64, TypeDecl::I64) => Ok(Info {
            string: format!("[ {} -eq {} ]", left.string, right.string),
            unquoted_string: format!("[ {} -eq {} ]", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        (TypeDecl::Str, TypeDecl::Str) => Ok(Info {
            string: format!("[ {} == {} ]", left.string, right.string),
            unquoted_string: format!("[ {} == {} ]", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        _ => {
            panic!("not implemented")
        }
    }
}

fn eval_not_eq_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (left.td, right.td) {
        (TypeDecl::I64, TypeDecl::I64) => Ok(Info {
            string: format!("[ {} -ne {} ]", left.string, right.string),
            unquoted_string: format!("[ {} -ne {} ]", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        (TypeDecl::Str, TypeDecl::Str) => Ok(Info {
            string: format!("[ {} != {} ]", left.string, right.string),
            unquoted_string: format!("[ {} != {} ]", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        _ => {
            panic!("not implemented")
        }
    }
}

fn eval_and_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (left.td, right.td) {
        (TypeDecl::ExitStatus, TypeDecl::ExitStatus) => Ok(Info {
            string: format!("{} && {}", left.string, right.string),
            unquoted_string: format!("{} && {}", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        _ => {
            panic!("not implemented {} && {}", left.string, right.string)
        }
    }
}

fn eval_or_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (left.td, right.td) {
        (TypeDecl::ExitStatus, TypeDecl::ExitStatus) => Ok(Info {
            string: format!("{} || {}", left.string, right.string),
            unquoted_string: format!("{} || {}", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        _ => {
            panic!("not implemented {} || {}", left.string, right.string)
        }
    }
}

fn eval_gt_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (left.td, right.td) {
        (TypeDecl::I64, TypeDecl::I64) => Ok(Info {
            string: format!("[ {} -gt {} ]", left.string, right.string),
            unquoted_string: format!("[ {} -gt {} ]", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        _ => {
            panic!("> not implemented for {} > {}", left.td, right.td)
        }
    }
}

fn eval_lt_operation<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    let left = eval_expression(&lhs, frame, depth)?;
    let right = eval_expression(&rhs, frame, depth)?;
    match (left.td, right.td) {
        (TypeDecl::I64, TypeDecl::I64) => Ok(Info {
            string: format!("[ {} -lt {} ]", left.string, right.string),
            unquoted_string: format!("[ {} -lt {} ]", left.string, right.string),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        _ => {
            panic!("< not implemented for {} < {}", left.td, right.td)
        }
    }
}

fn eval_expression<'src>(
    ex: &Expression<'src>,
    frame: &StackFrame<'src>,
    depth: usize,
) -> Result<Info, Error<'src>> {
    use ExprEnum::*;
    match &ex.expr {
        Ident(id) => eval_ident(id, frame),
        NameRef(id) => {
            if let Some(var) = frame.get_vars(**id) {
                return Ok(Info {
                    string: (*id).to_string(),
                    unquoted_string: (*id).to_string(),
                    name: Some((*id).to_string()),
                    td: var.clone(),
                });
            } else {
                panic!("variable {} not found", *id);
            }
        }
        IndexedArray(id, index_expr) => eval_indexed_array(id, index_expr, frame, depth),
        NumLiteral(n) => Ok(Info {
            string: n.to_string(),
            unquoted_string: n.to_string(),
            name: None,
            td: TypeDecl::I64,
        }),
        ExitStatus(n) => Ok(Info {
            string: n.to_string(),
            unquoted_string: n.to_string(),
            name: None,
            td: TypeDecl::ExitStatus,
        }),
        StrLiteral(str) => {
            let str = str.replace("\"", "\\\"");
            let quote = "\"";

            Ok(Info {
                string: format!("{}{}{}", quote, str, quote),
                unquoted_string: format!("{}", str),
                name: None,
                td: TypeDecl::Str,
            })
        }
        PatternLiteral(str) => Ok(Info {
            string: format!("{}", str),
            unquoted_string: format!("{}", str),
            name: None,
            td: TypeDecl::Pattern,
        }),
        FnInvoke {
            span: name,
            exprs: args,
            name: function_name,
        } => eval_fn_invoke(name, args, function_name, frame, depth),
        Add(lhs, rhs) => eval_add_operation(lhs, rhs, frame, depth),
        Sub(lhs, rhs) => eval_sub_operation(lhs, rhs, frame, depth),
        Mul(lhs, rhs) => eval_mul_operation(lhs, rhs, frame, depth),
        Eq(lhs, rhs) => eval_eq_operation(lhs, rhs, frame, depth),
        NotEq(lhs, rhs) => eval_not_eq_operation(lhs, rhs, frame, depth),
        And(lhs, rhs) => eval_and_operation(lhs, rhs, frame, depth),
        Or(lhs, rhs) => eval_or_operation(lhs, rhs, frame, depth),
        Gt(lhs, rhs) => eval_gt_operation(lhs, rhs, frame, depth),
        Lt(lhs, rhs) => eval_lt_operation(lhs, rhs, frame, depth),
        Negation(expr) => {
            let evaluated_expr = eval_expression(expr, frame, depth)?;
            match &evaluated_expr.td {
                TypeDecl::ExitStatus => Ok(Info {
                    string: format!("! {}", evaluated_expr.string),
                    unquoted_string: format!("! {}", evaluated_expr.string),
                    name: None,
                    td: TypeDecl::ExitStatus,
                }),
                _ => {
                    panic!("Negation is only allowed for I64 type");
                }
            }
        }
        Array(exprs) => {
            let evaluated_exprs: Vec<Info> = exprs
                .iter()
                .map(|e| eval_expression(e, frame, depth))
                .collect::<Result<Vec<_>, _>>()?;

            let exprs_string = evaluated_exprs
                .into_iter()
                .map(|info| info.string)
                .collect::<Vec<String>>()
                .join(" ");

            Ok(Info {
                string: format!("{}", exprs_string),
                unquoted_string: format!("{}", exprs_string),
                name: None,
                // 空の配列を許容しているため、ここでは配列の型が分からない
                td: TypeDecl::Array(ArrayTypeDecl::Any),
            })
        }
        _ => {
            panic!("expr not implemented")
        }
    }
}

fn parse_format_string<'src>(
    args: &Vec<Expression<'src>>,
    frame: &StackFrame<'src>,
    raw: bool,
    depth: usize,
) -> Result<String, Error<'src>> {
    let mut args_exprs: VecDeque<Info> = args
        .iter()
        .map(|arg| eval_expression(arg, frame, depth))
        .collect::<Result<VecDeque<_>, _>>()?;

    let template = args_exprs.pop_front().unwrap();

    let mut chars = template.string.chars().peekable();
    let mut result = String::new();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            match chars.peek() {
                Some('{') => {
                    chars.next(); // consume second '{'
                    if let Some('}') = chars.next() {
                        if let Some('}') = chars.next() {
                            // Handle `{{}}` → "{}"
                            result.push_str("{}");
                        } else {
                            // Unmatched braces like `{{}`
                            result.push_str("{");
                            result.push_str("}");
                        }
                    } else {
                        // Just `{{` without closing
                        result.push_str("{");
                    }
                }
                Some('}') => {
                    chars.next(); // consume '}'
                    match args_exprs.pop_front() {
                        Some(v) => {
                            if raw {
                                result.push_str(&v.unquoted_string);
                            } else {
                                result.push_str(&v.string);
                            }
                        }
                        None => {
                            panic!("not values");
                        }
                    }
                }
                _ => {
                    result.push('{');
                }
            }
        } else {
            result.push(ch);
        }
    }

    Ok(result)
}
