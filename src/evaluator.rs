use std::collections::{HashMap, VecDeque};

use crate::{
    standard,
    types::{Expression, FnDef, Statement, TypeDecl, UserFn},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I64,
    Str,
    Ident,
    ExitStatus,
    None,
}
pub type Variables = HashMap<String, Value>;
pub type Functions<'src> = HashMap<String, FnDef<'src>>;
struct Info {
    string: String,
    raw_string: String,
    value: Value,
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
}

pub fn eval_stmts<'src>(
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
                    eval_expression(expr, frame).string
                ));
            }
            Statement::VarDef(name, td, expr) => {
                let result = eval_expression(expr, frame);
                frame.vars.insert((*name).to_string(), result.value);
                lines.push(format!("{}{}={}", depth_space(depth), name, result.string));
            }
            Statement::VarAssign(name, expr) => {
                if !frame.vars.contains_key(*name) {
                    panic!("Variable is not defined");
                }
                let result = eval_expression(expr, frame);
                frame.vars.insert((*name).to_string(), result.value);
                lines.push(format!("{}{}={}", depth_space(depth), name, result.string));
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
                let result = eval_expression(expr, frame);
                lines.push(format!("{}echo {}", depth_space(depth), result.string));
            }
            Statement::If(cond, t_case, f_case) => {
                let cond_exp = eval_expression(cond, frame);
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

fn eval_expression<'src>(expr: &Expression, frame: &mut StackFrame<'src>) -> Info {
    use Expression::*;
    match expr {
        Ident(id) => {
            if let Some(v) = frame.vars.get(*id) {
                return Info {
                    string: format!("\"${{{}}}\"", *id),
                    raw_string: format!("${{{}}}", *id),
                    value: v.clone(),
                };
            }

            let c = frame.uplevel;
            loop {
                match c {
                    Some(s) => {
                        if let Some(v) = s.vars.get(*id) {
                            return Info {
                                string: format!("\"${{{}}}\"", *id),
                                raw_string: format!("${{{}}}", *id),
                                value: v.clone(),
                            };
                        }
                    }
                    None => panic!("{} not found", *id),
                }
            }
        }
        NumLiteral(n) => Info {
            string: n.to_string(),
            raw_string: n.to_string(),
            value: Value::I64,
        },
        StrLiteral(str) => {
            let str = str.replace("\"", "\\\"");
            Info {
                string: format!("\"{}\"", str),
                raw_string: str.to_string(),
                value: Value::Str,
            }
        }
        FnInvoke(name, args) => {
            if let Some(func) = frame.get_fn(*name) {
                match func {
                    FnDef::User(_) => {
                        let ret_type = func.ret_type().clone();
                        let new_args: Vec<Info> =
                            args.iter().map(|arg| eval_expression(arg, frame)).collect();

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
                            string: str.clone(),
                            raw_string: str,
                            value: Value::I64,
                        }
                    }
                    FnDef::Native(native) => match *name {
                        "echo" => {
                            let new_args: Vec<Info> =
                                args.iter().map(|arg| eval_expression(arg, frame)).collect();

                            let test = new_args
                                .iter()
                                .map(|v| v.string.clone())
                                .collect::<Vec<String>>()
                                .join(" ");
                            let str = format!("{} {}", name, test);
                            Info {
                                string: str.clone(),
                                raw_string: str,
                                value: Value::ExitStatus,
                            }
                        }
                        "capture" => {
                            let mut new_args: VecDeque<Info> =
                                args.iter().map(|arg| eval_expression(arg, frame)).collect();

                            let test = new_args
                                .iter()
                                .map(|v| v.string.clone())
                                .collect::<Vec<String>>()
                                .join(" ");

                            let str = format!("$({})", test);
                            Info {
                                string: str.clone(),
                                raw_string: str,
                                value: Value::Str,
                            }
                        }
                        "execute" => {
                            let new_args: Vec<Info> =
                                args.iter().map(|arg| eval_expression(arg, frame)).collect();

                            let mut test = new_args
                                .iter()
                                .map(|v| v.string.clone())
                                .collect::<Vec<String>>()
                                .join(" ");

                            println!("test: {}", test);
                            test.remove(test.len() - 1);
                            if !test.is_empty() {
                                test.remove(0);
                            }
                            println!("test: {}", test);

                            let str = format!("{}", test);
                            Info {
                                string: str.clone(),
                                raw_string: str,
                                value: Value::Str,
                            }
                        }
                        "args" => {
                            let new_args: Vec<Info> =
                                args.iter().map(|arg| eval_expression(arg, frame)).collect();

                            if new_args.len() != 1 {
                                panic!("args accept 1 argument");
                            }

                            let i = new_args.first().unwrap();

                            let str = format!("${}", i.string);
                            Info {
                                string: str.clone(),
                                raw_string: str,
                                value: Value::Str,
                            }
                        }
                        "format" => {
                            // TODO: "{{}"のようなものを弾く
                            let ret = parse_format_string(args, frame, true);
                            Info {
                                string: format!("\"{}\"", ret.clone()),
                                raw_string: ret,
                                value: Value::Str,
                            }
                        }
                        "command" => {
                            let new_args: VecDeque<Info> =
                                args.iter().map(|arg| eval_expression(arg, frame)).collect();

                            let test = new_args
                                .iter()
                                .map(|v| v.string.clone())
                                .collect::<Vec<String>>()
                                .join(" ");

                            let str = format!("{}", test);
                            Info {
                                string: str.clone(),
                                raw_string: str,
                                value: Value::Str,
                            }
                        }

                        _ => {
                            panic!("native function {} not implemented", name);
                        }
                    },
                }
            } else {
                panic!("Unknown function {name:?}")
            }
        }
        Add(lhs, rhs) => {
            let left = eval_expression(lhs, frame);
            let right = eval_expression(rhs, frame);
            match (left.value, right.value) {
                (Value::I64, Value::I64) => Info {
                    string: format!("$(({} + {}))", left.string, right.string),
                    raw_string: format!("$(({} + {}))", left.string, right.string),
                    value: Value::I64,
                },
                (Value::Str, Value::Str) => Info {
                    string: format!("\"{}{}\"", left.raw_string, right.raw_string),
                    raw_string: format!("{}{}", left.raw_string, right.raw_string),
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

fn parse_format_string(args: &Vec<Expression>, frame: &mut StackFrame, raw: bool) -> String {
    let mut new_args: VecDeque<Info> = args.iter().map(|arg| eval_expression(arg, frame)).collect();

    let template = new_args.pop_front().unwrap();

    let mut chars = template.raw_string.chars().peekable();
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
                    match new_args.pop_front() {
                        Some(v) => {
                            if raw {
                                result.push_str(&v.raw_string);
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

    result
}
