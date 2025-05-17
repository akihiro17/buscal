use core::panic;
use std::io::Read;

mod evaluator;
mod parser;
mod standard;
mod type_checker;
mod types;

use log::{error, info};

fn main() {
    env_logger::init();

    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read from stdin");
    }
    let parsed_statements = match parser::statements_finish(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            error!("Parse error: {e:?}");
            return;
        }
    };

    let mut tc_ctx = type_checker::Context::new();

    if let Err(err) = type_checker::type_check(&parsed_statements, &mut tc_ctx) {
        println!("Type check error: {err}");
        return;
    }
    info!("Type check OK");

    let mut frame = evaluator::StackFrame::new();
    let mut lines = vec![];
    lines.push("#!/usr/bin/env bash".to_string());
    evaluator::eval_stmts(&parsed_statements, &mut frame, &mut lines, 0);

    for line in lines {
        println!("{}", line);
    }
}
