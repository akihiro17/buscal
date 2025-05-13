use buscal::{evaluator, parser, type_checker, Span};
use clap::{Parser, Subcommand};
use core::panic;
use log::{error, info};
use std::{
    fs::File,
    io::{BufReader, BufWriter, Read, Write},
    path::{Path, PathBuf},
    process::Command,
    thread, time,
};
use tempfile::{tempfile, NamedTempFile};

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Adds files to myapp
    Build {
        src_file: String,
        #[arg(short, long)]
        out_file: String,
    },
    Run {
        src_file: String,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    let cli = Cli::parse();
    let mut execute_bash_script = false;
    let (source, out_file) = match &cli.command {
        Commands::Build { src_file, out_file } => {
            let source = std::fs::read_to_string(src_file)?;
            (source, Some(out_file))
        }
        Commands::Run { src_file } => {
            let source = std::fs::read_to_string(src_file)?;
            execute_bash_script = true;
            (source, None)
        }
    };

    let t = Span::new(&source);
    let parsed_statements = match parser::statements_finish(t) {
        Ok(stmts) => stmts,
        Err(err) => {
            eprintln!(
                "Parse error: :{}:{}: {err}",
                err.input.location_line(),
                err.input.get_column()
            );

            return Ok(());
        }
    };

    let mut tc_ctx = type_checker::Context::new();

    if let Err(err) = type_checker::type_check(&parsed_statements, &mut tc_ctx) {
        println!(
            "Type check error: {}:{}: {err}",
            err.span.location_line(),
            err.span.get_column()
        );

        return Ok(());
    }
    info!("Type check OK");

    let mut frame = evaluator::StackFrame::new();
    let mut lines = vec![];
    lines.push("#!/usr/bin/env bash".to_string());
    lines.push("set -e".to_string());
    if let Err(err) = evaluator::eval_stmts(&parsed_statements, &mut frame, &mut lines, 0) {
        eprintln!("Error: {err}");
    }

    match out_file {
        Some(output) => {
            let f = File::create(output)?;
            let mut writer = BufWriter::new(f);

            for line in lines {
                if line == "\n" {
                    writer.write(line.as_bytes())?;
                } else {
                    writer.write((line + "\n").as_bytes())?;
                }
            }
        }
        None => {
            let temp_file = NamedTempFile::new()?;
            let mut writer = BufWriter::new(&temp_file);

            for line in lines {
                if line == "\n" {
                    writer.write(line.as_bytes())?;
                } else {
                    writer.write((line + "\n").as_bytes())?;
                }
            }

            writer.flush()?;

            let output = Command::new("bash")
                .arg(temp_file.path())
                .output()
                .unwrap_or_else(|e| panic!("Failed to execute bash script: {}", e));

            if output.status.success() {
                print!("{}", String::from_utf8_lossy(&output.stdout));
            } else {
                eprint!("{}", String::from_utf8_lossy(&output.stderr));
            }
        }
    };

    Ok(())
}
