mod token;
mod lexer;
mod ast;
mod parser;
mod environment;
mod instructions;
mod object;
mod nativefns;
mod symtable;
mod compiler;
mod vm;
mod stepper;

use std::{fs, rc::Rc};
use compiler::Compiler;
use environment::Environment;
use parser::Parser;
use stepper::Stepper;
use vm::VM;

use clap::Parser as ClapParser;

#[derive(ClapParser)] // requires `derive` feature
#[command(name = "spruce")]
#[command(bin_name = "spruce")]
enum SpruceCli {
    #[command(short_flag='d')]
    Dump(RunArgs),
    #[command(short_flag='r')]
    Run(RunArgs),
    #[command(short_flag='s')]
    Step { path: String },
}

#[derive(clap::Args)]
#[command(author, version, about, long_about = None)]
struct RunArgs {
    path: String,
    #[clap(default_value_t=false, short, long)]
    script_mode: bool,
}

fn main() {
    match SpruceCli::parse() {
        SpruceCli::Run(args) | SpruceCli::Dump(args) => {
            if let Ok(source) = fs::read_to_string(&args.path) {
                match compile(source) {
                    Ok(env) => VM::new(env).run(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.path);
            }
        }
        SpruceCli::Step { path } => {
            if let Ok(source) = fs::read_to_string(&path) {
                match compile(source) {
                    Ok(e) => Stepper::new(e).run(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", path);
            }
        }
    }
}

fn compile(source: String) -> Result<Box<Environment>, String> {
    let source = Rc::new(source);
    let mut parser = match Parser::new(Rc::clone(&source)) {
        Ok(c) => c,
        Err(e) => return Err(e.to_string()),
    };
    let program = match parser.run() {
        Ok(p) => p,
        Err(e) => return Err(e.to_string()),
    };
    
    let mut compiler = Compiler::new(source);
    match compiler.run(program) {
        Ok(env) => Ok(env),
        Err(e) => Err(e.0),
    }
}