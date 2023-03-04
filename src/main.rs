mod token;
mod lexer;
mod environment;
mod instructions;
mod object;
mod nativefns;
mod symtable;
mod compiler;
mod vm;
mod stepper;

use std::env;
use compiler::Compiler;
use environment::Environment;
use stepper::Stepper;
use vm::VM;

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.len() != 3 {
        println!("Usage: spruce [debug|run] script");
        return;
    }

    match &args[1][0..] {
        "d" | "dump" => {
            match compile(true, &args[2]) {
                Ok(mut e) => {
                    e.dump();
                }
                Err(e) => eprintln!("{e}"),
            }
        }
        "s" | "step" => {
            match compile(true, &args[2]) {
                Ok(e) => Stepper::new(e).run(),
                Err(e) => eprintln!("{e}"),
            }
        }
        "r" | "run" => {
            match compile(true, &args[2],) {
                Ok(e) => VM::new(e).run(),
                Err(e) => eprintln!("{e}"),
            }
        }
        _ => {
            println!("Unknown sub-command '{}'", args[1]);
            return;
        },
    }
}

fn compile(is_file: bool, source: &String) -> Result<Box<Environment>, String> {
    let mut compiler = match Compiler::new(is_file, &source) {
        Ok(c) => c,
        Err(e) => return Err(e.to_string()),
    };

    match compiler.run() {
        Ok(env) => Ok(env),
        Err(e) => Err(e.0),
    }
}