mod token;
mod lexer;
mod environment;
mod instructions;
mod object;
mod nativefns;
mod symtable;
mod compiler;
mod vm;

use std::env;
use compiler::Compiler;
use environment::Environment;
use vm::VM;

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.len() != 3 {
        println!("Usage: spruce [debug|run] script");
        return;
    }

    match &args[1][0..] {
        "d" | "debug" => {
            match compile(&args[2]) {
                Ok(e) => {
                    e.dump();
                }
                Err(e) => eprintln!("{e}"),
            }
        }
        "r" | "run" => {
            match compile(&args[2]) {
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

fn compile(file_path: &String) -> Result<Box<Environment>, String> {
    let mut compiler = match Compiler::new(&file_path) {
        Ok(c) => c,
        Err(e) => return Err(e.to_string()),
    };

    match compiler.run() {
        Ok(env) => Ok(env),
        Err(e) => Err(e.0),
    }
}