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

use std::{env, fs, rc::Rc};
use compiler::Compiler;
use environment::Environment;
use parser::Parser;
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
            if let Ok(ref source) = fs::read_to_string(&args[2]) {
                match compile(false, source) {
                    Ok(e) => VM::new(e).run(),
                    _ => println!("FAIL!"),
                }
            } else {
                eprintln!("Could not load file '{}'", args[2]);
                return;
            }
        }
        _ => {
            println!("Unknown sub-command '{}'", args[1]);
            return;
        },
    }
}

fn compile(is_file: bool, source: &String) -> Result<Box<Environment>, String> {
    let source = if is_file {
        match fs::read_to_string(source) {
            Ok(s) => Rc::new(s),
            Err(e) => return Err(e.to_string()),
        }
    } else { Rc::new(source.clone()) };
    
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