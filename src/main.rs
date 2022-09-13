mod token;
mod lexer;
mod environment;
mod instructions;
mod object;
mod compiler;
mod vm;

use std::env;
use compiler::Compiler;
use vm::VM;

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.len() != 2 {
        println!("Usage: spruce script");
        return;
    }

    let mut compiler = match Compiler::new(&args[1]) {
        Ok(c) => c,
        Err(e) => {
            println!("{e}");
            return;
        },
    };

    let env = match compiler.run() {
        Ok(env) => env,
        Err(e) => {
            println!("{}", e.0);
            return;  
        },
    };

    env.dump();
    
    // match VM::new(env).run() {
    //     Err(e) => println!("Runtime: {}", e.0),
    //     _ => {},
    // };
}