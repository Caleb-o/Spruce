mod token;
mod lexer;
mod environment;
mod instructions;
mod object;
mod compiler;
mod vm;

use std::env;

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.len() != 2 {
        println!("Usage: spruce script");
        return;
    }
}