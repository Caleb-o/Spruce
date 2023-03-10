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
mod source;
mod util;
mod error;

use std::fs;
use stepper::Stepper;
use vm::VM;

use clap::Parser as ClapParser;

#[derive(ClapParser)] // requires `derive` feature
#[command(name = "spruce")]
#[command(bin_name = "spruce")]
enum SpruceCli {
    Dump(RunArgs),
    Run(RunArgs),
    Step(RunArgs),
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
        SpruceCli::Run(args) => {
            if let Ok(source) = fs::read_to_string(&args.path) {
                match util::compile_source(args.path, source, args.script_mode) {
                    Ok(env) => VM::new(env).run(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.path);
            }
        }
        SpruceCli::Dump(args) => {
            if let Ok(source) = fs::read_to_string(&args.path) {
                match util::compile_source(args.path, source, args.script_mode) {
                    Ok(mut env) => env.dump(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.path);
            }
        }
        SpruceCli::Step(args) => {
            if let Ok(source) = fs::read_to_string(&args.path) {
                match util::compile_source(args.path, source, args.script_mode) {
                    Ok(e) => Stepper::new(e).run(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.path);
            }
        }
    }
}