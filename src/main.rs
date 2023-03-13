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
mod heap;
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

#[derive(clap::Args, Clone)]
#[command(author, version, about, long_about = None)]
pub struct RunArgs {
    pub file_path: String,
    #[clap(default_value_t=false, short, long)]
    pub script_mode: bool,
    #[clap(default_value_t=false, short='g', long)]
    pub no_global: bool,
    #[clap(default_value_t=false, short='m', long)]
    pub no_global_mut: bool,
}

fn main() {
    match SpruceCli::parse() {
        SpruceCli::Run(args) => {
            if let Ok(source) = fs::read_to_string(&args.file_path) {
                match util::compile_source(source, args) {
                    Ok(env) => VM::new(env).run(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.file_path);
            }
        }
        SpruceCli::Dump(args) => {
            if let Ok(source) = fs::read_to_string(&args.file_path) {
                match util::compile_source(source, args) {
                    Ok(mut env) => env.dump(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.file_path);
            }
        }
        SpruceCli::Step(args) => {
            if let Ok(source) = fs::read_to_string(&args.file_path) {
                match util::compile_source(source, args) {
                    Ok(e) => Stepper::new(e).run(),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.file_path);
            }
        }
    }
}