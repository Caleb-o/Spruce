mod frontend;
mod object;
mod nativefns;
mod compiler;
mod source;
mod util;
mod error;

use std::fs;

use clap::Parser as ClapParser;
use compiler::Compiler;

#[derive(ClapParser)] // requires `derive` feature
#[command(name = "spruce")]
#[command(bin_name = "spruce")]
enum SpruceCli {
    Dump(RunArgs),
    Check(RunArgs),
    Run(RunArgs),
}

#[derive(clap::Args, Clone)]
#[command(author, version, about, long_about = None)]
pub struct RunArgs {
    pub file_path: String,
    #[clap(default_value_t=false, short='g', long)]
    pub no_global: bool,
    #[clap(default_value_t=false, short='m', long)]
    pub no_global_mut: bool,
}

fn main() {
    match SpruceCli::parse() {
        SpruceCli::Check(args) => {
            if let Ok(source) = fs::read_to_string(&args.file_path) {
                match util::check_code(args.file_path.clone(), source, args) {
                    Ok(_) => println!("GOOD"),
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.file_path);
            }
        }
        SpruceCli::Run(args) => {
            if let Ok(source) = fs::read_to_string(&args.file_path) {
                match util::check_code(args.file_path.clone(), source, args) {
                    Ok((source, root)) => {
                        let mut compiler = Compiler::new(source);
                        if let Err(e) = compiler.run(root) {
                            eprintln!("{e}");
                        }
                    },
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.file_path);
            }
        }
        _ => unimplemented!(),
    }
}