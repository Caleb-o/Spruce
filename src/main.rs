mod frontend;
mod nativefns;
mod compiler;
mod source;
mod util;
mod error;
mod visitor;

use std::{fs, process::Command};

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
    #[clap(default_value_t=false, short='c', long)]
    pub compile: bool,
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
                match util::check_code(args.file_path.clone(), source, args.clone()) {
                    Ok((source, (root, symbols))) => {
                        let mut compiler = Compiler::new(source, symbols);
                        if let Err(e) = compiler.run(root) {
                            eprintln!("{e}");
                        } else {
                            if args.compile {
                                print!("Compiling output source...");
                                let command = Command::new("csc")
                                    .args(["out.cs", "-langversion:10.0", "-o", "-nullable:enable"])
                                    .output();
                                
                                if let Ok(command) = command {
                                    if command.status.success() {
                                        println!("Done!");
                                    } else {
                                        println!("\n=== Error ===\n{}", String::from_utf8(command.stdout).unwrap());
                                    }
                                } else {
                                    eprintln!("Could not compile script with csc");
                                }
                            }
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