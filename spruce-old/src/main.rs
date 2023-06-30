mod compiler;
mod error;
mod frontend;
mod nativefns;
mod source;
mod util;
mod visitor;

use std::{fs, process::Command};

use clap::{Parser as ClapParser, ValueEnum};
use compiler::Compiler;

const LANG_VERSION: &'static str = "-langversion:9.0";

#[derive(ClapParser)] // requires `derive` feature
#[command(name = "spruce")]
#[command(bin_name = "spruce")]
enum SpruceCli {
    #[clap(visible_alias = "d")]
    Dump(RunArgs),
    #[clap(visible_alias = "c")]
    Check(RunArgs),
    #[clap(visible_alias = "r")]
    Run(RunArgs),
}

#[derive(Copy, Clone, ValueEnum, PartialEq)]
pub enum Backend {
    CSharp,
    VM,
}

#[derive(clap::Args, Clone)]
#[command(author, version, about, long_about = None)]
pub struct RunArgs {
    pub file_path: String,
    #[clap(default_value_t = false, short = 'c', long)]
    pub compile: bool,
    #[clap(default_value_t = false, short = 'g', long)]
    pub no_global: bool,
    #[clap(default_value_t = false, short = 'm', long)]
    pub no_global_mut: bool,
    #[clap(value_enum, default_value_t = Backend::CSharp, short = 'b', long)]
    pub backend: Backend,
    #[clap(default_value_t = false, short = 'a', long)]
    pub auto_run: bool,
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
                            if args.compile && args.backend == Backend::CSharp {
                                print!("Compiling output source...");
                                let command = Command::new("csc")
                                    .args(["out.cs", LANG_VERSION, "-o", "-nullable:enable"])
                                    .output();

                                if let Ok(command) = command {
                                    if command.status.success() {
                                        println!("Done!");

                                        if args.auto_run {
                                            let c = Command::new("mono")
                                                .args(["out.exe"])
                                                .output()
                                                .unwrap();

                                            println!("{}", String::from_utf8_lossy(&c.stdout));
                                        }
                                    } else {
                                        println!(
                                            "\n=== Error ===\n{}",
                                            String::from_utf8(command.stdout).unwrap()
                                        );
                                    }
                                } else {
                                    eprintln!("Could not compile script with csc");
                                }
                            }
                        }
                    }
                    Err(e) => eprintln!("{e}"),
                }
            } else {
                eprintln!("Could not load file '{}'", args.file_path);
            }
        }
        _ => unimplemented!(),
    }
}
