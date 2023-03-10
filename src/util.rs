use std::{rc::Rc, time::Instant};

use crate::{compiler::Compiler, source::Source, parser::Parser, environment::Environment, ast::Ast, error::SpruceErr, RunArgs};

pub fn run_parser(file_path: String, source: String, args: RunArgs) -> Result<(Rc<Source>, Box<Ast>), SpruceErr> {
    let source = Rc::new(Source::new(file_path, source));

    let mut parser = match Parser::new(&source, args) {
        Ok(c) => c,
        Err(e) => return Err(e.into()),
    };

    match parser.run() {
        Ok(p) => Ok((source, p)),
        Err(e) => Err(e.into()),
    }
}

pub fn compile_source(source: String, args: RunArgs) -> Result<Box<Environment>, SpruceErr> {
    let timer = Instant::now();
    let (source, program) = run_parser(args.file_path.clone(), source, args.clone())?;
    
    let mut compiler = Compiler::new(source, args.clone());
    match compiler.run(program) {
        Ok(env) => {
            println!("\x1b[32mFinished\x1b[0m compilation in {:.3} seconds", timer.elapsed().as_secs_f32());
            Ok(env)
        },
        Err(e) => Err(e.into()),
    }
}