use std::rc::Rc;

use crate::{compiler::Compiler, source::Source, parser::Parser, environment::Environment, ast::Ast, error::SpruceErr};

pub fn run_parser(file_path: String, source: String, script_mode: bool) -> Result<(Rc<Source>, Box<Ast>), SpruceErr> {
    let source = Rc::new(Source::new(file_path, source));

    let mut parser = match Parser::new(&source, script_mode) {
        Ok(c) => c,
        Err(e) => return Err(e.into()),
    };

    match parser.run() {
        Ok(p) => Ok((source, p)),
        Err(e) => Err(e.into()),
    }
}

pub fn compile_source(file_path: String, source: String, script_mode: bool) -> Result<Box<Environment>, SpruceErr> {
    let (source, program) = run_parser(file_path, source, script_mode)?;
    
    let mut compiler = Compiler::new(source);
    match compiler.run(program, script_mode) {
        Ok(env) => Ok(env),
        Err(e) => Err(e.into()),
    }
}