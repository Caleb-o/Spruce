use std::rc::Rc;

use crate::{source::Source, error::SpruceErr, RunArgs, frontend::{parser::Parser, ast::Ast, analyser::Analyser}};

pub fn check_code(file_path: String, source: String, args: RunArgs) -> Result<(Rc<Source>, Box<Ast>), SpruceErr> {
    let source = Rc::new(Source::new(file_path, source));

    let mut parser = match Parser::new(&source, args.clone()) {
        Ok(c) => c,
        Err(e) => return Err(e.into()),
    };

    let (source, root) = match parser.run() {
        Ok(p) => (source, p),
        Err(e) => return Err(e.into()),
    };

    let mut analyser = Analyser::new(Rc::clone(&source), args);
    analyser.run(&root)?;

    Ok((source, root))
}

// pub fn compile_source(source: String, args: RunArgs) -> Result<Box<Environment>, SpruceErr> {
//     let timer = Instant::now();
//     let (source, program) = run_parser(args.file_path.clone(), source, args.clone())?;
    
//     let mut compiler = Compiler::new(source, args.clone());
//     match compiler.run(program) {
//         Ok(env) => {
//             println!("\x1b[32mFinished\x1b[0m compilation in {:.3} seconds", timer.elapsed().as_secs_f32());
//             Ok(env)
//         },
//         Err(e) => Err(e.into()),
//     }
// }