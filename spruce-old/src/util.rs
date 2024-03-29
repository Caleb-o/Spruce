use std::rc::Rc;

use crate::{source::Source, error::SpruceErr, RunArgs, frontend::{parser::Parser, analyser::Analyser, decorated_ast::DecoratedAst, ast::Ast, symbols::Symbols, name_resolution::NameResolver}};

pub fn check_code(file_path: String, source: String, args: RunArgs) -> Result<(Rc<Source>, (Rc<DecoratedAst>, Symbols)), SpruceErr> {
    let source = Rc::new(Source::new(file_path, source));

    let mut parser = match Parser::new(&source, args.clone()) {
        Ok(c) => c,
        Err(e) => return Err(e.into()),
    };

    let (source, root) = match parser.run() {
        Ok(p) => (source, p),
        Err(e) => return Err(e.into()),
    };

    let mut resolver = NameResolver::new(Rc::clone(&source), args.clone());
    let table = resolver.run(&root)?;

    let mut analyser = Analyser::new(Rc::clone(&source), args, table);
    let root = analyser.run(&root)?;

    Ok((source, root))
}

pub fn compile_source(file_path: String, source: String, args: RunArgs) -> Result<(Rc<Source>, Rc<Ast>), SpruceErr> {
    let source = Rc::new(Source::new(file_path, source));

    let mut parser = match Parser::new(&source, args.clone()) {
        Ok(c) => c,
        Err(e) => return Err(e.into()),
    };

    match parser.run() {
        Ok(p) => Ok((source, p)),
        Err(e) => Err(e.into()),
    }
}