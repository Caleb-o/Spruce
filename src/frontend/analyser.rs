use std::rc::Rc;

use crate::{source::Source, RunArgs, error::{SpruceErr, SpruceErrData}, nativefns, object::Object};

use super::{token::{Token, Span, TokenKind}, functiondata::{Function, FunctionMeta, ParamTypes}, symbols::Symbols, symtable::SymTable, environment::{Environment, ConstantValue}, ast::{Ast, AstData}, decorated_ast::DecoratedAst, sprucetype::SpruceType};

#[derive(Debug, Clone)]
struct LookAhead {
    token: Token,
    args: u8,
    // Position in bytecode
    position: u32,
}

pub struct Analyser {
    had_error: bool,
    source: Rc<Source>,
    args: RunArgs,
    unresolved: Vec<LookAhead>,
    constants: Vec<ConstantValue>,
    functable: Vec<FunctionMeta>,
    table: SymTable,
    symbol_values: Symbols,
    last_return: bool,
}

impl Analyser {
    pub fn new(source: Rc<Source>, args: RunArgs) -> Self {
        Self {
            had_error: false,
            source,
            args,
            unresolved: Vec::new(),
            constants: Vec::new(),
            functable: Vec::new(),
            table: SymTable::new(),
            symbol_values: Symbols::new(),
            last_return: false,
        }
    }

    pub fn run(&mut self, program: Box<Ast>) -> Result<Box<Environment>, SpruceErr> {
        nativefns::register_native_functions(self);

        let program = self.visit(&program)?;

        match self.find_function_str("main") {
            Some(_) => {}
            None => return Err(self.error("Cannot find function 'main'".into())),
        }

        // Try to resolve calls that were not during compilation
        self.resolve_function_calls();

        if self.had_error {
            return Err(self.error("Error(s) occured".into()));
        }

        Ok(Box::new(Environment::new(program)))
    }

    pub fn add_fn(
        &mut self,
        identifier: &Span,
        param_count: ParamTypes,
        has_return: bool,
    ) {
        let function = Function::Native { 
            identifier: identifier.slice_source(),
            param_types: None,
            has_return,
        };

        // Add to function table
        self.add_function(identifier.clone(), function);
    }

    #[inline]
    fn add_function(&mut self, identifier: Span, func: Function) {
        self.functable.push(FunctionMeta::new(identifier, func));
    }

    fn find_constant(&self, obj: &Object) -> Option<usize> {
        for (i, item) in self.constants.iter().enumerate() {
            if let ConstantValue::Obj(ref o) = item {
                if obj.is_exact(o) {
                    return Some(i);
                }
            }
        }

        None
    }
    
    #[inline]
    fn add_constant(&mut self, constant: Object) -> u32 {
        match self.find_constant(&constant) {
            Some(idx) => idx as u32,
            None => {
                self.constants.push(ConstantValue::Obj(constant));
                self.constants.len() as u32 - 1
            }
        }
    }
    
    #[inline]
    fn error(&mut self, message: String) -> SpruceErr {
        SpruceErr::new(message, SpruceErrData::Analyser { file_path: (*self.source.file_path).clone() })
    }

    #[inline]
    fn push_scope(&mut self) {
        self.table.new_scope();
    }

    #[inline]
    fn pop_scope(&mut self) {
        self.table.close_scope();
    }

    fn resolve_function_calls(&mut self) {
        let mut unresolved = Vec::new();

        for lookahead in self.unresolved.iter() {
            match self.find_function(&lookahead.token.span) {
                Some(func) => {
                    // Cannot resolve native calls, since they're part of the compiler
                    if let Function::User { meta_id, empty, ..} = &func.function {
                        // Generate the function if it is not empty
                        if *empty {
                            self.warning(format!(
                                    "Calling empty function '{}'",
                                    lookahead.token.span.slice_source()
                                ),
                                &lookahead.token
                            );
                        }

                        let func = &self.functable[*meta_id as usize];
                        if func.function.param_count() == lookahead.args {
                            todo!();
                        }
                    }
                }

                None => unresolved.push((lookahead.token.clone(), 0, lookahead.args)),
            }
        }

        // Identifiers that were still not found
        for (token, params, args) in unresolved {
            let id = token.span.slice_source().to_string();

            if params != args {
                self.error_no_exit(format!(
                    "Function '{id}' expected {params} argument(s), but received {args}",
                    ),
                    &token
                );
            } else {
                self.error_no_exit(format!(
                    "Function '{id}' does not exist",
                    ),
                    &token
                );
            }
        }
    }

    fn error_no_exit(&mut self, msg: String, token: &Token) {
        self.had_error = true;

        println!("{}", format!(
            "[\x1b[31mError\x1b[0m] {} - '{}' [{}:{}]",
            msg,
            token.span.source.file_path,
            token.line,
            token.column,
        ));
    }

    fn warning(&self, msg: String, token: &Token) {
        println!("{}", format!(
            "[\x1b[33mWarning\x1b[0m] {} '{}' [{}:{}]",
            msg,
            token.span.source.file_path,
            token.line,
            token.column,
        ));
    }

    fn find_function_str(&self, id: &str) -> Option<&FunctionMeta> {
        for func in &self.functable {
            if func.identifier.compare_str(id) {
                return Some(func);
            }
        }

        None
    }

    #[inline]
    fn find_function(&self, span: &Span) -> Option<&FunctionMeta> {
        self.find_function_str(span.slice_source())
    }

    fn register_local(&mut self, token: &Token, mutable: bool, func: Option<u32>) -> Option<usize> {
        let local = self.table.find_local(&token.span, false);

        match local {
            Some(local) => {
                // We aren't allowed to overwrite, it is an error
                self.error_no_exit(format!(
                        "Local with identifier '{}' already exists in scope",
                        local.identifier.slice_source(),
                    ),
                    token
                );
                None
            }

            None => Some(self.table.new_local(token.clone().span, mutable, func) as usize),
        }
    }

    fn register_function(
        &mut self,
        identifier: &Token,
        position: u32,
        parameters: &Option<Vec<Box<Ast>>>,
    ) -> Result<Box<DecoratedAst>, SpruceErr>
    {
        let func = self.find_function(&identifier.span);

        // Function already exists
        if func.is_some() {
            self.error_no_exit(
                format!(
                    "Function with identifier '{}' already exists",
                    identifier.span.slice_source()
                ),
                &identifier
            );
            return Err(self.error(format!(
                "Function with identifier '{}' already exists",
                identifier.span.slice_source()
            )));
        }

        _ = self.register_local(&identifier, false, Some(self.functable.len() as u32));
        
        self.push_scope();
        // self.evaluate_params(parameters)?;
        
        // FIXME: Need to get the types of the parameters
        let param_count = match parameters {
            Some(ref p) => p.len() as u8,
            None => 0,
        };

        let function = Function::User {
            meta_id: self.functable.len() as u32 - 1,
            param_types: None,
            empty: false
        };

        self.add_function(identifier.span.clone(), function);

        // Ok(())
        todo!()
    }

    fn mark_function_empty(&mut self, id: &Span) {
        for func in &mut self.functable {
            // Cannot mark native functions as empty
            if func.identifier.compare(id) {
                func.function.mark_empty();
            }
        }
    }

    fn literal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let index = match node.token.kind {
            // TODO: Int and Float
            TokenKind::Number => {
                let lexeme = &node.token.span;
                self.add_constant(Object::Number(
                    self.source.content[lexeme.start..lexeme.start + lexeme.len]
                    .parse::<f32>()
                    .unwrap()
                ))
            }
            
            TokenKind::String => {
                let lexeme = &node.token.span;
                self.add_constant(Object::String(
                    String::from(
                        &self.source.content[lexeme.start..lexeme.start + lexeme.len]
                    )
                ))
            }

            TokenKind::None =>  self.add_constant(Object::None),
            TokenKind::True =>  self.add_constant(Object::Boolean(true)),
            TokenKind::False => self.add_constant(Object::Boolean(false)),
            _ => unreachable!(),
        };

        Ok(DecoratedAst::new_literal(node.token, index, match node.token.kind {
            TokenKind::None => SpruceType::None,
            TokenKind::String => SpruceType::String,
            TokenKind::True | TokenKind::False => SpruceType::Bool,
            TokenKind::Number => SpruceType::Float,
            _ => unreachable!(),
        }))
    }

    fn visit(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        Ok(match node.data {
            AstData::Literal => self.literal(node)?,

            AstData::Empty => DecoratedAst::new_empty(node.token),
            _ => return Err(self.error(format!("Unknown node: {:#?}", *node))),
        })
    }
}