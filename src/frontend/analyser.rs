use std::{rc::Rc, mem::discriminant};

use crate::{source::Source, RunArgs, error::{SpruceErr, SpruceErrData}, nativefns, object::Object};

use super::{token::{Token, Span, TokenKind}, functiondata::{Function, FunctionMeta, ParamTypes}, symbols::Symbols, symtable::SymTable, environment::{Environment, ConstantValue}, ast::{Ast, AstData, TypeKind}, decorated_ast::{DecoratedAst, DecoratedAstData}, sprucetype::SpruceType};

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

    pub fn run(&mut self, program: &Box<Ast>) -> Result<Box<Environment>, SpruceErr> {
        nativefns::register_native_functions(self);

        let program = self.visit(program)?;

        // println!("{program:#?}");

        // match self.find_function_str("main") {
        //     Some(_) => {}
        //     None => return Err(self.error("Cannot find function 'main'".into())),
        // }

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
            identifier: identifier.slice_source().to_string(),
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

    fn register_local(&mut self, token: &Token, mutable: bool, kind: SpruceType) {
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
            }

            None => self.table.new_local(token.clone().span, mutable, kind),
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

        // FIXME
        // _ = self.register_local(&identifier, false, Some(self.functable.len() as u32));
        
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
            TokenKind::Int => {
                let lexeme = &node.token.span;
                self.add_constant(Object::Int(
                    self.source.content[lexeme.start..lexeme.start + lexeme.len]
                    .parse::<i32>()
                    .unwrap()
                ))
            }

            TokenKind::Float => {
                let lexeme = &node.token.span;
                self.add_constant(Object::Float(
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

        Ok(DecoratedAst::new_literal(node.token.clone(), index, match node.token.kind {
            TokenKind::None => SpruceType::None,
            TokenKind::String => SpruceType::String,
            TokenKind::True | TokenKind::False => SpruceType::Bool,
            TokenKind::Int => SpruceType::Int,
            TokenKind::Float => SpruceType::Float,
            _ => unreachable!(),
        }))
    }

    fn list_literal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::ListLiteral(inner) = &node.data else { unreachable!() };
        let mut values = Vec::new();
        let mut list_type = SpruceType::None;

        // Check type of first item, then compare to rest
        if inner.len() > 0 {
            let first = self.visit(&inner[0])?;
            list_type = self.find_type_of(&first)?;
            values.push(first);
        }

        for (idx, item) in inner.iter().enumerate() {
            let item = self.visit(&item)?;
            let type_of = &self.find_type_of(&item)?;
            
            if !list_type.is_same(type_of) {
                self.error_no_exit(format!(
                        "Item at index {idx} in list, has type '{:?}' but expects '{:?}'",
                        type_of,
                        list_type,
                    ),
                    &item.token
                )
            }

            values.push(item);
        }

        Ok(DecoratedAst::new_list_literal(node.token.clone(), values, SpruceType::List(Box::new(list_type))))
    }

    fn binary_op(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::BinaryOp { lhs, rhs } = &node.data  else { unreachable!() };
        let lhs = self.visit(&lhs)?;
        let rhs = self.visit(&rhs)?;

        let lhs_type = self.find_type_of(&lhs)?;
        let rhs_type = self.find_type_of(&rhs)?;

        if !lhs_type.is_same(&rhs_type) {
            self.error_no_exit(format!(
                    "Binary operation type mismatch, {:?} and {:?}",
                    lhs_type,
                    rhs_type
                ),
                &node.token
            )
        }

        // Check operation on type
        match &lhs_type {
            SpruceType::Bool => {
                self.error_no_exit(
                        "Cannot use binary operators on two bools".into()
                    , &node.token
                );
            }
            SpruceType::List(_) => {
                self.error_no_exit(
                        "Cannot use binary operators on two lists".into()
                    , &node.token
                );
            }
            SpruceType::Tuple(_) => {
                self.error_no_exit(
                        "Cannot use binary operators on two tuples".into()
                    , &node.token
                );
            }
            SpruceType::Function {..} => {
                self.error_no_exit(
                        "Cannot use binary operators on two functions".into()
                    , &node.token
                );
            }
            _ => {}
        }

        match node.token.kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star |
            TokenKind::Slash => {},
            _ => self.error_no_exit(format!(
                    "Unknown operator in binary operation '{}'",
                    node.token.span.slice_source(),
                ),
                &node.token,
            ),
        }

        Ok(DecoratedAst::new_binary_op(node.token.clone(), lhs_type, lhs, rhs))
    }

    fn unary_op(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::UnaryOp { rhs } = &node.data  else { unreachable!() };
        let rhs = self.visit(&rhs)?;
        let kind = self.find_type_of(&rhs)?;

        // Check operation on type
        match &kind {
            SpruceType::String => {
                self.error_no_exit(
                        "Cannot use unary operators on a string".into()
                    , &node.token
                );
            }
            SpruceType::List(_) => {
                self.error_no_exit(
                        "Cannot use unary operators on a list".into()
                    , &node.token
                );
            }
            SpruceType::Tuple(_) => {
                self.error_no_exit(
                        "Cannot use unary operators on a tuple".into()
                    , &node.token
                );
            }
            SpruceType::Function {..} => {
                self.error_no_exit(
                        "Cannot use unary operators on a function".into()
                    , &node.token
                );
            }
            _ => {}
        }

        match node.token.kind {
            TokenKind::Minus | TokenKind::Bang => {}
            _ => self.error_no_exit(format!(
                    "Unknown operator in unary operation '{}'",
                    node.token.span.slice_source(),
                ),
                &node.token,
            ),
        }

        Ok(DecoratedAst::new_unary_op(node.token.clone(), kind, rhs))
    }

    fn logical_op(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::LogicalOp { lhs, rhs } = &node.data else { unreachable!() };
        let lhs = self.visit(&lhs)?;
        let rhs = self.visit(&rhs)?;

        let lhs_type = self.find_type_of(&lhs)?;
        let rhs_type = self.find_type_of(&rhs)?;

        if !lhs_type.is_same(&rhs_type) {
            self.error_no_exit(format!(
                    "Logical operation type mismatch, {:?} and {:?}",
                    lhs_type,
                    rhs_type
                ),
                &node.token
            )
        }

        match node.token.kind {
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less |
            TokenKind::LessEqual | TokenKind::And | TokenKind::Or |
            TokenKind::EqualEqual | TokenKind::NotEqual => {}
            _ => self.error_no_exit(format!(
                    "Unknown operator in logical operation '{}'",
                    node.token.span.slice_source(),
                ),
                &node.token,
            ),
        }

        Ok(DecoratedAst::new_logical_op(node.token.clone(), lhs, rhs))
    }

    fn body(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Body(inner) = &node.data else { unreachable!() };
        let mut statements = Vec::new();

        for item in inner {
            statements.push(self.visit(item)?);
        }

        let kind = match statements.last() {
            Some(n) => match &n.data {
                DecoratedAstData::ExpressionStatement(k, is_statement, _) => {
                    if *is_statement {
                        SpruceType::None
                    } else {
                        k.clone()
                    }
                }
                _ => SpruceType::None,
            },
            None => SpruceType::None,
        };

        Ok(DecoratedAst::new_body(node.token.clone(), statements, kind))
    }

    fn expr_statement(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::ExpressionStatement(is_statement, inner) = &node.data else { unreachable!() };
        let inner = self.visit(inner)?;
        let kind = self.find_type_of(&inner)?;
        Ok(DecoratedAst::new_expr_statement(*is_statement, inner, kind))
    }

    fn identifier(&mut self, node: &Box<Ast>) -> Box<DecoratedAst> {
        let identifier = &node.token;
        let mut kind = SpruceType::None;

        match self.table.find_local(&identifier.span, true) {
            Some(local) => {
                kind = local.kind.clone();
            },
            None => self.error_no_exit(format!(
                    "Identifier '{}' does not exist in the current context",
                    identifier.span.slice_source(),
                ),
                &identifier.clone()
            ),
        }

        DecoratedAst::new_identifier(identifier.clone(), kind)
    }

    fn var_declaration(&mut self, var_decl: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::VarDeclaration { is_mutable, kind, expression } = &var_decl.data else { unreachable!() };
        let identifier = &var_decl.token;

        let expression = match expression {
            Some(expr) => self.visit(expr)?,
            None => DecoratedAst::new_empty(identifier.clone()),
        };

        let expr_kind = self.find_type_of(&expression)?;

        let kind = match kind {
            Some(kind) => {
                let kind = self.get_kind_from_ast(kind)?;

                if !kind.is_same(&expr_kind) {
                    self.error_no_exit(format!(
                            "Variable '{}' expected type {:?} but received {:?}",
                            identifier.span.slice_source(),
                            kind,
                            expr_kind,
                        ),
                        identifier
                    );
                }
                expr_kind
            },
            None => expr_kind,
        };

        self.register_local(identifier, *is_mutable, kind.clone());

        if self.table.is_global() {
            // Check mutable
            if self.args.no_global_mut && *is_mutable {
                self.error_no_exit(
                    format!(
                        "Cannot declare mutable variable '{}' in global scope, when the 'no-mutable' flag is set",
                        identifier.span.slice_source()
                    ),
                    identifier
                );
            }

            // Check global
            if self.args.no_global {
                self.error_no_exit(
                    format!(
                        "Cannot declare variable '{}' in global scope, when the 'no-global' flag is set",
                        identifier.span.slice_source()
                    ),
                    identifier
                );
            }
        }

        Ok(DecoratedAst::new_var_decl(var_decl.token.clone(), *is_mutable, kind, expression))
    }

    fn var_declarations(&mut self, var_decls: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::VarDeclarations(decls) = &var_decls.data else { unreachable!() };
        let mut declarations = Vec::new();
        for decl in decls {
            declarations.push(self.var_declaration(&decl)?);
        }
        Ok(DecoratedAst::new_var_decls(declarations))
    }

    fn var_assign(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::VarAssign { lhs, expression } = &node.data else { unreachable!() };
        let identifier = &node.token;

        let setter = self.visit(&lhs)?;
        let expr = self.visit(&expression)?;

        match self.table.find_local(&identifier.span, true) {
            Some(local) => {
                let is_mutable = local.mutable;
                let kind = local.kind.clone();
                let expr_type = self.find_type_of(&expr)?;

                if !is_mutable {
                    self.error_no_exit(format!(
                            "Cannot re-assign an immutable value '{}'",
                            identifier.span.slice_source(),
                        ),
                        &identifier
                    );
                }

                if !expr_type.is_same(&kind) {
                    self.error_no_exit(format!(
                            "Cannot assign type {:?} to '{}' where type {:?} is expected",
                            expr_type,
                            identifier.span.slice_source(),
                            kind,
                        ),
                        &identifier
                    );
                }
            }
            None => self.error_no_exit(format!(
                    "Cannot assign to variable '{}' as it does not exist or is not in the correct context",
                    identifier.span.slice_source(),
                ),
                &identifier
            ),
        }

        Ok(DecoratedAst::new_var_assign(node.token.clone(), setter, expr))
    }

    fn program(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Program { source, body } = &node.data else { unreachable!() };
        let mut statements = Vec::new();

        for item in body {
            statements.push(self.visit(item)?);
        }

        Ok(DecoratedAst::new_program(node.token.clone(), Rc::clone(source), statements))
    }

    fn visit(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        Ok(match node.data {
            AstData::Literal => self.literal(node)?,
            AstData::ListLiteral(_) => self.list_literal(node)?,

            AstData::Identifier => self.identifier(node),
            AstData::BinaryOp {..} => self.binary_op(node)?,
            AstData::UnaryOp {..} => self.unary_op(node)?,
            AstData::LogicalOp {..} => self.logical_op(node)?,

            AstData::VarDeclaration {..} => self.var_declaration(node)?,
            AstData::VarDeclarations(_) => self.var_declarations(node)?,
            AstData::VarAssign {..} => self.var_assign(node)?,

            AstData::Body(_) => self.body(node)?,
            AstData::ExpressionStatement(_, _) => self.expr_statement(node)?,

            AstData::Program {..} => self.program(node)?,
            AstData::Empty => DecoratedAst::new_empty(node.token.clone()),
            _ => return Err(self.error(format!("Unknown node in check: {:#?}", node))),
        })
    }

    fn find_type_of(&self, node: &Box<DecoratedAst>) -> Result<SpruceType, SpruceErr> {
        Ok(match &node.data {
            DecoratedAstData::Literal(t, _) => t.clone(),
            DecoratedAstData::ListLiteral(t, _) => t.clone(),
            
            DecoratedAstData::BinaryOp { kind, .. } => kind.clone(),
            DecoratedAstData::UnaryOp { kind, .. } => kind.clone(),
            DecoratedAstData::LogicalOp {..} => SpruceType::Bool,
            
            DecoratedAstData::Identifier(t) => t.clone(),
            DecoratedAstData::VarAssign { lhs, .. } => self.find_type_of(lhs)?,
            
            DecoratedAstData::Body(t, _) => t.clone(),
            DecoratedAstData::ExpressionStatement(t, _, _) => t.clone(),
            DecoratedAstData::Empty => SpruceType::Any,

            _ => return Err(SpruceErr::new(format!(
                    "Cannot find type of '{}' - {:#?}",
                    node.token.span.slice_source(),
                    node.data,
                ),
                SpruceErrData::Analyser { file_path: (*self.source.file_path).clone() }
            ))
        })
    }

    fn get_kind_from_ast(&mut self, node: &Box<Ast>) -> Result<SpruceType, SpruceErr> {
        let AstData::Type { kind, inner } = &node.data else { unreachable!() };

        Ok(match *kind {
            TypeKind::Standard => {
                match node.token.span.slice_source() {
                    "any" => SpruceType::Any,
                    "int" => SpruceType::Int,
                    "float" => SpruceType::Float,
                    "bool" => SpruceType::Bool,
                    // TODO: Check for custom type before error
                    _ => return Err(self.error(format!(
                        "Unknown type identifier '{}'",
                        node.token.span.slice_source(),
                    ))),
                }
            }
            TypeKind::List => SpruceType::List(Box::new(self.get_kind_from_ast(match inner {
                Some(ref o) => o,
                None => unreachable!(),
            })?)),
            _ => unimplemented!(),
        })
    }
}