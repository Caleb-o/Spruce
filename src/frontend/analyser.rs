use std::{rc::Rc, mem::discriminant, collections::HashSet};

use crate::{source::Source, RunArgs, error::{SpruceErr, SpruceErrData}, nativefns::{self, ParamKind}, object::Object, visitor::Visitor};

use super::{token::{Token, Span, TokenKind}, functiondata::{Function, FunctionMeta}, symbols::Symbols, environment::{ConstantValue}, ast::{Ast, AstData, TypeKind}, decorated_ast::{DecoratedAst, DecoratedAstData, FunctionType}, sprucetype::SpruceType, name_resolution::{ResolutionTable, FunctionSignatureKind}, symtable::SymTable};

#[derive(Clone, Copy, PartialEq, Eq)]
enum ScopeType {
    None, Function, Method, Defer,
}

pub struct Analyser {
    had_error: bool,
    source: Rc<Source>,
    args: RunArgs,
    constants: Vec<ConstantValue>,
    functable: Vec<FunctionMeta>, // Type resolved table
    table: SymTable,
    res_table: Box<ResolutionTable>,
    defer_count: u32,
    last_return: bool,
    scope_type: ScopeType,
}

impl Analyser {
    pub fn new(source: Rc<Source>, args: RunArgs, res_table: Box<ResolutionTable>) -> Self {
        Self {
            had_error: false,
            source,
            args,
            constants: Vec::new(),
            functable: Vec::new(),
            table: SymTable::new(),
            res_table,
            defer_count: 0,
            last_return: false,
            scope_type: ScopeType::None,
        }
    }

    pub fn run(&mut self, program: &Box<Ast>) -> Result<(Box<DecoratedAst>, Symbols), SpruceErr> {
        nativefns::register_native_functions(self);

        self.register_all_functions()?;
        
        let program = self.visit(program)?;

        match self.find_function_str("main") {
            Some(_) => {} // Type-check main function for correct signature
            None => return Err(self.error("Cannot find function 'main'".into())),
        }

        if self.had_error {
            return Err(self.error("Error(s) occured".into()));
        }

        let ResolutionTable { symbol_values, .. } = &*self.res_table;
        Ok((program, symbol_values.clone()))
    }

    pub fn add_native_fn(
        &mut self,
        identifier: &'static str,
        param_types: ParamKind,
        return_type: SpruceType,
    ) {
        let function = Function::Native { 
            identifier,
            param_types,
            return_type,
        };

        // Add to function table
        self.add_function(identifier.to_string(), function);
    }

    fn register_all_functions(&mut self) -> Result<(), SpruceErr> {
        let signatures = self.res_table.signatures.clone();

        for func in signatures.into_iter() {
            if let FunctionSignatureKind::User { parameters, return_id } = &func.kind {

                let param_types = if let Some(parameters) = parameters {
                    let mut inner = Vec::new();
                    for item in parameters {
                        inner.push(self.get_type_from_ast(item)?);
                    }

                    Some(inner)
                } else { None };

                let return_type = match return_id {
                    Some(return_id) => self.get_type_from_ast(return_id)?,
                    None => SpruceType::None,
                };

                self.add_function(func.identifier.clone(), Function::User {
                    param_types,
                    return_type,
                    empty: false,
                })
            }
        }

        Ok(())
    }

    #[inline]
    fn add_function(&mut self, identifier: String, func: Function) {
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
    fn push_scope_type(&mut self, kind: ScopeType) -> ScopeType {
        let prev = self.scope_type;
        self.scope_type = kind;
        prev
    }

    #[inline]
    fn pop_scope_type(&mut self, kind: ScopeType) {
        self.scope_type = kind;
    }

    #[inline]
    fn push_scope(&mut self) {
        self.table.new_scope();
    }

    #[inline]
    fn pop_scope(&mut self) {
        self.table.close_scope();
    }
    
    #[inline]
    fn error(&mut self, message: String) -> SpruceErr {
        SpruceErr::new(message, SpruceErrData::Analyser { file_path: (*self.source.file_path).clone() })
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

    fn find_function_str(&self, id: &str) -> Option<&FunctionMeta> {
        for func in &self.functable {
            if func.identifier.as_str() == id {
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
        parameters: Option<Vec<Box<SpruceType>>>,
        return_type: Box<SpruceType>,
    ) -> Result<(), SpruceErr>
    {
        self.register_local(&identifier, false, SpruceType::Function {
            is_native: false,
            parameters: parameters.clone(),
            return_type: return_type.clone(),
        });
        
        self.add_function(identifier.span.slice_source().to_string(), Function::User {
            param_types: parameters.and_then(|p| Some(p.iter().map(|k| *k.clone()).collect())),
            return_type: *return_type,
            empty: false
        });

        Ok(())
    }

    fn mark_function_empty(&mut self, id: &Span) {
        for func in &mut self.functable {
            // Cannot mark native functions as empty
            if func.identifier.as_str() == id.slice_source() {
                func.function.mark_empty();
            }
        }
    }

    #[inline]
    fn evaluate_params(
        &mut self,
        token: Token,
        parameters: &Option<Vec<Box<Ast>>>,
    ) -> Result<(Box<DecoratedAst>, Option<Vec<Box<SpruceType>>>), SpruceErr> {
        // Register locals from parameters
        if let Some(ref parameters) = parameters {
            let mut params = Vec::new();
            let mut types = Vec::new();

            for param in parameters {
                let AstData::Parameter { type_name } = &param.data else { unreachable!("{:#?}", param.data) };
                let kind = self.get_type_from_ast(type_name)?;
                self.register_local(&param.token, false, kind.clone());
                
                types.push(Box::new(kind.clone()));
                params.push(DecoratedAst::new_parameter(param.token.clone(), kind));
            }

            Ok((DecoratedAst::new_parameter_list(token,
                if parameters.len() == 0 {
                        None
                    } else {
                        Some(params)
                    },
                ),
                Some(types),
            ))
        } else {
            Ok((DecoratedAst::new_parameter_list(token, None), None))
        }
    }

    fn find_type_of(&self, node: &Box<DecoratedAst>) -> Result<SpruceType, SpruceErr> {
        Ok(match &node.data {
            DecoratedAstData::Literal(kind, _) => kind.clone(),
            DecoratedAstData::TupleLiteral(kind, _) => kind.clone(),
            DecoratedAstData::ListLiteral(kind, _) => kind.clone(),
            DecoratedAstData::SymbolLiteral(_) => SpruceType::Symbol,
            
            DecoratedAstData::BinaryOp { kind, .. } => kind.clone(),
            DecoratedAstData::UnaryOp { kind, .. } => kind.clone(),
            DecoratedAstData::LogicalOp {..} => SpruceType::Bool,
            
            DecoratedAstData::FunctionCall { kind, .. } => kind.clone(),
            DecoratedAstData::Identifier(kind) => match kind {
                SpruceType::Lazy(inner) => {
                    let mut inner = inner;

                    while let SpruceType::Lazy(i) = &**inner {
                        inner = i;
                    }

                    *inner.clone()
                },
                _ => kind.clone(),
            },
            DecoratedAstData::VarAssign { lhs, .. } => self.find_type_of(lhs)?,
            DecoratedAstData::VarAssignEqual { lhs, .. } => self.find_type_of(lhs)?,
            DecoratedAstData::Type(kind) => kind.clone(),

            DecoratedAstData::Parameter(kind) => kind.clone(),
            DecoratedAstData::Function { function_type, parameters, kind, .. } => {
                if *function_type == FunctionType::Anonymous {
                    SpruceType::Function {
                        is_native: false,
                        parameters: match &parameters.data {
                            DecoratedAstData::ParameterList(parameters) => {
                                if let Some(parameters) = parameters {
                                    let mut kinds = Vec::new();

                                    for item in parameters {
                                        let DecoratedAstData::Parameter(kind) = &item.data else { unreachable!() };
                                        kinds.push(Box::new(kind.clone()));
                                    }

                                    Some(kinds)
                                } else {
                                    None
                                }
                            }
                            _ => unreachable!(),
                        },
                        return_type: Box::new(kind.clone()),
                    }
                } else {
                    let func = self.find_function(&node.token.span).unwrap();
                    func.function.to_type()
                }
            },
            DecoratedAstData::IndexGetter { expression, ..} => self.find_type_of(expression)?,
            DecoratedAstData::Ternary { kind, ..} => kind.clone(),
            DecoratedAstData::IfStatement { kind, ..} => kind.clone(),
            DecoratedAstData::ForStatement {..} => SpruceType::None,
            DecoratedAstData::DoWhileStatement {..} => SpruceType::None,
            DecoratedAstData::Body(kind, _) => kind.clone(),
            DecoratedAstData::ExpressionStatement(kind, _, _) => kind.clone(),
            DecoratedAstData::Lazy(inner) => SpruceType::Lazy(Box::new(self.find_type_of(inner)?)),
            DecoratedAstData::Defer(_, _) => SpruceType::None,
            DecoratedAstData::Return(kind, _) => kind.clone(),
            DecoratedAstData::Empty => SpruceType::Any,

            _ => return Err(SpruceErr::new(format!(
                    "Cannot find type of '{}' - {:#?}",
                    node.token.span.slice_source(),
                    node,
                ),
                SpruceErrData::Analyser { file_path: (*self.source.file_path).clone() }
            ))
        })
    }

    fn get_type_from_ast(&mut self, node: &Box<Ast>) -> Result<SpruceType, SpruceErr> {
        let AstData::Type { kind } = &node.data else { unreachable!() };

        Ok(match &*kind {
            TypeKind::Standard => {
                match node.token.span.slice_source() {
                    "any" => SpruceType::Any,
                    "int" => SpruceType::Int,
                    "float" => SpruceType::Float,
                    "bool" => SpruceType::Bool,
                    "string" => SpruceType::String,
                    "symbol" => SpruceType::Symbol,
                    // TODO: Check for custom type before error
                    _ => {
                        self.error_no_exit(format!(
                                "Unknown type identifier '{}'",
                                node.token.span.slice_source(),
                            ),
                            &node.token
                        );
                        SpruceType::Error
                    },
                }
            }
            TypeKind::Tuple(ref inner) => {
                let mut types = Vec::new();

                for item in inner {
                    types.push(Box::new(self.get_type_from_ast(item)?))
                }

                SpruceType::Tuple(types)
            },
            TypeKind::List(ref inner) => SpruceType::List(Box::new(self.get_type_from_ast(inner)?)),
            TypeKind::Lazy(inner) => SpruceType::Lazy(Box::new(self.get_type_from_ast(inner)?)),
            // TypeKind::
            TypeKind::Function { parameters, return_type } => {
                SpruceType::Function {
                    is_native: false,
                    parameters: match parameters {
                        Some(ref parameters) => {
                            let mut types = Vec::new();

                            for kind in parameters {
                                types.push(Box::new(self.get_type_from_ast(kind)?));
                            }

                            Some(types)
                        }
                        None => None,
                    },
                    return_type: Box::new(self.get_type_from_ast(&return_type)?),
                }
            }
            _ => unimplemented!("Type from AST"),
        })
    }
}

impl Visitor<Ast, Box<DecoratedAst>> for Analyser {
    fn visit(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        Ok(match &node.data {
            AstData::Literal => self.visit_literal(node)?,
            AstData::TupleLiteral(_) => self.visit_tuple_literal(node)?,
            AstData::ListLiteral(_) => self.visit_list_literal(node)?,
            AstData::SymbolLiteral => self.visit_symbol_literal(node)?,

            AstData::Type {..} => self.visit_type(node)?,
            AstData::Identifier => self.visit_identifier(node)?,
            AstData::BinaryOp {..} => self.visit_binary_op(node)?,
            AstData::UnaryOp {..} => self.visit_unary_op(node)?,
            AstData::LogicalOp {..} => self.visit_logical_op(node)?,

            AstData::VarDeclaration {..} => self.visit_var_declaration(node)?,
            AstData::VarDeclarations(_) => self.visit_var_declarations(node)?,
            AstData::VarAssign {..} => self.visit_var_assign(node)?,
            AstData::VarAssignEqual {..} => self.visit_var_assign_equal(node)?,
            AstData::FunctionCall {..} => self.visit_function_call(node)?,

            AstData::Ternary {..} => self.visit_ternary(node)?,
            AstData::IfStatement {..} => self.visit_if_statement(node)?,
            AstData::ForStatement {..} => self.visit_for_statement(node)?,
            AstData::DoWhileStatement {..} => self.visit_do_while_statement(node)?,
            AstData::Body(_) => self.visit_body(node, true)?,
            AstData::ExpressionStatement(_, _) => self.visit_expression_statement(node)?,

            AstData::TypeDefinition { inner } => self.visit(inner)?,
            AstData::StructDefinition {..} => self.visit_struct_def(node)?,

            AstData::IndexGetter {..} => self.visit_index_getter(node)?,

            AstData::Function {..} => {
                let prev = self.push_scope_type(ScopeType::Function);
                let func = self.visit_function(node)?;
                self.pop_scope_type(prev);
                
                func
            },
            AstData::Lazy(_) => self.visit_lazy(node)?,
            AstData::Defer(_) => self.visit_defer(node)?,
            AstData::Return(_) => self.visit_return_statement(node)?,
            AstData::Program {..} => self.visit_program(node)?,
            AstData::Empty => DecoratedAst::new_empty(node.token.clone()),

            AstData::Comment => DecoratedAst::new_comment(node.token.clone()),
            _ => return Err(self.error(format!("Unknown node in check: {:#?}", node))),
        })
    }

    fn visit_identifier(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let identifier = &node.token;
        let mut kind = SpruceType::None;

        match self.table.find_local(&identifier.span, true) {
            Some(local) => {
                kind = local.kind.clone();
            },
            None => {
                match self.find_function(&identifier.span) {
                    Some(f) => kind = f.function.to_type(),
                    None => self.error_no_exit(format!(
                            "Identifier '{}' does not exist in the current context",
                            identifier.span.slice_source(),
                        ),
                        &identifier.clone()
                    )
                }
            }
        }

        Ok(DecoratedAst::new_identifier(identifier.clone(), kind))
    }

    fn visit_literal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
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

    fn visit_symbol_literal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::SymbolLiteral = &node.data else { unreachable!() };
        let index = self.res_table.symbol_values.find_or_add(&node.token.span);

        Ok(DecoratedAst::new_symbol(node.token.clone(), index))
    }

    fn visit_map_literal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_tuple_literal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::TupleLiteral(inner) = &node.data else { unreachable!() };
        let mut types = Vec::new();
        let mut values = Vec::new();

        for item in inner {
            let item = self.visit(&item)?;
            let type_of = self.find_type_of(&item)?;
            
            types.push(Box::new(type_of));
            values.push(item);
        }

        Ok(DecoratedAst::new_tuple_literal(node.token.clone(), values, SpruceType::Tuple(types)))
    }

    fn visit_list_literal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::ListLiteral(inner) = &node.data else { unreachable!() };
        let mut values = Vec::new();
        let mut list_type = SpruceType::None;

        // Check type of first item, then compare to rest
        if inner.len() > 0 {
            let first = self.visit(&inner[0])?;
            list_type = self.find_type_of(&first)?;
            values.push(first);
        }

        for (idx, item) in inner.iter().skip(1).enumerate() {
            let item = self.visit(&item)?;
            let type_of = &self.find_type_of(&item)?;
            
            if !list_type.is_same(type_of) {
                self.error_no_exit(format!(
                        "Item at index {idx} in list, has type {} but expects {}",
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

    fn visit_expression_statement(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::ExpressionStatement(is_statement, inner) = &node.data else { unreachable!() };
        let inner = self.visit(inner)?;
        let kind = self.find_type_of(&inner)?;
        Ok(DecoratedAst::new_expr_statement(*is_statement, inner, kind))
    }

    fn visit_comment(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_binary_op(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::BinaryOp { lhs, rhs } = &node.data  else { unreachable!() };

        match (&lhs.data, &rhs.data) {
            (AstData::Lazy(_), _) | (_, AstData::Lazy(_)) => {
                self.error_no_exit(
                    "Binary operation cannot contain lazy expressions".into(),
                    &node.token
                )
            }
            _ => {}
        }

        let lhs = self.visit(&lhs)?;
        let rhs = self.visit(&rhs)?;

        let lhs_type = self.find_type_of(&lhs)?;
        let rhs_type = self.find_type_of(&rhs)?;

        if !lhs_type.is_same(&rhs_type) {
            self.error_no_exit(format!(
                    "Binary operation type mismatch, {} and {}",
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
            SpruceType::Symbol => {
                self.error_no_exit(
                        "Cannot use binary operators on two symbols".into()
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

    fn visit_unary_op(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::UnaryOp { rhs } = &node.data  else { unreachable!() };
        
        if let AstData::Lazy(_) = &rhs.data {
            self.error_no_exit(
                "Unary operation cannot contain lazy expressions".into(),
                &node.token
            )
        }
        
        let rhs = self.visit(&rhs)?;
        let kind = self.find_type_of(&rhs)?;

        // Check operation on type
        match &kind {
            SpruceType::String => {
                self.error_no_exit(
                        format!(
                            "Cannot use unary operator '{}' on a string",
                            node.token.span.slice_source(),
                        )
                    , &node.token
                );
            }
            SpruceType::Symbol => {
                self.error_no_exit(
                    format!(
                        "Cannot use unary operator '{}' on a symbol",
                        node.token.span.slice_source(),
                    )
                    , &node.token
                );
            }
            SpruceType::List(_) => {
                self.error_no_exit(
                    format!(
                        "Cannot use unary operator '{}' on a list",
                        node.token.span.slice_source(),
                    )
                    , &node.token
                );
            }
            SpruceType::Tuple(_) => {
                self.error_no_exit(
                    format!(
                        "Cannot use unary operator '{}' on a tuple",
                        node.token.span.slice_source(),
                    )
                    , &node.token
                );
            }
            SpruceType::Function {..} => {
                self.error_no_exit(
                    format!(
                        "Cannot use unary operator '{}' on a function",
                        node.token.span.slice_source(),
                    )
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

    fn visit_logical_op(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::LogicalOp { lhs, rhs } = &node.data else { unreachable!() };

        match (&lhs.data, &rhs.data) {
            (AstData::Lazy(_), _) | (_, AstData::Lazy(_)) => {
                self.error_no_exit(
                    "Logical operation cannot contain lazy expressions".into(),
                    &node.token
                )
            }
            _ => {}
        }

        let lhs = self.visit(&lhs)?;
        let rhs = self.visit(&rhs)?;

        let lhs_type = self.find_type_of(&lhs)?;
        let rhs_type = self.find_type_of(&rhs)?;

        if !lhs_type.is_same(&rhs_type) {
            self.error_no_exit(format!(
                    "Logical operation type mismatch, {} and {}",
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

    fn visit_parameter(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        unreachable!()
    }

    fn visit_parameter_list(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        unreachable!()
    }

    fn visit_function(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Function { anonymous, parameters, return_type, body } = &node.data else { unreachable!() };
        let identifier = &node.token;

        let return_type = match *return_type {
            Some(ref return_) => {
                let expr = self.visit(return_)?;
                self.find_type_of(&expr)?
            },
            None => SpruceType::None,
        };
        
        let parameters = if *anonymous {
            self.push_scope();
            self.table.mark_depth_limit();
            let (parameters, _) = self.evaluate_params(identifier.clone(), parameters)?;
            parameters
        } else {
            let (parameters, types) = self.evaluate_params(identifier.clone(), parameters)?;
            self.register_function(identifier, types, Box::new(return_type.clone()))?;
            
            self.push_scope();
            if !self.table.is_global() {
                self.table.mark_depth_limit();
            }

            parameters
        };
        
        let body_ast = match &body.data {
            AstData::Return(_) => self.visit_return_statement(body)?,
            AstData::Body(_) => self.visit_body(body, false)?,
            _ => unreachable!("BODY AST"),
        };
        let body_type = self.find_type_of(&body_ast)?;

        // Make sure the body matches the return type
        if !body_type.is_same(&return_type) {
            self.error_no_exit(format!(
                    "Function '{}' expects the return type {}, but has {}",
                    if *anonymous { "anonymous" } else { node.token.span.slice_source() },
                    return_type,
                    body_type,
                ),
                &node.token
            );
        }

        self.pop_scope();
        self.table.reset_mark();
        
        if let DecoratedAstData::Body(_, ref statements) = &body_ast.data {
            if statements.len() == 0 {
                self.mark_function_empty(&identifier.span);
            }
        }

        let function_type = if *anonymous {
            FunctionType::Anonymous  
        } else if self.table.get_depth() > 0 && self.scope_type != ScopeType::Method {
            FunctionType::Inner
        } else {
            FunctionType::Standard
        };
        
        Ok(DecoratedAst::new_function(node.token.clone(), function_type, parameters, body_type, body_ast))
    }

    fn visit_function_call(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::FunctionCall { lhs, arguments } = &node.data else { unreachable!() };

        let (lhs, function) = match lhs.data {
            AstData::Identifier => {
                let kind = match self.table.find_local(&lhs.token.span, true) {
                    Some(local) => local.kind.clone(),
                    None => {
                        match self.find_function(&lhs.token.span) {
                            Some(f) => f.function.to_type(),
                            None => return Err(self.error(format!(
                                "Cannot call non-function '{}'",
                                lhs.token.span.slice_source(),
                            )))
                        }
                    }
                };

                let id = DecoratedAst::new_identifier(lhs.token.clone(), kind.clone());
                (id, kind)
            }

            _ => {
                let lhs = self.visit(lhs)?;
                let kind = self.find_type_of(&lhs)?;
                (lhs, kind)
            }
        };

        let mut is_any = function.is_same(&SpruceType::Unresolved);

        if !is_any {
            let function = match function {
                SpruceType::Function {..} => &function,
                _ => return Err(self.error(format!(
                    "Cannot call non-function '{}'",
                    lhs.token.span.slice_source(),
                )))
            };

            let SpruceType::Function { parameters, .. } = &function else { unreachable!() };
    
            match &parameters {
                Some(parameters) => {
                    if parameters.len() == 1 {
                        let p1 = &parameters[0];
                        if p1.is_same(&SpruceType::Any) {
                            is_any = true;
                        } else if arguments.len() > 1 {
                            self.error_no_exit(format!(
                                    "Function '{}' requires {} arguments, but receieved {}",
                                    lhs.token.span.slice_source(),
                                    parameters.len(),
                                    arguments.len(),
                                ),
                                &node.token,
                            );
                        }
                    }
                }
                None => {
                    if arguments.len() > 0 {
                        self.error_no_exit(format!(
                                "Function '{}' requires {} arguments, but receieved {}",
                                lhs.token.span.slice_source(),
                                if parameters.is_some() { parameters.as_ref().unwrap().len() } else { 0 },
                                arguments.len(),
                            ),
                            &node.token,
                        );
                    }
                }
            }
        }
        
        let mut args = Vec::new();
        let mut args_kind = Vec::new();

        for (idx, arg) in arguments.iter().enumerate() {
            args.push(self.visit(arg)?);

            args_kind.push(self.find_type_of(&args.last().unwrap())?);

            if !is_any {
                let SpruceType::Function { parameters, .. } = &function else { unreachable!() };

                if let Some(parameters) = &parameters {
                    if idx < parameters.len() {
                        let left_type = self.find_type_of(args.last().unwrap())?;
                        if !left_type.is_same(&parameters[idx]) {
                            self.error_no_exit(format!(
                                    "Argument {} in call to '{}' expects type {} but received {}",
                                    idx + 1,
                                    lhs.token.span.slice_source(),
                                    parameters[idx],
                                    left_type,
                                ),
                                &arg.token,
                            );
                        }
                    }
                }
            }
        }

        Ok(DecoratedAst::new_function_call(lhs.token.clone(), match function {
            SpruceType::Function { return_type, ..} => *return_type.clone(),
            _ => SpruceType::Unresolved,
            }, 
        lhs, args))
    }

    fn visit_var_declaration(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::VarDeclaration { is_mutable, kind, expression } = &node.data else { unreachable!() };
        let identifier = &node.token;
        
        if !is_mutable && expression.is_none() {
            self.error_no_exit(format!(
                    "Immutable variable '{}' must be bound to a value",
                    identifier.span.slice_source(),
                ),
                identifier
            );
        }

        let expression = match expression {
            Some(expr) => self.visit(expr)?,
            None => DecoratedAst::new_empty(identifier.clone()),
        };

        let expr_kind = self.find_type_of(&expression)?;

        let kind = match kind {
            Some(kind) => {
                let kind = self.get_type_from_ast(kind)?;

                if !kind.is_same(&expr_kind) {
                    self.error_no_exit(format!(
                            "Variable '{}' expected type {} but received {}",
                            identifier.span.slice_source(),
                            kind,
                            expr_kind,
                        ),
                        identifier
                    );
                }
                kind
            },
            None => expr_kind.clone(),
        };

        if let DecoratedAstData::Empty = &expression.data {
            if kind.is_same(&SpruceType::Any) {
                self.error_no_exit(format!(
                        "Cannot use type 'any' on '{}' without initialising with an expression",
                        identifier.span.slice_source(),
                    ),
                    identifier,
                );
            }
        }

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

        Ok(DecoratedAst::new_var_decl(node.token.clone(), *is_mutable, expr_kind, expression))
    }

    fn visit_var_declarations(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::VarDeclarations(decls) = &node.data else { unreachable!() };
        let mut declarations = Vec::new();
        for decl in decls {
            declarations.push(self.visit_var_declaration(&decl)?);
        }
        Ok(DecoratedAst::new_var_decls(declarations))
    }

    fn visit_var_assign(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
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
                
                if discriminant(&kind) == discriminant(&SpruceType::Any) {
                    self.error_no_exit(format!(
                        "Currently cannot assign value to un-initialised variable '{}'",
                        identifier.span.slice_source(),
                    ),
                    &identifier
                );
                } else if !expr_type.is_same(&kind) {
                    self.error_no_exit(format!(
                            "Cannot assign type {} to '{}' where type {} is expected",
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

    fn visit_var_assign_equal(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::VarAssignEqual { operator, lhs, expression } = &node.data else { unreachable!() };
        let identifier = &node.token;

        let lhs = self.visit(&lhs)?;
        let expression = self.visit(&expression)?;

        match self.table.find_local(&identifier.span, true) {
            Some(local) => {
                let is_mutable = local.mutable;
                let kind = local.kind.clone();
                let expr_type = self.find_type_of(&expression)?;

                if !is_mutable {
                    self.error_no_exit(format!(
                            "Cannot re-assign an immutable value '{}'",
                            identifier.span.slice_source(),
                        ),
                        &identifier
                    );
                }
                
                if discriminant(&kind) == discriminant(&SpruceType::Any) {
                    self.error_no_exit(format!(
                        "Currently cannot assign value to un-initialised variable '{}'",
                        identifier.span.slice_source(),
                    ),
                    &identifier
                );
                } else if !expr_type.is_same(&kind) {
                    self.error_no_exit(format!(
                            "Cannot assign type {} to '{}' where type {} is expected",
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

        Ok(DecoratedAst::new_var_assign_equal(node.token.clone(), operator.clone(), lhs, expression))
    }

    #[inline]
    fn visit_type(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        Ok(DecoratedAst::new_type(node.token.clone(), self.get_type_from_ast(&node)?))
    }

    fn visit_type_def(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        unreachable!()
    }

    fn visit_struct_def(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::StructDefinition { is_ref, items } = &node.data else { unreachable!() };

        self.push_scope();

        let mut kind = SpruceType::Error;

        let items = if items.is_some() {
            let mut inner = Vec::new();

            let mut field_names = HashSet::new();
            let mut func_names = HashSet::new();

            let mut field_types = Vec::new();
            let mut func_types = Vec::new();

            for item in items.as_ref().unwrap() {
                match &item.data {
                    AstData::Parameter { type_name } => {
                        if !field_names.insert(item.token.span.slice_source()) {
                            self.error_no_exit(format!(
                                "Struct '{}' already contains field '{}'",
                                node.token.span.slice_source(),
                                item.token.span.slice_source(),
                            ), &item.token);
                        }

                        let kind = self.get_type_from_ast(type_name)?;
                        field_types.push((item.token.span.clone(), Box::new(kind.clone())));
                        
                        inner.push(DecoratedAst::new_parameter(item.token.clone(), kind));
                    }
                    AstData::Function {..} => {
                        if !func_names.insert(item.token.span.slice_source()) {
                            self.error_no_exit(format!(
                                "Struct '{}' already contains method '{}'",
                                node.token.span.slice_source(),
                                item.token.span.slice_source(),
                            ), &item.token);
                        }

                        let prev = self.push_scope_type(ScopeType::Method);
                        let func = self.visit_function(item)?;
                        self.pop_scope_type(prev);

                        func_types.push(Box::new(self.find_type_of(&func)?));
                        inner.push(func);
                    }
                    _ => return Err(self.error(format!(
                        "Unknown item in struct body {:#?}",
                        item,
                    )))
                }
            }

            kind = SpruceType::Struct {
                is_ref: *is_ref,
                identifier: node.token.span.clone(),
                fields: Some(field_types),
                methods: Some(func_types),
            };

            Some(inner)
        } else { None };

        self.pop_scope();

        Ok(DecoratedAst::new_struct_definition(node.token.clone(), kind, *is_ref, items))
    }

    fn visit_ternary(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Ternary { condition, true_body, false_body } = &node.data else { unreachable!() };

        let condition = self.visit(condition)?;
        let true_body = self.visit(true_body)?;
        let false_body = self.visit(false_body)?;

        let condition_type = self.find_type_of(&condition)?;

        if !condition_type.is_same(&SpruceType::Bool) {
            self.error_no_exit(format!(
                    "Ternary statement condition expects a bool, but is a {}",
                    condition_type,
                ),
                &condition.token
            );
        }

        let true_type = self.find_type_of(&true_body)?;
        let false_type = self.find_type_of(&false_body)?;

        if !true_type.is_same(&false_type) {
            self.error_no_exit(format!(
                    "Ternary statement branches must return the same type. Expected {} but received {}",
                    true_type,
                    false_type,
                ),
                &false_body.token
            );
        }

        Ok(DecoratedAst::new_ternary(node.token.clone(), condition, true_type, true_body, false_body))
    }

    fn visit_if_statement(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::IfStatement { is_expression, condition, true_body, false_body } = &node.data else { unreachable!() };

        let condition = self.visit(condition)?;
        let true_body = self.visit(true_body)?;
        let false_body = match false_body {
            Some(ref body) => Some(self.visit(body)?),
            None => None,
        };

        let condition_type = self.find_type_of(&condition)?;

        if !condition_type.is_same(&SpruceType::Bool) {
            self.error_no_exit(format!(
                    "If statement condition expects a bool, but is a {}",
                    condition_type,
                ),
                &condition.token
            );
        }

        let true_type = self.find_type_of(&true_body)?;
        match false_body {
            Some(ref body) => {
                let false_type = self.find_type_of(body)?;

                if !true_type.is_same(&false_type) {
                    self.error_no_exit(format!(
                            "If statement branches must return the same type. Expected {} but received {}",
                            true_type,
                            false_type,
                        ),
                        &body.token
                    );
                }
            }
            None => {},
        };

        Ok(DecoratedAst::new_if_statement(node.token.clone(), *is_expression, condition, true_type, true_body, false_body))
    }

    fn visit_for_statement(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::ForStatement { variable, condition, increment, body } = &node.data else { unreachable!() };

        self.push_scope();

        let variable = match variable {
            Some(variable) => Some(self.visit(variable)?),
            None => None,
        };

        let condition = self.visit(condition)?;

        let increment = match increment {
            Some(increment) => Some(self.visit(increment)?),
            None => None,
        };

        let body = self.visit(body)?;

        self.pop_scope();

        Ok(DecoratedAst::new_for_statement(node.token.clone(), variable, condition, increment, body))
    }

    fn visit_do_while_statement(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::DoWhileStatement { body, condition } = &node.data else { unreachable!() };

        let condition = self.visit(condition)?;
        let body = self.visit(body)?;

        Ok(DecoratedAst::new_do_while_statement(node.token.clone(), body, condition))
    }

    fn visit_index_getter(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::IndexGetter { expression, index } = &node.data else { unreachable!() };

        let expression = self.visit(expression)?;
        let index = self.visit(index)?;
        let index_type = self.find_type_of(&index)?;

        if !index_type.is_same(&SpruceType::Int) {
            self.error_no_exit(format!(
                "Must index items with an integer, but received {}",
                index_type,
            ), &node.token);
        }

        Ok(DecoratedAst::new_index_getter(node.token.clone(), expression, index))
    }

    fn visit_index_setter(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_property_getter(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_property_setter(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_switch_statement(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_switch_case(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_lazy(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Lazy(expression) = &node.data else { unreachable!() };
        let body = self.visit(expression)?;
        Ok(DecoratedAst::new_lazy(node.token.clone(), body))
    }

    fn visit_defer(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Defer(expression) = &node.data else { unreachable!() };
        
        let prev = self.push_scope_type(ScopeType::Defer);

        self.defer_count += 1;
        let body = self.visit(expression)?;

        if let DecoratedAstData::Body(_, statements) = &body.data {
            if statements.len() > 0 {
                if let DecoratedAstData::ExpressionStatement(_, is_statement, _) = statements.last().unwrap().data {
                    if !is_statement {
                        self.error_no_exit(
                            "Cannot use expression as last item in defer block".into(),
                            &node.token,
                        );
                    }
                }
            }
        }

        self.pop_scope_type(prev);

        Ok(DecoratedAst::new_defer(node.token.clone(), self.defer_count - 1, body))
    }

    fn visit_return_statement(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Return(expression) = &node.data else { unreachable!() };

        if self.scope_type == ScopeType::Defer {
            self.error_no_exit(
                "Cannot return inside of a defer block".into(),
                &node.token,
            )
        }

        let (kind, expression) = match expression {
            Some(expr) => {
                let expr = self.visit(expr)?;
                (self.find_type_of(&expr)?, Some(expr))
            }
            None => (SpruceType::None, None),
        };

        Ok(DecoratedAst::new_return(node.token.clone(), kind, expression))
    }

    fn visit_body(&mut self, node: &Box<Ast>, new_scope: bool) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Body(inner) = &node.data else { unreachable!() };
        let mut statements = Vec::new();

        if new_scope {
            self.push_scope();
        }

        for (idx, item) in inner.iter().enumerate() {
            statements.push(self.visit(item)?);

            if let AstData::ExpressionStatement(is_statement, _) = &item.data {
                if !is_statement && idx < inner.len() - 1 {
                    self.error_no_exit(
                        "Trying to use an open expression before the last statement".into(),
                        &item.token
                    );
                }
            }
        }

        if new_scope {
            self.pop_scope();
        }

        let kind = match statements.last() {
            Some(n) => match &n.data {
                DecoratedAstData::ExpressionStatement(kind, is_statement, _) => {
                    if *is_statement {
                        SpruceType::None
                    } else {
                        kind.clone()
                    }
                }
                DecoratedAstData::Return(kind, _) => kind.clone(),
                DecoratedAstData::Body(kind, _) => kind.clone(),
                _ => SpruceType::None,
            },
            None => SpruceType::None,
        };

        Ok(DecoratedAst::new_body(node.token.clone(), statements, kind))
    }

    fn visit_include(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        todo!()
    }

    fn visit_program(&mut self, node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        let AstData::Program { source, body } = &node.data else { unreachable!() };
        let mut statements = Vec::new();

        for item in body {
            statements.push(self.visit(item)?);
        }

        Ok(DecoratedAst::new_program(node.token.clone(), Rc::clone(source), statements))
    }

    fn visit_empty(&mut self, _node: &Box<Ast>) -> Result<Box<DecoratedAst>, SpruceErr> {
        unreachable!()
    }
}