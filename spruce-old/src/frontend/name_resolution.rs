use std::{rc::Rc, collections::HashSet};

use crate::{source::Source, RunArgs, error::{SpruceErr, SpruceErrData}, nativefns, visitor::Visitor};

use super::{token::{Span, Token}, symtable::SymTable, symbols::Symbols, ast::{Ast, AstData, TypeKind}, sprucetype::SpruceType};

#[derive(Debug, Clone)]
pub enum FunctionSignatureKind {
    User {
        parameters: Option<Vec<Rc<Ast>>>,
        return_id: Option<Rc<Ast>>,
    },
    Native,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub identifier: String,
    pub kind: FunctionSignatureKind,
}

#[derive(Debug, Clone)]
pub struct StructSignature {
    pub identifier: String,
    pub is_ref: bool,
    pub fields: Option<Vec<Rc<Ast>>>,
    pub functions: Option<Vec<FunctionSignature>>,
}

#[derive(Debug, Clone)]
pub struct ResolutionTable {
    pub symbol_values: Symbols,
    pub signatures: Vec<FunctionSignature>,
    pub struct_types: Vec<StructSignature>,
}

impl ResolutionTable {
    pub fn new() -> Self {
        Self {
            symbol_values: Symbols::new(),
            signatures: Vec::new(),
            struct_types: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct LookAhead {
    token: Token,
    args: u8,
}

pub struct NameResolver {
    had_error: bool,
    source: Rc<Source>,
    args: RunArgs,
    table: SymTable,
    unresolved: Vec<LookAhead>,
    res_table: Box<ResolutionTable>,
}

impl NameResolver {
    pub fn new(source: Rc<Source>, args: RunArgs) -> Self {
        Self {
            had_error: false,
            source: Rc::clone(&source),
            args,
            table: SymTable::new(),
            unresolved: Vec::new(),
            res_table: Box::new(ResolutionTable::new()),
        }
    }

    pub fn run(&mut self, program: &Rc<Ast>) -> Result<Box<ResolutionTable>, SpruceErr> {
        nativefns::register_native_functions_ids(self);

        self.visit_program(program)?;

        // Try to resolve calls that were not during compilation
        self.resolve_function_calls();

        if self.had_error {
            return Err(self.error("Error(s) occured".into()));
        }
        
        Ok(self.res_table.clone())
    }

    #[inline]
    pub fn add_native_fn(&mut self, identifier: &'static str) {
        self.add_function(identifier.to_string(), FunctionSignatureKind::Native);
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

    #[inline]
    fn add_function(&mut self, identifier: String, kind: FunctionSignatureKind) {
        self.res_table.signatures.push(FunctionSignature { 
            identifier,
            kind,
        });
    }

    #[inline]
    fn push_scope(&mut self) {
        self.table.new_scope();
    }

    #[inline]
    fn pop_scope(&mut self) {
        self.table.close_scope();
    }

    fn find_function_str(&self, id: &str) -> Option<()> {
        for func in &self.res_table.signatures {
            if func.identifier.as_str() == id {
                return Some(());
            }
        }

        None
    }

    #[inline]
    fn find_function(&self, span: &Span) -> Option<()> {
        self.find_function_str(span.slice_source())
    }

    fn find_struct_str(&self, id: &str) -> Option<StructSignature> {
        for struct_ in &self.res_table.struct_types {
            if struct_.identifier.as_str() == id {
                return Some(struct_.clone());
            }
        }

        None
    }

    #[inline]
    fn find_struct(&self, span: &Span) -> Option<StructSignature> {
        self.find_struct_str(span.slice_source())
    }

    fn find_struct_field(&self, signature: &StructSignature, field_name: &Span) -> bool {
        if let Some(fields) = &signature.fields {
            for field in fields {
                if field.token.span.slice_source() == field_name.slice_source() {
                    return true;
                }
            }
        }

        false
    }

    fn register_local(&mut self, token: &Token, mutable: bool) {
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

            None => self.table.new_local(token.clone().span, mutable, Rc::new(SpruceType::Any)),
        }
    }

    fn resolve_function_calls(&mut self) {
        let mut unresolved = Vec::new();

        for lookahead in &self.unresolved {
            match self.find_function(&lookahead.token.span) {
                Some(_) => {}
                None => unresolved.push((lookahead.token.clone(), lookahead.args.clone())),
            }
        }

        // Identifiers that were still not found
        for (token, _args) in unresolved {
            self.error_no_exit(format!(
                    "Function '{}' does not exist",
                    token.span.slice_source(),
                ),
                &token
            );
        }
    }

    #[inline]
    fn evaluate_params(
        &mut self,
        function_name: &Token,
        parameters: &Option<Vec<Rc<Ast>>>,
    ) -> Result<Option<Vec<Rc<Ast>>>, SpruceErr> {
        // Register locals from parameters
        if let Some(parameters) = parameters {
            let mut out = Vec::new();
            let mut param_names = HashSet::new();

            for param in parameters {
                let AstData::Parameter { type_signature } = &param.data else { unreachable!("{:#?}", param.data) };
                self.get_type_from_ast(type_signature)?;
                
                if !param_names.insert(param.token.span.slice_source()) {
                    self.error_no_exit(
                        format!(
                            "Function with identifier '{}' already contains a parameter '{}'",
                            function_name.span.slice_source(),
                            param.token.span.slice_source(),
                        ),
                        function_name
                    );
                }
                
                self.register_local(&param.token, false);
                // Kinda gross here, might need to use Rc instead of box
                out.push(type_signature.clone());
            }
            return Ok(Some(out));
        }

        Ok(None)
    }

    fn register_function(
        &mut self,
        identifier: &Token,
        parameters: Option<Vec<Rc<Ast>>>,
        return_id: Option<Rc<Ast>>,
    ) -> Result<(), SpruceErr>
    {
        // Function already exists
        if self.find_function(&identifier.span).is_some() {
            self.error_no_exit(
                format!(
                    "Function with identifier '{}' already exists",
                    identifier.span.slice_source()
                ),
                &identifier
            );
            return Ok(());
        }

        self.register_local(&identifier, false);
        
        self.add_function(
            identifier.span.slice_source().to_string(),
            FunctionSignatureKind::User { parameters, return_id }
        );

        Ok(())
    }

    fn add_struct_type(&mut self, signature: StructSignature) {
        for item in &self.res_table.struct_types {
            if item.identifier.as_str() == signature.identifier.as_str() {
                self.error(format!(
                    "Type '{}' has already been defined",
                    signature.identifier,
                ));
                return;
            }
        }

        self.res_table.struct_types.push(signature);
    }

    fn get_type_from_ast(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Type { kind } = &node.data else { unreachable!() };

        Ok(match &*kind {
            TypeKind::Standard => {
                match node.token.span.slice_source() {
                    "any" | "int" | "float" | "bool" | "string" | "symbol" => {},
                    _ => {
                        if let Some(_) = self.find_struct(&node.token.span) {
                            return Ok(());
                        }

                        self.error_no_exit(format!(
                                "Unknown type identifier '{}'",
                                node.token.span.slice_source(),
                            ),
                            &node.token
                        );
                    },
                }
            }
            TypeKind::Tuple(inner) => {
                for item in inner {
                    self.get_type_from_ast(item)?;
                }
            },
            TypeKind::Array(inner) => self.get_type_from_ast(inner)?,
            TypeKind::Lazy(inner) => self.get_type_from_ast(inner)?,
            TypeKind::ErrorOrValue(lhs, rhs) => {
                self.get_type_from_ast(lhs)?;
                self.get_type_from_ast(rhs)?;
            },
            TypeKind::Function { parameters, return_type } => {
                match parameters {
                    Some(parameters) => {
                        for kind in parameters {
                            self.get_type_from_ast(kind)?;
                        }
                    }
                    None => {},
                }

                self.get_type_from_ast(&return_type)?;
            }
        })
    }
}

impl Visitor<Ast, ()> for NameResolver {
    fn visit(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        Ok(match &node.data {
            AstData::TupleLiteral(_) => self.visit_tuple_literal(node)?,
            AstData::ArrayLiteral(_) => self.visit_array_literal(node)?,
            AstData::SymbolLiteral => self.visit_symbol_literal(node)?,
            AstData::StructLiteral(_, _) => self.visit_struct_literal(node)?,
            AstData::ErrorOrValue {..} => self.visit_error_or_value(node)?,

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
            AstData::Payload(_) => self.visit_payload(node)?,
            AstData::IfStatement {..} => self.visit_if_statement(node)?,
            AstData::ForStatement {..} => self.visit_for_statement(node)?,
            AstData::DoWhileStatement {..} => self.visit_do_while_statement(node)?,
            AstData::Body(_) => self.visit_body(node, true)?,
            AstData::ExpressionStatement(_, _) => self.visit_expression_statement(node)?,

            AstData::Function {..} => self.visit_function(node)?,
            AstData::StructDefinition {..} => self.visit_struct_def(node)?,

            AstData::IndexGetter {..} => self.visit_index_getter(node)?,
            AstData::IndexSetter {..} => self.visit_index_setter(node)?,

            AstData::PropertyGetter {..} => self.visit_property_getter(node)?,
            AstData::PropertySetter {..} => self.visit_property_setter(node)?,

            AstData::Raw {..} => self.visit_raw(node)?,
            AstData::Defer {..} => self.visit_defer(node)?,
            AstData::Lazy {..} => self.visit_lazy(node)?,
            AstData::Return {..} => self.visit_return_statement(node)?,
            AstData::Program {..} => self.visit_program(node)?,

            AstData::Literal | AstData::Comment | AstData::Empty => {},
            _ => return Err(self.error(format!("Unknown node in resolver: {:#?}", node))),
        })
    }

    fn visit_identifier(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let identifier = &node.token;

        match self.table.find_local(&identifier.span, true) {
            Some(_) => {},
            None => {
                match self.find_function(&identifier.span) {
                    Some(_) => {},
                    None => self.error_no_exit(format!(
                            "Identifier '{}' does not exist in the current context",
                            identifier.span.slice_source(),
                        ),
                        &identifier.clone()
                    )
                }
            }
        }

        Ok(())
    }

    fn visit_literal(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        unreachable!()
    }

    #[inline]
    fn visit_symbol_literal(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::SymbolLiteral = &node.data else { unreachable!() };
        _= self.res_table.symbol_values.find_or_add(&node.token.span);
        Ok(())
    }

    fn visit_struct_literal(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::StructLiteral(identifier, arguments) = &node.data else { unreachable!() };
        let mut fields = HashSet::new();

        let signature = if let Some(signature) = self.find_struct(&identifier.span) {
            Some(signature)
        } else {
            self.error_no_exit(format!(
                "Cannot find struct with identifier '{}'",
                identifier.span.slice_source(),
            ), identifier);
            None
        };

        for (argument_identifier, arg) in arguments {
            if !fields.insert(argument_identifier.span.slice_source()) {
                self.error_no_exit(format!(
                    "Field '{}' has already been defined in struct literal",
                    argument_identifier.span.slice_source(),
                ), argument_identifier);
            }

            if let Some(signature) = &signature {
                if !self.find_struct_field(signature, &argument_identifier.span) {
                    self.error_no_exit(format!(
                        "Struct '{}' does not contain a field named '{}'",
                        signature.identifier,
                        argument_identifier.span.slice_source(),
                    ), argument_identifier);
                }

                if arg.is_none() {
                    if let None = self.table.find_local(&argument_identifier.span, true) {
                        self.error_no_exit(format!(
                            "Cannot find local '{}' to insert as field in struct '{}'",
                            argument_identifier.span.slice_source(),
                            signature.identifier,
                        ), argument_identifier);
                    }
                }
            }

            if let Some(arg) = arg {
                self.visit(arg)?;
            }
        }

        Ok(())
    }

    fn visit_tuple_literal(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::TupleLiteral(inner) = &node.data else { unreachable!() };

        for item in inner {
            self.visit(&item)?;
        }

        Ok(())
    }

    fn visit_array_literal(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::ArrayLiteral(inner) = &node.data else { unreachable!() };

        for item in inner {
            self.visit(&item)?;
        }

        Ok(())
    }

    fn visit_error_or_value(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::ErrorOrValue { expression, .. } = &node.data else { unreachable!() };
        self.visit(expression)
    }

    fn visit_expression_statement(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::ExpressionStatement(_, inner) = &node.data else { unreachable!() };
        self.visit(inner)
    }

    fn visit_comment(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        unreachable!()
    }

    fn visit_raw(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Raw { returns, .. } = &node.data else { unreachable!() };

        if let Some(returns) = returns {
            self.visit_type(returns)?;
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::BinaryOp { lhs, rhs } = &node.data  else { unreachable!() };
        self.visit(&lhs)?;
        self.visit(&rhs)?;
        Ok(())
    }

    fn visit_unary_op(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::UnaryOp { rhs } = &node.data  else { unreachable!() };
        self.visit(&rhs)?;
        Ok(())
    }

    fn visit_logical_op(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::LogicalOp { lhs, rhs } = &node.data else { unreachable!() };
        self.visit(&lhs)?;
        self.visit(&rhs)?;
        Ok(())
    }

    fn visit_parameter(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        unreachable!()
    }

    fn visit_parameter_list(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        unreachable!()
    }

    fn visit_function(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Function { anonymous, parameters, return_type, body } = &node.data else { unreachable!() };
        let identifier = &node.token;
        
        if let Some(return_type) = return_type {
            self.visit(return_type)?;
        }
        
        if *anonymous {
            self.push_scope();
            self.table.mark_depth_limit();
            self.evaluate_params(identifier, parameters)?;
        } else {
            let params = self.evaluate_params(identifier, parameters)?;
            
            if self.table.is_global() {
                self.register_function(identifier, params, match return_type {
                    Some(ret) => Some(ret.clone()),
                    None => None,
                })?;
            } else {
                self.register_local(&identifier, false);
            }
            
            self.push_scope();
            if !self.table.is_global() {
                self.table.mark_depth_limit();
            }
        };
        
        match &body.data {
            AstData::Return(_) => self.visit_return_statement(body)?,
            AstData::Body(_) => self.visit_body(body, false)?,
            _ => unreachable!("BODY AST"),
        };
        
        self.pop_scope();
        self.table.reset_mark();
        
        Ok(())
    }

    fn visit_function_call(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::FunctionCall { lhs, arguments } = &node.data else { unreachable!() };

        match lhs.data {
            AstData::Identifier => {
                if let None = self.table.find_local(&lhs.token.span, true) {
                    if let None = self.find_function(&lhs.token.span) {
                        self.unresolved.push(LookAhead {
                            token: lhs.token.clone(),
                            args: arguments.len() as u8,
                        });
                    }
                }
            }

            _ => self.visit(lhs)?,
        }

        for arg in arguments {
            self.visit(arg)?;
        }

        Ok(())
    }

    fn visit_var_declaration(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarDeclaration { is_mutable, kind: _, expression } = &node.data else { unreachable!() };
        let identifier = &node.token;
        
        if !is_mutable && expression.is_none() {
            self.error_no_exit(format!(
                    "Immutable variable '{}' must be bound to a value",
                    identifier.span.slice_source(),
                ),
                identifier
            );
        }

        if let Some(expression) = expression {
            self.visit(expression)?;
        }

        self.register_local(identifier, *is_mutable);

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

        Ok(())
    }

    fn visit_var_declarations(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarDeclarations(decls) = &node.data else { unreachable!() };
        for decl in decls {
            self.visit_var_declaration(&decl)?;
        }
        Ok(())
    }

    fn visit_var_assign(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarAssign { lhs, expression } = &node.data else { unreachable!() };
        let identifier = &node.token;

        self.visit(&lhs)?;
        self.visit(&expression)?;

        match self.table.find_local(&identifier.span, true) {
            Some(_) => {},
            None => self.error_no_exit(format!(
                    "Cannot assign to variable '{}' as it does not exist or is not in the correct context",
                    identifier.span.slice_source(),
                ),
                &identifier
            ),
        }

        Ok(())
    }

    fn visit_var_assign_equal(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarAssignEqual { operator: _, lhs, expression } = &node.data else { unreachable!() };

        self.visit(&lhs)?;
        self.visit(&expression)?;

        match self.table.find_local(&node.token.span, true) {
            Some(_) => {}
            None => self.error_no_exit(format!(
                    "Cannot assign to variable '{}' as it does not exist or is not in the correct context",
                    node.token.span.slice_source(),
                ),
                &node.token
            ),
        }

        Ok(())
    }

    #[inline]
    fn visit_type(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        self.get_type_from_ast(&node)
    }

    fn visit_struct_def(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::StructDefinition { is_ref, items } = &node.data else { unreachable!() };
        
        self.push_scope();

        let signature = StructSignature {
            identifier: node.token.span.slice_source().to_string(),
            is_ref: *is_ref,
            fields: if let Some(items) = items {
                let mut fields = Vec::new();
                let mut field_names = HashSet::new();

                for item in items {
                    if let AstData::StructField { type_signature, default_value } = &item.data {
                        self.visit_type(type_signature)?;

                        if let Some(default_value) = default_value {
                            self.visit(default_value)?;
                        }

                        if !field_names.insert(item.token.span.slice_source()) {
                            self.error_no_exit(format!(
                                "Struct definition of '{}' already contains the field '{}'",
                                node.token.span.slice_source(),
                                item.token.span.slice_source(),
                            ), &item.token);
                        } else {
                            fields.push(Rc::clone(item));
                            self.register_local(&item.token, true);
                        }
                    }
                }

                Some(fields)
            } else { None },
            functions: if let Some(items) = items {
                let mut functions = Vec::new();

                for item in items {
                    if let AstData::Function { parameters, return_type, body, .. } = &item.data {
                        let parameters = self.evaluate_params(&item.token, parameters)?;

                        if let Some(return_type) = return_type {
                            self.visit(return_type)?;
                        }

                        self.visit(body)?;

                        functions.push(FunctionSignature {
                            identifier: item.token.span.slice_source().to_string(),
                            kind: FunctionSignatureKind::User {
                                parameters,
                                return_id: match return_type {
                                    Some(ret) => Some(ret.clone()),
                                    None => None,
                                }
                            },
                        });
                    }
                }

                Some(functions)
            } else { None },
        };

        self.pop_scope();
        self.add_struct_type(signature);

        Ok(())
    }

    fn visit_struct_field(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        unreachable!();
    }

    fn visit_payload(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Payload(expression) = &node.data else { unreachable!() };
        self.visit(expression)
    }

    fn visit_if_statement(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::IfStatement { condition, true_body, false_body, .. } = &node.data else { unreachable!() };

        self.visit(condition)?;
        self.visit(true_body)?;
        if let Some(false_body) = false_body {
            self.visit(false_body)?;
        }
        
        Ok(())
    }

    fn visit_for_statement(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::ForStatement { variable, condition, increment, body } = &node.data else { unreachable!() };

        self.push_scope();

        if let Some(variable) = variable {
            self.visit(variable)?;
        }

        self.visit(condition)?;

        if let Some(increment) = increment {
            self.visit(increment)?;
        }

        self.visit(body)?;
        self.pop_scope();

        Ok(())
    }

    fn visit_do_while_statement(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::DoWhileStatement { body, condition } = &node.data else { unreachable!() };

        self.visit(body)?;
        self.visit(condition)?;

        Ok(())
    }

    fn visit_index_getter(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::IndexGetter { expression, index } = &node.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(index)?;

        Ok(())
    }

    fn visit_index_setter(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::IndexSetter { expression, rhs } = &node.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(rhs)?;

        Ok(())
    }

    fn visit_property_getter(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::PropertyGetter { lhs, .. } = &node.data else { unreachable!() };
        self.visit(lhs)
    }

    fn visit_property_setter(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::PropertySetter { lhs, expression } = &node.data else { unreachable!() };
        self.visit(lhs)?;
        self.visit(expression)?;
        Ok(())
    }

    fn visit_switch_statement(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_switch_case(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_lazy(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Lazy(inner) = &node.data else { unreachable!() };
        self.visit(inner)
    }

    fn visit_defer(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Defer(expression) = &node.data else { unreachable!() };
        self.visit(expression)?;
        Ok(())
    }

    fn visit_return_statement(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Return(expression) = &node.data else { unreachable!() };

        if let Some(expression) = expression {
            self.visit(expression)?
        }

        Ok(())
    }

    fn visit_body(&mut self, node: &Rc<Ast>, new_scope: bool) -> Result<(), SpruceErr> {
        let AstData::Body(inner) = &node.data else { unreachable!() };

        if new_scope {
            self.push_scope();
        }

        for item in inner {
            self.visit(item)?;
        }

        if new_scope {
            self.pop_scope();
        }

        Ok(())
    }

    fn visit_include(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_program(&mut self, node: &Rc<Ast>) -> Result<(), SpruceErr> {
        let AstData::Program { source: _, body } = &node.data else { unreachable!() };
        let mut statements = Vec::new();

        for item in body {
            statements.push(self.visit(item)?);
        }

        Ok(())
    }

    fn visit_empty(&mut self, _node: &Rc<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }
}