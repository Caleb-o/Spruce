use std::{collections::HashSet, rc::Rc};

use spruce_front::{
    ast::{Ast, AstData},
    source::Source,
    token::{Span, Token},
    types::TypeKind,
};
use spruce_shared::{
    error::{SpruceErr, SpruceErrData},
    visitor::Visitor,
};

use crate::{sprucetype::SpruceType, symbols::Symbols, symtable::SymTable};

#[derive(Debug, Clone)]
pub enum FunctionSignatureKind {
    User {
        parameters: Option<Vec<Box<Ast>>>,
        return_id: Option<Box<Ast>>,
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
    pub fields: Option<Vec<Box<Ast>>>,
    pub functions: Option<Vec<FunctionSignature>>,
}

#[derive(Debug, Clone)]
pub struct ResolutionTable<'a> {
    pub symbol_values: Symbols<'a>,
    pub signatures: Vec<FunctionSignature>,
    pub struct_types: Vec<StructSignature>,
}

impl<'a> ResolutionTable<'a> {
    pub fn new(source: &Source) -> Self {
        Self {
            symbol_values: Symbols::new(source),
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

pub struct NameResolver<'a> {
    had_error: bool,
    source: &'a Source,
    table: SymTable,
    unresolved: Vec<LookAhead>,
}

impl<'a> NameResolver<'a> {
    pub fn new(source: &Source) -> Self {
        Self {
            had_error: false,
            source,
            table: SymTable::new(),
            unresolved: Vec::new(),
        }
    }

    pub fn run(&mut self, program: &Box<Ast>) -> Result<Box<ResolutionTable>, SpruceErr> {
        // nativefns::register_native_functions_ids(self);

        let mut res_table = Box::new(ResolutionTable::new(self.source));

        self.visit_program(&(&mut res_table, program))?;

        // Try to resolve calls that were not during compilation
        self.resolve_function_calls();

        if self.had_error {
            return Err(self.error("Error(s) occured".into()));
        }

        Ok(res_table)
    }

    #[inline]
    pub fn add_native_fn(res_table: &mut Box<ResolutionTable>, identifier: &'static str) {
        Self::add_function(
            res_table,
            identifier.to_string(),
            FunctionSignatureKind::Native,
        );
    }

    #[inline]
    fn error(&mut self, message: String) -> SpruceErr {
        SpruceErr::new(
            message,
            SpruceErrData::Analyser {
                file_path: (*self.source.file_path).clone(),
            },
        )
    }

    fn error_no_exit(&mut self, msg: String, token: &Token) {
        self.had_error = true;

        println!(
            "{}",
            format!(
                "[\x1b[31mError\x1b[0m] {} - '{}' [{}:{}]",
                msg, self.source.file_path, token.line, token.column,
            )
        );
    }

    #[inline]
    fn add_function(
        res_table: &mut Box<ResolutionTable>,
        identifier: String,
        kind: FunctionSignatureKind,
    ) {
        res_table
            .signatures
            .push(FunctionSignature { identifier, kind });
    }

    #[inline]
    fn push_scope(&mut self) {
        self.table.new_scope();
    }

    #[inline]
    fn pop_scope(&mut self) {
        self.table.close_scope();
    }

    fn find_function_str(&self, res_table: &Box<ResolutionTable>, id: &str) -> Option<()> {
        for func in &res_table.signatures {
            if func.identifier.as_str() == id {
                return Some(());
            }
        }

        None
    }

    #[inline]
    fn find_function(&self, res_table: &Box<ResolutionTable>, span: &Span) -> Option<()> {
        self.find_function_str(res_table, self.source.slice_from(*span).unwrap())
    }

    fn find_struct_str(
        &self,
        res_table: &Box<ResolutionTable>,
        id: &str,
    ) -> Option<StructSignature> {
        for struct_ in &res_table.struct_types {
            if struct_.identifier.as_str() == id {
                return Some(struct_.clone());
            }
        }

        None
    }

    #[inline]
    fn find_struct(
        &self,
        res_table: &Box<ResolutionTable>,
        span: &Span,
    ) -> Option<StructSignature> {
        self.find_struct_str(res_table, self.source.slice_from(*span).unwrap())
    }

    fn find_struct_field(&self, signature: &StructSignature, field_name: &Span) -> bool {
        if let Some(fields) = &signature.fields {
            for field in fields {
                if field.token.lexeme.unwrap().compare(self.source, field_name) {
                    return true;
                }
            }
        }

        false
    }

    fn register_local(&mut self, token: &Token, mutable: bool) {
        let local = self
            .table
            .find_local(self.source, &token.lexeme.unwrap(), false);

        match local {
            Some(local) => {
                // We aren't allowed to overwrite, it is an error
                self.error_no_exit(
                    format!(
                        "Local with identifier '{}' already exists in scope",
                        self.source.slice_from(token.lexeme.unwrap()).unwrap(),
                    ),
                    token,
                );
            }

            None => self
                .table
                .new_local(token.lexeme.unwrap(), mutable, Box::new(SpruceType::Any)),
        }
    }

    fn resolve_function_calls(&mut self, res_table: &Box<ResolutionTable>) {
        let mut unresolved = Vec::new();

        for lookahead in &self.unresolved {
            match self.find_function(res_table, &lookahead.token.lexeme.unwrap()) {
                Some(_) => {}
                None => unresolved.push((lookahead.token, lookahead.args)),
            }
        }

        // Identifiers that were still not found
        for (token, _args) in unresolved {
            self.error_no_exit(
                format!(
                    "Function '{}' does not exist",
                    self.source.slice_from(token.lexeme.unwrap()).unwrap()
                ),
                &token,
            );
        }
    }

    #[inline]
    fn evaluate_params(
        &mut self,
        res_table: &mut Box<ResolutionTable>,
        function_name: &Token,
        parameters: &Option<Vec<Box<Ast>>>,
    ) -> Result<Option<Vec<Box<Ast>>>, SpruceErr> {
        // Register locals from parameters
        if let Some(parameters) = parameters {
            let mut out = Vec::new();
            let mut param_names = HashSet::new();

            for param in parameters {
                let AstData::Parameter { type_signature } = &param.data else { unreachable!("{:#?}", param.data) };
                self.get_type_from_ast(&(res_table, type_signature))?;

                if !param_names.insert(param.token.span.slice_souBoxe()) {
                    self.error_no_exit(
                        format!(
                            "Function with identifier '{}' already contains a parameter '{}'",
                            function_name.span.slice_souBoxe(),
                            param.token.span.slice_souBoxe(),
                        ),
                        function_name,
                    );
                }

                self.register_local(&param.token, false);
                // Kinda gross here, might need to use Box instead of box
                out.push(type_signature.clone());
            }
            return Ok(Some(out));
        }

        Ok(None)
    }

    fn register_function(
        &mut self,
        identifier: &Token,
        parameters: Option<Vec<Box<Ast>>>,
        return_id: Option<Box<Ast>>,
    ) -> Result<(), SpruceErr> {
        // Function already exists
        if self.find_function(&identifier.span).is_some() {
            self.error_no_exit(
                format!(
                    "Function with identifier '{}' already exists",
                    identifier.span.slice_souBoxe()
                ),
                &identifier,
            );
            return Ok(());
        }

        self.register_local(&identifier, false);

        self.add_function(
            identifier.span.slice_souBoxe().to_string(),
            FunctionSignatureKind::User {
                parameters,
                return_id,
            },
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

    fn get_type_from_ast(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::Type { kind } = &node.data else { unreachable!() };
        let span = node.token.lexeme.unwrap();
        let string = self.source.slice_from(span).unwrap();

        Ok(match &*kind {
            TypeKind::Standard => match string {
                "any" | "int" | "float" | "bool" | "string" | "symbol" => {}
                _ => {
                    if let Some(_) = self.find_struct(&span) {
                        return Ok(());
                    }

                    self.error_no_exit(format!("Unknown type identifier '{string}'"), &node.token);
                }
            },
            TypeKind::Tuple(inner) => {
                for item in inner {
                    self.get_type_from_ast(item)?;
                }
            }
            TypeKind::Array(inner) => self.get_type_from_ast(inner)?,
            TypeKind::Lazy(inner) => self.get_type_from_ast(inner)?,
            TypeKind::ErrorOrValue(lhs, rhs) => {
                self.get_type_from_ast(lhs)?;
                self.get_type_from_ast(rhs)?;
            }
            TypeKind::Function {
                parameters,
                return_type,
            } => {
                match parameters {
                    Some(parameters) => {
                        for kind in parameters {
                            self.get_type_from_ast(kind)?;
                        }
                    }
                    None => {}
                }

                self.get_type_from_ast(&return_type)?;
            }
        })
    }
}

type NType<'a> = (&'a mut Box<ResolutionTable<'a>>, &'a Box<Ast>);

impl<'a> Visitor<NType<'a>, ()> for NameResolver<'a> {
    fn visit(&mut self, item: &NType) -> Result<(), SpruceErr> {
        Ok(match &item.1.data {
            AstData::TupleLiteral(_) => self.visit_tuple_literal(item)?,
            AstData::ArrayLiteral(_) => self.visit_array_literal(item)?,
            AstData::SymbolLiteral => self.visit_symbol_literal(item)?,
            AstData::StructLiteral(_, _) => self.visit_struct_literal(item)?,
            AstData::ErrorOrValue { .. } => self.visit_error_or_value(item)?,

            AstData::Type { .. } => self.visit_type(item)?,
            AstData::Identifier => self.visit_identifier(item)?,
            AstData::BinaryOp { .. } => self.visit_binary_op(item)?,
            AstData::UnaryOp { .. } => self.visit_unary_op(item)?,
            AstData::LogicalOp { .. } => self.visit_logical_op(item)?,

            AstData::VarDeclaration { .. } => self.visit_var_declaration(item)?,
            AstData::VarDeclarations(_) => self.visit_var_declarations(item)?,
            AstData::VarAssign { .. } => self.visit_var_assign(item)?,
            AstData::VarAssignEqual { .. } => self.visit_var_assign_equal(item)?,

            AstData::FunctionCall { .. } => self.visit_function_call(item)?,
            AstData::Payload(_) => self.visit_payload(item)?,
            AstData::IfStatement { .. } => self.visit_if_statement(item)?,
            AstData::ForStatement { .. } => self.visit_for_statement(item)?,
            AstData::DoWhileStatement { .. } => self.visit_do_while_statement(item)?,
            AstData::Body(_) => self.visit_body(node, true)?,
            AstData::ExpressionStatement(_, _) => self.visit_expression_statement(item)?,

            AstData::Function { .. } => self.visit_function(item)?,
            AstData::StructDefinition { .. } => self.visit_struct_def(item)?,

            AstData::IndexGetter { .. } => self.visit_index_getter(item)?,
            AstData::IndexSetter { .. } => self.visit_index_setter(item)?,

            AstData::PropertyGetter { .. } => self.visit_property_getter(item)?,
            AstData::PropertySetter { .. } => self.visit_property_setter(item)?,

            AstData::Raw { .. } => self.visit_raw(item)?,
            AstData::Defer { .. } => self.visit_defer(item)?,
            AstData::Lazy { .. } => self.visit_lazy(item)?,
            AstData::Return { .. } => self.visit_return_statement(item)?,
            AstData::Program { .. } => self.visit_program(item)?,

            AstData::Literal | AstData::Comment | AstData::Empty => {}
            _ => return Err(self.error(format!("Unknown node in resolver: {:#?}", node))),
        })
    }

    fn visit_identifier(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let identifier = &node.token;
        let span = identifier.lexeme.unwrap();

        match self.table.find_local(self.source, &span, true) {
            Some(_) => {}
            None => match self.find_function(&span) {
                Some(_) => {}
                None => self.error_no_exit(
                    format!(
                        "Identifier '{}' does not exist in the current context",
                        self.source.slice_from(span).unwrap(),
                    ),
                    &identifier.clone(),
                ),
            },
        }

        Ok(())
    }

    fn visit_literal(&mut self, _item: &NType) -> Result<(), SpruceErr> {
        unreachable!()
    }

    #[inline]
    fn visit_symbol_literal(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::SymbolLiteral = &item.1.data else { unreachable!() };
        _ = item
            .0
            .symbol_values
            .find_or_add(&item.1.token.lexeme.unwrap());
        Ok(())
    }

    fn visit_struct_literal(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::StructLiteral(identifier, arguments) = &node.data else { unreachable!() };
        let mut fields = HashSet::new();

        let signature = if let Some(signature) = self.find_struct(&identifier.span) {
            Some(signature)
        } else {
            self.error_no_exit(
                format!(
                    "Cannot find struct with identifier '{}'",
                    identifier.span.slice_souBoxe(),
                ),
                identifier,
            );
            None
        };

        for (argument_identifier, arg) in arguments {
            if !fields.insert(argument_identifier.span.slice_souBoxe()) {
                self.error_no_exit(
                    format!(
                        "Field '{}' has already been defined in struct literal",
                        argument_identifier.span.slice_souBoxe(),
                    ),
                    argument_identifier,
                );
            }

            if let Some(signature) = &signature {
                if !self.find_struct_field(signature, &argument_identifier.span) {
                    self.error_no_exit(
                        format!(
                            "Struct '{}' does not contain a field named '{}'",
                            signature.identifier,
                            argument_identifier.span.slice_souBoxe(),
                        ),
                        argument_identifier,
                    );
                }

                if arg.is_none() {
                    if let None = self.table.find_local(&argument_identifier.span, true) {
                        self.error_no_exit(
                            format!(
                                "Cannot find local '{}' to insert as field in struct '{}'",
                                argument_identifier.span.slice_souBoxe(),
                                signature.identifier,
                            ),
                            argument_identifier,
                        );
                    }
                }
            }

            if let Some(arg) = arg {
                self.visit(arg)?;
            }
        }

        Ok(())
    }

    fn visit_tuple_literal(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::TupleLiteral(inner) = &node.data else { unreachable!() };

        for item in inner {
            self.visit(&item)?;
        }

        Ok(())
    }

    fn visit_array_literal(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::ArrayLiteral(inner) = &node.data else { unreachable!() };

        for item in inner {
            self.visit(&item)?;
        }

        Ok(())
    }

    fn visit_error_or_value(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::ErrorOrValue { expression, .. } = &node.data else { unreachable!() };
        self.visit(expression)
    }

    fn visit_expression_statement(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::ExpressionStatement(_, inner) = &node.data else { unreachable!() };
        self.visit(inner)
    }

    fn visit_comment(&mut self, _node: &Box<Ast>) -> Result<(), SpruceErr> {
        unreachable!()
    }

    fn visit_raw(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::Raw { returns, .. } = &node.data else { unreachable!() };

        if let Some(returns) = returns {
            self.visit_type(returns)?;
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::BinaryOp { lhs, rhs } = &node.data  else { unreachable!() };
        self.visit(&lhs)?;
        self.visit(&rhs)?;
        Ok(())
    }

    fn visit_unary_op(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::UnaryOp { rhs } = &node.data  else { unreachable!() };
        self.visit(&rhs)?;
        Ok(())
    }

    fn visit_logical_op(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::LogicalOp { lhs, rhs } = &node.data else { unreachable!() };
        self.visit(&lhs)?;
        self.visit(&rhs)?;
        Ok(())
    }

    fn visit_parameter(&mut self, _node: &Box<Ast>) -> Result<(), SpruceErr> {
        unreachable!()
    }

    fn visit_parameter_list(&mut self, _node: &Box<Ast>) -> Result<(), SpruceErr> {
        unreachable!()
    }

    fn visit_function(&mut self, item: &NType) -> Result<(), SpruceErr> {
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
                self.register_function(
                    identifier,
                    params,
                    match return_type {
                        Some(ret) => Some(ret.clone()),
                        None => None,
                    },
                )?;
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

    fn visit_function_call(&mut self, item: &NType) -> Result<(), SpruceErr> {
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

    fn visit_var_declaration(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::VarDeclaration { is_mutable, kind: _, expression } = &node.data else { unreachable!() };
        let identifier = &node.token;

        if !is_mutable && expression.is_none() {
            self.error_no_exit(
                format!(
                    "Immutable variable '{}' must be bound to a value",
                    identifier.span.slice_souBoxe(),
                ),
                identifier,
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
                        identifier.span.slice_souBoxe()
                    ),
                    identifier
                );
            }

            // Check global
            if self.args.no_global {
                self.error_no_exit(
                    format!(
                        "Cannot declare variable '{}' in global scope, when the 'no-global' flag is set",
                        identifier.span.slice_souBoxe()
                    ),
                    identifier
                );
            }
        }

        Ok(())
    }

    fn visit_var_declarations(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::VarDeclarations(decls) = &node.data else { unreachable!() };
        for decl in decls {
            self.visit_var_declaration(&decl)?;
        }
        Ok(())
    }

    fn visit_var_assign(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::VarAssign { lhs, expression } = &node.data else { unreachable!() };
        let identifier = &node.token;

        self.visit(&lhs)?;
        self.visit(&expression)?;

        match self.table.find_local(&identifier.span, true) {
            Some(_) => {},
            None => self.error_no_exit(format!(
                    "Cannot assign to variable '{}' as it does not exist or is not in the correct context",
                    identifier.span.slice_souBoxe(),
                ),
                &identifier
            ),
        }

        Ok(())
    }

    fn visit_var_assign_equal(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::VarAssignEqual { operator: _, lhs, expression } = &node.data else { unreachable!() };

        self.visit(&lhs)?;
        self.visit(&expression)?;

        match self.table.find_local(&node.token.span, true) {
            Some(_) => {}
            None => self.error_no_exit(format!(
                    "Cannot assign to variable '{}' as it does not exist or is not in the correct context",
                    node.token.span.slice_souBoxe(),
                ),
                &node.token
            ),
        }

        Ok(())
    }

    #[inline]
    fn visit_type(&mut self, item: &NType) -> Result<(), SpruceErr> {
        self.get_type_from_ast(&node)
    }

    fn visit_struct_def(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::StructDefinition { is_ref, items } = &node.data else { unreachable!() };

        self.push_scope();

        let signature = StructSignature {
            identifier: node.token.span.slice_souBoxe().to_string(),
            is_ref: *is_ref,
            fields: if let Some(items) = items {
                let mut fields = Vec::new();
                let mut field_names = HashSet::new();

                for item in items {
                    if let AstData::StructField {
                        type_signature,
                        default_value,
                    } = &item.data
                    {
                        self.visit_type(type_signature)?;

                        if let Some(default_value) = default_value {
                            self.visit(default_value)?;
                        }

                        if !field_names.insert(item.token.span.slice_souBoxe()) {
                            self.error_no_exit(
                                format!(
                                    "Struct definition of '{}' already contains the field '{}'",
                                    node.token.span.slice_souBoxe(),
                                    item.token.span.slice_souBoxe(),
                                ),
                                &item.token,
                            );
                        } else {
                            fields.push(Box::clone(item));
                            self.register_local(&item.token, true);
                        }
                    }
                }

                Some(fields)
            } else {
                None
            },
            functions: if let Some(items) = items {
                let mut functions = Vec::new();

                for item in items {
                    if let AstData::Function {
                        parameters,
                        return_type,
                        body,
                        ..
                    } = &item.data
                    {
                        let parameters = self.evaluate_params(&item.token, parameters)?;

                        if let Some(return_type) = return_type {
                            self.visit(return_type)?;
                        }

                        self.visit(body)?;

                        functions.push(FunctionSignature {
                            identifier: item.token.span.slice_souBoxe().to_string(),
                            kind: FunctionSignatureKind::User {
                                parameters,
                                return_id: match return_type {
                                    Some(ret) => Some(ret.clone()),
                                    None => None,
                                },
                            },
                        });
                    }
                }

                Some(functions)
            } else {
                None
            },
        };

        self.pop_scope();
        self.add_struct_type(signature);

        Ok(())
    }

    fn visit_struct_field(&mut self, _node: &Box<Ast>) -> Result<(), SpruceErr> {
        unreachable!();
    }

    fn visit_payload(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::Payload(expression) = &node.data else { unreachable!() };
        self.visit(expression)
    }

    fn visit_if_statement(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::IfStatement { condition, true_body, false_body, .. } = &node.data else { unreachable!() };

        self.visit(condition)?;
        self.visit(true_body)?;
        if let Some(false_body) = false_body {
            self.visit(false_body)?;
        }

        Ok(())
    }

    fn visit_for_statement(&mut self, item: &NType) -> Result<(), SpruceErr> {
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

    fn visit_do_while_statement(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::DoWhileStatement { body, condition } = &node.data else { unreachable!() };

        self.visit(body)?;
        self.visit(condition)?;

        Ok(())
    }

    fn visit_index_getter(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::IndexGetter { expression, index } = &node.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(index)?;

        Ok(())
    }

    fn visit_index_setter(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::IndexSetter { expression, rhs } = &node.data else { unreachable!() };

        self.visit(expression)?;
        self.visit(rhs)?;

        Ok(())
    }

    fn visit_property_getter(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::PropertyGetter { lhs, .. } = &node.data else { unreachable!() };
        self.visit(lhs)
    }

    fn visit_property_setter(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::PropertySetter { lhs, expression } = &node.data else { unreachable!() };
        self.visit(lhs)?;
        self.visit(expression)?;
        Ok(())
    }

    fn visit_switch_statement(&mut self, _node: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_switch_case(&mut self, _node: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_lazy(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::Lazy(inner) = &node.data else { unreachable!() };
        self.visit(inner)
    }

    fn visit_defer(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::Defer(expression) = &node.data else { unreachable!() };
        self.visit(expression)?;
        Ok(())
    }

    fn visit_return_statement(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::Return(expression) = &node.data else { unreachable!() };

        if let Some(expression) = expression {
            self.visit(expression)?
        }

        Ok(())
    }

    fn visit_body(&mut self, item: &NType, new_scope: bool) -> Result<(), SpruceErr> {
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

    fn visit_include(&mut self, _item: &NType) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_program(&mut self, item: &NType) -> Result<(), SpruceErr> {
        let AstData::Program { source: _, body } = &node.data else { unreachable!() };
        let mut statements = Vec::new();

        for item in body {
            statements.push(self.visit(item)?);
        }

        Ok(())
    }

    fn visit_empty(&mut self, _node: &Box<Ast>) -> Result<(), SpruceErr> {
        todo!()
    }
}
