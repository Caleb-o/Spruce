use std::rc::Rc;

use crate::{source::Source, RunArgs, error::{SpruceErr, SpruceErrData}, nativefns};

use super::{token::{Span, Token}, symtable::SymTable, symbols::Symbols, ast::{Ast, AstData, TypeKind}, sprucetype::SpruceType};

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
pub struct ResolutionTable {
    pub symbol_values: Symbols,
    pub signatures: Vec<FunctionSignature>,
}

impl ResolutionTable {
    pub fn new() -> Self {
        Self {
            symbol_values: Symbols::new(),
            signatures: Vec::new(),
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

    pub fn run(&mut self, program: &Box<Ast>) -> Result<Box<ResolutionTable>, SpruceErr> {
        nativefns::register_native_functions_ids(self);

        self.visit(program)?;

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
    fn warning(&self, msg: String, token: &Token) {
        println!("{}", format!(
            "[\x1b[33mWarning\x1b[0m] {} '{}' [{}:{}]",
            msg,
            token.span.source.file_path,
            token.line,
            token.column,
        ));
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

            None => self.table.new_local(token.clone().span, mutable, SpruceType::Any),
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

    fn tuple_literal(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::TupleLiteral(inner) = &node.data else { unreachable!() };

        for item in inner {
            self.visit(&item)?;
        }

        Ok(())
    }

    fn list_literal(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ListLiteral(inner) = &node.data else { unreachable!() };

        for item in inner {
            self.visit(&item)?;
        }

        Ok(())
    }

    fn symbol_literal(&mut self, node: &Box<Ast>) {
        let AstData::SymbolLiteral = &node.data else { unreachable!() };
        self.res_table.symbol_values.find_or_add(&node.token.span);
    }

    #[inline]
    fn type_id(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        self.get_type_from_ast(&node)
    }

    fn identifier(&mut self, node: &Box<Ast>) {
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
    }

    fn binary_op(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::BinaryOp { lhs, rhs } = &node.data  else { unreachable!() };
        self.visit(&lhs)?;
        self.visit(&rhs)?;
        Ok(())
    }

    fn unary_op(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::UnaryOp { rhs } = &node.data  else { unreachable!() };
        self.visit(&rhs)?;
        Ok(())
    }

    fn logical_op(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::LogicalOp { lhs, rhs } = &node.data else { unreachable!() };
        self.visit(&lhs)?;
        self.visit(&rhs)?;
        Ok(())
    }

    fn var_declaration(&mut self, var_decl: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarDeclaration { is_mutable, kind: _, expression } = &var_decl.data else { unreachable!() };
        let identifier = &var_decl.token;
        
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

    fn var_declarations(&mut self, var_decls: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::VarDeclarations(decls) = &var_decls.data else { unreachable!() };
        for decl in decls {
            self.var_declaration(&decl)?;
        }
        Ok(())
    }

    fn var_assign(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
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

    fn var_assign_equal(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
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

    fn function_call(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::FunctionCall { lhs, arguments } = &node.data else { unreachable!() };

        match lhs.data {
            AstData::Identifier => {
                match self.table.find_local(&lhs.token.span, true) {
                    Some(local) => {},
                    None => {
                        match self.find_function(&lhs.token.span) {
                            Some(f) => {},
                            None => {
                                self.unresolved.push(LookAhead {
                                    token: lhs.token.clone(),
                                    args: arguments.len() as u8,
                                });
                            },
                        }
                    }
                };
            }

            _ => self.visit(lhs)?,
        }

        for arg in arguments {
            self.visit(arg)?;
        }

        Ok(())
    }

    fn ternary(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Ternary { condition, true_body, false_body } = &node.data else { unreachable!() };

        self.visit(condition)?;
        self.visit(true_body)?;
        self.visit(false_body)?;

        Ok(())
    }

    fn if_statement(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::IfStatement { is_expression, condition, true_body, false_body } = &node.data else { unreachable!() };

        self.visit(condition)?;
        self.visit(true_body)?;
        if let Some(false_body) = false_body {
            self.visit(false_body)?;
        }
        
        Ok(())
    }

    fn for_statement(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
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

    fn body(&mut self, node: &Box<Ast>, new_scope: bool) -> Result<(), SpruceErr> {
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

    fn expr_statement(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::ExpressionStatement(is_statement, inner) = &node.data else { unreachable!() };
        self.visit(inner)?;
        Ok(())
    }

    #[inline]
    fn evaluate_params(
        &mut self,
        token: Token,
        parameters: &Option<Vec<Box<Ast>>>,
    ) -> Result<Option<Vec<Box<Ast>>>, SpruceErr> {
        // Register locals from parameters
        if let Some(parameters) = parameters {
            let mut out = Vec::new();
            for param in parameters {
                let AstData::Parameter { type_name } = &param.data else { unreachable!("{:#?}", param.data) };
                self.get_type_from_ast(type_name)?;
                
                self.register_local(&param.token, false);
                // Kinda gross here, might need to use Rc instead of box
                out.push(param.clone());
            }
            return Ok(Some(out));
        }

        Ok(None)
    }

    fn function(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Function { anonymous, parameters, return_type, body } = &node.data else { unreachable!() };
        let identifier = &node.token;
        
        if let Some(return_type) = return_type {
            self.visit(return_type)?;
        }
        
        if *anonymous {
            self.push_scope();
            self.table.mark_depth_limit();
            self.evaluate_params(identifier.clone(), parameters)?;
        } else {
            let params = self.evaluate_params(identifier.clone(), parameters)?;
            
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
            AstData::Return(_) => self.return_statement(body)?,
            AstData::Body(_) => self.body(body, false)?,
            _ => unreachable!("BODY AST"),
        };
        
        self.pop_scope();
        self.table.reset_mark();
        
        Ok(())
    }

    fn return_statement(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Return(expression) = &node.data else { unreachable!() };

        if let Some(expression) = expression {
            self.visit(expression)?
        }

        Ok(())
    }

    fn program(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Program { source: _, body } = &node.data else { unreachable!() };
        let mut statements = Vec::new();

        for item in body {
            statements.push(self.visit(item)?);
        }

        Ok(())
    }

    fn visit(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        Ok(match node.data {
            AstData::TupleLiteral(_) => self.tuple_literal(node)?,
            AstData::ListLiteral(_) => self.list_literal(node)?,
            AstData::SymbolLiteral => self.symbol_literal(node),

            AstData::Type {..} => self.type_id(node)?,
            AstData::Identifier => self.identifier(node),
            AstData::BinaryOp {..} => self.binary_op(node)?,
            AstData::UnaryOp {..} => self.unary_op(node)?,
            AstData::LogicalOp {..} => self.logical_op(node)?,  

            AstData::VarDeclaration {..} => self.var_declaration(node)?,
            AstData::VarDeclarations(_) => self.var_declarations(node)?,
            AstData::VarAssign {..} => self.var_assign(node)?,
            AstData::VarAssignEqual {..} => self.var_assign_equal(node)?,
            AstData::FunctionCall {..} => self.function_call(node)?,

            AstData::Ternary {..} => self.ternary(node)?,
            AstData::IfStatement {..} => self.if_statement(node)?,
            AstData::ForStatement {..} => self.for_statement(node)?,
            AstData::Body(_) => self.body(node, true)?,
            AstData::ExpressionStatement(_, _) => self.expr_statement(node)?,

            AstData::Function {..} => self.function(node)?,
            AstData::Return(_) => self.return_statement(node)?,
            AstData::Program {..} => self.program(node)?,

            AstData::Literal | AstData::Comment | AstData::Empty => {},
            _ => return Err(self.error(format!("Unknown node in check: {:#?}", node))),
        })
    }

    fn register_function(
        &mut self,
        identifier: &Token,
        parameters: Option<Vec<Box<Ast>>>,
        return_id: Option<Box<Ast>>,
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

    fn get_type_from_ast(&mut self, node: &Box<Ast>) -> Result<(), SpruceErr> {
        let AstData::Type { kind } = &node.data else { unreachable!() };

        Ok(match &*kind {
            TypeKind::Standard => {
                match node.token.span.slice_source() {
                    "any" | "int" | "float" | "bool" | "string" | "symbol" => {},
                    // TODO: Check for custom type before error
                    _ => {
                        self.error_no_exit(format!(
                                "Unknown type identifier '{}'",
                                node.token.span.slice_source(),
                            ),
                            &node.token
                        );
                    },
                }
            }
            TypeKind::Tuple(ref inner) => {
                for item in inner {
                    self.get_type_from_ast(item)?;
                }
            },
            TypeKind::List(ref inner) => self.get_type_from_ast(inner)?,
            TypeKind::Function { parameters, return_type } => {
                match parameters {
                    Some(ref parameters) => {
                        for kind in parameters {
                            self.get_type_from_ast(kind)?;
                        }
                    }
                    None => {},
                }

                self.get_type_from_ast(&return_type)?;
            }
            _ => unimplemented!("Type from AST"),
        })
    }
}