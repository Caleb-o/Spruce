use std::{rc::Rc, fs::{self, File}, path::Path, io::Write};

use crate::{source::Source, frontend::{decorated_ast::{DecoratedAst, DecoratedAstData, FunctionType}, sprucetype::SpruceType, token::TokenKind, symbols::Symbols}, error::{SpruceErr, SpruceErrData}};

const SPRUCE_PRE: &'static str = "SprucePrelude";
const BYTES_BEFORE_WRITE: usize = 1024 * 1024;

pub struct Compiler {
    source: Rc<Source>,
    depth: u32,
    symbols: Symbols,
    file: File,
    output_code: String,
}

impl Compiler {
    pub fn new(source: Rc<Source>, symbols: Symbols) -> Self {
        if Path::new("out.cs").exists() {
            fs::remove_file("out.cs").unwrap();
        }

        Self {
            source,
            depth: 0,
            symbols,
            file: fs::OpenOptions::new().append(true).create(true).open("out.cs").unwrap(),
            output_code: String::with_capacity(BYTES_BEFORE_WRITE),
        }
    }

    pub fn run(&mut self, root: Box<DecoratedAst>) -> Result<(), SpruceErr> {
        self.boiler_plate(root)?;

        match fs::write("out.cs", &self.output_code) {
            Ok(_) => {}
            Err(_) => eprintln!("Could not write to file"),
        }

        Ok(())
    }

    #[inline]
    fn error(&mut self, message: String) -> SpruceErr {
        SpruceErr::new(message, SpruceErrData::Compiler { file_path: (*self.source.file_path).clone() })
    }
    
    #[inline]
    fn indent(&mut self) {
        self.depth += 1;
    }
    
    #[inline]
    fn dedent(&mut self) {
        self.depth -= 1;
    }

    fn tab_string(&self) -> String {
        let mut string = String::with_capacity((self.depth * 4) as usize);

        for _ in 0..string.capacity() {
            string.push(' ');
        }

        string
    }

    fn generate_symbol_enum(&mut self) {
        if self.symbols.get_table().len() == 0 {
            return;
        }

        self.output_code.push_str("enum Symbol\n{\n");
        self.indent();

        for item in self.symbols.get_table() {
            self.output_code.push_str(&format!(
                "{}{},\n",
                self.tab_string(),
                item.slice_source(),
            ));
        }
        
        self.dedent();
        self.output_code.push_str("}\n\n");
    }

    fn boiler_plate(&mut self, root: Box<DecoratedAst>) -> Result<(), SpruceErr> {
        self.output_code.push_str(&fs::read_to_string("prelude/prelude.cs").unwrap());
        self.output_code.push_str("\n\n");
        self.generate_symbol_enum();
        self.output_code.push_str("namespace Application\n{\n");
        self.output_code.push_str("sealed class Program\n{\n");
        
        self.indent();
        
        self.visit(&root)?;
        
        self.output_code.push_str(&format!(
            "{}public static void Main() {{ new Program().main(); }}\n",
            self.tab_string(),
        ));
        self.dedent();

        self.output_code.push_str("}\n}");
        Ok(())
    }

    fn literal(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        if node.token.kind == TokenKind::String {
            self.output_code.push_str(&format!("\"{}\"", node.token.span.slice_source()));
            return Ok(());
        }

        self.output_code.push_str(node.token.span.slice_source());
        Ok(())
    }

    fn tuple_literal(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::TupleLiteral(_, expressions) = &node.data else { unreachable!() };
        
        self.output_code.push('(');
        
        for (idx, item) in expressions.iter().enumerate() {
            self.visit(item)?;
            
            if idx < expressions.len() - 1 { 
                self.output_code.push_str(", ");
            }
        }
        self.output_code.push(')');

        Ok(())
    }

    fn list_literal(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ListLiteral(_, values) = &node.data else { unreachable!() };

        self.output_code.push_str("new(){ ");

        for (idx, arg) in values.iter().enumerate() {
            self.visit(arg)?;

            if idx < values.len() - 1 {
                self.output_code.push_str(", ");
            }
        }

        self.output_code.push_str(" }");

        Ok(())
    }

    fn symbol_literal(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::SymbolLiteral(value) = &node.data else { unreachable!() };
        self.output_code.push_str(&format!(
            "Symbol.{}",
            self.symbols.get_table()[*value as usize].slice_source(),
        ));
        Ok(())
    }

    fn binary_op(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::BinaryOp { kind: _, lhs, rhs } = &node.data else { unreachable!() };
        
        self.visit(lhs)?;
        self.output_code.push_str(&format!(
            " {} ",
            node.token.span.slice_source(),
        ));
        self.visit(rhs)?;
        Ok(())
    }

    fn logical_op(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::LogicalOp { kind: _, lhs, rhs } = &node.data else { unreachable!() };
        
        self.visit(lhs)?;
        self.output_code.push_str(&format!(
            " {} ",
            node.token.span.slice_source(),
        ));
        self.visit(rhs)?;
        Ok(())
    }

    fn identifier(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Identifier(kind) = &node.data else { unreachable!() };

        match kind {
            SpruceType::Function { is_native, .. } => {
                if *is_native {
                    let identifier = node.token.span.slice_source();
                    let identifier = identifier[0..1].to_uppercase() + &identifier[1..];
                    self.output_code.push_str(&format!("{SPRUCE_PRE}.{identifier}"));
                } else {
                    self.output_code.push_str(node.token.span.slice_source());
                }
            }
            SpruceType::Lazy(inner) => {
                let mut get_str = String::new();

                for _ in 0..(1 + self.get_inner_lazy(inner)) {
                    get_str.push_str(".Get()");
                }

                self.output_code.push_str(&format!(
                    "{}{}",
                    node.token.span.slice_source(),
                    get_str,
                ));
            }
            _ => self.output_code.push_str(node.token.span.slice_source()),
        }

        Ok(())
    }

    fn var_declaration(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::VarDeclaration { is_mutable: _, kind, expression } = &node.data else { unreachable!() };
        
        self.output_code.push_str(&format!(
            "{}{} {}",
            self.tab_string(),
            Compiler::as_cs_type(kind),
            node.token.span.slice_source(),
        ));
        
        match expression.data {
            DecoratedAstData::Empty => {}
            _ => {
                self.output_code.push_str(" = ");
                self.visit(expression)?;
            }
        }
        self.output_code.push_str(";\n");
        Ok(())
    }

    fn var_declarations(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::VarDeclarations(decls) = &node.data else { unreachable!() };
        
        for decl in decls {
            self.var_declaration(decl)?;
        }

        Ok(())
    }

    fn var_assign(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::VarAssign { lhs, expression } = &node.data else { unreachable!() };
        
        self.visit(lhs)?;
        self.output_code.push_str(" = ");
        self.visit(expression)?;
        
        Ok(())
    }

    fn var_assign_equal(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::VarAssignEqual { operator, lhs, expression } = &node.data else { unreachable!() };
        
        self.visit(lhs)?;
        self.output_code.push_str(&format!(
            " {} ",
            operator.span.slice_source(),
        ));
        self.visit(expression)?;
        
        Ok(())
    }
    
    fn function_call(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::FunctionCall { kind: _, lhs, arguments } = &node.data else { unreachable!() };

        self.visit(lhs)?;

        self.output_code.push('(');

        for (idx, arg) in arguments.iter().enumerate() {
            self.visit(arg)?;

            if idx < arguments.len() - 1 {
                self.output_code.push_str(", ");
            }
        }

        self.output_code.push(')');

        Ok(())
    }

    fn if_statement(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::IfStatement { is_expression, condition, kind: _, true_body, false_body } = &node.data else { unreachable!() };

        if *is_expression {
            // Convert to ternary
            self.output_code.push_str(" (");
            self.visit(condition)?;
            self.output_code.push_str(") ? ");
            self.wrap_in_lambda_call(true_body)?;
            self.output_code.push_str(" : ");
            self.wrap_in_lambda_call(false_body.as_ref().unwrap())?;
        } else {
            self.output_code.push_str(&format!(
                "{}if (",
                self.tab_string(),
            ));
            self.visit(condition)?;
            self.output_code.push_str(")\n");
            self.visit(true_body)?;

            if let Some(false_body) = false_body {
                self.output_code.pop();
                self.output_code.push_str(&format!(
                    "{}else\n",
                    self.tab_string(),
                ));
                self.visit(false_body)?;
            }
        }

        Ok(())
    }

    fn terary_expression(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Ternary { condition, kind: _, true_body, false_body } = &node.data else { unreachable!() };

        self.output_code.push('(');
        self.visit(condition)?;
        self.output_code.push_str(") ? ");
        self.wrap_in_lambda_call(true_body)?;
        self.output_code.push_str(" : ");
        self.wrap_in_lambda_call(false_body)?;

        Ok(())
    }

    fn for_statement(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ForStatement { variable, condition, increment, body } = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{}for (",
            self.tab_string(),
        ));

        if let Some(variable) = variable {
            self.visit(variable)?;
            self.output_code.pop();
        } else {
            self.output_code.push(';');
        }
        
        self.visit(condition)?;
        self.output_code.push_str("; ");
            
        if let Some(increment) = increment {
            self.visit(increment)?;
        }

        self.output_code.push_str(") ");

        self.visit(body)?;
        Ok(())
    }

    fn do_while_statement(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::DoWhileStatement { body, condition } = &node.data else { unreachable!() };

        self.output_code.push_str(&format!("{}do ", self.tab_string()));
        self.visit(body)?;

        self.output_code.push_str(" while (");
        self.visit(condition)?;
        self.output_code.push_str(");\n");
        Ok(())
    }

    fn body(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Body(_, statements) = &node.data else { unreachable!() };
        self.output_code.push_str("{\n");

        self.indent();

        for item in statements {
            self.visit(item)?;
        }

        self.dedent();

        self.output_code.push_str(&format!("{}}}", self.tab_string()));
        Ok(())
    }

    fn anonymous_function(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Function { parameters, kind, body, .. } = &node.data else { unreachable!() };
        let DecoratedAstData::ParameterList(params) = &parameters.data else { unreachable!() };

        let kind = Compiler::as_cs_type(&kind);

        if kind == "void" && (params.is_none() || params.as_ref().unwrap().len() == 0) {
           self.output_code.push_str("((Action)((");
        } else {
            self.output_code.push_str("((Func<");
            
            if let Some(params) = params {
                for item in params {
                    let DecoratedAstData::Parameter(kind) = &item.data else { unreachable!() };

                    self.output_code.push_str(&Compiler::as_cs_type(kind));
                    self.output_code.push_str(", ");
                }
            }

            self.output_code.push_str(&format!("{kind}>)(("));
        }

        if let Some(params) = params {
            for (idx, item) in params.iter().enumerate() {
                let DecoratedAstData::Parameter(_) = &item.data else { unreachable!() };

                self.output_code.push_str(item.token.span.slice_source());

                if idx < params.len() - 1 {
                    self.output_code.push_str(", ");
                }
            }
        }
        self.output_code.push_str(") => ");

        self.visit(body)?;
        self.output_code.push_str("))");

        Ok(())
    }

    fn function(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Function { function_type, parameters, kind, body } = &node.data else { unreachable!() };

        match function_type {
            FunctionType::Standard => {
                self.output_code.push_str(&format!("{}public ", self.tab_string()));
            }
            FunctionType::Inner => {
                self.output_code.push_str(&format!("{}", self.tab_string()));
            }
            FunctionType::Anonymous => {
                self.anonymous_function(node)?;
                return Ok(());
            }
        }

        self.output_code.push_str(&format!(
            "{} {}(",
            Compiler::as_cs_type(kind),
            node.token.span.slice_source(),
        ));
        
        self.visit(parameters)?;

        self.output_code.push_str(&format!(")\n{}", self.tab_string()));
        self.visit(body)?;
        self.output_code.push_str("\n\n");

        Ok(())
    }

    fn parameter_list(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ParameterList(parameters) = &node.data else { unreachable!() };

        if let Some(parameters) = parameters {
            for (idx, param) in parameters.iter().enumerate() {
                self.visit(param)?;

                if idx < parameters.len() - 1 {
                    self.output_code.push_str(", ");
                }
            }
        }

        Ok(())
    }

    fn parameter(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Parameter(kind) = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{} {}",
            Compiler::as_cs_type(kind),
            node.token.span.slice_source(),
        ));

        Ok(())
    }

    fn expression_statement(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ExpressionStatement(_, is_statement, expr) = &node.data else { unreachable!() };

        if *is_statement {
            if let DecoratedAstData::Body(_, statements) = &expr.data {
                if statements.len() > 0 {
                    self.output_code.push_str(&self.tab_string());
                    self.body(expr)?;
                    self.output_code.push_str("\n\n");
                }
                return Ok(());
            } else {
                self.output_code.push_str(&self.tab_string());
                self.visit(expr)?;
            }
        } else {
            self.output_code.push_str(&self.tab_string());
            self.output_code.push_str("return ");

            match expr.data {
                DecoratedAstData::Body(_, _) => self.wrap_in_lambda_call(expr)?,
                _ => self.visit(expr)?,
            }
        }

        self.output_code.push_str(";\n");
        Ok(())
    }

    fn lazy(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Lazy(expression) = &node.data else { unreachable!() };

        self.output_code.push_str("new(");
        self.wrap_in_lambda(expression)?;
        self.output_code.push(')');

        Ok(())
    }

    fn defer(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Defer(count, expression) = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{}using var __spruce_defer_{} = new {}.Defer(",
            self.tab_string(),
            count,
            SPRUCE_PRE,
        ));

        self.wrap_in_lambda(expression)?;

        self.output_code.push_str(");\n");

        Ok(())
    }

    fn return_statement(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Return(_, expr) = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{}return",
            self.tab_string(),
        ));

        if let Some(expr) = expr {
            self.visit(expr)?;
        }

        self.output_code.push_str(";\n");

        Ok(())
    }

    fn program(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Program { source: _, body } = &node.data else { unreachable!() };
        
        for item in body {
            self.visit(item)?;
        }

        Ok(())
    }

    fn comment(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Comment = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{}// {}\n",
            self.tab_string(),
            node.token.span.slice_source(),
        ));

        Ok(())
    }

    fn visit(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        match node.data {
            DecoratedAstData::Literal(_, _) => self.literal(node)?,
            DecoratedAstData::TupleLiteral(_, _) => self.tuple_literal(node)?,
            DecoratedAstData::ListLiteral(_, _) => self.list_literal(node)?,
            DecoratedAstData::SymbolLiteral(_) => self.symbol_literal(node)?,
            DecoratedAstData::BinaryOp {..} => self.binary_op(node)?,
            DecoratedAstData::LogicalOp {..} => self.logical_op(node)?,
            DecoratedAstData::Identifier(_) => self.identifier(node)?,

            DecoratedAstData::VarDeclaration {..} => self.var_declaration(node)?,
            DecoratedAstData::VarDeclarations(_) => self.var_declarations(node)?,
            DecoratedAstData::VarAssign {..} => self.var_assign(node)?,
            DecoratedAstData::VarAssignEqual {..} => self.var_assign_equal(node)?,
            DecoratedAstData::FunctionCall {..} => self.function_call(node)?,

            DecoratedAstData::IfStatement {..} => self.if_statement(node)?,
            DecoratedAstData::Ternary {..} => self.terary_expression(node)?,
            DecoratedAstData::ForStatement {..} => self.for_statement(node)?,
            DecoratedAstData::DoWhileStatement {..} => self.do_while_statement(node)?,
            DecoratedAstData::Body(_, _) => self.body(node)?,
            DecoratedAstData::Function {..} => self.function(node)?,
            DecoratedAstData::ParameterList(_) => self.parameter_list(node)?,
            DecoratedAstData::Parameter(_) => self.parameter(node)?,
            DecoratedAstData::ExpressionStatement(_, _, _) => self.expression_statement(node)?,

            DecoratedAstData::Lazy(_) => self.lazy(node)?,
            DecoratedAstData::Defer(_, _) => self.defer(node)?,
            DecoratedAstData::Return(_, _) => self.return_statement(node)?,
            DecoratedAstData::Program {..} => self.program(node)?,
            DecoratedAstData::Comment => self.comment(node)?,
            DecoratedAstData::Empty => {}

            _ => return Err(self.error(format!(
                "Unknown node in compilation '{:#?}'",
                node,
            ))),
        }

        if self.output_code.len() >= BYTES_BEFORE_WRITE {
            write!(self.file, "{}", &self.output_code).unwrap();
            self.output_code.clear();
        }

        Ok(())
    }

    fn get_inner_lazy(&self, kind: &Box<SpruceType>) -> u32 {
        match &**kind {
            SpruceType::Lazy(inner) => 1 + self.get_inner_lazy(inner),
            _ => 0,
        }
    }

    fn get_type_from_ast(&self, node: &Box<DecoratedAst>) -> Result<String, SpruceErr> {
        let kind = Compiler::as_cs_type(match &node.data {
            DecoratedAstData::BinaryOp { kind, .. } => kind,
            DecoratedAstData::UnaryOp { kind, .. } => kind,
            DecoratedAstData::LogicalOp { kind, .. } => kind,
            DecoratedAstData::Body(kind, _) => kind,
            DecoratedAstData::Literal(kind, _) => kind,
            DecoratedAstData::ListLiteral(kind, _) => kind,
            DecoratedAstData::IfStatement { kind, .. } => kind,
            DecoratedAstData::Ternary { kind, .. } => kind,
            DecoratedAstData::FunctionCall { kind, .. } => kind,
            _ => return Err(SpruceErr::new(format!(
                    "Cannot cast node '{:#?}' to type",
                    node.data,
                ),
                SpruceErrData::Compiler { file_path: self.source.file_path.to_string() },
            )),
        });

        Ok(kind)
    }

    #[inline]
    fn lambda_prefix(kind: &String) -> String {
        if kind == "void" {
            "((Action)(() => ".into()
        } else {
            format!("((Func<{kind}>)(() => ")
        }
    }

    fn wrap_in_lambda(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let kind = self.get_type_from_ast(node)?;
        self.output_code.push_str(&Compiler::lambda_prefix(&kind));

        self.visit(node)?;
        self.output_code.push_str("))");
        Ok(())
    }

    #[inline]
    fn wrap_in_lambda_call(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        self.wrap_in_lambda(node)?;
        self.output_code.push_str("()");
        Ok(())
    }

    fn as_cs_type(kind: &SpruceType) -> String {
        match kind {
            SpruceType::None => "void".into(),
            SpruceType::Bool => "bool".into(),
            SpruceType::Int => "int".into(),
            SpruceType::String => "string".into(),
            SpruceType::Symbol => "Symbol".into(),
            SpruceType::Tuple(kinds) => {
                let mut string = String::from("(");

                for (idx, kind) in kinds.iter().enumerate() {
                    string.push_str(&Compiler::as_cs_type(kind));

                    if idx < kinds.len() - 1 {
                        string.push_str(", ");
                    }
                }

                string.push(')');
                string
            }
            SpruceType::List(inner) => {
                format!("List<{}>", Compiler::as_cs_type(inner))
            }
            SpruceType::Lazy(inner) => {
                format!(
                    "{}.Lazy<{}>",
                    SPRUCE_PRE,
                    Compiler::as_cs_type(inner)
                )
            }
            SpruceType::Function { is_native: _, parameters, return_type } => {
                let mut string = String::from("Func<");

                if let Some(parameters) = parameters {
                    for (idx, param) in parameters.iter().enumerate() {
                        string.push_str(&Compiler::as_cs_type(&param));
                        
                        if idx < parameters.len() - 1 {
                            string.push_str(", ");
                        }
                    }
                    
                    string.push_str(", ");
                }
                string.push_str(&Compiler::as_cs_type(&return_type));
                string.push('>');

                string
            }
            _ => unimplemented!(),
        }
    }
}