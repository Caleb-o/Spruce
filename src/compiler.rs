use std::{rc::Rc, fs};

use crate::{source::Source, frontend::{decorated_ast::{DecoratedAst, DecoratedAstData, FunctionType}, sprucetype::SpruceType, token::TokenKind, symbols::Symbols}, error::{SpruceErr, SpruceErrData}};

pub struct Compiler {
    source: Rc<Source>,
    depth: u32,
    symbols: Symbols,
    output_code: String,
}

impl Compiler {
    pub fn new(source: Rc<Source>, symbols: Symbols) -> Self {
        Self {
            source,
            depth: 0,
            symbols,
            output_code: String::new(),
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
        self.output_code.push_str("using System;\n");
        self.output_code.push_str("using System.Collections.Generic;\n\n");
        self.output_code.push_str("namespace Application;\n\n");
        self.generate_symbol_enum();
        self.output_code.push_str("sealed class Program\n{\n");
        
        self.indent();
        
        self.visit(&root)?;
        
        self.output_code.push_str(&format!(
            "{}public static void Main() {{ new Program().main(); }}\n",
            self.tab_string(),
        ));
        self.dedent();

        self.output_code.push_str("}\n");
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

    fn identifier(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        self.output_code.push_str(node.token.span.slice_source());
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

    fn body(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Body(_, statements) = &node.data else { unreachable!() };
        self.output_code.push_str(&format!("{}{{\n", self.tab_string()));

        self.indent();

        for item in statements {
            self.visit(item)?;
        }

        self.dedent();

        self.output_code.push_str(&format!("{}}}\n\n", self.tab_string()));
        Ok(())
    }

    fn function(&mut self, node: &Box<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Function { function_type, parameters, kind, body } = &node.data else { unreachable!() };

        match function_type {
            FunctionType::Standard => {
                self.output_code.push_str(&format!("{}public ", self.tab_string()));
            }
            FunctionType::Anonymous | FunctionType::Inner => {
                self.output_code.push_str(&format!("{}", self.tab_string()));
            }
        }

        self.output_code.push_str(&format!(
            "{} {}(",
            Compiler::as_cs_type(kind),
            node.token.span.slice_source(),
        ));
        
        self.visit(parameters)?;

        self.output_code.push_str(")\n");
        self.visit(body)?;

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

        self.output_code.push_str(&self.tab_string());
        if *is_statement {
            self.visit(expr)?;
        } else {
            self.output_code.push_str("return ");
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
            DecoratedAstData::Identifier(_) => self.identifier(node)?,

            DecoratedAstData::VarDeclaration {..} => self.var_declaration(node)?,
            DecoratedAstData::VarDeclarations(_) => self.var_declarations(node)?,
            DecoratedAstData::VarAssign {..} => self.var_assign(node)?,
            DecoratedAstData::FunctionCall {..} => self.function_call(node)?,

            DecoratedAstData::Body(_, _) => self.body(node)?,
            DecoratedAstData::Function {..} => self.function(node)?,
            DecoratedAstData::ParameterList(_) => self.parameter_list(node)?,
            DecoratedAstData::Parameter(_) => self.parameter(node)?,
            DecoratedAstData::ExpressionStatement(_, _, _) => self.expression_statement(node)?,

            DecoratedAstData::Program {..} => self.program(node)?,
            DecoratedAstData::Comment => self.comment(node)?,
            DecoratedAstData::Empty => {}

            _ => return Err(self.error(format!(
                "Unknown node in compilation '{:#?}'",
                node,
            ))),
        }

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
            SpruceType::Function { parameters, return_type } => {
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