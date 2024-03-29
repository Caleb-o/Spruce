use std::{
    fs::{self, File},
    io::Write,
    path::Path,
    rc::Rc,
};

use crate::{
    error::{SpruceErr, SpruceErrData},
    frontend::{
        decorated_ast::{DecoratedAst, DecoratedAstData, FunctionType},
        sprucetype::SpruceType,
        symbols::Symbols,
        token::TokenKind,
    },
    source::Source,
    visitor::Visitor,
};

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
            file: fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open("out.cs")
                .unwrap(),
            output_code: String::with_capacity(BYTES_BEFORE_WRITE),
        }
    }

    pub fn run(&mut self, root: Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        self.boiler_plate(root)?;

        match fs::write("out.cs", &self.output_code) {
            Ok(_) => {}
            Err(_) => eprintln!("Could not write to file"),
        }

        Ok(())
    }

    #[inline]
    fn error(&mut self, message: String) -> SpruceErr {
        SpruceErr::new(
            message,
            SpruceErrData::Compiler {
                file_path: (*self.source.file_path).clone(),
            },
        )
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
            self.output_code
                .push_str(&format!("{}{},\n", self.tab_string(), item.slice_source(),));
        }

        self.dedent();
        self.output_code.push_str("}\n\n");
    }

    fn boiler_plate(&mut self, root: Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        self.output_code
            .push_str(&fs::read_to_string("prelude/prelude.cs").unwrap());
        self.output_code.push_str("\n\n");
        self.generate_symbol_enum();
        self.output_code.push_str("sealed class Program\n{\n");

        self.indent();
        self.visit(&root)?;

        self.output_code.push_str(&format!(
            "{}public static void Main() {{ try {{ new Program().main(); }} catch (Exception ex) {{ Console.WriteLine($\"Error Occured: {{ex}}\"); }} }}\n",
            self.tab_string(),
        ));
        self.dedent();
        self.output_code.push_str("}\n");

        Ok(())
    }

    fn anonymous_function(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
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

    fn get_inner_lazy(&self, kind: &Rc<SpruceType>) -> u32 {
        match &**kind {
            SpruceType::Lazy(inner) => 1 + self.get_inner_lazy(inner),
            _ => 0,
        }
    }

    fn unwrap_error_as_ok(kind: &Rc<SpruceType>) -> String {
        match &**kind {
            SpruceType::ErrorOrValue(_, value) => Compiler::as_cs_type(value),
            _ => unreachable!(),
        }
    }

    fn unwrap_error_as_types(kind: &Rc<SpruceType>) -> (String, String) {
        match &**kind {
            SpruceType::ErrorOrValue(lhs, rhs) => {
                (Compiler::as_cs_type(lhs), Compiler::as_cs_type(rhs))
            }
            _ => unreachable!(),
        }
    }

    fn get_type_from_ast(&self, node: &Rc<DecoratedAst>) -> Result<String, SpruceErr> {
        let kind = Compiler::as_cs_type(match &node.data {
            DecoratedAstData::Identifier(kind) => kind,
            DecoratedAstData::BinaryOp { kind, .. } => kind,
            DecoratedAstData::UnaryOp { kind, .. } => kind,
            DecoratedAstData::LogicalOp { kind, .. } => kind,
            DecoratedAstData::Body(kind, _) => kind,
            DecoratedAstData::Literal(kind) => kind,
            DecoratedAstData::ArrayLiteral(kind, _) => kind,
            DecoratedAstData::IfStatement { kind, .. } => kind,
            DecoratedAstData::FunctionCall { kind, .. } => kind,
            DecoratedAstData::StructField { kind, .. } => kind,
            _ => {
                return Err(SpruceErr::new(
                    format!("Cannot cast node '{:#?}' to type", node.data,),
                    SpruceErrData::Compiler {
                        file_path: self.source.file_path.to_string(),
                    },
                ))
            }
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

    fn wrap_in_lambda(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let kind = self.get_type_from_ast(node)?;
        self.output_code.push_str(&Compiler::lambda_prefix(&kind));

        self.visit(node)?;
        self.output_code.push_str("))");
        Ok(())
    }

    fn wrap_in_lambda_call(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        match &node.data {
            DecoratedAstData::Body(_, _) => {
                self.wrap_in_lambda(node)?;
                self.output_code.push_str("()");
            }
            _ => self.visit(node)?,
        }
        Ok(())
    }

    fn wrap_in_body(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        self.output_code.push_str("{\n");
        self.indent();

        self.visit(node)?;

        self.dedent();
        self.output_code
            .push_str(&format!("{}}}", self.tab_string()));
        Ok(())
    }

    fn as_cs_type(kind: &SpruceType) -> String {
        match kind {
            SpruceType::None => "void".into(),
            SpruceType::Bool => "bool".into(),
            SpruceType::Int => "int".into(),
            SpruceType::Float => "double".into(),
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
            SpruceType::Array(inner) => {
                format!("{}.Array<{}>", SPRUCE_PRE, Compiler::as_cs_type(inner))
            }
            SpruceType::Lazy(inner) => {
                format!("{}.Lazy<{}>", SPRUCE_PRE, Compiler::as_cs_type(inner))
            }
            SpruceType::Function {
                parameters,
                return_type,
                ..
            } => {
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
            SpruceType::Struct { identifier, .. } => identifier.as_ref().unwrap().clone(),
            SpruceType::ErrorOrValue(lhs, rhs) => {
                format!(
                    "{SPRUCE_PRE}.ErrorOrValue<{}, {}>",
                    Compiler::as_cs_type(lhs),
                    Compiler::as_cs_type(rhs),
                )
            }
            n @ _ => unimplemented!("Type: {n}"),
        }
    }
}

impl Visitor<DecoratedAst, ()> for Compiler {
    fn visit(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        match node.data {
            DecoratedAstData::Literal(_) => self.visit_literal(node)?,
            DecoratedAstData::TupleLiteral(_, _) => self.visit_tuple_literal(node)?,
            DecoratedAstData::ArrayLiteral(_, _) => self.visit_array_literal(node)?,
            DecoratedAstData::SymbolLiteral(_) => self.visit_symbol_literal(node)?,
            DecoratedAstData::StructLiteral(_, _) => self.visit_struct_literal(node)?,
            DecoratedAstData::ErrorOrValue { .. } => self.visit_error_or_value(node)?,
            DecoratedAstData::BinaryOp { .. } => self.visit_binary_op(node)?,
            DecoratedAstData::UnaryOp { .. } => self.visit_unary_op(node)?,
            DecoratedAstData::LogicalOp { .. } => self.visit_logical_op(node)?,
            DecoratedAstData::Identifier(_) => self.visit_identifier(node)?,

            DecoratedAstData::VarDeclaration { .. } => self.visit_var_declaration(node)?,
            DecoratedAstData::VarDeclarations(_) => self.visit_var_declarations(node)?,
            DecoratedAstData::VarAssign { .. } => self.visit_var_assign(node)?,
            DecoratedAstData::VarAssignEqual { .. } => self.visit_var_assign_equal(node)?,
            DecoratedAstData::FunctionCall { .. } => self.visit_function_call(node)?,

            DecoratedAstData::StructDefinition { .. } => self.visit_struct_def(node)?,
            DecoratedAstData::StructField { .. } => self.visit_struct_field(node)?,

            DecoratedAstData::IfStatement { .. } => self.visit_if_statement(node)?,
            DecoratedAstData::Payload(_, _) => self.visit_payload(node)?,
            DecoratedAstData::ForStatement { .. } => self.visit_for_statement(node)?,
            DecoratedAstData::DoWhileStatement { .. } => self.visit_do_while_statement(node)?,
            DecoratedAstData::Body(_, _) => self.visit_body(node, true)?,
            DecoratedAstData::Function { .. } => self.visit_function(node)?,
            DecoratedAstData::ParameterList(_) => self.visit_parameter_list(node)?,
            DecoratedAstData::Parameter(_) => self.visit_parameter(node)?,
            DecoratedAstData::ExpressionStatement(_, _, _) => {
                self.visit_expression_statement(node)?
            }

            DecoratedAstData::IndexGetter { .. } => self.visit_index_getter(node)?,
            DecoratedAstData::IndexSetter { .. } => self.visit_index_setter(node)?,

            DecoratedAstData::GetProperty { .. } => self.visit_property_getter(node)?,
            DecoratedAstData::SetProperty { .. } => self.visit_property_setter(node)?,

            DecoratedAstData::Raw { .. } => self.visit_raw(node)?,
            DecoratedAstData::Lazy(_) => self.visit_lazy(node)?,
            DecoratedAstData::Defer(_, _) => self.visit_defer(node)?,
            DecoratedAstData::Return(_, _) => self.visit_return_statement(node)?,
            DecoratedAstData::Program { .. } => self.visit_program(node)?,
            DecoratedAstData::Comment => self.visit_comment(node)?,
            DecoratedAstData::Empty => {}

            _ => return Err(self.error(format!("Unknown node in compilation '{:#?}'", node,))),
        }

        if self.output_code.len() >= BYTES_BEFORE_WRITE {
            write!(self.file, "{}", &self.output_code).unwrap();
            self.output_code.clear();
        }

        Ok(())
    }

    fn visit_identifier(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Identifier(kind) = &node.data else { unreachable!() };

        match &**kind {
            SpruceType::Function { is_native, .. } => {
                if *is_native {
                    let identifier = node.token.span.slice_source();
                    let identifier = identifier[0..1].to_uppercase() + &identifier[1..];
                    self.output_code
                        .push_str(&format!("{SPRUCE_PRE}.{identifier}"));
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

    fn visit_literal(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        if node.token.kind == TokenKind::String {
            self.output_code
                .push_str(&format!("\"{}\"", node.token.span.slice_source()));
            return Ok(());
        }

        self.output_code.push_str(node.token.span.slice_source());
        Ok(())
    }

    fn visit_symbol_literal(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::SymbolLiteral(value) = &node.data else { unreachable!() };
        self.output_code.push_str(&format!(
            "Symbol.{}",
            self.symbols.get_table()[*value as usize].slice_source(),
        ));
        Ok(())
    }

    fn visit_struct_literal(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::StructLiteral(_, items) = &node.data else { unreachable!() };

        self.output_code
            .push_str(&format!("new {}(){{ ", node.token.span.slice_source(),));

        for (idx, (field_name, arg)) in items.iter().enumerate() {
            self.output_code
                .push_str(&format!("{} = ", field_name.slice_source(),));

            if let Some(arg) = arg {
                self.visit(arg)?;
            } else {
                self.output_code.push_str(field_name.slice_source());
            }

            if idx < items.len() - 1 {
                self.output_code.push_str(", ");
            }
        }

        self.output_code.push_str(" }");

        Ok(())
    }

    fn visit_tuple_literal(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
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

    fn visit_array_literal(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ArrayLiteral(kind, values) = &node.data else { unreachable!() };

        self.output_code
            .push_str(&format!("new {}(){{ ", Compiler::as_cs_type(kind)));

        for (idx, arg) in values.iter().enumerate() {
            self.visit(arg)?;

            if idx < values.len() - 1 {
                self.output_code.push_str(", ");
            }
        }

        self.output_code.push_str(" }");

        Ok(())
    }

    fn visit_error_or_value(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ErrorOrValue { kind, expression, .. } = &node.data else { unreachable!() };

        match &expression.data {
            DecoratedAstData::IfStatement { .. } => self.visit(expression)?,
            _ => {
                self.output_code
                    .push_str(&format!("new {}(", Compiler::as_cs_type(kind)));
                self.visit(expression)?;
                self.output_code.push(')');
            }
        }

        Ok(())
    }

    fn visit_expression_statement(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ExpressionStatement(_, is_statement, expr) = &node.data else { unreachable!() };

        if *is_statement {
            if let DecoratedAstData::Body(_, statements) = &expr.data {
                if statements.len() > 0 {
                    self.output_code.push_str(&self.tab_string());
                    self.visit_body(expr, true)?;
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

    fn visit_comment(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Comment = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{}// {}\n",
            self.tab_string(),
            node.token.span.slice_source(),
        ));

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::BinaryOp { lhs, rhs, .. } = &node.data else { unreachable!() };

        self.visit(lhs)?;
        self.output_code
            .push_str(&format!(" {} ", node.token.span.slice_source(),));
        self.visit(rhs)?;
        Ok(())
    }

    fn visit_unary_op(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::UnaryOp { rhs, .. } = &node.data else { unreachable!() };

        self.output_code.push_str(node.token.span.slice_source());
        self.visit(rhs)?;
        Ok(())
    }

    fn visit_logical_op(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::LogicalOp { kind: _, lhs, rhs } = &node.data else { unreachable!() };

        self.visit(lhs)?;
        self.output_code
            .push_str(&format!(" {} ", node.token.span.slice_source(),));
        self.visit(rhs)?;
        Ok(())
    }

    fn visit_parameter(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Parameter(kind) = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{} {}",
            Compiler::as_cs_type(kind),
            node.token.span.slice_source(),
        ));

        Ok(())
    }

    fn visit_parameter_list(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
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

    fn visit_function(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Function { function_type, parameters, kind, body } = &node.data else { unreachable!() };

        match function_type {
            FunctionType::Standard | FunctionType::Method => {
                self.output_code
                    .push_str(&format!("{}public ", self.tab_string()));
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

        self.output_code
            .push_str(&format!(")\n{}", self.tab_string()));
        match &body.data {
            DecoratedAstData::Body(_, _) => self.visit_body(body, true)?,
            _ => self.wrap_in_body(body)?,
        }
        self.output_code.push_str("\n\n");

        Ok(())
    }

    fn visit_function_call(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
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

    fn visit_var_declaration(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
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

    fn visit_var_declarations(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::VarDeclarations(decls) = &node.data else { unreachable!() };

        for decl in decls {
            self.visit_var_declaration(decl)?;
        }

        Ok(())
    }

    fn visit_var_assign(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::VarAssign { lhs, expression } = &node.data else { unreachable!() };

        self.visit(lhs)?;
        self.output_code.push_str(" = ");
        self.visit(expression)?;

        Ok(())
    }

    fn visit_var_assign_equal(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::VarAssignEqual { operator, lhs, expression } = &node.data else { unreachable!() };

        self.visit(lhs)?;
        self.output_code
            .push_str(&format!(" {} ", operator.span.slice_source(),));
        self.visit(expression)?;

        Ok(())
    }

    fn visit_type(&mut self, _node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_struct_def(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::StructDefinition { kind, is_ref, items } = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{}public {} {} {{\n",
            self.tab_string(),
            if *is_ref { "sealed class" } else { "struct" },
            node.token.span.slice_source(),
        ));

        self.indent();

        if let Some(items) = items {
            for item in items {
                self.visit(item)?;
            }

            if items.len() > 0 {
                if let DecoratedAstData::Function { .. } = &items.last().unwrap().data {
                    self.output_code.pop();
                }
            }
        }

        let SpruceType::Struct { fields, .. } = &**kind else { unreachable!() };
        self.output_code.push_str(&format!(
            "\n{}public override string ToString(){{\n",
            self.tab_string()
        ));
        self.indent();

        if let Some(fields) = fields {
            self.output_code.push_str(&format!(
                "{}return $\"{} {{{{ ",
                self.tab_string(),
                node.token.span.slice_source(),
            ));

            for (idx, field) in fields.iter().enumerate() {
                self.output_code
                    .push_str(&format!("{{{}}}", field.identifier.span.slice_source(),));

                if idx < fields.len() - 1 {
                    self.output_code.push_str(", ");
                }
            }

            self.output_code.push_str(" }}\"");
        } else {
            self.output_code.push_str(&format!(
                "{}return $\"{} {{}}\"",
                self.tab_string(),
                node.token.span.slice_source(),
            ));
        }

        self.output_code.push_str(";\n");

        self.dedent();
        self.output_code
            .push_str(&format!("{}}}\n", self.tab_string()));

        self.dedent();

        self.output_code
            .push_str(&format!("{}}}\n\n", self.tab_string(),));

        Ok(())
    }

    fn visit_struct_field(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::StructField { kind, default_value } = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "{}public {} {}",
            self.tab_string(),
            Compiler::as_cs_type(&*kind),
            node.token.span.slice_source(),
        ));

        if let Some(default_value) = default_value {
            self.output_code.push_str(" = ");
            self.visit(default_value)?;
        }

        self.output_code.push_str(";\n");

        Ok(())
    }

    fn visit_payload(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Payload(kind, expression) = &node.data else { unreachable!() };

        self.output_code.push_str(&format!(
            "((Func<{}>)(() => {{\n",
            Compiler::unwrap_error_as_ok(kind),
        ));
        self.indent();

        self.output_code
            .push_str(&format!("{}var __error_value = ", self.tab_string(),));
        self.visit(expression)?;

        let (lhs_kind, rhs_kind) = Compiler::unwrap_error_as_types(kind);
        self.output_code.push_str(&format!(
            ";\n{}if ({}.Is_ok(__error_value)) {{ return {}.Get_ok(__error_value); }} else {{ throw new {}.SpruceErrorValue<{}, {}>(__error_value); }}",
            self.tab_string(),
            SPRUCE_PRE, SPRUCE_PRE, SPRUCE_PRE,
            lhs_kind, rhs_kind,
        ));

        self.dedent();

        self.output_code
            .push_str(&format!("\n{}}}))()", self.tab_string(),));

        Ok(())
    }

    fn visit_if_statement(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
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
            self.output_code
                .push_str(&format!("{}if (", self.tab_string(),));
            self.visit(condition)?;
            self.output_code
                .push_str(&format!(")\n{}", self.tab_string()));
            self.visit(true_body)?;

            if let Some(false_body) = false_body {
                self.output_code.pop();
                self.output_code
                    .push_str(&format!("{}else\n", self.tab_string(),));
                self.visit(false_body)?;
            } else {
                self.output_code.push('\n');
            }
        }

        Ok(())
    }

    fn visit_for_statement(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::ForStatement { variable, condition, increment, body } = &node.data else { unreachable!() };

        self.output_code
            .push_str(&format!("{}for (", self.tab_string(),));

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
        self.output_code.push('\n');
        Ok(())
    }

    fn visit_do_while_statement(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::DoWhileStatement { body, condition } = &node.data else { unreachable!() };

        self.output_code
            .push_str(&format!("{}do ", self.tab_string()));
        self.visit(body)?;

        self.output_code.push_str(" while (");
        self.visit(condition)?;
        self.output_code.push_str(");\n");
        Ok(())
    }

    fn visit_index_getter(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::IndexGetter { expression, index } = &node.data else { unreachable!() };

        self.visit(expression)?;
        self.output_code.push('[');
        self.visit(index)?;
        self.output_code.push(']');

        Ok(())
    }

    fn visit_index_setter(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::IndexSetter { expression, rhs } = &node.data else { unreachable!() };

        self.visit(expression)?;
        self.output_code.push_str(" = ");
        self.visit(rhs)?;

        Ok(())
    }

    fn visit_property_getter(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::GetProperty { lhs, property } = &node.data else { unreachable!() };

        self.visit(lhs)?;
        self.output_code.push('.');
        self.visit(property)?;

        Ok(())
    }

    fn visit_property_setter(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::SetProperty { lhs, expression } = &node.data else { unreachable!() };

        self.visit(lhs)?;
        self.output_code.push_str(" = ");
        self.visit(expression)?;

        Ok(())
    }

    fn visit_switch_statement(&mut self, _node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_switch_case(&mut self, _node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_raw(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Raw { code, .. } = &node.data else { unreachable!() };

        for item in code {
            self.output_code.push_str(&format!(
                "{}{}\n",
                self.tab_string(),
                item.token.span.slice_source(),
            ));
        }

        Ok(())
    }

    fn visit_lazy(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Lazy(expression) = &node.data else { unreachable!() };

        self.output_code.push_str("new(");
        self.wrap_in_lambda(expression)?;
        self.output_code.push(')');

        Ok(())
    }

    fn visit_defer(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
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

    fn visit_return_statement(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Return(_, expr) = &node.data else { unreachable!() };

        self.output_code
            .push_str(&format!("{}return", self.tab_string(),));

        if let Some(expr) = expr {
            self.output_code.push(' ');
            self.visit(expr)?;
        }

        self.output_code.push_str(";\n");

        Ok(())
    }

    fn visit_body(&mut self, node: &Rc<DecoratedAst>, _new_scope: bool) -> Result<(), SpruceErr> {
        let DecoratedAstData::Body(_, statements) = &node.data else { unreachable!() };
        self.output_code.push_str("{\n");

        self.indent();

        for item in statements {
            self.visit(item)?;
        }

        self.dedent();

        self.output_code
            .push_str(&format!("{}}}", self.tab_string()));
        Ok(())
    }

    fn visit_include(&mut self, _node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        todo!()
    }

    fn visit_program(&mut self, node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        let DecoratedAstData::Program { source: _, body } = &node.data else { unreachable!() };

        for item in body {
            self.visit(item)?;
        }

        Ok(())
    }

    fn visit_empty(&mut self, _node: &Rc<DecoratedAst>) -> Result<(), SpruceErr> {
        unreachable!()
    }
}
