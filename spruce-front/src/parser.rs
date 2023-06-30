use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use spruce_shared::error::{SpruceErr, SpruceErrData};

use crate::{source::Source, types::TypeKind};

use super::{
    ast::{Ast, AstData, ErrorOrValue},
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    source: &'a Source,
    current: Token,
    had_error: bool,
    included: HashSet<PathBuf>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a Source) -> Parser<'a> {
        let mut lexer = Lexer::new(source);
        let token = lexer.next();

        Self {
            lexer,
            source,
            current: token,
            had_error: false,
            included: HashSet::new(),
        }
    }

    pub fn run(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let program = self.outer_statements();
        if self.had_error {
            Err(SpruceErr::new(
                "Encountered an error while parsing".into(),
                SpruceErrData::Parser {
                    file_path: (*self.lexer.source.file_path).clone(),
                    line: self.current.line,
                    column: self.current.column,
                },
            ))
        } else {
            program
        }
    }

    fn error(&self, message: String) -> SpruceErr {
        SpruceErr::new(
            message,
            SpruceErrData::Parser {
                file_path: (*self.lexer.source.file_path).clone(),
                line: self.current.line,
                column: self.current.column,
            },
        )
    }

    fn consume(&mut self, expected: TokenKind, msg: &str) -> Result<(), SpruceErr> {
        if self.current.kind == expected {
            self.current = self.lexer.next();
            return Ok(());
        }

        Err(self.error(String::from(msg)))
    }

    fn consume_here(&mut self) {
        self.current = self.lexer.next();
    }

    fn is_any_of(&self, kinds: &[TokenKind]) -> bool {
        kinds.iter().any(|k| self.current.kind == *k)
    }

    fn symbol(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.current;
        self.consume(TokenKind::Identifier, "Expect identifier after '`'")?;

        Ok(Ast::new_symbol(identifier))
    }

    fn primary(&mut self) -> Result<Box<Ast>, SpruceErr> {
        match self.current.kind {
            TokenKind::Int
            | TokenKind::Float
            | TokenKind::String
            | TokenKind::None
            | TokenKind::True
            | TokenKind::False => {
                let token = self.current;
                self.consume_here();
                Ok(Ast::new_literal(token))
            }

            TokenKind::LParen => {
                self.consume_here();
                let expr = self.expression()?;

                if self.current.kind == TokenKind::Comma {
                    let mut exprs = vec![expr];

                    while self.current.kind == TokenKind::Comma {
                        self.consume_here();
                        exprs.push(self.expression()?);
                    }

                    self.consume(TokenKind::RParen, "Expect ')' to close group expression")?;
                    return Ok(Ast::new_tuple_literal(exprs[0].token.clone(), exprs));
                }

                self.consume(TokenKind::RParen, "Expect ')' to close group expression")?;
                Ok(expr)
            }

            TokenKind::Lazy => {
                let token = self.current;
                self.consume_here();
                Ok(Ast::new_lazy(token, self.body()?))
            }

            TokenKind::LSquare => self.array_literal(),
            TokenKind::At => self.struct_literal(None),
            TokenKind::Pipe => self.anon_function(),
            TokenKind::Backtick => self.symbol(),
            TokenKind::LCurly => self.body(),

            TokenKind::If => self.if_expression_statement(true),

            TokenKind::Identifier => {
                let token = self.current;
                self.consume_here();
                let identifier = Ast::new_identifier(token);

                // FIXME: Allow struct literals without colliding with blocks
                // Ok(if self.current.kind == TokenKind::LCurly {
                //     self.struct_literal(Some(identifier))?
                // } else {
                //     identifier
                // })
                Ok(identifier)
            }

            TokenKind::ResultError | TokenKind::ResultOk => {
                let token = self.current;
                self.consume_here();
                let expression = self.expression()?;

                Ok(Ast::new_error_or_value(
                    match token.kind {
                        TokenKind::ResultError => ErrorOrValue::Error,
                        TokenKind::ResultOk => ErrorOrValue::Value,
                        _ => unreachable!(),
                    },
                    expression,
                ))
            }

            _ => Err(self.error(format!(
                "Unexpected token found {:?} '{}'",
                self.current.kind,
                self.source
                    .maybe_slice_from(self.current.lexeme)
                    .unwrap_or(&self.current.kind.to_string()),
            ))),
        }
    }

    fn call(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.primary()?;

        loop {
            node = match self.current.kind {
                TokenKind::LParen => self.function_call(node)?,
                TokenKind::LSquare => self.index(node)?,
                TokenKind::Dot => self.dot_property(node)?,
                _ => break,
            }
        }

        Ok(node)
    }

    fn unary(&mut self) -> Result<Box<Ast>, SpruceErr> {
        loop {
            match self.current.kind {
                TokenKind::Minus | TokenKind::Bang => {
                    let token = self.current;
                    self.consume_here();
                    return Ok(Ast::new_unary_op(token, self.expression()?));
                }
                _ => break,
            }
        }

        self.call()
    }

    fn factor(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.unary()?;

        loop {
            match self.current.kind {
                TokenKind::Star | TokenKind::Slash => {
                    let token = self.current;
                    self.consume_here();
                    node = Ast::new_binary_op(token, node, self.unary()?);
                }

                _ => break,
            }
        }

        Ok(node)
    }

    fn term(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.factor()?;

        loop {
            match self.current.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    let token = self.current;
                    self.consume_here();
                    node = Ast::new_binary_op(token, node, self.factor()?);
                }

                _ => break,
            }
        }

        Ok(node)
    }

    fn comparison(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.term()?;

        loop {
            match self.current.kind {
                TokenKind::Greater
                | TokenKind::Less
                | TokenKind::GreaterEqual
                | TokenKind::LessEqual => {
                    let token = self.current;
                    self.consume_here();
                    node = Ast::new_logical_op(token, node, self.term()?);
                }

                _ => break,
            }
        }

        Ok(node)
    }

    fn equality(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.comparison()?;

        loop {
            match self.current.kind {
                TokenKind::EqualEqual | TokenKind::NotEqual => {
                    let token = self.current;
                    self.consume_here();
                    node = Ast::new_logical_op(token, node, self.comparison()?);
                }

                _ => break,
            }
        }

        Ok(node)
    }

    fn or(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.equality()?;

        while self.current.kind == TokenKind::And {
            let token = self.current;
            self.consume_here();
            node = Ast::new_logical_op(token, node, self.expression()?);
        }

        Ok(node)
    }

    fn and(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.or()?;

        while self.current.kind == TokenKind::And {
            let token = self.current;
            self.consume_here();
            node = Ast::new_logical_op(token, node, self.expression()?);
        }

        Ok(node)
    }

    fn payload(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.and()?;

        if self.current.kind == TokenKind::QuestionMark {
            self.consume_here();
            node = Ast::new_payload(node.token.clone(), node);
        }

        Ok(node)
    }

    fn assignment(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = self.payload()?;

        loop {
            node = match self.current.kind {
                TokenKind::Equal => {
                    self.consume_here();
                    match node.data {
                        AstData::Identifier => {
                            Ast::new_var_assign(node.token.clone(), node, self.expression()?)
                        }
                        AstData::IndexGetter { .. } => {
                            Ast::new_index_setter(node.token.clone(), node, self.expression()?)
                        }
                        AstData::PropertyGetter { .. } => {
                            Ast::new_property_setter(node.token.clone(), node, self.expression()?)
                        }
                        _ => {
                            return Err(self.error(format!(
                                "Cannot use '{}':{:?} on lhs of assignment",
                                self.source
                                    .maybe_slice_from(node.token.lexeme)
                                    .unwrap_or(&self.current.kind.to_string()),
                                node.token.kind,
                            )))
                        }
                    }
                }

                TokenKind::PlusEqual
                | TokenKind::MinusEqual
                | TokenKind::StarEqual
                | TokenKind::SlashEqual => {
                    let operator = self.current;
                    self.consume_here();
                    match node.data {
                        AstData::Identifier => Ast::new_var_assign_equal(
                            node.token.clone(),
                            operator,
                            node,
                            self.expression()?,
                        ),
                        _ => {
                            return Err(self.error(format!(
                                "Cannot use '{}':{:?} on lhs of assignment operator '{}'",
                                self.source
                                    .maybe_slice_from(node.token.lexeme)
                                    .unwrap_or(&self.current.kind.to_string()),
                                node.token.kind,
                                self.source
                                    .maybe_slice_from(operator.lexeme)
                                    .unwrap_or(&self.current.kind.to_string()),
                            )))
                        }
                    }
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn expression(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.assignment()
    }

    fn expression_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let node = Ast::new_expr_statement(true, self.expression()?);
        self.consume(
            TokenKind::SemiColon,
            "Expect ';' after expression statement",
        )?;
        Ok(node)
    }

    fn array_literal(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();

        let mut values = Vec::new();

        if self.current.kind != TokenKind::RSquare {
            values.push(self.expression()?);

            while self.current.kind == TokenKind::Comma {
                self.consume_here();
                values.push(self.expression()?);
            }
        }

        self.consume(
            TokenKind::RSquare,
            "Expect ']' after array literal arguments",
        )?;

        Ok(Ast::new_array_literal(token, values))
    }

    fn struct_literal(&mut self, identifier: Option<Box<Ast>>) -> Result<Box<Ast>, SpruceErr> {
        let (token, identifier) = match identifier {
            Some(identifier) => (identifier.token.clone(), identifier.token.clone()),
            _ => {
                let token = self.current;
                self.consume_here();

                let identifier = self.current;
                self.consume(
                    TokenKind::Identifier,
                    "Expect identifier after '@' in struct literal",
                )?;
                (token, identifier)
            }
        };

        self.consume(
            TokenKind::LCurly,
            "Expect '{' after '@' to start map literal",
        )?;

        let mut values = Vec::new();

        if self.current.kind != TokenKind::RCurly {
            let identifier = self.current;
            self.consume(TokenKind::Identifier, "Expect identifier as key in map")?;
            let mut expr = None;

            if self.current.kind == TokenKind::Colon {
                self.consume_here();
                expr = Some(self.expression()?);
            }

            values.push((identifier, expr));

            while self.current.kind == TokenKind::Comma {
                self.consume_here();
                let identifier = self.current;
                self.consume(TokenKind::Identifier, "Expect identifier as key in map")?;
                let mut expr = None;

                if self.current.kind == TokenKind::Colon {
                    self.consume_here();
                    expr = Some(self.expression()?);
                }

                values.push((identifier, expr));
            }
        }

        self.consume(TokenKind::RCurly, "Expect '}' after map literal")?;

        Ok(Ast::new_struct_literal(token, identifier, values))
    }

    fn function_call(&mut self, lhs: Box<Ast>) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume(TokenKind::LParen, "Expect '(' after function identifier")?;

        // We track arguments, since native functions can have N..Any parameters
        let mut arguments = Vec::new();
        if self.current.kind != TokenKind::RParen {
            arguments.push(self.expression()?);

            while self.current.kind == TokenKind::Comma {
                self.consume(TokenKind::Comma, "Expect ',' after function argument")?;
                arguments.push(self.expression()?);
            }
        }

        self.consume(TokenKind::RParen, "Expect ')' after argument list")?;
        Ok(Ast::new_function_call(token, lhs, arguments))
    }

    fn index(&mut self, expression: Box<Ast>) -> Result<Box<Ast>, SpruceErr> {
        self.consume(TokenKind::LSquare, "Expect '[' after expression to index")?;
        let index = self.expression()?;
        self.consume(TokenKind::RSquare, "Expect ']' after index expression")?;

        Ok(Ast::new_index_getter(
            expression.token.clone(),
            expression,
            index,
        ))
    }

    fn dot_property(&mut self, lhs: Box<Ast>) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.current;
        self.consume(TokenKind::Identifier, "Expect identifier after '.'")?;
        Ok(Ast::new_property_getter(
            identifier.clone(),
            lhs,
            Ast::new_identifier(identifier),
        ))
    }

    fn single_statement_block(&mut self) -> Result<Box<Ast>, SpruceErr> {
        match self.current.kind {
            TokenKind::If => self.if_expression_statement(false),
            TokenKind::For => self.for_statement(),
            _ => self.body(),
        }
    }

    fn if_expression_statement(&mut self, force_else: bool) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();

        let condition = self.expression()?;
        let true_body = self.single_statement_block()?;
        let mut false_body = None;

        if self.current.kind == TokenKind::Else {
            self.consume_here();

            false_body = Some(if self.current.kind == TokenKind::If {
                self.if_expression_statement(force_else)?
            } else {
                self.body()?
            });
        } else if force_else {
            return Err(self.error("If expected an else branch".into()));
        }

        Ok(Ast::new_if_statement(
            token, force_else, condition, true_body, false_body,
        ))
    }

    fn for_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();

        let mut variable = None;

        if self.is_any_of(&[TokenKind::Var, TokenKind::Val]) {
            variable = Some(self.var_declaration()?);
            self.consume(
                TokenKind::SemiColon,
                "Expect ';' after binding declarion in for",
            )?;
        }

        // Evaluate condition
        let condition = self.expression()?;
        let mut increment = None;

        if self.current.kind == TokenKind::SemiColon {
            self.consume(TokenKind::SemiColon, "Expect ';' after for condition")?;
            increment = Some(self.expression()?);
        }

        Ok(Ast::new_for_statement(
            token,
            variable,
            condition,
            increment,
            self.single_statement_block()?,
        ))
    }

    fn do_while_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();

        let body = self.single_statement_block()?;

        self.consume(TokenKind::While, "Expect 'while' after do block")?;
        let condition = self.expression()?;

        Ok(Ast::new_do_while_statement(token, body, condition))
    }

    fn switch_case(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        let case = if self.current.kind == TokenKind::Else {
            self.consume_here();
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenKind::Colon, "Expect ':' after switch case value")?;
        Ok(Ast::new_switch_case(
            token,
            case,
            if self.current.kind == TokenKind::LCurly {
                self.body()?
            } else {
                self.expression_statement()?
            },
        ))
    }

    fn switch_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();

        let condition = self.expression()?;
        self.consume(TokenKind::LCurly, "Expect '{' after switch condition")?;

        let mut cases = Vec::new();

        while self.current.kind != TokenKind::RCurly {
            cases.push(self.switch_case()?);
        }

        self.consume(TokenKind::RCurly, "Expect '}' after switch cases")?;
        Ok(Ast::new_switch_statement(token, condition, cases))
    }

    fn return_statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();
        let mut expression = None;

        if self.current.kind != TokenKind::SemiColon {
            expression = Some(self.expression()?);
        }

        Ok(Ast::new_return(token, expression))
    }

    fn defer(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();

        let expression = match self.current.kind {
            TokenKind::LCurly => self.body()?,
            _ => self.expression()?,
        };

        Ok(Ast::new_defer(token, expression))
    }

    fn statement(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let mut node = match self.current.kind {
            TokenKind::Function => {
                let func = self.function()?;
                if let AstData::Function { body, .. } = &func.data {
                    if let AstData::Return(_) = &body.data {
                        self.consume(TokenKind::SemiColon, "Expect ';' after function statement")?;
                    }
                }
                func
            }
            TokenKind::If => self.if_expression_statement(false)?,
            TokenKind::For => self.for_statement()?,
            TokenKind::Do => self.do_while_statement()?,
            TokenKind::Switch => self.switch_statement()?,
            TokenKind::Var | TokenKind::Val => self.var_declaration()?,
            TokenKind::Return => self.return_statement()?,
            TokenKind::Comment => {
                let current = self.current;
                self.consume_here();
                Ast::new_comment(current)
            }
            TokenKind::Raw => self.raw_code()?,
            TokenKind::Defer => self.defer()?,
            _ => {
                let node = self.expression()?;
                let is_stmt = match node.data {
                    _ if self.current.kind == TokenKind::SemiColon => {
                        self.consume_here();
                        true
                    }
                    _ => false,
                };
                return Ok(Ast::new_expr_statement(is_stmt, node));
            }
        };

        // Trailing if statement
        match node.data {
            // Disallow after certain types of statement
            AstData::Comment
            | AstData::IfStatement { .. }
            | AstData::Raw { .. }
            | AstData::SwitchStatement { .. }
            | AstData::Function { .. } => {}
            _ => {
                if self.current.kind == TokenKind::If {
                    let token = self.current;
                    self.consume_here();
                    node = Ast::new_if_statement(
                        token.clone(),
                        false,
                        self.expression()?,
                        Ast::new_body(token, vec![node]),
                        None,
                    );

                    self.consume(
                        TokenKind::SemiColon,
                        "Expect ';' after trailing if statement",
                    )?;
                    return Ok(node);
                }
            }
        }

        match node.data {
            AstData::Comment
            | AstData::SwitchStatement { .. }
            | AstData::Function { .. }
            | AstData::IfStatement { .. }
            | AstData::ForStatement { .. }
            | AstData::Raw { .. } => {}
            _ => self.consume(TokenKind::SemiColon, "Expect ';' after statement")?,
        }

        Ok(node)
    }

    fn body(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume(TokenKind::LCurly, "Expect '{' to start block body")?;
        let mut statements = Vec::new();

        while self.current.kind != TokenKind::RCurly {
            statements.push(self.statement()?);
        }

        self.consume(TokenKind::RCurly, "Expect '}' to end block body")?;

        Ok(Ast::new_body(token, statements))
    }

    #[inline]
    fn consume_parameter(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let param_name = self.current;
        self.consume(
            TokenKind::Identifier,
            "Expected identifier in parameter list",
        )?;

        let type_name = self.collect_type()?;

        Ok(Ast::new_parameter(param_name, type_name))
    }

    #[inline]
    fn consume_struct_field(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let field_name = self.current;
        self.consume(
            TokenKind::Identifier,
            "Expected identifier in struct fields",
        )?;

        let type_signature = self.collect_type()?;

        let default_value = if self.current.kind == TokenKind::Equal {
            self.consume_here();
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Ast::new_struct_field(
            field_name,
            type_signature,
            default_value,
        ))
    }

    fn collect_params(
        &mut self,
        left: TokenKind,
        right: TokenKind,
    ) -> Result<Option<Vec<Box<Ast>>>, SpruceErr> {
        let parameters = if self.current.kind == left {
            let mut parameters = Vec::new();
            self.consume(
                left,
                &format!("Expect '{:?}' at the start of parameter list", left),
            )?;

            // Consume paarameter list
            // TODO: Underscore to add unnamed parameter
            if self.current.kind != right {
                parameters.push(self.consume_parameter()?);

                while self.current.kind == TokenKind::Comma {
                    self.consume_here();
                    parameters.push(self.consume_parameter()?);
                }
            }

            self.consume(
                right,
                &format!("Expect '{:?}' after function parameter list", right),
            )?;
            Some(parameters)
        } else {
            None
        };

        Ok(parameters)
    }

    fn collect_body(&mut self) -> Result<Box<Ast>, SpruceErr> {
        Ok(if self.current.kind == TokenKind::Equal {
            let token = self.current;
            self.consume_here();
            Ast::new_return(token, Some(self.expression()?))
        } else {
            self.body()?
        })
    }

    fn anon_function(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        let parameters = self.collect_params(TokenKind::Pipe, TokenKind::Pipe)?;
        let return_type = match self.current.kind {
            TokenKind::LCurly | TokenKind::Equal => None,
            _ => Some(self.collect_type()?),
        };
        let body = self.collect_body()?;

        Ok(Ast::new_function(
            token,
            true,
            parameters,
            return_type,
            body,
        ))
    }

    fn include(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();
        match self.current.kind {
            TokenKind::Identifier | TokenKind::String => {
                // TODO: Check if path is already included, can use std lib token
                let include = if self.current.kind == TokenKind::Identifier {
                    let identifier = self
                        .source
                        .maybe_slice_from(self.current.lexeme)
                        .unwrap_or(&self.current.kind.to_string())
                        .to_string();

                    let path = PathBuf::from(identifier);
                    if self.included.contains(&path) {
                        let noop = Ast::new_empty(self.current);
                        self.consume_here();
                        return Ok(noop);
                    }
                    self.included.insert(path);

                    let token = self.current;
                    self.consume_here();

                    Ast::new_std_include(token)
                } else {
                    let maybe_path = Path::new(&*self.lexer.source.file_path)
                        .parent()
                        .unwrap()
                        .join(
                            self.source
                                .maybe_slice_from(self.current.lexeme)
                                .unwrap_or(&self.current.kind.to_string()),
                        );

                    let include_path = fs::canonicalize(&maybe_path);

                    if include_path.is_err() {
                        let path = maybe_path.to_str().unwrap().to_string();
                        return Err(self.error(format!("Include path does not exist '{}'", path)));
                    }

                    let include_path = include_path.unwrap();

                    if self.included.contains(&include_path) {
                        let noop = Ast::new_empty(self.current);
                        self.consume_here();
                        return Ok(noop);
                    }

                    let _source = fs::read_to_string(&include_path).unwrap();
                    // // let program = match util::compile_source(
                    // //     include_str.clone(),
                    // //     source,
                    // //     self.args.clone(),
                    // // ) {
                    // //     Ok((_, program)) => program,
                    // //     Err(e) => {
                    // //         return Err(self.error(format!(
                    // //             "Could not parse '{}' because {}",
                    // //             include_str, e.message
                    // //         )))
                    // //     }
                    // // };

                    self.included.insert(include_path);
                    // program

                    // FIXME: Make the above code not trash to allow imports
                    Ast::new_empty(self.current)
                };

                self.consume_here();

                Ok(include)
            }
            _ => Err(self.error(format!(
                "Include expected string or identifier, but recieved '{}'",
                self.source
                    .maybe_slice_from(self.current.lexeme)
                    .unwrap_or(&self.current.kind.to_string())
            ))),
        }
    }

    fn function(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let identifier = self.current;
        self.consume(TokenKind::Identifier, "Expected identifier after 'fn'")?;

        let parameters = self.collect_params(TokenKind::LParen, TokenKind::RParen)?;

        let return_type = match self.current.kind {
            TokenKind::LCurly | TokenKind::Equal => None,
            _ => {
                self.consume(TokenKind::Colon, "Expect ':' before return type")?;
                Some(self.collect_type()?)
            }
        };

        let body = self.collect_body()?;

        Ok(Ast::new_function(
            identifier,
            false,
            parameters,
            return_type,
            body,
        ))
    }

    fn collect_var_decl(&mut self, is_mutable: bool) -> Result<Box<Ast>, SpruceErr> {
        let identifier = self.current;
        self.consume(
            TokenKind::Identifier,
            "Expected identifier after 'var'/'val'",
        )?;

        let kind = if self.current.kind == TokenKind::Colon {
            self.consume_here();
            Some(self.collect_type()?)
        } else {
            None
        };

        // Produce the expression
        let mut expr = None;
        if self.current.kind == TokenKind::Equal {
            self.consume_here();
            expr = Some(self.expression()?);
        }

        Ok(Ast::new_var_decl(identifier, is_mutable, kind, expr))
    }

    fn collect_type(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let kind = match self.current.kind {
            TokenKind::Identifier | TokenKind::None => {
                let identifier = self.current;
                self.consume_here();
                Ast::new_type(identifier, TypeKind::Standard)
            }
            TokenKind::LSquare => {
                self.consume_here();
                let inner = self.collect_type()?;
                self.consume(TokenKind::RSquare, "Expect closing ']' after type")?;
                Ast::new_type(inner.token.clone(), TypeKind::Array(inner))
            }
            TokenKind::LParen => {
                self.consume_here();
                let mut types = vec![self.collect_type()?];

                while self.current.kind == TokenKind::Comma {
                    self.consume_here();
                    types.push(self.collect_type()?);
                }

                self.consume(TokenKind::RParen, "Expect closing ')' after type")?;
                Ast::new_type(self.current, TypeKind::Tuple(types))
            }
            TokenKind::Lazy => {
                let token = self.current;
                self.consume_here();

                Ast::new_type(token, TypeKind::Lazy(self.collect_type()?))
            }
            TokenKind::Function => {
                let token = self.current;
                self.consume_here();
                self.consume(TokenKind::LParen, "Expect '(' to start function type")?;

                let parameters = if self.current.kind != TokenKind::RParen {
                    let mut types = vec![self.collect_type()?];

                    while self.current.kind == TokenKind::Comma {
                        self.consume_here();
                        types.push(self.collect_type()?);
                    }

                    Some(types)
                } else {
                    None
                };

                self.consume(TokenKind::RParen, "Expect ')' to close function type")?;
                self.consume(
                    TokenKind::Colon,
                    "Expect ':' after function type parameter list",
                )?;
                let return_type = self.collect_type()?;

                Ast::new_type(
                    token,
                    TypeKind::Function {
                        parameters,
                        return_type,
                    },
                )
            }
            _ => {
                return Err(self.error(format!(
                    "Unknown item in type definition '{}'",
                    self.source
                        .maybe_slice_from(self.current.lexeme)
                        .unwrap_or(&self.current.kind.to_string()),
                )))
            }
        };

        // Zig-like Error union
        if self.current.kind == TokenKind::Bang {
            let token = self.current;
            self.consume_here();
            let rhs_kind = self.collect_type()?;
            return Ok(Ast::new_type(token, TypeKind::ErrorOrValue(kind, rhs_kind)));
        }
        // TODO: Optional?

        Ok(kind)
    }

    fn var_declaration(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let is_mutable = self.current.kind == TokenKind::Var;
        self.consume_here();

        let mut decls = vec![self.collect_var_decl(is_mutable)?];

        while self.current.kind == TokenKind::Comma {
            self.consume_here();
            decls.push(self.collect_var_decl(is_mutable)?);
        }

        Ok(Ast::new_var_decls(decls))
    }

    fn struct_def(&mut self, identifier: Token) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();

        let is_ref = if self.current.kind == TokenKind::Ref {
            self.consume_here();
            true
        } else {
            false
        };

        self.consume(TokenKind::LCurly, "Expect '{' after struct")?;

        let items = if self.current.kind != TokenKind::RCurly {
            let mut items = Vec::new();

            while self.current.kind != TokenKind::RCurly {
                match self.current.kind {
                    TokenKind::Function => {
                        let func = self.function()?;
                        if let AstData::Function { body, .. } = &func.data {
                            if let AstData::Return(_) = &body.data {
                                self.consume(
                                    TokenKind::SemiColon,
                                    "Expect ';' after function statement",
                                )?;
                            }
                        }
                        items.push(func);
                    }
                    TokenKind::Identifier => {
                        items.push(self.consume_struct_field()?);
                        self.consume(TokenKind::SemiColon, "Expect ';' after field statement")?;
                    }
                    TokenKind::Comment => {
                        let current = self.current;
                        self.consume_here();
                        items.push(Ast::new_comment(current));
                    }
                    _ => {
                        return Err(self.error(format!(
                            "Unknown item in type definition scope {:?}",
                            self.current.kind
                        )))
                    }
                }
            }

            Some(items)
        } else {
            None
        };

        self.consume(TokenKind::RCurly, "Expect '}' after struct items")?;

        Ok(Ast::new_struct_definition(identifier, is_ref, items))
    }

    fn type_def(&mut self) -> Result<Box<Ast>, SpruceErr> {
        self.consume_here();
        let identifier = self.current;
        self.consume(TokenKind::Identifier, "Expect identifier after 'type'")?;

        self.consume(TokenKind::Colon, "Expect ':' after type def identifier")?;

        Ok(match self.current.kind {
            TokenKind::Struct => self.struct_def(identifier)?,
            _ => {
                return Err(self.error(format!(
                    "Unexpected item after type def '{}'",
                    self.source
                        .maybe_slice_from(self.current.lexeme)
                        .unwrap_or(&self.current.kind.to_string()),
                )))
            }
        })
    }

    fn raw_code(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        self.consume_here();

        let returns = if self.current.kind == TokenKind::Return {
            self.consume_here();
            Some(self.collect_type()?)
        } else {
            None
        };

        self.consume(TokenKind::LCurly, "Expect '{' after raw")?;

        let mut code = Vec::new();
        while self.current.kind == TokenKind::String {
            code.push(self.primary()?);
        }

        self.consume(TokenKind::RCurly, "Expect '}' after raw code")?;

        Ok(Ast::new_raw(token, returns, code))
    }

    fn outer_statements(&mut self) -> Result<Box<Ast>, SpruceErr> {
        let token = self.current;
        let mut statements = Vec::new();

        while self.current.kind != TokenKind::EndOfFile {
            match self.current.kind {
                TokenKind::Include => {
                    statements.push(self.include()?);
                    self.consume(TokenKind::SemiColon, "Expect ';' after include statement")?;
                }
                TokenKind::Function => {
                    let func = self.function()?;
                    if let AstData::Function { body, .. } = &func.data {
                        if let AstData::Return(_) = &body.data {
                            self.consume(
                                TokenKind::SemiColon,
                                "Expect ';' after function statement",
                            )?;
                        }
                    }
                    statements.push(func);
                }
                TokenKind::Var | TokenKind::Val => {
                    statements.push(self.var_declaration()?);
                    self.consume(TokenKind::SemiColon, "Expect ';' after variable statement")?;
                }
                TokenKind::Type => statements.push(self.type_def()?),
                TokenKind::Comment => {
                    let current = self.current;
                    self.consume_here();
                    statements.push(Ast::new_comment(current));
                }
                TokenKind::Raw => statements.push(self.raw_code()?),
                _ => {
                    return Err(self.error(format!(
                        "Unknown item in outer scope {:?}",
                        self.current.kind
                    )))
                }
            }
        }

        Ok(Ast::new_program(token, statements))
    }
}

#[test]
fn simple_binary_operation() {
    let source = Source::from("1 + 2");
    let mut parser = Parser::new(&source);

    let expr = parser.expression();
    assert!(expr.is_ok());
    let expr = expr.unwrap();

    assert!(matches!(expr.data, AstData::BinaryOp { .. }));

    let AstData::BinaryOp { lhs, rhs } = expr.data else { unreachable!() };
    assert!(matches!(lhs.data, AstData::Literal));
    assert!(matches!(rhs.data, AstData::Literal));
}
