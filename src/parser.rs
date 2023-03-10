use std::{rc::Rc, path::Path, fs, io::Error};

use crate::{token::{Token, TokenKind}, lexer::Lexer, ast::{Ast, AstData}, source::Source, util};

pub struct Parser {
    lexer: Lexer,
    current: Token,
    script_mode: bool,
    had_error: bool,
}

#[derive(Debug)]
pub struct ParserErr {
    pub file_path: String,
    pub message: String,
    pub line: u32,
    pub column: u16,
}

impl Parser {
    pub fn new(source: &Rc<Source>, script_mode: bool) -> Result<Self, Error> {
        let mut lexer = Lexer::new(source)?;
        let token = lexer.next();

        Ok(Self {
            lexer,
            current: token,
            script_mode,
            had_error: false,
        })
    }

    pub fn run(&mut self) -> Result<Box<Ast>, ParserErr> {
        let program = self.outer_statements();
        if self.had_error {
            Err(ParserErr {
                file_path: (*self.lexer.source.file_path).clone(),
                message: "Encountered an error while parsing".into(),
                line: self.current.line,
                column: self.current.column,
            })
        } else {
            program
        }
    }
    
    fn error(&self, message: String) -> ParserErr {
        ParserErr {
            file_path: (*self.lexer.source.file_path).clone(),
            message,
            line: self.current.line,
            column: self.current.column,
        }
    }

    fn consume(&mut self, expected: TokenKind, msg: &str) -> Result<(), ParserErr> {
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

    fn primary(&mut self) -> Result<Box<Ast>, ParserErr> {
        match self.current.kind {
            TokenKind::Number | TokenKind::String | TokenKind::None
            | TokenKind::True | TokenKind::False => {
                let token = self.current.clone();
                self.consume_here();
                Ok(Ast::new_literal(token))
            }

            TokenKind::LParen => {
                self.consume_here();
                let expr = self.expression()?;
                self.consume(TokenKind::RParen, "Expect ')' to close group expression")?;
                Ok(expr)
            },

            TokenKind::LSquare => self.list_literal(),
            TokenKind::Pipe => self.anon_function(),

            TokenKind::Identifier => {
                let token = self.current.clone();
                self.consume_here();
                Ok(Ast::new_identifier(token))
            },

            _ => Err(self.error(format!(
                "Unexpected token found {:?} '{}'",
                self.current.kind,
                self.current.span.slice_source(),
            ))),
        }
    }

    fn call(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.primary()?;

        while self.is_any_of(&[TokenKind::LParen, TokenKind::LSquare]) {
        	node = match self.current.kind {
                TokenKind::LParen => self.function_call(node)?,
                TokenKind::LSquare => self.index(node)?,
                _ => unreachable!(),
            }
        }

        Ok(node)
    }

    fn unary(&mut self) -> Result<Box<Ast>, ParserErr> {
        if self.is_any_of(&[TokenKind::Minus, TokenKind::Bang]) {
            match self.current.kind {
                TokenKind::Minus | TokenKind::Bang => {
                    let token = self.current.clone();
                    self.consume_here();
                    return Ok(Ast::new_unary_op(token, self.call()?));
                },
                _ => unreachable!(),
            }
        }

        self.call()
    }

    fn factor(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.unary()?;
        
        loop {
            match self.current.kind {
                TokenKind::Star | TokenKind::Slash => {
                    let token = self.current.clone();
                    self.consume_here();
                    node = Ast::new_binary_op(token, node, self.unary()?);
                }
    
                _ => break,
            }
        }

        Ok(node)
    }

    fn term(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.factor()?;
        
        loop {
            match self.current.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    let token = self.current.clone();
                    self.consume_here();
                    node = Ast::new_binary_op(token, node, self.factor()?);
                }
    
                _ => break,
            }
        }

        Ok(node)
    }

    fn comparison(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.term()?;
        
        loop {
            match self.current.kind {
                TokenKind::Greater |TokenKind::Less
                | TokenKind::GreaterEqual | TokenKind::LessEqual => {
                    let token = self.current.clone();
                    self.consume_here();
                    node = Ast::new_logical_op(token, node, self.term()?);
                }
    
                _ => break,
            }
        }

        Ok(node)
    }

    fn equality(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.comparison()?;

        loop {
            match self.current.kind {
                TokenKind::EqualEqual | TokenKind::NotEqual => {
                    let token = self.current.clone();
                    self.consume_here();
                    node = Ast::new_binary_op(token, node, self.comparison()?);
                }
    
                _ => break,
            }
        }

        Ok(node)
    }

    fn type_equality(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.equality()?;

        if self.is_any_of(&[TokenKind::Is, TokenKind::Ensure]) {
        	let is_assert = self.current.kind == TokenKind::Ensure;
        	self.consume_here();
        	let type_id = self.current.clone();
            match self.current.kind {
                TokenKind::Identifier | TokenKind::None => self.consume_here(),
                _ => {
                    self.consume(TokenKind::Identifier, &format!(
                        "Expect identifier after is/ensure, but found '{}'",
                        type_id.span.slice_source()
                    ))?;
                }
            }
            node = Ast::new_type_check(is_assert, node, type_id);
        }

        Ok(node)
    }

    fn assignment(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.type_equality()?;

        while self.current.kind == TokenKind::Equal {
            self.consume_here();
            node = match node.data {
                AstData::Identifier => Ast::new_var_assign(node.token.clone(), node, self.expression()?),
                AstData::IndexGetter {..} => Ast::new_index_setter(node.token.clone(), node, self.expression()?),
                _ => unreachable!(),
            }
        }

        Ok(node)
    }

    fn expression(&mut self) -> Result<Box<Ast>, ParserErr> {
        self.assignment()
    }

    fn list_literal(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
        self.consume_here();

        let mut values = Vec::new();

        if self.current.kind != TokenKind::RSquare {
            values.push(self.expression()?);
            
            while self.current.kind == TokenKind::Comma {
                self.consume_here();
                values.push(self.expression()?);
            }
        }

        self.consume(TokenKind::RSquare, "Expect ']' after list literal arguments")?;

        Ok(Ast::new_list_literal(token, values))
    }

    fn function_call(&mut self, lhs: Box<Ast>) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
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

    fn index(&mut self, expression: Box<Ast>) -> Result<Box<Ast>, ParserErr> {
        self.consume(TokenKind::LSquare, "Expect '[' after expression to index")?;
        let index = self.expression()?;
        self.consume(TokenKind::RSquare, "Expect ']' after index expression")?;
        
        Ok(Ast::new_index_getter(expression.token.clone(), expression, index))
    }

    fn single_statement_block(&mut self) -> Result<Box<Ast>, ParserErr> {
        match self.current.kind {
            TokenKind::If => self.if_statement(),
            TokenKind::For => self.for_statement(),
            _ => self.body(),
        }
    }

    fn if_statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
        self.consume_here();

        let condition = self.expression()?;
        let true_body = self.single_statement_block()?;
        let mut false_body = None;

        if self.current.kind == TokenKind::Else {
            self.consume_here();

            false_body = Some(if self.current.kind == TokenKind::If {
                self.if_statement()?
            } else {
                self.body()?
            });
        }
        
        Ok(Ast::new_if_statement(token, condition, true_body, false_body))
    }

    fn for_statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
        self.consume_here();
        
        let mut variable = None;

        if self.is_any_of(&[TokenKind::Var, TokenKind::Val]) {
            variable = Some(self.var_declaration()?);
            self.consume(TokenKind::SemiColon, "Expect ';' after binding declarion in for")?;
        }

        // Evaluate condition
        let condition = self.expression()?;
        let mut increment = None;

        if self.current.kind == TokenKind::SemiColon {
            self.consume(TokenKind::SemiColon, "Expect ';' after for condition")?;
            increment = Some(self.expression()?);
        }

        Ok(Ast::new_for_statement(token, variable, condition, increment, self.single_statement_block()?))
    }

    fn do_while_statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
        self.consume_here();

        let body = self.single_statement_block()?;

        self.consume(TokenKind::While, "Expect 'while' after do block")?;
        let condition = self.expression()?;

        Ok(Ast::new_do_while_statement(token, body, condition))
    }

    fn return_statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
        self.consume_here();
        let mut expression = None;

        if self.current.kind != TokenKind::SemiColon {
            expression = Some(self.expression()?);
        }

        Ok(Ast::new_return(token, expression))
    }

    fn statement(&mut self) -> Result<Box<Ast>, ParserErr> {
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
            TokenKind::If => self.if_statement()?,
            TokenKind::For => self.for_statement()?,
            TokenKind::Do => self.do_while_statement()?,
            TokenKind::Var | TokenKind::Val => self.var_declaration()?,
            TokenKind::Return => self.return_statement()?,
            TokenKind::LCurly => self.body()?,
            _ => Ast::new_expr_statement(self.expression()?),
        };

        // Trailing if statement
        if self.current.kind == TokenKind::If {
            let token = self.current.clone();
            self.consume_here();
            node = Ast::new_trailing_if(token, node, self.expression()?);
        }

        match node.data {
            AstData::Function {..} | AstData::IfStatement {..} | AstData::ForStatement {..} | AstData::Body(_) => {}
            _ => self.consume(TokenKind::SemiColon, "Expect ';' after statement")?,
        }
        
        Ok(node)
    }

    fn body(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
        self.consume(TokenKind::LCurly, "Expect '{' to start block body")?;
        let mut statements = Vec::new();

        while self.current.kind != TokenKind::RCurly {
            statements.push(self.statement()?);
        }

        self.consume(TokenKind::RCurly, "Expect '}' to end block body")?;

        Ok(Ast::new_body(token, statements))
    }

    #[inline]
    fn consume_parameter(&mut self) -> Result<Box<Ast>, ParserErr> {
        let param_name = self.current.clone();
        let mut type_name = None;
        self.consume(TokenKind::Identifier, "Expected identifier in parameter list")?;

        if self.current.kind == TokenKind::Colon {
            self.consume_here();
            type_name = Some(self.current.clone());
            match self.current.kind {
                TokenKind::Identifier | TokenKind::None => self.consume_here(),
                _ => self.consume(TokenKind::Identifier, "Expected type name after identifier")?,
            }
            self.consume(TokenKind::Identifier, "")?;
        }

        Ok(Ast::new_parameter(param_name, type_name))
    }

    fn collect_params_and_body(
        &mut self,
        left: TokenKind,
        right: TokenKind
    ) -> Result<(Option<Vec<Box<Ast>>>, Box<Ast>), ParserErr> {
        let parameters = if self.current.kind == left {
            let mut parameters = Vec::new();
            self.consume(left, &format!("Expect '{:?}' at the start of parameter list", left))?;
    
            // Consume paarameter list
            // TODO: Underscore to add unnamed parameter
            if self.current.kind != right {
                parameters.push(self.consume_parameter()?);
                
                while self.current.kind == TokenKind::Comma {
                    self.consume_here();
                    parameters.push(self.consume_parameter()?);
                }
            }
            
            self.consume(right, &format!("Expect '{:?}' after function parameter list", right))?;
            Some(parameters)
        } else {None};

        let body = if self.current.kind == TokenKind::Equal {
            let token = self.current.clone();
            self.consume_here();
            Ast::new_return(token, Some(self.expression()?))
        } else {
            self.body()?
        };

        Ok((parameters, body))
    }

    fn anon_function(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
        let (parameters, body) = self.collect_params_and_body(TokenKind::Pipe, TokenKind::Pipe)?;

        Ok(Ast::new_function(token, true, parameters, body))
    }

    fn include(&mut self) -> Result<Box<Ast>, ParserErr> {
        self.consume_here();
        match self.current.kind {
            TokenKind::Identifier | TokenKind::String => {
                let include = if self.current.kind == TokenKind::Identifier {
                    todo!("Import standard library item");
                } else {
                    let include_path = Path::new(&*self.lexer.source.file_path).parent().unwrap();
                    let include_path = include_path.join(self.current.span.slice_source());

                    let include_str = String::from(include_path.to_str().unwrap());

                    if !Path::exists(&include_path) {
                        return Err(self.error(format!(
                            "Include path does not exist '{}'",
                            include_str.clone()
                        )))
                    }

                    let source = fs::read_to_string(&include_path).unwrap();
                    let program = match util::run_parser(include_str.clone(), source, self.script_mode) {
                        Ok((_, program)) => program,
                        Err(e) => return Err(self.error(format!("Could not parse '{}' because {}", include_str, e.message))),
                    };

                    program
                };

                self.consume_here();

                Ok(include)
            },
            _ => Err(self.error(format!(
                "Include expected string or identifier, but recieved '{}'",
                self.current.span.slice_source()
            ))),
        }
    }

    fn function(&mut self) -> Result<Box<Ast>, ParserErr> {
        self.consume_here();

        let identifier = self.current.clone();
        self.consume(TokenKind::Identifier, "Expected identifier after 'fn'")?;

        let (parameters, body) = self.collect_params_and_body(TokenKind::LParen, TokenKind::RParen)?;

        Ok(Ast::new_function(identifier, false, parameters, body))
    }

    fn var_declaration(&mut self) -> Result<Box<Ast>, ParserErr> {
        let is_mutable = self.current.kind == TokenKind::Var;
        self.consume_here();

        let identifier = self.current.clone();
        self.consume(TokenKind::Identifier, "Expected identifier after 'var'/'val'")?;

        // Produce the expression
        let mut expr = None;
        if self.current.kind == TokenKind::Equal {
            self.consume_here();
            expr = Some(self.expression()?);
        }

        Ok(Ast::new_var_decl(identifier, is_mutable, expr))
    }

    fn outer_statements(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current.clone();
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
                            self.consume(TokenKind::SemiColon, "Expect ';' after function statement")?;
                        }
                    }
                    statements.push(func);
                }
                TokenKind::Var | TokenKind::Val => {
                    statements.push(self.var_declaration()?);
                    self.consume(TokenKind::SemiColon, "Expect ';' after variable statement")?;
                }
                _ => {
                    if self.script_mode {
                        statements.push(self.statement()?);
                    } else {
                        return Err(self.error(format!(
                            "Unknown item in outer scope {:?}",
                            self.current.kind
                        )));
                    }
                }
            }
        }

        Ok(Ast::new_program(token, Rc::clone(&self.lexer.source), statements))
    }
}