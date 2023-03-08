use std::{io::Error, fmt::Display};

use crate::{token::{Token, TokenKind}, lexer::Lexer, ast::{Ast, AstData}};

pub struct Parser {
    lexer: Lexer,
    current: Token,
    had_error: bool,
}

pub struct ParserErr {
    message: String,
    line: u32,
    column: u16,
}

impl Display for ParserErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [{}:{}]", self.message, self.line, self.column)
    }
}

impl Parser {
    pub fn new(is_file: bool, source: &str) -> Result<Self, Error> {
        let mut lexer = Lexer::new(is_file, source)?;
        let token = lexer.next();

        Ok(Self {
            lexer,
            current: token,
            had_error: false,
        })
    }

    pub fn run(&mut self) -> Result<Box<Ast>, ParserErr> {
        let program = self.outer_statements();
        if self.had_error {
            Err(ParserErr {
                message: "Encountered an error while parsing".into(),
                line: self.current.line,
                column: self.current.column,
            })
        } else {
            program
        }
    }
    
    fn error(&self, msg: String) -> ParserErr {
        ParserErr {
            message: format!("[\x1b[31mError\x1b[0m] {msg}"),
            line: self.current.line,
            column: self.current.column,
        }
    }

    fn consume(&mut self, expected: TokenKind, msg: &'static str) -> Result<(), ParserErr> {
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
                let token = self.current;
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
            TokenKind::Function => self.anon_function(),

            TokenKind::Identifier => {
                let token = self.current;
                self.consume_here();
                Ok(Ast::new_identifier(token))
            },

            _ => Err(self.error(format!(
                "Unexpected instruction found {:?} '{}'",
                self.current.kind,
                self.current.span.slice_from(&self.lexer.source),
            ))),
        }
    }

    fn call(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.primary()?;

        while self.is_any_of(&[TokenKind::LParen]) {
        	node = match self.current.kind {
                TokenKind::LParen => self.function_call(node)?,
                _ => unreachable!(),
            }
        }

        Ok(node)
    }

    fn unary(&mut self) -> Result<Box<Ast>, ParserErr> {
        if self.is_any_of(&[TokenKind::Minus, TokenKind::Bang]) {
            match self.current.kind {
                TokenKind::Minus | TokenKind::Bang => {
                    self.consume_here();
                    return self.call();
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
                    let token = self.current;
                    self.consume_here();
                    node = Ast::new_unary_op(token, self.unary()?);
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
                    let token = self.current;
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
                    let token = self.current;
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
                    let token = self.current;
                    self.consume_here();
                    node = Ast::new_binary_op(token, node, self.comparison()?);
                }
    
                _ => break,
            }
        }

        Ok(node)
    }

    fn type_equality(&mut self) -> Result<Box<Ast>, ParserErr> {
        let node = self.equality()?;

        // if self.is_any_of(&[TokenKind::Is, TokenKind::Ensure]) {
        // 	let is_asrt = self.current.kind == TokenKind::Ensure;
        // 	self.consume_here();
        // 	let type_id = self.current;
        // 	self.consume(TokenKind::Identifier, "Expect identifier after is/ensure")?;
        // 	self.check_valid_type(env, &type_id, is_asrt)?;
        // }

        Ok(node)
    }

    fn assignment(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = self.type_equality()?;

        while self.current.kind == TokenKind::Equal {
            self.consume_here();
            node = Ast::new_var_assign(node.token, node, self.expression()?);
        }

        Ok(node)
    }

    fn expression(&mut self) -> Result<Box<Ast>, ParserErr> {
        self.assignment()
    }

    fn list_literal(&mut self) -> Result<Box<Ast>, ParserErr> {
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

        self.consume(TokenKind::RSquare, "Expect ']' after list literal arguments")?;

        Ok(Ast::new_list_literal(token, values))
    }

    fn function_call(&mut self, lhs: Box<Ast>) -> Result<Box<Ast>, ParserErr> {
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
        Ok(Ast::new_function_call(lhs.token, lhs, arguments))
    }

    fn if_statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current;
        self.consume_here();

        let condition = self.expression()?;
        // TODO: Allow single statements
        let true_body = self.body()?;
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
        let token = self.current;
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

        Ok(Ast::new_for_statement(token, variable, condition, increment, self.body()?))
    }

    fn do_while_statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current;
        self.consume_here();

        let body = self.body()?;

        self.consume(TokenKind::While, "Expect 'while' after do block")?;
        let condition = self.expression()?;

        Ok(Ast::new_do_while_statement(token, body, condition))
    }

    fn return_statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current;
        self.consume_here();
        let mut expression = None;

        if self.current.kind != TokenKind::SemiColon {
            expression = Some(self.expression()?);
        }

        Ok(Ast::new_return(token, expression))
    }

    fn statement(&mut self) -> Result<Box<Ast>, ParserErr> {
        let mut node = match self.current.kind {
            TokenKind::If => self.if_statement()?,
            TokenKind::For => self.for_statement()?,
            TokenKind::Do => self.do_while_statement()?,
            TokenKind::Var | TokenKind::Val => self.var_declaration()?,
            TokenKind::Return => self.return_statement()?,
            _ => Ast::new_expr_statement(self.expression()?),
        };

        // Trailing if statement
        if self.current.kind == TokenKind::If {
            let token = self.current;
            self.consume_here();
            node = Ast::new_trailing_if(token, node, self.expression()?);
        }

        match node.data {
            AstData::IfStatement {..} | AstData::ForStatement {..} => {}
            _ => self.consume(TokenKind::SemiColon, "Expect ';' after statement")?,
        }
        
        Ok(node)
    }

    fn body(&mut self) -> Result<Box<Ast>, ParserErr> {
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
    fn consume_parameter(&mut self) -> Result<Box<Ast>, ParserErr> {
        let param_name = self.current;
        let mut type_name = None;
        self.consume(TokenKind::Identifier, "Expected identifier in parameter list")?;

        if self.current.kind == TokenKind::Colon {
            self.consume_here();
            type_name = Some(self.current);
            self.consume(TokenKind::Identifier, "Expected type name after identifier")?;
        }

        Ok(Ast::new_parameter(param_name, type_name))
    }

    fn anon_function(&mut self) -> Result<Box<Ast>, ParserErr> {
        let token = self.current;
        self.consume_here();

        let parameters = if self.current.kind == TokenKind::LParen {
            let mut parameters = Vec::new();
            self.consume(TokenKind::LParen, "Expect '(' at the start of parameter list")?;

            // Consume paarameter list
            // TODO: Underscore to add unnamed parameter
            if self.current.kind != TokenKind::RParen {
                parameters.push(self.consume_parameter()?);
                
                while self.current.kind == TokenKind::Comma {
                    self.consume_here();
                    parameters.push(self.consume_parameter()?);
                }
            }
            
            self.consume(TokenKind::RParen, "Expect ')' after function parameter list")?;
            
            Some(parameters)
        } else {None};

        Ok(Ast::new_function(token, true, parameters, self.body()?))
    }

    fn function(&mut self) -> Result<Box<Ast>, ParserErr> {
        self.consume_here();

        let identifier = self.current;
        self.consume(TokenKind::Identifier, "Expected identifier after 'fn'")?;

        let parameters = if self.current.kind == TokenKind::LParen {
            let mut parameters = Vec::new();
            self.consume(TokenKind::LParen, "Expect '(' at the start of parameter list")?;
    
            // Consume paarameter list
            // TODO: Underscore to add unnamed parameter
            if self.current.kind != TokenKind::RParen {
                parameters.push(self.consume_parameter()?);
                
                while self.current.kind == TokenKind::Comma {
                    self.consume_here();
                    parameters.push(self.consume_parameter()?);
                }
            }
            
            self.consume(TokenKind::RParen, "Expect ')' after function parameter list")?;
            Some(parameters)
        } else {None};


        Ok(Ast::new_function(identifier, false, parameters, self.body()?))
    }

    fn var_declaration(&mut self) -> Result<Box<Ast>, ParserErr> {
        let is_mutable = self.current.kind == TokenKind::Var;
        self.consume_here();

        let identifier = self.current;
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
        let token = self.current;
        let mut statements = Vec::new();

        while self.current.kind != TokenKind::EndOfFile {
            match self.current.kind {
                TokenKind::Function => statements.push(self.function()?),
                TokenKind::Var | TokenKind::Val => {
                    statements.push(self.var_declaration()?);
                    self.consume(TokenKind::SemiColon, "Expect ';' after statement")?;
                },
                _ => {
                    return Err(self.error(format!(
                        "Unknown item in outer scope {:?}",
                        self.current.kind
                    )));
                }
            }
        }

        Ok(Ast::new_body(token, statements))
    }
}