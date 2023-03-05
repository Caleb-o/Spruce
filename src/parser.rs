use std::io::Error;

use crate::{token::{Token, TokenKind}, lexer::Lexer, ast::Ast};

pub struct Parser {
    lexer: Lexer,
    current: Token,
    had_error: bool,
}

pub struct ParserErr {
    message: String,
    line: u32,
    column: u32,
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
    
    fn error(&self, msg: String) -> ParserErr {
		ParserErr {
            message: format!(
                    "[\x1b[31mError\x1b[0m] {} [{}:{}]",
                    msg,
                    self.current.line,
                    self.current.column,
                ),
            line: self.current.line,
            column: self.current.column,
        }
	}

	fn error_no_exit(&mut self, msg: String, token: &Token) {
		self.had_error = true;

		println!("{}", format!(
			"[\x1b[31mError\x1b[0m] {} [{}:{}]",
			msg,
			token.line,
			token.column,
		));
	}

	fn warning(&self, msg: String, token: &Token) {
		println!("{}", format!(
			"[\x1b[33mWarning\x1b[0m] {} [{}:{}]",
			msg,
			token.line,
			token.column,
		));
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

			// TokenKind::LSquare => self.list_literal(env),
			// TokenKind::Function => self.anon_function(env),

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
		let node = self.primary()?;

		// while self.current.kind == TokenKind::LSquare {
		// 	self.index(env)?;
		// }

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

	fn expression(&mut self) -> Result<Box<Ast>, ParserErr> {
		self.type_equality()
	}
}