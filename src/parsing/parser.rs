use std::rc::Rc;
use crate::{lexing::{ lexer::Lexer, token::{Token, TokenKind, Lexeme} }, errors::spruce_error::SpruceError};
use super::ast::AST;

pub struct Parser {
	lexer: Lexer,
	current: Rc<Token>,
}

impl Parser {
	pub fn new(source: String) -> Self {
		let mut lexer = Lexer::new(source);
		let current = lexer.next().unwrap();
		Self { lexer , current }
	}

	pub fn parse(&mut self) -> Result<AST, SpruceError> {
		let mut body = Box::new(AST::Body { statements: vec![] });
		self.top_level_statements(&mut body)?;

		Ok(AST::Program(body))
	}

	fn consume(&mut self, expect: TokenKind, message: &str) -> Result<(), SpruceError> {
		if self.current.kind == expect {
			self.current = self.lexer.next().unwrap();
			Ok(())
		} else {
			Err(SpruceError::Parser(
				format!("{}. Expected kind '{:?}' but received '{:?}' {}:{}",
					message,
					expect,
					self.current.kind,
					self.current.line,
					self.current.column,
				)
			))
		}
	}

	fn primary(&mut self, body: &mut AST) -> Result<Box<AST>, SpruceError> {
		match self.current.kind {
			TokenKind::Number => {
				let inner = if let Lexeme::Number(n) = self.current.lexeme { n } else { unreachable!() };
				self.consume(TokenKind::Number, "Expected number in expression")?;
				
				Ok(Box::new(AST::Number(inner)))
			}

			TokenKind::String => {
				let inner = if let Lexeme::String(s) = self.current.lexeme.clone() { s } else { unreachable!() };
				self.consume(TokenKind::String, "Expected string in expression")?;
				
				Ok(Box::new(AST::String(inner)))
			}

			TokenKind::Function => {
				self.function_definition(body)
			}

			TokenKind::Identifier => {
				let inner = if let Lexeme::String(s) = self.current.lexeme.clone() { s } else { unreachable!() };
				self.consume(TokenKind::Identifier, "Expected identifier in expression")?;

				Ok(Box::new(AST::Identifier(inner)))
			}

			_ => Err(SpruceError::Parser(
				format!("Unexpected item found in expression '{}' {}:{}",
					self.current.lexeme,
					self.current.line,
					self.current.column
				)
			)),
		}
	}

	fn factor(&mut self, body: &mut AST) -> Result<Box<AST>, SpruceError> {
		let mut left = self.primary(body)?;

		loop {
			match self.current.kind {
				TokenKind::Star | TokenKind::Slash => {
					let operator = self.current.clone();
					self.consume(self.current.kind, "Expect * or / in factor binary operation")?;

					let right = self.primary(body)?;
					left = Box::new(AST::BinOp { operator, left, right });
				}

				_ => break,
			}
		}

		Ok(left)
	}

	fn term(&mut self, body: &mut AST) -> Result<Box<AST>, SpruceError> {
		let mut left = self.factor(body)?;

		loop {
			match self.current.kind {
				TokenKind::Plus | TokenKind::Minus => {
					let operator = self.current.clone();
					self.consume(self.current.kind, "Expect + or - in term binary operation")?;

					let right = self.factor(body)?;
					left = Box::new(AST::BinOp { operator, left, right });
				}

				_ => break,
			}
		}

		Ok(left)
	}

	fn expression(&mut self, body: &mut AST) -> Result<Box<AST>, SpruceError> {
		self.term(body)
	}

	fn statement(&mut self, body: &mut AST) -> Result<Box<AST>, SpruceError> {
		let expr = self.expression(body)?;
		self.consume(TokenKind::Semicolon, "Expect ';' after statement")?;

		Ok(expr)
	}

	fn function_definition(&mut self, mut _body: &AST) -> Result<Box<AST>, SpruceError> {
		self.consume(TokenKind::Function, "Expect 'fn' in function definition")?;

		self.consume(TokenKind::OpenParen, "Expect '(' after function keyword")?;
		self.consume(TokenKind::CloseParen, "Expect ')' after function arguments")?;
		
		self.consume(TokenKind::OpenCurly, "Expect '{' after function arguments")?;
		self.consume(TokenKind::CloseCurly, "Expect '}' after function body")?;

		// FIXME
		Ok(Box::new(
			AST::FunctionDefinition { 
				parameters: vec![],
				returns: None,
				body: Box::new(AST::Body { statements: vec![] })
			}
		))
	}

	fn const_declaration(&mut self, identifier: Rc<Token>, body: &mut AST) -> Result<(), SpruceError> {
		self.consume(TokenKind::ColonColon, "Expect '::' after identifier constant")?;
		let expr = self.statement(body)?;

		match body {
    		AST::Body { statements } => {
			    statements.push(Box::new(AST::ConstDeclaration { identifier: identifier.clone(), expression: expr }));
		    }
			_ => {},
		}

		Ok(())
	}

	fn top_level_statements(&mut self, body: &mut AST) -> Result<(), SpruceError> {
		while self.current.kind != TokenKind::Eof {
			match self.current.kind {
				TokenKind::Identifier => {
					let id = self.current.clone();
					self.consume(TokenKind::Identifier, "Expected identifier to start top-level statement")?;
					
					// TODO: Allow assignments, fn calls etc
					if self.current.kind == TokenKind::ColonColon {
						self.const_declaration(id, body)?;
					}
				},

				_ => return Err(SpruceError::Parser(
					format!("Unexpected item found in top-level '{}' {}:{}",
						self.current.lexeme,
						self.current.line,
						self.current.column
					)
				)),
			}
		}

		Ok(())
	}
}