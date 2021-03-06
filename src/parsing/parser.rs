use std::rc::Rc;
use crate::{lexing::{ lexer::Lexer, token::{Token, TokenKind, Lexeme} }, errors::spruce_error::SpruceError};
use super::ast::{AST, Body, FunctionDefinition, FunctionCall, VariableDeclaration, VariableAssign, BinOp, UnaryOp, Range, Node, Println, Argument };

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

	pub fn parse(&mut self) -> Result<Node, SpruceError> {
		let mut body = Body { statements: vec![] };
		self.collect_statements(&mut body, TokenKind::Eof)?;

		Ok(Node::new(Rc::new(Token::new(0, 0, TokenKind::Eof, Lexeme::None)), AST::Body(body)))
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

	fn insert(body: &mut Body, node: Node) {
		body.statements.push(node);
	}

	fn primary(&mut self, _body: &mut Body) -> Result<Node, SpruceError> {
		let current = Rc::clone(&self.current);

		match self.current.kind {
			TokenKind::Number => {
				let inner = if let Lexeme::Number(n) = current.lexeme { n } else { unreachable!() };
				self.consume(TokenKind::Number, "Expected number in expression")?;
				
				Ok(Node::new(current, AST::Number(inner)))
			}

			TokenKind::String => {
				let inner = if let Lexeme::String(s) = current.lexeme.clone() { s } else { unreachable!() };
				self.consume(TokenKind::String, "Expected string in expression")?;
				
				Ok(Node::new(current, AST::String(inner)))
			}

			TokenKind::Function => {
				self.function_definition()
			}

			TokenKind::Identifier => {
				let inner = if let Lexeme::String(s) = self.current.lexeme.clone() { s } else { unreachable!() };
				self.consume(TokenKind::Identifier, "Expected identifier in expression")?;

				Ok(Node::new(current, AST::Identifier(inner)))
			}

			TokenKind::OpenCurly => {
				Ok(self.body()?)
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

	fn call(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let mut left = self.primary(body)?;

		loop {
			match self.current.kind {
				TokenKind::OpenParen => {
					left = self.function_call(body, left)?;
				}
				
				_ => break,
			}
		}

		Ok(left)
	}

	fn unary(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		match self.current.kind {
			TokenKind::Minus | TokenKind::Bang => {
				let operator = Rc::clone(&self.current);
				self.consume(self.current.kind, "Expect ! or - in unary operation")?;

				let right = self.call(body)?;
				return Ok(Node::new(Rc::clone(&self.current), AST::UnaryOp(UnaryOp { operator, right })));
			}

			_ => {},
		}
		
		self.call(body)
	}

	fn factor(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let mut left = self.unary(body)?;

		loop {
			match self.current.kind {
				TokenKind::Star | TokenKind::Slash => {
					let operator = Rc::clone(&self.current);
					self.consume(self.current.kind, "Expect * or / in factor binary operation")?;

					let right = self.unary(body)?;
					left = Node::new(Rc::clone(&self.current), AST::BinOp(BinOp { operator, left, right }));
				}

				_ => break,
			}
		}

		Ok(left)
	}

	fn term(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let mut left = self.factor(body)?;

		loop {
			match self.current.kind {
				TokenKind::Plus | TokenKind::Minus => {
					let operator = Rc::clone(&self.current);
					self.consume(self.current.kind, "Expect + or - in term binary operation")?;

					let right = self.factor(body)?;
					left = Node::new(Rc::clone(&self.current), AST::BinOp(BinOp { operator, left, right }));
				}

				_ => break,
			}
		}

		Ok(left)
	}

	fn assignment(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let left = self.term(body)?;

		if self.current.kind == TokenKind::Equal {
			return self.variable(Rc::clone(&left.ast), body, false);
		}

		Ok(left)
	}

	fn range(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let left = self.assignment(body)?;

		if self.current.kind == TokenKind::DotDot {
			self.consume(TokenKind::DotDot, "Expect '..' in range")?;
			let right = self.assignment(body)?;
			return Ok(Node::new(Rc::clone(&self.current), AST::Range(Range { left, right })));
		}

		Ok(left)
	}

	fn expression(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		self.range(body)
	}

	fn statement(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		match self.current.kind {
			TokenKind::Let => {
				self.consume(TokenKind::Let, "Expect let to start variable declaration")?;

				let identifier = Rc::clone(&self.current);
				self.consume(TokenKind::Identifier, "Expect identifier after let")?;
				self.consume(TokenKind::Equal, "Expect '=' after identifier in let binding")?;

				let expr = self.expression(body)?;
				self.consume(TokenKind::Semicolon, "Expect ';' after statement")?;

				Ok(
					Node::new(Rc::clone(&identifier), AST::VariableDeclaration(
						VariableDeclaration {
							identifier: identifier.lexeme.to_string(),
							expression: expr,
						}
					))
				)
			}


			TokenKind::Println => {
				let token = Rc::clone(&self.current);
				self.consume(TokenKind::Println, "Expect println")?;

				self.consume(TokenKind::OpenParen, "Expect '(' after println")?;
				let expressions = self.get_arguments(body)?;
				self.consume(TokenKind::CloseParen, "Expect ')' after println arguments")?;
				self.consume(TokenKind::Semicolon, "Expect ';' after statement")?;

				Ok(
					Node::new(token, AST::Println(
						Println { expressions }
					))
				)
			}

			_ => {
				// Expression statement
				let expr = self.expression(body)?;
				self.consume(TokenKind::Semicolon, "Expect ';' after statement")?;

				Ok(expr)
			}
		}
	}

	fn get_arguments(&mut self, body: &mut Body) -> Result<Vec<Node>, SpruceError> {
		let mut args: Vec<Node> = Vec::new();

		while self.current.kind != TokenKind::CloseParen {
			args.push(Node::new(
				Rc::clone(&self.current),
				AST::Argument(
					Argument {
						label: Rc::clone(&self.current),
						expr: self.expression(body)?
					}
				)
			));

			if self.current.kind != TokenKind::CloseParen {
				self.consume(TokenKind::Comma, "Expect ',' after argument")?;
			}
		}

		Ok(args)
	}

	fn get_parameters(&mut self) -> Result<Vec<Rc<Token>>, SpruceError> {
		let mut params: Vec<Rc<Token>> = Vec::new();

		while self.current.kind != TokenKind::CloseParen {
			params.push(Rc::clone(&self.current));
			self.consume(TokenKind::Identifier, "Expect identifier in parameter list")?;

			if self.current.kind != TokenKind::CloseParen {
				self.consume(TokenKind::Comma, "Expect ',' after parameter")?;
			}
		}

		Ok(params)
	}

	fn body(&mut self) -> Result<Node, SpruceError> {
		let current = Rc::clone(&self.current);
		self.consume(TokenKind::OpenCurly, "Expect '{' to start body")?;

		let mut body = Body { statements: vec![] };
		self.collect_statements(&mut body, TokenKind::CloseCurly)?;

		self.consume(TokenKind::CloseCurly, "Expect '}' after body")?;

		Ok(Node::new(current, AST::Body(body)))
	}

	fn function_definition(&mut self) -> Result<Node, SpruceError> {
		self.consume(TokenKind::Function, "Expect 'fn' in function definition")?;

		self.consume(TokenKind::OpenParen, "Expect '(' after function keyword")?;
		let parameters = self.get_parameters()?;
		self.consume(TokenKind::CloseParen, "Expect ')' after function arguments")?;

		if self.current.kind == TokenKind::FatArrow {
			// Single-line, partial bodied functions
			let arrow = Rc::clone(&self.current);
			self.consume(TokenKind::FatArrow, "Expect '=>' for single-line or partial bodied functions")?;

			let mut body = Body { statements: Vec::new() };
			let stmt = self.statement(&mut body)?;
			Parser::insert(&mut body, stmt);

			while self.current.kind == TokenKind::Pipe {
				self.consume(TokenKind::Pipe, "Expect pipe for partial bodied functions")?;
				let stmt = self.statement(&mut body)?;
				Parser::insert(&mut body, stmt);
			}

			Ok(Node::new(
				Rc::clone(&arrow),
				AST::FunctionDefinition(FunctionDefinition { 
					parameters,
					returns: None,
					body: Node::new(Rc::clone(&arrow), AST::Body(body)),
				}
			)))
		} else {
			// Body
			Ok(Node::new(
				Rc::clone(&self.current),
				AST::FunctionDefinition(FunctionDefinition { 
					parameters,
					returns: None,
					body: self.body()?,
				}
			)))
		}
	}

	fn function_call(&mut self, body: &mut Body, caller: Node) -> Result<Node, SpruceError> {
		self.consume(TokenKind::OpenParen, "Expect '(' to start function call")?;
		let arguments = self.get_arguments(body)?;
		self.consume(TokenKind::CloseParen, "Expect ')' after function call arguments")?;

		Ok(Node::new(Rc::clone(&self.current), AST::FunctionCall(FunctionCall { caller, arguments })))
	}

	fn variable(&mut self, identifier: Rc<AST>, body: &mut Body, declare: bool) -> Result<Node, SpruceError> {
		self.consume(TokenKind::Equal, "Expect '=' after identifier")?;

		let expression = self.expression(body)?;

		if let AST::Identifier(id) = &*identifier {
			return if declare {
				Ok(Node::new(Rc::clone(&self.current), AST::VariableDeclaration(VariableDeclaration { identifier: id.clone(), expression })))
			} else {
				Ok(Node::new(Rc::clone(&self.current), AST::VariableAssign(VariableAssign { identifier: id.clone(), expression })))
			}
		}

		Err(SpruceError::Parser(
			format!("Variable declaration requires an identifier, but received '{}' {}:{}",
				identifier,
				self.current.line,
				self.current.column,
			)
		))
	}

	fn collect_statements(&mut self, body: &mut Body, end: TokenKind) -> Result<(), SpruceError> {
		while self.current.kind != end {
			let stmt = self.statement(body)?;
			Parser::insert(body, stmt);
		}

		Ok(())
	}
}