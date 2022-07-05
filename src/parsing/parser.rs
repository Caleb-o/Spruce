use std::rc::Rc;
use crate::{lexing::{ lexer::Lexer, token::{Token, TokenKind, Lexeme} }, errors::spruce_error::SpruceError};
use super::ast::{AST, Body, FunctionDefinition, FunctionCall, ConstDeclaration, VariableDeclaration, VariableAssign, BinOp, UnaryOp, Range, Node };

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
		let mut body = Body { statements: vec![] };
		self.collect_statements(&mut body, TokenKind::Eof)?;

		Ok(AST::Program(Box::new(AST::Body(body))))
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

	fn insert(body: &mut Body, node: Box<AST>) {
		body.statements.push(node);
	}

	fn primary(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let current = self.current.clone();

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
				self.function_definition(body)
			}

			TokenKind::Identifier => {
				let inner = if let Lexeme::String(s) = self.current.lexeme.clone() { s } else { unreachable!() };
				self.consume(TokenKind::Identifier, "Expected identifier in expression")?;

				Ok(Node::new(current, AST::Identifier(inner)))
			}

			TokenKind::Unset => {
				self.consume(TokenKind::Unset, "Expect unset in expression")?;
				Ok(Node::new(current, AST::Unset))
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
				let operator = self.current.clone();
				self.consume(self.current.kind, "Expect ! or - in unary operation")?;

				let right = self.call(body)?;
				return Ok(Node::new(self.current.clone(), AST::UnaryOp(UnaryOp { operator, right: right.ast })));
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
					let operator = self.current.clone();
					self.consume(self.current.kind, "Expect * or / in factor binary operation")?;

					let right = self.unary(body)?;
					left = Node::new(self.current.clone(), AST::BinOp(BinOp { operator, left: left.ast, right: right.ast }));
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
					let operator = self.current.clone();
					self.consume(self.current.kind, "Expect + or - in term binary operation")?;

					let right = self.factor(body)?;
					left = Node::new(self.current.clone(), AST::BinOp(BinOp { operator, left: left.ast, right: right.ast }));
				}

				_ => break,
			}
		}

		Ok(left)
	}

	fn assignment(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let left = self.term(body)?;

		match self.current.kind {
			TokenKind::ColonColon => return self.const_declaration(left, body),
			TokenKind::Walrus => return self.variable(left.ast, body, true),
			TokenKind::Equal => return self.variable(left.ast, body, false),

			_ => {}
		}

		Ok(left)
	}

	fn range(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let left = self.assignment(body)?;

		if self.current.kind == TokenKind::DotDot {
			self.consume(TokenKind::DotDot, "Expect '..' in range")?;
			let right = self.assignment(body)?;
			return Ok(Node::new(self.current.clone(), AST::Range(Range { left: left.ast, right: right.ast })));
		}

		Ok(left)
	}

	fn expression(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		self.range(body)
	}

	fn statement(&mut self, body: &mut Body) -> Result<Node, SpruceError> {
		let expr = self.expression(body)?;
		self.consume(TokenKind::Semicolon, "Expect ';' after statement")?;

		Ok(expr)
	}

	fn get_arguments(&mut self, body: &mut Body) -> Result<Vec<Box<AST>>, SpruceError> {
		let mut args: Vec<Box<AST>> = Vec::new();

		while self.current.kind != TokenKind::CloseParen {
			args.push(self.expression(body)?.ast);

			if self.current.kind != TokenKind::CloseParen {
				self.consume(TokenKind::Comma, "Expect ',' after argument")?;
			}
		}

		Ok(args)
	}

	fn get_parameters(&mut self) -> Result<Vec<Rc<Token>>, SpruceError> {
		let mut params: Vec<Rc<Token>> = Vec::new();

		while self.current.kind != TokenKind::CloseParen {
			params.push(self.current.clone());
			self.consume(TokenKind::Identifier, "Expect identifier in parameter list")?;

			if self.current.kind != TokenKind::CloseParen {
				self.consume(TokenKind::Comma, "Expect ',' after parameter")?;
			}
		}

		Ok(params)
	}

	fn body(&mut self) -> Result<Node, SpruceError> {
		let current = self.current.clone();
		self.consume(TokenKind::OpenCurly, "Expect '{' to start body")?;

		let mut body = Body { statements: vec![] };
		self.collect_statements(&mut body, TokenKind::CloseCurly)?;

		self.consume(TokenKind::CloseCurly, "Expect '}' after body")?;

		Ok(Node::new(current, AST::Body(body)))
	}

	fn function_definition(&mut self, mut _body: &Body) -> Result<Node, SpruceError> {
		self.consume(TokenKind::Function, "Expect 'fn' in function definition")?;

		self.consume(TokenKind::OpenParen, "Expect '(' after function keyword")?;
		let parameters = self.get_parameters()?;
		self.consume(TokenKind::CloseParen, "Expect ')' after function arguments")?;

		let body = self.body()?;

		// FIXME
		Ok(Node::new(
			self.current.clone(),
			AST::FunctionDefinition(FunctionDefinition { 
				parameters,
				returns: None,
				body: body.ast,
			}
		)))
	}

	fn function_call(&mut self, body: &mut Body, caller: Node) -> Result<Node, SpruceError> {
		self.consume(TokenKind::OpenParen, "Expect '(' to start function call")?;
		let arguments = self.get_arguments(body)?;
		self.consume(TokenKind::CloseParen, "Expect ')' after function call arguments")?;

		Ok(Node::new(self.current.clone(), AST::FunctionCall(FunctionCall { caller: caller.ast, arguments })))
	}

	fn const_declaration(&mut self, identifier: Node, body: &mut Body) -> Result<Node, SpruceError> {
		self.consume(TokenKind::ColonColon, "Expect '::' after identifier constant")?;
		let expr = self.expression(body)?;

		if let AST::Identifier(id) = *identifier.ast {
			return Ok(Node::new(
				self.current.clone(),
				AST::ConstDeclaration(ConstDeclaration { 
					identifier: id.clone(),
					expression: expr.ast,
				})
			));
		}

		Err(SpruceError::Parser(
			format!("Constant declaration requires an identifier, but received '{}' {}:{}",
				identifier.ast,
				self.current.line,
				self.current.column,
			)
		))
	}

	fn variable(&mut self, identifier: Box<AST>, body: &mut Body, declare: bool) -> Result<Node, SpruceError> {
		if declare {
			self.consume(TokenKind::Walrus, "Expect ':=' after identifier")?;
		} else {
			self.consume(TokenKind::Equal, "Expect '=' after identifier")?;
		}

		let expr = self.expression(body)?;

		if let AST::Identifier(id) = *identifier {
			if declare {
				return Ok(Node::new(self.current.clone(), AST::VariableDeclaration(VariableDeclaration { identifier: id.clone(), expression: expr.ast })));
			} else {
				return Ok(Node::new(self.current.clone(), AST::VariableAssign(VariableAssign { identifier: id.clone(), expression: expr.ast })));
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
			Parser::insert(body, stmt.ast);
		}

		Ok(())
	}
}