use std::rc::Rc;
use super::token::{Token, TokenKind, Lexeme};
use crate::errors::spruce_error::SpruceError;

pub struct Lexer {
	ip: usize,
	line: usize,
	column: u16,
	source: String,
}

impl Lexer {
	pub fn new(source: String) -> Self {
		Self { ip: 0, line: 1, column: 1, source }
	}

	fn advance(&mut self) {
		self.ip += 1;
		self.column += 1;
	}

	fn advance_line(&mut self) {
		self.ip += 1;
		self.column = 1;
		self.line += 1;
	}

	fn peek(&self) -> char {
		if self.ip >= self.source.len() {
			return '\0';
		}
		self.source.as_bytes()[self.ip] as char
	}

	fn is_identifier(&self) -> bool {
		let ch = self.peek();
		char::is_alphanumeric(ch) || ch == '_'
	}

	fn skip_whitespace(&mut self) {
		while self.ip < self.source.len() {
			match self.source.as_bytes()[self.ip] {
				b' ' | b'\t' | b'\r' => self.advance(),
				b'\n' => self.advance_line(),
				_ => break,
			}
		}
	}

	fn get_char(&mut self, kind: TokenKind) -> Rc<Token> {
		self.advance();
		Rc::new(Token::new(self.line, self.column - 1, kind, Lexeme::Char(self.source.as_bytes()[self.ip-1])))
	}

	fn find_keyword(&self, lexeme: &String) -> Option<TokenKind> {
		match lexeme.as_str() {
			"true" | "false" => Some(TokenKind::Bool),
			_ => None,
		}
	}

	fn get_identifier(&mut self) -> Rc<Token> {
		let start_ip = self.ip;
		let start_col = self.column;
		self.advance();

		while self.ip < self.source.len() && self.is_identifier() {
			self.advance();
		}

		let lexeme = self.source[start_ip..self.ip].to_string();
		let kind = self.find_keyword(&lexeme);

		Rc::new(
			Token::new(
				self.line, start_col, 
				if let Some(key) = kind { key } else { TokenKind::Identifier },
				Lexeme::String(lexeme),
			)
		)
	}

	fn get_string(&mut self) -> Rc<Token> {
		self.advance();

		let start_ip = self.ip;
		let start_col = self.column;

		while self.ip < self.source.len() && self.peek() != '"' {
			if self.source.as_bytes()[self.ip] == b'\n' {
				self.advance_line()
			} else {
				self.advance();
			}
		}

		// Consume final quote
		self.advance();

		Rc::new(Token::new(self.line, start_col, TokenKind::String, Lexeme::String(self.source[start_ip..self.ip-1].to_string())))
	}

	fn get_number(&mut self) -> Result<Rc<Token>, SpruceError> {
		let start_ip = self.ip;
		let start_col = self.column;
		let mut has_decimal = false;
		
		while self.ip < self.source.len() && char::is_digit(self.peek(), 10) {
			self.advance();
			
			if self.peek() == '.' {
				if has_decimal {
					return Err(SpruceError::Lexer(format!("Cannot use multiple decimals in a number {}:{}", self.line, self.column)));
				}

				has_decimal = true;
				self.advance();
			}
		}

		Ok(Rc::new(Token::new(self.line, start_col, TokenKind::Number, Lexeme::Number(self.source[start_ip..self.ip].parse::<f32>().unwrap()))))
	}

	pub fn next(&mut self) -> Result<Rc<Token>, SpruceError> {
		self.skip_whitespace();

		if self.ip >= self.source.len() {
			return Ok(Rc::new(Token::new(self.line, self.column, TokenKind::Eof, Lexeme::None)));
		}

		let current = self.peek();

		// Numbers
		if char::is_digit(current, 10) {
			return self.get_number();
		}

		// Identifiers
		if self.is_identifier() {
			return Ok(self.get_identifier());
		}
		
		
		// Characters
		match current {
			'+' => return Ok(self.get_char(TokenKind::Plus)),
			'-' => return Ok(self.get_char(TokenKind::Minus)),
			'*' => return Ok(self.get_char(TokenKind::Star)),
			'/' => return Ok(self.get_char(TokenKind::Slash)),

			'(' => return Ok(self.get_char(TokenKind::OpenParen)),
			')' => return Ok(self.get_char(TokenKind::CloseParen)),
			'{' => return Ok(self.get_char(TokenKind::OpenCurly)),
			'}' => return Ok(self.get_char(TokenKind::CloseCurly)),
			'[' => return Ok(self.get_char(TokenKind::OpenSquare)),
			']' => return Ok(self.get_char(TokenKind::CloseSquare)),

			'"' => return Ok(self.get_string()),
			_ => {} // Skip to error
		}

		// End of the line
		Err(SpruceError::Lexer(
			format!("Unknown character found '{}' at {}:{}", current, self.line, self.column)
		))
	}
}