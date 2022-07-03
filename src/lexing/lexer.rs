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

	pub fn next(&mut self) -> Result<Rc<Token>, SpruceError> {
		self.skip_whitespace();

		if self.ip >= self.source.len() {
			return Ok(Rc::new(Token::new(self.line, self.column, TokenKind::Eof, Lexeme::None)));
		}

		let current = self.source.as_bytes()[self.ip];
		
		match current {
			b'+' => return Ok(self.get_char(TokenKind::Plus)),
			b'-' => return Ok(self.get_char(TokenKind::Minus)),
			b'*' => return Ok(self.get_char(TokenKind::Star)),
			b'/' => return Ok(self.get_char(TokenKind::Slash)),

			b'(' => return Ok(self.get_char(TokenKind::OpenParen)),
			b')' => return Ok(self.get_char(TokenKind::CloseParen)),
			b'{' => return Ok(self.get_char(TokenKind::OpenCurly)),
			b'}' => return Ok(self.get_char(TokenKind::CloseCurly)),
			b'[' => return Ok(self.get_char(TokenKind::OpenSquare)),
			b']' => return Ok(self.get_char(TokenKind::CloseSquare)),
			_ => {} // Skip to error
		}

		// End of the line
		Err(SpruceError::Lexer(
			format!("Unknown character found '{}' at {}:{}", self.source.as_bytes()[self.ip] as char, self.line, self.column)
		))
	}
}