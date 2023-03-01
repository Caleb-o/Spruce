use std::{fs, io::Error};

use crate::token::{Token, TokenKind, Span};

#[derive(Debug)]
pub struct Lexer {
	pub source: String,
	line: u32,
	column: u32,
	pos: usize,
}

impl Lexer {
	pub fn new(filepath: &str) -> Result<Self, Error> {
		let source = fs::read_to_string(filepath)?;
		Ok(Self {
			source,
			line: 1,
			column: 1,
			pos: 0,
		})
	}

	pub fn next(&mut self) -> Token {
		self.skip_whitespace();

		if self.is_at_end() {
			return self.make_token(
				TokenKind::EndOfFile,
				Span::new(self.pos, 0),
				self.column
			);
		}
		
		let current = self.peek();

		if current == '_' || current.is_alphabetic() {
			return self.read_identifier();
		}

		if current.is_numeric() {
			return self.read_numeric();
		}

		match current {
			'"' => self.read_string('"'),
			'\'' => self.read_string('\''),

			'+' => self.make_char_token(TokenKind::Plus),
			'-' => self.make_char_token(TokenKind::Minus),
			'*' => self.make_char_token(TokenKind::Star),
			'/' => self.make_char_token(TokenKind::Slash),

			'=' => self.make_char_token_matches(TokenKind::Equal, '=', TokenKind::EqualEqual),
			'>' => self.make_char_token_matches(TokenKind::Greater, '=', TokenKind::GreaterEqual),
			'<' => self.make_char_token_matches(TokenKind::Less, '=', TokenKind::LessEqual),
			'!' => self.make_char_token_matches(TokenKind::Not, '=', TokenKind::NotEqual),

			'(' => self.make_char_token(TokenKind::LParen),
			')' => self.make_char_token(TokenKind::RParen),
			'[' => self.make_char_token(TokenKind::LSquare),
			']' => self.make_char_token(TokenKind::RSquare),
			'{' => self.make_char_token(TokenKind::LCurly),
			'}' => self.make_char_token(TokenKind::RCurly),

			';' => self.make_char_token(TokenKind::SemiColon),
			':' => self.make_char_token(TokenKind::Colon),
			',' => self.make_char_token(TokenKind::Comma),
			
			_ => self.make_token(
					TokenKind::Error,
					Span::new(self.pos, self.pos),
					self.column
				),
		}
	}

	pub fn peek(&self) -> char {
		match self.is_at_end() {
			true => '\0',
			false => self.source
				.chars()
				.nth(self.pos)
				.unwrap(),
		}
	}

	// Hack to get the next token, then reset state
	pub fn peek_type(&mut self) -> TokenKind {
		let col = self.column;
		let line = self.line;
		let pos = self.pos;

		let token = self.next();

		self.column = col;
		self.line = line;
		self.pos = pos;

		token.kind
	}

	fn make_token(
		&self,
		kind: TokenKind,
		span: Span,
		column: u32,
	) -> Token {
		Token { span, kind, line: self.line, column }
	}

	fn make_char_token(
		&mut self,
		kind: TokenKind,
	) -> Token {
		self.advance();
		Token { span: Span::new(self.pos - 1, 1), kind, line: self.line, column: self.column - 1 }
	}

	fn make_char_token_matches(
		&mut self,
		kind: TokenKind,
		next: char,
		other: TokenKind,
	) -> Token {
		self.advance();

		if self.peek() == next {
			self.advance();
			return Token { span: Span::new(self.pos - 2, 2), kind: other, line: self.line, column: self.column - 2 }
		}

		Token { span: Span::new(self.pos - 1, 1), kind, line: self.line, column: self.column - 1 }
	}

	fn check_if_matches(&self, start:usize, potential: &[(&'static str, TokenKind)]) -> TokenKind {
		let begin = start + 1;

		for (rest, kind) in potential {
			if &self.source[begin..begin + rest.len()] == *rest {
				return *kind;
			}
		}

		TokenKind::Identifier
	}

	fn get_identifier_type(&self, start: usize, len: usize) -> TokenKind {
		let lexeme = &self.source[start..start + len];
		
		// Fixme: chars.nth seems dumb here
		match lexeme.chars().nth(0).unwrap() {
			'e' => self.check_if_matches(start, &[("lse", TokenKind::Else)]),
			'f' => self.check_if_matches(start, &[
				("alse", TokenKind::False),
				("n", TokenKind::Function),
				("or", TokenKind::For),
			]),
			'i' => self.check_if_matches(start, &[
					("f", TokenKind::If),
					("s", TokenKind::Is),
				]),
			'n' => self.check_if_matches(start, &[("one", TokenKind::None)]),
			'r' => self.check_if_matches(start, &[("eturn", TokenKind::Return)]),
			's' => self.check_if_matches(start, &[("ruct", TokenKind::Struct)]),
			't' => self.check_if_matches(start, &[("rue", TokenKind::True)]),
			'v' => self.check_if_matches(start, &[
				("ar", TokenKind::Var),
				("al", TokenKind::Val),
			]),
			_ => TokenKind::Identifier,
		}
	}

	fn read_identifier(&mut self) -> Token {
		let pos = self.pos;
		let column = self.column;
		
		self.advance();

		while !self.is_at_end() && (self.peek() == '_' || self.peek().is_alphanumeric()) {
			self.advance();
		}

		let kind = self.get_identifier_type(pos, self.pos - pos);

		self.make_token(
			kind,
			Span::new(pos, self.pos - pos),
			column,
		)
	}

	fn read_string(&mut self, end: char) -> Token {
		let column = self.column;
		
		self.advance();
		let pos = self.pos;

		while !self.is_at_end() && self.peek() != end {
			self.advance();
		}

		self.advance();

		self.make_token(
			TokenKind::String,
			Span::new(pos, self.pos - pos - 1),
			column,
		)
	}

	fn read_numeric(&mut self) -> Token {
		let pos = self.pos;
		let column = self.column;
		let mut is_float = false;
		
		self.advance();

		if self.peek() == '.' {
			self.advance();
			is_float = true;
		}

		while !self.is_at_end() && self.peek().is_numeric() {
			// TODO: Allow floats
			self.advance();

			if self.peek() == '.' {
				self.advance();
				if is_float {
					return self.make_token(
						TokenKind::Error,
						Span::new(self.pos, self.pos),
						self.column
					);
				}
				is_float = true;
			}
		}

		self.make_token(
			TokenKind::Number,
			Span::new(pos, self.pos - pos),
			column,
		)
	}

	fn is_at_end(&self) -> bool {
		self.pos >= self.source.len()
	}

	fn advance(&mut self) {
		self.pos += 1;
		self.column += 1;
	}

	fn advance_line(&mut self) {
		self.pos += 1;
		self.line += 1;
		self.column = 1;
	}

	fn skip_whitespace(&mut self) {
		while !self.is_at_end() {
			match self.peek() {
				'\n' => self.advance_line(),
				' ' | '\t' | '\r' => self.advance(),

				// Comments
				'#' => {
					self.advance();
					while !self.is_at_end() && self.peek() != '\n' {
						self.advance();
					}
				}

				_ => break,
			}
		}
	}
}