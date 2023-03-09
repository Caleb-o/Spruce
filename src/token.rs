use std::rc::Rc;

use crate::source::Source;

#[derive(Debug, Clone)]
pub struct Span {
	pub start: usize,
	pub len: usize,
	pub source: Rc<Source>,
}

impl Span {
	pub fn new(start: usize, len: usize, source: Rc<Source>) -> Self {
		Span {
			start,
			len,
			source,
		}
	}
	
	pub fn slice_source<'a>(&'a self) -> &'a str {
		&self.source.content[self.start..self.start + self.len]
	}
	
	pub fn compare(&self, other: &Span, source: &str) -> bool {
		let aend = self.start + self.len;
		let bend = other.start + other.len;

		if aend >= source.len() || bend >= source.len() {
			return false;
		}

		self.slice_source() == other.slice_source()
	}

	pub fn compare_str(&self, string: &str, source: &str) -> bool {
		let aend = self.start + self.len;

		if aend >= source.len() || string.len() >= source.len() {
			return false;
		}

		self.slice_source() == string
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
#[allow(unused)]
pub enum TokenKind {
	Plus, Minus, Star, Slash,
	Equal, EqualEqual, Bang, Not, NotEqual,
	Greater, GreaterEqual, Less, LessEqual,
	
	SemiColon, Colon, Comma,

	LParen, RParen,
	LSquare, RSquare,
	LCurly, RCurly,
	
	// Literal types
	Number, String, True, False, None,
	
	If, Else, For, Val, Var,
	Do, While,
	Function, Struct, Is, Return,
	Ensure, Identifier,
	
	EndOfFile,
	Error,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub span: Span,
	pub kind: TokenKind,
	pub line: u32,
	pub column: u16,
}