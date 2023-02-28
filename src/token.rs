#[derive(Debug, Clone, Copy)]
pub struct Span {
	pub start: usize,
	pub len: usize,
}

impl Span {
	pub fn new(start: usize, len: usize) -> Self {
		Span {
			start,
			len,
		}
	}

	pub fn is_underscore(&self, source: &str) -> bool {
		if self.len > 1 {
			return false;
		}

		source.chars().nth(self.start + 1).unwrap() == '_'
	}

	pub fn slice_from<'a>(&'a self, source: &'a str) -> &'a str {
		&source[self.start..self.start + self.len]
	}
	
	pub fn compare(&self, other: &Span, source: &str) -> bool {
		let aend = self.start + self.len;
		let bend = other.start + other.len;

		if aend >= source.len() || bend >= source.len() {
			return false;
		}

		self.slice_from(source) == other.slice_from(source)
	}

	pub fn compare_str(&self, string: &str, source: &str) -> bool {
		let aend = self.start + self.len;

		if aend >= source.len() || string.len() >= source.len() {
			return false;
		}

		self.slice_from(source) == string
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
	Int, Float, String, True, False, None,
	
	If, Else, For, Let, Var,
	Function, Struct, Is, Return,
	Identifier,
	
	EndOfFile,
	Error,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
	pub span: Span,
	pub kind: TokenKind,
	pub line: u64,
	pub column: u32,
}