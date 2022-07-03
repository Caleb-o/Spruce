#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
	Plus, Minus, Star, Slash,

	Number, String, Bool,
	Colon, ColonColon, Equal,
	Dot, Comma, Semicolon,

	OpenParen, CloseParen,
	OpenCurly, CloseCurly,
	OpenSquare, CloseSquare,

	Identifier,
	Eof,
}

#[derive(Clone, Debug)]
pub enum Lexeme {
	None,
	Char(u8),
	String(String),
}

impl std::fmt::Display for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Lexeme::None => write!(f, "None"),
			Lexeme::Char(c) => write!(f, "{}", *c as char),
			Lexeme::String(s) => write!(f, "{}", *s),
		}
    }
}

#[derive(Clone)]
pub struct Token {
	pub line: usize,
	pub column: u16, // Columns shouldn't be very large
	pub kind: TokenKind,
	pub lexeme: Lexeme,
}

impl Token {
	pub fn new(line: usize, column: u16, kind: TokenKind, lexeme: Lexeme) -> Self {
		Self { line , column, kind, lexeme }
	}
}