#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
	Plus, Minus, Star, Slash,

	Colon, ColonColon, Equal, Bang,
	Dot, DotDot, Comma, Semicolon, Arrow,
	EqualEqual, NotEqual, Greater, GreaterEq,
	Less, LessEq,

	OpenParen, CloseParen,
	OpenCurly, CloseCurly,
	OpenSquare, CloseSquare,
	
	Let,
	Identifier,
	Number, String, Bool,
	Function,
	Eof,
}

#[derive(Clone, Debug)]
pub enum Lexeme {
	None,
	Char(u8),
	String(String),
	Number(f32),
}

impl std::fmt::Display for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Lexeme::None => write!(f, "End of File"),
			Lexeme::Char(c) => write!(f, "{}", *c as char),
			Lexeme::String(s) => write!(f, "{}", *s),
			Lexeme::Number(n) => write!(f, "{}", *n),
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