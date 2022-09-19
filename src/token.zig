pub const TokenKind = enum(u8) {
    // Operators
    Plus, Minus, Star, Slash,
	Equal, EqualEqual, Bang, Not, NotEqual,
	Greater, GreaterEqual, Less, LessEqual,
	
	SemiColon, Colon, Comma,

    // Braces
	LParen, RParen,
	LSquare, RSquare,
	LCurly, RCurly,
	
	// Literal types
	Int, Float, String, True, False, None,
	
    // Keywords
	If, Else, While, Let, Var,
	Function, Struct, Is, Return,
	
    Identifier,
	
	EndOfFile,
	Error,
};

pub const Span = struct {
   start: usize,
   // Length should never be more than 256 ever (unless it's a meme)
   len: u8,
};

pub const Token = struct {
    kind: TokenKind,
    span: Span,
    line: usize,
    column: u16,
};