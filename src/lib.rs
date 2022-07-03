pub mod errors;
pub mod lexing;
use std::fs;

use errors::spruce_error::SpruceError;
use lexing::{lexer::Lexer, token::TokenKind};

pub fn run(filename: String) -> Result<(), SpruceError> {
	let content = fs::read_to_string(filename);

	if let Err(e) = content {
		return Err(SpruceError::General(e.to_string()));
	}

	let mut lexer = Lexer::new(content.unwrap());
    
	while let Ok(current) = lexer.next() {
		if current.kind == TokenKind::Eof {
			break;
		}

		println!("{:?} '{}' {}:{}", current.kind, current.lexeme, current.line, current.column);
	}

	Ok(())
}

#[test]
fn valid_tokens() {
	let mut lexer = Lexer::new("+ - * / () {} []".into());
    
	while let Ok(current) = lexer.next() {
		if current.kind == TokenKind::Eof {
			break;
		}
	}
}