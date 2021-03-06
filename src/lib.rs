pub mod errors;
pub mod lexing;
pub mod parsing;
pub mod analysis;
pub mod runtime;
use std::fs;

use errors::spruce_error::SpruceError;
use parsing::parser::Parser;
use analysis::analyser::Analyser;
use runtime::interpreter::Interpreter;

pub fn run(filename: String) -> Result<(), SpruceError> {
	let content = fs::read_to_string(filename);

	if let Err(e) = content {
		return Err(SpruceError::General(e.to_string()));
	}

	let mut parser = Parser::new(content.unwrap());
	match parser.parse() {
		Ok(b) => {
			let mut analyser = Analyser::new();
			if !analyser.run(&b) {
				return Err(SpruceError::Analyser("Errors occured".into()));
			}

			Interpreter::new().run(b);
		}

		Err(e) => println!("{e}"),
	}

	Ok(())
}

// -------- TESTS --------

// ---- LEXER

#[test]
fn test_valid_tokens() {
	use lexing::{lexer::Lexer, token::TokenKind};
	let mut lexer = Lexer::new(fs::read_to_string("./testsrc/lexer/valid_tokens.sp").unwrap());
    
	loop {
		match lexer.next() {
			Ok(t) => {
				if t.kind == TokenKind::Eof {
					break;
				}
			}
			Err(e) => panic!("{}", e),
		}
	}
}

// ---- PARSER

#[test]
fn test_range() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/range.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.ast.to_sexpr(), "((var foo 10)(var bar 5)(var range foo..(* bar bar)))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_function_def_and_call() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/function_def_and_call.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.ast.to_sexpr(), "((var foo (fn (a, b)()))(call foo(10, 20))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_variables() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/variable.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.ast.to_sexpr(), "((var bar (+ 20 30))(var = bar 2))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_function_body() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/function_body.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(
			p.ast.to_sexpr(),
			"((var add_three (fn (value)((var add_two (fn (value)((var = value (+ value 2)))))(var = value (+ (call add_two(value) 1)))))(call add_three(bar))"
		),
		Err(e) => panic!("{e}"),
	}
}