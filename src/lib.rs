pub mod errors;
pub mod lexing;
pub mod parsing;
pub mod analysis;
use std::fs;

use errors::spruce_error::SpruceError;
use parsing::parser::Parser;
use analysis::analyser::Analyser;

pub fn run(filename: String) -> Result<(), SpruceError> {
	let content = fs::read_to_string(filename);

	if let Err(e) = content {
		return Err(SpruceError::General(e.to_string()));
	}

	let mut parser = Parser::new(content.unwrap());
	match parser.parse() {
		Ok(p) => {
			let mut analyser = Analyser::new();
			analyser.run(&p);
		}

		Err(e) => println!("{e}"),
	}

	println!("Done!");
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
fn test_constant_basic() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/constant_basic.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.to_sexpr(), "((const foo (fn ()()))(const bar 120)(const baz My String)(const number (+ 100 (* 200 3))))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_constant_operation() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/constant_operation.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.to_sexpr(), "((const foo (+ 200 300))(const bar 120)(const baz (+ bar (* foo (- 1)))))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_constant_range() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/constant_range.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.to_sexpr(), "((const foo 10)(const bar 5)(const range foo..(* bar bar)))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_function_def_and_call() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/function_def_and_call.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.to_sexpr(), "((const foo (fn (a, b)()))(call foo(10, 20))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_variables() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/variable.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(p.to_sexpr(), "((var bar (+ 20 30))(var = bar 2))"),
		Err(e) => panic!("{e}"),
	}
}

#[test]
fn test_function_body() {
	let mut parser = Parser::new(fs::read_to_string("./testsrc/parser/function_body.sp").unwrap());
	match parser.parse() {
		Ok(p) => assert_eq!(
			p.to_sexpr(),
			"((const add_three (fn (value)((const add_two (fn (value)((var = value (+ value 2)))))(var = value (+ (call add_two(value) 1)))))(call add_three(bar))"
		),
		Err(e) => panic!("{e}"),
	}
}