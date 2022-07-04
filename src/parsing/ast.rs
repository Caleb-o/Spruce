use std::rc::Rc;
use crate::lexing::token::Token;

pub enum AST {
	Program(Box<AST>),
	Body { statements: Vec<Box<AST>> },

	FunctionDefinition { parameters: Vec<Box<AST>>, returns: Option<Box<AST>>, body: Box<AST> },

	ConstDeclaration { identifier: Rc<Token>, expression: Box<AST> },

	Identifier(String),
	Number(f32),
	String(String),

	BinOp { operator: Rc<Token>, left: Box<AST>, right: Box<AST> },
	UnaryOp { operator: Rc<Token>, right: Box<AST> },
}

impl AST {
	pub fn to_sexpr(&self) -> String {
		match self {
			AST::Program(b) => {
				b.to_sexpr()
			}
			
			AST::Body { statements } => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				
				for stmt in statements.iter() {
					sexpr.push_str(&stmt.to_sexpr());
				}
				
				sexpr.push(')');
				sexpr
			}

			AST::FunctionDefinition { parameters: _, returns: _, body } => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("fn ");
				sexpr.push_str(&body.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::ConstDeclaration { identifier, expression } => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("const ");
				sexpr.push_str(&format!("{}", identifier.lexeme));
				sexpr.push(' ');
				sexpr.push_str(&expression.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::BinOp { operator, left, right } => {
				let mut sexpr = String::with_capacity(32);

				sexpr.push('(');
				sexpr.push_str(&format!("{}", operator.lexeme));
				sexpr.push(' ');
				sexpr.push_str(&left.to_sexpr());
				sexpr.push(' ');
				sexpr.push_str(&right.to_sexpr());
				sexpr.push(')');

				sexpr
			}

			AST::UnaryOp { operator, right } => {
				let mut sexpr = String::with_capacity(32);

				sexpr.push('(');
				sexpr.push_str(&format!("{}", operator.lexeme));
				sexpr.push(' ');
				sexpr.push_str(&right.to_sexpr());
				sexpr.push(')');

				sexpr
			}

			AST::Number(n) => {
				n.to_string()
			}

			AST::String(s) => {
				s.clone()
			}

			AST::Identifier(i) => {
				i.clone()
			}
		}
	}
}