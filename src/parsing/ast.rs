use std::rc::Rc;
use crate::lexing::token::Token;

pub enum AST {
	Program(Box<AST>),
	Body { statements: Vec<Box<AST>> },

	Argument { label: Rc<Token>, expr: Box<AST> },

	FunctionDefinition { parameters: Vec<Rc<Token>>, returns: Option<Box<AST>>, body: Box<AST> },
	FunctionCall { caller: Box<AST>, arguments: Vec<Box<AST>> },

	ConstDeclaration { identifier: String, expression: Box<AST> },
	VariableDeclaration { identifier: String, expression: Box<AST> },
	VariableAssign { identifier: String, expression: Box<AST> },

	Identifier(String),
	Number(f32),
	String(String),
	Unset,

	BinOp { operator: Rc<Token>, left: Box<AST>, right: Box<AST> },
	UnaryOp { operator: Rc<Token>, right: Box<AST> },
	Range { left: Box<AST>, right: Box<AST> },
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			AST::Program(_) => write!(f, "Program"),
			AST::Body { statements: _ } => write!(f, "Body"),
			AST::Argument { label, expr: _ } => write!(f, "Argument '{}'", label.lexeme),

			AST::FunctionDefinition { parameters: _, returns: _, body: _ } => write!(f, "Function Definition"),
			AST::FunctionCall { caller: _, arguments: _ } => write!(f, "Function Call"),
			AST::ConstDeclaration { identifier, expression: _ } => write!(f, "Const Declaration '{}'", identifier),
			AST::VariableDeclaration { identifier, expression: _ } => write!(f, "Variable Declaration '{}'", identifier),
			AST::VariableAssign { identifier, expression: _ } => write!(f, "Variable Assignment '{}'", identifier),

			AST::Identifier(id) => write!(f, "Identifier '{}'", id),
			AST::Number(n) => write!(f, "Number '{}'", n),
			AST::String(s) => write!(f, "String '{}'", s),
			AST::Unset => write!(f, "Unset"),
			
			AST::BinOp { operator: _, left: _, right: _ } => write!(f, "Binary Op"),
			AST::UnaryOp { operator: _, right: _ } => write!(f, "Unary Op"),
			AST::Range { left: _, right: _ } => write!(f, "Range"),
		}
    }
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

			AST::Argument { label, expr } => {
				let mut sexpr = String::with_capacity(16);
				
				sexpr.push('(');
				sexpr.push_str(&format!("{}: ", label.lexeme));
				sexpr.push_str(&expr.to_sexpr());
				sexpr.push(')');

				sexpr
			}

			AST::FunctionDefinition { parameters, returns: _, body } => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("fn ");
				sexpr.push('(');
				
				for (idx, param) in parameters.iter().enumerate() {
					sexpr.push_str(&format!("{}", param.lexeme));
					
					if idx < parameters.len() - 1 {
						sexpr.push_str(", ");
					}
				}
				
				sexpr.push(')');

				sexpr.push_str(&body.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::FunctionCall { caller, arguments } => {
				let mut sexpr = String::with_capacity(32);

				sexpr.push('(');
				sexpr.push_str(&format!("call {}(", &caller.to_sexpr()));

				for (idx, arg) in arguments.iter().enumerate() {
					sexpr.push_str(&arg.to_sexpr());

					if idx < arguments.len() - 1 {
						sexpr.push_str(", ");
					}
				}
				sexpr.push(')');
				sexpr
			}

			AST::ConstDeclaration { identifier, expression } => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("const ");
				sexpr.push_str(&format!("{}", identifier));
				sexpr.push(' ');
				sexpr.push_str(&expression.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::VariableDeclaration { identifier, expression } => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("var ");
				sexpr.push_str(&format!("{}", identifier));
				sexpr.push(' ');
				sexpr.push_str(&expression.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::VariableAssign { identifier, expression } => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("var = ");
				sexpr.push_str(&format!("{}", identifier));
				sexpr.push(' ');
				sexpr.push_str(&expression.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::BinOp { operator, left, right } => {
				let mut sexpr = String::with_capacity(16);

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
				let mut sexpr = String::with_capacity(16);

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

			AST::Unset => {
				"Unset".into()
			}

			AST::Identifier(i) => {
				i.clone()
			}

			AST::Range { left, right } => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push_str(&left.to_sexpr());
				sexpr.push_str("..");
				sexpr.push_str(&right.to_sexpr());

				sexpr
			}
		}
	}
}