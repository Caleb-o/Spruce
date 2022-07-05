use std::rc::Rc;
use crate::lexing::token::Token;

pub struct Body { pub statements: Vec<Box<AST>> }
pub struct Argument { pub label: Rc<Token>, pub expr: Box<AST> }
pub struct FunctionDefinition { pub parameters: Vec<Rc<Token>>, pub returns: Option<Box<AST>>, pub body: Box<AST> }
pub struct FunctionCall { pub caller: Box<AST>, pub arguments: Vec<Box<AST>> }
pub struct ConstDeclaration { pub identifier: String, pub expression: Box<AST> }
pub struct VariableDeclaration { pub identifier: String, pub expression: Box<AST> }
pub struct VariableAssign { pub identifier: String, pub expression: Box<AST> }
pub struct BinOp { pub operator: Rc<Token>, pub left: Box<AST>, pub right: Box<AST> }
pub struct UnaryOp { pub operator: Rc<Token>, pub right: Box<AST> }
pub struct Range { pub left: Box<AST>, pub right: Box<AST> }

pub enum AST {
	Program(Box<AST>),
	Body(Body),

	Argument(Argument),

	FunctionDefinition(FunctionDefinition),
	FunctionCall(FunctionCall),

	ConstDeclaration(ConstDeclaration),
	VariableDeclaration(VariableDeclaration),
	VariableAssign(VariableAssign),

	Identifier(String),
	Number(f32),
	String(String),
	Unset,

	BinOp(BinOp),
	UnaryOp(UnaryOp),
	Range(Range),
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			AST::Program(_) => write!(f, "Program"),
			AST::Body(_) => write!(f, "Body"),
			AST::Argument(arg) => write!(f, "Argument '{}'", arg.label.lexeme),

			AST::FunctionDefinition(_) => write!(f, "Function Definition"),
			AST::FunctionCall(_) => write!(f, "Function Call"),
			AST::ConstDeclaration(decl) => write!(f, "Const Declaration '{}'", decl.identifier),
			AST::VariableDeclaration(decl) => write!(f, "Variable Declaration '{}'", decl.identifier),
			AST::VariableAssign(assign) => write!(f, "Variable Assignment '{}'", assign.identifier),

			AST::Identifier(id) => write!(f, "Identifier '{}'", id),
			AST::Number(n) => write!(f, "Number '{}'", n),
			AST::String(s) => write!(f, "String '{}'", s),
			AST::Unset => write!(f, "Unset"),
			
			AST::BinOp(_) => write!(f, "Binary Op"),
			AST::UnaryOp(_) => write!(f, "Unary Op"),
			AST::Range(_) => write!(f, "Range"),
		}
    }
}

impl AST {
	pub fn to_sexpr(&self) -> String {
		match self {
			AST::Program(b) => {
				b.to_sexpr()
			}
			
			AST::Body(body) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				
				for stmt in body.statements.iter() {
					sexpr.push_str(&stmt.to_sexpr());
				}
				
				sexpr.push(')');
				sexpr
			}

			AST::Argument(arg) => {
				let mut sexpr = String::with_capacity(16);
				
				sexpr.push('(');
				sexpr.push_str(&format!("{}: ", arg.label.lexeme));
				sexpr.push_str(&arg.expr.to_sexpr());
				sexpr.push(')');

				sexpr
			}

			AST::FunctionDefinition(def) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("fn ");
				sexpr.push('(');
				
				for (idx, param) in def.parameters.iter().enumerate() {
					sexpr.push_str(&format!("{}", param.lexeme));
					
					if idx < def.parameters.len() - 1 {
						sexpr.push_str(", ");
					}
				}
				
				sexpr.push(')');

				sexpr.push_str(&def.body.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::FunctionCall(call) => {
				let mut sexpr = String::with_capacity(32);

				sexpr.push('(');
				sexpr.push_str(&format!("call {}(", &call.caller.to_sexpr()));

				for (idx, arg) in call.arguments.iter().enumerate() {
					sexpr.push_str(&arg.to_sexpr());

					if idx < call.arguments.len() - 1 {
						sexpr.push_str(", ");
					}
				}
				sexpr.push(')');
				sexpr
			}

			AST::ConstDeclaration(decl) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("const ");
				sexpr.push_str(&format!("{}", decl.identifier));
				sexpr.push(' ');
				sexpr.push_str(&decl.expression.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::VariableDeclaration(decl) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("var ");
				sexpr.push_str(&format!("{}", decl.identifier));
				sexpr.push(' ');
				sexpr.push_str(&decl.expression.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::VariableAssign(assign) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("var = ");
				sexpr.push_str(&format!("{}", assign.identifier));
				sexpr.push(' ');
				sexpr.push_str(&assign.expression.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::BinOp(op) => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push('(');
				sexpr.push_str(&format!("{}", op.operator.lexeme));
				sexpr.push(' ');
				sexpr.push_str(&op.left.to_sexpr());
				sexpr.push(' ');
				sexpr.push_str(&op.right.to_sexpr());
				sexpr.push(')');

				sexpr
			}

			AST::UnaryOp(op) => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push('(');
				sexpr.push_str(&format!("{}", op.operator.lexeme));
				sexpr.push(' ');
				sexpr.push_str(&op.right.to_sexpr());
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

			AST::Range(range) => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push_str(&range.left.to_sexpr());
				sexpr.push_str("..");
				sexpr.push_str(&range.right.to_sexpr());

				sexpr
			}
		}
	}
}