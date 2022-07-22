use std::rc::Rc;
use crate::lexing::token::Token;

#[derive(Clone)]
pub struct Node {
	pub token: Rc<Token>,
	pub ast: Rc<AST>,
}

impl Node {
	pub fn new(token: Rc<Token>, ast: AST) -> Self {
		Self { token, ast: Rc::new(ast) }
	}
}

pub struct Body { pub statements: Vec<Node> }
pub struct Argument { pub label: Rc<Token>, pub expr: Node }
#[derive(Clone)]
pub struct FunctionDefinition { pub parameters: Vec<Rc<Token>>, pub returns: Option<Node>, pub body: Node }
pub struct FunctionCall { pub caller: Node, pub arguments: Vec<Node> }
pub struct VariableDeclaration { pub identifier: String, pub expression: Node }
pub struct VariableAssign { pub identifier: String, pub expression: Node }
pub struct BinOp { pub operator: Rc<Token>, pub left: Node, pub right: Node }
pub struct UnaryOp { pub operator: Rc<Token>, pub right: Node }
pub struct Range { pub left: Node, pub right: Node }
pub struct Println { pub expressions: Vec<Node> }

pub enum AST {
	Body(Body),

	Argument(Argument),

	FunctionDefinition(FunctionDefinition),
	FunctionCall(FunctionCall),

	VariableDeclaration(VariableDeclaration),
	VariableAssign(VariableAssign),

	Identifier(String),
	Number(f32),
	String(String),

	BinOp(BinOp),
	UnaryOp(UnaryOp),
	Range(Range),

	Println(Println),
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			AST::Body(_) => write!(f, "Body"),
			AST::Argument(arg) => write!(f, "Argument '{}'", arg.label.lexeme),

			AST::FunctionDefinition(_) => write!(f, "Function Definition"),
			AST::FunctionCall(_) => write!(f, "Function Call"),
			AST::VariableDeclaration(decl) => write!(f, "Variable Declaration '{}'", decl.identifier),
			AST::VariableAssign(assign) => write!(f, "Variable Assignment '{}'", assign.identifier),

			AST::Identifier(id) => write!(f, "Identifier '{}'", id),
			AST::Number(n) => write!(f, "Number '{}'", n),
			AST::String(s) => write!(f, "String '{}'", s),
			
			AST::BinOp(_) => write!(f, "Binary Op"),
			AST::UnaryOp(_) => write!(f, "Unary Op"),
			AST::Range(_) => write!(f, "Range"),

			AST::Println(_) => write!(f, "Print"),
		}
    }
}

impl AST {
	pub fn to_sexpr(&self) -> String {
		match self {
			AST::Body(body) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				
				for stmt in body.statements.iter() {
					sexpr.push_str(&stmt.ast.to_sexpr());
				}
				
				sexpr.push(')');
				sexpr
			}

			AST::Argument(arg) => {
				arg.expr.ast.to_sexpr()
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

				sexpr.push_str(&def.body.ast.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::FunctionCall(call) => {
				let mut sexpr = String::with_capacity(32);

				sexpr.push('(');
				sexpr.push_str(&format!("call {}(", &call.caller.ast.to_sexpr()));

				for (idx, arg) in call.arguments.iter().enumerate() {
					sexpr.push_str(&arg.ast.to_sexpr());

					if idx < call.arguments.len() - 1 {
						sexpr.push_str(", ");
					}
				}
				sexpr.push(')');
				sexpr
			}

			AST::VariableDeclaration(decl) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("var ");
				sexpr.push_str(&format!("{}", decl.identifier));
				sexpr.push(' ');
				sexpr.push_str(&decl.expression.ast.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::VariableAssign(assign) => {
				let mut sexpr = String::with_capacity(64);

				sexpr.push('(');
				sexpr.push_str("var = ");
				sexpr.push_str(&format!("{}", assign.identifier));
				sexpr.push(' ');
				sexpr.push_str(&assign.expression.ast.to_sexpr());
				sexpr.push(')');
				sexpr
			}

			AST::BinOp(op) => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push('(');
				sexpr.push_str(&format!("{}", op.operator.lexeme));
				sexpr.push(' ');
				sexpr.push_str(&op.left.ast.to_sexpr());
				sexpr.push(' ');
				sexpr.push_str(&op.right.ast.to_sexpr());
				sexpr.push(')');

				sexpr
			}

			AST::UnaryOp(op) => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push('(');
				sexpr.push_str(&format!("{}", op.operator.lexeme));
				sexpr.push(' ');
				sexpr.push_str(&op.right.ast.to_sexpr());
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

			AST::Range(range) => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push_str(&range.left.ast.to_sexpr());
				sexpr.push_str("..");
				sexpr.push_str(&range.right.ast.to_sexpr());

				sexpr
			}

			AST::Println(println) => {
				let mut sexpr = String::with_capacity(16);

				sexpr.push_str("println (");
				for (idx, expr) in println.expressions.iter().enumerate() {
					sexpr.push_str(&expr.ast.to_sexpr());
					
					if idx < println.expressions.len() - 1 {
						sexpr.push_str(", ");
					}
				}
				sexpr.push(')');

				sexpr
			}
		}
	}
}