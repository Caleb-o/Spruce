use std::collections::HashMap;

use crate::parsing::ast::{Node, AST, Body, VariableDeclaration, Println};

use super::Value;

pub struct Interpreter {
	variables: Vec<HashMap<String, Value>>,
}

impl Interpreter {
	pub fn new() -> Self {
		Self { variables: Vec::new() }
	}

	pub fn run(&mut self, root: Node) {
		let _ = self.visit(&root);
	}

	fn visit(&mut self, node: &Node) -> Value {
		match *node.ast {
			AST::Body(ref b) => self.visit_body(b),
			AST::Argument(_) => todo!(),
			AST::FunctionDefinition(_) => todo!(),
			AST::FunctionCall(_) => todo!(),
			AST::VariableDeclaration(ref decl) => self.visit_var_decl(decl),
			AST::VariableAssign(_) => todo!(),
			AST::Identifier(ref id) => self.visit_identifier(id),
			AST::Number(num) => Value::Number(num),
			AST::String(ref string) => Value::String(string.clone()),
			AST::BinOp(_) => todo!(),
			AST::UnaryOp(_) => todo!(),
			AST::Range(_) => todo!(),
			AST::Println(ref print) => self.visit_print(print),
		}
	}

	fn visit_body(&mut self, body: &Body) -> Value {
		self.variables.push(HashMap::new());

		for node in body.statements.iter() {
			self.visit(node);
		}

		self.variables.pop();

		Value::Unit
	}

	fn visit_var_decl(&mut self, decl: &VariableDeclaration) -> Value {
		let expr_value = self.visit(&decl.expression);
		self.variables.last_mut().unwrap().insert(decl.identifier.clone(), expr_value);

		Value::Unit
	}

	fn visit_identifier(&mut self, id: &String) -> Value {
		for scope in self.variables.iter().rev() {
			if scope.contains_key(id) {
				return (*scope.get(id).unwrap()).clone();
			}
		}

		Value::Unit
	}

	fn visit_print(&mut self, print_line: &Println) -> Value {
		for expr in print_line.expressions.iter() {
			let expr_value = self.visit(&expr);
			print!("{expr_value}");
		}

		Value::Unit
	}
}