use std::collections::HashMap;

use crate::parsing::ast::{Node, AST, Body, VariableDeclaration, Println, FunctionDefinition, FunctionCall};

use super::{Value, FnValue};

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

	fn find_value(&self, key: &String) -> Value {
		for scope in self.variables.iter().rev() {
			if scope.contains_key(key) {
				return (*scope.get(key).unwrap()).clone();
			}
		}

		Value::Unit
	}

	fn visit(&mut self, node: &Node) -> Value {
		match *node.ast {
			AST::Body(ref b) => self.visit_body(b),
			AST::Argument(_) => todo!(),
			AST::FunctionDefinition(ref fndef) => self.visit_fn_def(fndef),
			AST::FunctionCall(ref call) => self.visit_call(call),
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

	fn visit_fn_def(&mut self, fndef: &FunctionDefinition) -> Value {
		Value::Function(FnValue {
			definition: fndef.clone(),
			body: fndef.body.clone(),
		})
	}

	fn visit_call(&mut self, call: &FunctionCall) -> Value {
		let caller = self.visit(&call.caller);

		if let Value::Function(ref func) = caller {
			self.variables.push(HashMap::new());

			for (idx, arg) in call.arguments.iter().enumerate() {
				let value = self.visit(arg);
				
				self.variables.last_mut().unwrap().insert(
					func.definition.parameters[idx].lexeme.to_string(),
					value
				);
			}

			let ret_value = self.visit(&func.body);
			self.variables.pop();

			return ret_value;
		}

		Value::Unit
	}

	fn visit_var_decl(&mut self, decl: &VariableDeclaration) -> Value {
		let expr_value = self.visit(&decl.expression);
		self.variables.last_mut().unwrap().insert(decl.identifier.clone(), expr_value);

		Value::Unit
	}

	fn visit_identifier(&mut self, id: &String) -> Value {
		self.find_value(id)
	}

	fn visit_print(&mut self, print_line: &Println) -> Value {
		for expr in print_line.expressions.iter() {
			let expr_value = self.visit(&expr);
			print!("{expr_value}");
		}
		println!();

		Value::Unit
	}
}