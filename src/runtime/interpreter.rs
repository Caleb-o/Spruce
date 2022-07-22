use std::collections::HashMap;

use crate::parsing::ast::{Node, AST, Body, VariableDeclaration, Println, FunctionDefinition, FunctionCall, VariableAssign};

use super::{Value, FnValue, Callable};

pub struct Interpreter {
	pub(super) variables: Vec<HashMap<String, Value>>,
}

impl Interpreter {
	pub fn new() -> Self {
		Self { variables: Vec::new() }
	}

	pub fn run(&mut self, root: Node) {
		let _ = self.visit(&root);
	}

	pub(super) fn begin(&mut self) {
		self.variables.push(HashMap::new());
	}

	pub(super) fn end(&mut self) {
		self.variables.pop();
	}

	fn find_value(&self, key: &String) -> Value {
		for scope in self.variables.iter().rev() {
			if scope.contains_key(key) {
				return (*scope.get(key).unwrap()).clone();
			}
		}

		Value::Unit
	}

	pub(super) fn visit(&mut self, node: &Node) -> Value {
		match *node.ast {
			AST::Body(ref b) => self.visit_body(b),
			AST::Argument(ref arg) => self.visit(&arg.expr),
			AST::FunctionDefinition(ref fndef) => self.visit_fn_def(fndef),
			AST::FunctionCall(ref call) => self.visit_call(call),
			AST::VariableDeclaration(ref decl) => self.visit_var_decl(decl),
			AST::VariableAssign(ref assign) => self.visit_var_assign(assign),
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
		self.begin();

		for node in body.statements.iter() {
			self.visit(node);
		}

		self.end();

		Value::Unit
	}

	fn visit_fn_def(&mut self, fndef: &FunctionDefinition) -> Value {
		Value::Function(FnValue {
			definition: fndef.clone(),
			body: fndef.body.clone(),
		})
	}

	fn visit_call(&mut self, call: &FunctionCall) -> Value {
		self.visit(&call.caller).call(self, call)
	}

	fn visit_var_decl(&mut self, decl: &VariableDeclaration) -> Value {
		let expr_value = self.visit(&decl.expression);
		self.variables.last_mut().unwrap().insert(decl.identifier.clone(), expr_value);

		Value::Unit
	}

	fn visit_var_assign(&mut self, assign: &VariableAssign) -> Value {
		let expr_value = self.visit(&assign.expression);
		// FIXME: Add assignments to outer scopes
		self.variables.last_mut().unwrap().insert(assign.identifier.clone(), expr_value.clone());

		expr_value
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