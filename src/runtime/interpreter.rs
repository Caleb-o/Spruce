use std::{collections::HashMap, rc::Rc};

use crate::parsing::ast::{Node, AST, Body, VariableDeclaration, Println, FunctionDefinition, FunctionCall, VariableAssign};

use super::{Value, FnValue, Callable};

pub struct Interpreter {
	pub(super) variables: Vec<HashMap<String, Rc<Value>>>,
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

	fn find_value(&self, key: &String) -> Rc<Value> {
		for scope in self.variables.iter().rev() {
			if scope.contains_key(key) {
				return Rc::clone(&*scope.get(key).unwrap());
			}
		}

		Rc::new(Value::Unit)
	}

	pub(super) fn visit(&mut self, node: &Node) -> Rc<Value> {
		match *node.ast {
			AST::Body(ref b) => self.visit_body(b),
			AST::Argument(ref arg) => self.visit(&arg.expr),
			AST::FunctionDefinition(ref fndef) => self.visit_fn_def(fndef),
			AST::FunctionCall(ref call) => self.visit_call(call),
			AST::VariableDeclaration(ref decl) => self.visit_var_decl(decl),
			AST::VariableAssign(ref assign) => self.visit_var_assign(assign),
			AST::Identifier(ref id) => self.visit_identifier(id),
			AST::Number(num) => Rc::new(Value::Number(num)),
			AST::String(ref string) => Rc::new(Value::String(string.clone())),
			AST::BinOp(_) => todo!(),
			AST::UnaryOp(_) => todo!(),
			AST::Range(_) => todo!(),
			AST::Println(ref print) => self.visit_print(print),
		}
	}

	fn visit_body(&mut self, body: &Body) -> Rc<Value> {
		self.begin();

		for node in body.statements.iter() {
			self.visit(node);
		}

		self.end();

		Rc::new(Value::Unit)
	}

	fn visit_fn_def(&mut self, fndef: &FunctionDefinition) -> Rc<Value> {
		Rc::new(Value::Function(FnValue {
			definition: fndef.clone(),
		}))
	}

	fn visit_call(&mut self, call: &FunctionCall) -> Rc<Value> {
		self.visit(&call.caller).call(self, call)
	}

	fn visit_var_decl(&mut self, decl: &VariableDeclaration) -> Rc<Value> {
		let expr_value = self.visit(&decl.expression);
		self.variables.last_mut().unwrap().insert(decl.identifier.clone(), expr_value);

		Rc::new(Value::Unit)
	}

	fn visit_var_assign(&mut self, assign: &VariableAssign) -> Rc<Value> {
		let expr_value = self.visit(&assign.expression);
		// FIXME: Add assignments to outer scopes
		self.variables.last_mut().unwrap().insert(assign.identifier.clone(), Rc::clone(&expr_value));

		expr_value
	}

	fn visit_identifier(&mut self, id: &String) -> Rc<Value> {
		self.find_value(id)
	}

	fn visit_print(&mut self, print_line: &Println) -> Rc<Value> {
		for expr in print_line.expressions.iter() {
			let expr_value = self.visit(&expr);
			print!("{expr_value}");
		}
		println!();

		Rc::new(Value::Unit)
	}
}