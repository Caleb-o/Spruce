use std::rc::Rc;

use crate::{parsing::ast::{AST, Body, VariableDeclaration, BinOp, VariableAssign, Node, FunctionDefinition, FunctionCall, Println}, lexing::token::Token};
use super::symbols::{Symbol, SymbolTable};


pub struct Analyser {
	table: SymbolTable,
	had_error: bool,
}

impl Analyser {
	pub fn new() -> Self {
		Self { table: SymbolTable::new(), had_error: false }
	}

	pub fn run(&mut self, root: &Node) -> bool {
		self.visit(root);
		!self.had_error
	}

	fn error(&mut self, token: &Rc<Token>, msg: String) {
		self.had_error = true;
		println!("Error: {} [{}:{} '{}']", msg, token.line, token.column, token.lexeme);
	}

	fn visit(&mut self, node: &Node) {
		match &*node.ast {
			AST::Body(b) => self.visit_body(&b),
			AST::VariableDeclaration(decl) => self.visit_var_decl(node, &decl),
			AST::VariableAssign(assign) => self.visit_var_assign(node, &assign),

			AST::FunctionDefinition(def) => self.visit_func_def(&def),
			AST::FunctionCall(call) => self.visit_func_call(&call),

			AST::BinOp(op) => self.visit_binary_op(&op),
			AST::Identifier(id) => self.visit_identifier(node, &id),

			AST::Println(print) => self.visit_println(print),

			// Discard - no use in analysis right now
			AST::String(_) | AST::Number(_) => {},

			AST::Argument(ref arg) => self.visit(&arg.expr),
			AST::UnaryOp(_) => todo!(),
			AST::Range(_) => todo!(),
		}
	}

	fn visit_body(&mut self, body: &Body) {
		for s in &body.statements {
			self.visit(s);
		}
	}

	fn visit_var_decl(&mut self, node: &Node, decl: &VariableDeclaration) {
		if self.table.contains(&decl.identifier) {
			self.error(&node.token,
				format!("Identifier '{}' already exists in scope depth {}",
					&decl.identifier,
					self.table.top(),
				)
			);
		} else {
			self.table.declare(
				&decl.identifier, 
				Rc::new(Symbol::Declaration { 
					identifier: decl.identifier.clone(),
					ast: Rc::clone(&node.ast),
				}
			));
		}

		self.visit(&decl.expression);
	}

	fn visit_var_assign(&mut self, node: &Node, assign: &VariableAssign) {
		let sym = self.table.find(&assign.identifier);

		 if let None = sym {
			self.error(&node.token,
				format!("Identifier '{}' does not exist in any scope",
					&assign.identifier,
				)
			);
		}

		self.visit(&assign.expression);
	}

	fn visit_func_def(&mut self, def: &FunctionDefinition) {
		self.table.begin();
		
		for param in def.parameters.iter() {
			self.table.declare(
				&param.lexeme.to_string(), 
				Rc::new(Symbol::Identifer { 
					identifier: param.lexeme.to_string(), 
				})
			)
		}
		
		self.visit(&def.body);

		self.table.end();
	}

	fn visit_func_call(&mut self, call: &FunctionCall) {
		self.visit(&call.caller);
		
		self.table.begin();

		for arg in call.arguments.iter() {
			self.visit(arg);

			self.table.declare(
				&arg.token.lexeme.to_string(), 
				Rc::new(Symbol::Declaration { 
					identifier: arg.token.lexeme.to_string(), 
					ast: Rc::clone(&arg.ast),
				})
			)
		}

		self.table.end();
	}

	fn visit_binary_op(&mut self, op: &BinOp) {
		self.visit(&op.left);
		self.visit(&op.right);
	}

	fn visit_identifier(&mut self, node: &Node, id: &String) {
		if let None = self.table.find(&id) {
			self.error(&node.token, format!("Could not find identifier '{}'", id));
		}
	}

	fn visit_println(&mut self, print_line: &Println) {
		for expr in print_line.expressions.iter() {
			self.visit(expr);
		}
	}
}