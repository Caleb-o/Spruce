use crate::{
	parsing::ast::{AST, Body, ConstDeclaration, VariableDeclaration, BinOp},
};
use super::symbols::{Symbol, SymbolTable};

pub struct Analyser {
	table: SymbolTable,
	had_error: bool,
}

impl Analyser {
	pub fn new() -> Self {
		Self { table: SymbolTable::new(), had_error: false }
	}

	pub fn run(&mut self, root: &AST) -> bool {
		if let AST::Program(p) = root {
			self.visit(&p);
		}
		
		self.had_error
	}

	fn error(&mut self, msg: String) {
		self.had_error = true;
		println!("Error: {}", msg);
	}

	fn visit(&mut self, node: &AST) {
		match node {
			AST::Body(b) => self.visit_body(&b),
			AST::ConstDeclaration(decl) => self.visit_const_decl(&decl),
			AST::VariableDeclaration(decl) => self.visit_var_decl(&decl),

			AST::BinOp(op) => self.visit_binary_op(&op),
			AST::Identifier(id) => self.visit_identifier(&id),

			// Discard - no use in analysis right now
			AST::Number(_) | AST::Unset => {},

			_ => println!("Unknown node: \"{}\"", node),
		}
	}

	fn visit_body(&mut self, body: &Body) {
		for s in &body.statements {
			self.visit(s);
		}
	}

	fn visit_const_decl(&mut self, decl: &ConstDeclaration) {
		if self.table.contains(decl.identifier.clone()) {
			self.error(format!("Identifier '{}' already exists in scope depth {}",
				&decl.identifier,
				self.table.top(),
			));
		}

		if let AST::Unset = *decl.expression {
			self.error(format!("Constant '{}' must be set to a value", &decl.identifier));
		} else {
			self.table.declare(decl.identifier.clone(), Symbol::Declaration { 
				identifier: decl.identifier.clone(), 
				is_const: true,
				is_set: true,
			});
		}

		self.visit(&decl.expression);
	}

	fn visit_var_decl(&mut self, decl: &VariableDeclaration) {
		if self.table.contains(decl.identifier.clone()) {
			self.error(format!("Identifier '{}' already exists in scope depth {}",
				&decl.identifier,
				self.table.top(),
			));
		} else {
			self.table.declare(decl.identifier.clone(), Symbol::Declaration { 
				identifier: decl.identifier.clone(),
				is_const: true,
				is_set: if let AST::Unset = *decl.expression { false } else { true },
			});
		}

		self.visit(&decl.expression);
	}

	fn visit_binary_op(&mut self, op: &BinOp) {
		self.visit(&op.left);
		self.visit(&op.right);
	}

	fn visit_identifier(&mut self, id: &String) {
		if let Some(sym) = self.table.find(id.clone()) {
			match sym {
				Symbol::Declaration { identifier: _, is_const: _, is_set } => {
					if !is_set {
						self.error(format!("Cannot use identifier '{}' with an unset value", id));
					}
				}

				_ => {}
			}
		}
	}
}