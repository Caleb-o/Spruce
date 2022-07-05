use std::rc::Rc;

use crate::{parsing::ast::{AST, Body, ConstDeclaration, VariableDeclaration, BinOp, VariableAssign, Node}, lexing::token::Token};
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
			AST::ConstDeclaration(decl) => self.visit_const_decl(node, &decl),
			AST::VariableDeclaration(decl) => self.visit_var_decl(node, &decl),
			AST::VariableAssign(assign) => self.visit_var_assign(node, &assign),

			AST::BinOp(op) => self.visit_binary_op(&op),
			AST::Identifier(id) => self.visit_identifier(node, &id),

			// Discard - no use in analysis right now
			AST::String(_) | AST::Number(_) | AST::Unset => {},

			_ => println!("Unknown node: \"{}\"", node.ast),
		}
	}

	fn visit_body(&mut self, body: &Body) {
		for s in &body.statements {
			self.visit(s);
		}
	}

	fn visit_const_decl(&mut self, node: &Node, decl: &ConstDeclaration) {
		if self.table.contains(&decl.identifier) {
			self.error(&node.token,
				format!("Identifier '{}' already exists in scope depth {}",
					&decl.identifier,
					self.table.top(),
				)
			);
		}

		if let AST::Unset = *decl.expression.ast {
			self.error(&node.token, format!("Constant '{}' must be set to a value", &decl.identifier));
		} else {
			self.table.declare(&decl.identifier, Symbol::Declaration { 
				identifier: decl.identifier.clone(), 
				is_const: true,
				is_set: true,
			});
		}

		self.visit(&decl.expression);
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
			self.table.declare(&decl.identifier, Symbol::Declaration { 
				identifier: decl.identifier.clone(),
				is_const: false,
				is_set: if let AST::Unset = *decl.expression.ast { false } else { true },
			});
		}

		self.visit(&decl.expression);
	}

	fn visit_var_assign(&mut self, node: &Node, assign: &VariableAssign) {
		let sym = self.table.find(&assign.identifier);

		match sym {
			Some(s) => {
				if let Symbol::Declaration { identifier: _, is_const, is_set: _ } = s {
					if *is_const {
						self.error(&node.token,
							format!("Identifier '{}' is constant and cannot be re-assigned to",
								&assign.identifier,
							)
						);
					}
				}
			}
			None => {
				self.error(&node.token,
					format!("Identifier '{}' does not exist in any scope",
						&assign.identifier,
					)
				);
			}
		}

		self.visit(&assign.expression);
	}

	fn visit_binary_op(&mut self, op: &BinOp) {
		self.visit(&op.left);
		self.visit(&op.right);
	}

	fn visit_identifier(&mut self, node: &Node, id: &String) {
		if let Some(sym) = self.table.find(&id) {
			match sym {
				Symbol::Declaration { identifier: _, is_const: _, is_set } => {
					if !is_set {
						self.error(&node.token, format!("Cannot read from identifier '{}' that has an unset value", id));
					}
				}

				_ => {}
			}
		}
	}
}