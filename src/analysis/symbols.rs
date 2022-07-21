use std::{collections::HashMap, rc::Rc};

use crate::parsing::ast::AST;

pub enum Symbol {
	Declaration { identifier: String, is_const: bool, ast: Rc<AST> },
}

struct Scope {
	symbols: HashMap<String, Option<Rc<Symbol>>>,
}

impl Scope {
	fn new() -> Self {
		Self { symbols: HashMap::new() }
	}

	fn insert(&mut self, key: String, sym: Option<Rc<Symbol>>) {
		self.symbols.insert(key, sym);
	}

	fn lookup(&self, key: &String) -> Option<Rc<Symbol>> {
		(*self.symbols.get(key).unwrap()).clone()
	}

	fn contains(&mut self, key: &String) -> bool {
		self.symbols.contains_key(key)
	}
}

pub struct SymbolTable {
	scope: Vec<Scope>,
}

impl SymbolTable {
	pub fn new() -> Self {
		Self { scope: vec![ Scope::new() ] }
	}

	pub fn top(&self) -> usize {
		self.scope.len() - 1
	}

	pub fn declare(&mut self, key: &String, sym: Rc<Symbol>) {
		let idx = self.scope.len() - 1;
		self.scope[idx].insert(key.clone(), Some(sym));
	}

	pub fn contains(&mut self, key: &String) -> bool {
		let idx = self.scope.len() - 1;
		self.scope[idx].contains(key)
	}

	pub fn get(&mut self, key: &String) -> Option<Rc<Symbol>> {
		let idx = self.scope.len() - 1;
		self.scope[idx].lookup(key)
	}

	pub fn find(&self, key: &String) -> Option<Rc<Symbol>> {
		for idx in (0..self.scope.len()).rev() {
			let sym = self.scope[idx].lookup(key);

			if sym.is_some() {
				return sym;
			}
		}

		None
	}
}