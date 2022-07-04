use crate::{errors::spruce_error::SpruceError, parsing::ast::AST};

use super::symbols::{Symbol, SymbolTable};

pub struct Analyser {
	table: SymbolTable,
}

impl Analyser {
	pub fn new() -> Self {
		Self { table: SymbolTable::new() }
	}

	pub fn run() -> Result<(), SpruceError> {
		Ok(())
	}

	fn visit(&mut self, node: &AST) -> Result<(), SpruceError> {
		Ok(())
	}
}