use crate::token::Span;

#[derive(Debug, Clone, Copy)]
pub struct Local {
	pub identifier: Span,
	pub depth: u16,
	pub position: u16,
	pub mutable: bool,
}

impl Local {
    pub fn is_global(&self) -> bool {
        self.depth == 0
    }
}

pub struct SymTable {
	depth: u16,
	locals: Vec<Local>,
}

impl SymTable {
	pub fn new() -> Self {
		Self { depth: 0, locals: Vec::new() }
	}

	pub fn new_scope(&mut self) {
		self.depth += 1;
	}

	pub fn close_scope(&mut self) {
		assert!(self.depth > 0);
        let start = self.count_scope() as usize;
		self.depth -= 1;
        self.locals.drain(self.locals.len() - start..);
	}

	pub fn new_local(
		&mut self,
		identifier: Span,
		mutable: bool,
	) -> u16 {
        let position = self.find_position();
		self.locals.push(Local { 
			identifier,
			depth: self.depth,
			position,
			mutable,
		});
        position
	}

    pub fn find_local(&self, source: &String, span: &Span, anyscope: bool) -> Option<&Local> {
        for local in self.locals.iter().rev() {
            if !anyscope && local.depth < self.depth {
                return None;
            }

            if local.identifier.compare(span, source) {
                return Some(local);
            }
        }

        None
    }

    fn find_position(&self) -> u16 {
        if self.depth > 1 {
            self.find_local_position()
        } else {
            self.find_global_position()
        }
    }

    fn count_scope(&self) -> u16 {
		self.locals
			.iter()
			.rev()
			.fold(0, |acc, l| {
				if l.depth >= self.depth {
					acc + 1
				} else {
                    acc
                }
			})
    }

    fn find_local_position(&self) -> u16 {
		self.locals
			.iter()
			.rev()
			.fold(0, |acc, l| {
				if l.depth > 1 {
					acc + 1
				} else {
                    acc
                }
			})
    }

	fn find_global_position(&self) -> u16 {
		self.locals
			.iter()
			.fold(0, |acc, l| {
				if l.depth == 1 {
                    acc + 1
				} else {
                    acc
                }
			})
	}
}