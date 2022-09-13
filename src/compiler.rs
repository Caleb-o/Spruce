use std::{io::Error, fmt::Display, rc::Rc, fs};

use crate::{lexer::Lexer, token::{Span, Token, TokenKind}, environment::{Environment, ConstantValue}, object::Object, instructions::Instruction, vm::{VM, RuntimeErr}};

#[derive(Debug, Clone, Copy)]
struct Local {
	identifier: Span,
	// Position in bytecode
	position: usize,
	// Cannot shadow/overwrite parameters
	is_param: bool,
}

#[derive(Debug, Clone)]
struct LookAhead {
	span: Span,
	identifier: String,
	// Position in bytecode
	position: usize,
}

#[derive(Clone)]
#[repr(u8)]
pub enum Function {
	User { 
		identifier: Span,
		position: usize,
		parameters: Vec<Span>,
		empty: bool,
	},
	Native {
		identifier: String,
		param_count: u8,
		function: Rc<dyn Fn(&mut VM) -> Result<(), RuntimeErr>>,
	},
}

impl Function {
	pub fn is_empty(&self) -> bool {
		match *self {
			Function::User { identifier: _, position: _, parameters: _, empty } => {
				empty
			}
			_ => false,
		}
	}

	pub fn mark_empty(&mut self) {
		match *self {
			Function::User { identifier: _, position: _, parameters: _, ref mut empty } => {
				*empty = true;
			}
			_ => {}
		}
	}
}

pub struct Compiler {
	had_error: bool,
	current: Token,
	lexer: Lexer,
	unresolved: Vec<LookAhead>,
	locals: Vec<Local>,
	functable: Vec<Function>,
}

pub struct CompilerErr(pub String);

impl Display for CompilerErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Compiler {
	pub fn new(filepath: &str) -> Result<Self, Error> {
		let mut lexer = Lexer::new(filepath)?;

		Ok(Self {
			had_error: false,
			current: lexer.next(),
			lexer,
			unresolved: Vec::new(),
			locals: Vec::new(),
			functable: Vec::new()
		})
	}

	pub fn run(&mut self) -> Result<Box<Environment>, CompilerErr> {
		let mut env = Box::new(Environment::new());
		self.register_native_functions(&mut env);
		self.outer_statements(&mut env)?;

		match self.find_function_str("main") {
			Some(ref func) => {
				if let Function::User { identifier: _, position, .. } = func {
					// Set the main entry point
					env.entry = *position;
				}
			}
			None => return Err(self.error("Cannot find function 'main'".into())),
		}

		// Try to resolve calls that were not during compilation
		self.resolve_function_calls(&mut env);

		if self.had_error {
			return Err(self.error("Error(s) occured".into()));
		}

		Ok(env)
	}

	pub fn add_fn(
		&mut self,
		env: &mut Box<Environment>,
		identifier: &'static str,
		param_count: u8,
		function: Rc<dyn Fn(&mut VM) -> Result<(), RuntimeErr>>,
	) {
		let function = Function::Native { 
			identifier: identifier.to_string(),
			param_count,
			function,
		};
		// Add to function table
		self.functable.push(function.clone());

		// Add to environment
		env.add_constant_function(ConstantValue::Func(function));
	}

	fn register_native_functions(&mut self, env: &mut Box<Environment>) {
		self.add_fn(env, "print", 1, Rc::new(|vm| {
			let obj = vm.drop()?;
			print!("{obj}");

			Ok(())
		}));

		self.add_fn(env, "println", 1, Rc::new(|vm| {
			let obj = vm.drop()?;
			println!("{obj}");

			Ok(())
		}));

		self.add_fn(env, "read_file", 1, Rc::new(|vm| {
			let file_name = vm.drop()?;
			
			if !matches!(file_name, Object::String(_)) {
				vm.warning(format!("read_file expected a string but received {}", file_name));
				vm.push(Object::None);
				return Ok(());
			}

			match fs::read_to_string(match file_name { Object::String(s) => s, _ => unreachable!() }) {
				Ok(content) => {
					vm.push(Object::String(content));
					vm.push(Object::Boolean(true));
				},
				Err(_) => {
					vm.push(Object::None);
					vm.push(Object::Boolean(false));
				},
			}

			Ok(())
		}));

		self.add_fn(env, "strlen", 1, Rc::new(|vm| {
			let string = vm.peek();
			
			if !matches!(string, Object::String(_)) {
				vm.warning(format!("strlen expected a string but received {}", string));
				vm.push(Object::None);
				return Ok(());
			}

			vm.push(Object::Int(match string { Object::String(s) => s, _ => unreachable!() }.len() as i32));
			Ok(())
		}));

		self.add_fn(env, "make_array", 1, Rc::new(|vm| {
			let integer = vm.drop()?;
			
			if !matches!(integer, Object::Int(_)) {
				vm.warning(format!("make_array expected an integer but received {}", integer));
				vm.push(Object::None);
				return Ok(());
			}

			let capacity = match integer { Object::Int(v) => v, _ => unreachable!() } as usize;
			let mut arr = Vec::with_capacity(capacity);

			for _ in 0..capacity {
				arr.push(Box::new(vm.drop()?));
			}

			vm.push(Object::Array(arr));
			Ok(())
		}));

		self.add_fn(env, "new_array", 1, Rc::new(|vm| {
			let integer = vm.drop()?;
			
			if !matches!(integer, Object::Int(_)) {
				vm.warning(format!("new_array expected an integer but received {}", integer));
				vm.push(Object::None);
				return Ok(());
			}

			let capacity = match integer { Object::Int(v) => v, _ => unreachable!() } as usize;
			vm.push(Object::Array(Vec::with_capacity(capacity)));
			Ok(())
		}));

		self.add_fn(env, "array_get", 2, Rc::new(|vm| {
			let index = vm.drop()?;
			let array = vm.peek();
			
			if !matches!(index, Object::Int(_)) || !matches!(array, Object::Array(_)) {
				vm.warning(format!(
					"new_array expected an integer and array but received {} and {}",
					index, array,
				));
				vm.push(Object::None);
				return Ok(());
			}

			let index = match index { Object::Int(v) => v, _ => unreachable!() } as usize;
			let array = match array { Object::Array(ref v) => v, _ => unreachable!() };

			if index > array.len() {
				vm.push(Object::Int(0));
				return Ok(());
			}

			vm.push(*array[index].clone());
			Ok(())
		}));

		self.add_fn(env, "array_set", 3, Rc::new(|vm| {
			let item = vm.drop()?;
			let index = vm.drop()?;
			let array = vm.peek_mut();

			if let Object::Int(index) = index {
				if let Object::Array(ref mut array) = array {
					if index as usize > array.len() {
						return Ok(());
					}
		
					array[index as usize] = Box::new(item);
				}
			}

			Ok(())
		}));

		self.add_fn(env, "array_push", 2, Rc::new(|vm| {
			let item = vm.drop()?;
			let array = vm.peek_mut();

			if let Object::Array(ref mut array) = array {
				array.push(Box::new(item));
			}

			Ok(())
		}));

		self.add_fn(env, "array_len", 1, Rc::new(|vm| {
			if let Object::Array(array) = vm.peek() {
				vm.push(Object::Int(array.len() as i32));
			} else {
				vm.push(Object::Int(0));
			}

			Ok(())
		}));
	}

	fn resolve_function_calls(&mut self, env: &mut Box<Environment>) {
		let mut unresolved: Vec<String> = Vec::new();

		for lookahead in self.unresolved.iter() {
			match self.find_function(lookahead.span) {
				Some(ref mut func) => {
					match func {
						Function::User { identifier: _, position, parameters, empty } => {
							// Generate the function if it is not empty
							if !empty {
								env.code[lookahead.position] = Instruction::Call(*position - 1, parameters.len() as u8); 
							} else {
								// We cannot remove without it breaking, so we replace with a NoOp
								env.code[lookahead.position] = Instruction::NoOp;
							}
						}

						_ => {}
					}
				}

				None => {
					unresolved.push(lookahead.identifier.clone());
				}
			}
		}

		// Identifiers that were still not found
		for identifier in unresolved {
			self.error_no_exit(format!(
				"Function '{}' does not exist",
				identifier,
			));
		}
	}

	fn error(&self, msg: String) -> CompilerErr {
		CompilerErr(format!(
			"[Compiler Error] {} [{}:{}]",
			msg,
			self.current.line,
			self.current.column,
		))
	}

	fn error_no_exit(&mut self, msg: String) {
		self.had_error = true;

		println!("{}", format!(
			"[Compiler Error] {} [{}:{}]",
			msg,
			self.current.line,
			self.current.column,
		));
	}

	fn consume(&mut self, expected: TokenKind, msg: &'static str) -> Result<(), CompilerErr> {
		if self.current.kind == expected {
			self.current = self.lexer.next();
			return Ok(());
		}

		Err(self.error(String::from(msg)))
	}

	fn consume_any(&mut self) {
		self.current = self.lexer.next();
	}

	fn find_function_str(&self, id: &str) -> Option<&Function> {
		for func in &self.functable {
			match func {
				Function::User { identifier, .. } => {
					if identifier.compare_str(id, &self.lexer.source) {
						return Some(func);
					}
				}
				Function::Native { identifier, param_count: _, function: _ } => {
					if id == identifier.as_str() {
						return Some(func);
					}
				}
			}
		}

		None
	}

	fn find_function(&self, span: Span) -> Option<&Function> {
		self.find_function_str(span.slice_from(&self.lexer.source))
	}

	fn find_local(&self, span: Span) -> Option<Local> {
		for local in self.locals.iter() {
			if local.identifier.compare(&span, &self.lexer.source) {
				return Some(*local);
			}
		}

		None
	}

	fn register_local(&mut self, span: Span, overwrite: bool) -> Option<usize> {
		let local = self.find_local(span);

		match local {
			Some(local) => {
				if !overwrite {
					return Some(local.position);
				}

				// We aren't allowed to overwrite, it is an error
				self.error_no_exit(format!(
					"Local with identifier '{}' already exists in scope",
					local.identifier.slice_from(&self.lexer.source),
				));
				None
			}

			None => {
				self.locals.push(Local {
					identifier: span,
					position: self.locals.len(),
				});
				Some(self.locals.len() - 1)
			}
		}
	}

	fn register_function(
		&mut self,
		identifier: Span,
		parameters: Vec<Span>,
		env: &mut Box<Environment>,
	) -> Result<(), CompilerErr>
	{
		let func = self.find_function(identifier);

		// Function already exists
		if func.is_some() {
			self.error_no_exit(
				format!(
					"Function with identifier '{}' already exists",
					identifier.slice_from(&self.lexer.source)
				)
			);
		}

		// Register locals from parameters
		for param in parameters.iter() {
			if param.is_underscore(&self.lexer.source) {
				break;
			}

			let slot = self.register_local(*param, true).unwrap();
			env.add_op(Instruction::SetLocal(slot as u8));
		}

		self.functable.push(Function::User {
			identifier,
			position: env.op_here(),
			parameters,
			empty: false
		});

		Ok(())
	}

	fn mark_function_empty(&mut self, id: Span) {
		for func in &mut self.functable {
			match func {
				Function::User { identifier, .. } => {
					if identifier.compare(&id, &self.lexer.source) {
						func.mark_empty();
					}
				}
				// Cannot mark native functions as empty
				Function::Native { identifier: _, param_count: _, function: _ } => {}
			}
		}
	}

	fn function_call(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_any();

		let identifier = self.current;
		self.consume(TokenKind::Identifier, "Expected identifier after '$'")?;

		let id = identifier.span.slice_from(&self.lexer.source);

		match self.find_function(identifier.span) {
			Some(func) => {
				if !func.is_empty() {
					// Only generate the call if the function is not empty
					match func {
						Function::User { identifier: _, position, parameters, empty: _ } => {
							env.add_op(Instruction::Call(*position, parameters.len() as u8));
						}
						Function::Native { identifier: _, param_count, function: _ } => {
							env.add_op(Instruction::CallNative(
								env.find_constant_func_loc(id),
								*param_count,
							));
						}
					}
				}
			}

			None => {
				// Push the call to a stack of unresolved calls
				// They will be filled in at the end, if they exist
				env.add_op(Instruction::Call(0, 0));

				let string = identifier.span.slice_from(&self.lexer.source).to_string();
				self.unresolved.push(LookAhead {
					span: identifier.span,
					identifier: string,
					position: env.op_here() - 1,
				});
			}
		}
		
		Ok(())
	}

	fn if_block(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_any();

		let before_block = env.add_jump_op(Instruction::JumpNot(0));
		self.body(env)?;
		let true_block = env.add_jump_op(Instruction::Jump(0));

		if self.current.kind == TokenKind::Else {
			self.consume_any();
			env.patch_jump_op(before_block);

			if self.current.kind == TokenKind::LCurly {
				self.body(env)?;
			} else {
				// Compile instructions prior to if
				while self.current.kind != TokenKind::EndOfFile || self.current.kind != TokenKind::If {
					self.instruction(env)?;
				}

				self.if_block(env)?;
			}
		} else {
			env.patch_jump_op(before_block);
		}
		
		env.patch_jump_op(true_block);
		Ok(())
	}

	fn bind_identifiers(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_any();

		self.consume(TokenKind::Pipe, "Expect '|' after bind")?;

		let mut identifiers = Vec::new();

		if self.current.kind != TokenKind::Pipe {
			identifiers.push(self.current);
			self.consume(TokenKind::Identifier, "Expect identifier in bind list")?;

			while self.current.kind == TokenKind::Comma {
				self.consume_any();

				identifiers.push(self.current);
				self.consume(TokenKind::Identifier, "Expect identifier in bind list")?;
			}
		}

		self.consume(TokenKind::Pipe, "Expect '|' after ")?;

		for identifier in identifiers {
			if let Some(slot) = self.register_local(identifier.span, false) {
				env.add_op(Instruction::SetLocal(slot as u8));
			}
		}

		Ok(())
	}

	fn loop_block(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_any();

		let pre_condition = env.add_jump_op(Instruction::Jump(0));
		let start = env.op_here();
		let condition = env.add_jump_op(Instruction::JumpNot(0));

		env.patch_jump_op(pre_condition);
		self.body(env)?;
		env.add_jump_op(Instruction::Jump(start));
		env.patch_jump_op(condition);

		Ok(())
	}

	fn identifier(&mut self, env: &mut Box<Environment>) {
		match self.find_local(self.current.span) {
			Some(ref local) => {
				env.add_op(Instruction::GetLocal(local.position as u8));
			},

			None => self.error_no_exit(format!(
				"Identifier '{}' does not exist",
				self.current.span.slice_from(&self.lexer.source),
			)),
		}

		self.consume_any();
	}

	fn instruction(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		match self.current.kind {
			TokenKind::Int => {
				let span = self.current.span;
				let index = env.add_constant(Object::Int(
					self.lexer.source[span.start..span.start + span.len]
						.parse::<i32>()
						.unwrap()
				));
				env.add_op(Instruction::Push(index));
				self.consume_any();
				Ok(())
			}
			
			TokenKind::String => {
				let span = self.current.span;
				let index = env.add_constant(Object::String(
					String::from(
						&self.lexer.source[span.start..span.start + span.len]
					)
				));
				env.add_op(Instruction::Push(index));
				self.consume_any();
				Ok(())
			}

			TokenKind::True => {
				self.consume_any();
				let index = env.add_constant(Object::Boolean(true));
				env.add_op(Instruction::Push(index));
				Ok(())
			}

			TokenKind::False => {
				self.consume_any();
				let index = env.add_constant(Object::Boolean(false));
				env.add_op(Instruction::Push(index));
				Ok(())
			}

			TokenKind::Dollar => self.function_call(env),
			TokenKind::If => self.if_block(env),
			TokenKind::Bind => self.bind_identifiers(env),
			TokenKind::Loop => self.loop_block(env),

			TokenKind::Identifier => { 
				self.identifier(env);
				Ok(())
			},

			TokenKind::Drop => {
				self.consume_any();
				Ok(env.add_op(Instruction::Drop))
			}

			TokenKind::Dup => {
				self.consume_any();
				Ok(env.add_op(Instruction::Dup))
			}

			TokenKind::Swap => {
				self.consume_any();
				Ok(env.add_op(Instruction::Swap))
			}

			TokenKind::Equal => {
				self.consume_any();
				Ok(env.add_op(Instruction::Equal))
			}

			TokenKind::GreaterEqual => {
				self.consume_any();
				Ok(env.add_op(Instruction::GreaterEqual))
			}

			TokenKind::LessEqual => {
				self.consume_any();
				Ok(env.add_op(Instruction::LessEqual))
			}

			TokenKind::Plus => {
				self.consume_any();
				Ok(env.add_op(Instruction::Add))
			}
			
			TokenKind::Minus => {
				self.consume_any();
				Ok(env.add_op(Instruction::Sub))
			}

			TokenKind::Star => {
				self.consume_any();
				Ok(env.add_op(Instruction::Mul))
			}

			TokenKind::Slash => {
				self.consume_any();
				Ok(env.add_op(Instruction::Div))
			}

			TokenKind::Greater => {
				self.consume_any();
				Ok(env.add_op(Instruction::Greater))
			}

			TokenKind::Less => {
				self.consume_any();
				Ok(env.add_op(Instruction::Less))
			}

			_ => Err(self.error(format!(
				"Unexpected instruction found {:?} '{}'",
				self.current.kind,
				self.current.span.slice_from(&self.lexer.source),
			))),
		}
	}

	fn body(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume(TokenKind::LCurly, "Expect '{' to start block body")?;

		while self.current.kind != TokenKind::RCurly {
			self.instruction(env)?;
		}

		self.consume(TokenKind::RCurly, "Expect '}' to end block body")?;

		Ok(())
	}

	fn function(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_any();

		let identifier = self.current.span;
		self.consume(TokenKind::Identifier, "Expected identifier after 'func'")?;

		let mut parameters = Vec::new();
		let mut has_underscore = false;

		// Consume paarameter list
		// TODO: Underscore to add unnamed parameter
		if self.current.kind != TokenKind::Colon {
			let param = self.current.span;
			self.consume(TokenKind::Identifier, "Expected identifier in parameter list")?;
			parameters.push(param);

			if param.is_underscore(&self.lexer.source) {
				has_underscore = true;
			}

			while self.current.kind == TokenKind::Comma {
				self.consume_any();

				let param = self.current.span;
				self.consume(TokenKind::Identifier, "Expected identifier in parameter list after comma")?;
				parameters.push(param);

				if param.is_underscore(&self.lexer.source) {
					// Cannot use identifiers after an underscore
					if has_underscore {
						self.error_no_exit(format!(
							"Trying to use an identifer at parameter position {}, after an underscore in function '{}'",
							parameters.len(),
							param.slice_from(&self.lexer.source),
						));
					}
					
					has_underscore = true;
				}
			}
		}

		self.consume(TokenKind::Colon, "Expect ':' after function identifier or parameter list")?;

		self.register_function(identifier, parameters, env)?;
		let is_main = identifier.compare_str("main", &self.lexer.source);

		let start_loc = env.op_here();

		self.body(env)?;

		// Don't generate pointless returns for empty functions
		if start_loc != env.op_here() {
			if is_main {
				env.add_op(Instruction::Halt);
			} else {
				env.add_op(Instruction::Return);
			}
		} else {
			self.mark_function_empty(identifier);
		}

		// Remove locals after function body
		self.locals.clear();

		Ok(())
	}

	fn outer_statements(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		while self.current.kind != TokenKind::EndOfFile {
			match self.current.kind {
				TokenKind::Function => self.function(env)?,
				_ => {
					return Err(self.error(format!(
						"Unknown item in outer scope {:?}",
						self.current.kind
					)));
				}
			}
		}

		Ok(())
	}
}