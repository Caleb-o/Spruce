use std::{io::Error, fmt::Display, rc::Rc, fs};

use crate::{lexer::Lexer, token::{Span, Token, TokenKind}, environment::{Environment, ConstantValue}, object::Object, instructions::{Instruction, ParamKind}, vm::{VM, RuntimeErr}};

#[derive(Debug, Clone, Copy)]
struct Local {
	identifier: Span,
	// Position in bytecode
	position: usize,
	// Cannot shadow/overwrite parameters
	is_param: bool,
}

type NativeFunction = Rc<dyn Fn(&mut VM, usize) -> Result<(), RuntimeErr>>;

#[derive(Debug, Clone)]
struct LookAhead {
	span: Span,
	identifier: String,
	args: usize,
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
		param_count: ParamKind,
		function: NativeFunction,
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
		param_count: ParamKind,
		function: NativeFunction,
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

	// TODO: Move to different file
	fn register_native_functions(&mut self, env: &mut Box<Environment>) {
		self.add_fn(env, "print", ParamKind::Any, Rc::new(|vm, args| {
			let mut values = Vec::new();

			for _ in 0..args {
				values.push(vm.drop()?);
			}

			values.into_iter().rev().for_each(|v| print!("{v}"));

			Ok(())
		}));

		self.add_fn(env, "println", ParamKind::Any, Rc::new(|vm, args| {
			let mut values = Vec::new();

			for _ in 0..args {
				values.push(vm.drop()?);
			}

			values.into_iter().rev().for_each(|v| println!("{v}"));

			Ok(())
		}));

		self.add_fn(env, "read_file", ParamKind::Count(1), Rc::new(|vm, args| {
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

		self.add_fn(env, "strlen", ParamKind::Count(1), Rc::new(|vm, args| {
			let string = vm.peek();
			
			if !matches!(string, Object::String(_)) {
				vm.warning(format!("strlen expected a string but received {}", string));
				vm.push(Object::None);
				return Ok(());
			}

			if let Object::String(s) = string {
				vm.push(Object::Int(s.len() as i32));
			}
			Ok(())
		}));
	}

	fn resolve_function_calls(&mut self, env: &mut Box<Environment>) {
		let mut unresolved: Vec<(String, usize, usize)> = Vec::new();

		for lookahead in self.unresolved.iter() {
			match self.find_function(lookahead.span) {
				Some(ref mut func) => {
					match func {
						Function::User { identifier, position, parameters, empty } => {
							// Generate the function if it is not empty
							if !empty {
								let id = identifier.slice_from(&self.lexer.source).to_string();
								
								if parameters.len() != lookahead.args {
									unresolved.push((id.clone(), parameters.len(), lookahead.args));
									continue;
								}

								env.code[lookahead.position] = Instruction::Call(*position, parameters.len()); 
							} else {
								// We cannot remove without it breaking, so we replace with a NoOp
								env.code[lookahead.position] = Instruction::NoOp;
							}
						}

						// Cannot resolve native calls, since they're part of the compiler
						_ => {}
					}
				}

				None => unresolved.push((lookahead.identifier.clone(), 0, lookahead.args)),
			}
		}

		// Identifiers that were still not found
		for (identifier, params, args) in unresolved {
			if params != args {
				self.error_no_exit(format!(
					"Function '{identifier}' expected {params} argument(s), but received {args}",
				));
			} else {
				self.error_no_exit(format!(
					"Function '{identifier}' does not exist",
				));
			}
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

	fn register_local(&mut self, span: Span, is_param: bool, overwrite: bool) -> Option<usize> {
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
					is_param,
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

			let slot = self.register_local(*param, true, true).unwrap();
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
		let identifier = self.current;
		self.consume(TokenKind::Identifier, "Expected identifier in function call")?;

		self.consume(TokenKind::LParen, "Expect '(' after function identifier")?;
		
		// We track arguments, since native functions can have N..Any parameters
		let mut arg_count: usize = 0;
		if self.current.kind != TokenKind::RParen {
			self.expression(env)?;
			arg_count += 1;
			
			while self.current.kind == TokenKind::Comma {
				self.consume(TokenKind::Comma, "Expect ',' after function argument")?;
				self.expression(env)?;
				arg_count += 1;
			}
		}
		
		self.consume(TokenKind::RParen, "Expect ')' after argument list")?;
		let mut fnerr: Option<(String, u8)> = None;

		match self.find_function(identifier.span) {
			Some(func) => {
				if !func.is_empty() {
					// Only generate the call if the function is not empty
					match func {
						Function::User { identifier, position, parameters, empty: _ } => {
							let id = identifier.slice_from(&self.lexer.source).to_string();
							
							if parameters.len() != arg_count {
								fnerr = Some((id.clone(), parameters.len() as u8));
							}

							env.add_op(Instruction::CallNative(
								env.find_constant_func_loc(&id),
								parameters.len(),
							));
							
							env.add_op(Instruction::Call(*position, parameters.len()));
						}
						Function::Native { identifier, param_count, function: _ } => {
							if let ParamKind::Count(c) = param_count {
								if *c as usize != arg_count {
									fnerr = Some((identifier.clone(), *c));
								}

								env.add_op(Instruction::CallNative(
									env.find_constant_func_loc(&identifier),
									*c as usize,
								));
							} else {
								// Add call with N arguments
								env.add_op(Instruction::CallNative(
									env.find_constant_func_loc(&identifier),
									arg_count,
								));
							}
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
					args: arg_count,
					position: env.op_here() - 1,
				});
			}
		}

		// Display error if it occured
		if fnerr.is_some() {
			let fnerr = fnerr.unwrap();
			self.error_no_exit(format!(
				"Function '{}' expected {} argument(s), but received {arg_count}",
				fnerr.0, fnerr.1,
			));
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

			if self.current.kind == TokenKind::If {
				self.if_block(env)?;
			} else {
				self.body(env)?;
			}
		} else {
			env.patch_jump_op(before_block);
		}
		
		env.patch_jump_op(true_block);
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

	fn primary(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
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

			TokenKind::Identifier => {
				if self.lexer.peek_type() == TokenKind::LParen {
					self.function_call(env)?;
				} else {
					self.identifier(env);
				}
				Ok(())
			},

			_ => Err(self.error(format!(
				"Unexpected instruction found {:?} '{}'",
				self.current.kind,
				self.current.span.slice_from(&self.lexer.source),
			))),
		}
	}

	fn unary(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		// TODO
		self.primary(env)
	}

	fn factor(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.unary(env)?;
		
		loop {
			match self.current.kind {
				TokenKind::Star => {
					self.consume_any();
					self.unary(env)?;
					env.add_op(Instruction::Mul);
				}
	
				TokenKind::Slash => {
					self.consume_any();
					self.unary(env)?;
					env.add_op(Instruction::Div);
				}
	
				_ => break,
			}
		}

		Ok(())
	}

	fn term(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.factor(env)?;
		
		loop {
			match self.current.kind {
				TokenKind::Plus => {
					self.consume_any();
					self.factor(env)?;
					env.add_op(Instruction::Add);
				}
				
				TokenKind::Minus => {
					self.consume_any();
					self.factor(env)?;
					env.add_op(Instruction::Sub);
				}
	
				_ => break,
			}
		}

		Ok(())
	}

	fn comparison(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.term(env)?;
		
		loop {
			match self.current.kind {
				TokenKind::Greater => {
					self.consume_any();
					self.term(env)?;
					env.add_op(Instruction::Greater);
				}
	
				TokenKind::Less => {
					self.consume_any();
					self.term(env)?;
					env.add_op(Instruction::Less);
				}
	
				TokenKind::GreaterEqual => {
					self.consume_any();
					self.term(env)?;
					env.add_op(Instruction::GreaterEqual);
				}
	
				TokenKind::LessEqual => {
					self.consume_any();
					self.term(env)?;
					env.add_op(Instruction::LessEqual);
				}
	
				_ => break,
			}
		}

		Ok(())
	}

	fn equality(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.comparison(env)?;

		loop {
			match self.current.kind {
				TokenKind::Equal => {
					self.consume_any();
					self.comparison(env)?;
					env.add_op(Instruction::Equal);
				}
	
				TokenKind::NotEqual => {
					self.consume_any();
					self.comparison(env)?;
					env.add_op(Instruction::Equal);
				}
	
				_ => break,
			}
		}

		Ok(())
	}

	fn assignment(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.equality(env)?;

		loop {
			match self.current.kind {
				TokenKind::Equal => todo!("Var assignment"),
				_ => break,
			}
		}

		Ok(())
	}

	fn expression(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.assignment(env)
	}

	fn statement(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		match self.current.kind {
			TokenKind::If => self.if_block(env)?,
			// Default as expression statement
			// TODO: Check that this is only assignment or function call
			_ => self.expression(env)?,
		}
		
		self.consume(TokenKind::SemiColon, "Expect ';' after statement")
	}

	fn body(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume(TokenKind::LCurly, "Expect '{' to start block body")?;

		while self.current.kind != TokenKind::RCurly {
			self.statement(env)?;
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

		self.consume(TokenKind::LParen, "Expect '(' at the start of parameter list")?;

		// Consume paarameter list
		// TODO: Underscore to add unnamed parameter
		if self.current.kind != TokenKind::RParen {
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

		self.consume(TokenKind::RParen, "Expect ')' after function parameter list")?;

		self.register_function(identifier, parameters, env)?;
		let is_main = identifier.compare_str("main", &self.lexer.source);

		let start_loc = env.op_here();

		self.body(env)?;

		// Don't generate pointless returns for empty functions
		if start_loc != env.op_here() {
			if is_main {
				env.add_op(Instruction::Halt);
			} else {
				env.add_op(Instruction::Return(0));
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