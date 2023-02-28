use std::{io::Error, fmt::Display, rc::Rc, fs, thread, time::Duration};

use crate::{lexer::Lexer, token::{Span, Token, TokenKind}, environment::{Environment, ConstantValue}, object::Object, instructions::{Instruction, ParamKind}, vm::{VM, RuntimeErr}};

#[derive(Debug, Clone, Copy)]
struct Local {
	identifier: Span,
	// Position in bytecode
	position: u16,
	is_global: bool,
	mutable: bool,
}

type NativeFunction = Rc<dyn Fn(&mut VM, u8) -> Result<(), RuntimeErr>>;

#[derive(Debug, Clone)]
struct LookAhead {
	span: Span,
	identifier: String,
	args: u8,
	// Position in bytecode
	position: u32,
}

#[derive(Clone)]
#[repr(u8)]
pub enum Function {
	User { 
		identifier: Span,
		position: u32,
		parameters: Option<Vec<Span>>,
		empty: bool,
	},
	Native {
		identifier: &'static str,
		param_count: ParamKind,
		function: NativeFunction,
		has_return: bool,
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

type LocalTable = Vec<Vec<Local>>;

pub struct Compiler {
	had_error: bool,
	current: Token,
	lexer: Lexer,
	unresolved: Vec<LookAhead>,
	locals: LocalTable,
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
			functable: Vec::new(),
		})
	}

	pub fn run(&mut self) -> Result<Box<Environment>, CompilerErr> {
		let mut env = Box::new(Environment::new());
		self.register_native_functions(&mut env);

		self.push_scope();
		self.outer_statements(&mut env)?;
		self.pop_scope();

		match self.find_function_str("main") {
			Some(func) => {
				if let Function::User { identifier: _, position, parameters, .. } = func {
					env.add_call(parameters.as_ref().map_or(0, |p| p.len()) as u8, *position);
				}
			}
			None => return Err(self.error("Cannot find function 'main'".into())),
		}

		env.add_op(Instruction::Halt);

		// Try to resolve calls that were not during compilation
		self.resolve_function_calls(&mut env);

		if self.had_error {
			return Err(CompilerErr("Error(s) occured".into()));
		}

		Ok(env)
	}

	pub fn add_fn(
		&mut self,
		env: &mut Box<Environment>,
		identifier: &'static str,
		param_count: ParamKind,
		has_return: bool,
		function: NativeFunction,
	) {
		let function = Function::Native { 
			identifier,
			param_count,
			function,
			has_return,
		};
		// Add to function table
		self.functable.push(function.clone());

		// Add to environment
		env.add_constant_function(ConstantValue::Func(function));
	}

	// TODO: Move to different file
	fn register_native_functions(&mut self, env: &mut Box<Environment>) {
		self.add_fn(env, "print", ParamKind::Any, false, Rc::new(|vm, args| {
			vm.stack_slice_from_call()
				.iter()
				.for_each(|o| print!("{o}"));

			(0..args).into_iter()
				.for_each(|_| {vm.drop().unwrap();});

			Ok(())
		}));

		self.add_fn(env, "println", ParamKind::Any, false, Rc::new(|vm, args| {
			vm.stack_slice_from_call()
				.iter()
				.for_each(|o| print!("{o}"));

			(0..args).into_iter()
				.for_each(|_| {vm.drop().unwrap();});

			println!();
			Ok(())
		}));

		self.add_fn(env, "time", ParamKind::Count(0), true, Rc::new(|vm, _| {
			let t = vm.started.elapsed().as_millis() as f32;
			vm.push(Object::Number(t));
			Ok(())
		}));

		self.add_fn(env, "sleep", ParamKind::Count(1), false, Rc::new(|vm, _| {
			if let Object::Number(n) = vm.drop()? {
				thread::sleep(Duration::from_millis(n as u64));
			}
			Ok(())
		}));

		self.add_fn(env, "read_file", ParamKind::Count(1), true, Rc::new(|vm, _| {
			if let Object::String(s) = vm.drop()? {
				match fs::read_to_string(s) {
					Ok(content) => {
						vm.push(Object::String(content));
						vm.push(Object::Boolean(true));
					},
					Err(_) => {
						vm.push(Object::None);
						vm.push(Object::Boolean(false));
					},
				}
			} else {
				vm.warning(format!("read_file expected a string but received {}", vm.peek()));
				vm.push(Object::None);
			}

			Ok(())
		}));

		self.add_fn(env, "strlen", ParamKind::Count(1), true, Rc::new(|vm, _args| {
			if let Object::String(s) = vm.drop()? {
				vm.push(Object::Number(s.len() as f32));
			} else {
				vm.warning(format!("strlen expected a string but received {}", vm.peek()));
				vm.push(Object::None);
			}

			Ok(())
		}));

		self.add_fn(env, "list_push", ParamKind::Count(2), true, Rc::new(|vm, _args| {
			let item = vm.drop()?;

			if let Object::List(ref list) = vm.drop()? {
				let mut inner = list.clone();
				inner.push(Box::new(item));
				vm.push(Object::List(inner));
			} else {
				vm.warning(format!("list_push expected a list but received {}", vm.peek()));
				vm.push(Object::None);
			}
			
			Ok(())
		}));

		self.add_fn(env, "list_pop", ParamKind::Count(1), true, Rc::new(|vm, _args| {
			// FIXME: Use pointer object type for it to effect items
			if let Object::List(ref list) = vm.drop()? {
				let mut inner = list.clone();
				let item = *inner.pop().unwrap();
				vm.push(item);
			} else {
				vm.warning(format!("list_push expected a list but received {}", vm.peek()));
				vm.push(Object::None);
			}
			
			Ok(())
		}));

		self.add_fn(env, "list_len", ParamKind::Count(1), true, Rc::new(|vm, _args| {
			if let Object::List(ref list) = vm.drop()? {
				vm.push(Object::Number(list.len() as f32));
			} else {
				vm.warning(format!("list_push expected a list but received {}", vm.peek()));
				vm.push(Object::None);
			}
			
			Ok(())
		}));

		self.add_fn(env, "dbg_stack_size", ParamKind::Count(0), false, Rc::new(|vm, _args| {
			println!("Stack size {}", vm.stack_size());
			Ok(())
		}));

		self.add_fn(env, "dbg_print", ParamKind::Count(0), false, Rc::new(|vm, _args| {
			for (index, item) in vm.get_stack().iter().enumerate() {
				println!("{index:0>4} {item}");
			}
			Ok(())
		}));
	}

	fn push_scope(&mut self) {
		self.locals.push(Vec::new());
	}

	fn pop_scope(&mut self) {
		_= self.locals.pop();
	}

	fn resolve_function_calls(&mut self, env: &mut Box<Environment>) {
		let mut unresolved = Vec::new();

		for lookahead in self.unresolved.iter() {
			match self.find_function(lookahead.span) {
				Some(ref mut func) => {
					// Cannot resolve native calls, since they're part of the compiler
					if let Function::User { identifier, position, parameters, empty } = func {
						// Generate the function if it is not empty
						if !empty {
							let id = identifier.slice_from(&self.lexer.source).to_string();
							let paramc = parameters.as_ref().map_or(0, |p| p.len()) as usize;
							
							// Correct function ID, but arity does not match
							if paramc != lookahead.args as usize {
								unresolved.push((id.clone(), paramc, lookahead.args));
								continue;
							}

							env.code[lookahead.position as usize] = paramc as u8;
							u32::to_be_bytes(*position)
								.into_iter()
								.enumerate()
								.for_each(
									|(i, b)| 
									env.code[lookahead.position as usize + 1 + i] = b
								);
						} else {
							// Patch call to no-op as it is empty
							env.code[lookahead.position as usize] = Instruction::NoOp as u8;
							(0..4)
								.for_each(
									|i|
									env.code[lookahead.position as usize + 1 + i] = Instruction::NoOp as u8
							);
						}
					}
				}

				None => unresolved.push((lookahead.identifier.clone(), 0, lookahead.args)),
			}
		}

		// Identifiers that were still not found
		for (identifier, params, args) in unresolved {
			if params != args as usize {
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

	fn consume_here(&mut self) {
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
				Function::Native { identifier, .. } => {
					if id == *identifier {
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

	fn find_local_in(&self, span: Span, topmost: bool, index: usize) -> Option<Local> {
		for local in self.locals[index].iter() {
			if local.identifier.compare(&span, &self.lexer.source) {
				return Some(*local);
			}
		}

		if topmost || index == 0 {
			return None;
		}

		return self.find_local_in(span, false, index - 1);
	}

	fn find_local(&self, span: Span, topmost: bool) -> Option<Local> {
		self.find_local_in(span, topmost, self.locals.len() - 1)
	}

	fn register_local(&mut self, span: Span, mutable: bool) -> Option<usize> {
		let local = self.find_local(span, true);

		match local {
			Some(local) => {
				// We aren't allowed to overwrite, it is an error
				self.error_no_exit(format!(
					"Local with identifier '{}' already exists in scope",
					local.identifier.slice_from(&self.lexer.source),
				));
				None
			}

			None => {
				let len = self.locals.len();
				let position = self.locals.last().unwrap().len() as u16;
				self.locals.last_mut().unwrap().push(Local {
					identifier: span,
					position,
					is_global: len == 1,
					mutable,
				});
				Some(self.locals.last().unwrap().len() - 1)
			}
		}
	}

	fn register_function(
		&mut self,
		identifier: Span,
		parameters: Option<Vec<Span>>,
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
		if let Some(ref params) = parameters {
			for param in params.iter() {
				_ = self.register_local(*param, false);
			}
		}

		let position = (env.op_here() + if env.code.len() >= u16::MAX as usize {
			5
		} else {
			3
		}) as u32;

		self.functable.push(Function::User {
			identifier,
			position,
			parameters,
			empty: false
		});

		Ok(())
	}

	fn mark_function_empty(&mut self, id: Span) {
		for func in &mut self.functable {
			// Cannot mark native functions as empty
			if let Function::User { identifier, .. } = func {
				if identifier.compare(&id, &self.lexer.source) {
					func.mark_empty();
				}
			}
		}
	}

	fn function_call(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		let identifier = self.current;
		self.consume(TokenKind::Identifier, "Expected identifier in function call")?;

		let mut arg_count: u8 = 0;
		self.consume(TokenKind::LParen, "Expect '(' after function identifier")?;
		
		// We track arguments, since native functions can have N..Any parameters
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
							let paramc = parameters.as_ref().map_or(0, |p| p.len()) as usize;
							
							if paramc != arg_count as usize {
								fnerr = Some((id.clone(), paramc as u8));
							}

							env.add_call(paramc as u8, *position);
						}
						Function::Native { identifier, param_count, .. } => {
							if let ParamKind::Count(c) = param_count {
								if *c != arg_count {
									fnerr = Some((identifier.to_string(), *c));
								}

								env.add_call_native(
									*c,
									env.find_constant_func_loc(&identifier) as u32,
								);
							} else {
								// Add call with N arguments
								env.add_call_native(
									arg_count,
									env.find_constant_func_loc(&identifier) as u32,
								);
							}
						}
					}
				}
			}

			None => {
				// Push the call to a stack of unresolved calls
				// They will be filled in at the end, if they exist
				let position = env.op_here() as u32 + 1;
				env.add_call(0, 0);

				let string = identifier.span.slice_from(&self.lexer.source).to_string();
				self.unresolved.push(LookAhead {
					span: identifier.span,
					identifier: string,
					args: arg_count,
					position,
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

	fn if_statement(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_here();

		self.expression(env)?;

		let before_block = env.add_jump_op(Instruction::JumpNot);
		self.body(env)?;
		let true_block = env.add_jump_op(Instruction::Jump);

		if self.current.kind == TokenKind::Else {
			self.consume_here();
			env.patch_jump_op(before_block);

			if self.current.kind == TokenKind::If {
				self.if_statement(env)?;
			} else {
				self.body(env)?;
			}
		} else {
			env.patch_jump_op(before_block);
		}
		
		env.patch_jump_op(true_block);
		Ok(())
	}

	fn while_statement(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_here();

		// Evaluate condition
		let start = env.op_here();
		self.expression(env)?;
		let before_block = env.add_jump_op(Instruction::JumpNot);

		self.body(env)?;
		// Return back before the condition to re-evaluate
		let jmp = env.add_jump_op(Instruction::Jump);
		env.patch_jump_op_to(jmp, start);

		env.patch_jump_op(before_block);

		Ok(())
	}

	fn identifier(&mut self, env: &mut Box<Environment>) {
		match self.find_local(self.current.span, false) {
			Some(ref local) => {
				if local.is_global {
					env.add_local(Instruction::GetGlobal, local.position);
				} else {
					env.add_local(Instruction::GetLocal, local.position);
				}
			},

			None => self.error_no_exit(format!(
				"Identifier '{}' does not exist",
				self.current.span.slice_from(&self.lexer.source),
			)),
		}

		self.consume_here();
	}

	fn primary(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		match self.current.kind {
			TokenKind::Int => {
				let span = self.current.span;
				env.add_constant(Object::Number(
					self.lexer.source[span.start..span.start + span.len]
						.parse::<f32>()
						.unwrap()
				));
				self.consume_here();
				Ok(())
			}
			
			TokenKind::String => {
				let span = self.current.span;
				env.add_constant(Object::String(
					String::from(
						&self.lexer.source[span.start..span.start + span.len]
					)
				));
				self.consume_here();
				Ok(())
			}

			TokenKind::True => {
				self.consume_here();
				env.add_constant(Object::Boolean(true));
				Ok(())
			}

			TokenKind::False => {
				self.consume_here();
				env.add_constant(Object::Boolean(false));
				Ok(())
			}

			TokenKind::None => {
				self.consume_here();
				env.add_constant(Object::None);
				Ok(())
			}

			TokenKind::LParen => {
				self.consume_here();
				self.expression(env)?;
				self.consume(TokenKind::RParen, "Expect ')' to close group expression")?;
				Ok(())
			},

			TokenKind::LSquare => self.list_literal(env),

			TokenKind::Identifier => {
				match self.lexer.peek_type() {
					TokenKind::LParen => self.function_call(env)?,
					TokenKind::Equal => self.var_assign(env)?,
					_ => self.identifier(env),
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
					self.consume_here();
					self.unary(env)?;
					env.add_op(Instruction::Mul);
				}
	
				TokenKind::Slash => {
					self.consume_here();
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
					self.consume_here();
					self.factor(env)?;
					env.add_op(Instruction::Add);
				}
				
				TokenKind::Minus => {
					self.consume_here();
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
					self.consume_here();
					self.term(env)?;
					env.add_op(Instruction::Greater);
				}
	
				TokenKind::Less => {
					self.consume_here();
					self.term(env)?;
					env.add_op(Instruction::Less);
				}
	
				TokenKind::GreaterEqual => {
					self.consume_here();
					self.term(env)?;
					env.add_op(Instruction::GreaterEqual);
				}
	
				TokenKind::LessEqual => {
					self.consume_here();
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
				TokenKind::EqualEqual => {
					self.consume_here();
					self.comparison(env)?;
					env.add_op(Instruction::EqualEqual);
				}
	
				TokenKind::NotEqual => {
					self.consume_here();
					self.comparison(env)?;
					env.add_op(Instruction::NotEqual);
				}
	
				_ => break,
			}
		}

		Ok(())
	}

	fn expression(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.equality(env)
	}

	fn list_literal(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_here();

		let mut count = 0;

		if self.current.kind != TokenKind::RSquare {
			self.expression(env)?;
			count += 1;

			while self.current.kind == TokenKind::Comma {
				self.consume_here();

				self.expression(env)?;
				count += 1;
			}
		}

		self.consume(TokenKind::RSquare, "Expect ']' after list literal arguments")?;

		env.add_op(Instruction::BuildList);
		env.add_opb(count as u8);

		Ok(())
	}

	fn var_declaration(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		let mutable = self.current.kind == TokenKind::Var;
		self.consume_here();

		let identifier = self.current;
		self.consume(TokenKind::Identifier, "Expected identifier after 'var'/'let'")?;

		_ = self.register_local(identifier.span, mutable);

		// Produce the expression
		if self.current.kind == TokenKind::Equal {
			self.consume_here();
			self.expression(env)?;
		} else {
			env.add_op(Instruction::None);
		}

		Ok(())
	}

	fn var_assign(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		let identifier = self.current;
		self.consume(TokenKind::Identifier, "Expected identifier in assignment")?;

		self.consume(TokenKind::Equal, "Expect '=' after identifier in assignment")?;

		self.expression(env)?;

		match self.find_local(identifier.span, false) {
			Some(local) => {
				if !local.mutable {
					self.error_no_exit(format!(
						"Cannot re-assign an immutable value '{}'",
						identifier.span.slice_from(&self.lexer.source),
					));
				} else {
					if local.is_global {
						env.add_local(Instruction::SetGlobal, local.position);
					} else {
						env.add_local(Instruction::SetLocal, local.position);
					}
				}
			}
			None => {
				self.error_no_exit(format!(
					"Cannot assign to variable '{}' as it does not exist",
					identifier.span.slice_from(&self.lexer.source),
				));
			}
		}

		Ok(())
	}

	fn return_statement(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_here();
		let mut has_expr = false;

		if self.current.kind != TokenKind::SemiColon {
			self.expression(env)?;
			has_expr = true;
		}

		if !has_expr {
			env.add_op(Instruction::None);
		}

		env.add_op(Instruction::Return);
		Ok(())
	}

	fn statement(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		match self.current.kind {
			TokenKind::If => {
				self.if_statement(env)?;
				return Ok(());
			},
			TokenKind::While => {
				self.while_statement(env)?;
				return Ok(());
			},
			// Default as expression statement
			// TODO: Check that this is only assignment or function call
			TokenKind::Var | TokenKind::Let => self.var_declaration(env)?,
			TokenKind::Return => self.return_statement(env)?,
			_ => {
				// Since expressions yield a value, it makes no sense to keep them
				// on the stack, but we still want their effect (like a function call)
				self.expression(env)?;
				// env.add_op(Instruction::Pop);
			},
		}
		
		self.consume(TokenKind::SemiColon, "Expect ';' after statement")
	}

	fn body(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume(TokenKind::LCurly, "Expect '{' to start block body")?;

		self.push_scope();

		while self.current.kind != TokenKind::RCurly {
			self.statement(env)?;
		}

		self.pop_scope();

		self.consume(TokenKind::RCurly, "Expect '}' to end block body")?;

		Ok(())
	}

	fn function(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		self.consume_here();
		self.push_scope();

		let identifier = self.current.span;
		self.consume(TokenKind::Identifier, "Expected identifier after 'func'")?;

		let parameters = if self.current.kind == TokenKind::LParen {
			let mut parameters = Vec::new();
			self.consume(TokenKind::LParen, "Expect '(' at the start of parameter list")?;
	
			// Consume paarameter list
			// TODO: Underscore to add unnamed parameter
			if self.current.kind != TokenKind::RParen {
				let param = self.current.span;
				self.consume(TokenKind::Identifier, "Expected identifier in parameter list")?;
				parameters.push(param);
	
				while self.current.kind == TokenKind::Comma {
					self.consume_here();
	
					let param = self.current.span;
					self.consume(TokenKind::Identifier, "Expected identifier in parameter list after comma")?;
					parameters.push(param);
				}
			}
			
			self.consume(TokenKind::RParen, "Expect ')' after function parameter list")?;
			Some(parameters)
		} else {None};

		self.register_function(identifier, parameters, env)?;

		let jmp = env.add_jump_op(Instruction::Jump);
		let start_loc = env.op_here();

		self.body(env)?;
		
		// Don't generate pointless returns for empty functions
		if start_loc != env.op_here() {
			if *env.code.last().unwrap() != Instruction::Return as u8 {
				// Only add return if the last instruction wasn't a return
				env.add_op(Instruction::None);
				env.add_op(Instruction::Return);
			}
		} else {
			self.mark_function_empty(identifier);
		}
		
		env.patch_jump_op(jmp);
		self.pop_scope();

		Ok(())
	}

	fn outer_statements(&mut self, env: &mut Box<Environment>) -> Result<(), CompilerErr> {
		while self.current.kind != TokenKind::EndOfFile {
			match self.current.kind {
				TokenKind::Function => self.function(env)?,
				TokenKind::Let | TokenKind::Var => {
					self.var_declaration(env)?;
					self.consume(TokenKind::SemiColon, "Expect ';' after statement")?;
				},
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