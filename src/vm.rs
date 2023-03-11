use std::{fmt::Display, time::Instant, mem::{transmute, discriminant}, collections::HashMap};

use crate::{environment::{Environment, ConstantValue, get_type_name}, object::Object, instructions::Instruction, compiler::Function};

pub(crate) struct CallFrame {
	identifier: Option<u32>,
	return_to: u32,
	stack_start: u32,
}

impl CallFrame {
	pub(crate) fn new(identifier: Option<u32>, return_to: u32, stack_start: u32) -> Self {
		Self { identifier, return_to, stack_start }
	}
}

pub struct VM {
	pub(crate) had_error: bool,
	pub(crate) env: Box<Environment>,
	pub(crate) ip: *mut u8,
	pub(crate) stack: Vec<Object>,
	pub(crate) frames: Vec<CallFrame>,
	pub(crate) started: Instant,
	pub(crate) running: bool,
}

#[derive(Debug)]
pub struct RuntimeErr(pub String);

impl Display for RuntimeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl VM {
	pub fn new(mut env: Box<Environment>) -> Self {
		let ip = env.code.as_mut_ptr();
		
		Self {
			had_error: false,
			env,
			ip,
			stack: Vec::with_capacity(512),
			frames: Vec::with_capacity(64),
			started: Instant::now(),
			running: true,
		}
	}

	#[inline]
	pub fn stack_slice_from_call(&self) -> &[Object] {
		&self.stack[self.frames.last().unwrap().stack_start as usize..]
	}

	#[inline]
	pub fn stack_size(&self) -> usize {
		self.stack.len()
	}

	#[inline]
	pub fn drop(&mut self) -> Result<Object, RuntimeErr> {
		match self.stack.pop() {
			Some(o) => Ok(o),
			None => Err(RuntimeErr(
				"Trying to pop an empty stack".into()
			)),
		}
	}

	#[inline]
	pub fn peek(&self) -> &Object {
		self.stack.last().unwrap()
	}

	#[inline]
	pub fn peek_mut(&mut self) -> &mut Object {
		self.stack.last_mut().unwrap()
	}
	
	#[inline]
	pub fn push(&mut self, object: Object) {
		self.stack.push(object);
	}

	#[inline]
	pub fn warning(&self, msg: String) {
		println!("Warning: {msg}");
	}

	pub fn run(&mut self) {
		if let Err(e) = self.run_inner() {
			println!("Runtime: {}", e.0);
			self.dump_stack_trace();
		}
	}

	#[inline]
	fn get_instruction(&self) -> u8 {
		unsafe {
			*self.ip
		}
	}

	#[inline]
	fn inc(&mut self) {
		unsafe {
			self.ip = self.ip.offset(1);
		}
	}
	
	#[inline]
	pub(crate) fn set_ip(&mut self, location: usize) {
		unsafe {
			self.ip = self.env.code.as_mut_ptr().add(location);
		}
	}
	
	#[inline]
	pub(crate) fn ip_distance(&mut self) -> usize {
		self.ip as usize - self.env.code.as_mut_ptr() as usize
	}

	pub(crate) fn print_current_stack(&self) {
		let mut last = self.stack.len() as u32;

		for (i, frame) in self.frames.iter().rev().enumerate() {
			print!("{} ", self.frames.len() - 1 - i);

			if let Some(id) = frame.identifier {
				let meta = &self.env.functions[id as usize];
				print!("{} -- ", meta.identifier);
			} else {
				print!("<script> -- ");
			}

			println!("{:#?}", frame.stack_start..last);
			(frame.stack_start..last).rev()
				.for_each(|idx| {
					println!("    {idx:0>4} {}", self.stack[idx as usize]);
				});
			
			last = frame.stack_start;
		}
	}
	
	#[inline]	
	pub(crate) fn run_instruction(&mut self, code: Instruction) -> Result<(), RuntimeErr> {
		// println!("CODE :: {} {:?}", code as u8, code);
		match code {
			Instruction::Pop => _ = self.drop()?,
			Instruction::Peek => self.push(self.peek().clone()),

			Instruction::Constant => {
				let idx = self.get_byte();
				let constant = match self.env.constants[idx as usize] {
					ConstantValue::Obj(ref o) => o,
					_ => return Err(RuntimeErr(
						"Trying to push a non-object to the stack".into()	
					))
				};
				self.stack.push(constant.clone());
			}

			Instruction::ConstantLong => {
				let idx = self.get_short();
				let constant = match self.env.constants[idx as usize] {
					ConstantValue::Obj(ref o) => o,
					_ => return Err(RuntimeErr(
						"Trying to push a non-object to the stack".into()	
					))
				};
				self.stack.push(constant.clone());
			}

			Instruction::Halt => {
				self.running = false;
				return Ok(());
			}

			Instruction::Negate => {
				let last = self.drop()?;

				match last {
					Object::Number(n) => self.push(Object::Number(-n)),
					Object::Boolean(b) => self.push(Object::Boolean(!b)),
					_ => return Err(RuntimeErr(format!(
						"Cannot negate value '{last}'"
					))),
				}
			},

			Instruction::BuildFn => {
				let arg_count = self.get_byte();
				let location = self.get_long();
				self.push(Object::AnonFunction(arg_count, location));
			}

			Instruction::BuildList => {
				let count = self.get_byte();
				let mut list = Vec::with_capacity(count as usize);

				for _ in 0..count {
					list.push(Box::new(self.drop()?));
				}
				list.reverse();

				self.push(Object::List(list));
			}

			Instruction::BuildSymbol => {
				let value = self.get_short();
				self.push(Object::Symbol(value));
			}

			Instruction::BuildMap => {
				let count = self.get_byte();
				let mut values = HashMap::new();

				for _ in 0..count {
					let value = self.drop()?;
					let identifier = match self.drop()? {
						Object::String(s) => s,
						n @ _ => return Err(RuntimeErr(format!(
							"Identifier '{n}' is not a string",
						))),
					};

					values.insert(identifier, Box::new(value));
				}

				self.push(Object::StringMap(values));
			}

			Instruction::Greater => {
				let size = self.stack_size() - 2;
				let (lhs, rhs) = self.pop_peek_check()?;

				if let Object::Number(ref mut l) = lhs {
					if let Object::Number(r) = rhs {
						self.stack[size] = Object::Boolean(*l > r);
					}
				}
			}

			Instruction::GreaterEqual => {
				let size = self.stack_size() - 2;
				let (lhs, rhs) = self.pop_peek_check()?;

				if let Object::Number(ref mut l) = lhs {
					if let Object::Number(r) = rhs {
						self.stack[size] = Object::Boolean(*l >= r);
					}
				}
			}

			Instruction::Less => {
				let size = self.stack_size() - 2;
				let (lhs, rhs) = self.pop_peek_check()?;

				if let Object::Number(ref mut l) = lhs {
					if let Object::Number(r) = rhs {
						self.stack[size] = Object::Boolean(*l < r);
					}
				}
			}

			Instruction::LessEqual => {
				let size = self.stack_size() - 2;
				let (lhs, rhs) = self.pop_peek_check()?;

				if let Object::Number(ref mut l) = lhs {
					if let Object::Number(r) = rhs {
						self.stack[size] = Object::Boolean(*l <= r);
					}
				}
			}

			Instruction::Add => {
				let (lhs, rhs) = self.pop_peek_check()?;

				match lhs {
					Object::Number(ref mut l) => {
						if let Object::Number(r) = rhs {
							*l += r;
						}
					}
					Object::String(ref mut l) => {
						if let Object::String(ref r) = rhs {
							l.push_str(r);
						}
					}
					Object::List(ref mut l) => {
						if let Object::List(r) = rhs {
							l.extend(r);
						}
					}
					_ => {}
				}
			}

			Instruction::Sub => {
				let (lhs, rhs) = self.pop_peek_check()?;

				if let Object::Number(ref mut l) = lhs {
					if let Object::Number(r) = rhs {
						*l -= r;
					}
				}
			}

			Instruction::Mul => {
				let (lhs, rhs) = self.pop_peek_check()?;

				if let Object::Number(ref mut l) = lhs {
					if let Object::Number(r) = rhs {
						*l *= r;
					}
				}
			}

			Instruction::Div => {
				let (lhs, rhs) = self.pop_2_check()?;

				if let Object::Number(l) = lhs {
					if let Object::Number(r) = rhs {
						if r == 0.0 {
							return Err(RuntimeErr("Trying to divide by 0".into()));
						}

						self.stack.push(Object::Number(l / r));
					}
				}
			}

			Instruction::EqualEqual => {
				let (lhs, rhs) = self.pop_2_check()?;

				match lhs {
					Object::Number(l) => {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l == r));
						}
					}
					Object::Symbol(l) => {
						if let Object::Symbol(r) = rhs {
							self.stack.push(Object::Boolean(l == r));
						}
					}
					_ => return Err(RuntimeErr(format!(
						"Cannot use {} in equality check",
						lhs
					))),
				}
			}

			Instruction::NotEqual => {
				let (lhs, rhs) = self.pop_2_check()?;

				match lhs {
					Object::Number(l) => {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l != r));
						}
					}
					Object::Symbol(l) => {
						if let Object::Symbol(r) = rhs {
							self.stack.push(Object::Boolean(l != r));
						}
					}
					_ => return Err(RuntimeErr(format!(
						"Cannot use {} in equality check",
						lhs
					))),
				}
			}

			Instruction::IndexGet => {
				let indexer = self.drop()?;
				let item = self.drop()?;

				match indexer {
					Object::Number(idx) => {
						match item {
							Object::String(ref v) => {
								let index = idx as usize;
								if index < v.len() {
									self.push(Object::String(String::from(&v[index..index + 1])));
								} else {
									return Err(RuntimeErr(format!(
										"Index out of bounds {} into item of {}",
										index, v.len()
									)))
								}
							}

							Object::List(ref v) => {
								let index = idx as usize;
								if index < v.len() {
									self.push((*v[index]).clone());
								} else {
									return Err(RuntimeErr(format!(
										"Index out of bounds {} into item of {}",
										index, v.len()
									)))
								}
							}

							_ => return Err(RuntimeErr(format!(
								"Cannot index {} with numeric value {}",
								item, idx
							)))
						}
					}
					_ => return Err(RuntimeErr(format!(
						"Cannot index {} with non-indexer value {}",
						item, indexer
					)))
				}
			}

			Instruction::IndexSet => {
				let value = self.drop()?;
				let indexer = self.drop()?;
				let item = self.peek_mut();

				match indexer {
					Object::Number(idx) => {
						match item {
							Object::String(ref mut v) => {
								let index = idx as usize;
								if index < v.len() {
									if let Object::String(ref s) = value {
										v.insert_str(index, s);
									} else {
										return Err(RuntimeErr(format!(
											"Cannot insert '{}' into string '{}'",
											value, item
										)))
									}
								} else {
									return Err(RuntimeErr(format!(
										"Index out of bounds {} into item of {}",
										index, v.len()
									)))
								}
							}

							Object::List(ref mut v) => {
								let index = idx as usize;
								if index < v.len() {
									v[index] = Box::new(value);
								} else {
									return Err(RuntimeErr(format!(
										"Index out of bounds {} into item of {}",
										index, v.len()
									)))
								}
							}

							_ => return Err(RuntimeErr(format!(
								"Cannot index {} with numeric value {}",
								item, idx
							)))
						}
					}
					_ => return Err(RuntimeErr(format!(
						"Cannot index {} with non-indexer value {}",
						item, indexer
					)))
				}
			}

			Instruction::GetProperty => {
				let identifier = self.drop()?;
				let object = self.drop()?;

				let identifier = match identifier {
					Object::String(s) => s,
					n @ _ => return Err(RuntimeErr(format!(
						"Cannot index object with value '{n}'",
					))),
				};

				match object {
					Object::StringMap(ref map) => {
						if !map.contains_key(&identifier) {
							return Err(RuntimeErr(format!(
								"Object {object} does not contain the field '{identifier}'",
							)));
						}

						self.push(*map[&identifier].clone());
					}
					n @ _ => return Err(RuntimeErr(format!(
						"Cannot index non-object with '{n}'",
					))),
				}
			}

			Instruction::SetProperty => {
				let value = self.drop()?;
				let identifier = self.drop()?;
				let object = self.peek_mut();

				let identifier = match identifier {
					Object::String(s) => s,
					n @ _ => return Err(RuntimeErr(format!(
						"Cannot index object with value '{n}'",
					))),
				};

				match object {
					Object::StringMap(ref mut map) => {
						if !map.contains_key(&identifier) {
							return Err(RuntimeErr(format!(
								"Object {object} does not contain the field '{identifier}'",
							)));
						}

						map.insert(identifier, Box::new(value));
					}
					n @ _ => return Err(RuntimeErr(format!(
						"Cannot index non-object with '{n}'",
					))),
				}
			}

			Instruction::GetGlobal => {
				let slot = self.get_short();
				self.stack.push(self.stack[slot as usize].clone());
			}
			
			Instruction::SetGlobal => {
				let slot = self.get_short();
				self.stack[slot as usize] = (*self.peek()).clone();
			}
			
			Instruction::GetLocal => {
				let slot = self.get_short();
				self.stack.push(self.stack[
						self.frames.last().unwrap().stack_start as usize + slot as usize
					].clone()
				);
			}
			
			Instruction::SetLocal => {
				let slot = self.get_short();
				self.stack[
					self.frames.last().unwrap().stack_start as usize + slot as usize
				] = (*self.peek()).clone();
			}

			Instruction::Jump => {
				let location = self.get_short() as usize;
				self.set_ip(location);
				return Ok(());
			}

			Instruction::JumpNot => {
				let top = self.drop()?;
				let loc = self.get_short();

				if !matches!(top, Object::Boolean(_)) {
					return Err(RuntimeErr(
						"Cannot use non-boolean in condition".into()
					));
				}

				if let Object::Boolean(v) = top {
					if !v {
						self.set_ip(loc as usize);
						return Ok(());
					}
				}
			}

			Instruction::GetFn => {
				let location = self.get_long();
				self.push(Object::Function(location));
			}

			Instruction::CallLocal => {
				let args = self.get_byte();
				let distance = self.ip_distance() as u32;
				let func = self.peek();

				match *func {
					Object::Function(meta_id) => {
						let meta = &self.env.functions[meta_id as usize];
	
						if args != meta.arg_count {
							return Err(RuntimeErr(format!(
								"Trying to call function {} with {} args, but require {}",
								meta.identifier, args, meta.arg_count,
							)));
						}
	
						self.frames.push(CallFrame::new(
							Some(meta_id),
							distance,
							self.stack.len() as u32 - args as u32,
						));
						self.set_ip(meta.location as usize);
						return Ok(());
					}

					Object::AnonFunction(arg_count, location) => {
						if args != arg_count {
							return Err(RuntimeErr(format!(
								"Trying to call anon function with {} args, but require {}",
								arg_count, args,
							)));
						}

						self.frames.push(CallFrame::new(
							None,
							distance,
							// -1 since we peek the function value now, instead of pop
							self.stack.len() as u32 - args as u32 - 1,
						));
						self.set_ip(location as usize);
						return Ok(());
					}

					_ => return Err(RuntimeErr(format!(
						"Cannot call non-function '{}'",
						func.get_type_name()
					))),
				}
			}
			
			Instruction::Call => {
				let meta_id = self.get_long();
				let distance = self.ip_distance() as u32;
				let meta = &self.env.functions[meta_id as usize];
				self.check_function_args_count(meta.arg_count)?;
				
				self.frames.push(CallFrame::new(
					Some(meta_id),
					distance,
					self.stack.len() as u32 - meta.arg_count as u32,
				));
				self.set_ip(meta.location as usize);
				return Ok(());
			}

			Instruction::CallNative => {
				let args = self.get_byte();
				let loc = self.get_long();
				self.check_function_args_count(args)?;

				if let ConstantValue::Func(f) = self.env.constants[loc as usize].clone() {
					if let Function::Native { param_count: _, function, .. } = f {
						let distance = self.ip_distance() as u32;
						self.frames.push(CallFrame::new(
							// TODO: Add a flag for native function
							None,
							distance,
							self.stack.len() as u32 - args as u32,
						));

						function(self, args)?;

						_ = self.frames.pop();
					}
				}
			}

			Instruction::TypeCheck => {
				let item = self.drop()?;
				let type_id = self.get_byte();
				self.push(Object::Boolean(self.get_type_match(&item, type_id)));
			}

			Instruction::TypeCheckAssert => {
				let type_id = self.get_byte();
				let item = self.peek();

				if !self.get_type_match(&item, type_id) {
					return Err(RuntimeErr(format!(
						"Expected type {} but received {}",
						get_type_name(type_id),
						item.get_type_name(),
					)));
				}
			}

			Instruction::ReturnNone => {
				let frame = self.frames.pop().unwrap();
				self.set_ip(frame.return_to as usize);

				// Remove all end values, except return
				self.stack.drain(frame.stack_start as usize..self.stack.len());
				self.stack.push(Object::None);
			}

			Instruction::Return => {
				let frame = self.frames.pop().unwrap();
				self.set_ip(frame.return_to as usize);
					
				// Remove all end values, except return
				self.stack.drain(frame.stack_start as usize..self.stack.len()-1);
			}

			Instruction::None => self.push(Object::None),
			Instruction::True => self.push(Object::Boolean(true)),
			Instruction::False => self.push(Object::Boolean(false)),

			Instruction::NoOp => {},

			_ => todo!(
				"Unimplemented instruction in VM '{:?}'",
				self.get_instruction()
			),
		}

		self.inc();

		Ok(())
	}

	fn run_inner(&mut self) -> Result<(), RuntimeErr> {
		// Initial frame
		self.frames.push(CallFrame::new(None, 0, 0));

		while !self.had_error && self.running {
			self.run_instruction(*unsafe { transmute::<*mut u8, &Instruction>(self.ip) })?;

			if self.had_error {
				return Err(RuntimeErr("Error occured".into()));
			}
		}
		Ok(())
	}

	fn dump_stack_trace(&self) {
		println!("=== Stack Trace ===");
		self.frames.iter()
			.for_each(|frame| {
				if let Some(id) = frame.identifier {
					println!("at {}()", self.env.functions[id as usize].identifier);
				} else {
					println!("at <anon>()");
				}
			});
	}

	fn check_function_args_count(&self, args: u8) -> Result<(), RuntimeErr> {
		if let Some(frame) = self.frames.last() {
			if args as usize > self.stack.len() - frame.stack_start as usize {
				return Err(RuntimeErr(format!(
					"Native Function requires {} arguments, but received {}",
					args,
					self.stack.len(),
				)));
			}
		} else {
			return Err(RuntimeErr("Frame does not exist".into()));
		}
		
		Ok(())
	}

	#[inline]
	fn pop_2_check(&mut self) -> Result<(Object, Object), RuntimeErr> {
		let rhs = self.drop()?;
		let lhs = self.drop()?;

		VM::check_types_match(lhs, rhs)
	}

	#[inline]
	fn pop_peek_check(&mut self) -> Result<(&mut Object, Object), RuntimeErr> {
		let rhs = self.drop()?;
		let lhs = self.peek_mut();

		if discriminant(lhs) == discriminant(&rhs) {
			return Ok((lhs, rhs));
		}
		
		Err(RuntimeErr(format!(
			"Value types do not match '{}' != '{}'",
			lhs, rhs,
		)))
	}
	
	#[inline]
	fn check_types_match(lhs: Object, rhs: Object) -> Result<(Object, Object), RuntimeErr> {
		if discriminant(&lhs) == discriminant(&rhs) {
			return Ok((lhs, rhs));
		}
		
		Err(RuntimeErr(format!(
			"Value types do not match '{}' != '{}'",
			lhs, rhs,
		)))
	}
	
	#[inline]
	fn get_byte(&mut self) -> u8 {
		self.inc();
		self.get_instruction()
	}
	
	#[inline]
	fn get_short(&mut self) -> u16 {
		self.inc();
		let a = self.get_instruction();
		self.inc();
		let b = self.get_instruction();
		
		u16::from_be_bytes([a, b])
	}

	#[inline]
	fn get_long(&mut self) -> u32 {
		self.inc();
		let a = self.get_instruction();
		self.inc();
		let b = self.get_instruction();
		self.inc();
		let c = self.get_instruction();
		self.inc();
		let d = self.get_instruction();

		u32::from_be_bytes([a, b, c, d])
	}

	fn get_type_match(&self, item: &Object, type_id: u8) -> bool {
		match type_id {
			0 => true,
			1 => matches!(item, Object::None),
			2 => matches!(item, Object::Number(_)),
			3 => matches!(item, Object::String(_)),
			4 => matches!(item, Object::Boolean(_)),
			5 => matches!(item, Object::List(_)),
			6 => matches!(item, Object::StringMap(_)),
			7 => matches!(item, Object::Symbol(_)),
			_ => false,
		}
	}
}