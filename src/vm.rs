use std::{fmt::Display, time::Instant, mem::transmute};

use crate::{environment::{Environment, ConstantValue, get_type_name}, object::Object, instructions::Instruction, compiler::Function};

struct CallFrame {
	identifier: Option<u32>,
	return_to: usize,
	stack_start: usize,
}

impl CallFrame {
	fn new(identifier: Option<u32>, return_to: usize, stack_start: usize) -> Self {
		Self { identifier, return_to, stack_start }
	}
}

pub struct VM {
	had_error: bool,
	env: Box<Environment>,
	ip: *mut u8,
	stack: Vec<Object>,
	frames: Vec<CallFrame>,
	pub started: Instant,
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
		}
	}

	#[inline]
	pub fn stack_slice_from_call(&self) -> &[Object] {
		&self.stack[self.frames.last().unwrap().stack_start..]
	}

	#[inline]
	pub fn stack_size(&self) -> usize {
		self.stack.len()
	}

	#[inline]
	pub fn get_stack(&self) -> &Vec<Object> {
		&self.stack
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
	fn set_ip(&mut self, location: usize) {
		unsafe {
			self.ip = self.env.code.as_mut_ptr().add(location);
		}
	}
	
	#[inline]
	fn ip_distance(&mut self) -> usize {
		self.ip as usize - self.env.code.as_mut_ptr() as usize
	}

	fn run_inner(&mut self) -> Result<(), RuntimeErr> {
		// Initial frame
		self.frames.push(CallFrame::new(None, 0, 0));

		while !self.had_error {
			// println!("CODE :: {:0>4} {code:?}", self.ip_distance());

			match *unsafe { transmute::<*mut u8, &Instruction>(self.ip) } {
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

				Instruction::Halt => break,
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

				Instruction::BuildList => {
					let count = self.get_byte();
					let mut list = Vec::with_capacity(count as usize);

					for _ in 0..count {
						list.push(Box::new(self.drop()?));
					}
					list.reverse();

					self.push(Object::List(list));
				}

				Instruction::Greater => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l > r));
						}
					}
				}

				Instruction::GreaterEqual => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l >= r));
						}
					}
				}

				Instruction::Less => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l < r));
						}
					}
				}

				Instruction::LessEqual => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l <= r));
						}
					}
				}

				Instruction::Add => {
					let (lhs, rhs) = self.pop_2_check()?;

					self.stack.push(match lhs {
						// Even though numbers are F32, they are added like integers
						Object::Number(l) => Object::Number(l as f32 + match rhs {
							Object::Number(r) => r  as f32,
							_ => unreachable!(),
						}),
						Object::String(l) => {
							let mut new_str = l.clone();
							new_str.push_str(match rhs {
								Object::String(ref r) => r,
								_ => unreachable!(),
							});
							Object::String(new_str)
						},
						_ => Object::None,
					});
				}

				Instruction::Sub => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Number(l - r));
						}
					}
				}

				Instruction::Mul => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Number(l * r));
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

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l == r));
						}
					}
				}

				Instruction::NotEqual => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Number(l) = lhs {
						if let Object::Number(r) = rhs {
							self.stack.push(Object::Boolean(l != r));
						}
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
						self.frames.last().unwrap().stack_start + slot as usize
						].clone()
					);
				}
				
				Instruction::SetLocal => {
					let slot = self.get_short();
					self.stack[
						self.frames.last().unwrap().stack_start + slot as usize
					] = (*self.peek()).clone();
				}

				Instruction::Jump => {
					let location = self.get_short() as usize;
					self.set_ip(location);
					continue;
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
							continue;
						}
					}
				}
				
				Instruction::Call => {
					let meta_id = self.get_long();
					let distance = self.ip_distance();
					let meta = &self.env.functions[meta_id as usize];
					self.check_function_args_count(meta.arg_count)?;
					
					self.frames.push(CallFrame::new(
						Some(meta_id),
						distance,
						self.stack.len() - meta.arg_count as usize,
					));

					self.set_ip(meta.location as usize);
					continue;
				},

				Instruction::CallNative => {
					let args = self.get_byte();
					let loc = self.get_long();
					self.check_function_args_count(args)?;

					if let ConstantValue::Func(f) = self.env.constants[loc as usize].clone() {
						if let Function::Native { param_count: _, function, .. } = f {
							let distance = self.ip_distance();
							self.frames.push(CallFrame::new(
								// TODO: Add a flag for native function
								None,
								distance,
								self.stack.len() - args as usize,
							));

							function(self, args)?;

							_ = self.frames.pop();
						}
					}
				},

				Instruction::TypeCheck => {
					let item = self.drop()?;
					let type_id = self.get_byte();
					self.push(Object::Boolean(match type_id {
						0 => item == Object::None,
						1 => matches!(item, Object::Number(_)),
						2 => matches!(item, Object::String(_)),
						3 => matches!(item, Object::Boolean(_)),
						_ => false,
					}));
				},

				Instruction::TypeCheckAssert => {
					let type_id = self.get_byte();
					let item = self.peek();
					let is_type = match type_id {
						0 => *item == Object::None,
						1 => matches!(*item, Object::Number(_)),
						2 => matches!(*item, Object::String(_)),
						3 => matches!(*item, Object::Boolean(_)),
						_ => false,
					};

					if !is_type {
						return Err(RuntimeErr(format!(
							"Expected type {} but received {}",
							get_type_name(type_id),
							item.get_type_name(),
						)));
					}
				},

				Instruction::Return => {
					let frame = self.frames.pop().unwrap();
					self.set_ip(frame.return_to as usize);

					// Remove all end values, except return
					self.stack.drain(frame.stack_start..self.stack.len()-1);
				},

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
		}

		if self.had_error {
			return Err(RuntimeErr("Error occured".into()));
		}
		Ok(())
	}

	fn dump_stack_trace(&self) {
		println!("=== Stack Trace ===");
		self.frames.iter()
			.rev()
			.for_each(|frame| {
				if let Some(id) = frame.identifier {
					println!("at {}()", self.env.functions[id as usize].identifier);
				} else {
					println!("at func()");
				}
			});
	}

	fn check_function_args_count(&self, args: u8) -> Result<(), RuntimeErr> {
		if args as usize > self.stack.len() - self.frames.last().unwrap().stack_start {
			return Err(RuntimeErr(format!(
				"Native Function requires {} arguments, but received {}",
				args,
				self.stack.len(),
			)));
		}
		
		Ok(())
	}

	#[inline]
	fn pop_2_check(&mut self) -> Result<(Object, Object), RuntimeErr> {
		let rhs = self.drop()?;
		let lhs = self.drop()?;

		VM::check_types_match(&lhs, &rhs)?;
		Ok((lhs, rhs))
	}
	
	#[inline]
	fn check_types_match(lhs: &Object, rhs: &Object) -> Result<(), RuntimeErr> {
		if lhs.is_similar(rhs) {
			return Ok(());
		}
		
		Err(RuntimeErr(format!(
			"Value types do not match or are not integers '{}' != '{}'",
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
}