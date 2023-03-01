use std::{fmt::Display, time::Instant};

use crate::{environment::{Environment, ConstantValue}, object::Object, instructions::Instruction, compiler::Function};

struct CallFrame {
	return_to: usize,
	stack_start: usize,
}

pub struct VM {
	had_error: bool,
	env: Box<Environment>,
	ip: usize,
	len: usize,
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
	pub fn new(env: Box<Environment>) -> Self {
		let len = env.op_here();
		
		Self {
			had_error: false,
			env,
			ip: 0,
			len,
			stack: Vec::with_capacity(512),
			frames: Vec::with_capacity(64),
			started: Instant::now(),
		}
	}

	pub fn stack_slice_from_call(&self) -> &[Object] {
		&self.stack[self.frames.last().unwrap().stack_start..]
	}

	pub fn stack_size(&self) -> usize {
		self.stack.len()
	}

	pub fn get_stack(&self) -> &Vec<Object> {
		&self.stack
	}

	pub fn drop(&mut self) -> Result<Object, RuntimeErr> {
		match self.stack.pop() {
			Some(o) => Ok(o),
			None => Err(RuntimeErr(
				"Trying to pop an empty stack".into()
			)),
		}
	}

	pub fn peek(&self) -> &Object {
		self.stack.last().unwrap()
	}

	pub fn push(&mut self, object: Object) {
		self.stack.push(object);
	}

	pub fn warning(&self, msg: String) {
		println!("Warning: {msg}");
	}

	pub fn run(&mut self) -> Result<(), RuntimeErr> {
		// Initial frame
		self.frames.push(CallFrame { return_to: 0, stack_start: 0 });
		
		while !self.had_error && self.ip < self.len {
			let code = match num::FromPrimitive::from_u8(self.env.code[self.ip]) {
				Some(c) => c,
				None => return Err(RuntimeErr(format!(
					"Could not cast byte '{}' to valid opcode",
					self.env.code[self.ip]
				))),
			};

			match code {
				Instruction::Constant => {
					let idx = self.get_byte_location();
					let constant = match self.env.constants[idx as usize] {
						ConstantValue::Obj(ref o) => o,
						_ => return Err(RuntimeErr(
							"Trying to push a non-object to the stack".into()	
						))
					};
					self.stack.push(constant.clone());
				}

				Instruction::ConstantLong => {
					let idx = self.get_short_location();
					let constant = match self.env.constants[idx as usize] {
						ConstantValue::Obj(ref o) => o,
						_ => return Err(RuntimeErr(
							"Trying to push a non-object to the stack".into()	
						))
					};
					self.stack.push(constant.clone());
				}

				Instruction::Halt => self.ip = self.len,
				Instruction::Negate => {
					let last = self.drop()?;
					
					if !matches!(last, Object::Number(_)) {
						return Err(RuntimeErr(format!(
							"Value type does not match or is not an integer '{last}'"
						)));
					}

					self.stack.push(Object::Number(match last { Object::Number(v) => -v, _ => unreachable!()}));
				},

				Instruction::BuildList => {
					let count = self.get_byte_location();
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
					let slot = self.get_short_location();
					self.stack.push(self.stack[slot as usize].clone());
				}
				
				Instruction::SetGlobal => {
					let slot = self.get_short_location();
					self.stack[slot as usize] = (*self.peek()).clone();
				}
				
				Instruction::GetLocal => {
					let slot = self.get_short_location();
					self.stack.push(self.stack[
						self.frames.last().unwrap().stack_start + slot as usize
						].clone()
					);
				}
				
				Instruction::SetLocal => {
					let slot = self.get_short_location();
					self.stack[
						self.frames.last().unwrap().stack_start + slot as usize
					] = (*self.peek()).clone();
				}

				Instruction::Jump => {
					self.ip = self.get_short_location() as usize;
					continue;
				}

				Instruction::JumpNot => {
					let top = self.drop()?;
					let loc = self.get_short_location();

					if !matches!(top, Object::Boolean(_)) {
						return Err(RuntimeErr(
							"Cannot use non-boolean in condition".into()
						));
					}

					if let Object::Boolean(v) = top {
						if !v {
							self.ip = loc as usize;
							continue;
						}
					}
				}
				
				Instruction::Call => {
					let args = self.get_byte_location();
					let loc = self.get_long_location();
					self.check_function_args_count(args)?;

					self.frames.push(CallFrame { 
						return_to: self.ip,
						stack_start: self.stack.len() - args as usize,
					});

					self.ip = loc as usize;
					continue;
				},

				Instruction::CallNative => {
					let args = self.get_byte_location();
					let loc = self.get_long_location();
					self.check_function_args_count(args)?;

					if let ConstantValue::Func(f) = self.env.constants[loc as usize].clone() {
						if let Function::Native { identifier: _, param_count: _, function, .. } = f {
							self.frames.push(CallFrame { 
								return_to: self.ip,
								stack_start: self.stack.len() - args as usize,
							});

							function(self, args)?;

							_ = self.frames.pop();
						}
					}
				},

				Instruction::Return => {
					let frame = self.frames.pop().unwrap();
					self.ip = frame.return_to;

					// Remove all end values, except return
					self.stack.drain(frame.stack_start..self.stack.len()-1);
				},

				Instruction::None => self.push(Object::None),
				Instruction::True => self.push(Object::Boolean(true)),
				Instruction::False => self.push(Object::Boolean(false)),

				Instruction::NoOp => {},

				_ => todo!(
					"Unimplemented instruction in VM '{:?}'",
					self.env.code[self.ip]
				),
			}

			self.ip += 1;
		}

		if self.had_error {
			return Err(RuntimeErr("Error occured".into()));
		}
		Ok(())
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

	fn pop_2_check(&mut self) -> Result<(Object, Object), RuntimeErr> {
		let rhs = self.drop()?;
		let lhs = self.drop()?;

		VM::check_types_match(&lhs, &rhs)?;
		Ok((lhs, rhs))
	}
	
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
	fn get_byte_location(&mut self) -> u8 {
		self.ip += 1;
		self.env.code[self.ip]
	}

	#[inline]
	fn get_short_location(&mut self) -> u16 {
		let a = self.env.code[self.ip + 1];
		let b = self.env.code[self.ip + 2];
		self.ip += 2;

		u16::from_be_bytes([a, b])
	}

	#[inline]
	fn get_long_location(&mut self) -> u32 {
		let a = self.env.code[self.ip + 1];
		let b = self.env.code[self.ip + 2];
		let c = self.env.code[self.ip + 3];
		let d = self.env.code[self.ip + 4];
		self.ip += 4;

		u32::from_be_bytes([a, b, c, d])
	}
}