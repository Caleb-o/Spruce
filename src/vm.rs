use std::fmt::Display;

use crate::{environment::{Environment, ConstantValue}, object::Object, instructions::{Instruction, ParamKind}, compiler::{Function, CompilerErr}};

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
}

pub struct RuntimeErr(pub String);

impl Display for RuntimeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl VM {
	pub fn new(env: Box<Environment>) -> Self {
		let entry = env.entry;
		let len = env.op_here();
		
		Self {
			had_error: false,
			env,
			ip: entry,
			len,
			stack: Vec::new(),
			frames: Vec::new(),
		}
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

	pub fn peek_mut(&mut self) -> &mut Object {
		self.stack.last_mut().unwrap()
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
			match self.env.code[self.ip] {
				Instruction::Push(idx) => {
					let constant = match self.env.constants[idx as usize] {
						ConstantValue::Obj(ref o) => o,
						_ => return Err(RuntimeErr(
							"Trying to push a non-object to the stack".into()	
						))
					};
					self.stack.push(constant.clone());
				}

				Instruction::Pop => _ = self.drop()?,
				Instruction::PopN(count) => {
					for _ in 0..count {
						_ = self.drop()?;
					}
				},
				Instruction::Halt => self.ip = self.len,
				Instruction::Negate => {
					let last = self.drop()?;
					
					if !matches!(last, Object::Int(_)) {
						return Err(RuntimeErr(format!(
							"Value type does not match or is not an integer '{}'",
							last
						)));
					}

					self.stack.push(Object::Int(match last { Object::Int(v) => -v, _ => unreachable!()}));
				},

				Instruction::Greater => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Boolean(l > r));
						}
					}
				}

				Instruction::GreaterEqual => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Boolean(l >= r));
						}
					}
				}

				Instruction::Less => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Boolean(l < r));
						}
					}
				}

				Instruction::LessEqual => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Boolean(l <= r));
						}
					}
				}

				Instruction::Add => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Int(l + r));
						}
					}
				}

				Instruction::Sub => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Int(l - r));
						}
					}
				}

				Instruction::Mul => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Int(l * r));
						}
					}
				}

				Instruction::Div => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							if r == 0 {
								return Err(RuntimeErr("Trying to divide by 0".into()));
							}

							self.stack.push(Object::Int(l / r));
						}
					}
				}

				Instruction::Equal => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Boolean(l == r));
						}
					}
				}

				Instruction::NotEqual => {
					let (lhs, rhs) = self.pop_2_check()?;

					if let Object::Int(l) = lhs {
						if let Object::Int(r) = rhs {
							self.stack.push(Object::Boolean(l != r));
						}
					}
				}

				Instruction::GetLocal(slot) => {
					self.stack.push(self.stack[slot as usize].clone());
				}

				Instruction::SetLocal(slot) => {
					self.stack[slot as usize] = self.peek().clone();
				}

				Instruction::Jump(loc) => {
					self.ip = loc;
					continue;
				}

				Instruction::JumpNot(loc) => {
					let top = self.stack.pop().unwrap();

					if !matches!(top, Object::Boolean(_)) {
						return Err(RuntimeErr(
							"Cannot use non-boolean in condition".into()
						));
					}

					if let Object::Boolean(v) = top {
						if !v {
							self.ip = loc;
							continue;
						}
					}
				}
				
				Instruction::Call(loc, args) => {
					self.check_function_args_count(args)?;

					self.frames.push(CallFrame { 
						return_to: self.ip,
						stack_start: self.stack.len() - args,
					});

					self.ip = loc;
					continue;
				},

				Instruction::CallNative(loc, args) => {
					self.check_function_args_count(args)?;

					let func = self.env.constants[loc as usize].clone();
					match func {
						ConstantValue::Func(ref f) => {
							match f {
								Function::Native { identifier: _, param_count: _, function } => {
									function(self, args)?;
								},
								_ => {}
							}
						},
						_ => {}
					}

					self.push(Object::None);
				},

				Instruction::Return(count) => {
					// None value is pushed by default
					if count == 0 {
						self.push(Object::None);
					}

					let frame = self.frames.pop().unwrap();
					self.ip = frame.return_to;
				},

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

	fn check_function_args_count(&self, args: usize) -> Result<(), RuntimeErr> {
		if args > self.stack.len() - self.frames.last().unwrap().stack_start {
			return Err(RuntimeErr(format!(
				"Native Function requires {} arguments, but the stack only contains {}",
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
		if !matches!(lhs, Object::Int(_)) || !matches!(rhs, Object::Int(_)) {
			return Err(RuntimeErr(format!(
				"Value types do not match or are not integers '{}' != '{}'",
				lhs, rhs,
			)));
		}

		Ok(())
	}
}