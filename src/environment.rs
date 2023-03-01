use std::fmt::Display;

use crate::{instructions::{Instruction, ParamKind}, object::Object, compiler::Function};


#[derive(Clone)]
pub enum ConstantValue {
	Obj(Object),
	Func(Function),
}

impl Display for ConstantValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
			ConstantValue::Obj(o) => o.to_string(),
			ConstantValue::Func(ref func) => {
				if let Function::Native { identifier, param_count, .. } = func {
					format!("<Function {identifier}({})>", match param_count {
						ParamKind::Any => "any".into(),
						ParamKind::Count(c) => c.to_string(),
					})
				} else {
					"<Function>".into()
				}
			},
		})
    }
}

pub struct Environment {
	pub code: Vec<u8>,
	pub constants: Vec<ConstantValue>,
}

impl Environment {
	pub fn new() -> Self {
		Self {
			code: Vec::new(),
			constants: Vec::new(),
		}
	}

	pub fn add_opb(&mut self, op: u8) {
		self.code.push(op);
	}

	pub fn add_op(&mut self, op: Instruction) {
		self.code.push(op as u8);
	}

	pub fn add_type_check(&mut self, id: u8) {
		self.code.push(Instruction::TypeCheck as u8);
		self.code.push(id);
	}

	pub fn add_local(&mut self, op: Instruction, location: u16) {
		self.code.push(op as u8);
		location.to_be_bytes().into_iter().for_each(|b| self.code.push(b));
	}

	pub fn add_call(&mut self, args: u8, location: u32) {
		self.code.push(Instruction::Call as u8);
		self.code.push(args);

		location.to_be_bytes().into_iter().for_each(|b| self.code.push(b));
	}

	pub fn add_call_native(&mut self, args: u8, location: u32) {
		self.code.push(Instruction::CallNative as u8);
		self.code.push(args);

		location.to_be_bytes().into_iter().for_each(|b| self.code.push(b));
	}

	pub fn add_constant(&mut self, constant: Object) {
		// FIXME: Only allow unique constants
		let idx = match self.find_constant(&constant) {
			Some(idx) => idx,
			None => {
				self.constants.push(ConstantValue::Obj(constant));
				self.constants.len() - 1
			}
		};

		if idx > 255 {
			// 16-bit location
			self.add_op(Instruction::ConstantLong);
			let bytes = (idx as u16).to_be_bytes();
			self.add_opb(bytes[0]);
			self.add_opb(bytes[1]);
		} else {
			// 8-bit location
			self.add_op(Instruction::Constant);
			self.add_opb(idx as u8);
		}
	}

	fn find_constant(&self, obj: &Object) -> Option<usize> {
		for (i, item) in self.constants.iter().enumerate() {
			if let ConstantValue::Obj(ref o) = item {
				if obj.is_exact(o) {
					return Some(i);
				}
			}
		}

		None
	}

	pub fn find_constant_func_loc(&self, id: &str) -> u8 {
		for (index, constant) in self.constants.iter().enumerate() {
			if let ConstantValue::Func(ref func) = constant {
				if let Function::Native { identifier, .. } = func {
					if id == *identifier {
						return index as u8;
					}
				}
			}
		}
		
		0
	}

	pub fn add_constant_function(&mut self, constant: ConstantValue) {
		self.constants.push(constant);
	}

	pub fn op_here(&self) -> usize {
		self.code.len()
	}

	pub fn add_jump_op(&mut self, op: Instruction) -> usize {
		self.code.push(op as u8);
		self.code.push(0);
		self.code.push(0);

		if self.code.len() >= u32::MAX as usize {
			self.code.push(0);
			self.code.push(0);
			return self.code.len() - 5;
		}

		self.code.len() - 3
	}

	pub fn patch_jump_op(&mut self, index: usize) {
		self.patch_jump_op_to(index, self.code.len());
	}

	pub fn patch_jump_op_to(&mut self, index: usize, location: usize) {
		match num::FromPrimitive::from_u8(self.code[index]).unwrap() {
			Instruction::Jump => {
				let bytes = (location as u16).to_be_bytes();
				self.code[index + 1] = bytes[0];
				self.code[index + 2] = bytes[1];
			}
			Instruction::JumpNot => {
				let bytes = (location as u16).to_be_bytes();
				self.code[index + 1] = bytes[0];
				self.code[index + 2] = bytes[1];
			}
			// TODO: Long Jumps
			_ => {}
		}
	}

	// Print out the environment
	pub fn dump(&self) {
		println!("=== {} Constants ===", self.constants.len());
		for (index, constant) in self.constants.iter().enumerate() {
			println!("{:0>3}  \"{}\"", index, constant);
		}
		println!();

		println!("=== {} Instructions ===", self.code.len());

		let mut offset = 0;

		while offset < self.code.len() {
			let instruction = self.code[offset];
			print!("{:0>4}  ", offset);
			
			offset = match num::FromPrimitive::from_u8(instruction).unwrap() {
				Instruction::Add => simple_instruction("ADD", offset),
				Instruction::Sub => simple_instruction("SUB", offset),
				Instruction::Mul => simple_instruction("MUL", offset),
				Instruction::Div => simple_instruction("DIV", offset),
				Instruction::EqualEqual => simple_instruction("EQUAL_EQUAL", offset),
				Instruction::NotEqual => simple_instruction("NOT_EQUAL", offset),

				Instruction::Negate => simple_instruction("NEGATE", offset),

				Instruction::Greater => simple_instruction("GREATE", offset),
				Instruction::GreaterEqual => simple_instruction("GREATER_EQUAL", offset),
				Instruction::Less => simple_instruction("LESS", offset),
				Instruction::LessEqual => simple_instruction("LESS_EQUAL", offset),

				Instruction::SetLocal => short_location_instruction("SET_LOCAL", offset, &self),
				Instruction::GetLocal => short_location_instruction("GET_LOCAL", offset, &self),
				
				Instruction::SetGlobal => short_location_instruction("SET_GLOBAL", offset, &self),
				Instruction::GetGlobal => short_location_instruction("GET_GLOBAL", offset, &self),

				Instruction::Constant => constant_instruction("CONSTANT", offset, &self),
				Instruction::ConstantLong => long_constant_instruction("CONSTANT_LONG", offset, &self),

				Instruction::Jump => short_location_instruction("JUMP", offset, &self),
				Instruction::JumpNot => short_location_instruction("JUMP_NOT", offset, &self),

				Instruction::Call => call_instruction("CALL", offset, &self),
				Instruction::CallNative => call_instruction("NATIVE_CALL", offset, &self),
				Instruction::Return => simple_instruction("RETURN", offset),
				Instruction::Halt => simple_instruction("HALT", offset),

				Instruction::TypeCheck => type_instruction("TYPE_CHECK", offset, &self),

				Instruction::None => simple_instruction("NONE", offset),
				Instruction::True => simple_instruction("TRUE", offset),
				Instruction::False => simple_instruction("FALSE", offset),
				Instruction::NoOp => simple_instruction("NO_OP", offset),

				_ => todo!("Unimplemented Debug Instruction '{:?}'", instruction),
			}
		}
	}
}

fn simple_instruction(name: &str, offset: usize) -> usize {
	println!("{name}");
	offset + 1
}

fn constant_instruction(name: &str, offset: usize, env: &Environment) -> usize {
	let location = env.code[offset + 1];
	let obj = &env.constants[location as usize];
	println!("{name}<{location} :: '{obj}'>");
	offset + 2
}

fn long_constant_instruction(name: &str, offset: usize, env: &Environment) -> usize {
	let location = u16::from_be_bytes([env.code[offset + 1], env.code[offset + 2]]);
	let obj = &env.constants[location as usize];
	println!("{name}<{location} :: '{obj}'>");
	offset + 3
}

fn short_location_instruction(name: &str, offset: usize, env: &Environment) -> usize {
	let location = u16::from_be_bytes([env.code[offset + 1], env.code[offset + 2]]);
	println!("{name}<{location}>");
	offset + 3
}

fn call_instruction(name: &str, offset: usize, env: &Environment) -> usize {
	let args = env.code[offset + 1];
	let location = u32::from_be_bytes([
		env.code[offset + 2],
		env.code[offset + 3],
		env.code[offset + 4],
		env.code[offset + 5],
	]);

	println!("{name}<{args}, {location}>");

	offset + 6
}

fn type_instruction(name: &str, offset: usize, env: &Environment) -> usize {
	let type_name = match env.code[offset + 1] {
		0 => "none",
		1 => "number",
		2 => "string",
		3 => "bool",
		_ => unreachable!(),
	};

	println!("{name}<{type_name}>");
	offset + 2
}