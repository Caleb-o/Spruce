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
	pub code: Vec<Instruction>,
	pub constants: Vec<ConstantValue>,
}

impl Environment {
	pub fn new() -> Self {
		Self {
			code: Vec::new(),
			constants: Vec::new()
		}
	}

	pub fn add_op(&mut self, op: Instruction) {
		self.code.push(op);
	}

	pub fn add_constant(&mut self, constant: Object) -> u8 {
		// FIXME: Only allow unique constants
		self.constants.push(ConstantValue::Obj(constant));
		self.constants.len() as u8 - 1
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
		self.code.push(op);
		self.code.len() - 1
	}

	pub fn patch_jump_op(&mut self, index: usize) {
		let location = self.code.len();

		match self.code[index] {
			Instruction::Jump(ref mut loc) => {
				*loc = location;
			}
			Instruction::JumpNot(ref mut loc) => {
				*loc = location;
			}
			_ => {}
		}
	}

	// Print out the environment
	pub fn dump(&self) {
		println!("=== {} Constants ===", self.constants.len());
		for (index, constant) in self.constants.iter().enumerate() {
			println!("{:0>3}  {}", index, constant);
		}
		println!();

		println!("=== {} Instructions ===", self.code.len());

		for (index, instruction) in self.code.iter().enumerate() {
			print!("{:0>4}  ", index);
			
			match instruction {
				Instruction::Add => println!("Add"),
				Instruction::Sub => println!("Sub"),
				Instruction::Mul => println!("Mul"),
				Instruction::Div => println!("Div"),
				Instruction::EqualEqual => println!("EqualEqual"),
				Instruction::NotEqual => println!("NotEqual"),

				Instruction::Negate => println!("Negate"),

				Instruction::Greater => println!("Greater"),
				Instruction::GreaterEqual => println!("GreaterEqual"),
				Instruction::Less => println!("Less"),
				Instruction::LessEqual => println!("LessEqual"),

				Instruction::SetLocal(slot) => println!("SetLocal<{slot}>"),
				Instruction::GetLocal(slot) => println!("GetLocal<{slot}>"),
				
				Instruction::SetGlobal(slot) => println!("SetGlobal<{slot}>"),
				Instruction::GetGlobal(slot) => println!("GetGlobal<{slot}>"),

				Instruction::Push(loc) => println!("Push<{loc} :: '{}'>", self.constants[*loc as usize]),
				Instruction::Pop => println!("Pop"),

				Instruction::Jump(loc) => println!("Jump<{loc}>"),
				Instruction::JumpNot(loc) => println!("JumpNot<{loc}>"),

				Instruction::Call(loc, args) => println!("Call<{loc}, {args}>"),
				Instruction::CallNative(_, args) => println!("NativeCall<{args}>"),
				Instruction::Return => println!("Return"),
				Instruction::Halt => println!("Halt"),

				Instruction::None => println!("None"),
				Instruction::NoOp => println!("NoOp"),

				_ => todo!("Unimplemented Debug Instruction '{:?}'", *instruction),
			}
		}
	}
}