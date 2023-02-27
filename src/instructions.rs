use std::fmt::Display;

use num_derive::FromPrimitive;

#[derive(Debug, Clone, Copy)]
pub enum ParamKind {
	Any,
	Count(u8),
}

impl Display for ParamKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match *self {
			ParamKind::Any => "any".into(),
			ParamKind::Count(c) => format!("{c}"),
		})
    }
}

#[derive(Debug, Clone, Copy, FromPrimitive)]
#[repr(u8)]
pub enum Instruction {
	Constant,
	ConstantLong,

	GetLocal, // u16
	SetLocal, // u16
	
	// Globals are just top-level
	// They are not stored in a special location
	GetGlobal, // u16
	SetGlobal, // u16

	// BuildList Size
	BuildList, // u8

	Jump,		// u16
	JumpNot,	// u16

	JumpLong,		// u32
	JumpLongNot,	// u32	

	EqualEqual, NotEqual,
	Less, LessEqual,
	Greater, GreaterEqual,

	Negate, Add, Sub, Mul, Div,

	None,

	Call, 		// ArgCount u8, Location u32
	CallNative, // ArgCount u8, index u32

	// Return
	Return,
	Halt,

	NoOp,
}