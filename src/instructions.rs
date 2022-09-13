use std::fmt::Display;

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

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Instruction {
	Push(u8),
	Pop, 
	PopN(u8),

	GetLocal(u8),
	SetLocal(u8),
	
	// Globals are just top-level
	// They are not stored in a special location
	GetGlobal(u8),
	SetGlobal(u8),

	Jump(usize),
	JumpNot(usize),

	Equal, NotEqual,
	Less, LessEqual,
	Greater, GreaterEqual,

	Negate, Add, Sub, Mul, Div,

	// Bytecode Location, ArgCount
	Call(usize, u8),
	// Native index, ArgCount
	CallNative(u8, ParamKind),

	// Return ValueCount
	Return(u8),
	Halt,

	NoOp,
}