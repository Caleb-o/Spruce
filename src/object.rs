use std::{fmt::Display, hash::Hash};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
	None,
	Number(f32),
	String(String),
	Boolean(bool),
	List(Vec<Box<Object>>),
	// Bytecode location, Parameter Count
	Function(u8, usize),
	// TODO: "Pointer" type, which holds its location in the stack
	//		 If we want a complex/large type, we don't really want to
	//		 duplicate it, especially if we want to mutate it.
}

impl Object {
	pub fn is_similar(&self, other: &Object) -> bool {
		if self != other {
			return false;
		}

		match &*self {
			Self::Number(v) => *v == match *other {
				Self::Number(v) => v,
				_ => unreachable!(),
			},
			Self::Boolean(v) => *v == match *other {
				Self::Boolean(v) => v,
				_ => unreachable!(),
			},
			Self::String(v) => v == match *other {
				Self::String(ref v) => v,
				_ => unreachable!(),
			},
			_ => false,
		}
	}
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
			Object::None => "None".into(),
			Object::Number(v) => v.to_string(),
			Object::Boolean(v) => v.to_string(),
			Object::String(v) => v.clone(),
			Object::List(ref list) => {
				let mut string = String::from("[");
				
				for i in 0..list.len() {
					string.push_str(&format!("{}", list[i]));

					if i < list.len() - 1 {
						string.push_str(", ");
					}
				}

				string.push(']');
				string
			},
			Object::Function(paramc, loc) => {
				format!("fn<({paramc}), {loc}>")
			},
		})
    }
}