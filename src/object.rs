use std::fmt::Display;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
	None,
	Number(f32),
	String(String),
	Boolean(bool),
	List(Vec<Box<Object>>),
	Function(u32),
	AnonFunction(u8, u32),
	Symbol(u16),
	// TODO: "Pointer" type, which holds its location in the stack
	//		 If we want a complex/large type, we don't really want to
	//		 duplicate it, especially if we want to mutate it.
}

impl Object {
	pub fn get_type_name(&self) -> &'static str {
		match *self {
			Object::None => "none",
    		Object::Number(_) => "number",
    		Object::String(_) => "string",
    		Object::Boolean(_) => "bool",
    		Object::List(_) => "list",
    		Object::Function(_) => "function",
    		Object::AnonFunction(_, _) => "anon",
			Object::Symbol(_) => "symbol",
		}
	}

	pub fn is_exact(&self, other: &Object) -> bool {
		match *self {
			Self::None => true,
			Self::Number(v) => {
				if let Self::Number(o) = *other {
					return v == o;
				}
				false
			},
			Self::Boolean(v) => {
				if let Self::Boolean(o) = *other {
					return v == o;
				}
				false
			},
			Self::String(ref v) => {
				if let Self::String(ref o) = other {
					return v.eq(o);
				}
				false
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
			}
			Object::Function(meta_id) => {
				format!("fn<{meta_id}>")
			}
			Object::AnonFunction(arg_count, location) => {
				format!("fn<{arg_count}, {location}>")
			}
			Object::Symbol(value) => format!("Symbol<{value}>"),
		})
    }
}