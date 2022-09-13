use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
	None,
	Int(i32),
	String(String),
	Boolean(bool),
	Array(Vec<Box<Object>>),
	// Bytecode location, Parameter Count
	Function(usize, u8),
	// TODO: "Pointer" type, which holds its location in the stack
	//		 If we want a complex/large type, we don't really want to
	//		 duplicate it, especially if we want to mutate it.
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
			Object::None => "None".into(),
			Object::Int(v) => v.to_string(),
			Object::Boolean(v) => v.to_string(),
			Object::String(v) => v.clone(),
			Object::Array(ref arr) => {
				let mut string = String::from("[");
				
				for i in 0..arr.len() {
					string.push_str(&format!("{}", arr[i]));

					if i < arr.len() - 1 {
						string.push_str(", ");
					}
				}

				string.push(']');
				string
			},
			Object::Function(loc, paramc) => {
				format!("fn<{}, ({})>", loc, paramc)
			},
		})
    }
}