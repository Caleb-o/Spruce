use std::{fmt::Display, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    None,
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Function(u32),
    AnonFunction(u8, u32),
    Symbol(u16),
    List(Vec<Box<Object>>),
    StringMap(HashMap<String, Box<Object>>),
    Ref(Rc<Object>),
    // TODO: "Pointer" type, which holds its location in the stack
    //		 If we want a complex/large type, we don't really want to
    //		 duplicate it, especially if we want to mutate it.
}

impl Object {
    pub fn get_type_name(&self) -> &'static str {
        match *self {
            Object::None => "none",
            Object::Int(_) => "int",
            Object::Float(_) => "float",
            Object::String(_) => "string",
            Object::Boolean(_) => "bool",
            Object::List(_) => "list",
            Object::Function(_) => "function",
            Object::AnonFunction(_, _) => "anon",
            Object::Symbol(_) => "symbol",
            Object::StringMap(_) => "stringmap",
            Object::Ref(_) => "ref",
        }
    }

    pub fn is_exact(&self, other: &Object) -> bool {
        match *self {
            Self::None => true,
            Self::Int(v) => {
                if let Self::Int(o) = *other {
                    return v == o;
                }
                false
            },
            Self::Float(v) => {
                if let Self::Float(o) = *other {
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
            Self::Symbol(ref v) => {
                if let Self::Symbol(ref o) = other {
                    return *v == *o;
                }
                false
            }
            _ => false,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Object::None => "None".into(),
            Object::Int(v) => v.to_string(),
            Object::Float(v) => v.to_string(),
            Object::Boolean(v) => v.to_string(),
            Object::String(v) => v.clone(),
            Object::List(ref list) => {
                let mut string = String::from("[");
                
                for (idx, item) in list.iter().enumerate() {
                    string.push_str(&format!("{item}"));

                    if idx < list.len() - 1 {
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
            Object::StringMap(ref map) => {
                let mut string = String::from("{");
                
                for (idx, (key, value)) in map.iter().enumerate() {
                    string.push_str(&format!("{key}: {value}"));

                    if idx < map.len() - 1 {
                        string.push_str(", ");
                    }
                }

                string.push('}');
                string
            }
            Object::Ref(id) => format!("Ref<{id}>"),
        })
    }
}