pub mod interpreter;

#[derive(Clone, Copy)]
pub struct FnValue {

}

#[derive(Clone)]
pub enum Value {
	Number(f32),
	String(String),
	Function(FnValue),
	Unit,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Number(n) => write!(f, "{}", n),
			Value::String(str) => write!(f, "{}", str),
			Value::Function(_) => write!(f, "<fn>"),
			
			Value::Unit => write!(f, "None"),
		}
    }
}