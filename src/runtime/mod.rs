use crate::parsing::ast::{Node, FunctionDefinition, FunctionCall};

use self::interpreter::Interpreter;

pub mod interpreter;

trait Callable {
	fn call(&self, interpreter: &mut Interpreter, call: &FunctionCall) -> Value;
}

#[derive(Clone)]
pub struct FnValue {
	definition: FunctionDefinition,
	body: Node,
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

impl Callable for Value {
	fn call(&self, interpreter: &mut Interpreter, call: &FunctionCall) -> Value {
		match self {
			Value::Number(num) => {
				for arg in call.arguments.iter() {
					let result = interpreter.visit(arg);
					// Call the function num amount of times
					match result {
						Value::Function(_) => {
							for _ in 0..(*num as i32) {
								_ = result.call(interpreter, call)
							}
						},
						_ => {},
					}
				}

				self.clone()
			},
			
			Value::Function(ref func) => call_func(interpreter, call, func),

			Value::String(_)
			| Value::Unit => self.clone(),
		}
	}
}

fn call_func(interpreter: &mut Interpreter, call: &FunctionCall, func: &FnValue) -> Value {
	interpreter.begin();

	for (idx, arg) in call.arguments.iter().enumerate() {
		let value = interpreter.visit(arg);
		
		if idx < func.definition.parameters.len() {
			interpreter.variables.last_mut().unwrap().insert(
				func.definition.parameters[idx].lexeme.to_string(),
				value
			);
		}
	}

	let ret_value = interpreter.visit(&func.body);
	interpreter.end();

	return ret_value;
}