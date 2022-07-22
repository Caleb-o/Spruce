use std::rc::Rc;

use crate::parsing::ast::{FunctionDefinition, FunctionCall};

use self::interpreter::Interpreter;

pub mod interpreter;

trait Callable {
	fn call(&self, interpreter: &mut Interpreter, call: &FunctionCall) -> Rc<Value>;
}

pub struct FnValue {
	definition: FunctionDefinition,
}

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

impl Callable for Rc<Value> {
	fn call(&self, interpreter: &mut Interpreter, call: &FunctionCall) -> Rc<Value> {
		match **self {
			Value::Number(num) => {
				for arg in call.arguments.iter() {
					let result = interpreter.visit(arg);
					// Call the function num amount of times
					match *result {
						Value::Function(_) => {
							for _ in 0..(num as i32) {
								_ = result.call(interpreter, call)
							}
						},
						_ => {},
					}
				}

				Rc::clone(&self)
			},
			
			Value::Function(ref func) => call_func(interpreter, call, func),

			Value::String(_)
			| Value::Unit => Rc::clone(&self),
		}
	}
}

fn call_func(interpreter: &mut Interpreter, call: &FunctionCall, func: &FnValue) -> Rc<Value> {
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

	let ret_value = interpreter.visit(&func.definition.body);
	interpreter.end();

	return ret_value;
}