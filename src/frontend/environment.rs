use std::fmt::Display;

use crate::{object::Object, nativefns::ParamKind};

use super::{functiondata::{Function, FunctionMeta}, decorated_ast::DecoratedAst};

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
                if let Function::Native { identifier, param_types, .. } = func {
                    format!("<Function {identifier}({})>", match param_types {
                        ParamKind::Any => "any".into(),
                        ParamKind::None => "none".into(),
                        ParamKind::With(ref types) => {
                            let mut type_string = String::new();

                            for (idx, typ) in types.iter().enumerate() {
                                type_string.push_str(&format!("{typ:?}"));
                                if idx < types.len() - 1 {
                                    type_string.push_str(", ".into());
                                }
                            }

                            type_string
                        },
                    })
                } else {
                    "<Function>".into()
                }
            },
        })
    }
}

#[derive(Clone)]
pub struct Environment {
    pub program: Box<DecoratedAst>,
    pub constants: Vec<ConstantValue>,
    pub functions: Vec<FunctionMeta>,
}

impl Environment {
    pub fn new(program: Box<DecoratedAst>) -> Self {
        Self {
            program,
            constants: Vec::new(),
            functions: Vec::new(),
        }
    }
}