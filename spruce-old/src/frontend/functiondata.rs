use std::rc::Rc;

use crate::nativefns::ParamKind;
use super::sprucetype::SpruceType;

pub type ParamTypes = Option<Vec<Rc<SpruceType>>>;

#[derive(Debug, Clone)]
pub struct FunctionMeta {
    pub identifier: String,
    // TODO: Change to Parameters - with types
    pub function: Function,
}

impl FunctionMeta {
    pub fn new(identifier: String, function: Function) -> Self {
        Self { identifier, function }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Function {
    User {
        identifier: String,
        param_types: ParamTypes,
        return_type: Rc<SpruceType>,
        empty: bool,
    },
    Native {
        identifier: &'static str,
        param_types: ParamKind,
        return_type: Rc<SpruceType>,
    },
}

impl Function {
    pub fn to_type(&self) -> SpruceType {
        match self {
            Self::User { identifier, param_types, return_type, empty: _ } => {
                SpruceType::Function {
                    is_native: false,
                    identifier: identifier.clone(),
                    parameters: param_types.as_ref()
                        .map_or_else(|| None,
                            |p| Some(
                            p.iter()
                            .map(|s| Rc::clone(s))
                            .collect()
                        )),
                    return_type: Rc::clone(return_type),
                }
            }
            Self::Native { identifier, param_types, return_type } => {
                SpruceType::Function { 
                    is_native: true,
                    identifier: identifier.to_string(),
                    parameters: match param_types {
                        ParamKind::Any => Some(vec![Rc::new(SpruceType::Any)]),
                        ParamKind::None => None,
                        ParamKind::With(params) => {
                            Some(params.iter().map(|t| Rc::new(t.clone())).collect())
                        }
                    },
                    return_type: Rc::clone(return_type),
                }
            }
        }
    }

    pub fn mark_empty(&mut self) {
        if let Function::User { ref mut empty, .. } = * self {
            *empty = true;
        } 
    }
}