use crate::nativefns::ParamKind;
use super::sprucetype::SpruceType;

pub type ParamTypes = Option<Vec<SpruceType>>;

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
        return_type: SpruceType,
        empty: bool,
    },
    Native {
        identifier: &'static str,
        param_types: ParamKind,
        return_type: SpruceType,
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
                            .map(|s| Box::new(s.clone()))
                            .collect()
                        )),
                    return_type: Box::new(return_type.clone())
                }
            }
            Self::Native { identifier, param_types, return_type } => {
                SpruceType::Function { 
                    is_native: true,
                    identifier: identifier.to_string(),
                    parameters: match param_types {
                        ParamKind::Any => Some(vec![Box::new(SpruceType::Any)]),
                        ParamKind::None => None,
                        ParamKind::With(params) => {
                            Some(params.iter().map(|t| Box::new(t.clone())).collect())
                        }
                    },
                    return_type: Box::new(return_type.clone())
                }
            }
        }
    }

    pub fn param_count(&self) -> i16 {
        match self {
            Function::User { param_types, .. } => {
                match param_types {
                    Some(p) => p.len() as i16,
                    None => 0,
                }
            }
            Function::Native { param_types, .. } => {
                match param_types {
                    ParamKind::Any => -1,
                    ParamKind::None => 0,
                    ParamKind::With(params) => params.len() as i16,
                }
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        if let Function::User { empty, .. } = *self {
            empty
        } else {
            false
        }
    }

    pub fn mark_empty(&mut self) {
        if let Function::User { ref mut empty, .. } = * self {
            *empty = true;
        } 
    }
}