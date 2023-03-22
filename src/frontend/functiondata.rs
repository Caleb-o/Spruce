use super::{sprucetype::SpruceType, token::Span};

pub type ParamTypes = Option<Vec<SpruceType>>;

#[derive(Debug, Clone)]
pub struct FunctionMeta {
    pub identifier: Span,
    // TODO: Change to Parameters - with types
    pub function: Function,
}

impl FunctionMeta {
    pub fn new(identifier: Span, function: Function) -> Self {
        Self { identifier, function }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Function {
    User {
        meta_id: u32,
        param_types: ParamTypes,
        return_type: SpruceType,
        empty: bool,
    },
    Native {
        identifier: String,
        param_types: ParamTypes,
        return_type: SpruceType,
    },
}

impl Function {
    pub fn param_count(&self) -> u8 {
        match self {
            Function::User { param_types, .. } => {
                match param_types {
                    Some(p) => p.len() as u8,
                    None => 0,
                }
            }
            Function::Native { param_types, .. } => {
                match param_types {
                    Some(p) => p.len() as u8,
                    None => 0,
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