use std::{mem::discriminant, fmt::Display, rc::Rc};

use super::token::Token;

#[derive(Debug, Clone)]
pub struct StructField {
    pub identifier: Token,
    pub kind: Rc<SpruceType>,
    pub has_default: bool,
}

// FIXME: Remove clone
#[derive(Debug, Clone)]
pub enum SpruceType {
    Error,
    Unresolved,
    None,
    Any,
    Int,
    Float,
    Bool,
    String,
    Tuple(Vec<Rc<SpruceType>>),
    Array(Rc<SpruceType>),
    Symbol,
    Lazy(Rc<SpruceType>),
    ErrorOrValue(Rc<SpruceType>, Rc<SpruceType>),
    Function {
        is_native: bool,
        identifier: String,
        parameters: Option<Vec<Rc<SpruceType>>>,
        return_type: Rc<SpruceType>,
    },
    Struct {
        is_ref: bool,
        identifier: Option<String>,
        fields: Option<Vec<StructField>>,
        methods: Option<Vec<Rc<SpruceType>>>,
    },
}

impl SpruceType {
    pub fn is_same(&self, other: &SpruceType) -> bool {
        match self {
            Self::Any => true,
            Self::Array(k) => {
                if discriminant(self) != discriminant(other) {
                    return false;
                }

                let Self::Array(j) = other else { unreachable!() };
                k.is_same(j)
            }
            Self::Tuple(k) => {
                if discriminant(self) != discriminant(other) {
                    return false;
                }

                let Self::Tuple(j) = other else { unreachable!() };
                
                if k.len() != j.len() {
                    return false;
                }

                for (lhs, rhs) in k.iter().zip(j) {
                    if !lhs.is_same(rhs) {
                        return false;
                    }
                }

                true
            }
            Self::Lazy(inner) => {
                if discriminant(self) != discriminant(other) {
                    return false;
                }

                let Self::Lazy(other_inner) = other else { unreachable!() };
                inner.is_same(other_inner)
            }
            Self::Function { parameters, return_type, .. } => {
                if discriminant(self) != discriminant(other) {
                    return false;
                }
                
                let s_parameters = parameters;
                let s_return_type = return_type;

                let Self::Function { parameters, return_type, .. } = other else { unreachable!() };

                if (s_parameters.is_some() && parameters.is_none()) ||
                    (s_parameters.is_none() && parameters.is_some()) {
                    return false;
                }

                match s_parameters {
                    Some(ref s_parameters) => {
                        let parameters = parameters.as_ref().unwrap(); 

                        if s_parameters.len() != parameters.len() {
                            return false;
                        }
        
                        for (lhs, rhs) in s_parameters.iter().zip(parameters) {
                            if !lhs.is_same(rhs) {
                                return false;
                            }
                        }
                    }
                    _ => {}
                }

                if !s_return_type.is_same(return_type) {
                    return false;
                }

                true
            }
            Self::Struct { identifier, .. } => {
                if discriminant(self) != discriminant(other) {
                    return false;
                }
                
                let s_identifier = identifier;
                let Self::Struct { identifier, .. } = other else { unreachable!() };
                
                s_identifier.as_ref().unwrap() == identifier.as_ref().unwrap()
            },
            Self::ErrorOrValue(lhs, rhs) => {
                if discriminant(self) != discriminant(other) {
                    // Check if other type matches one of the variants
                    return lhs.is_same(other) || rhs.is_same(other);
                }

                let Self::ErrorOrValue(olhs, orhs) = other else { unreachable!() };
                lhs.is_same(olhs) && rhs.is_same(orhs)
            }
            _ => discriminant(self) == discriminant(other),
        }
    }
}

impl Display for SpruceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::None => "none".into(),
            Self::Any => "any".into(),
            Self::Int => "int".into(),
            Self::Float => "float".into(),
            Self::Bool => "bool".into(),
            Self::String => "string".into(),
            Self::Symbol => "symbol".into(),
            Self::Array(inner) => format!("[{}]", inner),
            Self::Tuple(inner) => {
                let mut tuplestr = String::from("(");

                for (idx, param) in inner.iter().enumerate() {
                    tuplestr.push_str(&format!("{}", param));
                    
                    if idx < inner.len() - 1 {
                        tuplestr.push_str(", ");
                    }
                }
                
                tuplestr.push(')');
                tuplestr
            },
            Self::Lazy(inner) => format!("lazy {inner}"),
            Self::ErrorOrValue(lhs, rhs) => format!("{lhs}!{rhs}"),
            Self::Function { parameters, return_type, .. } => {
                let mut fnstr = String::from("fn(");

                if let Some(parameters) = parameters {
                    for (idx, param) in parameters.iter().enumerate() {
                        fnstr.push_str(&format!("{}", param));
                        
                        if idx < parameters.len() - 1 {
                            fnstr.push_str(", ");
                        }
                    }
                }
                
                fnstr.push_str(&format!("): {}", return_type));
                fnstr
            },
            Self::Struct { identifier, .. } => identifier.as_ref().unwrap().clone(),
            n @ _ => unimplemented!("{n:?}"),
        })
    }
}