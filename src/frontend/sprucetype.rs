use std::{mem::discriminant, fmt::Display};

#[derive(Debug, Clone)]
pub enum SpruceType {
    Error,
    None,
    Any,
    Int,
    Float,
    Bool,
    String,
    Tuple(Vec<Box<SpruceType>>),
    List(Box<SpruceType>),
    Function {
        parameters: Option<Vec<Box<SpruceType>>>,
        return_type: Box<SpruceType>,
    },
}

impl SpruceType {
    pub fn is_same(&self, other: &SpruceType) -> bool {
        match self {
            Self::Any => true,
            // TODO: List, Function
            Self::List(k) => {
                if discriminant(self) != discriminant(other) {
                    return false;
                }

                let Self::List(j) = other else { unreachable!() };
                k.is_same(j)
            }
            Self::Function { parameters, return_type } => {
                if discriminant(self) != discriminant(other) {
                    return false;
                }

                let s_parameters = parameters;
                let s_return_type = return_type;

                let SpruceType::Function { parameters, return_type } = other else { unreachable!() };

                if (s_parameters.is_some() && parameters.is_none()) ||
                    (s_parameters.is_none() && parameters.is_some()) {
                    return false;
                }

                let s_parameters = s_parameters.as_ref().unwrap();
                let parameters = parameters.as_ref().unwrap();

                if s_parameters.len() != parameters.len() {
                    return false;
                }

                for (lhs, rhs) in s_parameters.iter().zip(parameters) {
                    if !lhs.is_same(rhs) {
                        return false;
                    }
                }

                if !s_return_type.is_same(return_type) {
                    return false;
                }

                true
            }
            _ => discriminant(self) == discriminant(other),
        }
    }
}

impl Display for SpruceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Any => "any".into(),
            Self::Int => "int".into(),
            Self::Float => "float".into(),
            Self::Bool => "bool".into(),
            Self::String => "string".into(),
            Self::List(inner) => format!("[{}]", inner),
            Self::Function { parameters, return_type } => {
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
            _ => unimplemented!(),
        })
    }
}