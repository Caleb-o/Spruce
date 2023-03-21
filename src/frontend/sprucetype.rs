use std::mem::discriminant;

#[derive(Debug, Clone)]
pub enum SpruceType {
    None,
    Any,
    Int,
    Float,
    Bool,
    String,
    List(Box<SpruceType>),
    Function {
        parameters: Option<Vec<Box<SpruceType>>>,
        return_type: Box<SpruceType>,
    },
}

impl SpruceType {
    pub fn is_same(&self, other: &SpruceType) -> bool {
        match self {
            // TODO: List, Function
            _ => discriminant(self) == discriminant(other),
        }
    }
}