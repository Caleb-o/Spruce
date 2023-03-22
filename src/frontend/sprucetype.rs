use std::mem::discriminant;

#[derive(Debug, Clone)]
pub enum SpruceType {
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
            _ => discriminant(self) == discriminant(other),
        }
    }
}