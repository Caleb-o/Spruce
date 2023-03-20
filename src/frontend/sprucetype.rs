#[derive(Debug, Clone, PartialEq, Eq)]
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