use crate::ast::Ast;

#[derive(Debug)]
pub enum TypeKind {
    Standard,
    Tuple(Vec<Box<Ast>>),
    Array(Box<Ast>),
    Lazy(Box<Ast>),
    ErrorOrValue(Box<Ast>, Box<Ast>),
    Function {
        parameters: Option<Vec<Box<Ast>>>,
        return_type: Box<Ast>,
    },
}
