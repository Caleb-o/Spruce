use crate::token::Token;

pub struct Ast {
    pub token: Token,
    pub data: AstData,
}

pub enum AstData {
    Literal,
    Identifier,
    ListLiteral(Vec<Box<Ast>>),

    BinaryOp { lhs: Box<Ast>, rhs: Box<Ast> },
    UnaryOp { rhs: Box<Ast> },
    LogicalOp { lhs: Box<Ast>, rhs: Box<Ast> },

    Parameter { type_name: Option<Box<Ast>>, },
    Function { anonymous: bool, parameters: Option<Vec<Box<Ast>>>, body: Box<Ast> },
    FunctionCall { arguments: Vec<Box<Ast>> },

    VarDeclaration { is_mutable: bool, expression: Option<Box<Ast>> },
    VarAssign(Box<Ast>),

    IfStatement { condition: Box<Ast>, true_body: Box<Ast>, false_body: Option<Box<Ast>> },
    ForStatement { variable: Option<Box<Ast>>, condition: Box<Ast>, increment: Option<Box<Ast>> },
    DoWhileStatement { body: Box<Ast>, condition: Box<Ast> },
    TrailingIfStatement { statement: Box<Ast>, condition: Box<Ast> },

    IndexGetter { expression: Box<Ast>, index: Box<Ast> },
    IndexSetter { expression: Box<Ast>, rhs: Box<Ast> },

    Return(Option<Box<Ast>>),
    Body(Vec<Box<Ast>>),
}

impl Ast {
    pub fn new_literal(token: Token) -> Box<Self> {
        Box::new(Ast { 
            token,
            data: AstData::Literal,
        })
    }

    pub fn new_identifier(token: Token) -> Box<Self> {
        Box::new(Ast { 
            token,
            data: AstData::Identifier,
        })
    }

    pub fn new_binary_op(token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Ast { 
            token,
            data: AstData::BinaryOp { lhs, rhs },
        })
    }

    pub fn new_unary_op(token: Token, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Ast { 
            token,
            data: AstData::UnaryOp { rhs },
        })
    }

    pub fn new_logical_op(token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Ast { 
            token,
            data: AstData::LogicalOp { lhs, rhs },
        })
    }
}