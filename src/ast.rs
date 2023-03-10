use std::rc::Rc;

use crate::{token::Token, source::Source};

#[derive(Debug)]
pub struct Ast {
    pub token: Token,
    pub data: AstData,
}

#[derive(Debug)]
pub enum AstData {
    Literal,
    Identifier,
    ListLiteral(Vec<Box<Ast>>),
    ExpressionStatement(Box<Ast>),

    BinaryOp { lhs: Box<Ast>, rhs: Box<Ast> },
    UnaryOp { rhs: Box<Ast> },
    LogicalOp { lhs: Box<Ast>, rhs: Box<Ast> },

    Parameter { type_name: Option<Token> },
    Function { anonymous: bool, parameters: Option<Vec<Box<Ast>>>, body: Box<Ast> },
    FunctionCall { lhs: Box<Ast>, arguments: Vec<Box<Ast>> },

    VarDeclaration { is_mutable: bool, expression: Option<Box<Ast>> },
    VarAssign { lhs: Box<Ast>, expression: Box<Ast> },

    IfStatement { condition: Box<Ast>, true_body: Box<Ast>, false_body: Option<Box<Ast>> },
    ForStatement { variable: Option<Box<Ast>>, condition: Box<Ast>, increment: Option<Box<Ast>>, body: Box<Ast> },
    DoWhileStatement { body: Box<Ast>, condition: Box<Ast> },
    TrailingIfStatement { statement: Box<Ast>, condition: Box<Ast> },

    TypeCheck { is_assert: bool, expression: Box<Ast> },

    IndexGetter { expression: Box<Ast>, index: Box<Ast> },
    IndexSetter { expression: Box<Ast>, rhs: Box<Ast> },

    Return(Option<Box<Ast>>),
    Body(Vec<Box<Ast>>),
    Program { source: Rc<Source>, body: Vec<Box<Ast>> },
}

impl Ast {
    pub fn new_program(token: Token, source: Rc<Source>, body: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Program { 
                source,
                body,
            },
        })
    }

    pub fn new_body(token: Token, statements: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Body(statements),
        })
    }

    pub fn new_literal(token: Token) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::Literal,
        })
    }

    pub fn new_list_literal(token: Token, values: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::ListLiteral(values),
        })
    }

    pub fn new_identifier(token: Token) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::Identifier,
        })
    }

    pub fn new_var_decl(token: Token, is_mutable: bool, expression: Option<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarDeclaration { is_mutable, expression },
        })
    }

    pub fn new_var_assign(token: Token, lhs: Box<Ast>, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarAssign { lhs, expression },
        })
    }

    pub fn new_function(token: Token, anonymous: bool, parameters: Option<Vec<Box<Ast>>>, body: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Function { anonymous, parameters, body },
        })
    }

    pub fn new_expr_statement(expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token: expression.token.clone(),
            data: AstData::ExpressionStatement(expression),
        })
    }

    pub fn new_parameter(token: Token, type_name: Option<Token>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Parameter { type_name },
        })
    }

    pub fn new_return(token: Token, expression: Option<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Return(expression),
        })
    }

    pub fn new_if_statement(
        token: Token,
        condition: Box<Ast>,
        true_body: Box<Ast>,
        false_body: Option<Box<Ast>>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::IfStatement { condition, true_body, false_body },
        })
    }

    pub fn new_for_statement(
        token: Token,
        variable: Option<Box<Ast>>,
        condition: Box<Ast>,
        increment: Option<Box<Ast>>,
        body: Box<Ast>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::ForStatement { 
                variable,
                condition,
                increment,
                body
            },
        })
    }

    pub fn new_do_while_statement(
        token: Token,
        body: Box<Ast>,
        condition: Box<Ast>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::DoWhileStatement { body, condition },
        })
    }

    pub fn new_trailing_if(
        token: Token,
        statement: Box<Ast>,
        condition: Box<Ast>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::TrailingIfStatement { 
                statement,
                condition
            }
        })
    }

    pub fn new_function_call(
        token: Token,
        lhs: Box<Ast>,
        arguments: Vec<Box<Ast>>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::FunctionCall { lhs, arguments },
        })
    }

    pub fn new_type_check(
        is_assert: bool,
        expression: Box<Ast>,
        type_name: Token
    ) -> Box<Self> {
        Box::new(Self {
            token: type_name,
            data: AstData::TypeCheck {
                is_assert,
                expression,
             },
        })
    }

    pub fn new_binary_op(token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::BinaryOp { lhs, rhs },
        })
    }

    pub fn new_unary_op(token: Token, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::UnaryOp { rhs },
        })
    }

    pub fn new_logical_op(token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::LogicalOp { lhs, rhs },
        })
    }

    pub fn new_index_getter(token: Token, expression: Box<Ast>, index: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::IndexGetter { expression, index }
        })
    }

    pub fn new_index_setter(token: Token, expression: Box<Ast>, rhs: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::IndexSetter { expression, rhs }
        })
    }
}