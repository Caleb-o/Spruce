use std::rc::Rc;

use crate::source::Source;

use super::token::Token;

#[derive(Debug, Clone)]
pub struct Ast {
    pub token: Token,
    pub data: AstData,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Standard,
    Tuple(Vec<Box<Ast>>),
    Array(Box<Ast>),
    Lazy(Box<Ast>),
    Function {
        parameters: Option<Vec<Box<Ast>>>,
        return_type: Box<Ast>,
    },
}

#[derive(Debug, Clone)]
pub enum AstData {
    Identifier,
    Literal,
    SymbolLiteral,
    StructLiteral(Token, Vec<(Token, Option<Box<Ast>>)>),
    TupleLiteral(Vec<Box<Ast>>),
    ArrayLiteral(Vec<Box<Ast>>),
    ExpressionStatement(bool, Box<Ast>),

    Comment,

    BinaryOp { lhs: Box<Ast>, rhs: Box<Ast> },
    UnaryOp { rhs: Box<Ast> },
    LogicalOp { lhs: Box<Ast>, rhs: Box<Ast> },

    Parameter { type_name: Box<Ast> },
    Function { anonymous: bool, parameters: Option<Vec<Box<Ast>>>, return_type: Option<Box<Ast>>, body: Box<Ast> },
    FunctionCall { lhs: Box<Ast>, arguments: Vec<Box<Ast>> },

    VarDeclaration { is_mutable: bool, kind: Option<Box<Ast>>, expression: Option<Box<Ast>> },
    VarDeclarations(Vec<Box<Ast>>),
    VarAssign { lhs: Box<Ast>, expression: Box<Ast> },
    VarAssignEqual { operator: Token, lhs: Box<Ast>, expression: Box<Ast> },
    Type { kind: TypeKind },

    StructDefinition { is_ref: bool, items: Option<Vec<Box<Ast>>> },

    Ternary { condition: Box<Ast>, true_body: Box<Ast>, false_body: Box<Ast> },
    IfStatement { is_expression: bool, condition: Box<Ast>, true_body: Box<Ast>, false_body: Option<Box<Ast>> },
    ForStatement { variable: Option<Box<Ast>>, condition: Box<Ast>, increment: Option<Box<Ast>>, body: Box<Ast> },
    DoWhileStatement { body: Box<Ast>, condition: Box<Ast> },

    IndexGetter { expression: Box<Ast>, index: Box<Ast> },
    IndexSetter { expression: Box<Ast>, rhs: Box<Ast> },

    PropertyGetter { lhs: Box<Ast>, property: Box<Ast> },
    PropertySetter { lhs: Box<Ast>, expression: Box<Ast> },

    SwitchStatement { condition: Box<Ast>, cases: Vec<Box<Ast>> },
    SwitchCase { case: Option<Box<Ast>>, body: Box<Ast> },

    Lazy(Box<Ast>),
    Defer(Box<Ast>),
    Return(Option<Box<Ast>>),
    Body(Vec<Box<Ast>>),
    StdInclude,
    Program { source: Rc<Source>, body: Vec<Box<Ast>> },
    Empty,
}

impl Ast {
    pub fn new_empty(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Empty,
        })
    }

    pub fn new_std_include(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::StdInclude,
        })
    }

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

    pub fn new_symbol(token: Token) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::SymbolLiteral,
        })
    }

    pub fn new_struct_literal(token: Token, identifier: Token, values: Vec<(Token, Option<Box<Ast>>)>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::StructLiteral(identifier, values),
        })
    }

    pub fn new_tuple_literal(token: Token, values: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::TupleLiteral(values),
        })
    }

    pub fn new_array_literal(token: Token, values: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::ArrayLiteral(values),
        })
    }

    pub fn new_identifier(token: Token) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::Identifier,
        })
    }

    pub fn new_comment(token: Token) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::Comment,
        })
    }

    pub fn new_var_decl(token: Token, is_mutable: bool, kind: Option<Box<Ast>>, expression: Option<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarDeclaration { is_mutable, kind, expression },
        })
    }

    pub fn new_var_decls(decls: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token: decls[0].token.clone(),
            data: AstData::VarDeclarations(decls),
        })
    }

    pub fn new_var_assign(token: Token, lhs: Box<Ast>, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarAssign { lhs, expression },
        })
    }

    pub fn new_var_assign_equal(token: Token, operator: Token, lhs: Box<Ast>, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::VarAssignEqual { operator, lhs, expression },
        })
    }

    pub fn new_type(token: Token, kind: TypeKind) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Type { kind },
        })
    }

    pub fn new_struct_definition(token: Token, is_ref: bool, items: Option<Vec<Box<Ast>>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::StructDefinition { is_ref, items },
        })
    }

    pub fn new_function(
        token: Token,
        anonymous: bool,
        parameters: Option<Vec<Box<Ast>>>,
        return_type: Option<Box<Ast>>,
        body: Box<Ast>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Function { anonymous, parameters, return_type, body },
        })
    }

    pub fn new_expr_statement(is_statement: bool, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token: expression.token.clone(),
            data: AstData::ExpressionStatement(is_statement, expression),
        })
    }

    pub fn new_parameter(token: Token, type_name: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Parameter { type_name },
        })
    }

    pub fn new_lazy(token: Token, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Lazy(expression),
        })
    }

    pub fn new_defer(token: Token, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Defer(expression),
        })
    }

    pub fn new_return(token: Token, expression: Option<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Return(expression),
        })
    }

    pub fn new_ternary(
        token: Token,
        condition: Box<Ast>,
        true_body: Box<Ast>,
        false_body: Box<Ast>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::Ternary { condition, true_body, false_body },
        })
    }

    pub fn new_if_statement(
        token: Token,
        is_expression: bool,
        condition: Box<Ast>,
        true_body: Box<Ast>,
        false_body: Option<Box<Ast>>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::IfStatement { is_expression, condition, true_body, false_body },
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

    pub fn new_switch_case(token: Token, case: Option<Box<Ast>>, body: Box<Ast>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::SwitchCase { case, body },
        })
    }

    pub fn new_switch_statement(token: Token, condition: Box<Ast>, cases: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: AstData::SwitchStatement { condition, cases },
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

    pub fn new_property_getter(token: Token, lhs: Box<Ast>, property: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::PropertyGetter { lhs, property },
        })
    }

    pub fn new_property_setter(token: Token, lhs: Box<Ast>, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::PropertySetter { lhs, expression },
        })
    }
}