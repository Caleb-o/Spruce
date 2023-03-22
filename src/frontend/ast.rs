use std::rc::Rc;

use crate::source::Source;

use super::token::Token;

#[derive(Debug)]
pub struct Ast {
    pub token: Token,
    pub data: AstData,
}

#[derive(Debug)]
pub enum TypeKind {
    Standard,
    Tuple(Vec<Box<Ast>>),
    List(Box<Ast>),
    Function {
        parameters: Option<Vec<Box<Ast>>>,
        return_type: Box<Ast>,
    },
}

#[derive(Debug)]
pub enum AstData {
    Identifier,
    Literal,
    SymbolLiteral,
    MapLiteral(Vec<(Token, Option<Box<Ast>>)>),
    TupleLiteral(Vec<Box<Ast>>),
    ListLiteral(Vec<Box<Ast>>),
    ExpressionStatement(bool, Box<Ast>),

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

    Ternary { condition: Box<Ast>, true_body: Box<Ast>, false_body: Box<Ast> },
    IfStatement { condition: Box<Ast>, true_body: Box<Ast>, false_body: Option<Box<Ast>> },
    ForStatement { variable: Option<Box<Ast>>, condition: Box<Ast>, increment: Option<Box<Ast>>, body: Box<Ast> },
    DoWhileStatement { body: Box<Ast>, condition: Box<Ast> },
    TrailingIfStatement { statement: Box<Ast>, condition: Box<Ast> },

    TypeCheck { is_assert: bool, expression: Box<Ast> },

    IndexGetter { expression: Box<Ast>, index: Box<Ast> },
    IndexSetter { expression: Box<Ast>, rhs: Box<Ast> },

    SetProperty { lhs: Box<Ast>, expression: Box<Ast> },
    GetProperty { lhs: Box<Ast> },

    SwitchStatement { condition: Box<Ast>, cases: Vec<Box<Ast>> },
    SwitchCase { case: Option<Box<Ast>>, body: Box<Ast> },

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

    pub fn new_map_literal(token: Token, values: Vec<(Token, Option<Box<Ast>>)>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::MapLiteral(values),
        })
    }

    pub fn new_tuple_literal(token: Token, values: Vec<Box<Ast>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::TupleLiteral(values),
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

    pub fn new_property_getter(token: Token, lhs: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::GetProperty { lhs },
        })
    }

    pub fn new_property_setter(token: Token, lhs: Box<Ast>, expression: Box<Ast>) -> Box<Self> {
        Box::new(Self {
            token,
            data: AstData::SetProperty { lhs, expression },
        })
    }
}