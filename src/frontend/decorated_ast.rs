use std::rc::Rc;

use crate::source::Source;

use super::{token::{Token, Span}, sprucetype::SpruceType};

#[derive(Debug, Clone)]
pub struct DecoratedAst {
    pub token: Token,
    pub data: DecoratedAstData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionType {
    Standard, Anonymous, Inner, Method,
}

#[derive(Debug, Clone)]
pub enum DecoratedAstData {
    Identifier(SpruceType),
    Literal(SpruceType), // Constant index
    SymbolLiteral(u32), // Symbol Index
    StructLiteral(SpruceType, Vec<(Span, Option<Box<DecoratedAst>>)>),
    TupleLiteral(SpruceType, Vec<Box<DecoratedAst>>),
    ArrayLiteral(SpruceType, Vec<Box<DecoratedAst>>),
    ExpressionStatement(SpruceType, bool, Box<DecoratedAst>),

    Comment,

    BinaryOp { kind: SpruceType, lhs: Box<DecoratedAst>, rhs: Box<DecoratedAst> },
    UnaryOp { kind: SpruceType, rhs: Box<DecoratedAst> },
    LogicalOp { kind: SpruceType, lhs: Box<DecoratedAst>, rhs: Box<DecoratedAst> },

    Parameter(SpruceType),
    ParameterList(Option<Vec<Box<DecoratedAst>>>),
    Function { function_type: FunctionType, parameters: Box<DecoratedAst>, kind: SpruceType, body: Box<DecoratedAst> },
    FunctionCall { kind: SpruceType, lhs: Box<DecoratedAst>, arguments: Vec<Box<DecoratedAst>> },

    VarDeclaration { is_mutable: bool, kind: SpruceType, expression: Box<DecoratedAst> },
    VarDeclarations(Vec<Box<DecoratedAst>>),
    VarAssign { lhs: Box<DecoratedAst>, expression: Box<DecoratedAst> },
    VarAssignEqual { operator: Token, lhs: Box<DecoratedAst>, expression: Box<DecoratedAst> },
    Type(SpruceType),

    TypeDefinition { inner: Box<DecoratedAst> },
    StructDefinition { kind: SpruceType, is_ref: bool, items: Option<Vec<Box<DecoratedAst>>> },

    Ternary { condition: Box<DecoratedAst>, kind: SpruceType, true_body: Box<DecoratedAst>, false_body: Box<DecoratedAst> },
    IfStatement { is_expression: bool, condition: Box<DecoratedAst>, kind: SpruceType, true_body: Box<DecoratedAst>, false_body: Option<Box<DecoratedAst>> },
    ForStatement { variable: Option<Box<DecoratedAst>>, condition: Box<DecoratedAst>, increment: Option<Box<DecoratedAst>>, body: Box<DecoratedAst> },
    DoWhileStatement { body: Box<DecoratedAst>, condition: Box<DecoratedAst> },

    IndexGetter { expression: Box<DecoratedAst>, index: Box<DecoratedAst> },
    IndexSetter { expression: Box<DecoratedAst>, rhs: Box<DecoratedAst> },

    GetProperty { lhs: Box<DecoratedAst>, property: Box<DecoratedAst> },
    SetProperty { lhs: Box<DecoratedAst>, expression: Box<DecoratedAst> },

    SwitchStatement { condition: Box<DecoratedAst>, cases: Vec<Box<DecoratedAst>> },
    SwitchCase { case: Option<Box<DecoratedAst>>, body: Box<DecoratedAst> },

    Lazy(Box<DecoratedAst>),
    Defer(u32, Box<DecoratedAst>),
    Return(SpruceType, Option<Box<DecoratedAst>>),
    Body(SpruceType, Vec<Box<DecoratedAst>>),
    StdInclude,
    Program { source: Rc<Source>, body: Vec<Box<DecoratedAst>> },
    Empty,
}

impl DecoratedAst {
    pub fn new_empty(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Empty,
        })
    }

    pub fn new_std_include(token: Token) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::StdInclude,
        })
    }

    pub fn new_program(token: Token, source: Rc<Source>, body: Vec<Box<DecoratedAst>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Program { 
                source,
                body,
            },
        })
    }

    pub fn new_body(token: Token, statements: Vec<Box<DecoratedAst>>, kind: SpruceType) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Body(kind, statements),
        })
    }

    pub fn new_literal(token: Token, kind: SpruceType) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::Literal(kind),
        })
    }

    pub fn new_symbol(token: Token, index: u32) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::SymbolLiteral(index),
        })
    }

    pub fn new_struct_literal(token: Token, kind: SpruceType, values: Vec<(Span, Option<Box<DecoratedAst>>)>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::StructLiteral(kind, values),
        })
    }

    pub fn new_tuple_literal(token: Token, values: Vec<Box<DecoratedAst>>, kind: SpruceType) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::TupleLiteral(kind, values),
        })
    }

    pub fn new_array_literal(token: Token, values: Vec<Box<DecoratedAst>>, kind: SpruceType) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::ArrayLiteral(kind, values),
        })
    }

    pub fn new_identifier(token: Token, kind: SpruceType) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::Identifier(kind),
        })
    }

    pub fn new_var_decl(token: Token, is_mutable: bool, kind: SpruceType, expression: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::VarDeclaration { is_mutable, kind, expression },
        })
    }

    pub fn new_var_decls(decls: Vec<Box<DecoratedAst>>) -> Box<Self> {
        Box::new(Self {
            token: decls[0].token.clone(),
            data: DecoratedAstData::VarDeclarations(decls),
        })
    }

    pub fn new_var_assign(token: Token, lhs: Box<DecoratedAst>, expression: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::VarAssign { lhs, expression },
        })
    }

    pub fn new_var_assign_equal(token: Token, operator: Token, lhs: Box<DecoratedAst>, expression: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::VarAssignEqual { operator, lhs, expression },
        })
    }

    pub fn new_function(
        token: Token,
        function_type: FunctionType,
        parameters: Box<DecoratedAst>,
        kind: SpruceType,
        body: Box<DecoratedAst>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Function { function_type, parameters, kind, body },
        })
    }

    pub fn new_expr_statement(is_statement: bool, expression: Box<DecoratedAst>, kind: SpruceType) -> Box<Self> {
        Box::new(Self {
            token: expression.token.clone(),
            data: DecoratedAstData::ExpressionStatement(kind, is_statement, expression),
        })
    }

    pub fn new_comment(token: Token) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::Comment,
        })
    }

    pub fn new_parameter(token: Token, kind: SpruceType) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Parameter(kind),
        })
    }

    pub fn new_parameter_list(token: Token, parameters: Option<Vec<Box<DecoratedAst>>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::ParameterList(parameters),
        })
    }

    pub fn new_lazy(token: Token, expression: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Lazy(expression),
        })
    }

    pub fn new_defer(token: Token, count: u32, expression: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Defer(count, expression),
        })
    }

    pub fn new_return(token: Token, kind: SpruceType, expression: Option<Box<DecoratedAst>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Return(kind, expression),
        })
    }

    pub fn new_type(token: Token, kind: SpruceType) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Type(kind),
        })
    }

    pub fn new_type_definition(token: Token, inner: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::TypeDefinition { inner },
        })
    }

    pub fn new_struct_definition(token: Token, kind: SpruceType, is_ref: bool, items: Option<Vec<Box<DecoratedAst>>>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::StructDefinition { kind, is_ref, items },
        })
    }

    pub fn new_ternary(
        token: Token,
        condition: Box<DecoratedAst>,
        kind: SpruceType,
        true_body: Box<DecoratedAst>,
        false_body: Box<DecoratedAst>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::Ternary { condition, kind, true_body, false_body },
        })
    }

    pub fn new_if_statement(
        token: Token,
        is_expression: bool,
        condition: Box<DecoratedAst>,
        kind: SpruceType,
        true_body: Box<DecoratedAst>,
        false_body: Option<Box<DecoratedAst>>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::IfStatement { is_expression, condition, kind, true_body, false_body },
        })
    }

    pub fn new_for_statement(
        token: Token,
        variable: Option<Box<DecoratedAst>>,
        condition: Box<DecoratedAst>,
        increment: Option<Box<DecoratedAst>>,
        body: Box<DecoratedAst>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::ForStatement { 
                variable,
                condition,
                increment,
                body
            },
        })
    }

    pub fn new_do_while_statement(
        token: Token,
        body: Box<DecoratedAst>,
        condition: Box<DecoratedAst>
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::DoWhileStatement { body, condition },
        })
    }

    pub fn new_function_call(
        token: Token,
        kind: SpruceType,
        lhs: Box<DecoratedAst>,
        arguments: Vec<Box<DecoratedAst>>,
    ) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::FunctionCall { kind, lhs, arguments },
        })
    }

    pub fn new_switch_case(token: Token, case: Option<Box<DecoratedAst>>, body: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::SwitchCase { case, body },
        })
    }

    pub fn new_switch_statement(token: Token, condition: Box<DecoratedAst>, cases: Vec<Box<DecoratedAst>>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::SwitchStatement { condition, cases },
        })
    }

    pub fn new_binary_op(token: Token, kind: SpruceType, lhs: Box<DecoratedAst>, rhs: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::BinaryOp { kind, lhs, rhs },
        })
    }

    pub fn new_unary_op(token: Token, kind: SpruceType, rhs: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::UnaryOp { kind, rhs },
        })
    }

    pub fn new_logical_op(token: Token, lhs: Box<DecoratedAst>, rhs: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self { 
            token,
            data: DecoratedAstData::LogicalOp { kind: SpruceType::Bool, lhs, rhs },
        })
    }

    pub fn new_index_getter(token: Token, expression: Box<DecoratedAst>, index: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::IndexGetter { expression, index }
        })
    }

    pub fn new_index_setter(token: Token, expression: Box<DecoratedAst>, rhs: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::IndexSetter { expression, rhs }
        })
    }

    pub fn new_property_getter(token: Token, lhs: Box<DecoratedAst>, property: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::GetProperty { lhs, property },
        })
    }

    pub fn new_property_setter(token: Token, lhs: Box<DecoratedAst>, expression: Box<DecoratedAst>) -> Box<Self> {
        Box::new(Self {
            token,
            data: DecoratedAstData::SetProperty { lhs, expression },
        })
    }
}