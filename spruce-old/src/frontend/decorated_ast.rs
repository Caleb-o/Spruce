use std::rc::Rc;

use crate::source::Source;

use super::{token::{Token, Span}, sprucetype::SpruceType, ast::ErrorOrValue};

#[derive(Debug)]
pub struct DecoratedAst {
    pub token: Token,
    pub data: DecoratedAstData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionType {
    Standard, Anonymous, Inner, Method,
}

#[derive(Debug)]
pub enum DecoratedAstData {
    Identifier(Rc<SpruceType>),
    Literal(Rc<SpruceType>), // Constant index
    SymbolLiteral(u32), // Symbol Index
    StructLiteral(Rc<SpruceType>, Vec<(Span, Option<Rc<DecoratedAst>>)>),
    TupleLiteral(Rc<SpruceType>, Vec<Rc<DecoratedAst>>),
    ArrayLiteral(Rc<SpruceType>, Vec<Rc<DecoratedAst>>),
    ExpressionStatement(Rc<SpruceType>, bool, Rc<DecoratedAst>),
    ErrorOrValue { kind: Rc<SpruceType>, which: ErrorOrValue, expression: Rc<DecoratedAst> },

    Comment,

    BinaryOp { kind: Rc<SpruceType>, lhs: Rc<DecoratedAst>, rhs: Rc<DecoratedAst> },
    UnaryOp { kind: Rc<SpruceType>, rhs: Rc<DecoratedAst> },
    LogicalOp { kind: Rc<SpruceType>, lhs: Rc<DecoratedAst>, rhs: Rc<DecoratedAst> },

    Parameter(Rc<SpruceType>),
    ParameterList(Option<Vec<Rc<DecoratedAst>>>),
    Function { function_type: FunctionType, parameters: Rc<DecoratedAst>, kind: Rc<SpruceType>, body: Rc<DecoratedAst> },
    FunctionCall { kind: Rc<SpruceType>, lhs: Rc<DecoratedAst>, arguments: Vec<Rc<DecoratedAst>> },

    VarDeclaration { is_mutable: bool, kind: Rc<SpruceType>, expression: Rc<DecoratedAst> },
    VarDeclarations(Vec<Rc<DecoratedAst>>),
    VarAssign { lhs: Rc<DecoratedAst>, expression: Rc<DecoratedAst> },
    VarAssignEqual { operator: Token, lhs: Rc<DecoratedAst>, expression: Rc<DecoratedAst> },
    Type(Rc<SpruceType>),

    StructDefinition { kind: Rc<SpruceType>, is_ref: bool, items: Option<Vec<Rc<DecoratedAst>>> },
    StructField { kind: Rc<SpruceType>, default_value: Option<Rc<DecoratedAst>> },

    IfStatement { is_expression: bool, condition: Rc<DecoratedAst>, kind: Rc<SpruceType>, true_body: Rc<DecoratedAst>, false_body: Option<Rc<DecoratedAst>> },
    ForStatement { variable: Option<Rc<DecoratedAst>>, condition: Rc<DecoratedAst>, increment: Option<Rc<DecoratedAst>>, body: Rc<DecoratedAst> },
    DoWhileStatement { body: Rc<DecoratedAst>, condition: Rc<DecoratedAst> },

    IndexGetter { expression: Rc<DecoratedAst>, index: Rc<DecoratedAst> },
    IndexSetter { expression: Rc<DecoratedAst>, rhs: Rc<DecoratedAst> },

    GetProperty { lhs: Rc<DecoratedAst>, property: Rc<DecoratedAst> },
    SetProperty { lhs: Rc<DecoratedAst>, expression: Rc<DecoratedAst> },

    SwitchStatement { condition: Rc<DecoratedAst>, cases: Vec<Rc<DecoratedAst>> },
    SwitchCase { case: Option<Rc<DecoratedAst>>, body: Rc<DecoratedAst> },

    Payload(Rc<SpruceType>, Rc<DecoratedAst>),
    Raw { kind: Rc<SpruceType>, code: Vec<Rc<DecoratedAst>> },
    Lazy(Rc<DecoratedAst>),
    Defer(u32, Rc<DecoratedAst>),
    Return(Rc<SpruceType>, Option<Rc<DecoratedAst>>),
    Body(Rc<SpruceType>, Vec<Rc<DecoratedAst>>),
    StdInclude,
    Program { source: Rc<Source>, body: Vec<Rc<DecoratedAst>> },
    Empty,
}

impl DecoratedAst {
    pub fn new_empty(token: Token) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Empty,
        })
    }

    pub fn new_std_include(token: Token) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::StdInclude,
        })
    }

    pub fn new_program(token: Token, source: Rc<Source>, body: Vec<Rc<DecoratedAst>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Program { 
                source,
                body,
            },
        })
    }

    pub fn new_body(token: Token, statements: Vec<Rc<DecoratedAst>>, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Body(kind, statements),
        })
    }

    pub fn new_literal(token: Token, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::Literal(kind),
        })
    }

    pub fn new_symbol(token: Token, index: u32) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::SymbolLiteral(index),
        })
    }

    pub fn new_struct_literal(token: Token, kind: Rc<SpruceType>, values: Vec<(Span, Option<Rc<DecoratedAst>>)>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::StructLiteral(kind, values),
        })
    }

    pub fn new_tuple_literal(token: Token, values: Vec<Rc<DecoratedAst>>, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::TupleLiteral(kind, values),
        })
    }

    pub fn new_array_literal(token: Token, values: Vec<Rc<DecoratedAst>>, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::ArrayLiteral(kind, values),
        })
    }

    pub fn new_identifier(token: Token, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::Identifier(kind),
        })
    }

    pub fn new_var_decl(token: Token, is_mutable: bool, kind: Rc<SpruceType>, expression: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::VarDeclaration { is_mutable, kind, expression },
        })
    }

    pub fn new_var_decls(decls: Vec<Rc<DecoratedAst>>) -> Rc<Self> {
        Rc::new(Self {
            token: decls[0].token.clone(),
            data: DecoratedAstData::VarDeclarations(decls),
        })
    }

    pub fn new_var_assign(token: Token, lhs: Rc<DecoratedAst>, expression: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::VarAssign { lhs, expression },
        })
    }

    pub fn new_var_assign_equal(token: Token, operator: Token, lhs: Rc<DecoratedAst>, expression: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::VarAssignEqual { operator, lhs, expression },
        })
    }

    pub fn new_function(
        token: Token,
        function_type: FunctionType,
        parameters: Rc<DecoratedAst>,
        kind: Rc<SpruceType>,
        body: Rc<DecoratedAst>
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Function { function_type, parameters, kind, body },
        })
    }

    pub fn new_expr_statement(is_statement: bool, expression: Rc<DecoratedAst>, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self {
            token: expression.token.clone(),
            data: DecoratedAstData::ExpressionStatement(kind, is_statement, expression),
        })
    }

    pub fn new_error_or_value(which: ErrorOrValue, expression: Rc<DecoratedAst>, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self {
            token: expression.token.clone(),
            data: DecoratedAstData::ErrorOrValue { kind, which, expression }
        })
    }

    pub fn new_comment(token: Token) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::Comment,
        })
    }

    pub fn new_parameter(token: Token, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Parameter(kind),
        })
    }

    pub fn new_parameter_list(token: Token, parameters: Option<Vec<Rc<DecoratedAst>>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::ParameterList(parameters),
        })
    }

    pub fn new_payload(token: Token, kind: Rc<SpruceType>, expression: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Payload(kind, expression),
        })
    }

    pub fn new_raw(token: Token, kind: Rc<SpruceType>, code: Vec<Rc<DecoratedAst>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Raw { kind, code },
        })
    }

    pub fn new_lazy(token: Token, expression: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Lazy(expression),
        })
    }

    pub fn new_defer(token: Token, count: u32, expression: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Defer(count, expression),
        })
    }

    pub fn new_return(token: Token, kind: Rc<SpruceType>, expression: Option<Rc<DecoratedAst>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Return(kind, expression),
        })
    }

    pub fn new_type(token: Token, kind: Rc<SpruceType>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::Type(kind),
        })
    }

    pub fn new_struct_definition(token: Token, kind: Rc<SpruceType>, is_ref: bool, items: Option<Vec<Rc<DecoratedAst>>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::StructDefinition { kind, is_ref, items },
        })
    }

    pub fn new_struct_field(token: Token, kind: Rc<SpruceType>, default_value: Option<Rc<DecoratedAst>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::StructField { kind, default_value },
        })
    }

    pub fn new_if_statement(
        token: Token,
        is_expression: bool,
        condition: Rc<DecoratedAst>,
        kind: Rc<SpruceType>,
        true_body: Rc<DecoratedAst>,
        false_body: Option<Rc<DecoratedAst>>
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::IfStatement { is_expression, condition, kind, true_body, false_body },
        })
    }

    pub fn new_for_statement(
        token: Token,
        variable: Option<Rc<DecoratedAst>>,
        condition: Rc<DecoratedAst>,
        increment: Option<Rc<DecoratedAst>>,
        body: Rc<DecoratedAst>,
    ) -> Rc<Self> {
        Rc::new(Self {
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
        body: Rc<DecoratedAst>,
        condition: Rc<DecoratedAst>
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::DoWhileStatement { body, condition },
        })
    }

    pub fn new_function_call(
        token: Token,
        kind: Rc<SpruceType>,
        lhs: Rc<DecoratedAst>,
        arguments: Vec<Rc<DecoratedAst>>,
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::FunctionCall { kind, lhs, arguments },
        })
    }

    pub fn new_switch_case(token: Token, case: Option<Rc<DecoratedAst>>, body: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::SwitchCase { case, body },
        })
    }

    pub fn new_switch_statement(token: Token, condition: Rc<DecoratedAst>, cases: Vec<Rc<DecoratedAst>>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::SwitchStatement { condition, cases },
        })
    }

    pub fn new_binary_op(token: Token, kind: Rc<SpruceType>, lhs: Rc<DecoratedAst>, rhs: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::BinaryOp { kind, lhs, rhs },
        })
    }

    pub fn new_unary_op(token: Token, kind: Rc<SpruceType>, rhs: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::UnaryOp { kind, rhs },
        })
    }

    pub fn new_logical_op(token: Token, lhs: Rc<DecoratedAst>, rhs: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: DecoratedAstData::LogicalOp { kind: Rc::new(SpruceType::Bool), lhs, rhs },
        })
    }

    pub fn new_index_getter(token: Token, expression: Rc<DecoratedAst>, index: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::IndexGetter { expression, index }
        })
    }

    pub fn new_index_setter(token: Token, expression: Rc<DecoratedAst>, rhs: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::IndexSetter { expression, rhs }
        })
    }

    pub fn new_property_getter(token: Token, lhs: Rc<DecoratedAst>, property: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::GetProperty { lhs, property },
        })
    }

    pub fn new_property_setter(token: Token, lhs: Rc<DecoratedAst>, expression: Rc<DecoratedAst>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: DecoratedAstData::SetProperty { lhs, expression },
        })
    }
}