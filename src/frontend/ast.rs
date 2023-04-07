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
    Tuple(Vec<Rc<Ast>>),
    Array(Rc<Ast>),
    Lazy(Rc<Ast>),
    Function {
        parameters: Option<Vec<Rc<Ast>>>,
        return_type: Rc<Ast>,
    },
}

#[derive(Debug)]
pub enum AstData {
    Identifier,
    Literal,
    SymbolLiteral,
    StructLiteral(Token, Vec<(Token, Option<Rc<Ast>>)>),
    TupleLiteral(Vec<Rc<Ast>>),
    ArrayLiteral(Vec<Rc<Ast>>),
    ExpressionStatement(bool, Rc<Ast>),

    Comment,

    BinaryOp { lhs: Rc<Ast>, rhs: Rc<Ast> },
    UnaryOp { rhs: Rc<Ast> },
    LogicalOp { lhs: Rc<Ast>, rhs: Rc<Ast> },

    Parameter { type_signature: Rc<Ast> },
    Function { anonymous: bool, parameters: Option<Vec<Rc<Ast>>>, return_type: Option<Rc<Ast>>, body: Rc<Ast> },
    FunctionCall { lhs: Rc<Ast>, arguments: Vec<Rc<Ast>> },

    VarDeclaration { is_mutable: bool, kind: Option<Rc<Ast>>, expression: Option<Rc<Ast>> },
    VarDeclarations(Vec<Rc<Ast>>),
    VarAssign { lhs: Rc<Ast>, expression: Rc<Ast> },
    VarAssignEqual { operator: Token, lhs: Rc<Ast>, expression: Rc<Ast> },
    Type { kind: TypeKind },

    StructDefinition { is_ref: bool, items: Option<Vec<Rc<Ast>>> },
    StructField { type_signature: Rc<Ast>, default_value: Option<Rc<Ast>> },

    Ternary { condition: Rc<Ast>, true_body: Rc<Ast>, false_body: Rc<Ast> },
    IfStatement { is_expression: bool, condition: Rc<Ast>, true_body: Rc<Ast>, false_body: Option<Rc<Ast>> },
    ForStatement { variable: Option<Rc<Ast>>, condition: Rc<Ast>, increment: Option<Rc<Ast>>, body: Rc<Ast> },
    DoWhileStatement { body: Rc<Ast>, condition: Rc<Ast> },

    IndexGetter { expression: Rc<Ast>, index: Rc<Ast> },
    IndexSetter { expression: Rc<Ast>, rhs: Rc<Ast> },

    PropertyGetter { lhs: Rc<Ast>, property: Rc<Ast> },
    PropertySetter { lhs: Rc<Ast>, expression: Rc<Ast> },

    SwitchStatement { condition: Rc<Ast>, cases: Vec<Rc<Ast>> },
    SwitchCase { case: Option<Rc<Ast>>, body: Rc<Ast> },

    Raw { returns: Option<Rc<Ast>>, code: Vec<Rc<Ast>> },
    Lazy(Rc<Ast>),
    Defer(Rc<Ast>),
    Return(Option<Rc<Ast>>),
    Body(Vec<Rc<Ast>>),
    StdInclude,
    Program { source: Rc<Source>, body: Vec<Rc<Ast>> },
    Empty,
}

impl Ast {
    pub fn new_empty(token: Token) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Empty,
        })
    }

    pub fn new_std_include(token: Token) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::StdInclude,
        })
    }

    pub fn new_program(token: Token, source: Rc<Source>, body: Vec<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Program { 
                source,
                body,
            },
        })
    }

    pub fn new_body(token: Token, statements: Vec<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Body(statements),
        })
    }

    pub fn new_literal(token: Token) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::Literal,
        })
    }

    pub fn new_symbol(token: Token) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::SymbolLiteral,
        })
    }

    pub fn new_struct_literal(token: Token, identifier: Token, values: Vec<(Token, Option<Rc<Ast>>)>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::StructLiteral(identifier, values),
        })
    }

    pub fn new_tuple_literal(token: Token, values: Vec<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::TupleLiteral(values),
        })
    }

    pub fn new_array_literal(token: Token, values: Vec<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::ArrayLiteral(values),
        })
    }

    pub fn new_identifier(token: Token) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::Identifier,
        })
    }

    pub fn new_comment(token: Token) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::Comment,
        })
    }

    pub fn new_var_decl(token: Token, is_mutable: bool, kind: Option<Rc<Ast>>, expression: Option<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::VarDeclaration { is_mutable, kind, expression },
        })
    }

    pub fn new_var_decls(decls: Vec<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token: decls[0].token.clone(),
            data: AstData::VarDeclarations(decls),
        })
    }

    pub fn new_var_assign(token: Token, lhs: Rc<Ast>, expression: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::VarAssign { lhs, expression },
        })
    }

    pub fn new_var_assign_equal(token: Token, operator: Token, lhs: Rc<Ast>, expression: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::VarAssignEqual { operator, lhs, expression },
        })
    }

    pub fn new_type(token: Token, kind: TypeKind) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Type { kind },
        })
    }

    pub fn new_struct_definition(token: Token, is_ref: bool, items: Option<Vec<Rc<Ast>>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::StructDefinition { is_ref, items },
        })
    }

    pub fn new_struct_field(token: Token, type_signature: Rc<Ast>, default_value: Option<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::StructField { type_signature, default_value },
        })
    }

    pub fn new_function(
        token: Token,
        anonymous: bool,
        parameters: Option<Vec<Rc<Ast>>>,
        return_type: Option<Rc<Ast>>,
        body: Rc<Ast>
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Function { anonymous, parameters, return_type, body },
        })
    }

    pub fn new_expr_statement(is_statement: bool, expression: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token: expression.token.clone(),
            data: AstData::ExpressionStatement(is_statement, expression),
        })
    }

    pub fn new_parameter(token: Token, type_signature: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Parameter { type_signature },
        })
    }

    pub fn new_raw(token: Token, returns: Option<Rc<Ast>>, code: Vec<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Raw { returns, code },
        })
    }
    
    pub fn new_lazy(token: Token, expression: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Lazy(expression),
        })
    }

    pub fn new_defer(token: Token, expression: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Defer(expression),
        })
    }

    pub fn new_return(token: Token, expression: Option<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Return(expression),
        })
    }

    pub fn new_ternary(
        token: Token,
        condition: Rc<Ast>,
        true_body: Rc<Ast>,
        false_body: Rc<Ast>
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::Ternary { condition, true_body, false_body },
        })
    }

    pub fn new_if_statement(
        token: Token,
        is_expression: bool,
        condition: Rc<Ast>,
        true_body: Rc<Ast>,
        false_body: Option<Rc<Ast>>
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::IfStatement { is_expression, condition, true_body, false_body },
        })
    }

    pub fn new_for_statement(
        token: Token,
        variable: Option<Rc<Ast>>,
        condition: Rc<Ast>,
        increment: Option<Rc<Ast>>,
        body: Rc<Ast>,
    ) -> Rc<Self> {
        Rc::new(Self {
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
        body: Rc<Ast>,
        condition: Rc<Ast>
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::DoWhileStatement { body, condition },
        })
    }

    pub fn new_function_call(
        token: Token,
        lhs: Rc<Ast>,
        arguments: Vec<Rc<Ast>>,
    ) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::FunctionCall { lhs, arguments },
        })
    }

    pub fn new_switch_case(token: Token, case: Option<Rc<Ast>>, body: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::SwitchCase { case, body },
        })
    }

    pub fn new_switch_statement(token: Token, condition: Rc<Ast>, cases: Vec<Rc<Ast>>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::SwitchStatement { condition, cases },
        })
    }

    pub fn new_binary_op(token: Token, lhs: Rc<Ast>, rhs: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::BinaryOp { lhs, rhs },
        })
    }

    pub fn new_unary_op(token: Token, rhs: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::UnaryOp { rhs },
        })
    }

    pub fn new_logical_op(token: Token, lhs: Rc<Ast>, rhs: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self { 
            token,
            data: AstData::LogicalOp { lhs, rhs },
        })
    }

    pub fn new_index_getter(token: Token, expression: Rc<Ast>, index: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::IndexGetter { expression, index }
        })
    }

    pub fn new_index_setter(token: Token, expression: Rc<Ast>, rhs: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::IndexSetter { expression, rhs }
        })
    }

    pub fn new_property_getter(token: Token, lhs: Rc<Ast>, property: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::PropertyGetter { lhs, property },
        })
    }

    pub fn new_property_setter(token: Token, lhs: Rc<Ast>, expression: Rc<Ast>) -> Rc<Self> {
        Rc::new(Self {
            token,
            data: AstData::PropertySetter { lhs, expression },
        })
    }
}