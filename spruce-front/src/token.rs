#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    Slash,
    QuestionMark,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    Equal,
    EqualEqual,
    Bang,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Backtick,
    Pipe,
    SemiColon,
    Colon,
    Comma,
    At,
    Dot,

    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,

    // Literal types
    Int,
    Float,
    String,
    True,
    False,
    None,

    If,
    Else,
    For,
    Val,
    Var,
    Defer,
    Do,
    While,
    Include,
    Switch,
    Lazy,
    Function,
    Struct,
    Return,
    Raw,
    Identifier,
    And,
    Or,
    Type,
    Ref,

    ResultOk,
    ResultError,

    Comment,
    EndOfFile,
    Error,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub len: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
    pub column: u16,
    pub lexeme: Option<Span>,
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        format!("{self:?}")
    }
}
