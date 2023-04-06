use std::rc::Rc;

use crate::source::Source;

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub len: usize,
    pub source: Rc<Source>,
}

impl Span {
    pub fn new(start: usize, len: usize, source: Rc<Source>) -> Self {
        Span {
            start,
            len,
            source,
        }
    }
    
    #[inline]
    pub fn slice_source<'a>(&'a self) -> &'a str {
        &self.source.content[self.start..self.start + self.len]
    }
    
    pub fn compare(&self, other: &Span) -> bool {
        // TODO: Reconsider the use of aend/bend
        let aend = self.start + self.len;
        let bend = other.start + other.len;

        if aend >= self.source.content.len() || bend >= self.source.content.len() {
            return false;
        }

        self.slice_source() == other.slice_source()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
    Plus, Minus, Star, Slash, QuestionMark,
    PlusEqual, MinusEqual, StarEqual, SlashEqual,
    Equal, EqualEqual, Bang, NotEqual,
    Greater, GreaterEqual, Less, LessEqual,
    
    Backtick, Pipe, SemiColon, Colon, Comma, At, Dot,

    LParen, RParen,
    LSquare, RSquare,
    LCurly, RCurly,
    
    // Literal types
    Int, Float, String, True, False, None,
    
    If, Else, For, Val, Var, Defer,
    Do, While, Include, Switch, Lazy,
    Function, Struct, Return, Raw,
    Identifier, And, Or, Type, Ref,
    
    
    Comment,
    EndOfFile,
    Error,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
    pub line: u32,
    pub column: u16,
}