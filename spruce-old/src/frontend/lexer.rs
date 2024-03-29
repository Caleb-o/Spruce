use std::{io::Error, rc::Rc};

use crate::source::Source;

use super::token::{Token, Span, TokenKind};

pub struct Lexer {
    pub source: Rc<Source>,
    line: u32,
    column: u16,
    pos: usize,
}

impl Lexer {
    pub fn new(source: &Rc<Source>) -> Result<Self, Error> {
        Ok(Self {
            source: Rc::clone(source),
            line: 1,
            column: 1,
            pos: 0,
        })
    }

    pub fn next(&mut self) -> Token {
        self.skip_whitespace();

        // Comments
        if self.peek() == '#' {
            self.advance();

            let pos = self.pos;
            while !self.is_at_end() && self.peek() != '\n' {
                self.advance();
            }

            return self.make_token(
                TokenKind::Comment,
                Span::new(pos, self.pos - pos, Rc::clone(&self.source)),
                self.column
            );
        }

        if self.is_at_end() {
            return self.make_token(
                TokenKind::EndOfFile,
                Span::new(self.pos, 0, Rc::clone(&self.source)),
                self.column
            );
        }
        
        let current = self.peek();

        if current == '_' || current.is_alphabetic() {
            return self.read_identifier();
        }

        if current.is_numeric() {
            return self.read_numeric();
        }

        match current {
            '"' => self.read_string('"'),
            '\'' => self.read_string('\''),

            '+' => self.make_char_token_matches(TokenKind::Plus, '=', TokenKind::PlusEqual),
            '-' => self.make_char_token_matches(TokenKind::Minus, '=', TokenKind::MinusEqual),
            '*' => self.make_char_token_matches(TokenKind::Star, '=', TokenKind::StarEqual),
            '/' => self.make_char_token_matches(TokenKind::Slash, '=', TokenKind::SlashEqual),

            '=' => self.make_char_token_matches(TokenKind::Equal, '=', TokenKind::EqualEqual),
            '>' => self.make_char_token_matches(TokenKind::Greater, '=', TokenKind::GreaterEqual),
            '<' => self.make_char_token_matches(TokenKind::Less, '=', TokenKind::LessEqual),
            '!' => self.make_char_token_matches(TokenKind::Bang, '=', TokenKind::NotEqual),

            '(' => self.make_char_token(TokenKind::LParen),
            ')' => self.make_char_token(TokenKind::RParen),
            '[' => self.make_char_token(TokenKind::LSquare),
            ']' => self.make_char_token(TokenKind::RSquare),
            '{' => self.make_char_token(TokenKind::LCurly),
            '}' => self.make_char_token(TokenKind::RCurly),

            '?' => self.make_char_token(TokenKind::QuestionMark),
            '.' => self.make_char_token(TokenKind::Dot),
            '@' => self.make_char_token(TokenKind::At),
            '`' => self.make_char_token(TokenKind::Backtick),
            '|' => self.make_char_token(TokenKind::Pipe),
            ';' => self.make_char_token(TokenKind::SemiColon),
            ':' => self.make_char_token(TokenKind::Colon),
            ',' => self.make_char_token(TokenKind::Comma),
            
            _ => self.make_token(
                    TokenKind::Error,
                    Span::new(self.pos, self.pos, Rc::clone(&self.source)),
                    self.column
                ),
        }
    }

    pub fn peek(&self) -> char {
        match self.is_at_end() {
            true => '\0',
            false => self.source.content
                .chars()
                .nth(self.pos)
                .unwrap(),
        }
    }

    fn make_token(
        &self,
        kind: TokenKind,
        span: Span,
        column: u16,
    ) -> Token {
        Token { span, kind, line: self.line, column }
    }

    fn make_char_token(
        &mut self,
        kind: TokenKind,
    ) -> Token {
        self.advance();
        Token { span: Span::new(self.pos - 1, 1, Rc::clone(&self.source)), kind, line: self.line, column: self.column - 1 }
    }

    fn make_char_token_matches(
        &mut self,
        kind: TokenKind,
        next: char,
        other: TokenKind,
    ) -> Token {
        self.advance();

        if self.peek() == next {
            self.advance();
            return Token { span: Span::new(self.pos - 2, 2, Rc::clone(&self.source)), kind: other, line: self.line, column: self.column - 2 }
        }

        Token { span: Span::new(self.pos - 1, 1, Rc::clone(&self.source)), kind, line: self.line, column: self.column - 1 }
    }

    fn check_if_matches(&self, start: usize, len: usize, potential: &[(&'static str, TokenKind)]) -> TokenKind {
        for (rest, kind) in potential {
            let rlen = rest.len();
            if len - 1 == rlen && &self.source.content[start + 1..start + len] == *rest {
                return *kind;
            }
        }

        TokenKind::Identifier
    }

    fn get_identifier_type(&self, start: usize, len: usize) -> TokenKind {
        let lexeme = &self.source.content[start..start + len];
        
        // Fixme: chars.nth seems dumb here
        match lexeme.chars().nth(0).unwrap() {
            'a' => self.check_if_matches(start, len, &[("nd", TokenKind::And)]),
            'd' => self.check_if_matches(start, len, &[
                ("efer", TokenKind::Defer),
                ("o", TokenKind::Do),
            ]),
            'e' => self.check_if_matches(start, len, &[
                ("lse", TokenKind::Else),
                ("rror", TokenKind::ResultError),
            ]),
            'f' => self.check_if_matches(start, len, &[
                ("n", TokenKind::Function),
                ("alse", TokenKind::False),
                ("or", TokenKind::For),
            ]),
            'i' => self.check_if_matches(start, len, &[
                    ("f", TokenKind::If),
                    ("nclude", TokenKind::Include),
                ]),
            'l' => self.check_if_matches(start, len, &[("azy", TokenKind::Lazy)]),
            'n' => self.check_if_matches(start, len, &[("one", TokenKind::None)]),
            'o' => self.check_if_matches(start, len, &[
                ("r", TokenKind::Or),
                ("k", TokenKind::ResultOk),
            ]),
            'r' => self.check_if_matches(start, len, &[
                ("eturn", TokenKind::Return),
                ("ef", TokenKind::Ref),
                ("aw", TokenKind::Raw),
            ]),
            's' => self.check_if_matches(start, len, &[
                    ("truct", TokenKind::Struct),
                    ("witch", TokenKind::Switch),
                ]),
            't' => self.check_if_matches(start, len, &[
                ("rue", TokenKind::True),
                ("ype", TokenKind::Type),
            ]),
            'v' => self.check_if_matches(start, len, &[
                ("ar", TokenKind::Var),
                ("al", TokenKind::Val),
            ]),
            'w' => self.check_if_matches(start, len, &[("hile", TokenKind::While)]),
            _ => TokenKind::Identifier,
        }
    }

    fn read_identifier(&mut self) -> Token {
        let pos = self.pos;
        let column = self.column;
        
        self.advance();

        while !self.is_at_end() && (self.peek() == '_' || self.peek().is_alphanumeric()) {
            self.advance();
        }

        let kind = self.get_identifier_type(pos, self.pos - pos);

        self.make_token(
            kind,
            Span::new(pos, self.pos - pos, Rc::clone(&self.source)),
            column,
        )
    }

    fn read_string(&mut self, end: char) -> Token {
        let column = self.column;
        
        self.advance();
        let pos = self.pos;

        while !self.is_at_end() && self.peek() != end {
            self.advance();
        }

        self.advance();

        self.make_token(
            TokenKind::String,
            Span::new(pos, self.pos - pos - 1, Rc::clone(&self.source)),
            column,
        )
    }

    fn read_numeric(&mut self) -> Token {
        let pos = self.pos;
        let column = self.column;
        let mut is_float = false;
        
        self.advance();

        if self.peek() == '.' {
            self.advance();
            is_float = true;
        }

        while !self.is_at_end() && self.peek().is_numeric() {
            // TODO: Allow floats
            self.advance();

            if self.peek() == '.' {
                self.advance();
                if is_float {
                    return self.make_token(
                        TokenKind::Error,
                        Span::new(self.pos, self.pos, Rc::clone(&self.source)),
                        self.column
                    );
                }
                is_float = true;
            }
        }

        self.make_token(
            if is_float { TokenKind::Float } else { TokenKind::Int },
            Span::new(pos, self.pos - pos, Rc::clone(&self.source)),
            column,
        )
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.source.content.len()
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.column += 1;
    }

    fn advance_line(&mut self) {
        self.pos += 1;
        self.line += 1;
        self.column = 1;
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                '\n' => self.advance_line(),
                ' ' | '\t' | '\r' => self.advance(),

                _ => break,
            }
        }
    }
}