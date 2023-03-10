use std::{io, fmt::Display};

use crate::{parser::ParserErr, compiler::CompilerErr, vm::RuntimeErr};

pub enum SpruceErrData {
    Generic,
    Parser(ParserErr),
    Compiler(CompilerErr),
    Runtime(RuntimeErr),
}

pub struct SpruceErr {
    pub message: String,
    pub payload: SpruceErrData,
}

impl From<io::Error> for SpruceErr {
    fn from(value: io::Error) -> Self {
        Self { message: value.to_string(), payload: SpruceErrData::Generic }
    }
}

impl From<ParserErr> for SpruceErr {
    fn from(value: ParserErr) -> Self {
        Self { message: value.message.clone(), payload: SpruceErrData::Parser(value) }
    }
}

impl From<CompilerErr> for SpruceErr {
    fn from(value: CompilerErr) -> Self {
        Self { message: value.message.clone(), payload: SpruceErrData::Compiler(value) }
    }
}

impl From<RuntimeErr> for SpruceErr {
    fn from(value: RuntimeErr) -> Self {
        Self { message: value.0.clone(), payload: SpruceErrData::Runtime(value) }
    }
}

impl Display for SpruceErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "[\x1b[31mError\x1b[0m] {} - {}",
            self.message,
            match self.payload {
                SpruceErrData::Generic => "".into(),
                SpruceErrData::Parser(ref e) => format!(" '{}' [{}:{}]", e.file_path, e.line, e.column),
                SpruceErrData::Compiler(ref e) => format!(" '{}'", e.file_path),
                SpruceErrData::Runtime(_) => "".into(),
            }
        )
    }
}