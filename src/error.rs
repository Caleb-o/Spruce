use std::{io, fmt::Display};

pub enum SpruceErrData {
    Generic,
    Parser {
        file_path: String,
        line: u32,
        column: u16,
    },
    Analyser {
        file_path: String,
    },
    Compiler {
        file_path: String,
    },
    Runtime,
}

pub struct SpruceErr {
    pub message: String,
    pub payload: SpruceErrData,
}

impl SpruceErr {
    pub fn new(message: String, payload: SpruceErrData) -> Self {
        Self { message, payload }
    }
}

impl From<io::Error> for SpruceErr {
    fn from(value: io::Error) -> Self {
        Self { message: value.to_string(), payload: SpruceErrData::Generic }
    }
}

impl Display for SpruceErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "[\x1b[31mError\x1b[0m] {} - {}",
            self.message,
            match self.payload {
                SpruceErrData::Parser { ref file_path, line, column } => format!("'{}' [{}:{}]", file_path, line, column),
                SpruceErrData::Analyser { ref file_path } => format!("'{}'", file_path),
                SpruceErrData::Compiler { ref file_path } => format!("'{}'", file_path),
                _ => "".into(),
            }
        )
    }
}