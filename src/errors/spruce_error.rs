#[derive(Debug)]
pub enum SpruceError {
	General(String),
	Lexer(String),
}

impl std::fmt::Display for SpruceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			SpruceError::General(msg) => write!(f, "General: {}", msg),
			SpruceError::Lexer(msg) => write!(f, "Lexer: {}", msg),
		}
    }
}