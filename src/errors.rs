use thiserror::Error;

#[derive(Debug, Clone)]
pub enum CompileErrorKind {
    Lex,
    Parse,
    Semantic,
    Warning,
}

#[derive(Debug, Clone, Error)]
#[error("{kind:?}: {message}")]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub message: String,
    pub span: std::ops::Range<usize>,
}

impl CompileError {
    pub fn new(
        kind: CompileErrorKind,
        message: impl Into<String>,
        span: std::ops::Range<usize>,
    ) -> Self {
        Self {
            kind,
            message: message.into(),
            span,
        }
    }
}
