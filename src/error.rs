#[derive(Debug)]
pub enum ProgramError {
    Terminal(String),
    Render(String),
    Event(String),
    Io(std::io::Error),
}

impl ProgramError {
    pub fn terminal(message: impl Into<String>) -> Self {
        Self::Terminal(message.into())
    }

    pub fn render(message: impl Into<String>) -> Self {
        Self::Render(message.into())
    }

    pub fn event(message: impl Into<String>) -> Self {
        Self::Event(message.into())
    }
}

impl From<std::io::Error> for ProgramError {
    fn from(error: std::io::Error) -> Self {
        ProgramError::Io(error)
    }
}

impl From<std::fmt::Error> for ProgramError {
    fn from(error: std::fmt::Error) -> Self {
        ProgramError::Render(format!("Format error: {}", error))
    }
}
