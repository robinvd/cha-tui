use termwiz::Error as TermwizError;

#[derive(Debug)]
pub enum ProgramError {
    Terminal(TermwizError),
    Render(String),
    Event(String),
}

impl ProgramError {
    pub fn terminal(error: TermwizError) -> Self {
        Self::Terminal(error)
    }

    pub fn render(message: impl Into<String>) -> Self {
        Self::Render(message.into())
    }

    pub fn event(message: impl Into<String>) -> Self {
        Self::Event(message.into())
    }
}

impl From<TermwizError> for ProgramError {
    fn from(error: TermwizError) -> Self {
        ProgramError::Terminal(error)
    }
}
