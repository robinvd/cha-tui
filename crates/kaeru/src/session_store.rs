use std::fs;
use std::path::PathBuf;
use std::sync::Mutex;
use directories::ProjectDirs;
use miette::{IntoDiagnostic, Result};
use serde::{Deserialize, Serialize};
use time::OffsetDateTime;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SavedSession {
    pub id: String,
    #[serde(with = "time::serde::iso8601")]
    pub date: OffsetDateTime,
    pub prompt: String,
}

pub trait SessionIo: Send + Sync {
    fn load_sessions(&self) -> Result<Vec<SavedSession>>;
    fn save_session(&self, session: SavedSession) -> Result<()>;
    fn update_session_prompt(&self, id: &str, prompt: String) -> Result<()>;
}

pub struct RealIo;

impl RealIo {
    pub fn new() -> Self {
        Self
    }
}

impl SessionIo for RealIo {
    fn load_sessions(&self) -> Result<Vec<SavedSession>> {
        let path = session_file_path()?;
        if !path.exists() {
            return Ok(Vec::new());
        }
        let content = fs::read_to_string(path).into_diagnostic()?;
        if content.trim().is_empty() {
            return Ok(Vec::new());
        }
        let sessions: Vec<SavedSession> = serde_json::from_str(&content).into_diagnostic()?;
        Ok(sessions)
    }

    fn save_session(&self, session: SavedSession) -> Result<()> {
        let mut sessions = self.load_sessions()?;
        // Check if exists, update if so
        if let Some(existing) = sessions.iter_mut().find(|s| s.id == session.id) {
            *existing = session;
        } else {
            sessions.push(session);
        }
        // Sort by date desc
        sessions.sort_by(|a, b| b.date.cmp(&a.date));
        
        write_sessions(&sessions)
    }

    fn update_session_prompt(&self, id: &str, prompt: String) -> Result<()> {
        let mut sessions = self.load_sessions()?;
        if let Some(session) = sessions.iter_mut().find(|s| s.id == id) {
            session.prompt = prompt;
            write_sessions(&sessions)?;
        }
        Ok(())
    }
}

fn write_sessions(sessions: &[SavedSession]) -> Result<()> {
    let path = session_file_path()?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).into_diagnostic()?;
    }
    let content = serde_json::to_string_pretty(sessions).into_diagnostic()?;
    fs::write(path, content).into_diagnostic()?;
    Ok(())
}

fn session_file_path() -> Result<PathBuf> {
    let dirs = ProjectDirs::from("com", "chatui", "kaeru")
        .ok_or_else(|| miette::miette!("Could not determine config directory"))?;
    Ok(dirs.config_dir().join("sessions.json"))
}

pub struct FakeIo {
    sessions: Mutex<Vec<SavedSession>>,
}

impl FakeIo {
    pub fn new() -> Self {
        Self {
            sessions: Mutex::new(Vec::new()),
        }
    }
}

impl SessionIo for FakeIo {
    fn load_sessions(&self) -> Result<Vec<SavedSession>> {
        Ok(self.sessions.lock().unwrap().clone())
    }

    fn save_session(&self, session: SavedSession) -> Result<()> {
        let mut sessions = self.sessions.lock().unwrap();
        if let Some(existing) = sessions.iter_mut().find(|s| s.id == session.id) {
            *existing = session;
        } else {
            sessions.push(session);
        }
        sessions.sort_by(|a, b| b.date.cmp(&a.date));
        Ok(())
    }

    fn update_session_prompt(&self, id: &str, prompt: String) -> Result<()> {
        let mut sessions = self.sessions.lock().unwrap();
        if let Some(session) = sessions.iter_mut().find(|s| s.id == id) {
            session.prompt = prompt;
        }
        Ok(())
    }
}