use directories::ProjectDirs;
use miette::{IntoDiagnostic, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
#[cfg(test)]
use std::sync::Mutex;
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

pub struct RealIo {
    path: PathBuf,
}

impl RealIo {
    pub fn new() -> Result<Self> {
        Ok(Self {
            path: session_file_path()?,
        })
    }

    #[cfg(test)]
    fn for_session_file(path: PathBuf) -> Self {
        Self { path }
    }
}

impl SessionIo for RealIo {
    fn load_sessions(&self) -> Result<Vec<SavedSession>> {
        if !self.path.exists() {
            return Ok(Vec::new());
        }
        let content = fs::read_to_string(&self.path).into_diagnostic()?;
        if content.trim().is_empty() {
            return Ok(Vec::new());
        }
        let sessions: Vec<SavedSession> = serde_json::from_str(&content).into_diagnostic()?;
        Ok(sort_and_dedup_by_id_keep_latest(sessions))
    }

    fn save_session(&self, session: SavedSession) -> Result<()> {
        let mut sessions = self.load_sessions()?;
        sessions.retain(|s| s.id != session.id);
        sessions.push(session);
        sessions.sort_by(|a, b| b.date.cmp(&a.date));

        write_sessions(&self.path, &sessions)
    }

    fn update_session_prompt(&self, id: &str, prompt: String) -> Result<()> {
        let mut sessions = self.load_sessions()?;
        let mut updated = false;
        for session in &mut sessions {
            if session.id == id {
                session.prompt = prompt.clone();
                updated = true;
            }
        }
        if updated {
            let sessions = sort_and_dedup_by_id_keep_latest(sessions);
            write_sessions(&self.path, &sessions)?;
        }
        Ok(())
    }
}

fn write_sessions(path: &PathBuf, sessions: &[SavedSession]) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).into_diagnostic()?;
    }
    let content = serde_json::to_string_pretty(sessions).into_diagnostic()?;
    let tmp_path = path.with_extension("json.tmp");
    fs::write(&tmp_path, content).into_diagnostic()?;
    fs::rename(&tmp_path, path).into_diagnostic()?;
    Ok(())
}

fn session_file_path() -> Result<PathBuf> {
    let dirs = ProjectDirs::from("com", "chatui", "kaeru")
        .ok_or_else(|| miette::miette!("Could not determine config directory"))?;
    Ok(dirs.config_dir().join("sessions.json"))
}

fn sort_and_dedup_by_id_keep_latest(mut sessions: Vec<SavedSession>) -> Vec<SavedSession> {
    use std::collections::HashSet;

    sessions.sort_by(|a, b| b.date.cmp(&a.date));
    let mut seen = HashSet::new();
    sessions.retain(|session| seen.insert(session.id.clone()));
    sessions
}

#[cfg(test)]
pub struct FakeIo {
    sessions: Mutex<Vec<SavedSession>>,
}

#[cfg(test)]
impl FakeIo {
    pub fn new() -> Self {
        Self {
            sessions: Mutex::new(Vec::new()),
        }
    }
}

#[cfg(test)]
impl SessionIo for FakeIo {
    fn load_sessions(&self) -> Result<Vec<SavedSession>> {
        Ok(sort_and_dedup_by_id_keep_latest(
            self.sessions.lock().unwrap().clone(),
        ))
    }

    fn save_session(&self, session: SavedSession) -> Result<()> {
        let mut sessions = self.sessions.lock().unwrap();
        sessions.retain(|s| s.id != session.id);
        sessions.push(session);
        sessions.sort_by(|a, b| b.date.cmp(&a.date));
        Ok(())
    }

    fn update_session_prompt(&self, id: &str, prompt: String) -> Result<()> {
        let mut sessions = self.sessions.lock().unwrap();
        for session in sessions.iter_mut().filter(|s| s.id == id) {
            session.prompt = prompt.clone();
        }
        sessions.sort_by(|a, b| b.date.cmp(&a.date));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TempSessionStore {
        dir: PathBuf,
        file: PathBuf,
    }

    impl TempSessionStore {
        fn new() -> Self {
            use std::sync::atomic::{AtomicUsize, Ordering};

            static COUNTER: AtomicUsize = AtomicUsize::new(0);
            let counter = COUNTER.fetch_add(1, Ordering::Relaxed);

            let mut dir = std::env::temp_dir();
            dir.push(format!(
                "kaeru-session-store-test-{}-{}",
                std::process::id(),
                counter
            ));
            std::fs::create_dir_all(&dir).unwrap();
            let file = dir.join("sessions.json");
            Self { dir, file }
        }
    }

    impl Drop for TempSessionStore {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.dir);
        }
    }

    #[test]
    fn load_sessions_missing_file_returns_empty() {
        let temp = TempSessionStore::new();
        let io = RealIo::for_session_file(temp.file.clone());
        assert_eq!(io.load_sessions().unwrap(), Vec::new());
    }

    #[test]
    fn save_session_overwrites_by_id_and_cleans_tmp_file() {
        let temp = TempSessionStore::new();
        let path = temp.file.clone();
        let io = RealIo::for_session_file(path.clone());

        io.save_session(SavedSession {
            id: "s1".to_string(),
            date: OffsetDateTime::UNIX_EPOCH,
            prompt: "first".to_string(),
        })
        .unwrap();

        io.save_session(SavedSession {
            id: "s1".to_string(),
            date: OffsetDateTime::UNIX_EPOCH + time::Duration::hours(1),
            prompt: "second".to_string(),
        })
        .unwrap();

        let sessions = io.load_sessions().unwrap();
        assert_eq!(sessions.len(), 1);
        assert_eq!(sessions[0].prompt, "second");

        let tmp_path = path.with_extension("json.tmp");
        assert!(!tmp_path.exists());
    }

    #[test]
    fn save_and_load_sessions_sorted_by_date_desc() {
        let temp = TempSessionStore::new();
        let io = RealIo::for_session_file(temp.file.clone());

        io.save_session(SavedSession {
            id: "older".to_string(),
            date: OffsetDateTime::UNIX_EPOCH,
            prompt: "old".to_string(),
        })
        .unwrap();
        io.save_session(SavedSession {
            id: "newer".to_string(),
            date: OffsetDateTime::UNIX_EPOCH + time::Duration::hours(2),
            prompt: "new".to_string(),
        })
        .unwrap();

        let sessions = io.load_sessions().unwrap();
        assert_eq!(
            sessions.iter().map(|s| s.id.as_str()).collect::<Vec<_>>(),
            vec!["newer", "older"]
        );
    }

    #[test]
    fn update_session_prompt_updates_prompt() {
        let temp = TempSessionStore::new();
        let io = RealIo::for_session_file(temp.file.clone());

        io.save_session(SavedSession {
            id: "s1".to_string(),
            date: OffsetDateTime::UNIX_EPOCH,
            prompt: "old".to_string(),
        })
        .unwrap();

        io.update_session_prompt("s1", "updated".to_string())
            .unwrap();

        let sessions = io.load_sessions().unwrap();
        assert_eq!(sessions.len(), 1);
        assert_eq!(sessions[0].prompt, "updated");
    }
}
