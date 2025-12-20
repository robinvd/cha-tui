//! Project management.

use std::path::PathBuf;

use super::session::{Session, SessionId};

/// Unique identifier for a project.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ProjectId(pub u64);

/// A project containing multiple terminal sessions.
pub struct Project {
    pub id: ProjectId,
    pub name: String,
    pub path: PathBuf,
    pub sessions: Vec<Session>,
    pub next_session_number: usize,
}

impl Project {
    /// Create a new project with no sessions.
    pub fn new(id: ProjectId, name: String, path: PathBuf) -> Self {
        Self {
            id,
            name,
            path,
            sessions: Vec::new(),
            next_session_number: 1,
        }
    }

    /// Find a session by ID.
    pub fn session(&self, sid: SessionId) -> Option<&Session> {
        self.sessions.iter().find(|s| s.id == sid)
    }

    /// Find a session by ID (mutable).
    pub fn session_mut(&mut self, sid: SessionId) -> Option<&mut Session> {
        self.sessions.iter_mut().find(|s| s.id == sid)
    }

    /// Move a session up in the list. Returns true if moved.
    pub fn move_session_up(&mut self, sid: SessionId) -> bool {
        let Some(idx) = self.sessions.iter().position(|s| s.id == sid) else {
            return false;
        };
        if idx == 0 {
            return false;
        }
        self.sessions.swap(idx, idx - 1);
        true
    }

    /// Move a session down in the list. Returns true if moved.
    pub fn move_session_down(&mut self, sid: SessionId) -> bool {
        let Some(idx) = self.sessions.iter().position(|s| s.id == sid) else {
            return false;
        };
        if idx + 1 >= self.sessions.len() {
            return false;
        }
        self.sessions.swap(idx, idx + 1);
        true
    }

    /// Add a session to this project.
    pub fn add_session(&mut self, session: Session) {
        self.sessions.push(session);
        self.next_session_number += 1;
    }

    /// Remove a session by ID. Returns true if removed.
    pub fn remove_session(&mut self, sid: SessionId) -> bool {
        let len_before = self.sessions.len();
        self.sessions.retain(|s| s.id != sid);
        self.sessions.len() < len_before
    }
}
