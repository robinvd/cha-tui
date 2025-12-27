//! Project management.

use std::path::PathBuf;

use super::session::{Session, SessionId};

/// Unique identifier for a project.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ProjectId(pub u64);

/// Unique identifier for a worktree.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct WorktreeId(pub u64);

/// Unique identifier for a session within a project/worktree.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SessionKey {
    pub project: ProjectId,
    pub worktree: Option<WorktreeId>,
    pub session: SessionId,
}

/// A git worktree containing terminal sessions.
pub struct Worktree {
    pub id: WorktreeId,
    pub name: String,
    pub path: PathBuf,
    pub sessions: Vec<Session>,
    pub next_session_number: usize,
}

impl Worktree {
    /// Create a new worktree with no sessions.
    pub fn new(id: WorktreeId, name: String, path: PathBuf) -> Self {
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

    /// Add a session to this worktree.
    pub fn add_session(&mut self, session: Session) {
        self.sessions.push(session);
        self.next_session_number += 1;
    }

    /// Add a session after another session if present, otherwise append it.
    pub fn add_session_after(&mut self, session: Session, after: Option<SessionId>) {
        if let Some(after) = after
            && let Some(idx) = self.sessions.iter().position(|s| s.id == after)
        {
            self.sessions.insert(idx + 1, session);
            self.next_session_number += 1;
            return;
        }
        self.add_session(session);
    }

    /// Add a session at the start of the list.
    pub fn add_session_at_start(&mut self, session: Session) {
        self.sessions.insert(0, session);
        self.next_session_number += 1;
    }

    /// Remove a session by ID. Returns true if removed.
    pub fn remove_session(&mut self, sid: SessionId) -> bool {
        let len_before = self.sessions.len();
        self.sessions.retain(|s| s.id != sid);
        self.sessions.len() < len_before
    }
}

/// A project containing multiple terminal sessions.
pub struct Project {
    pub id: ProjectId,
    pub name: String,
    pub path: PathBuf,
    pub sessions: Vec<Session>,
    pub worktrees: Vec<Worktree>,
    pub next_session_number: usize,
    pub next_worktree_id: u64,
    pub worktrees_loaded: bool,
    pub worktrees_loading: bool,
}

impl Project {
    /// Create a new project with no sessions.
    pub fn new(id: ProjectId, name: String, path: PathBuf) -> Self {
        Self {
            id,
            name,
            path,
            sessions: Vec::new(),
            worktrees: Vec::new(),
            next_session_number: 1,
            next_worktree_id: 1,
            worktrees_loaded: false,
            worktrees_loading: false,
        }
    }

    /// Find a worktree by ID.
    pub fn worktree(&self, wid: WorktreeId) -> Option<&Worktree> {
        self.worktrees.iter().find(|w| w.id == wid)
    }

    /// Find a worktree by ID (mutable).
    pub fn worktree_mut(&mut self, wid: WorktreeId) -> Option<&mut Worktree> {
        self.worktrees.iter_mut().find(|w| w.id == wid)
    }

    /// Find a worktree by name.
    pub fn worktree_by_name(&self, name: &str) -> Option<&Worktree> {
        self.worktrees.iter().find(|w| w.name == name)
    }

    /// Find a session by ID in the main project or a worktree.
    pub fn session(&self, worktree: Option<WorktreeId>, sid: SessionId) -> Option<&Session> {
        match worktree {
            None => self.sessions.iter().find(|s| s.id == sid),
            Some(wid) => self.worktree(wid)?.session(sid),
        }
    }

    /// Find a session by ID in the main project or a worktree (mutable).
    pub fn session_mut(
        &mut self,
        worktree: Option<WorktreeId>,
        sid: SessionId,
    ) -> Option<&mut Session> {
        match worktree {
            None => self.sessions.iter_mut().find(|s| s.id == sid),
            Some(wid) => self.worktree_mut(wid)?.session_mut(sid),
        }
    }

    /// Move a session up in the list. Returns true if moved.
    pub fn move_session_up(&mut self, worktree: Option<WorktreeId>, sid: SessionId) -> bool {
        match worktree {
            None => {
                let Some(idx) = self.sessions.iter().position(|s| s.id == sid) else {
                    return false;
                };
                if idx == 0 {
                    return false;
                }
                self.sessions.swap(idx, idx - 1);
                true
            }
            Some(wid) => self
                .worktree_mut(wid)
                .map(|wt| wt.move_session_up(sid))
                .unwrap_or(false),
        }
    }

    /// Move a session down in the list. Returns true if moved.
    pub fn move_session_down(&mut self, worktree: Option<WorktreeId>, sid: SessionId) -> bool {
        match worktree {
            None => {
                let Some(idx) = self.sessions.iter().position(|s| s.id == sid) else {
                    return false;
                };
                if idx + 1 >= self.sessions.len() {
                    return false;
                }
                self.sessions.swap(idx, idx + 1);
                true
            }
            Some(wid) => self
                .worktree_mut(wid)
                .map(|wt| wt.move_session_down(sid))
                .unwrap_or(false),
        }
    }

    /// Add a session to this project or worktree.
    pub fn add_session(&mut self, worktree: Option<WorktreeId>, session: Session) {
        match worktree {
            None => {
                self.sessions.push(session);
                self.next_session_number += 1;
            }
            Some(wid) => {
                if let Some(worktree) = self.worktree_mut(wid) {
                    worktree.add_session(session);
                }
            }
        }
    }

    /// Add a session after another session if present, otherwise append it.
    pub fn add_session_after(
        &mut self,
        worktree: Option<WorktreeId>,
        session: Session,
        after: Option<SessionId>,
    ) {
        match worktree {
            None => {
                if let Some(after) = after
                    && let Some(idx) = self.sessions.iter().position(|s| s.id == after)
                {
                    self.sessions.insert(idx + 1, session);
                    self.next_session_number += 1;
                    return;
                }
                self.add_session(None, session);
            }
            Some(wid) => {
                if let Some(worktree) = self.worktree_mut(wid) {
                    worktree.add_session_after(session, after);
                }
            }
        }
    }

    /// Add a session at the start of the list.
    pub fn add_session_at_start(
        &mut self,
        worktree: Option<WorktreeId>,
        session: Session,
    ) {
        match worktree {
            None => {
                self.sessions.insert(0, session);
                self.next_session_number += 1;
            }
            Some(wid) => {
                if let Some(worktree) = self.worktree_mut(wid) {
                    worktree.add_session_at_start(session);
                }
            }
        }
    }

    /// Remove a session by ID. Returns true if removed.
    pub fn remove_session(&mut self, worktree: Option<WorktreeId>, sid: SessionId) -> bool {
        match worktree {
            None => {
                let len_before = self.sessions.len();
                self.sessions.retain(|s| s.id != sid);
                self.sessions.len() < len_before
            }
            Some(wid) => self
                .worktree_mut(wid)
                .map(|wt| wt.remove_session(sid))
                .unwrap_or(false),
        }
    }

    /// Add a worktree and return its id.
    pub fn add_worktree(&mut self, name: String, path: PathBuf) -> WorktreeId {
        let id = WorktreeId(self.next_worktree_id);
        self.next_worktree_id += 1;
        self.worktrees.push(Worktree::new(id, name, path));
        id
    }

    /// Remove a worktree by ID. Returns true if removed.
    pub fn remove_worktree(&mut self, wid: WorktreeId) -> bool {
        let len_before = self.worktrees.len();
        self.worktrees.retain(|w| w.id != wid);
        self.worktrees.len() < len_before
    }
}
