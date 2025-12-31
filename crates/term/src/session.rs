//! Session management for terminal sessions.

use std::collections::HashMap;
use std::path::Path;

use chatui::{TerminalMsg, TerminalState};
use smol::channel::Receiver;
#[cfg(not(test))]
use tracing::warn;

/// Unique identifier for a session.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SessionId(pub u64);

/// A terminal session within a project.
pub struct Session {
    pub id: SessionId,
    pub number: usize,
    pub title: Option<String>,
    pub custom_title: Option<String>,
    pub bell: bool,
    pub exited: bool,
    pub terminal: TerminalState,
    pub wakeup: Receiver<()>,
    /// Cached display name to detect changes from foreground process.
    pub cached_display_name: String,
    /// Whether the session has unseen output.
    pub has_unread_output: bool,
    /// Last terminal content version that was observed.
    pub last_seen_content_version: u64,
}

impl Session {
    /// Create a new session with a spawned terminal.
    pub fn spawn(
        path: &Path,
        number: usize,
        id: SessionId,
        env: HashMap<String, String>,
    ) -> std::io::Result<Self> {
        #[cfg(test)]
        let _ = path;
        #[cfg(test)]
        let _ = &env;

        #[cfg(test)]
        let terminal = TerminalState::spawn("true", &[]).unwrap();

        #[cfg(not(test))]
        let terminal = TerminalState::with_working_dir_and_env(path, env).or_else(|err| {
            warn!(
                ?err,
                "failed to start terminal in project dir, falling back to default"
            );
            TerminalState::new()
        })?;
        let wakeup = terminal.wakeup_receiver();
        let mut session = Session {
            id,
            number,
            title: None,
            custom_title: None,
            bell: false,
            exited: false,
            terminal,
            wakeup,
            cached_display_name: String::new(),
            has_unread_output: false,
            last_seen_content_version: 0,
        };
        session.last_seen_content_version = session.terminal.content_version();
        session.cached_display_name = session.display_name();
        Ok(session)
    }

    /// Generate the display name for this session.
    pub fn display_name(&self) -> String {
        let base = self
            .custom_title
            .clone()
            .or_else(|| self.title.clone())
            .or_else(|| {
                #[cfg(not(test))]
                {
                    self.terminal.foreground_process_name()
                }
                #[cfg(test)]
                {
                    None
                }
            })
            .unwrap_or_else(|| format!("session{}", self.number));
        if self.exited {
            format!("{base} [exited]")
        } else if self.bell {
            format!("{base} 🔔")
        } else {
            base
        }
    }

    /// Update cached display name, returns true if it changed.
    pub fn sync_display_name(&mut self) -> bool {
        let new_name = self.display_name();
        if new_name != self.cached_display_name {
            self.cached_display_name = new_name;
            true
        } else {
            false
        }
    }

    /// Sync OSC title from terminal, returns true if display name changed.
    pub fn sync_title(&mut self) -> bool {
        let new_title = self.terminal.title();
        if new_title != self.title {
            self.title = new_title;
        }
        self.sync_display_name()
    }

    /// Check and consume bell state, returns true if bell state changed.
    pub fn sync_bell(&mut self, is_active: bool) -> bool {
        let triggered = self.terminal.take_bell();

        if is_active {
            if self.bell {
                self.bell = false;
                return true;
            }
            return false;
        }

        if triggered && !self.bell {
            self.bell = true;
            return true;
        }

        false
    }

    /// Clear bell indicator, returns true if it was set.
    pub fn clear_bell(&mut self) -> bool {
        if self.bell {
            self.bell = false;
            return true;
        }
        false
    }

    /// Check if process exited, returns true if state changed.
    pub fn sync_exited(&mut self) -> bool {
        if !self.terminal.is_running() && !self.exited {
            self.exited = true;
            return true;
        }
        false
    }

    /// Update activity tracking for background sessions.
    /// Returns true if unread indicator changed.
    pub fn sync_activity(&mut self, is_active: bool) -> bool {
        let version = self.terminal.content_version();
        let mut changed = false;

        if is_active {
            if self.has_unread_output {
                self.has_unread_output = false;
                changed = true;
            }
            if self.last_seen_content_version != version {
                self.last_seen_content_version = version;
            }
        } else if version > self.last_seen_content_version {
            self.last_seen_content_version = version;
            if !self.has_unread_output {
                self.has_unread_output = true;
                changed = true;
            }
        }

        changed
    }

    /// Mark session as seen (clears unread indicator).
    /// Returns true if state changed.
    pub fn mark_seen(&mut self) -> bool {
        let version = self.terminal.content_version();
        let mut changed = false;

        if self.has_unread_output {
            self.has_unread_output = false;
            changed = true;
        }

        if self.last_seen_content_version != version {
            self.last_seen_content_version = version;
        }

        changed
    }

    /// Forward a terminal message to this session.
    pub fn update(&mut self, msg: TerminalMsg) {
        self.terminal.update(msg);
    }
}
