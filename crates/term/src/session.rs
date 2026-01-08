//! Session management for terminal sessions.

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use chatui::{TerminalMsg, TerminalNotification, TerminalState};
use smol::channel::Receiver;

use crate::term_io::TermIo;

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
    io: Arc<dyn TermIo>,
}

impl Session {
    /// Create a new session with a spawned terminal.
    pub fn spawn(
        io: Arc<dyn TermIo>,
        path: &Path,
        number: usize,
        id: SessionId,
        env: HashMap<String, String>,
        binary: Option<&str>,
    ) -> std::io::Result<Self> {
        let terminal = io.spawn_terminal(path, env, binary)?;
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
            io,
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
            .or_else(|| self.io.foreground_process_name(&self.terminal))
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
    pub fn sync_title(&mut self) -> TitleSync {
        let new_title = self.terminal.title();
        if new_title != self.title {
            self.title = new_title;
        }
        let display_changed = self.sync_display_name();
        TitleSync { display_changed }
    }

    /// Check and consume bell state, returns true if bell state changed.
    pub fn sync_bell(&mut self, _is_active: bool) -> BellSync {
        let triggered = self.terminal.take_bell();
        let mut changed = false;

        if triggered && !self.bell {
            self.bell = true;
            changed = true;
        }

        BellSync { triggered, changed }
    }

    /// Clear bell indicator, returns true if it was set.
    pub fn clear_bell(&mut self) -> bool {
        if self.bell {
            self.bell = false;
            return true;
        }
        false
    }

    /// Update activity tracking for background sessions.
    /// Returns true if unread indicator changed.
    pub fn sync_activity(&mut self, is_active: bool) -> bool {
        let version = self.terminal.text_content_version();
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
        let version = self.terminal.text_content_version();
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

    pub fn take_notifications(&self) -> Vec<TerminalNotification> {
        self.terminal.take_notifications()
    }
}

pub struct BellSync {
    pub triggered: bool,
    pub changed: bool,
}

pub struct TitleSync {
    pub display_changed: bool,
}
