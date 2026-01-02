use std::collections::HashMap;
use std::future::Future;
use std::io;
use std::path::{Path, PathBuf};
use std::pin::Pin;

use chatui::TerminalState;
use tracing::warn;

use crate::persistence;
use crate::project::Project;
use crate::vcs::{self, VcsKind};

pub type IoFuture<T> = Pin<Box<dyn Future<Output = T> + Send>>;

pub struct StartupScript {
    pub script_path: PathBuf,
    pub workspace_path: PathBuf,
    pub env: HashMap<String, String>,
}

pub trait TermIo: Send + Sync {
    fn current_dir(&self) -> io::Result<PathBuf>;
    fn canonicalize_dir(&self, path: &Path) -> io::Result<PathBuf>;
    fn is_dir(&self, path: &Path) -> bool;
    fn path_exists(&self, path: &Path) -> bool;
    fn load_worktrees(&self) -> bool;

    fn load_projects(&self) -> io::Result<Vec<persistence::PersistedProject>>;
    fn save_projects(&self, projects: &[Project]) -> io::Result<()>;

    fn resolve_startup_script(&self, workspace_path: &Path) -> Option<PathBuf>;
    fn run_startup_script(&self, script: StartupScript) -> bool;

    fn detect_vcs_sync(&self, path: &Path) -> VcsKind;
    fn detect_vcs(&self, path: PathBuf) -> IoFuture<VcsKind>;
    fn list_worktrees(
        &self,
        path: PathBuf,
        vcs: VcsKind,
    ) -> IoFuture<Result<Vec<vcs::Worktree>, String>>;
    fn add_worktree(&self, repo_path: &Path, name: &str, vcs: VcsKind) -> io::Result<PathBuf>;
    fn remove_worktree(
        &self,
        repo_path: &Path,
        worktree_path: &Path,
        vcs: VcsKind,
    ) -> io::Result<()>;
    fn copy_optional_file(&self, from_path: &Path, to_path: &Path) -> io::Result<()>;

    fn spawn_terminal(
        &self,
        path: &Path,
        env: HashMap<String, String>,
        binary: Option<&str>,
    ) -> io::Result<TerminalState>;
    fn foreground_process_name(&self, terminal: &TerminalState) -> Option<String>;
}

pub struct RealIo;

impl TermIo for RealIo {
    fn current_dir(&self) -> io::Result<PathBuf> {
        std::env::current_dir()
    }

    fn canonicalize_dir(&self, path: &Path) -> io::Result<PathBuf> {
        path.canonicalize()
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }

    fn path_exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn load_worktrees(&self) -> bool {
        true
    }

    fn load_projects(&self) -> io::Result<Vec<persistence::PersistedProject>> {
        persistence::load_projects()
    }

    fn save_projects(&self, projects: &[Project]) -> io::Result<()> {
        persistence::save_projects(projects)
    }

    fn resolve_startup_script(&self, workspace_path: &Path) -> Option<PathBuf> {
        let project_script = workspace_path.join(".term").join("init_project");
        if project_script.is_file() {
            return Some(project_script);
        }
        let config_script = directories::BaseDirs::new()?
            .home_dir()
            .join(".config")
            .join("term")
            .join("init_project");
        if config_script.is_file() {
            return Some(config_script);
        }
        None
    }

    fn run_startup_script(&self, script: StartupScript) -> bool {
        let mut command = std::process::Command::new(&script.script_path);
        command.current_dir(&script.workspace_path);
        command.envs(script.env);
        match command.status() {
            Ok(status) => status.success(),
            Err(_) => false,
        }
    }

    fn detect_vcs_sync(&self, path: &Path) -> VcsKind {
        vcs::detect(path)
    }

    fn detect_vcs(&self, path: PathBuf) -> IoFuture<VcsKind> {
        Box::pin(async move { vcs::detect_async(&path).await })
    }

    fn list_worktrees(
        &self,
        path: PathBuf,
        vcs: VcsKind,
    ) -> IoFuture<Result<Vec<vcs::Worktree>, String>> {
        Box::pin(async move {
            vcs::list_worktrees_async(&path, vcs)
                .await
                .map_err(|err| err.to_string())
        })
    }

    fn add_worktree(&self, repo_path: &Path, name: &str, vcs: VcsKind) -> io::Result<PathBuf> {
        vcs::add_worktree(repo_path, name, vcs)
    }

    fn remove_worktree(
        &self,
        repo_path: &Path,
        worktree_path: &Path,
        vcs: VcsKind,
    ) -> io::Result<()> {
        vcs::remove_worktree(repo_path, worktree_path, vcs)
    }

    fn copy_optional_file(&self, from_path: &Path, to_path: &Path) -> io::Result<()> {
        std::fs::copy(from_path, to_path).map(|_| ())
    }

    fn spawn_terminal(
        &self,
        path: &Path,
        env: HashMap<String, String>,
        binary: Option<&str>,
    ) -> io::Result<TerminalState> {
        TerminalState::with_working_dir_and_env_and_shell(path, env, binary).or_else(|err| {
            warn!(
                ?err,
                "failed to start terminal in project dir, falling back to default"
            );
            TerminalState::new()
        })
    }

    fn foreground_process_name(&self, terminal: &TerminalState) -> Option<String> {
        terminal.foreground_process_name()
    }
}

#[cfg(test)]
pub struct FakeIo;

#[cfg(test)]
impl FakeIo {
    pub fn new() -> Self {
        Self
    }
}

#[cfg(test)]
impl TermIo for FakeIo {
    fn current_dir(&self) -> io::Result<PathBuf> {
        std::env::current_dir()
    }

    fn canonicalize_dir(&self, path: &Path) -> io::Result<PathBuf> {
        Ok(path.to_path_buf())
    }

    fn is_dir(&self, _path: &Path) -> bool {
        true
    }

    fn path_exists(&self, _path: &Path) -> bool {
        false
    }

    fn load_worktrees(&self) -> bool {
        false
    }

    fn load_projects(&self) -> io::Result<Vec<persistence::PersistedProject>> {
        Ok(Vec::new())
    }

    fn save_projects(&self, _projects: &[Project]) -> io::Result<()> {
        Ok(())
    }

    fn resolve_startup_script(&self, _workspace_path: &Path) -> Option<PathBuf> {
        None
    }

    fn run_startup_script(&self, _script: StartupScript) -> bool {
        true
    }

    fn detect_vcs_sync(&self, _path: &Path) -> VcsKind {
        VcsKind::Git
    }

    fn detect_vcs(&self, _path: PathBuf) -> IoFuture<VcsKind> {
        Box::pin(async { VcsKind::Git })
    }

    fn list_worktrees(
        &self,
        _path: PathBuf,
        _vcs: VcsKind,
    ) -> IoFuture<Result<Vec<vcs::Worktree>, String>> {
        Box::pin(async { Ok(Vec::new()) })
    }

    fn add_worktree(&self, repo_path: &Path, name: &str, _vcs: VcsKind) -> io::Result<PathBuf> {
        Ok(repo_path.join(name))
    }

    fn remove_worktree(
        &self,
        _repo_path: &Path,
        _worktree_path: &Path,
        _vcs: VcsKind,
    ) -> io::Result<()> {
        Ok(())
    }

    fn copy_optional_file(&self, _from_path: &Path, _to_path: &Path) -> io::Result<()> {
        Ok(())
    }

    fn spawn_terminal(
        &self,
        _path: &Path,
        _env: HashMap<String, String>,
        binary: Option<&str>,
    ) -> io::Result<TerminalState> {
        if let Some(binary) = binary {
            return TerminalState::spawn(binary, &[]);
        }
        TerminalState::spawn("true", &[])
    }

    fn foreground_process_name(&self, _terminal: &TerminalState) -> Option<String> {
        None
    }
}
