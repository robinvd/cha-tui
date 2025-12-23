use std::io;
use std::path::{Path, PathBuf};
use super::{git, jj};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VcsKind {
    Git,
    Jj,
}

impl VcsKind {
    pub fn toggle(self) -> Self {
        match self {
            VcsKind::Git => VcsKind::Jj,
            VcsKind::Jj => VcsKind::Git,
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            VcsKind::Git => "git",
            VcsKind::Jj => "jj",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Worktree {
    pub name: String,
    pub path: PathBuf,
}

pub fn detect(repo_path: &Path) -> VcsKind {
    if jj::root(repo_path).is_ok() {
        VcsKind::Jj
    } else {
        VcsKind::Git
    }
}

pub fn list_worktrees(repo_path: &Path, vcs: VcsKind) -> io::Result<Vec<Worktree>> {
    match vcs {
        VcsKind::Git => git::list_worktrees(repo_path).map(|trees| {
            trees
                .into_iter()
                .map(|tree| Worktree {
                    name: tree.name,
                    path: tree.path,
                })
                .collect()
        }),
        VcsKind::Jj => jj::list_workspaces(repo_path).map(|trees| {
            trees
                .into_iter()
                .map(|tree| Worktree {
                    name: tree.name,
                    path: tree.path,
                })
                .collect()
        }),
    }
}

pub fn add_worktree(repo_path: &Path, name: &str, vcs: VcsKind) -> io::Result<PathBuf> {
    match vcs {
        VcsKind::Git => git::add_worktree(repo_path, name),
        VcsKind::Jj => jj::add_workspace(repo_path, name),
    }
}

pub fn remove_worktree(repo_path: &Path, path: &Path, vcs: VcsKind) -> io::Result<()> {
    match vcs {
        VcsKind::Git => git::remove_worktree(repo_path, path),
        VcsKind::Jj => jj::remove_workspace(repo_path, path),
    }
}
