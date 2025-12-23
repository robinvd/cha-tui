//! Git helpers for worktree management.

use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Clone, Debug)]
pub struct GitWorktree {
    pub name: String,
    pub path: PathBuf,
}

fn run_git(repo_path: &Path, args: &[&str]) -> io::Result<std::process::Output> {
    Command::new("git")
        .arg("-C")
        .arg(repo_path)
        .args(args)
        .output()
}

fn git_output(repo_path: &Path, args: &[&str]) -> io::Result<String> {
    let output = run_git(repo_path, args)?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(stderr.trim().to_string()));
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    Ok(stdout.to_string())
}

pub fn list_worktrees(repo_path: &Path) -> io::Result<Vec<GitWorktree>> {
    let repo_path = repo_path
        .canonicalize()
        .unwrap_or_else(|_| repo_path.to_path_buf());
    let worktrees_dir = repo_path.join(".worktrees");
    let output = git_output(&repo_path, &["worktree", "list", "--porcelain"])?;

    let mut worktrees = Vec::new();
    let mut current_path: Option<PathBuf> = None;

    let mut finalize = |path: Option<PathBuf>| {
        if let Some(path) = path {
            if !path.starts_with(&worktrees_dir) {
                return;
            }
            if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                worktrees.push(GitWorktree {
                    name: name.to_string(),
                    path,
                });
            }
        }
    };

    for line in output.lines() {
        if let Some(rest) = line.strip_prefix("worktree ") {
            finalize(current_path.take());
            current_path = Some(PathBuf::from(rest.trim()));
        } else if line.trim().is_empty() {
            finalize(current_path.take());
        }
    }
    finalize(current_path.take());

    Ok(worktrees)
}

pub fn branch_exists(repo_path: &Path, name: &str) -> io::Result<bool> {
    let ref_name = format!("refs/heads/{name}");
    let output = run_git(repo_path, &["show-ref", "--verify", &ref_name])?;
    Ok(output.status.success())
}

pub fn add_worktree(repo_path: &Path, name: &str) -> io::Result<PathBuf> {
    let repo_path = repo_path
        .canonicalize()
        .unwrap_or_else(|_| repo_path.to_path_buf());
    let worktrees_dir = repo_path.join(".worktrees");
    let path = worktrees_dir.join(name);
    std::fs::create_dir_all(&worktrees_dir)?;

    let use_existing_branch = branch_exists(&repo_path, name)?;
    let path_arg = path.to_string_lossy().to_string();
    let args = if use_existing_branch {
        vec!["worktree", "add", &path_arg, name]
    } else {
        vec!["worktree", "add", "-b", name, &path_arg]
    };

    let output = run_git(&repo_path, &args)?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(stderr.trim().to_string()));
    }

    Ok(path)
}

pub fn remove_worktree(repo_path: &Path, path: &Path) -> io::Result<()> {
    let path_arg = path.to_string_lossy().to_string();
    let output = run_git(repo_path, &["worktree", "remove", &path_arg])?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(stderr.trim().to_string()));
    }
    Ok(())
}
