use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Clone, Debug)]
pub struct JjWorkspace {
    pub name: String,
    pub path: PathBuf,
}

pub fn root(repo_path: &Path) -> io::Result<PathBuf> {
    let output = jj_output(repo_path, &["root"])?;
    let root = output.lines().next().unwrap_or("").trim();
    if root.is_empty() {
        Err(io::Error::other("jj root returned empty output"))
    } else {
        Ok(PathBuf::from(root))
    }
}

pub fn list_workspaces(repo_path: &Path) -> io::Result<Vec<JjWorkspace>> {
    let repo_root = root(repo_path)?;
    let repo_root = repo_root
        .canonicalize()
        .unwrap_or_else(|_| repo_root.to_path_buf());
    let worktrees_dir = repo_root.join(".worktrees");
    let output = jj_output(
        &repo_root,
        &["--no-pager", "workspace", "list", "-T", "name ++ \"\\n\""],
    )?;
    let mut worktrees = Vec::new();

    for name in output.lines().map(str::trim).filter(|name| !name.is_empty()) {
        if name == "default" {
            continue;
        }
        let path = worktrees_dir.join(name);
        if !path.exists() {
            continue;
        }
        worktrees.push(JjWorkspace {
            name: name.to_string(),
            path,
        });
    }

    Ok(worktrees)
}

pub fn add_workspace(repo_path: &Path, name: &str) -> io::Result<PathBuf> {
    let repo_root = root(repo_path)?;
    let repo_root = repo_root
        .canonicalize()
        .unwrap_or_else(|_| repo_root.to_path_buf());
    let worktrees_dir = repo_root.join(".worktrees");
    let path = worktrees_dir.join(name);
    std::fs::create_dir_all(&worktrees_dir)?;

    let path_arg = path.to_string_lossy().to_string();
    let output = run_jj(&repo_root, &["workspace", "add", &path_arg])?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(stderr.trim().to_string()));
    }

    Ok(path)
}

pub fn remove_workspace(repo_path: &Path, path: &Path) -> io::Result<()> {
    let path_arg = path.to_string_lossy().to_string();
    let repo_root = root(repo_path)?;
    let output = run_jj(&repo_root, &["workspace", "forget", &path_arg])?;
    if output.status.success() {
        remove_workspace_dir(path)?;
        return Ok(());
    }

    let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
    if name.is_empty() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(stderr.trim().to_string()));
    }

    let output = run_jj(&repo_root, &["workspace", "forget", name])?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(stderr.trim().to_string()));
    }
    remove_workspace_dir(path)?;
    Ok(())
}

fn remove_workspace_dir(path: &Path) -> io::Result<()> {
    if path.exists() {
        std::fs::remove_dir_all(path)?;
    }
    Ok(())
}

fn run_jj(repo_path: &Path, args: &[&str]) -> io::Result<std::process::Output> {
    Command::new("jj").current_dir(repo_path).args(args).output()
}

fn jj_output(repo_path: &Path, args: &[&str]) -> io::Result<String> {
    let output = run_jj(repo_path, args)?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(stderr.trim().to_string()));
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    Ok(stdout.to_string())
}
