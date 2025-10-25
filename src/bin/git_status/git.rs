use std::ffi::OsStr;
use std::process::Command;

use color_eyre::eyre::{Context, Result, eyre};

pub const PREVIEW_BYTE_LIMIT: usize = 1_048_576;

#[derive(Clone, Debug)]
pub struct FileEntry {
    pub path: String,
    pub display: String,
    pub code: char,
}

impl FileEntry {
    pub fn new(path: String, code: char, display: String) -> Self {
        Self {
            path,
            display,
            code,
        }
    }

    pub fn tree_label(&self) -> String {
        let rest = self
            .display
            .split_once(' ')
            .map(|(_, tail)| tail)
            .unwrap_or(self.display.as_str());

        if let Some((before, after)) = rest.split_once(" -> ") {
            format!("{} -> {}", last_component(before), last_component(after))
        } else {
            last_component(rest).to_string()
        }
    }
}

fn last_component(path: &str) -> &str {
    path.rsplit('/')
        .next()
        .filter(|component| !component.is_empty())
        .unwrap_or(path)
}

#[derive(Clone, Debug)]
pub struct GitStatus {
    pub unstaged: Vec<FileEntry>,
    pub staged: Vec<FileEntry>,
    pub branch: Option<String>,
}

pub fn load_status() -> Result<GitStatus> {
    let output = run_git(["status", "--porcelain", "--untracked-files=all"])?;

    let mut unstaged = Vec::new();
    let mut staged = Vec::new();

    for line in output.lines() {
        if line.trim().is_empty() {
            continue;
        }

        if let Some(parsed) = ParsedStatus::parse(line) {
            if parsed.include_unstaged() {
                unstaged.push(FileEntry::new(
                    parsed.path.clone(),
                    parsed.unstaged_code,
                    parsed.unstaged_display(),
                ));
            }

            if parsed.include_staged() {
                staged.push(FileEntry::new(
                    parsed.path.clone(),
                    parsed.staged_code,
                    parsed.staged_display(),
                ));
            }
        }
    }

    let branch = load_current_branch();

    Ok(GitStatus {
        unstaged,
        staged,
        branch,
    })
}

fn load_current_branch() -> Option<String> {
    let branch = run_git(["symbolic-ref", "--short", "HEAD"])
        .or_else(|_| run_git(["rev-parse", "--short", "HEAD"]))
        .ok()?;

    let trimmed = branch.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

struct ParsedStatus {
    staged_code: char,
    unstaged_code: char,
    path: String,
    display_path: String,
}

impl ParsedStatus {
    fn parse(line: &str) -> Option<Self> {
        if line.len() < 4 {
            return None;
        }

        let mut chars = line.chars();
        let staged_code = chars.next()?;
        let unstaged_code = chars.next()?;

        // Skip the space after status codes.
        let remainder = line[3..].to_string();
        let path = remainder
            .split(" -> ")
            .last()
            .map(str::to_string)
            .unwrap_or_else(|| remainder.clone());

        Some(Self {
            staged_code,
            unstaged_code,
            path,
            display_path: remainder,
        })
    }

    fn include_unstaged(&self) -> bool {
        self.unstaged_code != ' ' || self.staged_code == '?'
    }

    fn include_staged(&self) -> bool {
        self.staged_code != ' ' && self.staged_code != '?'
    }

    fn unstaged_display(&self) -> String {
        format!("{} {}", self.unstaged_code, self.display_path)
    }

    fn staged_display(&self) -> String {
        format!("{} {}", self.staged_code, self.display_path)
    }
}

pub struct LoadedContent {
    pub text: String,
    pub truncated: bool,
}

pub fn read_index_file(path: &str) -> Result<Option<LoadedContent>> {
    git_show(&format!(":{}", path))
}

pub fn read_head_file(path: &str) -> Result<Option<LoadedContent>> {
    git_show(&format!("HEAD:{}", path))
}

fn git_show(spec: &str) -> Result<Option<LoadedContent>> {
    let output = Command::new("git")
        .arg("show")
        .arg(spec)
        .env("GIT_PAGER", "cat")
        .output()
        .wrap_err_with(|| format!("git show {}", spec))?;

    if output.status.success() {
        let stdout = output.stdout;
        let truncated = stdout.len() > PREVIEW_BYTE_LIMIT;
        let slice = if truncated {
            &stdout[..PREVIEW_BYTE_LIMIT]
        } else {
            stdout.as_slice()
        };
        let text = String::from_utf8_lossy(slice).into_owned();
        return Ok(Some(LoadedContent { text, truncated }));
    }

    if matches!(output.status.code(), Some(128)) {
        return Ok(None);
    }

    Err(eyre!(
        "git show {} failed: {}",
        spec,
        String::from_utf8_lossy(&output.stderr)
    ))
}

pub fn stage_path(path: &str) -> Result<()> {
    run_git(["add", "--", path]).map(|_| ())
}

pub fn unstage_path(path: &str) -> Result<()> {
    run_git(["reset", "HEAD", "--", path]).map(|_| ())
}

pub fn discard_unstaged_changes(path: &str, code: char) -> Result<()> {
    // Untracked files ('?') are removed; tracked files are restored from index/HEAD.
    if code == '?' {
        return run_git(["clean", "-f", "--", path]).map(|_| ());
    }

    // Restore the worktree copy to the index state (discarding unstaged changes).
    run_git(["restore", "--worktree", "--", path]).map(|_| ())
}

pub fn run_git<I, S>(args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    run_git_internal(args)
}

fn run_git_internal<I, S>(args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args_vec: Vec<String> = args
        .into_iter()
        .map(|arg| arg.as_ref().to_string_lossy().into_owned())
        .collect();

    let output = Command::new("git")
        .args(&args_vec)
        .env("GIT_PAGER", "cat")
        .output()
        .wrap_err_with(|| format!("failed to execute git {}", args_vec.join(" ")))?;

    let status = output.status;
    if !status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(eyre!(
            "git {} failed: {}",
            args_vec.join(" "),
            stderr.trim()
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}
