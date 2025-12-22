//! OS-specific utilities for process inspection.

use std::os::fd::RawFd;

use tracing::debug;

/// Get a descriptive name for the foreground process controlling the PTY.
///
/// This attempts to format the process's argv so interpreter invocations show
/// the target script/binary (e.g. `python main.py`), falling back to the
/// executable name if argv is unavailable.
#[cfg(unix)]
pub fn foreground_process_name(pty_fd: RawFd) -> Option<String> {
    let pgrp = unsafe { libc::tcgetpgrp(pty_fd) };
    debug!(pty_fd, pgrp, "tcgetpgrp result");
    if pgrp <= 0 {
        return None;
    }

    let pid = pgrp as u32;

    if let Some(args) = get_process_cmdline(pid).and_then(format_cmdline) {
        debug!(pid, %args, "foreground process argv");
        return Some(args);
    }

    let fallback = get_process_name(pid);
    debug!(pid, ?fallback, "foreground process name fallback");
    fallback
}

#[cfg(target_os = "macos")]
fn get_process_name(pid: u32) -> Option<String> {
    use std::ffi::CStr;
    use std::path::Path;

    // First try proc_name (works for processes we own)
    const MAXCOMLEN: usize = 16;
    let mut name = [0i8; MAXCOMLEN + 1];

    let result = unsafe {
        libc::proc_name(
            pid as i32,
            name.as_mut_ptr() as *mut libc::c_void,
            name.len() as u32,
        )
    };

    if result > 0 {
        let cstr = unsafe { CStr::from_ptr(name.as_ptr()) };
        let name = cstr.to_string_lossy().into_owned();
        debug!(pid, %name, "proc_name succeeded");
        return Some(name);
    }

    // Fall back to proc_pidpath and extract the binary name
    let mut path_buf = [0u8; libc::PROC_PIDPATHINFO_MAXSIZE as usize];
    let result = unsafe {
        libc::proc_pidpath(
            pid as i32,
            path_buf.as_mut_ptr() as *mut libc::c_void,
            path_buf.len() as u32,
        )
    };

    debug!(pid, result, "proc_pidpath result");

    if result > 0 {
        let path_str = CStr::from_bytes_until_nul(&path_buf)
            .ok()
            .map(|s| s.to_string_lossy().into_owned())?;
        let name = Path::new(&path_str)
            .file_name()
            .and_then(|n| n.to_str())
            .map(|s| s.to_string());
        debug!(pid, ?name, %path_str, "proc_pidpath succeeded");
        return name;
    }

    debug!(pid, "both proc_name and proc_pidpath failed");
    None
}

#[cfg(target_os = "linux")]
fn get_process_name(pid: u32) -> Option<String> {
    std::fs::read_to_string(format!("/proc/{}/comm", pid))
        .ok()
        .map(|s| s.trim().to_string())
}

#[cfg(target_os = "linux")]
fn get_process_cmdline(pid: u32) -> Option<Vec<String>> {
    let data = std::fs::read(format!("/proc/{pid}/cmdline")).ok()?;
    if data.is_empty() {
        return None;
    }

    let args = data
        .split(|b| *b == 0)
        .filter_map(|part| {
            if part.is_empty() {
                None
            } else {
                Some(String::from_utf8_lossy(part).into_owned())
            }
        })
        .collect::<Vec<_>>();

    if args.is_empty() { None } else { Some(args) }
}

#[cfg(target_os = "macos")]
fn get_process_cmdline(pid: u32) -> Option<Vec<String>> {
    use libc::{c_int, c_void, sysctl, CTL_KERN, KERN_PROCARGS2};

    let mut mib = [CTL_KERN, KERN_PROCARGS2, pid as c_int];
    let mut size: usize = 0;

    unsafe {
        if sysctl(
            mib.as_mut_ptr(),
            mib.len() as u32,
            std::ptr::null_mut(),
            &mut size,
            std::ptr::null_mut(),
            0,
        ) != 0
        {
            return None;
        }

        let mut buf = vec![0u8; size.max(std::mem::size_of::<c_int>())];
        if sysctl(
            mib.as_mut_ptr(),
            mib.len() as u32,
            buf.as_mut_ptr() as *mut c_void,
            &mut size,
            std::ptr::null_mut(),
            0,
        ) != 0
        {
            return None;
        }

        buf.truncate(size);

        // Layout: argc (int) + exec path + argv strings (null separated) + env.
        let argc_size = std::mem::size_of::<c_int>();
        if buf.len() < argc_size {
            return None;
        }

        let argc = c_int::from_ne_bytes(buf[..argc_size].try_into().ok()?);
        if argc <= 0 {
            return None;
        }

        let mut offset = argc_size;

        // Skip exec path (null-terminated)
        while offset < buf.len() && buf[offset] != 0 {
            offset += 1;
        }
        while offset < buf.len() && buf[offset] == 0 {
            offset += 1;
        }

        let mut args = Vec::new();
        while offset < buf.len() && (args.len() as c_int) < argc {
            if let Some(rel_end) = buf[offset..].iter().position(|b| *b == 0) {
                let end = offset + rel_end;
                if end > offset
                    && let Ok(arg) = std::str::from_utf8(&buf[offset..end])
                {
                    args.push(arg.to_string());
                }
                offset = end + 1;
            } else {
                break;
            }
        }

        if args.is_empty() {
            None
        } else {
            Some(args)
        }
    }
}

#[cfg(not(unix))]
fn get_process_cmdline(_pid: u32) -> Option<Vec<String>> {
    None
}

fn format_cmdline(args: Vec<String>) -> Option<String> {
    if args.is_empty() {
        return None;
    }

    // Drop leading environment assignments so the display focuses on the command.
    fn is_env_assignment(arg: &str) -> bool {
        let (key, value) = match arg.split_once('=') {
            Some(pair) => pair,
            None => return false,
        };
        !key.is_empty()
            && !value.is_empty()
            && key
                .bytes()
                .enumerate()
                .all(|(i, b)| (i == 0 && (b.is_ascii_alphabetic() || b == b'_'))
                    || (i > 0 && (b.is_ascii_alphanumeric() || b == b'_')))
    }

    let mut args = args;
    while !args.is_empty() && is_env_assignment(&args[0]) {
        args.remove(0);
    }

    if args.is_empty() {
        return None;
    }

    let mut parts = Vec::with_capacity(args.len());
    let args_iter = args.into_iter().enumerate();

    for (idx, arg) in args_iter {
        if idx == 0 {
            let mut display = arg;
            if display.starts_with('-') && display.len() > 1 {
                display = display.trim_start_matches('-').to_string();
            }
            let display = std::path::Path::new(&display)
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or(&display)
                .to_string();
            parts.push(display);
        } else {
            parts.push(arg);
        }
    }

    let joined = parts.join(" ");
    const MAX_LEN: usize = 80;
    const EXTRA_CHARS: usize = 3;
    if joined.chars().count() > MAX_LEN {
        let limit = MAX_LEN + EXTRA_CHARS;
        let truncated: String = joined.chars().take(limit).collect();
        Some(format!("{}...", truncated.trim_end()))
    } else {
        Some(joined)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_cmdline() {
        let cases = vec![
            (vec!["ls"], Some("ls")),
            (vec!["/usr/bin/ls", "-la"], Some("ls -la")),
            (vec!["python3", "main.py"], Some("python3 main.py")),
            (vec!["-zsh"], Some("zsh")),
            (vec!["DEBUG=1", "./app"], Some("app")),
            (vec!["VAR1=1", "VAR2=2", "cmd", "arg"], Some("cmd arg")),
            (vec!["cmd", "ARG=val"], Some("cmd ARG=val")),
            (
                vec!["verylongcommandname" ; 10],
                Some("verylongcommandname verylongcommandname verylongcommandname verylongcommandname ver..."),
            ),
        ];

        for (input, expected) in cases {
            let input = input.into_iter().map(|s| s.to_string()).collect();
            assert_eq!(format_cmdline(input), expected.map(|s| s.to_string()));
        }
    }
}

#[cfg(not(unix))]
pub fn foreground_process_name(_pty_fd: RawFd) -> Option<String> {
    None
}
