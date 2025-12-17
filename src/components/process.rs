//! OS-specific utilities for process inspection.

use std::os::fd::RawFd;

use tracing::debug;

/// Get the name of the foreground process for a PTY.
///
/// Returns `None` if the foreground process cannot be determined.
#[cfg(unix)]
pub fn foreground_process_name(pty_fd: RawFd) -> Option<String> {
    // Get foreground process group
    let pgrp = unsafe { libc::tcgetpgrp(pty_fd) };
    debug!(pty_fd, pgrp, "tcgetpgrp result");
    if pgrp <= 0 {
        return None;
    }

    let name = get_process_name(pgrp as u32);
    debug!(pgrp, ?name, "foreground process name");
    name
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

#[cfg(not(unix))]
pub fn foreground_process_name(_pty_fd: RawFd) -> Option<String> {
    None
}
