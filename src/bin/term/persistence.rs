//! Persistence layer for saving and loading project configuration.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::{fs, io};

use super::project::Project;

/// Serializable representation of a project for persistence.
#[derive(Serialize, Deserialize)]
pub struct PersistedProject {
    pub name: String,
    pub path: PathBuf,
}

/// Root structure for the persisted state file.
#[derive(Serialize, Deserialize, Default)]
pub struct PersistedState {
    pub projects: Vec<PersistedProject>,
}

/// Returns the path to the configuration file.
pub fn config_path() -> io::Result<PathBuf> {
    if let Some(dir) = std::env::var_os("CHATUI_CONFIG_DIR") {
        return Ok(PathBuf::from(dir).join("term_projects.json"));
    }

    let result = directories::ProjectDirs::from("", "", "chatui")
        .map(|dirs| dirs.config_dir().join("term_projects.json"))
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "config directory unavailable"));

    tracing::info!("config path: {:?}", result);

    result
}

/// Load projects from the configuration file.
pub fn load_projects() -> io::Result<Vec<PersistedProject>> {
    let path = config_path()?;
    if !path.exists() {
        return Ok(Vec::new());
    }

    let data = fs::read_to_string(&path)?;
    let state: PersistedState = serde_json::from_str(&data)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;
    Ok(state.projects)
}

/// Save projects to the configuration file.
pub fn save_projects(projects: &[Project]) -> io::Result<()> {
    let path = config_path()?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let persisted_projects = projects
        .iter()
        .map(|project| PersistedProject {
            name: project.name.clone(),
            path: project.path.clone(),
        })
        .collect();
    let state = PersistedState {
        projects: persisted_projects,
    };
    let data = serde_json::to_string_pretty(&state).map_err(io::Error::other)?;
    fs::write(path, data)?;
    Ok(())
}
