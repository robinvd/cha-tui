//! Persistence layer for saving and loading project configuration.

use facet::Facet;
use std::path::{Path, PathBuf};
use std::{fs, io};

use super::project::Project;

#[derive(Facet)]
#[facet(transparent)]
struct PathProxy(String);

impl TryFrom<PathProxy> for PathBuf {
    type Error = io::Error;

    fn try_from(proxy: PathProxy) -> Result<Self, Self::Error> {
        Ok(PathBuf::from(proxy.0))
    }
}

impl From<&Path> for PathProxy {
    fn from(path: &Path) -> Self {
        PathProxy(path.to_string_lossy().into_owned())
    }
}

impl From<&PathBuf> for PathProxy {
    fn from(path: &PathBuf) -> Self {
        Self::from(path.as_path())
    }
}

#[derive(Facet)]
pub struct PersistedProject {
    pub name: String,
    #[facet(proxy = PathProxy)]
    pub path: PathBuf,
}

#[derive(Default, Facet)]
pub struct PersistedState {
    pub projects: Vec<PersistedProject>,
}

pub fn config_path_with_root(root: Option<&Path>) -> io::Result<PathBuf> {
    if let Some(dir) = root {
        return Ok(dir.join("term_projects.json"));
    }
    if let Some(dir) = std::env::var_os("CHATUI_CONFIG_DIR").map(PathBuf::from) {
        return Ok(dir.join("term_projects.json"));
    }

    let result = directories::ProjectDirs::from("", "", "chatui")
        .map(|dirs| dirs.config_dir().join("term_projects.json"))
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "config directory unavailable"));

    tracing::info!("config path: {:?}", result);

    result
}

/// Load projects from the configuration file.
pub fn load_projects() -> io::Result<Vec<PersistedProject>> {
    load_projects_from_root(None)
}

pub fn load_projects_from_root(root: Option<&Path>) -> io::Result<Vec<PersistedProject>> {
    let path = config_path_with_root(root)?;
    if !path.exists() {
        return Ok(Vec::new());
    }

    let data = fs::read_to_string(&path)?;
    let state: PersistedState = facet_json::from_str(&data)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;
    Ok(state.projects)
}

/// Save projects to the configuration file.
pub fn save_projects(projects: &[Project]) -> io::Result<()> {
    save_projects_to_root(projects, None)
}

pub fn save_projects_to_root(projects: &[Project], root: Option<&Path>) -> io::Result<()> {
    let path = config_path_with_root(root)?;
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
    let data = facet_json::to_string(&state).map_err(io::Error::other)?;
    fs::write(path, data)?;
    Ok(())
}
