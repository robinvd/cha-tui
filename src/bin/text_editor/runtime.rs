use std::env;
use std::fs;
use std::io;
use std::path::PathBuf;

use color_eyre::eyre::{Context, Result, eyre};
use once_cell::sync::OnceCell;
use tree_house::tree_sitter::Grammar as TreeHouseGrammar;

const GRAMMARS_DIR: &str = "grammars";
const QUERIES_DIR: &str = "queries";

pub const HELIX_RUNTIME_ENV: &str = "HELIX_RUNTIME";

static RUNTIME: OnceCell<TreeSitterRuntime> = OnceCell::new();

pub fn runtime() -> Result<&'static TreeSitterRuntime> {
    RUNTIME.get_or_try_init(TreeSitterRuntime::new)
}

pub struct TreeSitterRuntime {
    root: PathBuf,
}

impl TreeSitterRuntime {
    fn new() -> Result<Self> {
        let root = env::var(HELIX_RUNTIME_ENV)
            .wrap_err_with(|| format!("{} environment variable is not set", HELIX_RUNTIME_ENV))
            .map(PathBuf::from)?;

        if !root.is_dir() {
            return Err(eyre!(
                "Helix runtime directory '{}' does not exist or is not a directory",
                root.display()
            ));
        }

        Ok(Self { root })
    }

    pub fn load_grammar(&self, name: &str) -> Result<TreeHouseGrammar> {
        let path = self.grammar_path(name);
        unsafe {
            TreeHouseGrammar::new(name, &path)
                .wrap_err_with(|| format!("failed to load grammar '{}'", path.display()))
        }
    }

    pub fn load_query(&self, language: &str, filename: &str) -> Result<String> {
        let path = self.query_path(language, filename);
        fs::read_to_string(&path)
            .wrap_err_with(|| format!("failed to read query '{}'", path.display()))
    }

    pub fn load_query_or_empty(&self, language: &str, filename: &str) -> Result<String> {
        let path = self.query_path(language, filename);
        match fs::read_to_string(&path) {
            Ok(contents) => Ok(contents),
            Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(String::new()),
            Err(error) => {
                Err(error).wrap_err_with(|| format!("failed to read query '{}'", path.display()))
            }
        }
    }

    fn grammar_path(&self, name: &str) -> PathBuf {
        self.root.join(GRAMMARS_DIR).join(format!("{name}.so"))
    }

    fn query_path(&self, language: &str, filename: &str) -> PathBuf {
        self.root.join(QUERIES_DIR).join(language).join(filename)
    }
}
