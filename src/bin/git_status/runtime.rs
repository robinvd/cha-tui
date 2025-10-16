use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::mem;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use color_eyre::eyre::{Context, Result, eyre};
use libloading::Library;
use once_cell::sync::OnceCell;
use tree_sitter::{Language, ffi};
use tree_sitter_language::LanguageFn;

const GRAMMARS_DIR: &str = "grammars";
const QUERIES_DIR: &str = "queries";

type LanguageSymbol = unsafe extern "C" fn() -> *const ffi::TSLanguage;

pub const HELIX_RUNTIME_ENV: &str = "HELIX_RUNTIME";

static RUNTIME: OnceCell<TreeSitterRuntime> = OnceCell::new();

pub type LibraryHandle = Arc<Library>;

pub fn runtime() -> Result<&'static TreeSitterRuntime> {
    RUNTIME.get_or_try_init(TreeSitterRuntime::new)
}

pub struct TreeSitterRuntime {
    root: PathBuf,
    libraries: Mutex<HashMap<String, LibraryHandle>>,
}

pub struct LoadedLanguage {
    language: Language,
    library: LibraryHandle,
}

impl LoadedLanguage {
    pub fn into_parts(self) -> (Language, LibraryHandle) {
        (self.language, self.library)
    }
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

        Ok(Self {
            root,
            libraries: Mutex::new(HashMap::new()),
        })
    }

    pub fn load_language(&self, name: &str) -> Result<LoadedLanguage> {
        let library = {
            let mut libraries = self
                .libraries
                .lock()
                .expect("TreeSitterRuntime library cache poisoned");

            if let Some(existing) = libraries.get(name) {
                Arc::clone(existing)
            } else {
                let path = self.grammar_path(name);
                let library = unsafe {
                    Library::new(&path)
                        .wrap_err_with(|| format!("failed to load grammar '{}'", path.display()))?
                };
                let library = Arc::new(library);
                libraries.insert(name.to_string(), Arc::clone(&library));
                library
            }
        };

        unsafe {
            let symbol_name = format!("tree_sitter_{}", name.replace('-', "_"));
            let symbol: libloading::Symbol<LanguageSymbol> =
                library.get(symbol_name.as_bytes()).wrap_err_with(|| {
                    format!(
                        "failed to load symbol '{}' from grammar '{}'",
                        symbol_name, name
                    )
                })?;

            let func = *symbol;
            let raw_fn =
                mem::transmute::<LanguageSymbol, unsafe extern "C" fn() -> *const ()>(func);
            let lang_fn = LanguageFn::from_raw(raw_fn);
            let language = Language::new(lang_fn);
            Ok(LoadedLanguage { language, library })
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
