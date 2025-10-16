use std::collections::{HashMap, VecDeque};
use std::path::Path;
use std::sync::{Arc, Mutex};

use once_cell::sync::Lazy;
use tree_sitter_highlight::{HighlightConfiguration, HighlightEvent, Highlighter};

use chatui::dom::{Color, Style, TextSpan};

use color_eyre::eyre::Result;

use super::runtime::{self, LibraryHandle};

const HIGHLIGHT_NAMES: &[&str] = &[
    "attribute",
    "boolean",
    "comment",
    "comment.documentation",
    "constant",
    "constant.builtin",
    "constructor",
    "constructor.builtin",
    "embedded",
    "escape",
    "function",
    "function.builtin",
    "function.macro",
    "keyword",
    "keyword.control",
    "keyword.function",
    "keyword.operator",
    "keyword.storage",
    "label",
    "markup",
    "markup.bold",
    "markup.heading",
    "markup.italic",
    "markup.link",
    "markup.link.url",
    "markup.list",
    "markup.list.checked",
    "markup.list.numbered",
    "markup.list.unchecked",
    "markup.quote",
    "markup.raw",
    "markup.raw.block",
    "markup.raw.inline",
    "markup.strikethrough",
    "module",
    "number",
    "namespace",
    "operator",
    "property",
    "property.builtin",
    "punctuation",
    "punctuation.bracket",
    "punctuation.delimiter",
    "punctuation.special",
    "string",
    "string.escape",
    "string.regexp",
    "string.special",
    "string.special.symbol",
    "special",
    "tag",
    "type",
    "type.builtin",
    "variable",
    "variable.builtin",
    "variable.member",
    "variable.parameter",
];

const LANGUAGE_CACHE_CAPACITY: usize = 10;

static EXTENSION_LANGUAGE_MAP: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("rs", "rust");
    map.insert("md", "markdown");
    map.insert("markdown", "markdown");
    map.insert("mdown", "markdown");
    map.insert("mkd", "markdown");
    map.insert("mkdn", "markdown");
    map.insert("mdwn", "markdown");
    map.insert("mdtxt", "markdown");
    map.insert("mdtext", "markdown");
    map.insert("mdx", "markdown");
    map.insert("livemd", "markdown");
    map.insert("markdn", "markdown");
    map.insert("workbook", "markdown");
    map.insert("py", "python");
    map.insert("pyw", "python");
    map.insert("pyi", "python");
    map.insert("py3", "python");
    map.insert("cpy", "python");
    map.insert("ipy", "python");
    map.insert("ptl", "python");
    map.insert("pyt", "python");
    map.insert("rpy", "python");
    map.insert("go", "go");
    map.insert("js", "javascript");
    map.insert("mjs", "javascript");
    map.insert("cjs", "javascript");
    map.insert("es6", "javascript");
    map.insert("pac", "javascript");
    map.insert("rules", "javascript");
    map.insert("ts", "typescript");
    map.insert("mts", "typescript");
    map.insert("cts", "typescript");
    map.insert("tsx", "tsx");
    map.insert("jsx", "jsx");
    map.insert("json", "json");
    map.insert("jsonl", "json");
    map.insert("geojson", "json");
    map.insert("gltf", "json");
    map.insert("ipynb", "json");
    map.insert("webmanifest", "json");
    map.insert("arb", "json");
    map.insert("avsc", "json");
    map.insert("ldtk", "json");
    map.insert("ldtkl", "json");
    map.insert("sublime-build", "json");
    map.insert("sublime-color-scheme", "json");
    map.insert("sublime-commands", "json");
    map.insert("sublime-completions", "json");
    map.insert("sublime-keymap", "json");
    map.insert("sublime-macro", "json");
    map.insert("sublime-menu", "json");
    map.insert("sublime-mousemap", "json");
    map.insert("sublime-project", "json");
    map.insert("sublime-settings", "json");
    map.insert("sublime-theme", "json");
    map.insert("sublime-workspace", "json");
    map.insert("json5", "json5");
    map.insert("jsonc", "jsonc");
    map.insert("jsonnet", "jsonnet");
    map.insert("libsonnet", "jsonnet");
    map.insert("yml", "yaml");
    map.insert("yaml", "yaml");
    map.insert("sublime-syntax", "yaml");
    map.insert("toml", "toml");
    map.insert("sh", "bash");
    map.insert("bash", "bash");
    map.insert("zsh", "bash");
    map.insert("ksh", "bash");
    map.insert("mksh", "bash");
    map.insert("ash", "bash");
    map.insert("dash", "bash");
    map.insert("zprofile", "bash");
    map.insert("zlogin", "bash");
    map.insert("zlogout", "bash");
    map.insert("zshenv", "bash");
    map.insert("zshrc", "bash");
    map.insert("zshrc_apple_terminal", "bash");
    map.insert("bashrc_apple_terminal", "bash");
    map.insert("zsh-theme", "bash");
    map.insert("ebuild", "bash");
    map.insert("eclass", "bash");
    map.insert("bazelrc", "bash");
    map.insert("cshrc", "bash");
    map.insert("tcshrc", "bash");
    map.insert("renviron", "bash");
    map.insert("sql", "sql");
    map.insert("dsql", "sql");
    map.insert("html", "html");
    map.insert("htm", "html");
    map.insert("xhtml", "html");
    map.insert("xht", "html");
    map.insert("shtml", "html");
    map.insert("jsp", "html");
    map.insert("asp", "html");
    map.insert("aspx", "html");
    map.insert("cshtml", "html");
    map.insert("jshtm", "html");
    map.insert("rhtml", "html");
    map.insert("volt", "html");
    map.insert("css", "css");
    map.insert("scss", "scss");
    map.insert("svelte", "svelte");
    map.insert("vue", "vue");
    map.insert("java", "java");
    map.insert("jav", "java");
    map.insert("pde", "java");
    map.insert("kt", "kotlin");
    map.insert("kts", "kotlin");
    map.insert("swift", "swift");
    map.insert("swiftinterface", "swift");
    map.insert("c", "c");
    map.insert("h", "c");
    map.insert("cc", "cpp");
    map.insert("cpp", "cpp");
    map.insert("cxx", "cpp");
    map.insert("hpp", "cpp");
    map.insert("hh", "cpp");
    map.insert("hxx", "cpp");
    map.insert("h++", "cpp");
    map.insert("ipp", "cpp");
    map.insert("tpp", "cpp");
    map.insert("txx", "cpp");
    map.insert("ixx", "cpp");
    map.insert("ino", "cpp");
    map.insert("cu", "cpp");
    map.insert("cuh", "cpp");
    map.insert("cppm", "cpp");
    map.insert("ii", "cpp");
    map.insert("inl", "cpp");
    map.insert("cs", "c-sharp");
    map.insert("csx", "c-sharp");
    map.insert("cake", "c-sharp");
    map.insert("clj", "clojure");
    map.insert("cljs", "clojure");
    map.insert("cljc", "clojure");
    map.insert("clje", "clojure");
    map.insert("cljr", "clojure");
    map.insert("cljx", "clojure");
    map.insert("boot", "clojure");
    map.insert("edn", "clojure");
    map.insert("cmake", "cmake");
    map.insert("lua", "lua");
    map.insert("rockspec", "lua");
    map.insert("php", "php");
    map.insert("php4", "php");
    map.insert("php5", "php");
    map.insert("phtml", "php");
    map.insert("ctp", "php");
    map.insert("inc", "php");
    map.insert("rb", "ruby");
    map.insert("rake", "ruby");
    map.insert("gemspec", "ruby");
    map.insert("podspec", "ruby");
    map.insert("rabl", "ruby");
    map.insert("rjs", "ruby");
    map.insert("irb", "ruby");
    map.insert("rbi", "ruby");
    map.insert("rbs", "ruby");
    map.insert("jb", "ruby");
    map.insert("jbuilder", "ruby");
    map.insert("scala", "scala");
    map.insert("sc", "scala");
    map.insert("sbt", "scala");
    map.insert("hs", "haskell");
    map.insert("hsc", "haskell");
    map.insert("hs-boot", "haskell");
    map.insert("ex", "elixir");
    map.insert("exs", "elixir");
    map.insert("erl", "erlang");
    map.insert("hrl", "erlang");
    map.insert("app", "erlang");
    map.insert("fs", "fsharp");
    map.insert("fsi", "fsharp");
    map.insert("fsx", "fsharp");
    map.insert("fsscript", "fsharp");
    map.insert("f", "fortran");
    map.insert("f90", "fortran");
    map.insert("f95", "fortran");
    map.insert("f03", "fortran");
    map.insert("for", "fortran");
    map.insert("groovy", "groovy");
    map.insert("gradle", "groovy");
    map.insert("jenkinsfile", "groovy");
    map.insert("dart", "dart");
    map.insert("graphql", "graphql");
    map.insert("gql", "graphql");
    map.insert("graphqls", "graphql");
    map.insert("prisma", "prisma");
    map.insert("properties", "properties");
    map.insert("prefs", "properties");
    map.insert("ini", "ini");
    map.insert("cfg", "ini");
    map.insert("desktop", "ini");
    map.insert("container", "ini");
    map.insert("volume", "ini");
    map.insert("kube", "ini");
    map.insert("network", "ini");
    map.insert("directory", "ini");
    map.insert("nix", "nix");
    map.insert("ml", "ocaml");
    map.insert("mli", "ocaml-interface");
    map.insert("pl", "perl");
    map.insert("pm", "perl");
    map.insert("t", "perl");
    map.insert("psgi", "perl");
    map.insert("raku", "perl");
    map.insert("rakumod", "perl");
    map.insert("rakudoc", "perl");
    map.insert("rakutest", "perl");
    map.insert("nqp", "perl");
    map.insert("p6", "perl");
    map.insert("pl6", "perl");
    map.insert("pm6", "perl");
    map.insert("ps1", "powershell");
    map.insert("psm1", "powershell");
    map.insert("psd1", "powershell");
    map.insert("pscc", "powershell");
    map.insert("psrc", "powershell");
    map.insert("textproto", "textproto");
    map.insert("txtpb", "textproto");
    map.insert("textpb", "textproto");
    map.insert("templ", "templ");
    map.insert("typ", "typst");
    map.insert("typst", "typst");
    map.insert("bzl", "starlark");
    map.insert("bazel", "starlark");
    map.insert("star", "starlark");
    map.insert("make", "make");
    map.insert("mk", "make");
    map.insert("mak", "make");
    map.insert("just", "just");
    map.insert("j2", "jinja");
    map.insert("jinja", "jinja");
    map.insert("jinja2", "jinja");
    map.insert("tex", "latex");
    map.insert("sty", "latex");
    map.insert("cls", "latex");
    map.insert("bbx", "latex");
    map.insert("cbx", "latex");
    map.insert("rd", "latex");
    map.insert("res", "rescript");
    map.insert("r", "r");
    map.insert("rkt", "racket");
    map.insert("rktd", "racket");
    map.insert("rktl", "racket");
    map.insert("scrbl", "racket");
    map.insert("scm", "scheme");
    map.insert("ss", "scheme");
    map.insert("sld", "scheme");
    map.insert("zig", "zig");
    map.insert("zon", "zig");
    map.insert("gitconfig", "git-config");
    map.insert("hcl", "hcl");
    map.insert("tf", "hcl");
    map.insert("nomad", "hcl");
    map.insert("dockerfile", "dockerfile");
    map.insert("containerfile", "dockerfile");
    map
});

static FILE_LANGUAGE_MAP: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("dockerfile", "dockerfile");
    map.insert("containerfile", "dockerfile");
    map.insert("justfile", "just");
    map.insert("makefile", "make");
    map.insert("gnumakefile", "make");
    map.insert("cmakelists.txt", "cmake");
    map.insert("jenkinsfile", "groovy");
    map.insert("build", "starlark");
    map.insert("build.bazel", "starlark");
    map.insert("workspace", "starlark");
    map.insert("workspace.bazel", "starlark");
    map.insert("cargo.lock", "toml");
    map.insert("poetry.lock", "toml");
    map.insert("pdm.lock", "toml");
    map.insert("uv.lock", "toml");
    map.insert("mix.lock", "elixir");
    map.insert("gemfile", "ruby");
    map.insert("gemfile.lock", "ruby");
    map.insert("podfile", "ruby");
    map.insert("vagrantfile", "ruby");
    map
});

static REGISTRY: Lazy<HighlightRegistry> =
    Lazy::new(|| HighlightRegistry::new().expect("failed to initialize highlight registry"));

pub(crate) const EVERFOREST_FG: Color = Color::rgb(0xd3, 0xc6, 0xaa);
pub(crate) const EVERFOREST_RED: Color = Color::rgb(0xe6, 0x7e, 0x80);
pub(crate) const EVERFOREST_ORANGE: Color = Color::rgb(0xe6, 0x98, 0x75);
pub(crate) const EVERFOREST_YELLOW: Color = Color::rgb(0xdb, 0xbc, 0x7f);
pub(crate) const EVERFOREST_GREEN: Color = Color::rgb(0xa7, 0xc0, 0x80);
pub(crate) const EVERFOREST_AQUA: Color = Color::rgb(0x83, 0xc0, 0x92);
pub(crate) const EVERFOREST_BLUE: Color = Color::rgb(0x7f, 0xbb, 0xb3);
pub(crate) const EVERFOREST_PURPLE: Color = Color::rgb(0xd6, 0x99, 0xb6);
pub(crate) const EVERFOREST_GREY1: Color = Color::rgb(0x85, 0x92, 0x89);
pub(crate) const EVERFOREST_GREY2: Color = Color::rgb(0x9d, 0xa9, 0xa0);
pub(crate) const EVERFOREST_BG_GREEN: Color = Color::rgb(0x42, 0x50, 0x47);
pub(crate) const EVERFOREST_BG_RED: Color = Color::rgb(0x51, 0x40, 0x45);

pub fn language_for_path(path: &Path) -> Option<&'static str> {
    if let Some(ext) = path.extension().and_then(|ext| ext.to_str()) {
        let key = ext.to_ascii_lowercase();
        if let Some(language) = EXTENSION_LANGUAGE_MAP.get(key.as_str()) {
            return Some(*language);
        }
    }

    if let Some(file_name) = path.file_name().and_then(|name| name.to_str()) {
        let key = file_name.to_ascii_lowercase();
        if let Some(language) = FILE_LANGUAGE_MAP.get(key.as_str()) {
            return Some(*language);
        }
    }

    None
}

pub fn highlight_lines(path: &Path, source: &str) -> Option<Vec<Vec<TextSpan>>> {
    let language = language_for_path(path)?;
    REGISTRY.highlight(language, source).ok()
}

struct HighlightRegistry {
    styles: Vec<Style>,
    cache: Mutex<LanguageCache>,
}

impl HighlightRegistry {
    fn new() -> Result<Self> {
        Ok(Self {
            styles: HIGHLIGHT_NAMES.iter().map(|name| style_for(name)).collect(),
            cache: Mutex::new(LanguageCache::new(LANGUAGE_CACHE_CAPACITY)),
        })
    }

    fn highlight(&self, language: &str, source: &str) -> Result<Vec<Vec<TextSpan>>> {
        let base_config = self.get_or_load(language)?;
        let mut highlighter = Highlighter::new();
        let mut injection_configs: Vec<Arc<HighlightConfiguration>> = Vec::new();
        let mut injection_refs: Vec<*const HighlightConfiguration> = Vec::new();
        let mut injection_lookup: HashMap<String, usize> = HashMap::new();

        let iterator =
            highlighter.highlight(base_config.as_ref(), source.as_bytes(), None, |name| {
                if let Some(&idx) = injection_lookup.get(name) {
                    // SAFETY: Pointers originate from Arcs stored in `injection_configs`
                    // and remain valid for the duration of the highlight call.
                    return Some(unsafe { &*injection_refs[idx] });
                }
                match self.get_or_load(name) {
                    Ok(config) => {
                        let idx = injection_configs.len();
                        injection_lookup.insert(name.to_owned(), idx);
                        injection_refs.push(Arc::as_ptr(&config));
                        injection_configs.push(config);
                        // SAFETY: Pointer references the newly stored Arc.
                        Some(unsafe { &*injection_refs[idx] })
                    }
                    Err(_) => None,
                }
            })?;
        events_to_lines(iterator, source, &self.styles)
    }

    fn get_or_load(&self, language: &str) -> Result<Arc<HighlightConfiguration>> {
        if let Some(config) = self.try_get_cached(language) {
            return Ok(config);
        }

        let runtime = runtime::runtime()?;
        let highlights = runtime
            .load_query(language, "highlights.scm")
            .or_else(|_| runtime.load_query_or_empty(language, "highlights.scm"))?;
        let injections = runtime.load_query_or_empty(language, "injections.scm")?;
        let locals = runtime.load_query_or_empty(language, "locals.scm")?;
        let (ts_language, library) = runtime.load_language(language)?.into_parts();

        let mut configuration = HighlightConfiguration::new(
            ts_language,
            language,
            highlights.as_str(),
            injections.as_str(),
            locals.as_str(),
        )?;
        configuration.configure(HIGHLIGHT_NAMES);

        let configuration = Arc::new(configuration);

        let mut cache = self.cache.lock().expect("highlight cache poisoned");
        if let Some(config) = cache.get(language) {
            return Ok(config);
        }

        Ok(cache.insert(language.to_string(), configuration, library))
    }

    fn try_get_cached(&self, language: &str) -> Option<Arc<HighlightConfiguration>> {
        let mut cache = self.cache.lock().expect("highlight cache poisoned");
        cache.get(language)
    }
}

struct LanguageCache {
    max_size: usize,
    entries: HashMap<String, CacheEntry>,
    order: VecDeque<String>,
}

impl LanguageCache {
    fn new(max_size: usize) -> Self {
        Self {
            max_size,
            entries: HashMap::new(),
            order: VecDeque::new(),
        }
    }

    fn get(&mut self, name: &str) -> Option<Arc<HighlightConfiguration>> {
        if let Some(entry) = self.entries.get(name) {
            let config = Arc::clone(&entry.config);
            self.touch(name);
            Some(config)
        } else {
            None
        }
    }

    fn insert(
        &mut self,
        name: String,
        config: Arc<HighlightConfiguration>,
        library: LibraryHandle,
    ) -> Arc<HighlightConfiguration> {
        if let Some(existing) = self
            .entries
            .get(&name)
            .map(|entry| Arc::clone(&entry.config))
        {
            self.touch(&name);
            return existing;
        }

        self.entries.insert(
            name.clone(),
            CacheEntry {
                config: Arc::clone(&config),
                _library: library,
            },
        );
        self.touch(&name);
        self.evict_if_needed();
        config
    }

    fn touch(&mut self, name: &str) {
        if let Some(pos) = self.order.iter().position(|existing| existing == name) {
            self.order.remove(pos);
        }
        self.order.push_back(name.to_string());
    }

    fn evict_if_needed(&mut self) {
        while self.entries.len() > self.max_size {
            if let Some(name) = self.order.pop_front() {
                self.entries.remove(&name);
            } else {
                break;
            }
        }
    }
}

struct CacheEntry {
    config: Arc<HighlightConfiguration>,
    _library: LibraryHandle,
}

fn events_to_lines<I>(iter: I, source: &str, styles: &[Style]) -> Result<Vec<Vec<TextSpan>>>
where
    I: Iterator<Item = Result<HighlightEvent, tree_sitter_highlight::Error>>,
{
    let mut lines: Vec<Vec<TextSpan>> = Vec::new();
    let mut current_line: Vec<TextSpan> = Vec::new();
    let mut style_stack: Vec<Style> = Vec::new();

    for event in iter {
        match event? {
            HighlightEvent::HighlightStart(highlight) => {
                let style = styles.get(highlight.0).cloned().unwrap_or_default();
                style_stack.push(style);
            }
            HighlightEvent::HighlightEnd => {
                style_stack.pop();
            }
            HighlightEvent::Source { start, end } => {
                if start == end {
                    continue;
                }
                let fragment = &source[start..end];
                let style = style_stack.last().cloned().unwrap_or_else(Style::default);

                let mut fragment_start = 0;
                for (idx, ch) in fragment.char_indices() {
                    if ch == '\n' {
                        let segment = &fragment[fragment_start..idx];
                        push_segment(&mut current_line, segment, &style);
                        lines.push(std::mem::take(&mut current_line));
                        fragment_start = idx + ch.len_utf8();
                    }
                }

                let remaining = &fragment[fragment_start..];
                push_segment(&mut current_line, remaining, &style);
            }
        }
    }

    if !current_line.is_empty() {
        lines.push(current_line);
    }

    Ok(lines)
}

fn push_segment(line: &mut Vec<TextSpan>, segment: &str, style: &Style) {
    if segment.is_empty() {
        return;
    }
    if let Some(last) = line.last_mut()
        && last.style == *style
    {
        last.content.push_str(segment);
        return;
    }
    line.push(TextSpan::new(segment, style.clone()));
}

fn style_for(name: &str) -> Style {
    fn color_style(color: Color) -> Style {
        Style {
            fg: Some(color),
            ..Style::default()
        }
    }

    fn bold_color_style(color: Color) -> Style {
        Style {
            fg: Some(color),
            bold: true,
            ..Style::default()
        }
    }

    if name.starts_with("comment") {
        return color_style(EVERFOREST_GREY1);
    }

    if name.starts_with("constant.character.escape") || name == "escape" {
        return color_style(EVERFOREST_GREEN);
    }

    if name.starts_with("constant.builtin") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("constant.numeric") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("string.regexp") {
        return color_style(EVERFOREST_GREEN);
    }

    if name.starts_with("string.escape") {
        return color_style(EVERFOREST_GREEN);
    }

    if name.starts_with("string.special") {
        return color_style(EVERFOREST_YELLOW);
    }

    if name.starts_with("string") {
        return color_style(EVERFOREST_AQUA);
    }

    if name.starts_with("variable.builtin") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("variable.other.member") || name.starts_with("variable.member") {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "label" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name == "namespace" || name == "module" {
        return color_style(EVERFOREST_YELLOW);
    }

    if name.starts_with("keyword.operator") || name == "operator" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name.starts_with("keyword.directive") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("keyword") {
        return color_style(EVERFOREST_RED);
    }

    if name.starts_with("function") || name.starts_with("constructor") {
        return color_style(EVERFOREST_GREEN);
    }

    if name == "attribute" {
        return color_style(EVERFOREST_PURPLE);
    }

    if name == "tag" || name == "special" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name.starts_with("type") {
        return color_style(EVERFOREST_YELLOW);
    }

    if name.starts_with("boolean") || name.starts_with("number") {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("property") {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "punctuation.special" {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "punctuation.delimiter" {
        return color_style(EVERFOREST_GREY1);
    }

    if name == "punctuation.bracket" {
        return color_style(EVERFOREST_FG);
    }

    if name.starts_with("punctuation") {
        return color_style(EVERFOREST_GREY2);
    }

    if name.starts_with("markup.heading.") {
        let level = name.rsplit('.').next().unwrap_or("");
        return match level {
            "1" => bold_color_style(EVERFOREST_RED),
            "2" => bold_color_style(EVERFOREST_ORANGE),
            "3" => bold_color_style(EVERFOREST_YELLOW),
            "4" => bold_color_style(EVERFOREST_GREEN),
            "5" => bold_color_style(EVERFOREST_BLUE),
            "6" => bold_color_style(EVERFOREST_PURPLE),
            _ => Style {
                bold: true,
                ..Style::default()
            },
        };
    }

    if name == "markup.heading" {
        return bold_color_style(EVERFOREST_YELLOW);
    }

    if name == "markup.bold" {
        return Style {
            bold: true,
            ..Style::default()
        };
    }

    if name == "markup.list" || name.starts_with("markup.list.") {
        return color_style(EVERFOREST_RED);
    }

    if name == "markup.link.url" {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "markup.link.label" {
        return color_style(EVERFOREST_ORANGE);
    }

    if name == "markup.link.text" {
        return color_style(EVERFOREST_PURPLE);
    }

    if name.starts_with("markup.link") {
        return color_style(EVERFOREST_BLUE);
    }

    if name == "markup.quote" {
        return color_style(EVERFOREST_GREY1);
    }

    if name == "markup.raw.inline" {
        return color_style(EVERFOREST_GREEN);
    }

    if name == "markup.raw.block" {
        return color_style(EVERFOREST_AQUA);
    }

    if name.starts_with("markup") {
        return Style::default();
    }

    if name == "embedded" {
        return color_style(EVERFOREST_FG);
    }

    Style::default()
}

#[cfg(test)]
mod tests {
    use super::runtime::HELIX_RUNTIME_ENV;
    use super::*;
    use std::env;

    fn require_runtime_env() -> String {
        env::var(HELIX_RUNTIME_ENV).unwrap_or_else(|_| {
            panic!(
                "{} must be set for syntax highlight tests",
                HELIX_RUNTIME_ENV
            )
        })
    }

    #[test]
    fn rust_highlight_preserves_content() {
        let _ = require_runtime_env();
        let source = "fn main() { println!(\"hello\"); }";
        let lines = highlight_lines(Path::new("main.rs"), source).expect("expected highlight");
        assert_eq!(lines.len(), 1);
        let rendered: String = lines[0].iter().map(|span| span.content.as_str()).collect();
        assert_eq!(rendered, source);
    }

    #[test]
    fn markdown_highlight_preserves_content() {
        let _ = require_runtime_env();
        let source = "# Heading\n\nSome **bold** text.";
        let lines = highlight_lines(Path::new("README.md"), source).expect("expected highlight");
        assert_eq!(lines.len(), 3);
        let reconstructed: Vec<String> = lines
            .iter()
            .map(|line| {
                line.iter()
                    .map(|span| span.content.as_str())
                    .collect::<String>()
            })
            .collect();
        assert_eq!(reconstructed.join("\n"), source);
    }
}
