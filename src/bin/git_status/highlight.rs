use std::collections::HashMap;
use std::convert::TryFrom;
use std::path::Path;
use std::sync::Mutex;
use std::time::Duration;

use once_cell::sync::Lazy;
use ropey::Rope;
use tree_house::tree_sitter::Node;
use tree_house::{
    InjectionLanguageMarker, Language as TreeHouseLanguage, LanguageConfig,
    LanguageLoader as TreeHouseLanguageLoader, Syntax, TreeCursor,
    highlighter::{Highlight, HighlightEvent, Highlighter as TreeHouseHighlighter},
};

use chatui::dom::{Color, Style, TextSpan};

use color_eyre::eyre::{Result, WrapErr, eyre};

use super::runtime;

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

const SYNTAX_TIMEOUT_MILLIS: u64 = 100;

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
    map.insert("mm", "objective-cpp");
    map.insert("m", "objective-c");
    map.insert("hmap", "objective-c");
    map.insert("mmmap", "objective-cpp");
    map.insert("jsonml", "json");
    map.insert("cs", "c-sharp");
    map.insert("razor", "c-sharp");
    map.insert("csx", "c-sharp");
    map.insert("fs", "f-sharp");
    map.insert("fsi", "f-sharp");
    map.insert("fsx", "f-sharp");
    map.insert("fsscript", "f-sharp");
    map.insert("odin", "odin");
    map.insert("zig", "zig");
    map.insert("zon", "zig");
    map.insert("hs", "haskell");
    map.insert("lhs", "haskell");
    map.insert("cabal", "cabal");
    map.insert("lua", "lua");
    map.insert("rockspec", "lua");
    map.insert("pl", "perl");
    map.insert("pm", "perl");
    map.insert("rb", "ruby");
    map.insert("erb", "ruby");
    map.insert("gemspec", "ruby");
    map.insert("thor", "ruby");
    map.insert("rake", "ruby");
    map.insert("ru", "ruby");
    map.insert("rspec", "ruby");
    map.insert("purs", "purescript");
    map.insert("ex", "elixir");
    map.insert("exs", "elixir");
    map.insert("leex", "elixir");
    map.insert("eex", "elixir");
    map.insert("heex", "elixir");
    map.insert("ml", "ocaml");
    map.insert("mli", "ocaml-interface");
    map.insert("zig.zon", "zig");
    map.insert("nim", "nim");
    map.insert("nim.cfg", "nim");
    map.insert("nimble", "nim");
    map.insert("clj", "clojure");
    map.insert("cljs", "clojurescript");
    map.insert("cljc", "clojure");
    map.insert("edn", "clojure");
    map.insert("scala", "scala");
    map.insert("sc", "scala");
    map.insert("scm", "scheme");
    map.insert("ss", "scheme");
    map.insert("d", "d");
    map.insert("di", "d");
    map.insert("erl", "erlang");
    map.insert("hrl", "erlang");
    map.insert("hrlx", "erlang");
    map.insert("yrl", "erlang");
    map.insert("gleam", "gleam");
    map.insert("rsx", "rescript");
    map.insert("res", "rescript");
    map.insert("resi", "rescript-interface");
    map.insert("cmake", "cmake");
    map.insert("hpp.in", "cpp");
    map.insert("ipp.in", "cpp");
    map.insert("tpp.in", "cpp");
    map.insert("cc.in", "cpp");
    map.insert("cpp.in", "cpp");
    map.insert("ixx.in", "cpp");
    map.insert("txx.in", "cpp");
    map.insert("cxx.in", "cpp");
    map.insert("c.in", "c");
    map.insert("h.in", "c");
    map.insert("m.in", "objective-c");
    map.insert("mm.in", "objective-cpp");
    map.insert("cmod", "c");
    map.insert("cxxmod", "cpp");
    map.insert("cm", "scheme");
    map.insert("fnl", "fennel");
    map.insert("gle", "gcode");
    map.insert("jl", "julia");
    map.insert("csproj", "xml");
    map.insert("xaml", "xml");
    map.insert("xml", "xml");
    map.insert("plist", "xml");
    map.insert("svg", "xml");
    map.insert("tres", "gdresource");
    map.insert("tscn", "gdscene");
    map.insert("gd", "gdscript");
    map.insert("rpy", "renpy");
    map.insert("coffee", "coffeescript");
    map.insert("litcoffee", "coffeescript");
    map.insert("hx", "haxe");
    map.insert("hxml", "hxml");
    map.insert("dart", "dart");
    map.insert("vb", "visual-basic");
    map.insert("ino", "cpp");
    map.insert("pio", "progress");
    map.insert("ahk", "autohotkey");
    map.insert("nsi", "nsis");
    map.insert("nsh", "nsis");
    map.insert("slang", "slang");
    map.insert("slangh", "slang");
    map.insert("sml", "sml");
    map.insert("sbt", "scala");
    map.insert("hxsl", "hxsl");
    map.insert("s", "asm");
    map.insert("asm", "asm");
    map.insert("S", "asm");
    map.insert("nas", "asm");
    map.insert("ms", "asm");
    map.insert("mot", "asm");
    map.insert("v", "verilog");
    map.insert("sv", "systemverilog");
    map.insert("svh", "systemverilog");
    map.insert("vhd", "vhdl");
    map.insert("vhdl", "vhdl");
    map.insert("bf", "brainfuck");
    map.insert("b", "brainfuck");
    map.insert("lol", "lolcode");
    map.insert("zigmod", "zig");
    map.insert("el", "emacs-lisp");
    map.insert("lsp", "emacs-lisp");
    map.insert("asd", "commonlisp");
    map.insert("lisp", "commonlisp");
    map.insert("cl", "commonlisp");
    map.insert("l", "commonlisp");
    map.insert("jlmod", "julia");
    map.insert("pmod", "purescript");
    map.insert("mtsx", "tsx");
    map.insert("ctsx", "tsx");
    map.insert("mjs", "javascript");
    map.insert("cjs", "javascript");
    map.insert("sqlx", "sql");
    map.insert("pgsql", "sql");
    map.insert("pgtap", "sql");
    map.insert("prql", "prql");
    map.insert("edi", "edi");
    map.insert("proto", "protobuf");
    map.insert("capnp", "capnp");
    map.insert("graphql", "graphql");
    map.insert("gql", "graphql");
    map.insert("ql", "ql");
    map.insert("rego", "rego");
    map.insert("tfvars", "hcl");
    map.insert("hcl", "hcl");
    map.insert("tf", "hcl");
    map.insert("nomad", "hcl");
    map.insert("dockerfile", "dockerfile");
    map.insert("containerfile", "dockerfile");
    map.insert("cue", "cue");
    map.insert("just", "just");
    map.insert("j2", "jinja");
    map.insert("jinja", "jinja");
    map.insert("jinja2", "jinja");
    map.insert("templ", "templ");
    map.insert("typ", "typst");
    map.insert("typst", "typst");
    map.insert("ini", "ini");
    map.insert("cfg", "ini");
    map.insert("conf", "ini");
    map.insert("desktop", "ini");
    map.insert("editorconfig", "ini");
    map.insert("properties", "ini");
    map.insert("prefs", "ini");
    map.insert("service", "ini");
    map.insert("socket", "ini");
    map.insert("target", "ini");
    map.insert("timer", "ini");
    map.insert("path", "ini");
    map.insert("mount", "ini");
    map.insert("automount", "ini");
    map.insert("swap", "ini");
    map.insert("slice", "ini");
    map.insert("link", "ini");
    map.insert("netdev", "ini");
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

static LANGUAGE_NAME_MAP: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    let mut map = HashMap::new();
    for &lang in EXTENSION_LANGUAGE_MAP.values() {
        map.entry(lang).or_insert(lang);
    }
    for &lang in FILE_LANGUAGE_MAP.values() {
        map.entry(lang).or_insert(lang);
    }
    map.insert("python3", "python");
    map.insert("python2", "python");
    map.insert("shell", "bash");
    map.insert("sh", "bash");
    map.insert("zsh", "bash");
    map.insert("node", "javascript");
    map.insert("c++", "cpp");
    map.insert("c", "c");
    map.insert("cpp", "cpp");
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
pub(crate) const EVERFOREST_BG_GREEN_ACCENT: Color = Color::rgb(0x5c, 0x73, 0x64);
pub(crate) const EVERFOREST_BG_RED_ACCENT: Color = Color::rgb(0x79, 0x55, 0x5d);

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

pub fn highlight_with_context(path: &Path, source: &str) -> Option<HighlightResult> {
    let language = language_for_path(path)?;
    REGISTRY.highlight_with_context(language, source).ok()
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn highlight_lines(path: &Path, source: &str) -> Option<HighlightLines> {
    highlight_with_context(path, source).map(|(lines, _)| lines)
}

struct HighlightRegistry {
    loader: TreeHouseLoader,
}

type HighlightLines = Vec<Vec<TextSpan>>;
type LineContexts = Vec<Option<String>>;
type HighlightResult = (HighlightLines, LineContexts);

impl HighlightRegistry {
    fn new() -> Result<Self> {
        Ok(Self {
            loader: TreeHouseLoader::new()?,
        })
    }

    fn highlight_with_context(&self, language_name: &str, source: &str) -> Result<HighlightResult> {
        let language = self
            .loader
            .language_for_name(language_name)
            .wrap_err_with(|| format!("failed to resolve language '{language_name}'"))?;
        let palette = self.loader.palette();

        let rope = Rope::from_str(source);
        let slice = rope.slice(..);
        let timeout = Duration::from_millis(SYNTAX_TIMEOUT_MILLIS);
        let syntax = Syntax::new(slice, language, timeout, &self.loader)
            .map_err(|err| eyre!("failed to parse syntax for '{language_name}': {err}"))?;

        let byte_len = u32::try_from(source.len()).unwrap_or(u32::MAX);
        let mut highlighter = TreeHouseHighlighter::new(&syntax, slice, &self.loader, 0..byte_len);

        let mut lines: Vec<Vec<TextSpan>> = Vec::new();
        let mut current_line: Vec<TextSpan> = Vec::new();
        let mut style_stack: Vec<Style> = Vec::new();
        let mut position = 0usize;

        loop {
            let next_offset = highlighter.next_event_offset();
            if next_offset == u32::MAX {
                break;
            }

            let next = next_offset as usize;
            if next > position && next <= source.len() {
                let fragment = &source[position..next];
                let style = style_stack.last().cloned().unwrap_or_else(Style::default);
                append_fragment(&mut lines, &mut current_line, fragment, &style);
                position = next;
            }

            let (event, highlights) = highlighter.advance();
            match event {
                HighlightEvent::Refresh => {
                    style_stack.clear();
                    for highlight in highlights {
                        style_stack.push(palette.style_for(highlight));
                    }
                }
                HighlightEvent::Push => {
                    for highlight in highlights {
                        style_stack.push(palette.style_for(highlight));
                    }
                }
            }
        }

        if position < source.len() {
            let fragment = &source[position..];
            let style = style_stack.last().cloned().unwrap_or_else(Style::default);
            append_fragment(&mut lines, &mut current_line, fragment, &style);
        }

        if !current_line.is_empty() {
            lines.push(current_line);
        }

        let contexts = build_line_contexts(&syntax, source);

        Ok((lines, contexts))
    }
}

const MAX_CONTEXT_SEGMENTS: usize = 4;

fn build_line_contexts(syntax: &Syntax, source: &str) -> Vec<Option<String>> {
    if source.is_empty() {
        return Vec::new();
    }

    let byte_ranges = line_byte_ranges(source);
    if byte_ranges.is_empty() {
        return Vec::new();
    }

    let mut cursor = syntax.walk();
    let bytes = source.as_bytes();

    byte_ranges
        .into_iter()
        .map(|(start, end)| {
            let inclusive_end = if end > start { end - 1 } else { start };
            cursor.reset_to_byte_range(start, inclusive_end);
            let mut parts = extract_node_path(&mut cursor, bytes);
            if parts.is_empty() {
                return None;
            }
            if parts.len() > MAX_CONTEXT_SEGMENTS {
                parts.drain(..parts.len() - MAX_CONTEXT_SEGMENTS);
            }
            Some(parts.join(" -> "))
        })
        .collect()
}

fn line_byte_ranges(source: &str) -> Vec<(u32, u32)> {
    let mut ranges = Vec::new();
    let mut start = 0usize;
    for (idx, byte) in source.as_bytes().iter().enumerate() {
        if *byte == b'\n' {
            ranges.push((start as u32, idx as u32));
            start = idx + 1;
        }
    }

    if start < source.len() {
        ranges.push((start as u32, source.len() as u32));
    }

    ranges
}

fn extract_node_path(cursor: &mut TreeCursor<'_>, source_bytes: &[u8]) -> Vec<String> {
    let mut parts = Vec::new();

    loop {
        let node = cursor.node();
        if node.parent().is_none() {
            break;
        }

        if node.is_named()
            && let Some(description) = describe_node(&node, source_bytes)
            && parts.last() != Some(&description)
        {
            parts.push(description);
        }

        if !cursor.goto_parent() {
            break;
        }
    }

    parts.reverse();
    parts
}

fn describe_node(node: &Node<'_>, source_bytes: &[u8]) -> Option<String> {
    let kind = node.kind();
    if kind == "ERROR" || kind == "source_file" || kind == "program" {
        return None;
    }

    if let Some(text) = first_field_text(node, &["name", "identifier"], source_bytes) {
        return Some(format_named_kind(kind, &text));
    }

    if kind == "impl_item" {
        let trait_text = first_field_text(node, &["trait"], source_bytes);
        let type_text = first_field_text(node, &["type"], source_bytes);

        return match (trait_text, type_text) {
            (Some(trait_name), Some(type_name)) => {
                Some(format!("impl {trait_name} for {type_name}"))
            }
            (None, Some(type_name)) => Some(format!("impl {type_name}")),
            _ => Some("impl".to_string()),
        };
    }

    if kind == "impl_declaration"
        && let Some(type_name) = first_field_text(node, &["type"], source_bytes)
    {
        return Some(format!("impl {type_name}"));
    }

    if kind.ends_with("_list")
        || kind.ends_with("_block")
        || kind.ends_with("_clause")
        || kind.ends_with("_body")
        || kind.ends_with("_parameters")
        || kind.ends_with("_arguments")
        || kind.ends_with("_statement")
    {
        return None;
    }

    let display = kind.replace('_', " ");
    if display.is_empty() {
        None
    } else {
        Some(display)
    }
}

fn first_field_text(node: &Node<'_>, fields: &[&str], source_bytes: &[u8]) -> Option<String> {
    fields
        .iter()
        .find_map(|field| child_text_by_field(node, field, source_bytes))
}

fn child_text_by_field(node: &Node<'_>, field: &str, source_bytes: &[u8]) -> Option<String> {
    let mut cursor = node.walk();
    if !cursor.goto_first_child() {
        return None;
    }

    loop {
        if cursor.field_name() == Some(field)
            && let Some(text) = extract_node_text(&cursor.node(), source_bytes)
        {
            return Some(text);
        }

        if !cursor.goto_next_sibling() {
            break;
        }
    }

    None
}

fn extract_node_text(node: &Node<'_>, source_bytes: &[u8]) -> Option<String> {
    let range = node.byte_range();
    let start = usize::try_from(range.start).ok()?;
    let end = usize::try_from(range.end).ok()?;
    if start >= end || end > source_bytes.len() {
        return None;
    }

    let raw = std::str::from_utf8(&source_bytes[start..end]).ok()?.trim();
    if raw.is_empty() {
        return None;
    }

    let sanitized = raw
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .trim()
        .to_string();

    if sanitized.is_empty() {
        None
    } else {
        Some(sanitized)
    }
}

fn format_named_kind(kind: &str, name: &str) -> String {
    match kind {
        "function_item"
        | "function_definition"
        | "function_declaration"
        | "method_definition"
        | "method_declaration" => format!("fn {name}"),
        "struct_item" | "struct_definition" | "struct_declaration" => format!("struct {name}"),
        "enum_item" | "enum_definition" | "enum_declaration" => format!("enum {name}"),
        "class_definition" | "class_declaration" | "class_specifier" => format!("class {name}"),
        "interface_declaration" | "interface_definition" => format!("interface {name}"),
        "trait_item" | "trait_declaration" => format!("trait {name}"),
        "impl_item" | "impl_declaration" => format!("impl {name}"),
        "mod_item" | "module" | "module_definition" => format!("mod {name}"),
        "namespace_definition" | "namespace_declaration" => format!("namespace {name}"),
        _ => {
            let display = kind.replace('_', " ");
            format!("{display} {name}")
        }
    }
}

struct HighlightPalette {
    styles: Vec<Style>,
    name_to_highlight: HashMap<&'static str, Highlight>,
}

impl HighlightPalette {
    fn new(names: &[&'static str]) -> Self {
        let mut styles = Vec::with_capacity(names.len());
        let mut name_to_highlight = HashMap::with_capacity(names.len());
        for (idx, name) in names.iter().enumerate() {
            let style = style_for(name);
            styles.push(style);
            let highlight = Highlight::new(idx as u32);
            name_to_highlight.insert(*name, highlight);
        }
        Self {
            styles,
            name_to_highlight,
        }
    }

    fn highlight_for(&self, name: &str) -> Option<Highlight> {
        let mut candidate = name;
        loop {
            if let Some(highlight) = self.name_to_highlight.get(candidate) {
                return Some(*highlight);
            }
            if let Some((parent, _)) = candidate.rsplit_once('.') {
                candidate = parent;
            } else {
                return None;
            }
        }
    }

    fn style_for(&self, highlight: Highlight) -> Style {
        self.styles
            .get(highlight.idx())
            .cloned()
            .unwrap_or_else(Style::default)
    }
}

struct TreeHouseLoader {
    runtime: &'static runtime::TreeSitterRuntime,
    palette: HighlightPalette,
    state: Mutex<LoaderState>,
}

impl TreeHouseLoader {
    fn new() -> Result<Self> {
        let runtime = runtime::runtime()?;
        Ok(Self {
            runtime,
            palette: HighlightPalette::new(HIGHLIGHT_NAMES),
            state: Mutex::new(LoaderState::default()),
        })
    }

    fn palette(&self) -> &HighlightPalette {
        &self.palette
    }

    fn language_for_name(&self, name: &str) -> Result<TreeHouseLanguage> {
        self.language_for_key(name, name)
    }

    fn language_for_marker_name(&self, marker: &str) -> Result<TreeHouseLanguage> {
        self.language_for_key(marker, marker)
    }

    fn language_for_key(&self, lookup: &str, compile_name: &str) -> Result<TreeHouseLanguage> {
        let key = lookup.to_ascii_lowercase();
        {
            let state = self.state.lock().expect("language cache poisoned");
            if let Some(&language) = state.by_name.get(&key) {
                return Ok(language);
            }
        }

        let config = self.compile_language(compile_name)?;

        let mut state = self.state.lock().expect("language cache poisoned");
        if let Some(&language) = state.by_name.get(&key) {
            return Ok(language);
        }

        let idx = u32::try_from(state.entries.len()).unwrap_or(u32::MAX - 1);
        let language = TreeHouseLanguage::new(idx);
        let config_ref = Box::leak(config);
        state.by_name.insert(key, language);
        state.entries.push(LanguageEntry { config: config_ref });
        Ok(language)
    }

    fn compile_language(&self, name: &str) -> Result<Box<LanguageConfig>> {
        let grammar = self
            .runtime
            .load_grammar(name)
            .wrap_err_with(|| format!("failed to load grammar '{name}'"))?;
        let highlights = self
            .runtime
            .load_query(name, "highlights.scm")
            .or_else(|_| self.runtime.load_query_or_empty(name, "highlights.scm"))
            .wrap_err_with(|| format!("failed to load highlights for '{name}'"))?;
        let injections = self
            .runtime
            .load_query_or_empty(name, "injections.scm")
            .wrap_err_with(|| format!("failed to load injections for '{name}'"))?;
        let locals = self
            .runtime
            .load_query_or_empty(name, "locals.scm")
            .wrap_err_with(|| format!("failed to load locals for '{name}'"))?;

        let config = LanguageConfig::new(
            grammar,
            highlights.as_str(),
            injections.as_str(),
            locals.as_str(),
        )
        .wrap_err_with(|| format!("failed to compile language config for '{name}'"))?;

        config.configure(|capture| self.palette.highlight_for(capture));
        Ok(Box::new(config))
    }

    fn language_from_token(&self, token: &str) -> Option<TreeHouseLanguage> {
        canonical_language_from_token(token)
            .and_then(|name| self.language_for_marker_name(name).ok())
    }
}

impl TreeHouseLanguageLoader for TreeHouseLoader {
    fn language_for_marker(&self, marker: InjectionLanguageMarker) -> Option<TreeHouseLanguage> {
        match marker {
            InjectionLanguageMarker::Name(name) => self.language_for_marker_name(name).ok(),
            InjectionLanguageMarker::Match(text) => {
                let owned = text.to_string();
                self.language_from_token(owned.as_str())
            }
            InjectionLanguageMarker::Filename(text) => canonical_language_from_filename(&text)
                .and_then(|name| self.language_for_name(name).ok()),
            InjectionLanguageMarker::Shebang(text) => canonical_language_from_shebang(&text)
                .and_then(|name| self.language_for_name(name).ok()),
        }
    }

    fn get_config(&self, lang: TreeHouseLanguage) -> Option<&LanguageConfig> {
        let state = self.state.lock().expect("language cache poisoned");
        state.entries.get(lang.idx()).map(|entry| entry.config)
    }
}

#[derive(Default)]
struct LoaderState {
    by_name: HashMap<String, TreeHouseLanguage>,
    entries: Vec<LanguageEntry>,
}

struct LanguageEntry {
    config: &'static LanguageConfig,
}

fn append_fragment(
    lines: &mut Vec<Vec<TextSpan>>,
    current_line: &mut Vec<TextSpan>,
    fragment: &str,
    style: &Style,
) {
    if fragment.is_empty() {
        return;
    }

    let mut start = 0;
    for (idx, ch) in fragment.char_indices() {
        if ch == '\n' {
            let segment = &fragment[start..idx];
            push_segment(current_line, segment, style);
            lines.push(std::mem::take(current_line));
            start = idx + ch.len_utf8();
        }
    }

    let remaining = &fragment[start..];
    push_segment(current_line, remaining, style);
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

fn canonical_language_from_token(token: &str) -> Option<&'static str> {
    let trimmed =
        token.trim_matches(|c: char| c.is_ascii_whitespace() || c == '`' || c == '"' || c == '\'');
    if trimmed.is_empty() {
        return None;
    }

    let mut candidates = Vec::new();
    candidates.push(trimmed);
    if let Some(idx) = trimmed.rfind('/') {
        candidates.push(&trimmed[idx + 1..]);
    }
    if let Some(idx) = trimmed.rfind('.') {
        candidates.push(&trimmed[idx + 1..]);
    }

    for candidate in candidates {
        let candidate = candidate.trim();
        if candidate.is_empty() {
            continue;
        }
        let mut key = candidate.to_ascii_lowercase();
        if let Some(lang) = EXTENSION_LANGUAGE_MAP.get(key.as_str()) {
            return Some(*lang);
        }
        if let Some(lang) = FILE_LANGUAGE_MAP.get(key.as_str()) {
            return Some(*lang);
        }
        if let Some(lang) = LANGUAGE_NAME_MAP.get(key.as_str()) {
            return Some(*lang);
        }

        let stripped = key
            .trim_end_matches(|c: char| c.is_ascii_digit())
            .to_string();
        if stripped != key {
            key = stripped;
            if let Some(lang) = EXTENSION_LANGUAGE_MAP.get(key.as_str()) {
                return Some(*lang);
            }
            if let Some(lang) = LANGUAGE_NAME_MAP.get(key.as_str()) {
                return Some(*lang);
            }
        }
    }

    None
}

fn canonical_language_from_filename(filename: &ropey::RopeSlice<'_>) -> Option<&'static str> {
    let owned = filename.to_string();
    language_for_path(Path::new(owned.trim()))
}

fn canonical_language_from_shebang(shebang: &ropey::RopeSlice<'_>) -> Option<&'static str> {
    let text = shebang.to_string();
    let line = text.lines().next().unwrap_or("").trim();
    let line = line.trim_start_matches("#!");
    if line.is_empty() {
        return None;
    }

    let mut parts = line.split_whitespace();
    let first = parts.next()?;
    let command = first.rsplit('/').next().unwrap_or(first);

    if command == "env" {
        for arg in parts {
            if arg.starts_with('-') {
                continue;
            }
            return canonical_language_from_token(arg);
        }
        None
    } else {
        canonical_language_from_token(command)
    }
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
    fn go_highlight_preserves_content() {
        let _ = require_runtime_env();
        let source = "\
package capture

func (c Capturer) executeFinancialFallback(ctx context.Context, input *einvoice_input.Input) (*einvoice_output.Output, error) {
\t// Build a minimal financial component structure identical to the existing fallback path.
\tfinancialComponentStructure := &financial_input.ComponentStructure{}
\tfinancialComponentStructure.Financial = &financial_components.Financial{Enabled: true}
\tfinancialComponentStructure.DateDetails = &financial_components.DateDetails{Enabled: true}
\tfinancialComponentStructure.ReferenceDetails = &financial_components.ReferenceDetails{Enabled: true}
\tfinancialComponentStructure.TaxDetails = &financial_components.TaxDetails{Enabled: true}
\tfinancialComponentStructure.RelationDetails = &financial_components.RelationDetails{Enabled: true}
\tfinancialComponentStructure.RelationAddress = &financial_components.RelationAddress{Enabled: true}
\tfinancialComponentStructure.LineItems = &financial_components.LineItems{Enabled: true}
\tfinancialComponentStructure.AmountDetails = &financial_components.AmountDetails{Enabled: true}
\tfinancialComponentStructure.DocumentLanguage = &financial_components.DocumentLanguage{Enabled: true}
\tfinancialComponentStructure.PaymentDetails = &financial_components.PaymentDetails{Enabled: true}
\tfinancialComponentStructure.DocumentClassification = &financial_components.DocumentClassification{Enabled: true}

\tcomponentArray, err := c.ToComponentArray(financialComponentStructure)
\tif err != nil {
\t\treturn nil, errors.WithStack(err)
\t}

\tfinancialOutput := &financial_output.Output{}
\tif err = celery.ExecuteDocumentCaptureTask(ctx, c, componentArray, financialOutput); err != nil {
\t\treturn nil, errors.WithStack(err)
\t}

\treturn FinancialOutputToUniversalInvoice(financialOutput), nil
}";
        let lines = highlight_lines(Path::new("main.go"), source).expect("expected highlight");
        let reconstructed = lines
            .iter()
            .map(|line| {
                line.iter()
                    .map(|span| span.content.as_str())
                    .collect::<String>()
            })
            .collect::<Vec<String>>()
            .join("\n");
        if reconstructed != source {
            for (idx, (left, right)) in reconstructed.lines().zip(source.lines()).enumerate() {
                if left != right {
                    eprintln!("line {idx} mismatch\nleft : {left}\nright: {right}");
                    if let Some(spans) = lines.get(idx) {
                        for span in spans {
                            eprintln!("  span: {:?} style: {:?}", span.content, span.style);
                        }
                    }
                }
            }
        }
        assert_eq!(reconstructed, source);
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

    #[test]
    fn canonical_language_detection_handles_shebang() {
        let text = ropey::Rope::from_str("#!/usr/bin/env python3");
        let slice = text.slice(..);
        assert_eq!(
            canonical_language_from_shebang(&slice),
            Some("python"),
            "expected python from shebang"
        );
    }

    #[test]
    fn canonical_language_detection_handles_token_variants() {
        assert_eq!(canonical_language_from_token("Rust"), Some("rust"));
        assert_eq!(canonical_language_from_token("python3"), Some("python"));
        assert_eq!(canonical_language_from_token("shell"), Some("bash"));
        assert!(canonical_language_from_token("").is_none());
    }

    #[test]
    fn highlight_lines_applies_keyword_and_function_styles() {
        let _ = require_runtime_env();
        let source = "fn main() { println!(\"hello\"); }";
        let lines = highlight_lines(Path::new("main.rs"), source).expect("expected highlight");
        let spans: Vec<&TextSpan> = lines.iter().flat_map(|line| line.iter()).collect();

        let keyword_style = spans
            .iter()
            .find(|span| span.content.trim() == "fn")
            .map(|span| span.style.clone())
            .expect("expected keyword span");
        assert_eq!(keyword_style.fg, Some(EVERFOREST_RED));

        let function_style = spans
            .iter()
            .find(|span| span.content.contains("main"))
            .map(|span| span.style.clone())
            .expect("expected function span");
        assert_eq!(function_style.fg, Some(EVERFOREST_GREEN));
    }

    #[test]
    fn markdown_code_fences_trigger_language_injections() {
        let _ = require_runtime_env();
        let source = r#"# Demo

```rust
fn greet() {}
```

```python
def greet():
    print("hi")
```

```unknown
just text
```
"#;
        let lines = highlight_lines(Path::new("notes.md"), source).expect("expected highlight");
        let spans: Vec<&TextSpan> = lines.iter().flat_map(|line| line.iter()).collect();

        let rust_span = spans
            .iter()
            .find(|span| span.content.trim() == "fn")
            .map(|span| span.style.clone())
            .expect("expected rust span");
        let python_span = spans
            .iter()
            .find(|span| span.content.trim() == "print")
            .map(|span| span.style.clone())
            .expect("expected python span");

        let unknown_span = spans
            .iter()
            .find(|span| span.content.contains("just text"))
            .map(|span| span.style.clone())
            .expect("expected unknown span");
        assert_eq!(
            unknown_span,
            style_for("markup.raw.block"),
            "unknown code block should use raw markup style"
        );
        assert_ne!(
            rust_span, unknown_span,
            "rust injection should differ from raw markup style"
        );
        assert_ne!(
            python_span, unknown_span,
            "python injection should differ from raw markup style"
        );
    }

    #[test]
    fn loader_reuses_configs_and_supports_markers() {
        use tree_house::LanguageLoader as _;

        let _ = require_runtime_env();
        let loader = TreeHouseLoader::new().expect("loader init");

        let rust = loader.language_for_name("rust").expect("rust config");
        let rust_config_ptr = loader
            .get_config(rust)
            .map(|cfg| cfg as *const _)
            .expect("config");

        let rust_again = loader.language_for_name("rust").expect("rust cache");
        assert_eq!(rust.idx(), rust_again.idx());
        let rust_again_ptr = loader
            .get_config(rust_again)
            .map(|cfg| cfg as *const _)
            .expect("config");
        assert_eq!(rust_config_ptr, rust_again_ptr);

        let python_marker = loader
            .language_for_marker(InjectionLanguageMarker::Name("python"))
            .expect("python marker");
        assert_ne!(python_marker.idx(), rust.idx());

        let filename_rope = ropey::Rope::from_str("script.py");
        let filename_lang = loader
            .language_for_marker(InjectionLanguageMarker::Filename(filename_rope.slice(..)))
            .expect("filename marker");
        assert_eq!(filename_lang.idx(), python_marker.idx());

        let shebang_rope = ropey::Rope::from_str("#!/usr/bin/env bash");
        let shebang_lang = loader
            .language_for_marker(InjectionLanguageMarker::Shebang(shebang_rope.slice(..)))
            .expect("shebang marker");
        assert_ne!(shebang_lang.idx(), rust.idx());

        let nonexistent = loader.compile_language("definitely-not-a-real-language");
        assert!(nonexistent.is_err(), "expected missing language to error");
    }

    #[test]
    fn injection_name_marker_accepts_exact_language_name() {
        use tree_house::LanguageLoader as _;

        let _ = require_runtime_env();
        let loader = TreeHouseLoader::new().expect("loader init");

        let injection_lang = loader
            .language_for_marker(InjectionLanguageMarker::Name("markdown_inline"))
            .expect("expected markdown_inline to resolve");
        assert!(
            loader.get_config(injection_lang).is_some(),
            "expected markdown_inline config to exist"
        );
    }

    #[test]
    fn arduino_extension_still_highlights_via_cpp_grammar() {
        let _ = require_runtime_env();
        let source = "void setup() {}\nvoid loop() {}";
        let result = highlight_lines(Path::new("sketch.ino"), source)
            .expect("expected Arduino sketches to highlight");
        assert!(!result.is_empty(), "highlight result should not be empty");
    }
}
