mod diff_render;
mod git;
mod highlight;
mod runtime;

use std::collections::btree_map::Entry;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufReader, ErrorKind, Read};
use std::ops::Range;
use std::path::Path;
use std::result::Result as StdResult;

use chatui::components::scroll::{
    ScrollAxis, ScrollMsg, ScrollState, ScrollTarget, scrollable_content,
};
use chatui::dom::{Color, Node, TextSpan, renderable};
use chatui::event::{Event, Key, KeyCode};
use chatui::program::TaskFn;
use chatui::{
    InputMsg, InputState, InputStyle, Program, Style, Transition, TreeNodeKind, block_with_title,
    column, default_input_keybindings, input, modal, rich_text, row, text,
};
use chatui::{TreeMsg, TreeNode, TreeState, TreeStyle, tree_view};
use facet::Facet;
use facet_args as args;
use miette::{Context, IntoDiagnostic, Result, miette};
use similar::{Algorithm, ChangeTag, DiffOp, DiffTag, InlineChange, TextDiff};
use taffy::Dimension;
use taffy::prelude::{FromLength, TaffyZero};
use taffy::style::FlexWrap;
use tracing::info;
use tracing_subscriber::EnvFilter;

use crate::git::{FileEntry, GitStatus, LoadedContent};
use fuztea::{FuzzyFinder, FuzzyFinderEvent, FuzzyFinderMsg};
use smol::unblock;
mod shortcuts;

#[derive(Facet)]
struct Args {
    #[facet(args::subcommand)]
    command: Option<Command>,
}

#[derive(Facet)]
#[repr(u8)]
enum Command {
    /// Show changes from a commit (like git show)
    Show {
        /// Commit to show (default: HEAD)
        #[facet(args::positional, default = default_commit())]
        commit: String,
    },
    /// Show diff between commits (like git diff)
    Diff {
        /// Diff spec (commit, range, HEAD~N, etc.)
        #[facet(args::positional)]
        spec: String,
    },
}

fn default_commit() -> String {
    "HEAD".to_string()
}

enum ViewMode {
    WorkingTree(Box<WorkingTreeState>),
    Diff(Box<DiffModeState>),
}

impl Default for ViewMode {
    fn default() -> Self {
        ViewMode::WorkingTree(Box::default())
    }
}

impl ViewMode {
    fn is_diff(&self) -> bool {
        matches!(self, ViewMode::Diff(_))
    }

    fn spec(&self) -> Option<&str> {
        match self {
            ViewMode::Diff(state) => Some(&state.spec),
            ViewMode::WorkingTree(_) => None,
        }
    }

    fn working_tree(&self) -> Option<&WorkingTreeState> {
        match self {
            ViewMode::WorkingTree(state) => Some(state),
            ViewMode::Diff(_) => None,
        }
    }

    fn working_tree_mut(&mut self) -> Option<&mut WorkingTreeState> {
        match self {
            ViewMode::WorkingTree(state) => Some(state),
            ViewMode::Diff(_) => None,
        }
    }

    fn diff_state_mut(&mut self) -> Option<&mut DiffModeState> {
        match self {
            ViewMode::Diff(state) => Some(state),
            ViewMode::WorkingTree(_) => None,
        }
    }
}

struct WorkingTreeState {
    unstaged: Vec<FileEntry>,
    staged: Vec<FileEntry>,
    unstaged_tree: TreeState<FileNodeId>,
    staged_tree: TreeState<FileNodeId>,
    unstaged_initialized: bool,
    staged_initialized: bool,
    unstaged_scroll: ScrollState,
    staged_scroll: ScrollState,
    commit_modal: Option<CommitModal>,
    delete_modal: Option<DeleteModal>,
    current_branch: Option<String>,
    fuzzy_finder: Option<FuzzyFinderState>,
}

impl Default for WorkingTreeState {
    fn default() -> Self {
        Self {
            unstaged: Vec::new(),
            staged: Vec::new(),
            unstaged_tree: TreeState::default(),
            staged_tree: TreeState::default(),
            unstaged_initialized: false,
            staged_initialized: false,
            unstaged_scroll: ScrollState::default(),
            staged_scroll: ScrollState::default(),
            commit_modal: None,
            delete_modal: None,
            current_branch: None,
            fuzzy_finder: None,
        }
    }
}

struct DiffModeState {
    spec: String,
    changed: Vec<FileEntry>,
    changed_tree: TreeState<FileNodeId>,
    changed_initialized: bool,
    changed_scroll: ScrollState,
    fuzzy_finder: Option<FuzzyFinderState>,
}

impl Default for DiffModeState {
    fn default() -> Self {
        Self {
            spec: String::new(),
            changed: Vec::new(),
            changed_tree: TreeState::default(),
            changed_initialized: false,
            changed_scroll: ScrollState::default(),
            fuzzy_finder: None,
        }
    }
}

fn main() -> Result<()> {
    miette::set_panic_hook();

    let args: Args = facet_args::from_std_args()?;
    let view_mode = match args.command {
        Some(Command::Show { commit }) => ViewMode::Diff(Box::new(DiffModeState {
            spec: commit,
            ..Default::default()
        })),
        Some(Command::Diff { spec }) => ViewMode::Diff(Box::new(DiffModeState {
            spec,
            ..Default::default()
        })),
        None => ViewMode::WorkingTree(Box::default()),
    };

    init_tracing()?;

    let mut model = Model::new(view_mode);
    let mut program = Program::new(&mut model, update, view).map_event(map_event);

    if let Err(error) = smol::block_on(program.run_async()) {
        eprintln!("Program exited with error: {:?}", error);
    }

    Ok(())
}

fn map_event(event: Event) -> Option<Msg> {
    match event {
        Event::Key(key) => Some(Msg::Global(GlobalMsg::KeyPressed(key))),
        Event::FocusGained => Some(Msg::Global(GlobalMsg::RefreshStatus)),
        _ => None,
    }
}

fn update(model: &mut Model, msg: Msg) -> Transition<Msg> {
    info!("update {msg:?}");
    match msg {
        Msg::Global(global_msg) => handle_global_msg(model, global_msg),
        Msg::Navigation(nav_msg) => handle_navigation_msg(model, nav_msg),
        Msg::Diff(diff_msg) => handle_diff_msg(model, diff_msg),
        Msg::Commit(commit_msg) => handle_commit_msg(model, commit_msg),
        Msg::Delete(delete_msg) => handle_delete_msg(model, delete_msg),
        Msg::Fuzzy(fuzzy_msg) => handle_fuzzy_msg(model, fuzzy_msg),
        Msg::Section { focus, msg } => handle_section_msg(model, focus, msg),
        Msg::DiffLoaded { request_id, result } => model.handle_diff_loaded(request_id, result),
        Msg::StatusLoaded {
            preserve_diff_scroll,
            result,
        } => model.handle_status_loaded(preserve_diff_scroll, result),
        Msg::Scroll { focus, msg } => {
            model.update_section_scroll(focus, msg);
            Transition::Continue
        }
    }
}

fn handle_section_msg(model: &mut Model, focus: Focus, msg: SectionMsg) -> Transition<Msg> {
    match msg {
        SectionMsg::Tree(tree_msg) => model.handle_tree_message(focus, tree_msg),
    }
}

fn handle_global_msg(model: &mut Model, msg: GlobalMsg) -> Transition<Msg> {
    match msg {
        GlobalMsg::KeyPressed(key) => handle_key(model, key),
        GlobalMsg::Quit => Transition::Quit,
        GlobalMsg::RefreshStatus => model.start_status_refresh(true),
        GlobalMsg::ToggleShortcutsHelp => {
            model.toggle_shortcuts_help();
            Transition::Continue
        }
    }
}

fn handle_navigation_msg(model: &mut Model, msg: NavigationMsg) -> Transition<Msg> {
    match msg {
        NavigationMsg::ToggleStage => {
            model.toggle_stage_selected();
            Transition::Continue
        }
        NavigationMsg::ToggleFocus => {
            match model.focus {
                Focus::Unstaged => model.focus_staged(),
                Focus::Staged => model.focus_unstaged(),
                Focus::Changed => {}
            }
            match model.start_diff_refresh(false) {
                Some(task) => Transition::Task(task),
                None => Transition::Continue,
            }
        }
        NavigationMsg::MoveSelectionUp => {
            model.move_selection_up();
            match model.start_diff_refresh(false) {
                Some(task) => Transition::Task(task),
                None => Transition::Continue,
            }
        }
        NavigationMsg::MoveSelectionDown => {
            model.move_selection_down();
            match model.start_diff_refresh(false) {
                Some(task) => Transition::Task(task),
                None => Transition::Continue,
            }
        }
        NavigationMsg::CollapseNode => {
            if model.collapse_selected()
                && let Some(task) = model.start_diff_refresh(false)
            {
                return Transition::Task(task);
            }
            Transition::Continue
        }
        NavigationMsg::ExpandNode => {
            if model.expand_selected()
                && let Some(task) = model.start_diff_refresh(false)
            {
                return Transition::Task(task);
            }
            Transition::Continue
        }
        NavigationMsg::ScrollFiles(delta) => {
            model.scroll_files_in_focus(delta);
            Transition::Continue
        }
    }
}

fn handle_diff_msg(model: &mut Model, msg: DiffMsg) -> Transition<Msg> {
    match msg {
        DiffMsg::ScrollVertical(delta) => {
            model.scroll_diff(delta);
            Transition::Continue
        }
        DiffMsg::ScrollHorizontal(delta) => {
            model.scroll_diff_horizontal(delta);
            Transition::Continue
        }
        DiffMsg::Scroll(scroll_msg) => {
            model.diff_scroll.update(scroll_msg);
            Transition::Continue
        }
        DiffMsg::ToggleLineNumbers => {
            model.toggle_diff_line_numbers();
            Transition::Continue
        }
    }
}

fn handle_commit_msg(model: &mut Model, msg: CommitMsg) -> Transition<Msg> {
    match msg {
        CommitMsg::Open => {
            model.open_commit_modal();
            Transition::Continue
        }
        CommitMsg::Submit => model.submit_commit(),
        CommitMsg::Cancel => {
            model.close_commit_modal();
            Transition::Continue
        }
        CommitMsg::Input(input_msg) => {
            if let Some(modal) = model.commit_modal_mut() {
                modal.update(input_msg);
            }
            Transition::Continue
        }
    }
}

fn handle_delete_msg(model: &mut Model, msg: DeleteMsg) -> Transition<Msg> {
    match msg {
        DeleteMsg::Open => {
            model.open_delete_modal();
            Transition::Continue
        }
        DeleteMsg::Confirm => model.confirm_delete(),
        DeleteMsg::Cancel => {
            model.close_delete_modal();
            Transition::Continue
        }
    }
}

fn handle_fuzzy_msg(model: &mut Model, msg: FuzzyMsg) -> Transition<Msg> {
    match msg {
        FuzzyMsg::Open => {
            model.open_fuzzy_finder();
            Transition::Continue
        }
        FuzzyMsg::Inner(inner_msg) => model.handle_fuzzy_inner(inner_msg),
        FuzzyMsg::Accept => model.accept_fuzzy_finder(),
        FuzzyMsg::Cancel => model.cancel_fuzzy_finder(),
    }
}

fn handle_key(model: &mut Model, key: Key) -> Transition<Msg> {
    // If the fuzzy finder is active, it consumes all keys.
    if model.fuzzy_finder().is_some() {
        return handle_fuzzy_key(model, key);
    }

    // If the delete modal is active, it consumes keys like Enter/Esc/Y/N.
    if model.delete_modal().is_some() {
        return handle_delete_modal_key(model, key);
    }

    // Global/context shortcuts (including commit modal actions) take precedence
    // over raw character input handling inside modals.
    if let Some(msg) = shortcuts::message_for_key(model, key) {
        return update(model, msg);
    }

    // When commit modal is open, feed non-modified character input to it.
    if model.commit_modal().is_some() {
        return handle_commit_modal_key(model, key);
    }

    Transition::Continue
}

fn handle_commit_modal_key(model: &mut Model, key: Key) -> Transition<Msg> {
    if let Some(msg) = model.commit_modal().and_then(|modal| {
        default_input_keybindings(&modal.input, key, |input_msg| {
            Msg::Commit(CommitMsg::Input(input_msg))
        })
    }) {
        return update(model, msg);
    }

    Transition::Continue
}

fn handle_delete_modal_key(model: &mut Model, key: Key) -> Transition<Msg> {
    match key.code {
        KeyCode::Enter => update(model, Msg::Delete(DeleteMsg::Confirm)),
        KeyCode::Esc => update(model, Msg::Delete(DeleteMsg::Cancel)),
        KeyCode::Char('y') | KeyCode::Char('Y') => update(model, Msg::Delete(DeleteMsg::Confirm)),
        KeyCode::Char('n') | KeyCode::Char('N') => update(model, Msg::Delete(DeleteMsg::Cancel)),
        _ => Transition::Continue,
    }
}

fn handle_fuzzy_key(model: &mut Model, key: Key) -> Transition<Msg> {
    let inner_msg = FuzzyFinderMsg::KeyPressed(key);
    update(model, Msg::Fuzzy(FuzzyMsg::Inner(inner_msg)))
}

fn view(model: &Model) -> Node<Msg> {
    let layout = row(vec![
        render_left_pane(model)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO)
            .with_min_width(Dimension::from_length(20.))
            .with_min_height(Dimension::ZERO),
        render_diff_pane(model)
            .with_flex_grow(3.)
            .with_flex_basis(Dimension::ZERO)
            .with_min_height(Dimension::ZERO)
            .with_min_width(Dimension::ZERO),
    ])
    .with_width(Dimension::percent(1.))
    .with_flex_basis(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_shrink(1.)
    .with_min_height(Dimension::ZERO)
    .with_id("main-row");

    let title = if model.view_mode.is_diff() {
        "Git Diff"
    } else {
        "Git Status"
    };
    let mut header_spans = vec![TextSpan::new(title, title_style())];
    if let Some(error) = model.error.as_deref()
        && !error.is_empty()
    {
        header_spans.push(TextSpan::new(" - ", Style::default()));
        header_spans.push(TextSpan::new(error, error_style()));
    }

    let header_left = rich_text::<Msg>(header_spans)
        .with_flex_grow(1.)
        .with_flex_shrink(1.)
        .with_flex_basis(Dimension::ZERO)
        .with_min_width(Dimension::ZERO)
        .with_id("header-title");

    let header_right = if let Some(spec) = model.view_mode.spec() {
        let mut spans = vec![
            TextSpan::new("diff: ", Style::dim()),
            TextSpan::new(spec, branch_name_style()),
        ];
        if model.status_loading {
            spans.push(TextSpan::new("  refreshing…", Style::dim()));
        }
        rich_text::<Msg>(spans).with_id("header-diff-spec")
    } else {
        let branch_label = model
            .current_branch()
            .filter(|branch| !branch.is_empty())
            .unwrap_or("(unknown)");
        let mut branch_spans = vec![
            TextSpan::new("branch: ", Style::dim()),
            TextSpan::new(branch_label, branch_name_style()),
        ];
        if model.status_loading {
            branch_spans.push(TextSpan::new("  refreshing…", Style::dim()));
        }
        rich_text::<Msg>(branch_spans).with_id("header-branch")
    };

    let header = row(vec![header_left, header_right])
        .with_width(Dimension::percent(1.))
        .with_id("header");

    let base = column(vec![header, layout, render_shortcuts_bar(model)])
        .with_fill()
        .with_id("root");

    // Stack any active modal dialogs on top of the base UI.
    let mut layered = base;

    if let Some(wt) = model.view_mode.working_tree() {
        if let Some(modal) = &wt.commit_modal {
            layered = column(vec![layered, render_commit_modal(modal)])
                .with_fill()
                .with_id("root-with-commit-modal");
        }
        if let Some(modal) = &wt.delete_modal {
            layered = column(vec![layered, render_delete_modal(modal)])
                .with_fill()
                .with_id("root-with-delete-modal");
        }
    }

    layered
}

fn render_left_pane(model: &Model) -> Node<Msg> {
    let fuzzy_state = model.fuzzy_finder();
    let fuzzy_focus = fuzzy_state.map(|s| s.original_focus);
    match &model.view_mode {
        ViewMode::Diff(diff) => render_changed_pane(diff, fuzzy_state),
        ViewMode::WorkingTree(wt) => render_working_tree_pane(wt, model.focus, fuzzy_focus),
    }
}

fn render_working_tree_pane(
    wt: &WorkingTreeState,
    focus: Focus,
    fuzzy_focus: Option<Focus>,
) -> Node<Msg> {
    let unstaged_content = if fuzzy_focus == Some(Focus::Unstaged) {
        render_fuzzy_finder_inline(wt.fuzzy_finder.as_ref().unwrap())
    } else {
        render_file_tree(
            Model::UNSTAGED_ITEM_ID,
            &wt.unstaged_tree,
            focus == Focus::Unstaged,
            |tree_msg| Msg::Section {
                focus: Focus::Unstaged,
                msg: SectionMsg::Tree(tree_msg),
            },
        )
    }
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO);

    let unstaged_title = if fuzzy_focus == Some(Focus::Unstaged) {
        "Find in Unstaged"
    } else {
        "Unstaged Changes"
    };
    let unstaged = block_with_title(
        unstaged_title,
        vec![
            scrollable_content(
                "unstaged-section-content",
                &wt.unstaged_scroll,
                1,
                |scroll_msg| Msg::Scroll {
                    focus: Focus::Unstaged,
                    msg: scroll_msg,
                },
                unstaged_content,
            )
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO),
        ],
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO)
    .with_id("unstaged-section");

    let staged_content = if fuzzy_focus == Some(Focus::Staged) {
        render_fuzzy_finder_inline(wt.fuzzy_finder.as_ref().unwrap())
    } else {
        render_file_tree(
            Model::STAGED_ITEM_ID,
            &wt.staged_tree,
            focus == Focus::Staged,
            |tree_msg| Msg::Section {
                focus: Focus::Staged,
                msg: SectionMsg::Tree(tree_msg),
            },
        )
    }
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO);

    let staged_title = if fuzzy_focus == Some(Focus::Staged) {
        "Find in Staged"
    } else {
        "Staged Changes"
    };
    let staged = block_with_title(
        staged_title,
        vec![
            scrollable_content(
                "staged-section-content",
                &wt.staged_scroll,
                1,
                |scroll_msg| Msg::Scroll {
                    focus: Focus::Staged,
                    msg: scroll_msg,
                },
                staged_content,
            )
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO),
        ],
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO)
    .with_id("staged-section");

    column(vec![
        unstaged.with_flex_grow(1.).with_flex_basis(Dimension::ZERO),
        staged.with_flex_grow(1.).with_flex_basis(Dimension::ZERO),
    ])
    .with_id("left-pane")
}

fn render_changed_pane(diff: &DiffModeState, fuzzy_state: Option<&FuzzyFinderState>) -> Node<Msg> {
    let is_fuzzy_active = fuzzy_state.is_some();
    let changed_content = if is_fuzzy_active {
        render_fuzzy_finder_inline(fuzzy_state.unwrap())
    } else {
        render_file_tree(
            Model::CHANGED_ITEM_ID,
            &diff.changed_tree,
            true,
            |tree_msg| Msg::Section {
                focus: Focus::Changed,
                msg: SectionMsg::Tree(tree_msg),
            },
        )
    }
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO);

    let title = if is_fuzzy_active {
        "Find in Changed"
    } else {
        "Changed"
    };
    let changed = block_with_title(
        title,
        vec![
            scrollable_content(
                Model::CHANGED_CONTAINER_ID,
                &diff.changed_scroll,
                1,
                |scroll_msg| Msg::Scroll {
                    focus: Focus::Changed,
                    msg: scroll_msg,
                },
                changed_content,
            )
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO),
        ],
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO)
    .with_id("changed-section");

    column(vec![
        changed.with_flex_grow(1.).with_flex_basis(Dimension::ZERO),
    ])
    .with_id("left-pane")
}

fn render_file_tree(
    id_prefix: &'static str,
    tree: &TreeState<FileNodeId>,
    is_active: bool,
    map_msg: impl Fn(TreeMsg<FileNodeId>) -> Msg + 'static,
) -> Node<Msg> {
    if tree.visible().is_empty() {
        return column(vec![text::<Msg>("(no files)").with_style(inactive_style())])
            .with_min_height(Dimension::ZERO)
            .with_flex_grow(1.)
            .with_flex_basis(Dimension::ZERO);
    }

    let mut style = file_tree_style();
    // Ensure selection colors match the legacy list styling.
    style.active_selected_row = active_selection_style();
    style.inactive_selected_row = inactive_selection_style();

    tree_view(id_prefix, tree, &style, map_msg, is_active)
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO)
}

fn file_tree_style() -> TreeStyle {
    TreeStyle {
        indent: 2,
        inactive_row: Style::default(),
        active_row: Style::default(),
        inactive_selected_row: inactive_selection_style(),
        active_selected_row: active_selection_style(),
        branch_collapsed: String::from(">"),
        branch_expanded: String::from("v"),
        leaf_bullet: String::from(" "),
    }
}

fn render_diff_pane(model: &Model) -> Node<Msg> {
    let mut diff_title = match model.current_entry() {
        Some(entry) => {
            if model.diff_truncated {
                format!(
                    "Diff Preview - {} (file too large; showing first 1MB without highlighting)",
                    entry.path
                )
            } else {
                format!("Diff Preview - {}", entry.path)
            }
        }
        None => {
            if let Some(selected) = model.selected_id(model.focus) {
                format!("Diff Preview - {}", selected.path())
            } else {
                "Diff Preview".to_string()
            }
        }
    };
    if model.diff_loading {
        diff_title.push_str(" (loading…)");
    }
    let content = scrollable_content(
        "diff-pane-content",
        &model.diff_scroll,
        3,
        |scroll_msg| Msg::Diff(DiffMsg::Scroll(scroll_msg)),
        block_with_title(
            diff_title,
            vec![render_diff_lines(
                &model.diff_lines,
                model.show_diff_line_numbers,
            )],
        ),
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO);

    column(vec![content])
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO)
        .with_id("diff-pane")
}

fn render_shortcuts_bar(model: &Model) -> Node<Msg> {
    let shortcuts = shortcuts::display_shortcuts(model);
    let items: Vec<Node<Msg>> = shortcuts
        .into_iter()
        .enumerate()
        .map(|(idx, shortcut)| {
            let spans = vec![
                TextSpan::new(shortcut.label, shortcut_key_style()),
                TextSpan::new(" ", Style::default()),
                TextSpan::new(shortcut.description, shortcut_description_style()),
            ];

            let item = row(vec![rich_text::<Msg>(spans)])
                .with_padding_2d(1, 0)
                .with_flex_grow(0.)
                .with_flex_shrink(0.)
                .with_id_mixin("shortcut-item", idx as u64);

            if let Some(msg) = shortcut.msg.clone() {
                item.on_click(move || msg.clone())
            } else {
                item
            }
        })
        .collect();

    let mut shortcuts_row = row(items)
        .with_width(Dimension::percent(1.))
        .with_id("shortcuts-items");

    shortcuts_row = shortcuts_row.with_flex_wrap(FlexWrap::Wrap);
    if !model.show_all_shortcuts {
        shortcuts_row = shortcuts_row.with_height(Dimension::length(1.));
    }

    block_with_title(
        "Shortcuts",
        vec![row(vec![
            shortcuts_row
                .with_flex_grow(1.)
                .with_flex_shrink(1.)
                .with_min_width(Dimension::from_length(0.0))
                .with_overflow_x(taffy::Overflow::Clip),
            // Make the help toggle in the shortcuts bar clickable so users can switch
            // between the compact and expanded views via mouse.
            rich_text(&[
                TextSpan::new("?", shortcut_key_style()),
                TextSpan::new(" ", Style::default()),
                TextSpan::new(
                    if model.show_all_shortcuts {
                        "less"
                    } else {
                        "more"
                    },
                    shortcut_description_style(),
                ),
            ])
            .on_click(|| Msg::Global(GlobalMsg::ToggleShortcutsHelp))
            .with_id("more-less-button"),
        ])],
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(0.)
    .with_flex_shrink(0.)
    .with_id("shortcuts-bar")
}

fn render_diff_lines(lines: &[DiffLine], show_line_numbers: bool) -> Node<Msg> {
    if lines.is_empty() {
        return column(vec![
            text::<Msg>("No diff available").with_style(inactive_style()),
        ])
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO);
    }

    renderable(diff_render::DiffLeaf::new(
        lines.to_vec(),
        show_line_numbers,
    ))
    .with_flex_shrink(0.)
    .with_overflow_y(taffy::Overflow::Visible)
    .with_overflow_x(taffy::Overflow::Visible)
    .with_id("diff_lines")
}

fn render_commit_modal(state: &CommitModal) -> Node<Msg> {
    let title = text::<Msg>("Commit staged changes").with_style(Style::bold());
    let input_style = commit_input_style();

    let prompt = text::<Msg>("> ").with_style(Style::fg(highlight::EVERFOREST_GREEN));
    let input_field = input::<Msg>("commit-modal-input", &state.input, &input_style, |msg| {
        Msg::Commit(CommitMsg::Input(msg))
    })
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO)
    .with_min_width(Dimension::ZERO);
    let input_row = row(vec![prompt, input_field])
        .with_fill()
        .with_id("commit-modal-input-row");

    let content = column(vec![title, input_row])
        .with_min_width(Dimension::length(30.0))
        .with_min_height(Dimension::length(5.0));

    modal(vec![
        block_with_title("Commit", vec![content])
            .with_min_width(Dimension::length(42.0))
            .with_min_height(Dimension::length(5.0))
            .with_id("commit-modal-block"),
    ])
    .with_id("commit-modal")
}

fn commit_input_style() -> InputStyle {
    let mut style = InputStyle::default();
    style.text.fg = Some(highlight::EVERFOREST_GREEN);
    style.text.bg = Some(highlight::EVERFOREST_FG);

    let mut selection = style.selection;
    selection.bg = Some(highlight::EVERFOREST_BG_GREEN);
    selection.fg = Some(highlight::EVERFOREST_FG);
    style.selection = selection;

    let mut cursor = style.cursor;
    cursor.bg = Some(highlight::EVERFOREST_GREEN);
    cursor.fg = Some(highlight::EVERFOREST_BG_GREEN);
    style.cursor = cursor;

    style
}

fn render_delete_modal(state: &DeleteModal) -> Node<Msg> {
    let title = text::<Msg>("Delete unstaged changes?").with_style(Style::bold());
    let details = text::<Msg>(&state.display_text()).with_style(Style::dim());
    let instructions = text::<Msg>("y/Enter to confirm, n/Esc to cancel").with_style(Style::dim());

    let content = column(vec![title, details, instructions])
        .with_min_width(Dimension::length(36.0))
        .with_min_height(Dimension::length(3.0));

    modal(vec![
        block_with_title("Confirm Delete", vec![content])
            .with_min_width(Dimension::length(48.0))
            .with_min_height(Dimension::length(5.0))
            .with_id("delete-modal-block"),
    ])
    .with_id("delete-modal")
}

fn render_fuzzy_finder_inline(state: &FuzzyFinderState) -> Node<Msg> {
    fuztea::view(&state.finder, |msg| Msg::Fuzzy(FuzzyMsg::Inner(msg)))
}

fn title_style() -> Style {
    let mut style = Style::bold();
    style.fg = Some(highlight::EVERFOREST_BLUE);
    style
}

fn error_style() -> Style {
    Style::fg(highlight::EVERFOREST_RED)
}

fn inactive_style() -> Style {
    Style::fg(highlight::EVERFOREST_GREY2)
}

fn branch_name_style() -> Style {
    Style::fg(highlight::EVERFOREST_AQUA)
}

fn active_selection_style() -> Style {
    let mut style = Style::bold();
    style.fg = Some(highlight::EVERFOREST_FG);
    style.bg = Some(highlight::EVERFOREST_BG_GREEN);
    style
}

fn inactive_selection_style() -> Style {
    Style {
        fg: Some(highlight::EVERFOREST_PURPLE),
        ..Style::default()
    }
}

fn shortcut_key_style() -> Style {
    let mut style = Style::bold();
    style.fg = Some(highlight::EVERFOREST_BLUE);
    style
}

fn shortcut_description_style() -> Style {
    Style::dim()
}

fn status_row_styles(code: char) -> Option<(Style, Style)> {
    let bg = status_forground(code)?;

    let normal = Style {
        fg: Some(bg),
        // bg: Some(highlight::EVERFOREST_FG),
        ..Style::default()
    };

    let selected = Style {
        // fg: None,
        bg: Some(bg),
        ..Style::default()
    };

    Some((normal, selected))
}

fn status_forground(code: char) -> Option<Color> {
    match code {
        // New
        'A' | '?' => Some(highlight::EVERFOREST_GREEN),
        // Removed
        'D' => Some(highlight::EVERFOREST_RED),
        // Modified
        'M' | 'T' => Some(highlight::EVERFOREST_YELLOW),
        // Renamed
        'R' | 'C' => Some(highlight::EVERFOREST_PURPLE),
        _ => Some(highlight::EVERFOREST_AQUA),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
enum Focus {
    #[default]
    Unstaged,
    Staged,
    Changed,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum FileNodeId {
    File(String),
    Dir(String),
}

impl FileNodeId {
    fn path(&self) -> &str {
        match self {
            Self::File(path) | Self::Dir(path) => path,
        }
    }

    fn is_dir(&self) -> bool {
        matches!(self, Self::Dir(_))
    }

    fn mixin(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        match self {
            Self::File(path) => {
                "file".hash(&mut hasher);
                path.hash(&mut hasher);
            }
            Self::Dir(path) => {
                "dir".hash(&mut hasher);
                path.hash(&mut hasher);
            }
        }
        hasher.finish()
    }
}

fn parent_dir_node(path: &str) -> Option<FileNodeId> {
    let (parent, _) = path.rsplit_once('/')?;
    if parent.is_empty() {
        None
    } else {
        Some(FileNodeId::Dir(parent.to_string()))
    }
}

#[derive(Default)]
struct DirBuilder {
    name: String,
    path: String,
    children: BTreeMap<String, NodeBuilder>,
    codes: BTreeSet<char>,
    directory_entry: Option<FileEntry>,
}

enum NodeBuilder {
    Dir(DirBuilder),
    File(FileEntry),
}

impl DirBuilder {
    fn root() -> Self {
        Self {
            name: String::new(),
            path: String::new(),
            children: BTreeMap::new(),
            codes: BTreeSet::new(),
            directory_entry: None,
        }
    }

    fn new(name: String, path: String) -> Self {
        Self {
            name,
            path,
            children: BTreeMap::new(),
            codes: BTreeSet::new(),
            directory_entry: None,
        }
    }

    fn insert(&mut self, components: &[&str], entry: &FileEntry) {
        if components.is_empty() {
            return;
        }

        self.codes.insert(entry.code);

        if components.len() == 1 {
            if components[0].is_empty() {
                self.directory_entry = Some(entry.clone());
            } else {
                self.children
                    .insert(components[0].to_string(), NodeBuilder::File(entry.clone()));
            }
            return;
        }

        let child_name = components[0].to_string();
        let child_path = if self.path.is_empty() {
            child_name.clone()
        } else {
            format!("{}/{}", self.path, child_name)
        };

        match self.children.entry(child_name.clone()) {
            Entry::Vacant(vacant) => {
                let mut dir = DirBuilder::new(child_name, child_path);
                dir.insert(&components[1..], entry);
                vacant.insert(NodeBuilder::Dir(dir));
            }
            Entry::Occupied(mut occupied) => {
                if let NodeBuilder::Dir(dir) = occupied.get_mut() {
                    dir.insert(&components[1..], entry);
                }
            }
        }
    }

    fn into_nodes(self) -> Vec<TreeNode<FileNodeId>> {
        self.children
            .into_values()
            .map(|child| child.into_tree_node())
            .collect()
    }

    fn into_tree_node(self) -> TreeNode<FileNodeId> {
        let DirBuilder {
            name,
            path,
            children,
            codes,
            directory_entry,
        } = self;
        let mut directory_entry = directory_entry;

        let mut labels = Vec::new();
        if !name.is_empty() {
            labels.push(name);
        }

        let mut path_acc = path;
        let mut children_map = children;
        let code = aggregate_code(&codes);
        let mut collapsed = false;

        loop {
            if children_map.len() != 1 {
                break;
            }

            let (child_key, child_builder) = children_map.into_iter().next().unwrap();
            match child_builder {
                NodeBuilder::Dir(dir) => {
                    collapsed = true;
                    let DirBuilder {
                        name: child_name,
                        path: child_path,
                        children: child_children,
                        codes: _,
                        directory_entry: child_entry,
                    } = dir;
                    if !child_name.is_empty() {
                        labels.push(child_name);
                    } else if !child_key.is_empty() {
                        labels.push(child_key);
                    }
                    path_acc = child_path;
                    children_map = child_children;
                    if child_entry.is_some() {
                        directory_entry = child_entry;
                    }
                }
                NodeBuilder::File(file) => {
                    let mut map = BTreeMap::new();
                    map.insert(child_key, NodeBuilder::File(file));
                    children_map = map;
                    break;
                }
            }
        }

        if code == '?'
            && children_map.len() == 1
            && let Some(NodeBuilder::File(file)) = children_map.values().next()
        {
            let entry = file.clone();
            let prefix = if labels.is_empty() {
                String::new()
            } else {
                format!("{}/", labels.join("/"))
            };
            let label = if prefix.is_empty() {
                format!("{} {}", entry.code, entry.tree_label())
            } else {
                format!("{} {}{}", entry.code, prefix, entry.tree_label())
            };
            let node = TreeNode::leaf(
                FileNodeId::File(entry.path.clone()),
                vec![TextSpan::new(label, Style::default())],
            );
            return apply_status_styles(node, entry.code);
        }

        let children: Vec<TreeNode<FileNodeId>> = children_map
            .into_values()
            .map(|child| child.into_tree_node())
            .collect();

        if children.is_empty()
            && let Some(entry) = directory_entry.as_ref()
        {
            let label = entry.display.clone();
            let node = TreeNode::leaf(
                FileNodeId::Dir(path_acc.clone()),
                vec![TextSpan::new(label, Style::default())],
            );
            return apply_status_styles(node, entry.code);
        }

        let label_name = if collapsed {
            let joined = if labels.is_empty() {
                path_acc.clone()
            } else {
                labels.join("/")
            };
            if joined.is_empty() {
                String::from("/")
            } else {
                format!("{joined}/")
            }
        } else if directory_entry.is_some() {
            if path_acc.is_empty() {
                String::from("/")
            } else {
                format!("{path_acc}/")
            }
        } else if let Some(last) = labels.last() {
            last.clone()
        } else if !path_acc.is_empty() {
            path_acc.clone()
        } else {
            String::from(".")
        };

        let label = format!("{} {}", code, label_name);
        let node = TreeNode::branch(
            FileNodeId::Dir(path_acc),
            vec![TextSpan::new(label, Style::default())],
            children,
        );
        apply_status_styles(node, code)
    }
}

impl NodeBuilder {
    fn into_tree_node(self) -> TreeNode<FileNodeId> {
        match self {
            Self::Dir(dir) => dir.into_tree_node(),
            Self::File(entry) => {
                let code = entry.code;
                let label = format!("{} {}", code, entry.tree_label());
                let node = TreeNode::leaf(
                    FileNodeId::File(entry.path.clone()),
                    vec![TextSpan::new(label, Style::default())],
                );
                apply_status_styles(node, code)
            }
        }
    }
}

fn build_file_tree(entries: &[FileEntry]) -> Vec<TreeNode<FileNodeId>> {
    let mut root = DirBuilder::root();
    for entry in entries {
        let components: Vec<&str> = entry.path.split('/').collect();
        if components.is_empty() {
            continue;
        }
        root.insert(&components, entry);
    }
    root.into_nodes()
}

fn aggregate_code(codes: &BTreeSet<char>) -> char {
    if codes.is_empty() {
        ' '
    } else if codes.len() == 1 {
        *codes.iter().next().unwrap()
    } else {
        '*'
    }
}

fn apply_status_styles(node: TreeNode<FileNodeId>, code: char) -> TreeNode<FileNodeId> {
    if node.kind == TreeNodeKind::Leaf
        && let Some((row, selected)) = status_row_styles(code)
    {
        node.with_row_style(row).with_selected_row_style(selected)
    } else {
        node
    }
}

struct CommitModal {
    input: InputState,
}

impl CommitModal {
    fn update(&mut self, msg: InputMsg) {
        self.input.update(msg);
    }

    fn message(&self) -> String {
        self.input.value()
    }
}

impl Default for CommitModal {
    fn default() -> Self {
        Self {
            input: InputState::new_multiline(),
        }
    }
}

#[derive(Default)]
struct DeleteModal {
    path: String,
}

impl DeleteModal {
    fn new(path: String) -> Self {
        Self { path }
    }

    fn display_text(&self) -> String {
        format!("This will discard unstaged changes for: {}", self.path)
    }
}

struct FuzzyFinderState {
    finder: FuzzyFinder,
    original_focus: Focus,
    original_selection: Option<FileNodeId>,
}

#[derive(Default)]
struct Model {
    view_mode: ViewMode,
    focus: Focus,
    diff_lines: Vec<DiffLine>,
    diff_truncated: bool,
    diff_scroll: ScrollState,
    error: Option<String>,
    show_all_shortcuts: bool,
    show_diff_line_numbers: bool,
    status_loading: bool,
    diff_loading: bool,
    next_diff_request_id: u64,
    active_diff_request: Option<DiffRequest>,
}

struct DiffRequest {
    id: u64,
    preserve_scroll: bool,
}

enum DiffMode {
    Sync,
    Async,
}

impl Model {
    const UNSTAGED_CONTAINER_ID: &'static str = "unstaged-section-content";
    const STAGED_CONTAINER_ID: &'static str = "staged-section-content";
    const CHANGED_CONTAINER_ID: &'static str = "changed-section-content";
    const UNSTAGED_ITEM_ID: &'static str = "unstaged-entry";
    const STAGED_ITEM_ID: &'static str = "staged-entry";
    const CHANGED_ITEM_ID: &'static str = "changed-entry";

    fn new(view_mode: ViewMode) -> Self {
        let mut model = Self {
            view_mode,
            diff_scroll: ScrollState::both(),
            ..Default::default()
        };

        if let Err(err) = model.refresh_status() {
            model.set_error(err);
            model.diff_lines = vec![DiffLine::plain("Unable to load git status")];
            model.diff_truncated = false;
        }

        model
    }

    fn tree_state(&self, focus: Focus) -> Option<&TreeState<FileNodeId>> {
        match (&self.view_mode, focus) {
            (ViewMode::WorkingTree(state), Focus::Unstaged) => Some(&state.unstaged_tree),
            (ViewMode::WorkingTree(state), Focus::Staged) => Some(&state.staged_tree),
            (ViewMode::Diff(state), Focus::Changed) => Some(&state.changed_tree),
            _ => None,
        }
    }

    fn tree_state_mut(&mut self, focus: Focus) -> Option<&mut TreeState<FileNodeId>> {
        match (&mut self.view_mode, focus) {
            (ViewMode::WorkingTree(state), Focus::Unstaged) => Some(&mut state.unstaged_tree),
            (ViewMode::WorkingTree(state), Focus::Staged) => Some(&mut state.staged_tree),
            (ViewMode::Diff(state), Focus::Changed) => Some(&mut state.changed_tree),
            _ => None,
        }
    }

    fn selected_id(&self, focus: Focus) -> Option<FileNodeId> {
        self.tree_state(focus)?.selected().cloned()
    }

    fn select_tree_id(&mut self, focus: Focus, id: &FileNodeId) {
        if let Some(state) = self.tree_state_mut(focus) {
            state.select(id.clone());
            state.ensure_selected();
        }
    }

    fn ensure_tree_selection(&mut self, focus: Focus) {
        if let Some(state) = self.tree_state_mut(focus) {
            state.ensure_selected();
        }
    }

    fn select_if_different(&mut self, focus: Focus, id: &FileNodeId) -> bool {
        let should_change = self
            .tree_state(focus)
            .and_then(|state| state.selected())
            .map(|selected| selected != id)
            .unwrap_or(true);
        if should_change {
            self.select_tree_id(focus, id);
        }
        should_change
    }

    fn collapse_selected(&mut self) -> bool {
        let focus = self.focus;
        let Some(id) = self.selected_id(focus) else {
            return false;
        };
        let mut changed = false;

        match &id {
            FileNodeId::Dir(path) => {
                if self.tree_state(focus).is_some_and(|s| s.is_expanded(&id)) {
                    if let Some(state) = self.tree_state_mut(focus) {
                        state.set_expanded(id.clone(), false);
                    }
                    changed = true;
                } else if let Some(parent) = parent_dir_node(path) {
                    if self.select_if_different(focus, &parent) {
                        changed = true;
                    }
                    if self
                        .tree_state(focus)
                        .is_some_and(|s| s.is_expanded(&parent))
                    {
                        if let Some(state) = self.tree_state_mut(focus) {
                            state.set_expanded(parent.clone(), false);
                        }
                        changed = true;
                    }
                }
            }
            FileNodeId::File(path) => {
                if let Some(parent) = parent_dir_node(path) {
                    if self.select_if_different(focus, &parent) {
                        changed = true;
                    }
                    if self
                        .tree_state(focus)
                        .is_some_and(|s| s.is_expanded(&parent))
                    {
                        if let Some(state) = self.tree_state_mut(focus) {
                            state.set_expanded(parent.clone(), false);
                        }
                        changed = true;
                    }
                }
            }
        }

        if changed {
            self.queue_scroll_for_focus(focus);
        }
        changed
    }

    fn expand_selected(&mut self) -> bool {
        let focus = self.focus;
        let Some(id) = self.selected_id(focus) else {
            return false;
        };
        let mut changed = false;

        match &id {
            FileNodeId::Dir(_) => {
                if !self.tree_state(focus).is_some_and(|s| s.is_expanded(&id)) {
                    if let Some(state) = self.tree_state_mut(focus) {
                        state.set_expanded(id.clone(), true);
                    }
                    changed = true;
                }
            }
            FileNodeId::File(path) => {
                if let Some(parent) = parent_dir_node(path)
                    && !self
                        .tree_state(focus)
                        .is_some_and(|s| s.is_expanded(&parent))
                {
                    if let Some(state) = self.tree_state_mut(focus) {
                        state.set_expanded(parent.clone(), true);
                    }
                    changed = true;
                }
            }
        }

        if changed {
            self.queue_scroll_for_focus(focus);
        }
        changed
    }

    fn is_first_selected(&self, focus: Focus) -> bool {
        let Some(state) = self.tree_state(focus) else {
            return true;
        };
        match (state.selected(), state.visible().first()) {
            (Some(selected), Some(first)) => selected == &first.id,
            _ => true,
        }
    }

    fn is_last_selected(&self, focus: Focus) -> bool {
        let Some(state) = self.tree_state(focus) else {
            return true;
        };
        match (state.selected(), state.visible().last()) {
            (Some(selected), Some(last)) => selected == &last.id,
            _ => true,
        }
    }

    fn set_error(&mut self, err: miette::Report) {
        self.error = Some(err.to_string());
    }

    fn clear_error(&mut self) {
        self.error = None;
    }

    fn open_commit_modal(&mut self) {
        if let Some(state) = self.view_mode.working_tree_mut() {
            state.commit_modal = Some(CommitModal::default());
        }
    }

    fn close_commit_modal(&mut self) {
        if let Some(state) = self.view_mode.working_tree_mut() {
            state.commit_modal = None;
        }
    }

    fn commit_modal(&self) -> Option<&CommitModal> {
        self.view_mode
            .working_tree()
            .and_then(|s| s.commit_modal.as_ref())
    }

    fn commit_modal_mut(&mut self) -> Option<&mut CommitModal> {
        self.view_mode
            .working_tree_mut()
            .and_then(|s| s.commit_modal.as_mut())
    }

    fn delete_modal(&self) -> Option<&DeleteModal> {
        self.view_mode
            .working_tree()
            .and_then(|s| s.delete_modal.as_ref())
    }

    fn open_delete_modal(&mut self) {
        let entry_path = if self.focus == Focus::Unstaged {
            self.current_entry().map(|e| e.path.clone())
        } else {
            None
        };

        if let Some(path) = entry_path
            && let Some(state) = self.view_mode.working_tree_mut()
        {
            state.delete_modal = Some(DeleteModal::new(path));
        }
    }

    fn close_delete_modal(&mut self) {
        if let Some(state) = self.view_mode.working_tree_mut() {
            state.delete_modal = None;
        }
    }

    fn fuzzy_finder(&self) -> Option<&FuzzyFinderState> {
        match &self.view_mode {
            ViewMode::WorkingTree(state) => state.fuzzy_finder.as_ref(),
            ViewMode::Diff(state) => state.fuzzy_finder.as_ref(),
        }
    }

    fn fuzzy_finder_mut(&mut self) -> Option<&mut FuzzyFinderState> {
        match &mut self.view_mode {
            ViewMode::WorkingTree(state) => state.fuzzy_finder.as_mut(),
            ViewMode::Diff(state) => state.fuzzy_finder.as_mut(),
        }
    }

    fn open_fuzzy_finder(&mut self) {
        let entries = self.entries_for_focus(self.focus);
        if entries.is_empty() {
            return;
        }

        let (mut finder, handle) = FuzzyFinder::new();
        for entry in entries {
            handle.push_item(entry.path.clone());
        }
        finder.tick();

        let original_selection = self.selected_id(self.focus);
        let fuzzy_state = FuzzyFinderState {
            finder,
            original_focus: self.focus,
            original_selection,
        };

        match &mut self.view_mode {
            ViewMode::WorkingTree(state) => state.fuzzy_finder = Some(fuzzy_state),
            ViewMode::Diff(state) => state.fuzzy_finder = Some(fuzzy_state),
        }
    }

    fn close_fuzzy_finder(&mut self) {
        match &mut self.view_mode {
            ViewMode::WorkingTree(state) => state.fuzzy_finder = None,
            ViewMode::Diff(state) => state.fuzzy_finder = None,
        }
    }

    fn handle_fuzzy_inner(&mut self, msg: FuzzyFinderMsg) -> Transition<Msg> {
        let Some(fuzzy_state) = self.fuzzy_finder_mut() else {
            return Transition::Continue;
        };

        let event = fuztea::update(&mut fuzzy_state.finder, msg, |inner| {
            Msg::Fuzzy(FuzzyMsg::Inner(inner))
        });

        match event {
            FuzzyFinderEvent::Continue => Transition::Continue,
            FuzzyFinderEvent::Cancel => update(self, Msg::Fuzzy(FuzzyMsg::Cancel)),
            FuzzyFinderEvent::Select => {
                self.update_diff_for_fuzzy_selection();
                Transition::Continue
            }
            FuzzyFinderEvent::Activate => update(self, Msg::Fuzzy(FuzzyMsg::Accept)),
            FuzzyFinderEvent::Task(task) => Transition::Task(task),
        }
    }

    fn update_diff_for_fuzzy_selection(&mut self) {
        let current_path = {
            let Some(fuzzy_state) = self.fuzzy_finder() else {
                return;
            };
            fuzzy_state.finder.current_selection()
        };

        let Some(path) = current_path else {
            return;
        };

        let focus = self.focus;
        let id = FileNodeId::File(path);
        self.select_tree_id(focus, &id);
        self.update_diff_sync(false);
    }

    fn accept_fuzzy_finder(&mut self) -> Transition<Msg> {
        let selected_path = {
            let Some(fuzzy_state) = self.fuzzy_finder() else {
                return Transition::Continue;
            };
            fuzzy_state.finder.submission().map(String::from)
        };

        let Some(path) = selected_path else {
            return self.cancel_fuzzy_finder();
        };

        let focus = self.focus;
        self.close_fuzzy_finder();

        let id = FileNodeId::File(path);
        self.select_tree_id(focus, &id);
        self.queue_scroll_for_focus(focus);

        match self.start_diff_refresh(false) {
            Some(task) => Transition::Task(task),
            None => Transition::Continue,
        }
    }

    fn cancel_fuzzy_finder(&mut self) -> Transition<Msg> {
        let (original_focus, original_selection) = {
            let Some(fuzzy_state) = self.fuzzy_finder() else {
                return Transition::Continue;
            };
            (
                fuzzy_state.original_focus,
                fuzzy_state.original_selection.clone(),
            )
        };

        self.close_fuzzy_finder();

        if let Some(id) = original_selection {
            self.focus = original_focus;
            self.select_tree_id(original_focus, &id);
            self.queue_scroll_for_focus(original_focus);
        }

        match self.start_diff_refresh(false) {
            Some(task) => Transition::Task(task),
            None => Transition::Continue,
        }
    }

    fn current_branch(&self) -> Option<&str> {
        self.view_mode
            .working_tree()
            .and_then(|s| s.current_branch.as_deref())
    }

    fn toggle_shortcuts_help(&mut self) {
        self.show_all_shortcuts = !self.show_all_shortcuts;
    }

    fn toggle_diff_line_numbers(&mut self) {
        self.show_diff_line_numbers = !self.show_diff_line_numbers;
    }

    fn submit_commit(&mut self) -> Transition<Msg> {
        let message = match self.commit_modal() {
            Some(modal) => modal.message(),
            None => return Transition::Continue,
        };

        if message.trim().is_empty() {
            self.set_error(miette!("Commit message cannot be empty"));
            return Transition::Continue;
        }

        match git::run_git(["commit", "-m", message.as_str()]) {
            Ok(_) => {
                self.close_commit_modal();
                if let Err(err) = self.refresh_status() {
                    self.set_error(err);
                }
            }
            Err(err) => {
                self.set_error(err);
            }
        }

        Transition::Continue
    }

    fn confirm_delete(&mut self) -> Transition<Msg> {
        let path = match self.delete_modal() {
            Some(modal) => modal.path.clone(),
            None => return Transition::Continue,
        };

        // Determine the selected entry code to decide how to discard.
        let code = match self.focus {
            Focus::Unstaged => self.current_entry().map(|e| e.code).unwrap_or(' '),
            Focus::Staged | Focus::Changed => ' ',
        };

        let result = git::discard_unstaged_changes(&path, code);
        self.close_delete_modal();

        match result.and_then(|_| self.refresh_status()) {
            Ok(_) => {}
            Err(err) => self.set_error(err),
        }

        Transition::Continue
    }

    fn start_status_refresh(&mut self, preserve_diff_scroll: bool) -> Transition<Msg> {
        if self.status_loading {
            return Transition::Continue;
        }

        self.status_loading = true;
        let future = async move {
            let result = unblock(git::load_status)
                .await
                .map_err(|err| err.to_string());
            Msg::StatusLoaded {
                preserve_diff_scroll,
                result,
            }
        };

        Transition::Task(Box::pin(future))
    }

    fn handle_status_loaded(
        &mut self,
        preserve_diff_scroll: bool,
        result: StdResult<GitStatus, String>,
    ) -> Transition<Msg> {
        self.status_loading = false;

        match result {
            Ok(status) => {
                if let Some(task) = self.apply_status(status, preserve_diff_scroll, DiffMode::Async)
                {
                    Transition::Task(task)
                } else {
                    Transition::Continue
                }
            }
            Err(err) => {
                self.set_error(miette!("{}", err));
                Transition::Continue
            }
        }
    }

    fn refresh_status(&mut self) -> Result<()> {
        match &self.view_mode {
            ViewMode::WorkingTree(_) => {
                let status = git::load_status()?;
                let _ = self.apply_status(status, false, DiffMode::Sync);
            }
            ViewMode::Diff(state) => {
                let files = git::load_diff_files(&state.spec)?;
                self.apply_diff_files(files, false, DiffMode::Sync);
            }
        }
        Ok(())
    }

    fn apply_diff_files(
        &mut self,
        files: Vec<FileEntry>,
        preserve_diff_scroll: bool,
        diff_mode: DiffMode,
    ) -> Option<TaskFn<Msg>> {
        let state = self.view_mode.diff_state_mut()?;
        state.changed = files;
        let changed_nodes = build_file_tree(&state.changed);
        state.changed_tree.set_items(changed_nodes);
        if !state.changed_initialized && !state.changed_tree.visible().is_empty() {
            state.changed_tree.expand_all();
            state.changed_initialized = true;
        }
        self.focus = Focus::Changed;
        self.ensure_tree_selection(Focus::Changed);
        self.clamp_file_scrolls();
        self.clear_error();

        match diff_mode {
            DiffMode::Sync => {
                self.update_diff_sync(preserve_diff_scroll);
                None
            }
            DiffMode::Async => self.start_diff_refresh(preserve_diff_scroll),
        }
    }

    fn apply_status(
        &mut self,
        status: GitStatus,
        preserve_diff_scroll: bool,
        diff_mode: DiffMode,
    ) -> Option<TaskFn<Msg>> {
        let previous_focus = if preserve_diff_scroll {
            Some(self.focus)
        } else {
            None
        };
        let previous_selection = if preserve_diff_scroll {
            self.selected_id(self.focus)
        } else {
            None
        };
        let (previous_diff_offset_y, previous_diff_offset_x) = if preserve_diff_scroll {
            (
                Some(self.diff_scroll.offset_y()),
                Some(self.diff_scroll.offset_x()),
            )
        } else {
            (None, None)
        };

        let wt = self.view_mode.working_tree_mut()?;

        let GitStatus {
            unstaged,
            staged,
            branch,
        } = status;
        wt.unstaged = unstaged;
        wt.staged = staged;
        wt.current_branch = branch;
        let unstaged_nodes = build_file_tree(&wt.unstaged);
        let staged_nodes = build_file_tree(&wt.staged);
        wt.unstaged_tree.set_items(unstaged_nodes);
        wt.staged_tree.set_items(staged_nodes);
        if !wt.unstaged_initialized && !wt.unstaged_tree.visible().is_empty() {
            wt.unstaged_tree.expand_all();
            wt.unstaged_initialized = true;
        }
        if !wt.staged_initialized && !wt.staged_tree.visible().is_empty() {
            wt.staged_tree.expand_all();
            wt.staged_initialized = true;
        }
        self.ensure_focus_valid();
        self.clear_error();
        let diff_task = match diff_mode {
            DiffMode::Sync => {
                self.update_diff_sync(preserve_diff_scroll);
                None
            }
            DiffMode::Async => self.start_diff_refresh(preserve_diff_scroll),
        };
        if let (Some(prev_focus), Some(prev_selection)) = (previous_focus, previous_selection)
            && self.focus == prev_focus
            && self
                .tree_state(self.focus)
                .and_then(|s| s.selected())
                .is_some_and(|current| current == &prev_selection)
        {
            if let Some(offset) = previous_diff_offset_y {
                self.diff_scroll
                    .set_offset_for(ScrollAxis::Vertical, offset);
            }
            if let Some(offset) = previous_diff_offset_x {
                self.diff_scroll
                    .set_offset_for(ScrollAxis::Horizontal, offset);
            }
        }

        diff_task
    }

    fn ensure_focus_valid(&mut self) {
        match &self.view_mode {
            ViewMode::Diff(_) => {
                self.ensure_tree_selection(Focus::Changed);
                self.clamp_file_scrolls();
            }
            ViewMode::WorkingTree(wt) => {
                if self.focus == Focus::Unstaged
                    && wt.unstaged_tree.visible().is_empty()
                    && !wt.staged_tree.visible().is_empty()
                {
                    self.focus = Focus::Staged;
                } else if self.focus == Focus::Staged
                    && wt.staged_tree.visible().is_empty()
                    && !wt.unstaged_tree.visible().is_empty()
                {
                    self.focus = Focus::Unstaged;
                }
                self.ensure_tree_selection(Focus::Unstaged);
                self.ensure_tree_selection(Focus::Staged);
                self.clamp_file_scrolls();
            }
        }
    }

    fn start_diff_refresh(&mut self, preserve_scroll: bool) -> Option<TaskFn<Msg>> {
        let selected_id = self.selected_id(self.focus);
        match (self.current_entry(), selected_id.as_ref()) {
            (Some(entry), _) => {
                let entry = entry.clone();
                let request_id = self.next_diff_request_id;
                self.next_diff_request_id = self.next_diff_request_id.wrapping_add(1);
                self.active_diff_request = Some(DiffRequest {
                    id: request_id,
                    preserve_scroll,
                });
                self.diff_loading = true;
                if !preserve_scroll || self.diff_lines.is_empty() {
                    self.diff_lines = vec![DiffLine::styled("Loading diff…", Style::dim())];
                }
                if !preserve_scroll {
                    self.diff_scroll.reset();
                }
                self.diff_truncated = false;

                let focus = self.focus;
                let diff_spec = self.view_mode.spec().map(String::from);

                let future = async move {
                    let result = unblock(move || diff_for(&entry, focus, diff_spec.as_deref()))
                        .await
                        .map_err(|err| err.to_string());
                    Msg::DiffLoaded { request_id, result }
                };

                Some(Box::pin(future))
            }
            (None, Some(FileNodeId::Dir(_))) => {
                self.active_diff_request = None;
                self.diff_loading = false;
                self.diff_truncated = false;
                self.diff_lines = vec![DiffLine::plain("Select a file to view diff")];
                self.diff_scroll.reset();
                None
            }
            (None, _) => {
                self.active_diff_request = None;
                self.diff_loading = false;
                self.diff_truncated = false;
                self.diff_lines = vec![DiffLine::plain("No file selected")];
                self.diff_scroll.reset();
                None
            }
        }
    }

    fn update_diff_sync(&mut self, preserve_scroll: bool) {
        self.diff_loading = false;
        self.active_diff_request = None;

        let selected_id = self.selected_id(self.focus);
        let diff_spec = self.view_mode.spec();
        match (self.current_entry(), selected_id.as_ref()) {
            (Some(entry), _) => match diff_for(entry, self.focus, diff_spec) {
                Ok(preview) => self.apply_diff_preview(preview, preserve_scroll),
                Err(err) => {
                    self.diff_lines = vec![DiffLine::plain("Failed to load diff")];
                    self.diff_truncated = false;
                    self.set_error(err);
                    self.diff_scroll.reset();
                }
            },
            (None, Some(FileNodeId::Dir(_))) => {
                self.diff_lines = vec![DiffLine::plain("Select a file to view diff")];
                self.diff_truncated = false;
                self.diff_scroll.reset();
            }
            (None, _) => {
                self.diff_lines = vec![DiffLine::plain("No file selected")];
                self.diff_truncated = false;
                self.diff_scroll.reset();
            }
        }
    }

    fn handle_diff_loaded(
        &mut self,
        request_id: u64,
        result: StdResult<DiffPreview, String>,
    ) -> Transition<Msg> {
        let Some(active) = self.active_diff_request.as_ref() else {
            return Transition::Continue;
        };

        if active.id != request_id {
            return Transition::Continue;
        }

        let preserve_scroll = active.preserve_scroll;
        self.active_diff_request = None;
        self.diff_loading = false;

        match result {
            Ok(preview) => {
                self.apply_diff_preview(preview, preserve_scroll);
            }
            Err(err) => {
                self.diff_lines = vec![DiffLine::plain("Failed to load diff")];
                self.diff_truncated = false;
                if !preserve_scroll {
                    self.diff_scroll.reset();
                }
                self.set_error(miette!("{}", err));
            }
        }

        Transition::Continue
    }

    fn apply_diff_preview(&mut self, preview: DiffPreview, preserve_scroll: bool) {
        let DiffPreview { lines, truncated } = preview;
        if lines.is_empty() {
            self.diff_lines = vec![DiffLine::plain("No diff available")];
            self.diff_truncated = false;
        } else {
            self.diff_lines = lines;
            self.diff_truncated = truncated;
        }

        if !preserve_scroll {
            self.diff_scroll.reset();
        }

        if matches!(self.error.as_deref(), Some(msg) if msg.starts_with("git diff")) {
            self.clear_error();
        }
    }

    fn current_entry(&self) -> Option<&FileEntry> {
        let focus = self.focus;
        let selected = self.tree_state(focus)?.selected()?;
        let path = match selected {
            FileNodeId::File(path) => path,
            FileNodeId::Dir(_) => return None,
        };
        self.entries_for_focus(focus)
            .iter()
            .find(|entry| entry.path == *path)
    }

    fn entries_for_focus(&self, focus: Focus) -> &[FileEntry] {
        match (&self.view_mode, focus) {
            (ViewMode::WorkingTree(state), Focus::Unstaged) => &state.unstaged,
            (ViewMode::WorkingTree(state), Focus::Staged) => &state.staged,
            (ViewMode::Diff(state), Focus::Changed) => &state.changed,
            _ => &[],
        }
    }

    fn scroll_state_mut(&mut self, focus: Focus) -> Option<&mut ScrollState> {
        match (&mut self.view_mode, focus) {
            (ViewMode::WorkingTree(state), Focus::Unstaged) => Some(&mut state.unstaged_scroll),
            (ViewMode::WorkingTree(state), Focus::Staged) => Some(&mut state.staged_scroll),
            (ViewMode::Diff(state), Focus::Changed) => Some(&mut state.changed_scroll),
            _ => None,
        }
    }

    fn move_selection_up(&mut self) {
        let focus = self.focus;
        let is_first = self.is_first_selected(focus);

        match (&mut self.view_mode, focus) {
            (ViewMode::WorkingTree(wt), Focus::Unstaged) => {
                if wt.unstaged_tree.visible().is_empty() {
                    return;
                }
                if is_first {
                    if !wt.staged_tree.visible().is_empty() {
                        self.focus = Focus::Staged;
                        wt.staged_tree.select_last();
                        wt.staged_tree.ensure_selected();
                        self.queue_scroll_for_staged();
                    }
                } else {
                    wt.unstaged_tree.select_prev();
                    self.queue_scroll_for_unstaged();
                }
            }
            (ViewMode::WorkingTree(wt), Focus::Staged) => {
                if wt.staged_tree.visible().is_empty() {
                    return;
                }
                if is_first {
                    if !wt.unstaged_tree.visible().is_empty() {
                        self.focus = Focus::Unstaged;
                        wt.unstaged_tree.select_last();
                        wt.unstaged_tree.ensure_selected();
                        self.queue_scroll_for_unstaged();
                    }
                } else {
                    wt.staged_tree.select_prev();
                    self.queue_scroll_for_staged();
                }
            }
            (ViewMode::Diff(diff), Focus::Changed) => {
                if diff.changed_tree.visible().is_empty() {
                    return;
                }
                if !is_first {
                    diff.changed_tree.select_prev();
                    self.queue_scroll_for_changed();
                }
            }
            _ => {}
        }
    }

    fn move_selection_down(&mut self) {
        let focus = self.focus;
        let is_last = self.is_last_selected(focus);

        match (&mut self.view_mode, focus) {
            (ViewMode::WorkingTree(wt), Focus::Unstaged) => {
                if wt.unstaged_tree.visible().is_empty() {
                    return;
                }
                if is_last {
                    if !wt.staged_tree.visible().is_empty() {
                        self.focus = Focus::Staged;
                        wt.staged_tree.select_first();
                        wt.staged_tree.ensure_selected();
                        self.queue_scroll_for_staged();
                    }
                } else {
                    wt.unstaged_tree.select_next();
                    self.queue_scroll_for_unstaged();
                }
            }
            (ViewMode::WorkingTree(wt), Focus::Staged) => {
                if wt.staged_tree.visible().is_empty() {
                    return;
                }
                if is_last {
                    return;
                }
                wt.staged_tree.select_next();
                self.queue_scroll_for_staged();
            }
            (ViewMode::Diff(diff), Focus::Changed) => {
                if diff.changed_tree.visible().is_empty() {
                    return;
                }
                if !is_last {
                    diff.changed_tree.select_next();
                    self.queue_scroll_for_changed();
                }
            }
            _ => {}
        }
    }

    fn focus_unstaged(&mut self) {
        if let Some(wt) = self.view_mode.working_tree_mut()
            && !wt.unstaged_tree.visible().is_empty()
        {
            self.focus = Focus::Unstaged;
            wt.unstaged_tree.ensure_selected();
            self.queue_scroll_for_unstaged();
        }
    }

    fn focus_staged(&mut self) {
        if let Some(wt) = self.view_mode.working_tree_mut()
            && !wt.staged_tree.visible().is_empty()
        {
            self.focus = Focus::Staged;
            wt.staged_tree.ensure_selected();
            self.queue_scroll_for_staged();
        }
    }

    fn toggle_stage_selected(&mut self) {
        if self.view_mode.is_diff() {
            return;
        }
        if let Some(id) = self.selected_id(self.focus) {
            self.toggle_stage_for_id(self.focus, &id);
        }
    }

    fn toggle_stage_for_id(&mut self, focus: Focus, id: &FileNodeId) {
        if self.view_mode.is_diff() {
            return;
        }
        let path = id.path();
        let result = match focus {
            Focus::Unstaged => git::stage_path(path),
            Focus::Staged => git::unstage_path(path),
            Focus::Changed => return,
        };

        if let Err(err) = result {
            self.set_error(err);
            return;
        }

        if let Err(err) = self.refresh_status() {
            self.set_error(err);
        }
    }

    fn handle_tree_message(
        &mut self,
        focus: Focus,
        tree_msg: TreeMsg<FileNodeId>,
    ) -> Transition<Msg> {
        match tree_msg {
            TreeMsg::Activate(id) => {
                self.focus = focus;
                self.select_tree_id(focus, &id);
                self.queue_scroll_for_focus(focus);
                match self.start_diff_refresh(false) {
                    Some(task) => Transition::Task(task),
                    None => Transition::Continue,
                }
            }
            TreeMsg::ToggleExpand(id) => {
                self.focus = focus;
                self.select_tree_id(focus, &id);
                if id.is_dir() {
                    if let Some(state) = self.tree_state_mut(focus) {
                        state.toggle_expanded(&id);
                    }
                    self.queue_scroll_for_focus(focus);
                    match self.start_diff_refresh(false) {
                        Some(task) => Transition::Task(task),
                        None => Transition::Continue,
                    }
                } else {
                    self.queue_scroll_for_focus(focus);
                    self.toggle_stage_for_id(focus, &id);
                    Transition::Continue
                }
            }
            TreeMsg::DoubleClick(id) => {
                self.focus = focus;
                self.select_tree_id(focus, &id);
                self.queue_scroll_for_focus(focus);
                self.toggle_stage_for_id(focus, &id);
                Transition::Continue
            }
        }
    }

    fn scroll_diff(&mut self, delta: i32) {
        self.diff_scroll.update(ScrollMsg::AxisDelta {
            axis: ScrollAxis::Vertical,
            amount: delta,
        });
    }

    fn scroll_diff_horizontal(&mut self, delta: i32) {
        self.diff_scroll.update(ScrollMsg::AxisDelta {
            axis: ScrollAxis::Horizontal,
            amount: delta,
        });
    }

    fn scroll_files(&mut self, focus: Focus, delta: i32) {
        self.update_section_scroll(
            focus,
            ScrollMsg::AxisDelta {
                axis: ScrollAxis::Vertical,
                amount: delta,
            },
        );
    }

    fn scroll_files_in_focus(&mut self, delta: i32) {
        let focus = self.focus;
        self.scroll_files(focus, delta);
    }

    fn queue_scroll_for_focus(&mut self, focus: Focus) {
        match focus {
            Focus::Unstaged => self.queue_scroll_for_unstaged(),
            Focus::Staged => self.queue_scroll_for_staged(),
            Focus::Changed => self.queue_scroll_for_changed(),
        }
    }

    fn queue_scroll_for_unstaged(&mut self) {
        if let Some(id) = self.selected_id(Focus::Unstaged)
            && let Some(wt) = self.view_mode.working_tree_mut()
        {
            let target = ScrollTarget::with_mixin(Self::UNSTAGED_ITEM_ID, id.mixin());
            wt.unstaged_scroll
                .ensure_visible(Self::UNSTAGED_CONTAINER_ID, target);
        }
    }

    fn queue_scroll_for_staged(&mut self) {
        if let Some(id) = self.selected_id(Focus::Staged)
            && let Some(wt) = self.view_mode.working_tree_mut()
        {
            let target = ScrollTarget::with_mixin(Self::STAGED_ITEM_ID, id.mixin());
            wt.staged_scroll
                .ensure_visible(Self::STAGED_CONTAINER_ID, target);
        }
    }

    fn queue_scroll_for_changed(&mut self) {
        if let Some(id) = self.selected_id(Focus::Changed)
            && let Some(diff) = self.view_mode.diff_state_mut()
        {
            let target = ScrollTarget::with_mixin(Self::CHANGED_ITEM_ID, id.mixin());
            diff.changed_scroll
                .ensure_visible(Self::CHANGED_CONTAINER_ID, target);
        }
    }

    fn update_section_scroll(&mut self, focus: Focus, msg: ScrollMsg) {
        if matches!(msg, ScrollMsg::AxisDelta { .. })
            && self
                .tree_state(focus)
                .map(|s| s.visible().is_empty())
                .unwrap_or(true)
        {
            return;
        }
        if let Some(scroll) = self.scroll_state_mut(focus) {
            scroll.update(msg);
        }
    }

    fn clamp_file_scrolls(&mut self) {
        match &mut self.view_mode {
            ViewMode::WorkingTree(wt) => {
                if wt.unstaged_tree.visible().is_empty() {
                    wt.unstaged_scroll.reset();
                } else {
                    let max_offset = (wt.unstaged_tree.visible().len().saturating_sub(1)) as f32;
                    if wt.unstaged_scroll.offset() > max_offset {
                        wt.unstaged_scroll.set_offset(max_offset);
                    }
                }
                if wt.staged_tree.visible().is_empty() {
                    wt.staged_scroll.reset();
                } else {
                    let max_offset = (wt.staged_tree.visible().len().saturating_sub(1)) as f32;
                    if wt.staged_scroll.offset() > max_offset {
                        wt.staged_scroll.set_offset(max_offset);
                    }
                }
            }
            ViewMode::Diff(diff) => {
                if diff.changed_tree.visible().is_empty() {
                    diff.changed_scroll.reset();
                } else {
                    let max_offset = (diff.changed_tree.visible().len().saturating_sub(1)) as f32;
                    if diff.changed_scroll.offset() > max_offset {
                        diff.changed_scroll.set_offset(max_offset);
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Msg {
    Global(GlobalMsg),
    Navigation(NavigationMsg),
    Diff(DiffMsg),
    Commit(CommitMsg),
    Delete(DeleteMsg),
    Fuzzy(FuzzyMsg),
    Section {
        focus: Focus,
        msg: SectionMsg,
    },
    DiffLoaded {
        request_id: u64,
        result: StdResult<DiffPreview, String>,
    },
    StatusLoaded {
        preserve_diff_scroll: bool,
        result: StdResult<GitStatus, String>,
    },
    Scroll {
        focus: Focus,
        msg: ScrollMsg,
    },
}

#[derive(Clone, Debug)]
enum GlobalMsg {
    KeyPressed(Key),
    Quit,
    RefreshStatus,
    ToggleShortcutsHelp,
}

#[derive(Clone, Debug)]
enum NavigationMsg {
    ToggleStage,
    ToggleFocus,
    MoveSelectionUp,
    MoveSelectionDown,
    CollapseNode,
    ExpandNode,
    ScrollFiles(i32),
}

#[derive(Clone, Debug)]
enum DiffMsg {
    ScrollVertical(i32),
    ScrollHorizontal(i32),
    Scroll(ScrollMsg),
    ToggleLineNumbers,
}

#[derive(Clone, Debug)]
enum CommitMsg {
    Open,
    Submit,
    Cancel,
    Input(InputMsg),
}

#[derive(Clone, Debug)]
enum DeleteMsg {
    Open,
    Confirm,
    Cancel,
}

#[derive(Clone, Debug)]
enum FuzzyMsg {
    Open,
    Inner(FuzzyFinderMsg),
    Accept,
    Cancel,
}

#[derive(Clone, Debug)]
enum SectionMsg {
    Tree(TreeMsg<FileNodeId>),
}

#[derive(Clone, Debug, PartialEq)]
struct DiffLine {
    spans: Vec<TextSpan>,
    line_numbers: Option<LineNumbers>,
}

#[derive(Clone, Debug, PartialEq)]
struct LineNumbers {
    old: Option<usize>,
    new: Option<usize>,
}

impl DiffLine {
    fn plain(content: impl Into<String>) -> Self {
        let content = content.into();
        Self {
            spans: vec![TextSpan::new(content, Style::default())],
            line_numbers: None,
        }
    }

    fn styled(content: impl Into<String>, style: Style) -> Self {
        let content = content.into();
        if style == Style::default() {
            return Self::plain(content);
        }
        Self {
            spans: vec![TextSpan::new(content, style)],
            line_numbers: None,
        }
    }

    fn from_spans(spans: Vec<TextSpan>) -> Self {
        Self {
            spans,
            line_numbers: None,
        }
    }

    fn with_line_numbers(mut self, old: Option<usize>, new: Option<usize>) -> Self {
        self.line_numbers = Some(LineNumbers { old, new });
        self
    }
}

#[derive(Clone, Debug)]
struct DiffPreview {
    lines: Vec<DiffLine>,
    truncated: bool,
}

fn diff_for(entry: &FileEntry, focus: Focus, diff_spec: Option<&str>) -> Result<DiffPreview> {
    let versions = load_file_versions(entry, focus, diff_spec)?;
    if versions.old.is_none() && versions.new.is_none() {
        return Ok(DiffPreview {
            lines: Vec::new(),
            truncated: versions.truncated,
        });
    }

    let lines = build_diff_lines(&versions);

    Ok(DiffPreview {
        lines,
        truncated: versions.truncated,
    })
}

struct FileVersions {
    old: Option<FileContent>,
    new: Option<FileContent>,
    truncated: bool,
    is_binary: bool,
}

#[derive(Clone)]
struct FileContent {
    lines: Vec<LineContent>,
    ends_with_newline: bool,
}

#[derive(Clone)]
struct LineContent {
    text: String,
    spans: Vec<TextSpan>,
    context: Option<String>,
}

impl FileContent {
    fn from_source(path: &str, source: LoadedContent) -> Self {
        let LoadedContent {
            text,
            truncated,
            is_binary: _,
        } = source;
        let ends_with_newline = text.ends_with('\n');
        let text_lines: Vec<String> = text.lines().map(str::to_string).collect();
        let line_count = text_lines.len();
        let (highlight_lines, line_contexts) = if truncated {
            (None, vec![None; line_count])
        } else {
            match highlight::highlight_with_context(Path::new(path), &text) {
                Some((lines, contexts))
                    if lines.len() == line_count && contexts.len() == line_count =>
                {
                    (Some(lines), contexts)
                }
                Some((lines, _)) if lines.len() == line_count => {
                    (Some(lines), vec![None; line_count])
                }
                _ => (None, vec![None; line_count]),
            }
        };
        let mut lines = Vec::with_capacity(text_lines.len());
        for (idx, text_line) in text_lines.into_iter().enumerate() {
            let spans = highlight_lines
                .as_ref()
                .and_then(|hl| hl.get(idx))
                .and_then(|spans| {
                    let rendered: String = spans.iter().map(|span| span.content.as_str()).collect();
                    if rendered == text_line {
                        Some(spans.clone())
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| vec![TextSpan::new(text_line.clone(), Style::default())]);

            lines.push(LineContent {
                text: text_line,
                spans,
                context: line_contexts.get(idx).cloned().unwrap_or(None),
            });
        }

        Self {
            lines,
            ends_with_newline,
        }
    }

    fn line(&self, index: usize) -> Option<&LineContent> {
        self.lines.get(index)
    }

    fn context(&self, index: usize) -> Option<&str> {
        self.lines
            .get(index)
            .and_then(|line| line.context.as_deref())
    }

    fn as_text(&self) -> String {
        let mut text = self
            .lines
            .iter()
            .map(|line| line.text.as_str())
            .collect::<Vec<_>>()
            .join("\n");
        if self.ends_with_newline {
            text.push('\n');
        }
        text
    }
}

fn load_file_versions(
    entry: &FileEntry,
    focus: Focus,
    diff_spec: Option<&str>,
) -> Result<FileVersions> {
    let (old_source, new_source) = match diff_spec {
        Some(spec) => {
            let base_ref = git::diff_base_ref(spec);
            let target_ref = git::diff_target_ref(spec);
            let old = git::read_ref_file(&base_ref, &entry.path)?;
            let new = git::read_ref_file(&target_ref, &entry.path)?;
            (old, new)
        }
        None => {
            let old_source = match focus {
                Focus::Unstaged => {
                    if entry.code == '?' {
                        None
                    } else {
                        git::read_index_file(&entry.path)?
                    }
                }
                Focus::Staged => git::read_head_file(&entry.path)?,
                Focus::Changed => None,
            };

            let new_source = match focus {
                Focus::Unstaged => read_worktree_file(&entry.path)?,
                Focus::Staged => git::read_index_file(&entry.path)?,
                Focus::Changed => None,
            };
            (old_source, new_source)
        }
    };

    let old_truncated = old_source.as_ref().is_some_and(|content| content.truncated);
    let new_truncated = new_source.as_ref().is_some_and(|content| content.truncated);
    let old_binary = old_source.as_ref().is_some_and(|content| content.is_binary);
    let new_binary = new_source.as_ref().is_some_and(|content| content.is_binary);

    Ok(FileVersions {
        old: old_source.map(|content| FileContent::from_source(&entry.path, content)),
        new: new_source.map(|content| FileContent::from_source(&entry.path, content)),
        truncated: old_truncated || new_truncated,
        is_binary: old_binary || new_binary,
    })
}

fn read_worktree_file(path: &str) -> Result<Option<LoadedContent>> {
    match File::open(path) {
        Ok(file) => load_limited_from_file(file)
            .map(Some)
            .wrap_err_with(|| format!("failed to read {}", path)),
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(None),
        Err(err) => Err(err)
            .into_diagnostic()
            .wrap_err_with(|| format!("failed to read {}", path)),
    }
}

fn load_limited_from_file(file: File) -> Result<LoadedContent> {
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader
        .by_ref()
        .take(git::PREVIEW_BYTE_LIMIT as u64)
        .read_to_end(&mut buffer)
        .into_diagnostic()
        .wrap_err("failed to read file prefix")?;

    let truncated = if buffer.len() == git::PREVIEW_BYTE_LIMIT {
        let mut probe = [0u8; 1];
        loop {
            match reader.read(&mut probe) {
                Ok(0) => break false,
                Ok(_) => break true,
                Err(err) if err.kind() == ErrorKind::Interrupted => continue,
                Err(err) => {
                    return Err(err)
                        .into_diagnostic()
                        .wrap_err("failed to read file tail");
                }
            }
        }
    } else {
        false
    };

    let sample_size = buffer.len().min(8000);
    let is_binary = buffer[..sample_size].contains(&0);
    let text = String::from_utf8_lossy(&buffer).into_owned();
    Ok(LoadedContent {
        text,
        truncated,
        is_binary,
    })
}

fn build_diff_lines(versions: &FileVersions) -> Vec<DiffLine> {
    const CONTEXT_RADIUS: usize = 3;

    if versions.is_binary {
        let mut style = Style::fg(highlight::EVERFOREST_GREY1);
        style.dim = true;
        return vec![DiffLine::styled(
            "Binary file (not shown)".to_string(),
            style,
        )];
    }

    let old_text = versions
        .old
        .as_ref()
        .map(|content| content.as_text())
        .unwrap_or_default();
    let new_text = versions
        .new
        .as_ref()
        .map(|content| content.as_text())
        .unwrap_or_default();

    if old_text.is_empty() && new_text.is_empty() {
        return Vec::new();
    }

    let diff = TextDiff::configure()
        .algorithm(Algorithm::Patience)
        .diff_lines(&old_text, &new_text);

    if diff
        .ops()
        .iter()
        .all(|op| matches!(op.tag(), DiffTag::Equal))
    {
        return Vec::new();
    }

    let mut result = Vec::new();

    for group in diff.grouped_ops(CONTEXT_RADIUS) {
        if let Some(header) = build_hunk_header(&group, versions) {
            let mut style = Style::fg(highlight::EVERFOREST_GREY1);
            style.dim = true;
            result.push(DiffLine::styled(header, style));
        }

        for op in &group {
            for change in diff.iter_inline_changes(op) {
                let (line_text, segments) = collect_inline_segments(&change);
                let change_tag = change.tag();

                let prefix = match change_tag {
                    ChangeTag::Delete => '-',
                    ChangeTag::Insert => '+',
                    ChangeTag::Equal => ' ',
                };

                let old_number = change.old_index().map(|idx| idx + 1);
                let new_number = change.new_index().map(|idx| idx + 1);

                let base_spans = match change_tag {
                    ChangeTag::Delete => highlight_with_fallback(
                        versions.old.as_ref(),
                        old_number.unwrap_or(0),
                        &line_text,
                    ),
                    ChangeTag::Insert | ChangeTag::Equal => highlight_with_fallback(
                        versions.new.as_ref(),
                        new_number.unwrap_or(0),
                        &line_text,
                    ),
                };

                let merged_spans = merge_inline_segments(base_spans, &segments, change_tag);

                let mut diff_line = DiffLine::from_spans(with_prefix(prefix, merged_spans));
                if old_number.is_some() || new_number.is_some() {
                    diff_line = diff_line.with_line_numbers(old_number, new_number);
                }
                result.push(diff_line);

                if change.missing_newline() {
                    result.push(DiffLine::plain("\\ No newline at end of file"));
                }
            }
        }
    }

    result
}

fn collect_inline_segments(change: &InlineChange<'_, str>) -> (String, Vec<(bool, String)>) {
    let mut segments = Vec::new();
    let mut line = String::new();

    for (emphasized, value) in change.iter_strings_lossy() {
        let mut owned = value.into_owned();
        if owned.ends_with('\n') {
            owned.pop();
        }

        if owned.is_empty() && !segments.is_empty() {
            continue;
        }

        line.push_str(&owned);
        segments.push((emphasized, owned));
    }

    if segments.is_empty() {
        segments.push((false, String::new()));
    }

    (line, segments)
}

fn merge_inline_segments(
    base_spans: Vec<TextSpan>,
    segments: &[(bool, String)],
    change_tag: ChangeTag,
) -> Vec<TextSpan> {
    if segments.is_empty() {
        return base_spans;
    }

    if base_spans.is_empty() {
        return build_spans_from_segments(segments, change_tag);
    }

    let mut result = Vec::new();
    let mut seg_index = 0usize;
    let mut seg_offset = 0usize;

    for base in base_spans {
        let mut remaining = base.content.as_str();
        if remaining.is_empty() {
            result.push(base);
            continue;
        }

        while !remaining.is_empty() {
            if seg_index >= segments.len() {
                let style = base.style;
                result.push(TextSpan::new(remaining.to_string(), style));
                break;
            }

            let (emphasized, segment_text) = &segments[seg_index];
            let segment_remaining = &segment_text[seg_offset..];

            if segment_remaining.is_empty() {
                seg_index += 1;
                seg_offset = 0;
                continue;
            }

            let take_len = prefix_match_len(remaining, segment_remaining);
            if take_len == 0 {
                return build_spans_from_segments(segments, change_tag);
            }

            let (take, rest) = remaining.split_at(take_len);
            remaining = rest;

            let mut style = base.style;
            if *emphasized {
                apply_inline_emphasis(&mut style, change_tag);
            }

            result.push(TextSpan::new(take.to_string(), style));

            seg_offset += take_len;
            if seg_offset >= segment_text.len() {
                seg_index += 1;
                seg_offset = 0;
            }
        }
    }

    while seg_index < segments.len() {
        let (emphasized, segment_text) = &segments[seg_index];
        let mut style = Style::default();
        if *emphasized {
            apply_inline_emphasis(&mut style, change_tag);
        }
        if !segment_text.is_empty() {
            result.push(TextSpan::new(segment_text.clone(), style));
        }
        seg_index += 1;
    }

    if result.is_empty() {
        vec![TextSpan::new(String::new(), Style::default())]
    } else {
        result
    }
}

fn build_spans_from_segments(segments: &[(bool, String)], change_tag: ChangeTag) -> Vec<TextSpan> {
    if segments.is_empty() {
        return vec![TextSpan::new(String::new(), Style::default())];
    }

    segments
        .iter()
        .map(|(emphasized, text)| {
            let mut style = Style::default();
            if *emphasized {
                apply_inline_emphasis(&mut style, change_tag);
            }
            TextSpan::new(text.clone(), style)
        })
        .collect()
}

fn inline_accent_color(change_tag: ChangeTag) -> Option<Color> {
    match change_tag {
        ChangeTag::Insert => Some(highlight::EVERFOREST_BG_GREEN_ACCENT),
        ChangeTag::Delete => Some(highlight::EVERFOREST_BG_RED_ACCENT),
        ChangeTag::Equal => None,
    }
}

fn apply_inline_emphasis(style: &mut Style, change_tag: ChangeTag) {
    style.bold = true;
    if let Some(bg) = inline_accent_color(change_tag) {
        style.bg = Some(bg);
    }
}

fn prefix_match_len(a: &str, b: &str) -> usize {
    let mut len = 0;
    let mut a_iter = a.chars();
    let mut b_iter = b.chars();

    loop {
        match (a_iter.next(), b_iter.next()) {
            (Some(ac), Some(bc)) if ac == bc => {
                len += ac.len_utf8();
            }
            _ => break,
        }
    }

    len
}

fn build_hunk_header(group: &[DiffOp], versions: &FileVersions) -> Option<String> {
    if group.is_empty() {
        return None;
    }

    let old_start = group
        .iter()
        .map(|op| op.old_range().start)
        .min()
        .unwrap_or(0);
    let old_end = group
        .iter()
        .map(|op| op.old_range().end)
        .max()
        .unwrap_or(old_start);
    let new_start = group
        .iter()
        .map(|op| op.new_range().start)
        .min()
        .unwrap_or(0);
    let new_end = group
        .iter()
        .map(|op| op.new_range().end)
        .max()
        .unwrap_or(new_start);

    let old_count = old_end.saturating_sub(old_start);
    let new_count = new_end.saturating_sub(new_start);

    let old_range = format_hunk_range(old_start, old_count);
    let new_range = format_hunk_range(new_start, new_count);

    let mut header = format!("@@ -{} +{} @@", old_range, new_range);
    if let Some(context) = hunk_context(group, versions) {
        header.push(' ');
        header.push_str(&context);
    }

    Some(header)
}

fn format_hunk_range(start: usize, count: usize) -> String {
    if count == 0 {
        format!("{},0", start)
    } else if count == 1 {
        (start + 1).to_string()
    } else {
        format!("{},{}", start + 1, count)
    }
}

fn hunk_context(group: &[DiffOp], versions: &FileVersions) -> Option<String> {
    if versions.truncated {
        return None;
    }

    if let Some(new_content) = &versions.new
        && let Some(context) = context_from_content(new_content, group, |op| op.new_range())
    {
        return Some(context);
    }

    if let Some(old_content) = &versions.old
        && let Some(context) = context_from_content(old_content, group, |op| op.old_range())
    {
        return Some(context);
    }

    None
}

fn context_from_content<F>(
    content: &FileContent,
    group: &[DiffOp],
    mut range_fn: F,
) -> Option<String>
where
    F: FnMut(&DiffOp) -> Range<usize>,
{
    let mut changed_ranges = Vec::new();
    let mut all_ranges = Vec::new();

    for op in group {
        let range = range_fn(op);
        if range.is_empty() {
            continue;
        }
        if !matches!(op.tag(), DiffTag::Equal) {
            changed_ranges.push(range.clone());
        }
        all_ranges.push(range);
    }

    for range in changed_ranges.iter().chain(all_ranges.iter()) {
        if let Some(context) = context_in_range(content, range) {
            return Some(context);
        }
    }

    None
}

fn context_in_range(content: &FileContent, range: &Range<usize>) -> Option<String> {
    let mut best: Option<(usize, usize, String)> = None;

    for idx in range.clone() {
        if let Some(context) = content.context(idx) {
            if context.is_empty() {
                continue;
            }

            let segments = context.split(" -> ").count();
            let length = context.len();

            match &mut best {
                Some((best_segments, best_length, best_value)) => {
                    if segments > *best_segments
                        || (segments == *best_segments && length > *best_length)
                    {
                        *best_segments = segments;
                        *best_length = length;
                        *best_value = context.to_string();
                    }
                }
                None => {
                    best = Some((segments, length, context.to_string()));
                }
            }
        }
    }

    best.map(|(_, _, value)| value)
}

fn highlight_with_fallback(
    content: Option<&FileContent>,
    line_number_one_based: usize,
    fallback: &str,
) -> Vec<TextSpan> {
    if line_number_one_based == 0 {
        return vec![TextSpan::new(fallback.to_string(), Style::default())];
    }

    let index = line_number_one_based - 1;
    if let Some(file) = content
        && let Some(line) = file.line(index)
        && line.text == fallback
    {
        return line.spans.clone();
    }

    vec![TextSpan::new(fallback.to_string(), Style::default())]
}

fn with_prefix(prefix: char, mut spans: Vec<TextSpan>) -> Vec<TextSpan> {
    let mut style = Style::default();
    match prefix {
        '+' => {
            style.fg = Some(highlight::EVERFOREST_GREEN);
            style.bg = Some(highlight::EVERFOREST_BG_GREEN);
            for span in &mut spans {
                span.style.bg = Some(highlight::EVERFOREST_BG_GREEN);
            }
        }
        '-' => {
            style.fg = Some(highlight::EVERFOREST_RED);
            style.bg = Some(highlight::EVERFOREST_BG_RED);
            for span in &mut spans {
                span.style.bg = Some(highlight::EVERFOREST_BG_RED);
            }
        }
        ' ' => {
            style.dim = true;
            for span in &mut spans {
                span.style.dim = true;
            }
        }
        _ => {}
    }

    let mut result = Vec::with_capacity(spans.len() + 1);
    result.push(TextSpan::new(prefix.to_string(), style));
    result.append(&mut spans);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use chatui::{Size, TreeNodeKind, buffer::DoubleBuffer, palette::Palette, render::Renderer};
    use taffy::{AvailableSpace, Dimension, compute_root_layout};

    fn render_tree_lines(
        tree: &TreeState<FileNodeId>,
        is_active: bool,
        width: u16,
        height: u16,
    ) -> Vec<String> {
        let mut node = render_file_tree("test-tree", tree, is_active, |tree_msg| Msg::Section {
            focus: Focus::Unstaged,
            msg: SectionMsg::Tree(tree_msg),
        })
        .with_width(Dimension::percent(1.0))
        .with_height(Dimension::percent(1.0));

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(width as f32),
                height: AvailableSpace::Definite(height as f32),
            },
        );
        chatui::dom::rounding::round_layout(&mut node);

        let mut buffer = DoubleBuffer::new(width as usize, height as usize);
        let palette = Palette::default();
        Renderer::new(&mut buffer, &palette)
            .render(&node, Size::new(width, height))
            .expect("render file tree");

        buffer
            .to_string()
            .lines()
            .map(|line| line.trim_end().to_string())
            .filter(|line| !line.trim().is_empty())
            .collect()
    }

    #[test]
    fn tree_label_truncates_to_file_name() {
        let entry = FileEntry::new("src/lib.rs".into(), 'M', "M src/lib.rs".into());
        assert_eq!(entry.tree_label(), "lib.rs");
    }

    #[test]
    fn tree_label_handles_rename() {
        let entry = FileEntry::new(
            "src/new.rs".into(),
            'R',
            "R src/old.rs -> src/new.rs".into(),
        );
        assert_eq!(entry.tree_label(), "old.rs -> new.rs");
    }

    #[test]
    fn build_file_tree_groups_into_directories() {
        let entries = vec![
            FileEntry::new("src/lib.rs".into(), 'M', "M src/lib.rs".into()),
            FileEntry::new("src/main.rs".into(), 'M', "M src/main.rs".into()),
            FileEntry::new("README.md".into(), 'A', "A README.md".into()),
        ];

        let nodes = build_file_tree(&entries);
        assert_eq!(nodes.len(), 2);

        let src_node = nodes
            .iter()
            .find(|node| matches!(node.id, FileNodeId::Dir(ref path) if path == "src"))
            .expect("src directory node");
        assert_eq!(src_node.children.len(), 2);
        assert!(
            src_node
                .label
                .first()
                .map(|span| span.content.starts_with('M'))
                .unwrap_or(false)
        );

        let readme_node = nodes
            .iter()
            .find(|node| matches!(node.id, FileNodeId::File(ref path) if path == "README.md"))
            .expect("readme node");
        assert!(matches!(readme_node.kind, TreeNodeKind::Leaf));
        assert_eq!(readme_node.label[0].content.trim(), "A README.md");
    }

    #[test]
    fn directory_with_mixed_statuses_uses_aggregate_marker() {
        let entries = vec![
            FileEntry::new("src/lib.rs".into(), 'M', "M src/lib.rs".into()),
            FileEntry::new("src/new.rs".into(), '?', "? src/new.rs".into()),
        ];

        let nodes = build_file_tree(&entries);
        let src_node = nodes
            .iter()
            .find(|node| matches!(node.id, FileNodeId::Dir(ref path) if path == "src"))
            .expect("src directory node");
        assert!(
            src_node
                .label
                .first()
                .map(|span| span.content.starts_with('*'))
                .unwrap_or(false)
        );
    }

    #[test]
    fn collapses_single_child_directories_into_path_segments() {
        let entries = vec![FileEntry::new(
            "src/dom/mod.rs".into(),
            'M',
            "M src/dom/mod.rs".into(),
        )];

        let nodes = build_file_tree(&entries);
        assert_eq!(nodes.len(), 1);

        let directory = nodes.first().expect("directory node");
        match &directory.id {
            FileNodeId::Dir(path) => assert_eq!(path, "src/dom"),
            other => panic!("expected collapsed directory id, got {other:?}"),
        }

        let dir_label = directory
            .label
            .first()
            .expect("directory label span")
            .content
            .clone();
        assert!(
            dir_label.starts_with('M'),
            "expected status marker, got {dir_label:?}"
        );
        assert!(
            dir_label.contains("src/dom/"),
            "expected directory label to include collapsed path, got {dir_label:?}"
        );

        assert_eq!(directory.children.len(), 1);
        let file = directory.children.first().expect("file node");
        match &file.id {
            FileNodeId::File(path) => assert_eq!(path, "src/dom/mod.rs"),
            other => panic!("expected file id, got {other:?}"),
        }
        let file_label = file.label.first().expect("file label span").content.clone();
        assert!(
            file_label.starts_with('M'),
            "expected status marker, got {file_label:?}"
        );
        assert!(
            file_label.ends_with("mod.rs"),
            "expected file label to show file name, got {file_label:?}"
        );
    }

    #[test]
    fn build_diff_lines_produces_line_numbers() {
        let old_content = FileContent::from_source(
            "file.txt",
            LoadedContent {
                text: "foo\n".to_string(),
                truncated: false,
                is_binary: false,
            },
        );
        let new_content = FileContent::from_source(
            "file.txt",
            LoadedContent {
                text: "bar\n".to_string(),
                truncated: false,
                is_binary: false,
            },
        );
        let versions = FileVersions {
            old: Some(old_content),
            new: Some(new_content),
            truncated: false,
            is_binary: false,
        };

        let lines = build_diff_lines(&versions);

        assert!(matches!(lines.get(0), Some(line) if line.line_numbers.is_none()));

        let removed = lines.get(1).expect("removed line");
        let removed_numbers = removed.line_numbers.as_ref().expect("removed line numbers");
        assert_eq!(removed_numbers.old, Some(1));
        assert!(removed_numbers.new.is_none());

        let added = lines.get(2).expect("added line");
        let added_numbers = added.line_numbers.as_ref().expect("added line numbers");
        assert!(added_numbers.old.is_none());
        assert_eq!(added_numbers.new, Some(1));

        let mut node = render_diff_lines(&lines, true)
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(20.0),
                height: AvailableSpace::Definite(3.0),
            },
        );
        chatui::dom::rounding::round_layout(&mut node);

        let mut buffer = DoubleBuffer::new(20, 3);
        let palette = Palette::default();
        Renderer::new(&mut buffer, &palette)
            .render(&node, Size::new(20, 3))
            .expect("render diff lines with gutter");

        let back = buffer.back_buffer();
        let removed_row: String = back[1].iter().map(|cell| cell.ch).collect();
        assert!(
            removed_row.starts_with("1   | -foo"),
            "expected removed row to show gutter, got {removed_row:?}"
        );

        let added_row: String = back[2].iter().map(|cell| cell.ch).collect();
        assert!(
            added_row.starts_with("  1 | +bar"),
            "expected added row to show gutter, got {added_row:?}"
        );
    }

    #[test]
    fn build_hunk_header_includes_context_path() {
        if std::env::var(runtime::HELIX_RUNTIME_ENV).is_err() {
            eprintln!(
                "Skipping build_hunk_header_includes_context_path because {} is not set",
                runtime::HELIX_RUNTIME_ENV
            );
            return;
        }

        let old_content = FileContent::from_source(
            "example.rs",
            LoadedContent {
                text: "impl Example {\n    fn foo() {}\n}\n".to_string(),
                truncated: false,
                is_binary: false,
            },
        );
        let new_content = FileContent::from_source(
            "example.rs",
            LoadedContent {
                text: "impl Example {\n    fn foo() {\n        println!(\"hi\");\n    }\n}\n"
                    .to_string(),
                truncated: false,
                is_binary: false,
            },
        );
        let versions = FileVersions {
            old: Some(old_content),
            new: Some(new_content),
            truncated: false,
            is_binary: false,
        };

        let lines = build_diff_lines(&versions);
        let header = lines
            .first()
            .expect("expected header line")
            .spans
            .first()
            .expect("expected header span")
            .content
            .clone();

        assert!(
            header.contains("impl Example -> fn foo"),
            "expected header to include context, got {header:?}"
        );
    }

    #[test]
    fn inline_insert_segments_use_accent_background() {
        let base_spans = vec![TextSpan::new("abc", Style::default())];
        let segments = vec![(true, "ab".to_string()), (false, "c".to_string())];

        let merged = merge_inline_segments(base_spans, &segments, ChangeTag::Insert);
        let accent_span = merged
            .iter()
            .find(|span| span.content == "ab")
            .expect("accented span");

        assert!(accent_span.style.bold, "accent should remain bold");
        assert_eq!(
            accent_span.style.bg,
            Some(highlight::EVERFOREST_BG_GREEN_ACCENT)
        );
    }

    #[test]
    fn inline_delete_segments_without_base_use_accent_background() {
        let base_spans = Vec::new();
        let segments = vec![(true, "xyz".to_string())];

        let merged = merge_inline_segments(base_spans, &segments, ChangeTag::Delete);
        let accent_span = merged
            .first()
            .expect("at least one generated span for emphasized diff segment");

        assert!(accent_span.style.bold, "accent should remain bold");
        assert_eq!(
            accent_span.style.bg,
            Some(highlight::EVERFOREST_BG_RED_ACCENT)
        );
    }

    #[test]
    fn diff_pane_renders_horizontal_scrollbar() {
        let mut model = Model::default();
        model.diff_scroll = ScrollState::both();
        let content = "0123456789abcdefghijklmnopqrstuvwxyz";
        model.diff_lines = vec![DiffLine::plain(content)];
        let content_width = content.len() as u16;
        model.diff_scroll.update(ScrollMsg::Resize {
            viewport: Size::new(18, 1),
            content: Size::new(content_width, 1),
        });
        model
            .diff_scroll
            .set_offset_for(ScrollAxis::Horizontal, 8.0);

        let mut pane = render_diff_pane(&model)
            .with_width(Dimension::percent(1.0))
            .with_height(Dimension::percent(1.0));
        compute_root_layout(
            &mut pane,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(24.0),
                height: AvailableSpace::Definite(6.0),
            },
        );
        chatui::dom::rounding::round_layout(&mut pane);

        let mut buffer = DoubleBuffer::new(24, 6);
        let palette = Palette::default();
        Renderer::new(&mut buffer, &palette)
            .render(&pane, Size::new(24, 6))
            .expect("render diff pane");

        let back = buffer.back_buffer();
        let has_horizontal_thumb = back
            .iter()
            .any(|row| row.iter().any(|cell| matches!(cell.ch, '█' | '▀')));
        assert!(
            has_horizontal_thumb,
            "expected horizontal scrollbar thumb to be rendered"
        );

        let mut scrolled_row = None;
        for (y, row) in back.iter().enumerate() {
            for x in 0..row.len().saturating_sub(1) {
                if row[x].ch == '│' && row[x + 1].ch == '8' {
                    scrolled_row = Some((y, x));
                    break;
                }
            }
            if scrolled_row.is_some() {
                break;
            }
        }

        let (row_idx, col_idx) =
            scrolled_row.expect("expected diff content with horizontal offset");
        let row = &back[row_idx];
        assert_ne!(
            row.get(col_idx + 1).map(|cell| cell.ch),
            Some('0'),
            "expected diff content to start after horizontal offset"
        );
    }

    #[test]
    fn diff_render_leaves_tab_characters_in_lines() {
        let spans = vec![TextSpan::new(
            "\tfinancialComponentStructure := value".to_string(),
            Style::default(),
        )];
        let line = DiffLine::from_spans(with_prefix('+', spans));
        let mut node = render_diff_lines(&[line], false)
            .with_width(Dimension::length(40.0))
            .with_height(Dimension::length(1.0));
        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(40.0),
                height: AvailableSpace::Definite(3.0),
            },
        );
        chatui::dom::rounding::round_layout(&mut node);

        let mut buffer = DoubleBuffer::new(40, 3);
        let palette = Palette::default();
        Renderer::new(&mut buffer, &palette)
            .render(&node, Size::new(40, 3))
            .expect("render diff lines with tabs");

        let rendered_row: String = buffer.back_buffer()[0].iter().map(|cell| cell.ch).collect();
        assert!(
            !rendered_row.contains('\t'),
            "expected diff rendering to expand tabs, row contained tab: {rendered_row:?}"
        );
    }

    #[test]
    fn unstaged_tree_renders_untracked_directory_case() {
        let entries = vec![
            FileEntry::new(
                "src/components/scroll.rs".into(),
                'M',
                "M src/components/scroll.rs".into(),
            ),
            FileEntry::new(
                "src/components/scroll/tests.rs".into(),
                '?',
                "? src/components/scroll/tests.rs".into(),
            ),
        ];

        let nodes = build_file_tree(&entries);
        let mut tree = TreeState::new();
        tree.set_items(nodes);
        tree.expand_all();
        tree.ensure_selected();

        let lines = render_tree_lines(&tree, true, 40, 6);
        assert_eq!(
            lines,
            vec![
                "v * src/components/".to_string(),
                "    ? scroll/tests.rs".to_string(),
                "    M scroll.rs".to_string(),
            ]
        );
    }

    #[test]
    fn binary_files_show_message_instead_of_diff() {
        let old_content = FileContent::from_source(
            "image.png",
            LoadedContent {
                text: String::from_utf8_lossy(&[0x89, 0x50, 0x4E, 0x47, 0x00]).to_string(),
                truncated: false,
                is_binary: true,
            },
        );
        let new_content = FileContent::from_source(
            "image.png",
            LoadedContent {
                text: String::from_utf8_lossy(&[0x89, 0x50, 0x4E, 0x47, 0x01]).to_string(),
                truncated: false,
                is_binary: true,
            },
        );
        let versions = FileVersions {
            old: Some(old_content),
            new: Some(new_content),
            truncated: false,
            is_binary: true,
        };

        let lines = build_diff_lines(&versions);

        assert_eq!(lines.len(), 1);
        let line_text = &lines[0].spans[0].content;
        assert_eq!(line_text, "Binary file (not shown)");
    }
}

fn init_tracing() -> miette::Result<()> {
    use std::fs::File;
    use std::path::PathBuf;
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

    let log_path = match std::env::var("GIT_STATUS_LOG") {
        Ok(path) => PathBuf::from(path),
        Err(_) => return Ok(()),
    };

    let writer = tracing_subscriber::fmt::writer::BoxMakeWriter::new({
        let log_path = log_path.clone();
        move || {
            File::options()
                .create(true)
                .append(true)
                .open(&log_path)
                .expect("log file should remain writable")
        }
    });

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_ansi(false)
                .with_writer(writer),
        )
        .with(EnvFilter::from_default_env())
        .try_init()
        .map_err(|error| miette::miette!("{}", error))?;

    Ok(())
}
