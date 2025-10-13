mod git;
mod highlight;

use std::collections::btree_map::Entry;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufReader, ErrorKind, Read};
use std::path::Path;

use chatui::components::scroll::{
    ScrollAxis, ScrollMsg, ScrollState, ScrollTarget, scrollable_content,
};
use chatui::dom::{Color, Node, Renderable, TextSpan, renderable};
use chatui::event::{Event, Key, KeyCode};
use chatui::render::RenderContext;
use chatui::{
    InputMsg, InputState, InputStyle, Program, Style, Transition, TreeNodeKind, block_with_title,
    column, default_input_keybindings, input, modal, rich_text, row, text,
};
use chatui::{TreeMsg, TreeNode, TreeState, TreeStyle, tree_view};
use color_eyre::eyre::{Context, Result, eyre};
use taffy::Dimension;
use taffy::prelude::{FromLength, TaffyZero};
use taffy::style::FlexWrap;
use tracing::info;
use tracing_subscriber::EnvFilter;

use crate::git::{FileEntry, LoadedContent};
mod shortcuts {
    use super::{Key, KeyCode, Model, Msg};

    #[derive(Clone)]
    struct Shortcut {
        label: &'static str,
        description: &'static str,
        show_in_bar: bool,
        binding: Option<Binding>,
        msg: Option<Msg>,
    }

    #[derive(Clone, Copy)]
    struct Binding {
        code: KeyCode,
        ctrl: ModifierRequirement,
        alt: ModifierRequirement,
        shift: ModifierRequirement,
    }

    #[derive(Clone, Copy)]
    enum ModifierRequirement {
        Any,
        Enabled,
        Disabled,
    }

    impl ModifierRequirement {
        const fn matches(self, value: bool) -> bool {
            match self {
                Self::Any => true,
                Self::Enabled => value,
                Self::Disabled => !value,
            }
        }
    }

    impl Binding {
        const fn new(
            code: KeyCode,
            ctrl: ModifierRequirement,
            alt: ModifierRequirement,
            shift: ModifierRequirement,
        ) -> Self {
            Self {
                code,
                ctrl,
                alt,
                shift,
            }
        }

        fn matches_key(self, key: Key) -> bool {
            let code_matches = match (self.code, key.code) {
                (KeyCode::Char(expected), KeyCode::Char(actual)) => {
                    if key.ctrl {
                        actual.eq_ignore_ascii_case(&expected)
                    } else {
                        actual == expected
                    }
                }
                _ => self.code == key.code,
            };

            code_matches
                && self.ctrl.matches(key.ctrl)
                && self.alt.matches(key.alt)
                && self.shift.matches(key.shift)
        }
    }

    pub(super) struct ShortcutDisplay {
        pub label: &'static str,
        pub description: &'static str,
        pub msg: Option<Msg>,
    }

    pub(super) fn message_for_key(model: &Model, key: Key) -> Option<Msg> {
        context_shortcuts(model)
            .iter()
            .chain(GLOBAL_SHORTCUTS.iter())
            .filter_map(|shortcut| shortcut.binding.map(|binding| (shortcut, binding)))
            .find(|(_, binding)| binding.matches_key(key))
            .and_then(|(shortcut, _)| shortcut.msg.as_ref().cloned())
    }

    pub(super) fn display_shortcuts(model: &Model) -> Vec<ShortcutDisplay> {
        context_shortcuts(model)
            .iter()
            .chain(GLOBAL_SHORTCUTS.iter())
            .filter(|shortcut| shortcut.show_in_bar)
            .map(|shortcut| ShortcutDisplay {
                label: shortcut.label,
                description: shortcut.description,
                msg: shortcut.msg.as_ref().cloned(),
            })
            .collect()
    }

    fn context_shortcuts(model: &Model) -> &'static [Shortcut] {
        if model.commit_modal.is_some() {
            COMMIT_MODAL_SHORTCUTS
        } else {
            NORMAL_SHORTCUTS
        }
    }

    const GLOBAL_SHORTCUTS: &[Shortcut] = &[
        Shortcut {
            label: "Ctrl-C",
            description: "Quit immediately",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('c'),
                ModifierRequirement::Enabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::Quit),
        },
        Shortcut {
            label: "?",
            description: "Toggle shortcuts",
            show_in_bar: false,
            binding: Some(Binding::new(
                KeyCode::Char('?'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ToggleShortcutsHelp),
        },
    ];

    const NORMAL_SHORTCUTS: &[Shortcut] = &[
        Shortcut {
            label: "Esc",
            description: "Quit",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Esc,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::Quit),
        },
        Shortcut {
            label: "q",
            description: "Quit",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('q'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::Quit),
        },
        Shortcut {
            label: "Enter",
            description: "Stage / Unstage",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Enter,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ToggleStage),
        },
        Shortcut {
            label: "Double-click",
            description: "Stage / Unstage",
            show_in_bar: true,
            binding: None,
            msg: None,
        },
        Shortcut {
            label: "Tab",
            description: "Toggle focus",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Tab,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ToggleFocus),
        },
        Shortcut {
            label: "Up",
            description: "Move selection up",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Up,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::MoveSelectionUp),
        },
        Shortcut {
            label: "Down",
            description: "Move selection down",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Down,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::MoveSelectionDown),
        },
        Shortcut {
            label: "⇠",
            description: "Collapse directory",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Left,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::CollapseNode),
        },
        Shortcut {
            label: "",
            description: "Expand directory",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Right,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ExpandNode),
        },
        Shortcut {
            label: "j",
            description: "Scroll files",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('j'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ScrollFiles(1)),
        },
        Shortcut {
            label: "k",
            description: "Scroll files",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('k'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ScrollFiles(-1)),
        },
        Shortcut {
            label: "J",
            description: "Scroll diff",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('J'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ScrollDiff(1)),
        },
        Shortcut {
            label: "K",
            description: "Scroll diff",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('K'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ScrollDiff(-1)),
        },
        Shortcut {
            label: "l",
            description: "Scroll diff right",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('l'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ScrollDiffHorizontal(4)),
        },
        Shortcut {
            label: "h",
            description: "Scroll diff left",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('h'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ScrollDiffHorizontal(-4)),
        },
        Shortcut {
            label: "n",
            description: "Toggle line numbers",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('n'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::ToggleDiffLineNumbers),
        },
        Shortcut {
            label: "r",
            description: "Refresh status",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('r'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::RefreshStatus),
        },
        Shortcut {
            label: "c",
            description: "Open commit",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('c'),
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::OpenCommitModal),
        },
        Shortcut {
            label: "Ctrl-D",
            description: "Delete changes",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('d'),
                ModifierRequirement::Enabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::OpenDeleteModal),
        },
    ];

    const COMMIT_MODAL_SHORTCUTS: &[Shortcut] = &[
        Shortcut {
            label: "Ctrl-D",
            description: "Commit",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Char('d'),
                ModifierRequirement::Enabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::SubmitCommit),
        },
        Shortcut {
            label: "Esc",
            description: "Cancel",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Esc,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::CancelCommit),
        },
        Shortcut {
            label: "Backspace",
            description: "Delete character",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Backspace,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: None,
        },
    ];
}

fn main() -> Result<()> {
    color_eyre::install()?;
    init_tracing()?;

    let mut program = Program::new(Model::new(), update, view).map_event(map_event);

    if let Err(error) = smol::block_on(program.run_async()) {
        eprintln!("Program exited with error: {:?}", error);
    }

    Ok(())
}

fn map_event(event: Event) -> Option<Msg> {
    match event {
        Event::Key(key) => Some(Msg::KeyPressed(key)),
        Event::FocusGained => Some(Msg::RefreshStatus),
        _ => None,
    }
}

fn update(model: &mut Model, msg: Msg) -> Transition {
    info!("update {msg:?}");
    match msg {
        Msg::KeyPressed(key) => handle_key(model, key),
        Msg::Quit => Transition::Quit,
        Msg::ToggleStage => {
            model.toggle_stage_selected();
            Transition::Continue
        }
        Msg::ToggleFocus => {
            match model.focus {
                Focus::Unstaged => model.focus_staged(),
                Focus::Staged => model.focus_unstaged(),
            }
            model.update_diff();
            Transition::Continue
        }
        Msg::MoveSelectionUp => {
            model.move_selection_up();
            model.update_diff();
            Transition::Continue
        }
        Msg::MoveSelectionDown => {
            model.move_selection_down();
            model.update_diff();
            Transition::Continue
        }
        Msg::CollapseNode => {
            if model.collapse_selected() {
                model.update_diff();
            }
            Transition::Continue
        }
        Msg::ExpandNode => {
            if model.expand_selected() {
                model.update_diff();
            }
            Transition::Continue
        }
        Msg::ScrollFiles(delta) => {
            model.scroll_files_in_focus(delta);
            Transition::Continue
        }
        Msg::ScrollDiff(delta) => {
            model.scroll_diff(delta);
            Transition::Continue
        }
        Msg::ScrollDiffHorizontal(delta) => {
            model.scroll_diff_horizontal(delta);
            Transition::Continue
        }
        Msg::RefreshStatus => {
            if let Err(err) = model.refresh_status_preserving_diff_scroll() {
                model.set_error(err);
            }
            Transition::Continue
        }
        Msg::OpenCommitModal => {
            model.open_commit_modal();
            Transition::Continue
        }
        Msg::ToggleShortcutsHelp => {
            model.toggle_shortcuts_help();
            Transition::Continue
        }
        Msg::ToggleDiffLineNumbers => {
            model.toggle_diff_line_numbers();
            Transition::Continue
        }
        Msg::SubmitCommit => model.submit_commit(),
        Msg::CancelCommit => {
            model.close_commit_modal();
            Transition::Continue
        }
        Msg::CommitInput(input_msg) => {
            if let Some(modal) = model.commit_modal.as_mut() {
                modal.update(input_msg);
            }
            Transition::Continue
        }
        Msg::OpenDeleteModal => {
            model.open_delete_modal();
            Transition::Continue
        }
        Msg::ConfirmDelete => model.confirm_delete(),
        Msg::CancelDelete => {
            model.close_delete_modal();
            Transition::Continue
        }
        Msg::Staged(msg) => handle_section_msg(model, Focus::Staged, msg),
        Msg::Unstaged(msg) => handle_section_msg(model, Focus::Unstaged, msg),
        Msg::DiffScroll(scroll_msg) => {
            model.diff_scroll.update(scroll_msg);
            Transition::Continue
        }
        Msg::UnstagedScroll(scroll_msg) => {
            model.update_section_scroll(Focus::Unstaged, scroll_msg);
            Transition::Continue
        }
        Msg::StagedScroll(scroll_msg) => {
            model.update_section_scroll(Focus::Staged, scroll_msg);
            Transition::Continue
        }
    }
}

fn handle_section_msg(model: &mut Model, focus: Focus, msg: SectionMsg) -> Transition {
    match msg {
        SectionMsg::Tree(tree_msg) => model.handle_tree_message(focus, tree_msg),
    }
}

fn handle_key(model: &mut Model, key: Key) -> Transition {
    // If the delete modal is active, it consumes keys like Enter/Esc/Y/N.
    if model.delete_modal.is_some() {
        return handle_delete_modal_key(model, key);
    }

    // Global/context shortcuts (including commit modal actions) take precedence
    // over raw character input handling inside modals.
    if let Some(msg) = shortcuts::message_for_key(model, key) {
        return update(model, msg);
    }

    // When commit modal is open, feed non-modified character input to it.
    if model.commit_modal.is_some() {
        return handle_commit_modal_key(model, key);
    }

    Transition::Continue
}

fn handle_commit_modal_key(model: &mut Model, key: Key) -> Transition {
    if let Some(msg) = model
        .commit_modal
        .as_ref()
        .and_then(|modal| default_input_keybindings(&modal.input, key, Msg::CommitInput))
    {
        return update(model, msg);
    }

    Transition::Continue
}

fn handle_delete_modal_key(model: &mut Model, key: Key) -> Transition {
    match key.code {
        KeyCode::Enter => update(model, Msg::ConfirmDelete),
        KeyCode::Esc => update(model, Msg::CancelDelete),
        KeyCode::Char('y') | KeyCode::Char('Y') => update(model, Msg::ConfirmDelete),
        KeyCode::Char('n') | KeyCode::Char('N') => update(model, Msg::CancelDelete),
        _ => Transition::Continue,
    }
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

    let mut header_spans = vec![TextSpan::new("Git Status", title_style())];
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

    let branch_label = model
        .current_branch
        .as_deref()
        .filter(|branch| !branch.is_empty())
        .unwrap_or("(unknown)");
    let branch_spans = vec![
        TextSpan::new("branch: ", Style::dim()),
        TextSpan::new(branch_label, branch_name_style()),
    ];
    let branch_node = rich_text::<Msg>(branch_spans).with_id("header-branch");

    let header = row(vec![header_left, branch_node])
        .with_width(Dimension::percent(1.))
        .with_id("header");

    let base = column(vec![header, layout, render_shortcuts_bar(model)])
        .with_fill()
        .with_id("root");

    // Stack any active modal dialogs on top of the base UI.
    let mut layered = base;
    if let Some(modal) = &model.commit_modal {
        layered = column(vec![layered, render_commit_modal(modal)])
            .with_fill()
            .with_id("root-with-commit-modal");
    }
    if let Some(modal) = &model.delete_modal {
        layered = column(vec![layered, render_delete_modal(modal)])
            .with_fill()
            .with_id("root-with-delete-modal");
    }

    layered
}

fn render_left_pane(model: &Model) -> Node<Msg> {
    let unstaged_list = render_file_tree(
        Model::UNSTAGED_ITEM_ID,
        &model.unstaged_tree,
        model.focus == Focus::Unstaged,
        |tree_msg| Msg::Unstaged(SectionMsg::Tree(tree_msg)),
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO);

    let unstaged = block_with_title(
        "Unstaged Changes",
        vec![
            scrollable_content(
                "unstaged-section-content",
                &model.unstaged_scroll,
                1,
                Msg::UnstagedScroll,
                unstaged_list,
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

    let staged_list = render_file_tree(
        Model::STAGED_ITEM_ID,
        &model.staged_tree,
        model.focus == Focus::Staged,
        |tree_msg| Msg::Staged(SectionMsg::Tree(tree_msg)),
    )
    .with_min_height(Dimension::ZERO)
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO);

    let staged = block_with_title(
        "Staged Changes",
        vec![
            scrollable_content(
                "staged-section-content",
                &model.staged_scroll,
                1,
                Msg::StagedScroll,
                staged_list,
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
    let diff_title = match model.current_entry() {
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
    let content = scrollable_content(
        "diff-pane-content",
        &model.diff_scroll,
        3,
        Msg::DiffScroll,
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
            .on_click(|| Msg::ToggleShortcutsHelp)
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

    renderable(DiffLeaf::new(lines.to_vec(), show_line_numbers))
        .with_flex_shrink(0.)
        .with_overflow_y(taffy::Overflow::Visible)
        .with_overflow_x(taffy::Overflow::Visible)
        .with_id("diff_lines")
}

#[derive(Clone, Debug)]
struct DiffLeaf {
    lines: Vec<DiffLine>,
    gutter: Option<GutterConfig>,
}

impl DiffLeaf {
    fn new(lines: Vec<DiffLine>, show_line_numbers: bool) -> Self {
        let gutter = if show_line_numbers {
            GutterConfig::from_lines(&lines)
        } else {
            None
        };

        Self { lines, gutter }
    }
}

impl Renderable for DiffLeaf {
    fn eq(&self, other: &dyn Renderable) -> bool {
        other
            .as_any()
            .downcast_ref::<Self>()
            .map(|o| o.lines == self.lines && o.gutter == self.gutter)
            .unwrap_or(false)
    }

    fn measure(
        &self,
        _style: &taffy::Style,
        _known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
    ) -> taffy::Size<f32> {
        let height = self.lines.len() as f32;
        let gutter_width = self
            .gutter
            .as_ref()
            .map(|gutter| gutter.total_width())
            .unwrap_or_default();
        let max_width = self
            .lines
            .iter()
            .map(|l| gutter_width + l.spans.iter().map(|s| s.content.len()).sum::<usize>())
            .max()
            .unwrap_or(0) as f32;

        let width = match available_space.width {
            taffy::AvailableSpace::Definite(w) => w.max(max_width),
            taffy::AvailableSpace::MinContent => max_width,
            taffy::AvailableSpace::MaxContent => max_width,
        };

        // Prefer the available width when definite; height is content height.
        taffy::Size { width, height }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        let area = ctx.area();
        // Use the inherited scroll from ancestors to determine the first
        // content row to render. This allows the scrollbar to move the visible
        // window through the diff lines.
        let start_idx = ctx.scroll_y().max(0.0).floor() as usize;
        let skip_cols = ctx.scroll_x().max(0.0).floor() as usize;
        let end_row = area.y.saturating_add(area.height);
        let visible_height = end_row
            .saturating_sub(area.y)
            .min(self.lines.len().saturating_sub(start_idx));

        info!("diff render, {}:{}", ctx.scroll_x(), ctx.scroll_y());

        let gutter_style = line_number_style();

        for (i, line_idx) in (0..visible_height).enumerate() {
            let idx = start_idx + line_idx;
            let y = area.y + i;
            if idx >= self.lines.len() || y >= area.y + area.height {
                break;
            }

            let mut remaining = area.width;
            let mut cursor_x = area.x;
            let mut fill_style: Option<Style> = None;
            let mut remaining_skip = skip_cols;

            if let Some(gutter) = &self.gutter {
                let gutter_text = gutter.format_line(&self.lines[idx]);
                let mut chars = gutter_text.chars();

                if remaining_skip > 0 {
                    let skipped = chars.by_ref().take(remaining_skip).count();
                    remaining_skip = remaining_skip.saturating_sub(skipped);
                }

                if remaining_skip == 0 && remaining > 0 {
                    let mut collected = String::new();
                    let mut taken = 0;
                    for ch in chars {
                        if remaining == 0 {
                            break;
                        }
                        collected.push(ch);
                        taken += 1;
                        remaining = remaining.saturating_sub(1);
                    }

                    if !collected.is_empty() {
                        let attrs = ctx.style_to_attributes(&gutter_style);
                        ctx.write_text(cursor_x, y, &collected, &attrs);
                        cursor_x += taken;
                    }
                }
            }
            for span in &self.lines[idx].spans {
                if remaining == 0 {
                    break;
                }
                let mut chars = span.content.chars();

                if span.style.bg.is_some() || span.style.dim {
                    fill_style = Some(span.style.clone());
                }
                if remaining_skip > 0 {
                    let skipped = chars.by_ref().take(remaining_skip).count();
                    remaining_skip = remaining_skip.saturating_sub(skipped);
                    if remaining_skip > 0 {
                        continue;
                    }
                }

                let mut collected = String::new();
                let mut taken = 0;
                for ch in chars {
                    if remaining == 0 {
                        break;
                    }
                    collected.push(ch);
                    taken += 1;
                    remaining = remaining.saturating_sub(1);
                }

                if collected.is_empty() {
                    continue;
                }
                let attrs = ctx.style_to_attributes(&span.style);
                ctx.write_text(cursor_x, y, &collected, &attrs);
                cursor_x += taken;
            }
            if remaining > 0
                && let Some(style) = &fill_style
            {
                let attrs = ctx.style_to_attributes(style);
                let padding = " ".repeat(remaining);
                ctx.write_text(cursor_x, y, &padding, &attrs);
            }
        }
    }

    fn debug_label(&self) -> &'static str {
        "diff"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
struct GutterConfig {
    old_width: usize,
    new_width: usize,
}

impl GutterConfig {
    fn from_lines(lines: &[DiffLine]) -> Option<Self> {
        let mut has_numbers = false;
        let mut old_width = 0;
        let mut new_width = 0;

        for line in lines {
            if let Some(numbers) = &line.line_numbers {
                if let Some(old) = numbers.old {
                    has_numbers = true;
                    old_width = old_width.max(digit_count(old));
                }
                if let Some(new) = numbers.new {
                    has_numbers = true;
                    new_width = new_width.max(digit_count(new));
                }
            }
        }

        has_numbers.then_some(Self {
            old_width,
            new_width,
        })
    }

    fn total_width(&self) -> usize {
        self.old_display_width() + 1 + self.new_display_width() + 3
    }

    fn format_line(&self, line: &DiffLine) -> String {
        let (old, new) = line
            .line_numbers
            .as_ref()
            .map(|numbers| {
                (
                    numbers
                        .old
                        .map(|value| value.to_string())
                        .unwrap_or_default(),
                    numbers
                        .new
                        .map(|value| value.to_string())
                        .unwrap_or_default(),
                )
            })
            .unwrap_or_else(|| (String::new(), String::new()));

        format!(
            "{old:>old_width$} {new:>new_width$} | ",
            old = old,
            new = new,
            old_width = self.old_display_width(),
            new_width = self.new_display_width(),
        )
    }

    fn old_display_width(&self) -> usize {
        self.old_width.max(1)
    }

    fn new_display_width(&self) -> usize {
        self.new_width.max(1)
    }
}

fn digit_count(value: usize) -> usize {
    let mut digits = 1;
    let mut n = value;
    while n >= 10 {
        n /= 10;
        digits += 1;
    }
    digits
}

fn normalized_number(value: usize) -> Option<usize> {
    (value != 0).then_some(value)
}

fn render_commit_modal(state: &CommitModal) -> Node<Msg> {
    let title = text::<Msg>("Commit staged changes").with_style(Style::bold());
    let instructions = text::<Msg>("Ctrl-D commits, Esc cancels").with_style(Style::dim());
    let input_style = commit_input_style();

    let prompt = text::<Msg>("> ").with_style(Style::fg(highlight::EVERFOREST_GREEN));
    let input_field = input::<Msg>(
        "commit-modal-input",
        &state.input,
        &input_style,
        Msg::CommitInput,
    )
    .with_flex_grow(1.)
    .with_flex_basis(Dimension::ZERO)
    .with_min_width(Dimension::ZERO);
    let input_row = row(vec![prompt, input_field])
        .with_width(Dimension::percent(1.0))
        .with_height(Dimension::length(3.))
        .with_id("commit-modal-input-row");

    let content = column(vec![title, instructions, input_row])
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

    let mut selection = style.selection.clone();
    selection.bg = Some(highlight::EVERFOREST_BG_GREEN);
    selection.fg = Some(highlight::EVERFOREST_FG);
    style.selection = selection;

    let mut cursor = style.cursor.clone();
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

fn title_style() -> Style {
    let mut style = Style::bold();
    style.fg = Some(highlight::EVERFOREST_BLUE);
    style
}

fn error_style() -> Style {
    Style::fg(highlight::EVERFOREST_RED)
}

fn line_number_style() -> Style {
    let mut style = Style::fg(highlight::EVERFOREST_GREY2);
    style.dim = true;
    style
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
        }
    }

    fn new(name: String, path: String) -> Self {
        Self {
            name,
            path,
            children: BTreeMap::new(),
            codes: BTreeSet::new(),
        }
    }

    fn insert(&mut self, components: &[&str], entry: &FileEntry) {
        if components.is_empty() {
            return;
        }

        self.codes.insert(entry.code);

        if components.len() == 1 {
            self.children
                .insert(components[0].to_string(), NodeBuilder::File(entry.clone()));
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
        let children = self
            .children
            .into_values()
            .map(|child| child.into_tree_node())
            .collect();
        let code = aggregate_code(&self.codes);
        let label = format!("{} {}", code, self.name);
        let node = TreeNode::branch(
            FileNodeId::Dir(self.path),
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

#[derive(Default)]
struct Model {
    unstaged: Vec<FileEntry>,
    staged: Vec<FileEntry>,
    unstaged_tree: TreeState<FileNodeId>,
    staged_tree: TreeState<FileNodeId>,
    unstaged_initialized: bool,
    staged_initialized: bool,
    focus: Focus,
    diff_lines: Vec<DiffLine>,
    diff_truncated: bool,
    diff_scroll: ScrollState,
    unstaged_scroll: ScrollState,
    staged_scroll: ScrollState,

    error: Option<String>,
    commit_modal: Option<CommitModal>,
    delete_modal: Option<DeleteModal>,
    show_all_shortcuts: bool,
    show_diff_line_numbers: bool,
    current_branch: Option<String>,
}

impl Model {
    const UNSTAGED_CONTAINER_ID: &'static str = "unstaged-section-content";
    const STAGED_CONTAINER_ID: &'static str = "staged-section-content";
    const UNSTAGED_ITEM_ID: &'static str = "unstaged-entry";
    const STAGED_ITEM_ID: &'static str = "staged-entry";

    fn new() -> Self {
        let mut model = Self {
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

    fn tree_state(&self, focus: Focus) -> &TreeState<FileNodeId> {
        match focus {
            Focus::Unstaged => &self.unstaged_tree,
            Focus::Staged => &self.staged_tree,
        }
    }

    fn tree_state_mut(&mut self, focus: Focus) -> &mut TreeState<FileNodeId> {
        match focus {
            Focus::Unstaged => &mut self.unstaged_tree,
            Focus::Staged => &mut self.staged_tree,
        }
    }

    fn selected_id(&self, focus: Focus) -> Option<FileNodeId> {
        self.tree_state(focus).selected().cloned()
    }

    fn select_tree_id(&mut self, focus: Focus, id: &FileNodeId) {
        let state = self.tree_state_mut(focus);
        state.select(id.clone());
        state.ensure_selected();
    }

    fn ensure_tree_selection(&mut self, focus: Focus) {
        self.tree_state_mut(focus).ensure_selected();
    }

    fn select_if_different(&mut self, focus: Focus, id: &FileNodeId) -> bool {
        let should_change = self
            .tree_state(focus)
            .selected()
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
                if self.tree_state(focus).is_expanded(&id) {
                    self.tree_state_mut(focus).set_expanded(id.clone(), false);
                    changed = true;
                } else if let Some(parent) = parent_dir_node(path) {
                    if self.select_if_different(focus, &parent) {
                        changed = true;
                    }
                    if self.tree_state(focus).is_expanded(&parent) {
                        self.tree_state_mut(focus)
                            .set_expanded(parent.clone(), false);
                        changed = true;
                    }
                }
            }
            FileNodeId::File(path) => {
                if let Some(parent) = parent_dir_node(path) {
                    if self.select_if_different(focus, &parent) {
                        changed = true;
                    }
                    if self.tree_state(focus).is_expanded(&parent) {
                        self.tree_state_mut(focus)
                            .set_expanded(parent.clone(), false);
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
                if !self.tree_state(focus).is_expanded(&id) {
                    self.tree_state_mut(focus).set_expanded(id.clone(), true);
                    changed = true;
                }
            }
            FileNodeId::File(path) => {
                if let Some(parent) = parent_dir_node(path)
                    && !self.tree_state(focus).is_expanded(&parent)
                {
                    self.tree_state_mut(focus)
                        .set_expanded(parent.clone(), true);
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
        let state = self.tree_state(focus);
        match (state.selected(), state.visible().first()) {
            (Some(selected), Some(first)) => selected == &first.id,
            _ => true,
        }
    }

    fn is_last_selected(&self, focus: Focus) -> bool {
        let state = self.tree_state(focus);
        match (state.selected(), state.visible().last()) {
            (Some(selected), Some(last)) => selected == &last.id,
            _ => true,
        }
    }

    fn set_error(&mut self, err: color_eyre::eyre::Report) {
        self.error = Some(err.to_string());
    }

    fn clear_error(&mut self) {
        self.error = None;
    }

    fn open_commit_modal(&mut self) {
        self.commit_modal = Some(CommitModal::default());
    }

    fn close_commit_modal(&mut self) {
        self.commit_modal = None;
    }

    fn open_delete_modal(&mut self) {
        if let Some(entry) = self.current_entry()
            && self.focus == Focus::Unstaged
        {
            self.delete_modal = Some(DeleteModal::new(entry.path.clone()));
        }
    }

    fn close_delete_modal(&mut self) {
        self.delete_modal = None;
    }

    fn toggle_shortcuts_help(&mut self) {
        self.show_all_shortcuts = !self.show_all_shortcuts;
    }

    fn toggle_diff_line_numbers(&mut self) {
        self.show_diff_line_numbers = !self.show_diff_line_numbers;
    }

    fn submit_commit(&mut self) -> Transition {
        let message = match self.commit_modal.as_ref() {
            Some(modal) => modal.message(),
            None => return Transition::Continue,
        };

        if message.trim().is_empty() {
            self.set_error(eyre!("Commit message cannot be empty"));
            return Transition::Continue;
        }

        match git::run_git(["commit", "-m", message.as_str()]) {
            Ok(_) => {
                self.commit_modal = None;
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

    fn confirm_delete(&mut self) -> Transition {
        let path = match self.delete_modal.as_ref() {
            Some(modal) => modal.path.clone(),
            None => return Transition::Continue,
        };

        // Determine the selected entry code to decide how to discard.
        let code = match self.focus {
            Focus::Unstaged => self.current_entry().map(|e| e.code).unwrap_or(' '),
            Focus::Staged => ' ',
        };

        let result = git::discard_unstaged_changes(&path, code);
        self.delete_modal = None;

        match result.and_then(|_| self.refresh_status()) {
            Ok(_) => {}
            Err(err) => self.set_error(err),
        }

        Transition::Continue
    }

    fn refresh_status(&mut self) -> Result<()> {
        self.refresh_status_internal(false)
    }

    fn refresh_status_preserving_diff_scroll(&mut self) -> Result<()> {
        self.refresh_status_internal(true)
    }

    fn refresh_status_internal(&mut self, preserve_diff_scroll: bool) -> Result<()> {
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

        let status = git::load_status()?;
        self.unstaged = status.unstaged;
        self.staged = status.staged;
        self.current_branch = status.branch;
        let unstaged_nodes = build_file_tree(&self.unstaged);
        let staged_nodes = build_file_tree(&self.staged);
        self.unstaged_tree.set_items(unstaged_nodes);
        self.staged_tree.set_items(staged_nodes);
        if !self.unstaged_initialized && !self.unstaged_tree.visible().is_empty() {
            self.unstaged_tree.expand_all();
            self.unstaged_initialized = true;
        }
        if !self.staged_initialized && !self.staged_tree.visible().is_empty() {
            self.staged_tree.expand_all();
            self.staged_initialized = true;
        }
        self.ensure_focus_valid();
        self.clear_error();
        self.update_diff();
        if let (Some(prev_focus), Some(prev_selection)) = (previous_focus, previous_selection)
            && self.focus == prev_focus
            && self
                .tree_state(self.focus)
                .selected()
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
        Ok(())
    }

    fn ensure_focus_valid(&mut self) {
        if self.focus == Focus::Unstaged
            && self.unstaged_tree.visible().is_empty()
            && !self.staged_tree.visible().is_empty()
        {
            self.focus = Focus::Staged;
        } else if self.focus == Focus::Staged
            && self.staged_tree.visible().is_empty()
            && !self.unstaged_tree.visible().is_empty()
        {
            self.focus = Focus::Unstaged;
        }
        self.ensure_tree_selection(Focus::Unstaged);
        self.ensure_tree_selection(Focus::Staged);
        self.clamp_file_scrolls();
    }

    fn update_diff(&mut self) {
        let selected_id = self.selected_id(self.focus);
        let diff_result = match (self.current_entry(), selected_id.as_ref()) {
            (Some(entry), _) => diff_for(entry, self.focus),
            (None, Some(FileNodeId::Dir(_))) => {
                self.diff_lines = vec![DiffLine::plain("Select a file to view diff")];
                self.diff_truncated = false;
                self.diff_scroll.reset();
                return;
            }
            (None, _) => {
                self.diff_lines = vec![DiffLine::plain("No file selected")];
                self.diff_truncated = false;
                self.diff_scroll.reset();
                return;
            }
        };

        match diff_result {
            Ok(preview) => {
                let DiffPreview { lines, truncated } = preview;
                if lines.is_empty() {
                    self.diff_lines = vec![DiffLine::plain("No diff available")];
                    self.diff_truncated = false;
                } else {
                    self.diff_lines = lines;
                    self.diff_truncated = truncated;
                }
                self.diff_scroll.reset();

                if matches!(self.error.as_deref(), Some(msg) if msg.starts_with("git diff")) {
                    self.clear_error();
                }
            }
            Err(err) => {
                self.diff_lines = vec![DiffLine::plain("Failed to load diff")];
                self.diff_truncated = false;
                self.set_error(err);
                self.diff_scroll.reset();
            }
        }
    }

    fn current_entry(&self) -> Option<&FileEntry> {
        let focus = self.focus;
        let selected = self.tree_state(focus).selected()?;
        let path = match selected {
            FileNodeId::File(path) => path,
            FileNodeId::Dir(_) => return None,
        };
        self.entries_for_focus(focus)
            .iter()
            .find(|entry| entry.path == *path)
    }

    fn entries_for_focus(&self, focus: Focus) -> &[FileEntry] {
        match focus {
            Focus::Unstaged => &self.unstaged,
            Focus::Staged => &self.staged,
        }
    }

    fn scroll_state_mut(&mut self, focus: Focus) -> &mut ScrollState {
        match focus {
            Focus::Unstaged => &mut self.unstaged_scroll,
            Focus::Staged => &mut self.staged_scroll,
        }
    }

    fn move_selection_up(&mut self) {
        match self.focus {
            Focus::Unstaged => {
                if self.unstaged_tree.visible().is_empty() {
                    return;
                }
                if self.is_first_selected(Focus::Unstaged) {
                    if !self.staged_tree.visible().is_empty() {
                        self.focus = Focus::Staged;
                        let state = self.tree_state_mut(Focus::Staged);
                        state.select_last();
                        state.ensure_selected();
                        self.queue_scroll_for_staged();
                    }
                } else {
                    self.tree_state_mut(Focus::Unstaged).select_prev();
                    self.queue_scroll_for_unstaged();
                }
            }
            Focus::Staged => {
                if self.staged_tree.visible().is_empty() {
                    return;
                }
                if self.is_first_selected(Focus::Staged) {
                    if !self.unstaged_tree.visible().is_empty() {
                        self.focus = Focus::Unstaged;
                        let state = self.tree_state_mut(Focus::Unstaged);
                        state.select_last();
                        state.ensure_selected();
                        self.queue_scroll_for_unstaged();
                    }
                } else {
                    self.tree_state_mut(Focus::Staged).select_prev();
                    self.queue_scroll_for_staged();
                }
            }
        }
    }

    fn move_selection_down(&mut self) {
        match self.focus {
            Focus::Unstaged => {
                if self.unstaged_tree.visible().is_empty() {
                    return;
                }
                if self.is_last_selected(Focus::Unstaged) {
                    if !self.staged_tree.visible().is_empty() {
                        self.focus = Focus::Staged;
                        let state = self.tree_state_mut(Focus::Staged);
                        state.select_first();
                        state.ensure_selected();
                        self.queue_scroll_for_staged();
                    }
                } else {
                    self.tree_state_mut(Focus::Unstaged).select_next();
                    self.queue_scroll_for_unstaged();
                }
            }
            Focus::Staged => {
                if self.staged_tree.visible().is_empty() {
                    return;
                }
                if self.is_last_selected(Focus::Staged) {
                    // Stay within staged list; no wrap-around.
                    return;
                }
                self.tree_state_mut(Focus::Staged).select_next();
                self.queue_scroll_for_staged();
            }
        }
    }

    fn focus_unstaged(&mut self) {
        if !self.unstaged_tree.visible().is_empty() {
            self.focus = Focus::Unstaged;
            self.ensure_tree_selection(Focus::Unstaged);
            self.queue_scroll_for_unstaged();
        }
    }

    fn focus_staged(&mut self) {
        if !self.staged_tree.visible().is_empty() {
            self.focus = Focus::Staged;
            self.ensure_tree_selection(Focus::Staged);
            self.queue_scroll_for_staged();
        }
    }

    fn toggle_stage_selected(&mut self) {
        if let Some(id) = self.selected_id(self.focus) {
            self.toggle_stage_for_id(self.focus, &id);
        }
    }

    fn toggle_stage_for_id(&mut self, focus: Focus, id: &FileNodeId) {
        let path = id.path();
        let result = match focus {
            Focus::Unstaged => git::stage_path(path),
            Focus::Staged => git::unstage_path(path),
        };

        if let Err(err) = result {
            self.set_error(err);
            return;
        }

        if let Err(err) = self.refresh_status() {
            self.set_error(err);
        }
    }

    fn handle_tree_message(&mut self, focus: Focus, tree_msg: TreeMsg<FileNodeId>) -> Transition {
        match tree_msg {
            TreeMsg::Activate(id) => {
                self.focus = focus;
                self.select_tree_id(focus, &id);
                self.queue_scroll_for_focus(focus);
                self.update_diff();
                Transition::Continue
            }
            TreeMsg::ToggleExpand(id) => {
                self.focus = focus;
                self.select_tree_id(focus, &id);
                if id.is_dir() {
                    self.tree_state_mut(focus).toggle_expanded(&id);
                    self.queue_scroll_for_focus(focus);
                    self.update_diff();
                    Transition::Continue
                } else {
                    self.queue_scroll_for_focus(focus);
                    self.toggle_stage_for_id(focus, &id);
                    Transition::Continue
                }
            }
        }
    }

    fn scroll_diff(&mut self, delta: i32) {
        self.diff_scroll.update(ScrollMsg::Delta(delta));
    }

    fn scroll_diff_horizontal(&mut self, delta: i32) {
        self.diff_scroll.update(ScrollMsg::AxisDelta {
            axis: ScrollAxis::Horizontal,
            amount: delta,
        });
    }

    fn scroll_files(&mut self, focus: Focus, delta: i32) {
        self.update_section_scroll(focus, ScrollMsg::Delta(delta));
    }

    fn scroll_files_in_focus(&mut self, delta: i32) {
        let focus = self.focus;
        self.scroll_files(focus, delta);
    }

    fn queue_scroll_for_focus(&mut self, focus: Focus) {
        match focus {
            Focus::Unstaged => self.queue_scroll_for_unstaged(),
            Focus::Staged => self.queue_scroll_for_staged(),
        }
    }

    fn queue_scroll_for_unstaged(&mut self) {
        if let Some(id) = self.selected_id(Focus::Unstaged) {
            let target = ScrollTarget::with_mixin(Self::UNSTAGED_ITEM_ID, id.mixin());
            self.unstaged_scroll
                .ensure_visible(Self::UNSTAGED_CONTAINER_ID, target);
        }
    }

    fn queue_scroll_for_staged(&mut self) {
        if let Some(id) = self.selected_id(Focus::Staged) {
            let target = ScrollTarget::with_mixin(Self::STAGED_ITEM_ID, id.mixin());
            self.staged_scroll
                .ensure_visible(Self::STAGED_CONTAINER_ID, target);
        }
    }

    fn update_section_scroll(&mut self, focus: Focus, msg: ScrollMsg) {
        if matches!(msg, ScrollMsg::Delta(_) | ScrollMsg::AxisDelta { .. })
            && self.tree_state(focus).visible().is_empty()
        {
            return;
        }
        self.scroll_state_mut(focus).update(msg);
        // if focus == self.focus {
        //     self.ensure_selected_visible();
        // }
    }

    fn clamp_file_scrolls(&mut self) {
        if self.unstaged_tree.visible().is_empty() {
            self.unstaged_scroll.reset();
        } else {
            let max_offset = (self.unstaged_tree.visible().len().saturating_sub(1)) as f32;
            if self.unstaged_scroll.offset() > max_offset {
                self.unstaged_scroll.set_offset(max_offset);
            }
        }
        if self.staged_tree.visible().is_empty() {
            self.staged_scroll.reset();
        } else {
            let max_offset = (self.staged_tree.visible().len().saturating_sub(1)) as f32;
            if self.staged_scroll.offset() > max_offset {
                self.staged_scroll.set_offset(max_offset);
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Msg {
    KeyPressed(Key),
    Quit,
    ToggleStage,
    ToggleFocus,
    MoveSelectionUp,
    MoveSelectionDown,
    CollapseNode,
    ExpandNode,
    ScrollFiles(i32),
    ScrollDiff(i32),
    ScrollDiffHorizontal(i32),
    RefreshStatus,
    OpenCommitModal,
    OpenDeleteModal,
    ToggleShortcutsHelp,
    ToggleDiffLineNumbers,
    SubmitCommit,
    CancelCommit,
    CommitInput(InputMsg),
    ConfirmDelete,
    CancelDelete,
    Staged(SectionMsg),
    Unstaged(SectionMsg),
    DiffScroll(ScrollMsg),
    UnstagedScroll(ScrollMsg),
    StagedScroll(ScrollMsg),
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

struct DiffPreview {
    lines: Vec<DiffLine>,
    truncated: bool,
}

fn diff_for(entry: &FileEntry, focus: Focus) -> Result<DiffPreview> {
    let mut diff_output = match focus {
        Focus::Unstaged => git::diff_unstaged(entry),
        Focus::Staged => git::diff_staged(entry),
    }?;

    if diff_output.trim().is_empty() {
        return Ok(DiffPreview {
            lines: Vec::new(),
            truncated: false,
        });
    }

    let diff_truncated = truncate_string(&mut diff_output, git::PREVIEW_BYTE_LIMIT);
    let versions = load_file_versions(entry, focus)?;
    let truncated = diff_truncated || versions.truncated;
    let lines = build_diff_lines(&diff_output, &versions);

    Ok(DiffPreview { lines, truncated })
}

struct FileVersions {
    old: Option<FileContent>,
    new: Option<FileContent>,
    truncated: bool,
}

#[derive(Clone)]
struct FileContent {
    lines: Vec<LineContent>,
}

#[derive(Clone)]
struct LineContent {
    text: String,
    spans: Vec<TextSpan>,
}

impl FileContent {
    fn from_source(path: &str, source: LoadedContent) -> Self {
        let LoadedContent { text, truncated } = source;
        let text_lines: Vec<String> = text.lines().map(str::to_string).collect();
        let line_count = text_lines.len();
        let highlight_lines = if truncated {
            None
        } else {
            match highlight::highlight_lines(Path::new(path), &text) {
                Some(lines) if lines.len() == line_count => Some(lines),
                _ => None,
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
            });
        }

        Self { lines }
    }

    fn line(&self, index: usize) -> Option<&LineContent> {
        self.lines.get(index)
    }
}

fn load_file_versions(entry: &FileEntry, focus: Focus) -> Result<FileVersions> {
    let old_source = match focus {
        Focus::Unstaged => {
            if entry.code == '?' {
                None
            } else {
                git::read_index_file(&entry.path)?
            }
        }
        Focus::Staged => git::read_head_file(&entry.path)?,
    };

    let new_source = match focus {
        Focus::Unstaged => read_worktree_file(&entry.path)?,
        Focus::Staged => git::read_index_file(&entry.path)?,
    };

    let old_truncated = old_source.as_ref().is_some_and(|content| content.truncated);
    let new_truncated = new_source.as_ref().is_some_and(|content| content.truncated);

    Ok(FileVersions {
        old: old_source.map(|content| FileContent::from_source(&entry.path, content)),
        new: new_source.map(|content| FileContent::from_source(&entry.path, content)),
        truncated: old_truncated || new_truncated,
    })
}

fn read_worktree_file(path: &str) -> Result<Option<LoadedContent>> {
    match File::open(path) {
        Ok(file) => load_limited_from_file(file)
            .map(Some)
            .wrap_err_with(|| format!("failed to read {}", path)),
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(None),
        Err(err) => Err(err).wrap_err_with(|| format!("failed to read {}", path)),
    }
}

fn load_limited_from_file(file: File) -> Result<LoadedContent> {
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader
        .by_ref()
        .take(git::PREVIEW_BYTE_LIMIT as u64)
        .read_to_end(&mut buffer)
        .wrap_err("failed to read file prefix")?;

    let truncated = if buffer.len() == git::PREVIEW_BYTE_LIMIT {
        let mut probe = [0u8; 1];
        loop {
            match reader.read(&mut probe) {
                Ok(0) => break false,
                Ok(_) => break true,
                Err(err) if err.kind() == ErrorKind::Interrupted => continue,
                Err(err) => return Err(err).wrap_err("failed to read file tail"),
            }
        }
    } else {
        false
    };

    let text = String::from_utf8_lossy(&buffer).into_owned();
    Ok(LoadedContent { text, truncated })
}

fn truncate_string(content: &mut String, limit: usize) -> bool {
    if content.len() <= limit {
        return false;
    }

    let mut idx = limit;
    while idx > 0 && !content.is_char_boundary(idx) {
        idx -= 1;
    }

    content.truncate(idx);
    true
}

fn build_diff_lines(diff: &str, versions: &FileVersions) -> Vec<DiffLine> {
    let mut result = Vec::new();
    let mut old_line = 0usize;
    let mut new_line = 0usize;

    for line in diff.lines() {
        if line.starts_with("diff --git") {
            // let mut style = Style::fg(highlight::EVERFOREST_BLUE);
            // style.bold = true;
            // result.push(DiffLine::styled(line.to_string(), style));
            continue;
        }

        if line.starts_with("index ") {
            result.push(DiffLine::plain(line.to_string()));
            continue;
        }

        if line.starts_with("---") || line.starts_with("+++") {
            // result.push(DiffLine::styled(
            //     line.to_string(),
            //     Style::fg(highlight::EVERFOREST_YELLOW),
            // ));
            continue;
        }

        if line.starts_with("@@") {
            if let Some((old_start, new_start)) = parse_hunk_header(line) {
                old_line = old_start;
                new_line = new_start;
            }
            let mut style = Style::fg(highlight::EVERFOREST_GREY1);
            style.dim = true;
            result.push(DiffLine::styled(line.to_string(), style));
            continue;
        }

        if line.starts_with('\\') {
            result.push(DiffLine::plain(line.to_string()));
            continue;
        }

        if let Some(content) = line.strip_prefix('+') {
            let spans = highlight_with_fallback(versions.new.as_ref(), new_line, content);
            let mut diff_line = DiffLine::from_spans(with_prefix('+', spans));
            let old_number = None;
            let new_number = normalized_number(new_line);
            if old_number.is_some() || new_number.is_some() {
                diff_line = diff_line.with_line_numbers(old_number, new_number);
            }
            result.push(diff_line);
            new_line = new_line.saturating_add(1);
            continue;
        }

        if let Some(content) = line.strip_prefix('-') {
            let spans = highlight_with_fallback(versions.old.as_ref(), old_line, content);
            let mut diff_line = DiffLine::from_spans(with_prefix('-', spans));
            let old_number = normalized_number(old_line);
            let new_number = None;
            if old_number.is_some() || new_number.is_some() {
                diff_line = diff_line.with_line_numbers(old_number, new_number);
            }
            result.push(diff_line);
            old_line = old_line.saturating_add(1);
            continue;
        }

        if let Some(content) = line.strip_prefix(' ') {
            let spans = highlight_with_fallback(versions.new.as_ref(), new_line, content);
            let mut diff_line = DiffLine::from_spans(with_prefix(' ', spans));
            let old_number = normalized_number(old_line);
            let new_number = normalized_number(new_line);
            if old_number.is_some() || new_number.is_some() {
                diff_line = diff_line.with_line_numbers(old_number, new_number);
            }
            result.push(diff_line);
            new_line = new_line.saturating_add(1);
            old_line = old_line.saturating_add(1);
            continue;
        }

        result.push(DiffLine::plain(line.to_string()));
    }

    result
}

fn parse_hunk_header(line: &str) -> Option<(usize, usize)> {
    let mut parts = line.split_whitespace();
    let _ = parts.next(); // @@
    let old_part = parts.next()?; // -start,count
    let new_part = parts.next()?; // +start,count

    let old_start = parse_hunk_range(old_part.strip_prefix('-')?)?;
    let new_start = parse_hunk_range(new_part.strip_prefix('+')?)?;

    Some((old_start, new_start))
}

fn parse_hunk_range(segment: &str) -> Option<usize> {
    segment
        .split(',')
        .next()
        .and_then(|value| value.parse::<usize>().ok())
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
    fn build_diff_lines_produces_line_numbers() {
        let versions = FileVersions {
            old: None,
            new: None,
            truncated: false,
        };
        let diff = "@@ -1 +1 @@\n-foo\n+bar\n";

        let lines = build_diff_lines(diff, &versions);

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
}

fn init_tracing() -> color_eyre::Result<()> {
    use std::fs::File;
    use std::path::PathBuf;
    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

    let log_path = PathBuf::from("gs.log");

    File::options().create(true).append(true).open(&log_path)?;

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
        .map_err(|error| color_eyre::eyre::eyre!(error))?;

    Ok(())
}
