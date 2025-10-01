use std::ffi::OsStr;
use std::fs;
use std::io::ErrorKind;
use std::path::Path;
use std::process::Command;

use chatui::components::scroll::{ScrollMsg, ScrollState, scrollable_content};
use chatui::dom::{Node, TextSpan};
use chatui::event::{Event, Key, KeyCode};
use chatui::{Program, Style, Transition, block_with_title, column, modal, rich_text, row, text};
use color_eyre::eyre::{Context, Result, eyre};
use taffy::Dimension;
use taffy::prelude::{FromLength, TaffyZero};
use taffy::style::FlexWrap;
use tracing::info;
use tracing_subscriber::EnvFilter;

mod highlight;
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
            label: "Left",
            description: "Focus unstaged",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Left,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::FocusUnstaged),
        },
        Shortcut {
            label: "Right",
            description: "Focus staged",
            show_in_bar: true,
            binding: Some(Binding::new(
                KeyCode::Right,
                ModifierRequirement::Disabled,
                ModifierRequirement::Disabled,
                ModifierRequirement::Any,
            )),
            msg: Some(Msg::FocusStaged),
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
            msg: Some(Msg::BackspaceCommit),
        },
    ];
}

fn main() -> Result<()> {
    color_eyre::install()?;
    init_tracing()?;

    let program = Program::new(Model::new(), update, view).map_event(map_event);

    if let Err(error) = program.run() {
        eprintln!("Program exited with error: {:?}", error);
    }

    Ok(())
}

fn map_event(event: Event) -> Option<Msg> {
    if let Event::Key(key) = event {
        Some(Msg::KeyPressed(key))
    } else {
        None
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
        Msg::FocusUnstaged => {
            model.focus_unstaged();
            model.update_diff();
            Transition::Continue
        }
        Msg::FocusStaged => {
            model.focus_staged();
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
        Msg::ScrollFiles(delta) => {
            model.scroll_files_in_focus(delta);
            Transition::Continue
        }
        Msg::ScrollDiff(delta) => {
            model.scroll_diff(delta);
            Transition::Continue
        }
        Msg::RefreshStatus => {
            if let Err(err) = model.refresh_status() {
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
        Msg::SubmitCommit => model.submit_commit(),
        Msg::CancelCommit => {
            model.close_commit_modal();
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
        Msg::BackspaceCommit => {
            model.backspace_commit_message();
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
        SectionMsg::ActivateFile(index) => {
            match focus {
                Focus::Unstaged => {
                    model.selected_unstaged = index;
                    model.focus = Focus::Unstaged;
                }
                Focus::Staged => {
                    model.selected_staged = index;
                    model.focus = Focus::Staged;
                }
            }
            model.update_diff();
            Transition::Continue
        }
        SectionMsg::ToggleStageEntry(index) => {
            match focus {
                Focus::Unstaged => {
                    model.selected_unstaged = index;
                    model.focus = Focus::Unstaged;
                }
                Focus::Staged => {
                    model.selected_staged = index;
                    model.focus = Focus::Staged;
                }
            }
            model.toggle_stage_selected();
            Transition::Continue
        }
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
    match key.code {
        KeyCode::Char(ch) => {
            if key.ctrl || key.alt {
                return Transition::Continue;
            }

            model.append_commit_char(ch);
            Transition::Continue
        }
        _ => Transition::Continue,
    }
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
    let unstaged_list = render_file_list(
        &model.unstaged,
        model.focus == Focus::Unstaged,
        model.selected_unstaged,
        false,
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

    let staged_list = render_file_list(
        &model.staged,
        model.focus == Focus::Staged,
        model.selected_staged,
        true,
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

fn render_file_list(
    entries: &[FileEntry],
    is_active: bool,
    selected: usize,
    is_staged: bool,
) -> Node<Msg> {
    let mut items = Vec::new();

    if entries.is_empty() {
        items.push(text::<Msg>("(no files)").with_style(inactive_style()));
    } else {
        for (idx, entry) in entries.iter().enumerate() {
            let mut node = text::<Msg>(&entry.display).on_mouse(move |event| {
                if event.is_double_click() {
                    Some(if is_staged {
                        Msg::Staged(SectionMsg::ToggleStageEntry(idx))
                    } else {
                        Msg::Unstaged(SectionMsg::ToggleStageEntry(idx))
                    })
                } else if event.is_single_click() {
                    Some(if is_staged {
                        Msg::Staged(SectionMsg::ActivateFile(idx))
                    } else {
                        Msg::Unstaged(SectionMsg::ActivateFile(idx))
                    })
                } else {
                    None
                }
            });

            if idx == selected {
                let mut style = if is_active {
                    active_selection_style()
                } else {
                    inactive_selection_style()
                };
                // Keep the status letter readable.
                if entry.code == '?' {
                    style.bold = true;
                }
                node = node.with_style(style);
            }

            items.push(node);
        }
    }

    column(items)
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO)
}

fn render_diff_pane(model: &Model) -> Node<Msg> {
    let diff_title = match model.current_entry() {
        Some(entry) => format!("Diff Preview - {}", entry.path),
        None => "Diff Preview".to_string(),
    };
    let content = scrollable_content(
        "diff-pane-content",
        &model.diff_scroll,
        3,
        Msg::DiffScroll,
        block_with_title(diff_title, vec![render_diff_lines(&model.diff_lines)]),
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

    // if model.show_all_shortcuts {
    //     shortcuts_row = shortcuts_row.with_flex_wrap(FlexWrap::Wrap);
    // }
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
                    if model.show_all_shortcuts { "less" } else { "more" },
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

fn render_diff_lines(lines: &[DiffLine]) -> Node<Msg> {
    if lines.is_empty() {
        return column(vec![
            text::<Msg>("No diff available").with_style(inactive_style()),
        ])
        .with_min_height(Dimension::ZERO)
        .with_flex_grow(1.)
        .with_flex_basis(Dimension::ZERO);
    }

    let rendered: Vec<_> = lines
        .iter()
        .map(|line| rich_text::<Msg>(line.spans.clone()).with_overflow_x(taffy::Overflow::Clip))
        .collect();

    column(rendered).with_id("diff_lines")
}

fn render_commit_modal(state: &CommitModal) -> Node<Msg> {
    let title = text::<Msg>("Commit staged changes").with_style(Style::bold());
    let instructions = text::<Msg>("Ctrl-D commits, Esc cancels").with_style(Style::dim());
    let input_value = format!("> {}", state.display_value());
    let input = text::<Msg>(input_value).with_style(Style::fg(highlight::EVERFOREST_GREEN));

    let content = column(vec![title, instructions, input])
        .with_min_width(Dimension::length(30.0))
        .with_min_height(Dimension::length(3.0));

    modal(vec![
        block_with_title("Commit", vec![content])
            .with_min_width(Dimension::length(42.0))
            .with_min_height(Dimension::length(5.0))
            .with_id("commit-modal-block"),
    ])
    .with_id("commit-modal")
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

fn inactive_style() -> Style {
    Style::fg(highlight::EVERFOREST_GREY2)
}

fn branch_name_style() -> Style {
    Style::fg(highlight::EVERFOREST_AQUA)
}

fn active_selection_style() -> Style {
    let mut style = Style::bold();
    style.fg = Some(highlight::EVERFOREST_GREEN);
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
enum Focus {
    #[default]
    Unstaged,
    Staged,
}

#[derive(Clone, Debug)]
struct FileEntry {
    path: String,
    display: String,
    code: char,
}

impl FileEntry {
    fn new(path: String, code: char, display: String) -> Self {
        Self {
            path,
            display,
            code,
        }
    }
}

#[derive(Default)]
struct CommitModal {
    message: String,
}

impl CommitModal {
    fn display_value(&self) -> String {
        if self.message.is_empty() {
            String::from("_")
        } else {
            let mut rendered = self.message.clone();
            rendered.push('_');
            rendered
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
    focus: Focus,
    selected_unstaged: usize,
    selected_staged: usize,
    diff_lines: Vec<DiffLine>,
    diff_scroll: ScrollState,
    unstaged_scroll: ScrollState,
    staged_scroll: ScrollState,

    error: Option<String>,
    commit_modal: Option<CommitModal>,
    delete_modal: Option<DeleteModal>,
    show_all_shortcuts: bool,
    current_branch: Option<String>,
}

impl Model {
    fn new() -> Self {
        let mut model = Self::default();

        if let Err(err) = model.refresh_status() {
            model.set_error(err);
            model.diff_lines = vec![DiffLine::plain("Unable to load git status")];
        }

        model
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

    fn append_commit_char(&mut self, ch: char) {
        if let Some(modal) = self.commit_modal.as_mut() {
            modal.message.push(ch);
        }
    }

    fn backspace_commit_message(&mut self) {
        if let Some(modal) = self.commit_modal.as_mut() {
            modal.message.pop();
        }
    }

    fn submit_commit(&mut self) -> Transition {
        let message = match self.commit_modal.as_ref() {
            Some(modal) => modal.message.clone(),
            None => return Transition::Continue,
        };

        if message.trim().is_empty() {
            self.set_error(eyre!("Commit message cannot be empty"));
            return Transition::Continue;
        }

        match run_git(["commit", "-m", message.as_str()]) {
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
            Focus::Unstaged => self
                .unstaged
                .get(self.selected_unstaged)
                .map(|e| e.code)
                .unwrap_or(' '),
            Focus::Staged => ' ',
        };

        let result = discard_unstaged_changes(&path, code);
        self.delete_modal = None;

        match result.and_then(|_| self.refresh_status()) {
            Ok(_) => {}
            Err(err) => self.set_error(err),
        }

        Transition::Continue
    }

    fn refresh_status(&mut self) -> Result<()> {
        let status = load_git_status()?;
        self.unstaged = status.unstaged;
        self.staged = status.staged;
        self.current_branch = status.branch;
        self.ensure_focus_valid();
        self.clear_error();
        self.update_diff();
        Ok(())
    }

    fn ensure_focus_valid(&mut self) {
        clamp_index(&mut self.selected_unstaged, self.unstaged.len());
        clamp_index(&mut self.selected_staged, self.staged.len());

        if self.focus == Focus::Unstaged && self.unstaged.is_empty() && !self.staged.is_empty() {
            self.focus = Focus::Staged;
        } else if self.focus == Focus::Staged && self.staged.is_empty() && !self.unstaged.is_empty()
        {
            self.focus = Focus::Unstaged;
        }
        self.clamp_file_scrolls();
    }

    fn update_diff(&mut self) {
        let diff_result = match self.current_entry() {
            Some(entry) => diff_for(entry, self.focus),
            None => {
                self.diff_lines = vec![DiffLine::plain("No file selected")];
                self.diff_scroll.reset();
                return;
            }
        };

        match diff_result {
            Ok(lines) => {
                if lines.is_empty() {
                    self.diff_lines = vec![DiffLine::plain("No diff available")];
                } else {
                    self.diff_lines = lines;
                }
                self.diff_scroll.reset();

                if matches!(self.error.as_deref(), Some(msg) if msg.starts_with("git diff")) {
                    self.clear_error();
                }
            }
            Err(err) => {
                self.diff_lines = vec![DiffLine::plain("Failed to load diff")];
                self.set_error(err);
                self.diff_scroll.reset();
            }
        }
    }

    fn current_entry(&self) -> Option<&FileEntry> {
        match self.focus {
            Focus::Unstaged => self.unstaged.get(self.selected_unstaged),
            Focus::Staged => self.staged.get(self.selected_staged),
        }
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
                if self.selected_unstaged > 0 {
                    self.selected_unstaged -= 1;
                    if (self.selected_unstaged as f32) < self.unstaged_scroll.offset() {
                        self.unstaged_scroll
                            .set_offset(self.selected_unstaged as f32);
                    }
                }
            }
            Focus::Staged => {
                if self.selected_staged > 0 {
                    self.selected_staged -= 1;
                    if (self.selected_staged as f32) < self.staged_scroll.offset() {
                        self.staged_scroll.set_offset(self.selected_staged as f32);
                    }
                } else if !self.unstaged.is_empty() {
                    self.focus = Focus::Unstaged;
                    self.selected_unstaged = self.unstaged.len().saturating_sub(1);
                    self.sync_scroll_to_selected();
                }
            }
        }
    }

    fn move_selection_down(&mut self) {
        match self.focus {
            Focus::Unstaged => {
                if self.selected_unstaged + 1 < self.unstaged.len() {
                    self.selected_unstaged += 1;
                    self.ensure_selected_visible();
                } else if !self.staged.is_empty() {
                    self.focus = Focus::Staged;
                    self.selected_staged = 0;
                    self.sync_scroll_to_selected();
                }
            }
            Focus::Staged => {
                if self.selected_staged + 1 < self.staged.len() {
                    self.selected_staged += 1;
                    self.ensure_selected_visible();
                }
            }
        }
    }

    fn focus_unstaged(&mut self) {
        if !self.unstaged.is_empty() {
            self.focus = Focus::Unstaged;
        }
    }

    fn focus_staged(&mut self) {
        if !self.staged.is_empty() {
            self.focus = Focus::Staged;
        }
    }

    fn toggle_stage_selected(&mut self) {
        let entry = match self.current_entry().cloned() {
            Some(entry) => entry,
            None => return,
        };

        let result = match self.focus {
            Focus::Unstaged => stage_path(&entry.path),
            Focus::Staged => unstage_path(&entry.path),
        };

        if let Err(err) = result {
            self.set_error(err);
            return;
        }

        if let Err(err) = self.refresh_status() {
            self.set_error(err);
        }
    }

    fn scroll_diff(&mut self, delta: i32) {
        self.diff_scroll.update(ScrollMsg::Delta(delta));
    }

    fn scroll_files(&mut self, focus: Focus, delta: i32) {
        self.update_section_scroll(focus, ScrollMsg::Delta(delta));
    }

    fn scroll_files_in_focus(&mut self, delta: i32) {
        let focus = self.focus;
        self.scroll_files(focus, delta);
    }

    fn ensure_selected_visible(&mut self) {
        match self.focus {
            Focus::Unstaged => {
                if (self.selected_unstaged as f32) < self.unstaged_scroll.offset() {
                    self.unstaged_scroll
                        .set_offset(self.selected_unstaged as f32);
                }
            }
            Focus::Staged => {
                if (self.selected_staged as f32) < self.staged_scroll.offset() {
                    self.staged_scroll.set_offset(self.selected_staged as f32);
                }
            }
        }
    }

    fn update_section_scroll(&mut self, focus: Focus, msg: ScrollMsg) {
        if matches!(msg, ScrollMsg::Delta(_)) && self.entries_for_focus(focus).is_empty() {
            return;
        }
        self.scroll_state_mut(focus).update(msg);
        // if focus == self.focus {
        //     self.ensure_selected_visible();
        // }
    }

    fn sync_scroll_to_selected(&mut self) {
        self.ensure_selected_visible();
    }

    fn clamp_file_scrolls(&mut self) {
        if self.unstaged.is_empty() {
            self.unstaged_scroll.reset();
        } else if self.unstaged_scroll.offset() as usize >= self.unstaged.len() {
            self.unstaged_scroll
                .set_offset(self.unstaged.len().saturating_sub(1) as f32);
        }
        if self.staged.is_empty() {
            self.staged_scroll.reset();
        } else if self.staged_scroll.offset() as usize >= self.staged.len() {
            self.staged_scroll
                .set_offset(self.staged.len().saturating_sub(1) as f32);
        }
    }
}

#[derive(Clone, Debug)]
enum Msg {
    KeyPressed(Key),
    Quit,
    ToggleStage,
    FocusUnstaged,
    FocusStaged,
    MoveSelectionUp,
    MoveSelectionDown,
    ScrollFiles(i32),
    ScrollDiff(i32),
    RefreshStatus,
    OpenCommitModal,
    OpenDeleteModal,
    ToggleShortcutsHelp,
    SubmitCommit,
    CancelCommit,
    ConfirmDelete,
    CancelDelete,
    BackspaceCommit,
    Staged(SectionMsg),
    Unstaged(SectionMsg),
    DiffScroll(ScrollMsg),
    UnstagedScroll(ScrollMsg),
    StagedScroll(ScrollMsg),
}

#[derive(Clone, Debug)]
enum SectionMsg {
    ActivateFile(usize),
    ToggleStageEntry(usize),
}

#[derive(Clone, Debug)]
struct DiffLine {
    spans: Vec<TextSpan>,
}

impl DiffLine {
    fn plain(content: impl Into<String>) -> Self {
        let content = content.into();
        Self {
            spans: vec![TextSpan::new(content, Style::default())],
        }
    }

    fn styled(content: impl Into<String>, style: Style) -> Self {
        let content = content.into();
        if style == Style::default() {
            return Self::plain(content);
        }
        Self {
            spans: vec![TextSpan::new(content, style)],
        }
    }

    fn from_spans(spans: Vec<TextSpan>) -> Self {
        Self { spans }
    }
}

struct GitStatus {
    unstaged: Vec<FileEntry>,
    staged: Vec<FileEntry>,
    branch: Option<String>,
}

fn load_git_status() -> Result<GitStatus> {
    let output = run_git(["status", "--porcelain"])?;

    let mut unstaged = Vec::new();
    let mut staged = Vec::new();

    for line in output.lines() {
        if line.trim().is_empty() {
            continue;
        }

        if let Some(parsed) = ParsedStatus::parse(line) {
            if parsed.include_unstaged() {
                unstaged.push(FileEntry::new(
                    parsed.path.clone(),
                    parsed.unstaged_code,
                    parsed.unstaged_display(),
                ));
            }

            if parsed.include_staged() {
                staged.push(FileEntry::new(
                    parsed.path.clone(),
                    parsed.staged_code,
                    parsed.staged_display(),
                ));
            }
        }
    }

    let branch = load_current_branch();

    Ok(GitStatus {
        unstaged,
        staged,
        branch,
    })
}

fn load_current_branch() -> Option<String> {
    let branch = run_git(["symbolic-ref", "--short", "HEAD"])
        .or_else(|_| run_git(["rev-parse", "--short", "HEAD"]))
        .ok()?;

    let trimmed = branch.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

struct ParsedStatus {
    staged_code: char,
    unstaged_code: char,
    path: String,
    display_path: String,
}

impl ParsedStatus {
    fn parse(line: &str) -> Option<Self> {
        if line.len() < 4 {
            return None;
        }

        let mut chars = line.chars();
        let staged_code = chars.next()?;
        let unstaged_code = chars.next()?;

        // Skip the space after status codes.
        let remainder = line[3..].to_string();
        let path = remainder
            .split(" -> ")
            .last()
            .map(str::to_string)
            .unwrap_or_else(|| remainder.clone());

        Some(Self {
            staged_code,
            unstaged_code,
            path,
            display_path: remainder,
        })
    }

    fn include_unstaged(&self) -> bool {
        self.unstaged_code != ' ' || self.staged_code == '?'
    }

    fn include_staged(&self) -> bool {
        self.staged_code != ' ' && self.staged_code != '?'
    }

    fn unstaged_display(&self) -> String {
        format!("{} {}", self.unstaged_code, self.display_path)
    }

    fn staged_display(&self) -> String {
        format!("{} {}", self.staged_code, self.display_path)
    }
}

fn diff_for(entry: &FileEntry, focus: Focus) -> Result<Vec<DiffLine>> {
    let diff_output = match focus {
        Focus::Unstaged => diff_unstaged(entry),
        Focus::Staged => diff_staged(entry),
    }?;

    if diff_output.trim().is_empty() {
        return Ok(Vec::new());
    }

    let versions = load_file_versions(entry, focus)?;
    Ok(build_diff_lines(&diff_output, &versions))
}

fn diff_unstaged(entry: &FileEntry) -> Result<String> {
    if entry.code == '?' {
        return run_git_allow_diff([
            "diff",
            "--color=never",
            "--no-index",
            "--",
            "/dev/null",
            &entry.path,
        ]);
    }

    run_git_allow_diff(["diff", "--color=never", "--", &entry.path])
}

fn diff_staged(entry: &FileEntry) -> Result<String> {
    run_git_allow_diff(["diff", "--cached", "--color=never", "--", &entry.path])
}

struct FileVersions {
    old: Option<FileContent>,
    new: Option<FileContent>,
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
    fn from_source(path: &str, source: String) -> Self {
        let text_lines: Vec<String> = source.lines().map(str::to_string).collect();
        let highlight_lines = match highlight::highlight_lines(Path::new(path), &source) {
            Some(lines) if lines.len() == text_lines.len() => Some(lines),
            _ => None,
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
                read_index_file(&entry.path)?
            }
        }
        Focus::Staged => read_head_file(&entry.path)?,
    };

    let new_source = match focus {
        Focus::Unstaged => read_worktree_file(&entry.path)?,
        Focus::Staged => read_index_file(&entry.path)?,
    };

    Ok(FileVersions {
        old: old_source.map(|content| FileContent::from_source(&entry.path, content)),
        new: new_source.map(|content| FileContent::from_source(&entry.path, content)),
    })
}

fn read_worktree_file(path: &str) -> Result<Option<String>> {
    match fs::read_to_string(path) {
        Ok(content) => Ok(Some(content)),
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(None),
        Err(err) => Err(err).wrap_err_with(|| format!("failed to read {}", path)),
    }
}

fn read_index_file(path: &str) -> Result<Option<String>> {
    git_show(&format!(":{}", path))
}

fn read_head_file(path: &str) -> Result<Option<String>> {
    git_show(&format!("HEAD:{}", path))
}

fn git_show(spec: &str) -> Result<Option<String>> {
    let output = Command::new("git")
        .arg("show")
        .arg(spec)
        .env("GIT_PAGER", "cat")
        .output()
        .wrap_err_with(|| format!("git show {}", spec))?;

    if output.status.success() {
        let stdout = String::from_utf8(output.stdout)
            .map_err(|err| eyre!("git show {} produced non-utf8 output: {}", spec, err))?;
        return Ok(Some(stdout));
    }

    if matches!(output.status.code(), Some(128)) {
        return Ok(None);
    }

    Err(eyre!(
        "git show {} failed: {}",
        spec,
        String::from_utf8_lossy(&output.stderr)
    ))
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
            result.push(DiffLine::from_spans(with_prefix('+', spans)));
            new_line = new_line.saturating_add(1);
            continue;
        }

        if let Some(content) = line.strip_prefix('-') {
            let spans = highlight_with_fallback(versions.old.as_ref(), old_line, content);
            result.push(DiffLine::from_spans(with_prefix('-', spans)));
            old_line = old_line.saturating_add(1);
            continue;
        }

        if let Some(content) = line.strip_prefix(' ') {
            let spans = highlight_with_fallback(versions.new.as_ref(), new_line, content);
            result.push(DiffLine::from_spans(with_prefix(' ', spans)));
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

fn stage_path(path: &str) -> Result<()> {
    run_git(["add", "--", path]).map(|_| ())
}

fn unstage_path(path: &str) -> Result<()> {
    run_git(["reset", "HEAD", "--", path]).map(|_| ())
}

fn run_git<I, S>(args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    run_git_internal(args, false)
}

fn run_git_allow_diff<I, S>(args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    run_git_internal(args, true)
}

fn run_git_internal<I, S>(args: I, allow_diff_exit: bool) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args_vec: Vec<String> = args
        .into_iter()
        .map(|arg| arg.as_ref().to_string_lossy().into_owned())
        .collect();

    let output = Command::new("git")
        .args(&args_vec)
        .env("GIT_PAGER", "cat")
        .output()
        .wrap_err_with(|| format!("failed to execute git {}", args_vec.join(" ")))?;

    let status = output.status;
    let acceptable = status.success() || (allow_diff_exit && matches!(status.code(), Some(1)));

    if !acceptable {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(eyre!(
            "git {} failed: {}",
            args_vec.join(" "),
            stderr.trim()
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn discard_unstaged_changes(path: &str, code: char) -> Result<()> {
    // Untracked files ('?') are removed; tracked files are restored from index/HEAD.
    if code == '?' {
        return run_git(["clean", "-f", "--", path]).map(|_| ());
    }

    // Restore the worktree copy to the index state (discarding unstaged changes).
    run_git(["restore", "--worktree", "--", path]).map(|_| ())
}

fn clamp_index(index: &mut usize, len: usize) {
    if len == 0 {
        *index = 0;
    } else if *index >= len {
        *index = len - 1;
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
