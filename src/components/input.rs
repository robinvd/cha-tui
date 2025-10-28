use ropey::Rope;
use smallvec::{SmallVec, smallvec};
use std::collections::BTreeMap;
use std::ops::Range;
use termwiz::cell::unicode_column_width;

use crate::Style;
use crate::buffer::{CellAttributes, CursorShape};
use crate::dom::{Color, Node, Renderable, renderable};
use crate::event::{Key, KeyCode, MouseEvent};
use crate::render::RenderContext;
use taffy::AvailableSpace;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputMode {
    SingleLine,
    Multiline,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ViewId(u64);

impl ViewId {
    const fn new(id: u64) -> Self {
        Self(id)
    }
}

fn digit_count(mut value: usize) -> usize {
    let mut digits = 1;
    while value >= 10 {
        value /= 10;
        digits += 1;
    }
    digits
}

impl Default for ViewId {
    fn default() -> Self {
        ViewId::new(0)
    }
}

const PRIMARY_VIEW_ID: ViewId = ViewId::new(0);

/// State container for the text input component.
///
/// # Feature Slots
/// - **Highlighting** – Stored in [`HighlightStore`] for future layered
///   syntax/search highlights. *Status: data only; renderer still uses a single base layer.*
/// - **Inline hints (ghost text)** – [`InlineHintStore`] captures uneditable
///   annotations from tooling such as LSP servers. *Status: stored but not rendered yet.*
/// - **Autocomplete** – [`AutocompleteState`] tracks suggestion panels.
///   *Status: not wired into UI yet.*
/// - **Gutter** – [`ViewState::gutter`] reserves metadata for line numbers or markers.
///   *Status: layout/rendering unchanged for now.*
/// - **Undo/redo** – [`EditHistory`] now records past/future stacks and replays
///   [`InputMsg::Undo`] / [`InputMsg::Redo`].
///   *Status: fully wired with default Ctrl+Z / Ctrl+Shift+Z / Ctrl+Y bindings.*
/// - **Multicursor** – [`CursorSet`] holds multiple carets.
///   *Status: only the primary caret is exposed through the public API.*
/// - **Multiple views** – [`views`] carries per-pane data (viewport, cursors).
///   *Status: only the first view (`PRIMARY_VIEW_ID`) is used; callers still interact with a single pane.*
#[derive(Clone, Debug)]
pub struct InputState {
    mode: InputMode,
    document: DocumentState,
    history: EditHistory,
    highlights: HighlightStore,
    last_change: Option<TextChangeSet>,
    #[allow(dead_code)]
    inline_hints: InlineHintStore,
    #[allow(dead_code)]
    autocomplete: AutocompleteState,
    views: Vec<ViewState>,
}

impl Default for InputState {
    fn default() -> Self {
        Self {
            mode: InputMode::SingleLine,
            document: DocumentState::default(),
            history: EditHistory::default(),
            highlights: HighlightStore::default(),

            last_change: None,
            inline_hints: InlineHintStore::default(),
            autocomplete: AutocompleteState::default(),
            views: vec![ViewState::new(PRIMARY_VIEW_ID)],
        }
    }
}

/// Rope-backed document metadata (revision tracking hooks for undo/redo).
#[derive(Clone, Debug)]
struct DocumentState {
    rope: Rope,
    revision: u64,
    is_dirty: bool,
}

impl Default for DocumentState {
    fn default() -> Self {
        Self {
            rope: Rope::new(),
            revision: 0,
            is_dirty: false,
        }
    }
}

/// Per-view state so each pane can track its cursors, viewport, and gutter.
#[derive(Clone, Debug)]
struct ViewState {
    #[allow(dead_code)]
    id: ViewId,
    cursor_set: CursorSet,
    #[allow(dead_code)]
    selection_mode: SelectionMode,
    viewport: Viewport,
    #[allow(dead_code)]
    gutter: GutterState,
}

impl ViewState {
    fn new(id: ViewId) -> Self {
        Self {
            id,
            cursor_set: CursorSet::default(),
            selection_mode: SelectionMode::Character,
            viewport: Viewport::default(),
            gutter: GutterState::default(),
        }
    }

    fn primary_caret(&self) -> &Caret {
        self.cursor_set.primary()
    }

    fn primary_caret_mut(&mut self) -> &mut Caret {
        self.cursor_set.primary_mut()
    }
}

/// Multicursor collection (primary caret first, extras to follow).
#[derive(Clone, Debug)]
struct CursorSet {
    carets: SmallVec<[Caret; 4]>,
    primary: usize,
}

impl CursorSet {
    fn primary(&self) -> &Caret {
        self.carets
            .get(self.primary)
            .unwrap_or_else(|| self.carets.first().expect("at least one caret"))
    }

    fn primary_mut(&mut self) -> &mut Caret {
        let idx = self.primary.min(self.carets.len().saturating_sub(1));
        &mut self.carets[idx]
    }

    fn reset_primary_preferred_column(&mut self) {
        if let Some(caret) = self.carets.get_mut(self.primary) {
            caret.preferred_column = None;
        }
    }
}

impl Default for CursorSet {
    fn default() -> Self {
        Self {
            carets: smallvec![Caret::default()],
            primary: 0,
        }
    }
}

/// Cursor head/anchor pair with an optional preferred column for vertical motion.
#[derive(Clone, Debug, Default)]
struct Caret {
    head: usize,
    anchor: usize,
    preferred_column: Option<usize>,
}
/// Selection granularity placeholder (line/block modes can be added later).
#[derive(Clone, Copy, Debug, Default)]
enum SelectionMode {
    #[default]
    Character,
}

/// Terminal viewport bookkeeping (dimensions + scroll offsets).
#[derive(Clone, Debug, Default)]
struct Viewport {
    cols: usize,
    rows: usize,
    scroll_x: usize,
    scroll_y: usize,
}

/// Gutter configuration per view (line numbers, breakpoints, etc.).
#[derive(Clone, Debug, Default)]
struct GutterState {
    width: usize,
    kind: GutterKind,
    #[allow(dead_code)]
    markers: Vec<GutterMarker>,
}

/// Currently a placeholder for future gutter variants.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum GutterKind {
    #[default]
    None,
    LineNumbers,
}

/// Gutter marker metadata (line-level markers such as diagnostics).
#[derive(Clone, Debug, Default)]
struct GutterMarker {
    #[allow(dead_code)]
    line: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct GutterMetrics {
    kind: GutterKind,
    width: usize,
    digits: usize,
}

impl GutterMetrics {
    fn none() -> Self {
        Self {
            kind: GutterKind::None,
            width: 0,
            digits: 0,
        }
    }
}

impl GutterState {
    fn set_kind(&mut self, kind: GutterKind) {
        if self.kind != kind {
            self.kind = kind;
            self.width = 0;
        }
    }

    fn update_width(&mut self, line_count: usize) {
        self.width = match self.kind {
            GutterKind::None => 0,
            GutterKind::LineNumbers => {
                let digits = digit_count(line_count.max(1));
                digits + 3
            }
        };
    }

    fn metrics(&self, line_count: usize) -> GutterMetrics {
        match self.kind {
            GutterKind::None => GutterMetrics::none(),
            GutterKind::LineNumbers => {
                let digits = digit_count(line_count.max(1));
                let width = digits + 3;
                GutterMetrics {
                    kind: self.kind,
                    width,
                    digits,
                }
            }
        }
    }
}

/// Highlight layers keyed by [`HighlightLayerId`].
#[derive(Clone, Debug, Default)]
struct HighlightStore {
    layers: BTreeMap<HighlightLayerId, HighlightLayer>,
    layer_order: Vec<HighlightLayerId>,
}

/// One highlight layer (e.g., syntax, search results).
#[derive(Clone, Debug, Default)]
struct HighlightLayer {
    /// Invariant: this should always be sorted and non overlapping
    spans: Vec<HighlightSpan>,
    priority: u8,
}

/// Highlight span with style info.
#[derive(Clone, Debug, PartialEq)]
pub struct HighlightSpan {
    /// Invariant: range.start<range.end
    pub range: Range<usize>,
    pub style: Style,
}

impl HighlightSpan {
    pub fn new(range: Range<usize>, style: Style) -> Self {
        Self { range, style }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighlightLayerId(u64);

impl HighlightLayerId {
    pub const fn new(id: u64) -> Self {
        Self(id)
    }
}

impl HighlightLayer {
    fn new(priority: u8, spans: Vec<HighlightSpan>) -> Self {
        let mut layer = Self { priority, spans };
        layer.normalize();
        layer
    }

    fn normalize(&mut self) {
        self.spans.retain(|span| span.range.start < span.range.end);
        self.spans
            .sort_by_key(|span| (span.range.start, span.range.end));
    }
}

impl HighlightStore {
    fn set_layer(
        &mut self,
        id: HighlightLayerId,
        priority: u8,
        spans: impl IntoIterator<Item = HighlightSpan>,
    ) {
        let layer = HighlightLayer::new(priority, spans.into_iter().collect());
        if layer.spans.is_empty() {
            self.layers.remove(&id);
        } else {
            self.layers.insert(id, layer);
        }
        self.sync_layer_order();
    }

    fn clear_layer(&mut self, id: HighlightLayerId) {
        self.layers.remove(&id);
        self.sync_layer_order();
    }

    fn clear(&mut self) {
        self.layers.clear();
        self.layer_order.clear();
    }

    fn resolved_runs<'a>(
        &'a self,
        base_style: &'a Style,
        range: Range<usize>,
    ) -> HighlightRuns<'a> {
        HighlightRuns::new(self, base_style, range)
    }

    fn sync_layer_order(&mut self) {
        self.layer_order
            .retain(|layer_id| self.layers.contains_key(layer_id));

        for id in self.layers.keys().copied() {
            if !self.layer_order.contains(&id) {
                self.layer_order.push(id);
            }
        }

        self.layer_order.sort_by_key(|id| {
            self.layers
                .get(id)
                .map(|layer| (layer.priority, id.0))
                .unwrap_or((u8::MAX, id.0))
        });
    }

    fn apply_edit(&mut self, transaction: &EditTransaction) {
        if self.layers.is_empty() || transaction.is_empty() {
            return;
        }

        for edit in transaction.edits.iter().rev() {
            self.apply_text_edit(edit);
        }

        self.layers.retain(|_, layer| {
            layer.normalize();
            !layer.spans.is_empty()
        });
        self.sync_layer_order();
    }

    fn apply_text_edit(&mut self, edit: &TextEdit) {
        let edit_start = edit.range.start;
        let edit_end = edit.range.end;
        let removed_len = edit_end.saturating_sub(edit_start);
        let inserted_len = edit.inserted.chars().count();
        let delta = inserted_len as isize - removed_len as isize;

        if removed_len == 0 && inserted_len == 0 {
            return;
        }

        for layer in self.layers.values_mut() {
            let mut idx = 0;
            while idx < layer.spans.len() {
                let span = &mut layer.spans[idx];

                if span.range.end <= edit_start {
                    idx += 1;
                    continue;
                }

                if span.range.start >= edit_end {
                    if delta != 0 {
                        Self::shift_range(&mut span.range, delta);
                    }
                    idx += 1;
                    continue;
                }

                let new_start =
                    Self::map_index(span.range.start, edit_start, edit_end, inserted_len, delta);
                let new_end =
                    Self::map_index(span.range.end, edit_start, edit_end, inserted_len, delta);

                if new_start >= new_end {
                    layer.spans.remove(idx);
                    continue;
                }

                span.range.start = new_start;
                span.range.end = new_end;
                idx += 1;
            }
        }
    }

    fn shift_range(range: &mut Range<usize>, delta: isize) {
        if delta == 0 {
            return;
        }

        let start = (range.start as isize + delta).max(0) as usize;
        let end = (range.end as isize + delta).max(0) as usize;
        range.start = start;
        range.end = end;
    }

    fn map_index(
        pos: usize,
        edit_start: usize,
        edit_end: usize,
        inserted_len: usize,
        delta: isize,
    ) -> usize {
        if pos <= edit_start {
            pos
        } else if pos >= edit_end {
            let new_pos = pos as isize + delta;
            if new_pos < 0 { 0 } else { new_pos as usize }
        } else {
            edit_start + inserted_len
        }
    }
}

struct HighlightRuns<'a> {
    base_style: &'a Style,
    pos: usize,
    end: usize,
    cursors: SmallVec<[LayerCursor<'a>; 4]>,
}

impl<'a> HighlightRuns<'a> {
    fn new(store: &'a HighlightStore, base_style: &'a Style, range: Range<usize>) -> Self {
        let Range { mut start, end } = range;
        if start >= end {
            return Self {
                base_style,
                pos: end,
                end,
                cursors: SmallVec::new(),
            };
        }

        if store.layer_order.is_empty() {
            return Self {
                base_style,
                pos: end,
                end,
                cursors: SmallVec::new(),
            };
        }

        let mut cursors: SmallVec<[LayerCursor<'a>; 4]> = SmallVec::new();
        for id in &store.layer_order {
            if let Some(layer) = store.layers.get(id)
                && let Some(cursor) = LayerCursor::new(layer, start, end)
            {
                cursors.push(cursor);
            }
        }

        if cursors.is_empty() {
            start = end;
        }

        Self {
            base_style,
            pos: start,
            end,
            cursors,
        }
    }
}

impl<'a> Iterator for HighlightRuns<'a> {
    type Item = HighlightRun;

    fn next(&mut self) -> Option<Self::Item> {
        while self.pos < self.end {
            let mut next_boundary = self.end;
            let mut style: Option<Style> = None;

            for cursor in self.cursors.iter_mut() {
                cursor.seek(self.pos);

                if let Some(span) = cursor.span_at(self.pos) {
                    let span_end = span.range.end.min(self.end);
                    if span_end < next_boundary {
                        next_boundary = span_end;
                    }
                    let merged = style.get_or_insert_with(|| self.base_style.clone());
                    merged.apply_overlay(&span.style);
                } else if let Some(start) = cursor.upcoming_start()
                    && start > self.pos
                    && start < next_boundary
                    && start < self.end
                {
                    next_boundary = start;
                }
            }

            if let Some(style) = style {
                if next_boundary <= self.pos {
                    self.pos = self.end;
                    return None;
                }
                let start = self.pos;
                let end = next_boundary;
                self.pos = end;
                return Some(HighlightRun {
                    range: start..end,
                    style,
                });
            }

            if next_boundary <= self.pos {
                self.pos = self.end;
                return None;
            }

            self.pos = next_boundary;
        }

        None
    }
}

struct LayerCursor<'a> {
    spans: &'a [HighlightSpan],
    index: usize,
}

impl<'a> LayerCursor<'a> {
    fn new(layer: &'a HighlightLayer, start: usize, end: usize) -> Option<Self> {
        if layer.spans.is_empty() {
            return None;
        }

        let spans = layer.spans.as_slice();
        let mut index = 0;
        while index < spans.len() && spans[index].range.end <= start {
            index += 1;
        }

        if index >= spans.len() {
            return None;
        }

        if spans[index].range.start >= end {
            return None;
        }

        Some(Self { spans, index })
    }

    fn seek(&mut self, pos: usize) {
        while self.index < self.spans.len() && self.spans[self.index].range.end <= pos {
            self.index += 1;
        }
    }

    fn span_at(&self, pos: usize) -> Option<&'a HighlightSpan> {
        let span = self.spans.get(self.index)?;
        if span.range.start <= pos && pos < span.range.end {
            Some(span)
        } else {
            None
        }
    }

    fn upcoming_start(&self) -> Option<usize> {
        self.spans.get(self.index).map(|span| span.range.start)
    }
}

/// Storage for inline hints (LSP-style inlay text).
#[derive(Clone, Debug, Default)]
struct InlineHintStore {
    #[allow(dead_code)]
    hints: Vec<InlineHint>,
}

/// Immutable inline hint injected by tooling.
#[derive(Clone, Debug)]
struct InlineHint {
    #[allow(dead_code)]
    id: InlineHintId,
    #[allow(dead_code)]
    range: Range<usize>,
    #[allow(dead_code)]
    text: Rope,
    #[allow(dead_code)]
    style: Style,
    #[allow(dead_code)]
    kind: InlineHintKind,
}

impl Default for InlineHint {
    fn default() -> Self {
        Self {
            id: InlineHintId(0),
            range: 0..0,
            text: Rope::new(),
            style: Style::default(),
            kind: InlineHintKind::TypeAnnotation,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct InlineHintId(u64);

/// Kind of inline hint (type annotations, parameter names, etc.).
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Default)]
enum InlineHintKind {
    #[default]
    TypeAnnotation,
    ParameterName,
    Other,
}

/// Autocomplete popup state (suggestions + selection).
#[allow(dead_code)]
#[derive(Clone, Debug)]
struct AutocompleteState {
    suggestions: Vec<CompletionItem>,
    trigger_range: Range<usize>,
    active: Option<usize>,
    panel_visible: bool,
    view: ViewId,
}

impl Default for AutocompleteState {
    fn default() -> Self {
        Self {
            suggestions: Vec::new(),
            trigger_range: 0..0,
            active: None,
            panel_visible: false,
            view: PRIMARY_VIEW_ID,
        }
    }
}

/// Renderable completion entry label.
#[derive(Clone, Debug, Default)]
struct CompletionItem {
    #[allow(dead_code)]
    label: String,
}

/// Undo/redo stacks plus helpers to walk history.
#[derive(Clone, Debug, Default)]
struct EditHistory {
    past: Vec<EditCommand>,
    future: Vec<EditCommand>,
}

impl EditHistory {
    fn record(&mut self, edit: EditCommand) {
        self.past.push(edit);
        self.future.clear();
    }

    fn undo(&mut self) -> Option<EditCommand> {
        let edit = self.past.pop()?;
        let inverse = edit.inverse();
        self.future.push(edit);
        Some(inverse)
    }

    fn redo(&mut self) -> Option<EditCommand> {
        let edit = self.future.pop()?;
        let replay = edit.clone();
        self.past.push(edit);
        Some(replay)
    }

    fn clear(&mut self) {
        self.past.clear();
        self.future.clear();
    }

    fn can_undo(&self) -> bool {
        !self.past.is_empty()
    }

    fn can_redo(&self) -> bool {
        !self.future.is_empty()
    }
}

/// Immutable caret state used to restore selection after replaying an edit.
#[derive(Clone, Copy, Debug, Default)]
struct CaretSnapshot {
    head: usize,
    anchor: usize,
}

impl CaretSnapshot {
    fn new(head: usize, anchor: usize) -> Self {
        Self { head, anchor }
    }
}

impl From<&Caret> for CaretSnapshot {
    fn from(value: &Caret) -> Self {
        Self {
            head: value.head,
            anchor: value.anchor,
        }
    }
}

/// Individual edit that can participate in undo/redo.
#[derive(Clone, Debug)]
struct EditCommand {
    before: CaretSnapshot,
    after: CaretSnapshot,
    transaction: EditTransaction,
}

impl EditCommand {
    fn new(before: CaretSnapshot, after: CaretSnapshot, transaction: EditTransaction) -> Self {
        Self {
            before,
            after,
            transaction,
        }
    }

    fn inverse(&self) -> Self {
        Self {
            before: self.after,
            after: self.before,
            transaction: self.transaction.inverse(),
        }
    }
}

/// Ordered collection of non-overlapping text edits.
#[derive(Clone, Debug)]
struct EditTransaction {
    edits: Vec<TextEdit>,
}

/// One applied text change (range, deleted text, inserted text).
#[derive(Clone, Debug)]
pub struct TextChange {
    pub range: Range<usize>,
    pub deleted: String,
    pub inserted: String,
}

/// Collection of applied text changes from a single input transaction.
#[derive(Clone, Debug)]
pub struct TextChangeSet {
    pub changes: Vec<TextChange>,
}

impl TextChangeSet {
    fn from_transaction(transaction: &EditTransaction) -> Self {
        let changes = transaction
            .edits
            .iter()
            .map(|edit| TextChange {
                range: edit.range.clone(),
                deleted: edit.deleted.clone(),
                inserted: edit.inserted.clone(),
            })
            .collect();
        Self { changes }
    }

    pub fn is_empty(&self) -> bool {
        self.changes.is_empty()
    }
}

impl EditTransaction {
    fn new(edits: Vec<TextEdit>) -> Self {
        debug_assert!(Self::non_overlapping(&edits));
        Self { edits }
    }

    fn single(edit: TextEdit) -> Self {
        Self::new(vec![edit])
    }

    fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }

    fn apply(&self, rope: &mut Rope) {
        for edit in self.edits.iter().rev() {
            edit.apply(rope);
        }
    }

    fn inverse(&self) -> Self {
        let edits = self.edits.iter().map(|edit| edit.inverse()).collect();
        EditTransaction { edits }
    }

    fn non_overlapping(edits: &[TextEdit]) -> bool {
        edits.is_empty()
            || edits
                .windows(2)
                .all(|pair| pair[0].range.end <= pair[1].range.start)
    }
}

/// Replace-operation describing removed and inserted text for a range.
#[derive(Clone, Debug)]
struct TextEdit {
    range: Range<usize>,
    deleted: String,
    inserted: String,
}

impl TextEdit {
    fn new(range: Range<usize>, deleted: String, inserted: String) -> Self {
        Self {
            range,
            deleted,
            inserted,
        }
    }

    fn apply(&self, rope: &mut Rope) {
        rope.remove(self.range.clone());
        if !self.inserted.is_empty() {
            rope.insert(self.range.start, &self.inserted);
        }
    }

    fn inverse(&self) -> Self {
        let inserted_len = self.inserted.chars().count();
        TextEdit {
            range: self.range.start..(self.range.start + inserted_len),
            deleted: self.inserted.clone(),
            inserted: self.deleted.clone(),
        }
    }
}

impl InputState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_mode(mode: InputMode) -> Self {
        Self {
            mode,
            ..Self::default()
        }
    }

    pub fn new_multiline() -> Self {
        Self::with_mode(InputMode::Multiline)
    }

    pub fn with_value_and_mode(value: impl Into<String>, mode: InputMode) -> Self {
        let mut state = Self::with_mode(mode);
        state.set_value(value);
        state
    }

    pub fn with_value(value: impl Into<String>) -> Self {
        Self::with_value_and_mode(value, InputMode::SingleLine)
    }

    pub fn with_value_multiline(value: impl Into<String>) -> Self {
        Self::with_value_and_mode(value, InputMode::Multiline)
    }

    fn primary_view(&self) -> &ViewState {
        self.views
            .first()
            .expect("InputState always maintains at least one view")
    }

    fn primary_view_mut(&mut self) -> &mut ViewState {
        self.views
            .first_mut()
            .expect("InputState always maintains at least one view")
    }

    fn primary_caret(&self) -> &Caret {
        self.primary_view().primary_caret()
    }

    fn primary_caret_mut(&mut self) -> &mut Caret {
        self.primary_view_mut().primary_caret_mut()
    }

    fn viewport(&self) -> &Viewport {
        &self.primary_view().viewport
    }

    fn viewport_mut(&mut self) -> &mut Viewport {
        &mut self.primary_view_mut().viewport
    }

    fn anchor_position(&self) -> usize {
        self.primary_caret().anchor
    }

    fn cursor_position_mut(&mut self) -> &mut usize {
        &mut self.primary_caret_mut().head
    }

    fn anchor_position_mut(&mut self) -> &mut usize {
        &mut self.primary_caret_mut().anchor
    }

    fn caret_snapshot(&self) -> CaretSnapshot {
        self.primary_caret().into()
    }

    fn set_caret_snapshot(&mut self, snapshot: CaretSnapshot) {
        let caret = self.primary_caret_mut();
        caret.head = snapshot.head;
        caret.anchor = snapshot.anchor;
    }

    fn refresh_gutter_metrics(&mut self) {
        let line_count = self.line_count();
        self.primary_view_mut().gutter.update_width(line_count);
    }

    fn commit_edit(&mut self, command: EditCommand) -> bool {
        if command.transaction.is_empty() {
            return false;
        }
        self.apply_edit(&command);
        self.history.record(command);
        self.mark_document_dirty();
        true
    }

    fn apply_edit(&mut self, command: &EditCommand) {
        command.transaction.apply(&mut self.document.rope);
        self.highlights.apply_edit(&command.transaction);
        self.last_change = Some(TextChangeSet::from_transaction(&command.transaction));
        self.set_caret_snapshot(command.after);
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        self.refresh_gutter_metrics();
    }

    fn mark_document_dirty(&mut self) {
        self.document.revision = self.document.revision.wrapping_add(1);
        self.document.is_dirty = true;
    }

    fn rope_slice_to_string(&self, range: Range<usize>) -> String {
        self.document.rope.slice(range).to_string()
    }

    fn preferred_column(&self) -> Option<usize> {
        self.primary_caret().preferred_column
    }

    fn set_preferred_column(&mut self, value: Option<usize>) {
        self.primary_caret_mut().preferred_column = value;
    }

    pub fn value(&self) -> String {
        self.document.rope.to_string()
    }

    pub fn rope(&self) -> &Rope {
        &self.document.rope
    }

    pub fn take_text_changes(&mut self) -> Option<TextChangeSet> {
        self.last_change.take()
    }

    pub fn len_chars(&self) -> usize {
        self.document.rope.len_chars()
    }

    pub fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }

    pub fn set_highlight_layer(
        &mut self,
        id: HighlightLayerId,
        priority: u8,
        spans: impl IntoIterator<Item = HighlightSpan>,
    ) {
        self.highlights.set_layer(id, priority, spans);
    }

    pub fn clear_highlight_layer(&mut self, id: HighlightLayerId) {
        self.highlights.clear_layer(id);
    }

    pub fn clear_highlights(&mut self) {
        self.highlights.clear();
    }

    pub fn cursor(&self) -> usize {
        self.primary_caret().head
    }

    pub fn set_line_number_gutter(&mut self, enabled: bool) {
        let kind = if enabled {
            GutterKind::LineNumbers
        } else {
            GutterKind::None
        };
        {
            let gutter = &mut self.primary_view_mut().gutter;
            gutter.set_kind(kind);
        }
        self.refresh_gutter_metrics();
    }

    pub fn line_number_gutter_enabled(&self) -> bool {
        matches!(self.primary_view().gutter.kind, GutterKind::LineNumbers)
    }

    pub fn scroll_x(&self) -> usize {
        self.viewport().scroll_x
    }

    pub fn scroll_y(&self) -> usize {
        self.viewport().scroll_y
    }

    pub fn mode(&self) -> InputMode {
        self.mode
    }

    fn gutter_width(&self) -> usize {
        self.primary_view().gutter.metrics(self.line_count()).width
    }

    pub fn is_multiline(&self) -> bool {
        matches!(self.mode, InputMode::Multiline)
    }

    pub fn set_mode(&mut self, mode: InputMode) {
        if self.mode != mode {
            self.mode = mode;
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        }
    }

    pub fn selection(&self) -> Option<(usize, usize)> {
        let cursor = self.primary_caret().head;
        let anchor = self.primary_caret().anchor;
        if cursor == anchor {
            None
        } else if cursor < anchor {
            Some((cursor, anchor))
        } else {
            Some((anchor, cursor))
        }
    }

    pub fn clear(&mut self) {
        self.document.rope = Rope::new();
        {
            let caret = self.primary_view_mut().primary_caret_mut();
            caret.head = 0;
            caret.anchor = 0;
            caret.preferred_column = None;
        }
        {
            let viewport = &mut self.primary_view_mut().viewport;
            viewport.scroll_x = 0;
            viewport.scroll_y = 0;
        }
        self.reset_preferred_column();
        self.history.clear();
        self.document.revision = self.document.revision.wrapping_add(1);
        self.document.is_dirty = false;
        self.last_change = None;
        self.refresh_gutter_metrics();
    }

    pub fn set_value(&mut self, value: impl Into<String>) {
        let value = value.into();
        self.document.rope = Rope::from_str(&value);
        let len = self.len_chars();
        {
            let caret = self.primary_view_mut().primary_caret_mut();
            caret.head = len;
            caret.anchor = len;
            caret.preferred_column = None;
        }
        {
            let viewport = &mut self.primary_view_mut().viewport;
            viewport.scroll_x = 0;
            viewport.scroll_y = 0;
        }
        self.reset_preferred_column();
        self.history.clear();
        self.document.revision = self.document.revision.wrapping_add(1);
        self.document.is_dirty = false;
        self.last_change = None;
        self.refresh_gutter_metrics();
    }

    pub fn undo(&mut self) -> bool {
        if let Some(edit) = self.history.undo() {
            self.apply_edit(&edit);
            self.mark_document_dirty();
            true
        } else {
            false
        }
    }

    pub fn redo(&mut self) -> bool {
        if let Some(edit) = self.history.redo() {
            self.apply_edit(&edit);
            self.mark_document_dirty();
            true
        } else {
            false
        }
    }

    pub fn can_undo(&self) -> bool {
        self.history.can_undo()
    }

    pub fn can_redo(&self) -> bool {
        self.history.can_redo()
    }

    fn reset_preferred_column(&mut self) {
        self.primary_view_mut()
            .cursor_set
            .reset_primary_preferred_column();
    }

    fn cursor_line(&self) -> usize {
        let idx = self.cursor().min(self.len_chars());
        self.document.rope.char_to_line(idx)
    }

    fn line_count(&self) -> usize {
        self.document.rope.len_lines().max(1)
    }

    fn column_in_line(&self, line_index: usize, char_index: usize) -> usize {
        let start = self.document.rope.line_to_char(line_index).min(char_index);
        self.display_width_between(start, char_index)
    }

    fn display_width_between(&self, start: usize, end: usize) -> usize {
        self.document
            .rope
            .slice(start..end)
            .chars()
            .fold(0usize, |acc, ch| {
                if ch == '\n' {
                    0
                } else {
                    acc + Self::char_width(ch)
                }
            })
    }

    fn index_at_line_column(&self, line_index: usize, column: usize) -> usize {
        let line_start = self
            .document
            .rope
            .line_to_char(line_index)
            .min(self.len_chars());
        let line_end = self
            .document
            .rope
            .line_to_char((line_index + 1).min(self.document.rope.len_lines()))
            .min(self.len_chars());

        let mut idx = line_start;
        let mut col = 0usize;
        while idx < line_end {
            let ch = self.document.rope.char(idx);
            if ch == '\n' {
                break;
            }
            let width = Self::char_width(ch);
            if col + width > column {
                break;
            }
            col += width;
            idx += 1;
        }

        idx
    }

    pub fn update(&mut self, msg: InputMsg) -> bool {
        self.last_change = None;
        match msg {
            InputMsg::InsertChar(ch) => self.insert_text(&ch.to_string()),
            InputMsg::InsertText(text) => self.insert_text(&text),
            InputMsg::DeleteBackward => self.delete_backward(),
            InputMsg::DeleteWordBackward => self.delete_word_backward(),
            InputMsg::DeleteToStart => self.delete_to_start(),
            InputMsg::DeleteToEnd => self.delete_to_end(),
            InputMsg::Undo => self.undo(),
            InputMsg::Redo => self.redo(),
            InputMsg::MoveLeft { extend } => self.move_left(extend),
            InputMsg::MoveRight { extend } => self.move_right(extend),
            InputMsg::MoveUp { extend } => self.move_up(extend),
            InputMsg::MoveDown { extend } => self.move_down(extend),
            InputMsg::MoveToStart { extend } => self.move_to_index(0, extend),
            InputMsg::MoveToEnd { extend } => {
                let len = self.len_chars();
                self.move_to_index(len, extend)
            }
            InputMsg::MoveLineStart { extend } => self.move_line_start(extend),
            InputMsg::MoveLineEnd { extend } => self.move_line_end(extend),
            InputMsg::MoveWordLeft { extend } => {
                let target = self.word_start_before(self.cursor());
                self.move_to_index(target, extend)
            }
            InputMsg::MoveWordRight { extend } => {
                let target = self.next_word_boundary(self.cursor());
                self.move_to_index(target, extend)
            }
            InputMsg::Pointer {
                column,
                row,
                click_count,
            } => self.handle_pointer(column as usize, row as usize, click_count.max(1)),
            InputMsg::Replace(text) => {
                self.set_value(text);
                self.ensure_cursor_visible();
                true
            }
            InputMsg::SelectRange { start, end } => {
                self.set_selection(start, end);
                self.ensure_cursor_visible();
                true
            }
            InputMsg::ClearSelection => {
                let cursor = self.cursor();
                if cursor != self.anchor_position() {
                    *self.anchor_position_mut() = cursor;
                    self.reset_preferred_column();
                    self.ensure_cursor_visible();
                    true
                } else {
                    false
                }
            }
            InputMsg::SetViewportSize { cols, rows } => {
                let cols = cols.max(1);
                let rows = rows.max(1);
                let viewport = self.viewport_mut();
                let changed = viewport.cols != cols || viewport.rows != rows;
                viewport.cols = cols;
                viewport.rows = rows;
                self.ensure_cursor_visible();
                changed
            }
        }
    }

    fn insert_text(&mut self, text: &str) -> bool {
        if text.is_empty() {
            return false;
        }

        let sanitized = if self.is_multiline() {
            let sanitized = text.replace('\r', "");
            if sanitized.is_empty() {
                return false;
            }
            sanitized
        } else {
            text.replace(['\n', '\r'], " ")
        };
        if sanitized.is_empty() {
            return false;
        }

        let before = self.caret_snapshot();
        let inserted_len = sanitized.chars().count();
        let (text_edit, new_pos) = if let Some((start, end)) = self.selection() {
            let deleted = self.rope_slice_to_string(start..end);
            (
                TextEdit::new(start..end, deleted, sanitized.clone()),
                start + inserted_len,
            )
        } else {
            let cursor = self.cursor();
            (
                TextEdit::new(cursor..cursor, String::new(), sanitized.clone()),
                cursor + inserted_len,
            )
        };

        let after = CaretSnapshot::new(new_pos, new_pos);
        let transaction = EditTransaction::single(text_edit);
        let command = EditCommand::new(before, after, transaction);
        self.commit_edit(command)
    }

    fn delete_backward(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        if self.cursor() == 0 {
            return false;
        }

        let prev = self.cursor() - 1;
        let range = prev..self.cursor();
        let deleted = self.rope_slice_to_string(range.clone());
        let before = self.caret_snapshot();
        let after = CaretSnapshot::new(prev, prev);
        let text_edit = TextEdit::new(range, deleted, String::new());
        let transaction = EditTransaction::single(text_edit);
        let command = EditCommand::new(before, after, transaction);
        self.commit_edit(command)
    }

    fn delete_word_backward(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        if self.cursor() == 0 {
            return false;
        }

        let boundary = self.word_start_before(self.cursor());
        if boundary == self.cursor() {
            return false;
        }

        let range = boundary..self.cursor();
        let deleted = self.rope_slice_to_string(range.clone());
        let before = self.caret_snapshot();
        let after = CaretSnapshot::new(boundary, boundary);
        let text_edit = TextEdit::new(range, deleted, String::new());
        let transaction = EditTransaction::single(text_edit);
        let command = EditCommand::new(before, after, transaction);
        self.commit_edit(command)
    }

    fn delete_to_start(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        if self.cursor() == 0 {
            self.ensure_cursor_visible();
            return false;
        }

        let range = 0..self.cursor();
        let deleted = self.rope_slice_to_string(range.clone());
        let before = self.caret_snapshot();
        let after = CaretSnapshot::new(0, 0);
        let text_edit = TextEdit::new(range, deleted, String::new());
        let transaction = EditTransaction::single(text_edit);
        let command = EditCommand::new(before, after, transaction);
        self.commit_edit(command)
    }

    fn delete_to_end(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        let len = self.len_chars();
        if self.cursor() == len {
            return false;
        }
        let start = self.cursor();
        let range = start..len;
        let deleted = self.rope_slice_to_string(range.clone());
        let before = self.caret_snapshot();
        let after = CaretSnapshot::new(start, start);
        let text_edit = TextEdit::new(range, deleted, String::new());
        let transaction = EditTransaction::single(text_edit);
        let command = EditCommand::new(before, after, transaction);
        self.commit_edit(command)
    }

    fn move_left(&mut self, extend: bool) -> bool {
        if !extend && let Some((start, _)) = self.selection() {
            *self.cursor_position_mut() = start;
            *self.anchor_position_mut() = start;
            self.reset_preferred_column();
            self.ensure_cursor_visible();
            return true;
        }

        if self.cursor() == 0 {
            if !extend {
                *self.anchor_position_mut() = self.cursor();
            }
            return false;
        }

        let new_pos = self.cursor() - 1;
        *self.cursor_position_mut() = new_pos;
        if !extend {
            *self.anchor_position_mut() = new_pos;
        }
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn ensure_cursor_visible(&mut self) {
        let gutter_width = self.gutter_width();
        let viewport_cols = {
            let viewport = self.viewport();
            let content_cols = viewport.cols.saturating_sub(gutter_width);
            content_cols.max(1)
        };
        let col = self.display_width_between(0, self.cursor());
        {
            let viewport = self.viewport_mut();
            let min_scroll = col.saturating_sub(viewport_cols.saturating_sub(1));
            if viewport.scroll_x > min_scroll {
                viewport.scroll_x = min_scroll;
            }
            if col < viewport.scroll_x {
                viewport.scroll_x = col;
            } else if col >= viewport.scroll_x + viewport_cols {
                viewport.scroll_x = col + 1 - viewport_cols;
            }
        }

        if self.is_multiline() {
            let viewport_rows = {
                let viewport = self.viewport();
                viewport.rows.max(1)
            };
            let cursor_line = self.cursor_line();
            let max_scroll_y = self.line_count().saturating_sub(viewport_rows);

            let viewport = self.viewport_mut();
            if cursor_line < viewport.scroll_y {
                viewport.scroll_y = cursor_line;
            } else if cursor_line >= viewport.scroll_y + viewport_rows {
                viewport.scroll_y = cursor_line + 1 - viewport_rows;
            }

            if viewport.scroll_y > max_scroll_y {
                viewport.scroll_y = max_scroll_y;
            }
        } else {
            self.viewport_mut().scroll_y = 0;
        }
    }

    fn move_right(&mut self, extend: bool) -> bool {
        if !extend && let Some((_, end)) = self.selection() {
            *self.cursor_position_mut() = end;
            *self.anchor_position_mut() = end;
            self.reset_preferred_column();
            self.ensure_cursor_visible();
            return true;
        }

        let len = self.len_chars();
        if self.cursor() >= len {
            if !extend {
                *self.anchor_position_mut() = self.cursor();
            }
            return false;
        }

        let new_pos = self.cursor() + 1;
        *self.cursor_position_mut() = new_pos;
        if !extend {
            *self.anchor_position_mut() = new_pos;
        }
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn move_up(&mut self, extend: bool) -> bool {
        self.move_vertical(-1, extend)
    }

    fn move_down(&mut self, extend: bool) -> bool {
        self.move_vertical(1, extend)
    }

    fn move_vertical(&mut self, line_delta: isize, extend: bool) -> bool {
        if !self.is_multiline() {
            if !extend {
                *self.anchor_position_mut() = self.cursor();
            }
            return false;
        }

        let line_count = self.line_count();
        if line_count <= 1 {
            if !extend {
                *self.anchor_position_mut() = self.cursor();
            }
            return false;
        }

        let current_line = self.cursor_line();
        let target_line = match line_delta {
            d if d < 0 => {
                if current_line == 0 {
                    if !extend {
                        *self.anchor_position_mut() = self.cursor();
                    }
                    return false;
                }
                current_line - 1
            }
            d if d > 0 => {
                if current_line + 1 >= line_count {
                    if !extend {
                        *self.anchor_position_mut() = self.cursor();
                    }
                    return false;
                }
                current_line + 1
            }
            _ => current_line,
        };

        let desired_col = match self.preferred_column() {
            Some(col) => col,
            None => {
                let col = self.column_in_line(current_line, self.cursor());
                self.set_preferred_column(Some(col));
                col
            }
        };

        let target_index = self.index_at_line_column(target_line, desired_col);

        if target_index == self.cursor() {
            if !extend {
                *self.anchor_position_mut() = target_index;
            }
            self.ensure_cursor_visible();
            return false;
        }

        *self.cursor_position_mut() = target_index;
        if !extend {
            *self.anchor_position_mut() = target_index;
        }
        self.set_preferred_column(Some(desired_col));
        self.ensure_cursor_visible();
        true
    }

    fn move_line_start(&mut self, extend: bool) -> bool {
        let line_index = self.cursor_line();
        let (line_start, _) = self.line_bounds(line_index);
        self.move_to_index(line_start, extend)
    }

    fn move_line_end(&mut self, extend: bool) -> bool {
        let line_index = self.cursor_line();
        let (_, line_end) = self.line_bounds(line_index);
        self.move_to_index(line_end, extend)
    }

    fn move_to_index(&mut self, index: usize, extend: bool) -> bool {
        let clamped = index.min(self.len_chars());
        if !extend {
            *self.anchor_position_mut() = clamped;
        }
        if self.cursor() == clamped && self.anchor_position() == clamped {
            return false;
        }
        *self.cursor_position_mut() = clamped;
        if !extend {
            *self.anchor_position_mut() = clamped;
        }
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
    }

    fn set_selection(&mut self, start: usize, end: usize) {
        let len = self.len_chars();
        let start = start.min(len);
        let end = end.min(len);
        *self.anchor_position_mut() = start;
        *self.cursor_position_mut() = end;
        self.reset_preferred_column();
        self.ensure_cursor_visible();
    }

    fn delete_selection(&mut self) -> bool {
        if let Some((start, end)) = self.selection() {
            let range = start..end;
            let before = self.caret_snapshot();
            let deleted = self.rope_slice_to_string(range.clone());
            let after = CaretSnapshot::new(range.start, range.start);
            let text_edit = TextEdit::new(range, deleted, String::new());
            let transaction = EditTransaction::single(text_edit);
            let command = EditCommand::new(before, after, transaction);
            self.commit_edit(command)
        } else {
            false
        }
    }

    fn handle_pointer(&mut self, column: usize, row: usize, click_count: u8) -> bool {
        let index = self.column_to_index(column, row);

        match click_count {
            1 => self.move_to_index(index, false),
            2 => {
                let (start, end) = self.word_range_at(index);
                self.set_selection(start, end);
                true
            }
            3 => {
                let len = self.len_chars();
                self.set_selection(0, len);
                true
            }
            _ => self.move_to_index(index, false),
        }
    }

    fn column_to_index(&self, column: usize, row: usize) -> usize {
        if self.is_multiline() {
            let line_count = self.line_count();
            let max_index = line_count - 1;
            let scroll_y = self.viewport().scroll_y;
            let absolute_row = scroll_y.saturating_add(row).min(max_index);
            self.index_at_line_column(absolute_row, column)
        } else {
            let mut consumed = 0usize;
            let mut index = 0usize;
            for ch in self.document.rope.chars() {
                let width = Self::char_width(ch);
                if column < consumed + width {
                    return index;
                }
                consumed += width;
                index += 1;
            }
            index
        }
    }

    fn prev_word_boundary(&self, mut index: usize) -> usize {
        if index == 0 {
            return 0;
        }

        if index > self.len_chars() {
            index = self.len_chars();
        }

        index = index.saturating_sub(1);

        let mut kind = self.word_kind_at(index).unwrap_or(WordKind::Whitespace);
        while index > 0 {
            let prev_kind = self.word_kind_at(index - 1).unwrap_or(WordKind::Whitespace);
            if prev_kind != kind {
                break;
            }
            index -= 1;
            kind = prev_kind;
        }
        index
    }

    fn next_word_boundary(&self, mut index: usize) -> usize {
        let len = self.len_chars();
        if index >= len {
            return len;
        }

        let kind = self.word_kind_at(index).unwrap_or(WordKind::Whitespace);
        while index < len {
            let next_kind = self.word_kind_at(index).unwrap_or(WordKind::Whitespace);
            if next_kind != kind {
                break;
            }
            index += 1;
        }
        index
    }

    fn word_start_before(&self, mut index: usize) -> usize {
        let len = self.len_chars();
        if index > len {
            index = len;
        }

        while index > 0 {
            match self.word_kind_at(index - 1) {
                Some(WordKind::Whitespace) => index -= 1,
                _ => break,
            }
        }

        if index == 0 {
            0
        } else {
            self.prev_word_boundary(index)
        }
    }

    fn word_range_at(&self, index: usize) -> (usize, usize) {
        let len = self.len_chars();
        if len == 0 {
            return (0, 0);
        }

        let idx = index.min(len.saturating_sub(1));
        let kind = self.word_kind_at(idx).unwrap_or(WordKind::Whitespace);
        let mut start = idx;
        while start > 0 {
            let prev_kind = self.word_kind_at(start - 1).unwrap_or(WordKind::Whitespace);
            if prev_kind != kind {
                break;
            }
            start -= 1;
        }

        let mut end = idx + 1;
        while end < len {
            let next_kind = self.word_kind_at(end).unwrap_or(WordKind::Whitespace);
            if next_kind != kind {
                break;
            }
            end += 1;
        }

        (start, end)
    }

    fn word_kind_at(&self, index: usize) -> Option<WordKind> {
        if index >= self.len_chars() {
            return None;
        }

        let ch = self.document.rope.char(index);
        Some(WordKind::classify(ch))
    }

    fn line_bounds(&self, line_index: usize) -> (usize, usize) {
        let len = self.len_chars();
        let start = self.document.rope.line_to_char(line_index).min(len);
        let mut end = self
            .document
            .rope
            .line_to_char((line_index + 1).min(self.document.rope.len_lines()))
            .min(len);

        if end > start && end > 0 {
            let last_idx = end - 1;
            if last_idx < len && self.document.rope.char(last_idx) == '\n' {
                end -= 1;
            }
        }

        (start, end)
    }

    fn char_width(ch: char) -> usize {
        if ch == '\n' {
            return 0;
        }
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        unicode_column_width(s, None).max(1)
    }
}

/// Visual customization knobs for the input widget (text, selection, cursor).
#[derive(Clone, Debug)]
pub struct InputStyle {
    pub text: Style,
    pub selection: Style,
    pub cursor: Style,
    pub cursor_symbol: &'static str,
    pub gutter: Style,
}

impl Default for InputStyle {
    fn default() -> Self {
        let selection = Style {
            bg: Some(Color::BrightBlue),
            fg: Some(Color::White),
            ..Style::default()
        };

        let cursor = Style {
            bg: Some(Color::White),
            fg: Some(Color::Black),
            bold: true,
            ..Style::default()
        };

        let gutter = Style {
            fg: Some(Color::BrightBlack),
            dim: true,
            ..Style::default()
        };

        Self {
            text: Style::default(),
            selection,
            cursor,
            cursor_symbol: " ",
            gutter,
        }
    }
}

#[derive(Clone, Debug)]
pub enum InputMsg {
    InsertChar(char),
    InsertText(String),
    DeleteBackward,
    DeleteWordBackward,
    DeleteToStart,
    DeleteToEnd,
    Undo,
    Redo,
    MoveLeft {
        extend: bool,
    },
    MoveRight {
        extend: bool,
    },
    MoveUp {
        extend: bool,
    },
    MoveDown {
        extend: bool,
    },
    MoveToStart {
        extend: bool,
    },
    MoveToEnd {
        extend: bool,
    },
    MoveLineStart {
        extend: bool,
    },
    MoveLineEnd {
        extend: bool,
    },
    MoveWordLeft {
        extend: bool,
    },
    MoveWordRight {
        extend: bool,
    },
    Pointer {
        column: u16,
        row: u16,
        click_count: u8,
    },
    Replace(String),
    SelectRange {
        start: usize,
        end: usize,
    },
    ClearSelection,
    SetViewportSize {
        cols: usize,
        rows: usize,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WordKind {
    Whitespace,
    Alphanumeric,
    Other,
}

impl WordKind {
    fn classify(ch: char) -> Self {
        if ch.is_whitespace() {
            Self::Whitespace
        } else if ch.is_alphanumeric() || ch == '_' {
            Self::Alphanumeric
        } else {
            Self::Other
        }
    }
}

pub fn input<Msg>(
    id: &'static str,
    state: &InputState,
    style: &InputStyle,
    map_msg: impl Fn(InputMsg) -> Msg + 'static,
) -> Node<Msg>
where
    Msg: 'static,
{
    use std::rc::Rc;

    let input_renderable = InputRenderable::new(state, style);
    let mut node = renderable::<Msg>(input_renderable)
        .with_id(id)
        .with_scroll_x(state.scroll_x() as f32)
        .with_scroll(state.scroll_y() as f32);

    let handler = Rc::new(map_msg);
    let mouse_handler = handler.clone();

    node = node.on_mouse(move |event: MouseEvent| {
        if event.buttons.left && event.click_count > 0 {
            let msg = InputMsg::Pointer {
                column: event.local_x,
                row: event.local_y,
                click_count: event.click_count,
            };
            Some(mouse_handler(msg))
        } else {
            None
        }
    });

    // Track viewport width via resize and update state
    let resize_handler = handler.clone();
    node = node.on_resize(move |layout| {
        let cols = layout.size.width.max(0.0).round() as usize;
        let rows = layout.size.height.max(0.0).round() as usize;
        Some(resize_handler(InputMsg::SetViewportSize { cols, rows }))
    });

    node
}

#[derive(Clone, Debug, PartialEq)]
struct HighlightRun {
    range: Range<usize>,
    style: Style,
}

/// Snapshot of the input plus styling information passed to the renderer.
#[derive(Clone, Debug)]
struct InputRenderable {
    rope: Rope,
    cursor: usize,
    selection: Option<(usize, usize)>,
    base_style: Style,
    selection_style: Style,
    cursor_style: Style,
    cursor_symbol: String,
    cursor_symbol_width: usize,
    max_width: usize,
    line_count: usize,
    cursor_line: usize,
    len_chars: usize,
    highlight_store: HighlightStore,
    gutter_metrics: GutterMetrics,
    gutter_style: Style,
}

#[derive(Clone, Debug, PartialEq)]
enum RunStyle {
    Base,
    Selection,
    Cursor,
    Highlight(Style),
}

impl InputRenderable {
    /// Create a new renderable
    ///
    /// This should not do any large amount of processing as this in the hot
    /// path for rendering. For example spans should be sorted when they are
    /// set and not here.
    fn new(state: &InputState, style: &InputStyle) -> Self {
        let rope = state.document.rope.clone();
        let len_chars = rope.len_chars();
        let cursor = state.cursor().min(len_chars);
        let selection = state.selection();

        let base_style = style.text.clone();
        let selection_style = style.selection.clone();
        let cursor_style = style.cursor.clone();

        let cursor_symbol = style.cursor_symbol.to_string();
        let symbol_width_sum: usize = cursor_symbol
            .chars()
            .map(InputState::char_width)
            .filter(|width| *width > 0)
            .sum();
        let cursor_symbol_width = if symbol_width_sum == 0 {
            1
        } else {
            symbol_width_sum
        };

        let line_count = rope.len_lines().max(1);
        let gutter_metrics = state.primary_view().gutter.metrics(line_count);
        let gutter_style = style.gutter.clone();
        let cursor_line = rope.char_to_line(cursor);
        let max_width = Self::compute_max_width(
            &rope,
            selection,
            cursor,
            cursor_line,
            len_chars,
            cursor_symbol_width,
            gutter_metrics.width,
        );

        Self {
            rope,
            cursor,
            selection,
            base_style,
            selection_style,
            cursor_style,
            cursor_symbol,
            cursor_symbol_width,
            max_width,
            line_count,
            cursor_line,
            len_chars,
            highlight_store: state.highlights.clone(),
            gutter_metrics,
            gutter_style,
        }
    }

    fn compute_max_width(
        rope: &Rope,
        selection: Option<(usize, usize)>,
        cursor: usize,
        cursor_line: usize,
        len_chars: usize,
        cursor_symbol_width: usize,
        gutter_width: usize,
    ) -> usize {
        let mut max_width = 0;
        let cursor_char = (cursor < len_chars).then(|| rope.char(cursor));

        for line_idx in 0..rope.len_lines().max(1) {
            let mut width = Self::line_display_width(rope, line_idx);
            if selection.is_none()
                && line_idx == cursor_line
                && (cursor == len_chars || matches!(cursor_char, Some('\n')))
            {
                width += cursor_symbol_width;
            }
            max_width = max_width.max(width);
        }

        if max_width == 0 && selection.is_none() {
            max_width = cursor_symbol_width.max(1);
        }

        max_width = max_width.max(1);

        max_width + gutter_width
    }

    fn line_display_width(rope: &Rope, line_idx: usize) -> usize {
        rope.line(line_idx)
            .chars()
            .filter(|&ch| ch != '\n')
            .map(InputState::char_width)
            .sum()
    }

    fn highlight_runs(&self) -> Vec<HighlightRun> {
        let mut runs: Vec<HighlightRun> = Vec::new();
        for run in self
            .highlight_store
            .resolved_runs(&self.base_style, 0..self.len_chars)
        {
            if let Some(last) = runs.last_mut()
                && last.range.end == run.range.start
                && last.style == run.style
            {
                last.range.end = run.range.end;
                continue;
            }
            runs.push(run);
        }
        runs
    }

    fn highlight_style_at(runs: &[HighlightRun], idx: usize) -> Option<&Style> {
        if runs.is_empty() {
            return None;
        }

        let mut left = 0;
        let mut right = runs.len();
        while left < right {
            let mid = (left + right) / 2;
            let run = &runs[mid];
            if idx < run.range.start {
                right = mid;
            } else if idx >= run.range.end {
                left = mid + 1;
            } else {
                return Some(&run.style);
            }
        }

        None
    }

    fn gutter_text(&self, line_idx: usize) -> Option<String> {
        match self.gutter_metrics.kind {
            GutterKind::None => None,
            GutterKind::LineNumbers => {
                if line_idx >= self.line_count {
                    return None;
                }
                let number = line_idx + 1;
                Some(format!(
                    "{number:>digits$} | ",
                    number = number,
                    digits = self.gutter_metrics.digits
                ))
            }
        }
    }

    fn run_style_for_index(&self, idx: usize, highlight_runs: &[HighlightRun]) -> RunStyle {
        if let Some((start, end)) = self.selection {
            if idx >= start && idx < end {
                return RunStyle::Selection;
            }
        } else if idx == self.cursor && self.cursor < self.len_chars {
            return RunStyle::Cursor;
        }
        if let Some(style) = Self::highlight_style_at(highlight_runs, idx) {
            return RunStyle::Highlight(style.clone());
        }
        RunStyle::Base
    }

    #[allow(clippy::too_many_arguments)]
    fn flush_run(
        ctx: &mut RenderContext<'_>,
        run_style: &mut Option<RunStyle>,
        run_text: &mut String,
        run_width: &mut usize,
        run_start_x: &mut usize,
        cursor_x: &mut usize,
        remaining: &mut usize,
        y: usize,
        cursor_position: &mut Option<(usize, usize)>,
        base_attrs: &CellAttributes,
        selection_attrs: &CellAttributes,
        cursor_attrs: &CellAttributes,
    ) {
        if run_text.is_empty() || *run_width == 0 {
            run_text.clear();
            *run_style = None;
            *run_width = 0;
            *run_start_x = *cursor_x;
            return;
        }

        let style_ref = match run_style.as_ref() {
            Some(style) => style,
            None => {
                run_text.clear();
                *run_width = 0;
                *run_start_x = *cursor_x;
                return;
            }
        };

        match style_ref {
            RunStyle::Base => ctx.write_text(*run_start_x, y, run_text, base_attrs),
            RunStyle::Selection => ctx.write_text(*run_start_x, y, run_text, selection_attrs),
            RunStyle::Cursor => ctx.write_text(*run_start_x, y, run_text, cursor_attrs),
            RunStyle::Highlight(highlight_style) => {
                let attrs = ctx.style_to_attributes(highlight_style);
                ctx.write_text(*run_start_x, y, run_text, &attrs);
            }
        }

        if matches!(style_ref, RunStyle::Cursor) && cursor_position.is_none() {
            cursor_position.replace((*run_start_x, y));
        }

        *cursor_x += *run_width;
        *remaining = remaining.saturating_sub(*run_width);
        run_text.clear();
        *run_width = 0;
        *run_style = None;
        *run_start_x = *cursor_x;
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_cursor_symbol(
        &self,
        ctx: &mut RenderContext<'_>,
        skip_cols: &mut usize,
        remaining: &mut usize,
        cursor_x: &mut usize,
        run_style: &mut Option<RunStyle>,
        run_text: &mut String,
        run_width: &mut usize,
        run_start_x: &mut usize,
        y: usize,
        cursor_position: &mut Option<(usize, usize)>,
        base_attrs: &CellAttributes,
        selection_attrs: &CellAttributes,
        cursor_attrs: &CellAttributes,
    ) {
        for ch in self.cursor_symbol.chars() {
            let width = InputState::char_width(ch);
            if width == 0 {
                continue;
            }
            if *skip_cols >= width {
                *skip_cols -= width;
                continue;
            } else if *skip_cols > 0 {
                *skip_cols = 0;
            }

            if width > *remaining {
                Self::flush_run(
                    ctx,
                    run_style,
                    run_text,
                    run_width,
                    run_start_x,
                    cursor_x,
                    remaining,
                    y,
                    cursor_position,
                    base_attrs,
                    selection_attrs,
                    cursor_attrs,
                );
                break;
            }

            if !matches!(run_style.as_ref(), Some(RunStyle::Cursor)) {
                Self::flush_run(
                    ctx,
                    run_style,
                    run_text,
                    run_width,
                    run_start_x,
                    cursor_x,
                    remaining,
                    y,
                    cursor_position,
                    base_attrs,
                    selection_attrs,
                    cursor_attrs,
                );
                if *remaining == 0 {
                    break;
                }
                *run_style = Some(RunStyle::Cursor);
                *run_start_x = *cursor_x;
            }

            if *run_width + width > *remaining {
                Self::flush_run(
                    ctx,
                    run_style,
                    run_text,
                    run_width,
                    run_start_x,
                    cursor_x,
                    remaining,
                    y,
                    cursor_position,
                    base_attrs,
                    selection_attrs,
                    cursor_attrs,
                );
                if *remaining == 0 {
                    break;
                }
                if !matches!(run_style.as_ref(), Some(RunStyle::Cursor)) {
                    *run_style = Some(RunStyle::Cursor);
                    *run_start_x = *cursor_x;
                }
            }

            if run_text.is_empty() {
                *run_style = Some(RunStyle::Cursor);
                *run_start_x = *cursor_x;
            }

            run_text.push(ch);
            *run_width += width;
        }

        Self::flush_run(
            ctx,
            run_style,
            run_text,
            run_width,
            run_start_x,
            cursor_x,
            remaining,
            y,
            cursor_position,
            base_attrs,
            selection_attrs,
            cursor_attrs,
        );
    }
}

impl Renderable for InputRenderable {
    fn eq(&self, other: &dyn Renderable) -> bool {
        let Some(o) = other.as_any().downcast_ref::<Self>() else {
            return false;
        };

        if o.rope != self.rope
            || o.cursor != self.cursor
            || o.selection != self.selection
            || o.base_style != self.base_style
            || o.selection_style != self.selection_style
            || o.cursor_style != self.cursor_style
            || o.cursor_symbol != self.cursor_symbol
            || o.cursor_symbol_width != self.cursor_symbol_width
            || o.max_width != self.max_width
            || o.line_count != self.line_count
            || o.cursor_line != self.cursor_line
            || o.len_chars != self.len_chars
            || o.gutter_metrics != self.gutter_metrics
            || o.gutter_style != self.gutter_style
        {
            return false;
        }

        let self_runs = self.highlight_runs();
        let other_runs = o.highlight_runs();
        self_runs.iter().eq(other_runs.iter())
    }

    fn measure(
        &self,
        _style: &taffy::Style,
        known_dimensions: taffy::Size<Option<f32>>,
        available_space: taffy::Size<taffy::AvailableSpace>,
    ) -> taffy::Size<f32> {
        let width = known_dimensions
            .width
            .unwrap_or(match available_space.width {
                AvailableSpace::Definite(w) => w,
                AvailableSpace::MinContent | AvailableSpace::MaxContent => self.max_width as f32,
            });

        let content_height = self.line_count.max(1) as f32;
        let height = known_dimensions.height.unwrap_or(content_height);

        taffy::Size { width, height }
    }

    fn render(&self, ctx: &mut RenderContext<'_>) {
        let area = ctx.area();
        if area.width == 0 || area.height == 0 {
            return;
        }

        let scroll_y = ctx.scroll_y().max(0.0).floor() as usize;
        let skip_cols = ctx.scroll_x().max(0.0).round() as usize;

        let base_attrs = ctx.style_to_attributes(&self.base_style);
        let selection_attrs = ctx.style_to_attributes(&self.selection_style);
        let cursor_attrs = ctx.style_to_attributes(&self.cursor_style);
        let highlight_runs_ref = self.highlight_runs();
        let highlight_runs: &[HighlightRun] = &highlight_runs_ref;
        let gutter_attrs = ctx.style_to_attributes(&self.gutter_style);
        let gutter_width = self.gutter_metrics.width.min(area.width);

        let blank_line = " ".repeat(area.width);
        let mut cursor_position: Option<(usize, usize)> = None;

        for row in 0..area.height {
            let y = area.y + row;
            if !blank_line.is_empty() {
                ctx.write_text(area.x, y, &blank_line, &base_attrs);
            }

            let line_idx = scroll_y + row;
            if line_idx >= self.line_count {
                if gutter_width > 0 {
                    if let Some(mut text) = self.gutter_text(line_idx) {
                        if text.len() > gutter_width {
                            let start = text.len() - gutter_width;
                            text.replace_range(..start, "");
                        }
                        ctx.write_text(area.x, y, &text, &gutter_attrs);
                    }
                }
                continue;
            }

            if gutter_width > 0 {
                if let Some(mut text) = self.gutter_text(line_idx) {
                    if text.len() > gutter_width {
                        let start = text.len() - gutter_width;
                        text.replace_range(..start, "");
                    }
                    ctx.write_text(area.x, y, &text, &gutter_attrs);
                }
            }

            let mut remaining = area.width.saturating_sub(gutter_width);
            let mut cursor_x = area.x + gutter_width;
            let mut line_skip = skip_cols;

            let mut run_style: Option<RunStyle> = None;
            let mut run_text = String::new();
            let mut run_width = 0usize;
            let mut run_start_x = cursor_x;

            if remaining == 0 {
                continue;
            }

            let line_start = self.rope.line_to_char(line_idx).min(self.len_chars);
            let line_end = if line_idx + 1 >= self.line_count {
                self.len_chars
            } else {
                self.rope.line_to_char(line_idx + 1).min(self.len_chars)
            };

            let mut idx = line_start;
            while idx < line_end {
                let ch = self.rope.char(idx);
                let style = self.run_style_for_index(idx, highlight_runs);

                if ch == '\n' {
                    if matches!(style, RunStyle::Cursor) && self.selection.is_none() {
                        Self::flush_run(
                            ctx,
                            &mut run_style,
                            &mut run_text,
                            &mut run_width,
                            &mut run_start_x,
                            &mut cursor_x,
                            &mut remaining,
                            y,
                            &mut cursor_position,
                            &base_attrs,
                            &selection_attrs,
                            &cursor_attrs,
                        );

                        self.emit_cursor_symbol(
                            ctx,
                            &mut line_skip,
                            &mut remaining,
                            &mut cursor_x,
                            &mut run_style,
                            &mut run_text,
                            &mut run_width,
                            &mut run_start_x,
                            y,
                            &mut cursor_position,
                            &base_attrs,
                            &selection_attrs,
                            &cursor_attrs,
                        );
                    }
                    idx += 1;
                    continue;
                }

                let width = InputState::char_width(ch);
                if width == 0 {
                    idx += 1;
                    continue;
                }

                if line_skip >= width {
                    line_skip -= width;
                    idx += 1;
                    continue;
                } else if line_skip > 0 {
                    line_skip = 0;
                }

                if !run_style.as_ref().is_some_and(|current| current == &style) {
                    Self::flush_run(
                        ctx,
                        &mut run_style,
                        &mut run_text,
                        &mut run_width,
                        &mut run_start_x,
                        &mut cursor_x,
                        &mut remaining,
                        y,
                        &mut cursor_position,
                        &base_attrs,
                        &selection_attrs,
                        &cursor_attrs,
                    );
                    if remaining == 0 {
                        break;
                    }
                    run_style = Some(style.clone());
                    run_start_x = cursor_x;
                }

                if width > remaining {
                    Self::flush_run(
                        ctx,
                        &mut run_style,
                        &mut run_text,
                        &mut run_width,
                        &mut run_start_x,
                        &mut cursor_x,
                        &mut remaining,
                        y,
                        &mut cursor_position,
                        &base_attrs,
                        &selection_attrs,
                        &cursor_attrs,
                    );
                    break;
                }

                if run_width + width > remaining {
                    Self::flush_run(
                        ctx,
                        &mut run_style,
                        &mut run_text,
                        &mut run_width,
                        &mut run_start_x,
                        &mut cursor_x,
                        &mut remaining,
                        y,
                        &mut cursor_position,
                        &base_attrs,
                        &selection_attrs,
                        &cursor_attrs,
                    );
                    if remaining == 0 {
                        break;
                    }
                    if run_style.is_none() {
                        run_style = Some(style.clone());
                        run_start_x = cursor_x;
                    }
                }

                if run_style.is_none() {
                    run_style = Some(style.clone());
                    run_start_x = cursor_x;
                }

                run_text.push(ch);
                run_width += width;
                idx += 1;
            }

            Self::flush_run(
                ctx,
                &mut run_style,
                &mut run_text,
                &mut run_width,
                &mut run_start_x,
                &mut cursor_x,
                &mut remaining,
                y,
                &mut cursor_position,
                &base_attrs,
                &selection_attrs,
                &cursor_attrs,
            );

            if self.selection.is_none()
                && self.cursor == self.len_chars
                && line_idx == self.cursor_line
            {
                self.emit_cursor_symbol(
                    ctx,
                    &mut line_skip,
                    &mut remaining,
                    &mut cursor_x,
                    &mut run_style,
                    &mut run_text,
                    &mut run_width,
                    &mut run_start_x,
                    y,
                    &mut cursor_position,
                    &base_attrs,
                    &selection_attrs,
                    &cursor_attrs,
                );
            }
        }

        if let Some((x, y)) = cursor_position {
            ctx.set_cursor(x, y, CursorShape::BlinkingBar);
        }
    }

    fn debug_label(&self) -> &'static str {
        "input"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

pub fn default_keybindings<UpdateMsg>(
    state: &InputState,
    key: Key,
    map: impl Fn(InputMsg) -> UpdateMsg,
) -> Option<UpdateMsg> {
    let msg = match key.code {
        KeyCode::Char(ch) if !key.ctrl && !key.alt => Some(InputMsg::InsertChar(ch)),
        KeyCode::Char('h') if key.ctrl => Some(InputMsg::DeleteBackward),
        KeyCode::Char('w') if key.ctrl => Some(InputMsg::DeleteWordBackward),
        KeyCode::Char('u') if key.ctrl => Some(InputMsg::DeleteToStart),
        KeyCode::Char('k') if key.ctrl => Some(InputMsg::DeleteToEnd),
        KeyCode::Char('z') if key.ctrl && key.shift => Some(InputMsg::Redo),
        KeyCode::Char('z') if key.ctrl => Some(InputMsg::Undo),
        KeyCode::Char('y') if key.ctrl => Some(InputMsg::Redo),
        KeyCode::Char('a') if key.ctrl => {
            if state.is_multiline() {
                Some(InputMsg::MoveLineStart { extend: key.shift })
            } else {
                Some(InputMsg::MoveToStart { extend: key.shift })
            }
        }
        KeyCode::Char('e') if key.ctrl => {
            if state.is_multiline() {
                Some(InputMsg::MoveLineEnd { extend: key.shift })
            } else {
                Some(InputMsg::MoveToEnd { extend: key.shift })
            }
        }
        KeyCode::Backspace if key.ctrl || key.alt => Some(InputMsg::DeleteWordBackward),
        KeyCode::Backspace => Some(InputMsg::DeleteBackward),
        KeyCode::Left if key.ctrl || key.alt => Some(InputMsg::MoveWordLeft { extend: key.shift }),
        KeyCode::Right if key.ctrl || key.alt => {
            Some(InputMsg::MoveWordRight { extend: key.shift })
        }
        KeyCode::Left => Some(InputMsg::MoveLeft { extend: key.shift }),
        KeyCode::Right => Some(InputMsg::MoveRight { extend: key.shift }),
        KeyCode::Up if !key.ctrl && !key.alt && state.is_multiline() => {
            Some(InputMsg::MoveUp { extend: key.shift })
        }
        KeyCode::Down if !key.ctrl && !key.alt && state.is_multiline() => {
            Some(InputMsg::MoveDown { extend: key.shift })
        }
        KeyCode::Enter if !key.ctrl && !key.alt && state.is_multiline() => {
            Some(InputMsg::InsertChar('\n'))
        }
        KeyCode::Char(_) => None,
        KeyCode::Enter | KeyCode::Esc | KeyCode::Tab | KeyCode::Up | KeyCode::Down => None,
    }?;

    Some(map(msg))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_and_delete_text() {
        let mut state = InputState::default();
        state.update(InputMsg::InsertText("abc".into()));
        assert_eq!(state.value(), "abc");

        state.update(InputMsg::MoveLeft { extend: false });
        state.update(InputMsg::DeleteBackward);
        assert_eq!(state.value(), "ac");
        assert_eq!(state.cursor(), 1);
    }

    #[test]
    fn undo_and_redo_restore_content() {
        let mut state = InputState::default();
        state.update(InputMsg::InsertText("abc".into()));
        assert_eq!(state.value(), "abc");

        assert!(state.update(InputMsg::Undo));
        assert_eq!(state.value(), "");

        assert!(state.update(InputMsg::Redo));
        assert_eq!(state.value(), "abc");
    }

    #[test]
    fn replacing_selection_is_single_history_entry() {
        let mut state = InputState::default();
        state.update(InputMsg::InsertText("hello".into()));
        state.set_selection(0, 5);

        state.update(InputMsg::InsertChar('x'));
        assert_eq!(state.value(), "x");

        assert!(state.update(InputMsg::Undo));
        assert_eq!(state.value(), "hello");
    }

    #[test]
    fn transactions_apply_multiple_edits_atomically() {
        let mut state = InputState::with_value("abcdef");
        let before = state.caret_snapshot();

        let first_range = 0..2;
        let second_range = 4..6;
        let first_edit = TextEdit::new(
            first_range.clone(),
            state.rope_slice_to_string(first_range),
            "12".into(),
        );
        let second_edit = TextEdit::new(
            second_range.clone(),
            state.rope_slice_to_string(second_range),
            "YZ".into(),
        );
        let transaction = EditTransaction::new(vec![first_edit, second_edit]);
        let after = CaretSnapshot::new(0, 0);
        let command = EditCommand::new(before, after, transaction);

        assert!(state.commit_edit(command));
        assert_eq!(state.value(), "12cdYZ");

        assert!(state.undo());
        assert_eq!(state.value(), "abcdef");
    }

    #[test]
    fn ctrl_w_deletes_previous_word() {
        let mut state = InputState::with_value("hello world");
        state.update(InputMsg::DeleteWordBackward);
        assert_eq!(state.value(), "hello ");
    }

    #[test]
    fn double_click_selects_word() {
        let mut state = InputState::with_value("foo bar");
        state.update(InputMsg::Pointer {
            column: 2,
            row: 0,
            click_count: 2,
        });
        assert_eq!(state.selection(), Some((0, 3)));
    }

    #[test]
    fn single_line_filters_newlines() {
        let mut state = InputState::default();
        state.update(InputMsg::InsertText("hello\nworld".into()));
        assert_eq!(state.value(), "hello world");
    }

    #[test]
    fn multiline_preserves_newlines() {
        let mut state = InputState::new_multiline();
        state.update(InputMsg::InsertText("hello\nworld".into()));
        assert_eq!(state.value(), "hello\nworld");
    }

    #[test]
    fn multiline_move_vertical_preserves_column() {
        let mut state = InputState::with_value_multiline("abcd\nef");
        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        assert_eq!(state.cursor(), 2);

        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.cursor(), 7);

        state.update(InputMsg::MoveUp { extend: false });
        assert_eq!(state.cursor(), 2);
    }

    #[test]
    fn pointer_uses_row_for_multiline_navigation() {
        let mut state = InputState::with_value_multiline("hello\nworld");
        state.update(InputMsg::Pointer {
            column: 0,
            row: 1,
            click_count: 1,
        });
        assert_eq!(state.cursor(), 6);
    }

    #[test]
    fn multiline_render_breaks_lines() {
        let mut state = InputState::with_value_multiline("foo\nbar");
        state.update(InputMsg::SetViewportSize { cols: 4, rows: 2 });
        assert_eq!(state.scroll_x(), 0);
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(4, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(4.0),
                height: AvailableSpace::Definite(2.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(4, 2))
            .expect("render should succeed");
        let rendered = renderer.buffer().to_string();
        let mut lines = rendered.lines();
        let first = lines.next().unwrap_or("");
        let second = lines.next().unwrap_or("");

        assert!(first.starts_with("foo"));
        assert!(second.starts_with("bar"));
    }

    #[test]
    fn cursor_before_newline_stays_on_previous_line() {
        let mut state = InputState::with_value_multiline("123\n4");
        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        assert_eq!(state.cursor(), 3);

        state.update(InputMsg::SetViewportSize { cols: 4, rows: 2 });

        let mut style = InputStyle::default();
        style.cursor_symbol = "|";

        let renderable = InputRenderable::new(&state, &style);
        assert_eq!(state.scroll_x(), 0);
        assert_eq!(renderable.line_count, 2);

        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(4, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(4.0),
                height: AvailableSpace::Definite(2.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(4, 2))
            .expect("render should succeed");
        let rendered = renderer.buffer().to_string();
        let mut lines = rendered.lines();

        let first = lines.next().unwrap_or("");
        let second = lines.next().unwrap_or("");

        assert!(first.starts_with("123|"), "first line: {:?}", first);
        assert!(second.starts_with("4"), "second line: {:?}", second);
    }

    #[test]
    fn multiline_vertical_scroll_keeps_cursor_visible() {
        let mut state = InputState::with_value_multiline("a\nb\nc");
        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::SetViewportSize { cols: 1, rows: 1 });
        assert_eq!(state.scroll_y(), 0);

        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.scroll_y(), 1);

        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.scroll_y(), 2);
    }

    #[test]
    fn multiline_move_line_boundaries() {
        let mut state = InputState::with_value_multiline("one\ntwo");
        state.update(InputMsg::MoveToStart { extend: false });
        assert_eq!(state.cursor(), 0);

        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.cursor(), 4);

        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        assert_eq!(state.cursor(), 6);

        state.update(InputMsg::MoveLineStart { extend: false });
        assert_eq!(state.cursor(), 4);
        assert_eq!(state.selection(), None);

        state.update(InputMsg::MoveLineEnd { extend: false });
        assert_eq!(state.cursor(), 7);

        state.update(InputMsg::MoveLineStart { extend: false });
        state.update(InputMsg::MoveLineEnd { extend: true });
        assert_eq!(state.cursor(), 7);
        assert_eq!(state.selection(), Some((4, 7)));
    }

    #[test]
    fn default_keybindings_enter_inserts_newline_in_multiline() {
        let state = InputState::new_multiline();
        let key = Key::new(KeyCode::Enter);
        let msg = default_keybindings(&state, key, |m| m);
        assert!(matches!(msg, Some(InputMsg::InsertChar('\n'))));
    }

    #[test]
    fn default_keybindings_enter_ignored_in_single_line() {
        let state = InputState::default();
        let key = Key::new(KeyCode::Enter);
        assert!(default_keybindings(&state, key, |m| m).is_none());
    }

    #[test]
    fn default_keybindings_maps_up_down_in_multiline() {
        let state = InputState::new_multiline();
        let up_key = Key::new(KeyCode::Up);
        let down_key = Key::new(KeyCode::Down);
        assert!(matches!(
            default_keybindings(&state, up_key, |m| m),
            Some(InputMsg::MoveUp { .. })
        ));
        assert!(matches!(
            default_keybindings(&state, down_key, |m| m),
            Some(InputMsg::MoveDown { .. })
        ));
    }

    #[test]
    fn default_keybindings_ctrl_a_e_multiline_move_line_bounds() {
        let state = InputState::new_multiline();
        let ctrl_a = Key::with_modifiers(KeyCode::Char('a'), true, false, false);
        let ctrl_shift_e = Key::with_modifiers(KeyCode::Char('e'), true, false, true);

        assert!(matches!(
            default_keybindings(&state, ctrl_a, |m| m),
            Some(InputMsg::MoveLineStart { extend: false })
        ));
        assert!(matches!(
            default_keybindings(&state, ctrl_shift_e, |m| m),
            Some(InputMsg::MoveLineEnd { extend: true })
        ));
    }

    #[test]
    fn default_keybindings_ctrl_a_e_single_line_move_buffer_bounds() {
        let state = InputState::default();
        let ctrl_a = Key::with_modifiers(KeyCode::Char('a'), true, false, false);
        let ctrl_e = Key::with_modifiers(KeyCode::Char('e'), true, false, false);

        assert!(matches!(
            default_keybindings(&state, ctrl_a, |m| m),
            Some(InputMsg::MoveToStart { extend: false })
        ));
        assert!(matches!(
            default_keybindings(&state, ctrl_e, |m| m),
            Some(InputMsg::MoveToEnd { extend: false })
        ));
    }

    #[test]
    fn cursor_highlights_character_under_cursor() {
        let mut state = InputState::with_value("abc");
        state.update(InputMsg::MoveToStart { extend: false });
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::{CursorShape, CursorState, DoubleBuffer};
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::{Palette, Rgba};
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(4, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(4.0),
                height: AvailableSpace::Definite(1.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(4, 1))
            .expect("render should succeed");

        let back = renderer.buffer().back_buffer();
        let cursor_cell = &back[0][0];
        let expected_bg = Rgba::opaque(229, 229, 229);
        let expected_fg = Rgba::opaque(0, 0, 0);

        assert_eq!(cursor_cell.ch, 'a');
        assert_eq!(cursor_cell.attrs.background(), Some(expected_bg));
        assert_eq!(cursor_cell.attrs.foreground(), Some(expected_fg));
        assert!(cursor_cell.attrs.is_bold());

        let next_cell = &back[0][1];
        assert_eq!(next_cell.ch, 'b');
        assert_eq!(next_cell.attrs.background(), None);
        assert!(!next_cell.attrs.is_bold());

        assert_eq!(
            renderer.buffer().cursor_state(),
            CursorState::Position {
                x: 0,
                y: 0,
                shape: CursorShape::BlinkingBar
            }
        );
    }

    #[test]
    fn highlight_layer_produces_runs() {
        let mut state = InputState::with_value("hello");
        state.set_highlight_layer(
            HighlightLayerId::new(1),
            0,
            [HighlightSpan::new(1..4, Style::fg(Color::BrightRed))],
        );

        let style = InputStyle::default();
        let renderable = InputRenderable::new(&state, &style);
        let expected_style = style.text.clone().merged(&Style::fg(Color::BrightRed));

        let runs = renderable.highlight_runs();
        assert_eq!(
            &runs[..],
            &[HighlightRun {
                range: 1..4,
                style: expected_style,
            }]
        );
    }

    #[test]
    fn highlight_layers_respect_priority() {
        let mut state = InputState::with_value("hello");
        let mut base_layer_style = Style::default();
        base_layer_style.bg = Some(Color::Blue);
        let mut overlay_style = Style::default();
        overlay_style.fg = Some(Color::Yellow);
        overlay_style.bold = true;

        state.set_highlight_layer(
            HighlightLayerId::new(1),
            0,
            [HighlightSpan::new(0..5, base_layer_style.clone())],
        );
        state.set_highlight_layer(
            HighlightLayerId::new(2),
            10,
            [HighlightSpan::new(0..5, overlay_style.clone())],
        );

        let style = InputStyle::default();
        let renderable = InputRenderable::new(&state, &style);
        let expected_style = style
            .text
            .clone()
            .merged(&base_layer_style)
            .merged(&overlay_style);

        let runs = renderable.highlight_runs();
        assert_eq!(
            &runs[..],
            &[HighlightRun {
                range: 0..5,
                style: expected_style,
            }]
        );
    }

    #[test]
    fn highlight_shifts_with_insertion_before_span() {
        let mut state = InputState::with_value("hello");
        state.set_highlight_layer(
            HighlightLayerId::new(1),
            0,
            [HighlightSpan::new(0..5, Style::fg(Color::BrightRed))],
        );

        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::InsertChar('!'));

        let style = InputStyle::default();
        let mut expected_style = style.text.clone();
        expected_style.apply_overlay(&Style::fg(Color::BrightRed));

        let renderable = InputRenderable::new(&state, &style);

        let runs = renderable.highlight_runs();
        assert_eq!(
            &runs[..],
            &[HighlightRun {
                range: 1..6,
                style: expected_style,
            }]
        );
    }

    #[test]
    fn highlight_expands_with_insertion_inside_span() {
        let mut state = InputState::with_value("hello");
        state.set_highlight_layer(
            HighlightLayerId::new(1),
            0,
            [HighlightSpan::new(0..5, Style::fg(Color::BrightBlue))],
        );

        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::InsertChar('!'));

        let style = InputStyle::default();
        let mut expected_style = style.text.clone();
        expected_style.apply_overlay(&Style::fg(Color::BrightBlue));

        let renderable = InputRenderable::new(&state, &style);

        let runs = renderable.highlight_runs();
        assert_eq!(
            &runs[..],
            &[HighlightRun {
                range: 0..6,
                style: expected_style,
            }]
        );
    }

    #[test]
    fn highlight_contracts_with_deletion_inside_span() {
        let mut state = InputState::with_value("hello");
        state.set_highlight_layer(
            HighlightLayerId::new(1),
            0,
            [HighlightSpan::new(0..5, Style::fg(Color::Cyan))],
        );

        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::MoveRight { extend: false });
        state.update(InputMsg::DeleteBackward);

        let style = InputStyle::default();
        let mut expected_style = style.text.clone();
        expected_style.apply_overlay(&Style::fg(Color::Cyan));

        let renderable = InputRenderable::new(&state, &style);

        let runs = renderable.highlight_runs();
        assert_eq!(
            &runs[..],
            &[HighlightRun {
                range: 0..4,
                style: expected_style,
            }]
        );
    }

    #[test]
    fn highlight_removed_when_span_deleted() {
        let mut state = InputState::with_value("hello");
        state.set_highlight_layer(
            HighlightLayerId::new(1),
            0,
            [HighlightSpan::new(1..4, Style::fg(Color::BrightGreen))],
        );

        state.set_selection(1, 4);
        state.update(InputMsg::DeleteBackward);

        let style = InputStyle::default();
        let renderable = InputRenderable::new(&state, &style);

        assert!(renderable.highlight_runs().is_empty());
    }

    #[test]
    fn clearing_highlights_removes_runs() {
        let mut state = InputState::with_value("hello");
        state.set_highlight_layer(
            HighlightLayerId::new(1),
            0,
            [HighlightSpan::new(0..5, Style::fg(Color::BrightGreen))],
        );
        state.clear_highlights();

        let style = InputStyle::default();
        let renderable = InputRenderable::new(&state, &style);
        assert!(renderable.highlight_runs().is_empty());
    }

    #[test]
    fn input_keeps_cursor_visible_with_horizontal_scroll() {
        // Set long value and narrow viewport; ensure state scrolls to keep cursor visible
        let mut state = InputState::with_value("hello world this is long");
        // Simulate resize to 8 cols
        state.update(InputMsg::SetViewportSize { cols: 8, rows: 1 });
        // Move cursor to end (already there by set_value)
        // Ensure scroll advanced
        assert!(state.scroll_x() > 0);

        let style = InputStyle::default();
        // Build node and ensure it asks for x-scroll
        let mut node = crate::input::<()>("input", &state, &style, |_| ());
        // Layout and render to 8x1
        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(8, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(8.0),
                height: AvailableSpace::Definite(1.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(8, 1))
            .expect("render should succeed");
        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        // Expect to see the last characters including the cursor placeholder/char region at end
        assert!(first_line.ends_with(' '));
    }

    #[test]
    fn input_windowing_handles_fullwidth_chars() {
        // Include fullwidth char and verify it shows within window after horizontal scroll
        let mut state = InputState::with_value("ab漢cdef");
        state.update(InputMsg::SetViewportSize { cols: 6, rows: 1 });
        // Move cursor to end to trigger scroll
        state.update(InputMsg::MoveToEnd { extend: false });
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(6, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(6.0),
                height: AvailableSpace::Definite(1.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(6, 1))
            .expect("render should succeed");
        let screen = renderer.buffer().to_string();
        let first_line = screen.lines().next().unwrap_or("");
        assert!(first_line.contains('漢'));
    }

    #[test]
    fn cursor_placeholder_cell_has_cursor_style() {
        // Only the placeholder cell at the cursor position should adopt the cursor style.
        let state = InputState::with_value("hi");
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::{CursorShape, CursorState, DoubleBuffer};
        use crate::dom::rounding::round_layout;
        use crate::event::Size;
        use crate::palette::{Palette, Rgba};
        use crate::render::Renderer;
        use taffy::{AvailableSpace, compute_root_layout};

        let mut buffer = DoubleBuffer::new(6, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        compute_root_layout(
            &mut node,
            u64::MAX.into(),
            taffy::Size {
                width: AvailableSpace::Definite(6.0),
                height: AvailableSpace::Definite(1.0),
            },
        );
        round_layout(&mut node);

        renderer
            .render(&node, Size::new(6, 1))
            .expect("render should succeed");

        let back = renderer.buffer().back_buffer();

        // First two cells are the text with default styling
        assert_eq!(back[0][0].ch, 'h');
        assert_eq!(back[0][0].attrs.background(), None);
        assert!(!back[0][0].attrs.is_bold());
        assert_eq!(back[0][1].ch, 'i');
        assert_eq!(back[0][1].attrs.background(), None);
        assert!(!back[0][1].attrs.is_bold());

        // The cursor placeholder space uses cursor style
        let expected_bg = Rgba::opaque(229, 229, 229); // White
        let expected_fg = Rgba::opaque(0, 0, 0); // Black
        let cursor_cell = &back[0][2];
        assert_eq!(cursor_cell.ch, ' ');
        assert_eq!(cursor_cell.attrs.background(), Some(expected_bg));
        assert_eq!(cursor_cell.attrs.foreground(), Some(expected_fg));
        assert!(cursor_cell.attrs.is_bold());

        for cell in &back[0][3..] {
            assert_eq!(cell.attrs.background(), None);
            assert!(!cell.attrs.is_bold());
        }

        assert_eq!(
            renderer.buffer().cursor_state(),
            CursorState::Position {
                x: 2,
                y: 0,
                shape: CursorShape::BlinkingBar
            }
        );
    }

    #[test]
    fn commit_style_cursor_tints_trailing_whitespace_with_text_style() {
        // Regression: cursor background should not bleed into the rest of the row.
        // Trailing whitespace should adopt the text background, matching the commit modal styling.
        let state = InputState::with_value("hi");

        let mut style = InputStyle::default();
        style.text.bg = Some(crate::dom::Color::rgb(0x44, 0x55, 0x66));
        style.text.fg = Some(crate::dom::Color::rgb(0x22, 0x33, 0x44));
        style.cursor.bg = Some(crate::dom::Color::rgb(0x11, 0x22, 0x33));
        style.cursor.fg = Some(crate::dom::Color::rgb(0xee, 0xdd, 0xcc));

        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::event::Size;
        use crate::palette::{Palette, Rgba};
        use crate::render::Renderer;

        let mut buffer = DoubleBuffer::new(8, 1);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        node.layout_state.layout.size.width = 8.0;
        node.layout_state.layout.size.height = 1.0;
        node.layout_state.layout.location.x = 0.0;
        node.layout_state.layout.location.y = 0.0;
        node.layout_state.unrounded_layout.size.width = 8.0;
        node.layout_state.unrounded_layout.size.height = 1.0;
        node.layout_state.unrounded_layout.location.x = 0.0;
        node.layout_state.unrounded_layout.location.y = 0.0;

        renderer
            .render(&node, Size::new(8, 1))
            .expect("render should succeed");

        let back = renderer.buffer().back_buffer();
        let row = &back[0];

        let expected_text_bg = Rgba::opaque(0x44, 0x55, 0x66);
        let expected_cursor_bg = Rgba::opaque(0x11, 0x22, 0x33);
        let expected_cursor_fg = Rgba::opaque(0xee, 0xdd, 0xcc);

        let text_len = state.len_chars();
        for cell in &row[..text_len] {
            assert_eq!(cell.attrs.background(), Some(expected_text_bg));
        }

        let cursor_cell = &row[text_len];
        assert_eq!(cursor_cell.ch, ' ');
        assert_eq!(cursor_cell.attrs.background(), Some(expected_cursor_bg));
        assert_eq!(cursor_cell.attrs.foreground(), Some(expected_cursor_fg));

        for cell in &row[(text_len + 1)..] {
            assert_eq!(cell.ch, ' ');
            assert_eq!(cell.attrs.background(), Some(expected_text_bg));
            assert_ne!(cell.attrs.background(), Some(expected_cursor_bg));
        }
    }

    #[test]
    fn line_number_gutter_reports_width() {
        let mut state = InputState::with_value_multiline("foo\nbar\nbaz");
        assert_eq!(state.gutter_width(), 0);
        assert!(!state.line_number_gutter_enabled());

        state.set_line_number_gutter(true);
        assert!(state.line_number_gutter_enabled());
        assert_eq!(state.gutter_width(), 4);

        state.set_line_number_gutter(false);
        assert!(!state.line_number_gutter_enabled());
        assert_eq!(state.gutter_width(), 0);
    }

    #[test]
    fn line_number_gutter_renders_numbers() {
        let mut state = InputState::with_value_multiline("foo\nbar");
        state.set_line_number_gutter(true);
        state.update(InputMsg::SetViewportSize { cols: 10, rows: 2 });
        let style = InputStyle::default();
        let mut node = crate::input::<()>("input", &state, &style, |_| ());

        use crate::buffer::DoubleBuffer;
        use crate::event::Size;
        use crate::palette::Palette;
        use crate::render::Renderer;

        let mut buffer = DoubleBuffer::new(10, 2);
        let palette = Palette::default();
        let mut renderer = Renderer::new(&mut buffer, &palette);

        node.layout_state.layout.size.width = 10.0;
        node.layout_state.layout.size.height = 2.0;
        node.layout_state.layout.location.x = 0.0;
        node.layout_state.layout.location.y = 0.0;
        node.layout_state.unrounded_layout.size.width = 10.0;
        node.layout_state.unrounded_layout.size.height = 2.0;
        node.layout_state.unrounded_layout.location.x = 0.0;
        node.layout_state.unrounded_layout.location.y = 0.0;

        renderer
            .render(&node, Size::new(10, 2))
            .expect("render should succeed");

        let back = renderer.buffer().back_buffer();
        let row0 = &back[0];
        assert_eq!(row0[0].ch, '1');
        assert_eq!(row0[1].ch, ' ');
        assert_eq!(row0[2].ch, '|');
        assert_eq!(row0[3].ch, ' ');
        assert_eq!(row0[4].ch, 'f');
        assert_eq!(row0[5].ch, 'o');
        assert_eq!(row0[6].ch, 'o');
        assert!(row0[0].attrs.is_dim());
        assert!(!row0[4].attrs.is_dim());

        let row1 = &back[1];
        assert_eq!(row1[0].ch, '2');
        assert_eq!(row1[2].ch, '|');
        assert_eq!(row1[4].ch, 'b');
        assert_eq!(row1[5].ch, 'a');
        assert_eq!(row1[6].ch, 'r');
    }
}
