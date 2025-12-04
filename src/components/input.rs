use ropey::Rope;
use smallvec::{SmallVec, smallvec};
use std::collections::{BTreeMap, HashSet};
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
///   *Status: Fully implemented.*
/// - **Multiple views** – [`views`] carries per-pane data (viewport, cursors).
///   *Status: only the first view (`PRIMARY_VIEW_ID`) is used; callers still interact with a single pane.*
#[derive(Clone, Debug)]
pub struct InputState {
    mode: InputMode,
    document: DocumentState,
    history: EditHistory,
    highlights: HighlightStore,
    last_change: Option<TextChangeSet>,
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
    #[allow(dead_code)]
    gutter: GutterState,
}

impl ViewState {
    fn new(id: ViewId) -> Self {
        Self {
            id,
            cursor_set: CursorSet::default(),
            selection_mode: SelectionMode::Character,
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

    fn iter(&self) -> impl Iterator<Item = &Caret> {
        self.carets.iter()
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut Caret> {
        self.carets.get_mut(index)
    }

    fn primary_index(&self) -> usize {
        self.primary.min(self.carets.len().saturating_sub(1))
    }

    fn snapshot(&self) -> CursorSetSnapshot {
        CursorSetSnapshot {
            carets: self.carets.iter().cloned().collect(),
            primary: self.primary_index(),
        }
    }

    fn replace_all(&mut self, carets: Vec<Caret>, primary: usize) {
        self.carets.clear();
        if carets.is_empty() {
            self.carets.push(Caret::default());
            self.primary = 0;
        } else {
            self.carets.extend(carets);
            self.primary = primary.min(self.carets.len().saturating_sub(1));
        }
    }

    fn upsert_caret(&mut self, caret: Caret, make_primary: bool) -> usize {
        if let Some(idx) = self
            .carets
            .iter()
            .position(|existing| existing.head == caret.head)
        {
            let existing = &mut self.carets[idx];
            existing.anchor = caret.anchor;
            existing.preferred_column = caret.preferred_column;
            if make_primary {
                self.primary = idx;
            }
            return idx;
        }

        // Capture fields before moving caret so we can search after push.
        let head = caret.head;
        let anchor = caret.anchor;
        let preferred_column = caret.preferred_column;

        self.carets.push(Caret {
            head,
            anchor,
            preferred_column,
        });
        self.carets.sort_by(|a, b| {
            let a_start = a.head.min(a.anchor);
            let b_start = b.head.min(b.anchor);
            a_start
                .cmp(&b_start)
                .then_with(|| a.head.cmp(&b.head))
                .then_with(|| a.anchor.cmp(&b.anchor))
        });

        let idx = self
            .carets
            .iter()
            .position(|existing| existing.head == head && existing.anchor == anchor)
            .or_else(|| {
                self.carets
                    .iter()
                    .position(|existing| existing.head == head)
            })
            .unwrap_or_else(|| self.primary_index());

        if make_primary {
            self.primary = idx;
        }

        idx
    }

    fn restore(&mut self, snapshot: &CursorSetSnapshot) {
        self.carets.clear();
        if snapshot.carets.is_empty() {
            self.carets.push(Caret::default());
            self.primary = 0;
            return;
        }

        self.carets.extend(snapshot.carets.iter().cloned());
        self.primary = snapshot.primary.min(self.carets.len().saturating_sub(1));
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

/// Public caret snapshot for external consumers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CursorPosition {
    pub head: usize,
    pub anchor: usize,
}

impl CursorPosition {
    pub fn as_range(&self) -> Range<usize> {
        if self.head <= self.anchor {
            self.head..self.anchor
        } else {
            self.anchor..self.head
        }
    }

    pub fn is_selection(&self) -> bool {
        self.head != self.anchor
    }
}

#[derive(Clone, Debug)]
struct CaretInfo {
    index: usize,
    head: usize,
    anchor: usize,
    preferred_column: Option<usize>,
}

impl CaretInfo {
    fn new(index: usize, caret: &Caret) -> Self {
        Self {
            index,
            head: caret.head,
            anchor: caret.anchor,
            preferred_column: caret.preferred_column,
        }
    }

    fn has_selection(&self) -> bool {
        self.head != self.anchor
    }

    fn clamped_positions(&self, len: usize) -> (usize, usize) {
        (self.head.min(len), self.anchor.min(len))
    }

    fn clamped_range(&self, len: usize) -> Range<usize> {
        let (head, anchor) = self.clamped_positions(len);
        if head <= anchor {
            head..anchor
        } else {
            anchor..head
        }
    }
}

#[derive(Clone, Debug)]
struct CaretEdit {
    index: usize,
    range: Range<usize>,
    inserted: String,
    head_offset: usize,
    anchor_offset: usize,
}

impl CaretEdit {
    fn new(
        index: usize,
        range: Range<usize>,
        inserted: String,
        head_offset: usize,
        anchor_offset: usize,
    ) -> Self {
        Self {
            index,
            range,
            inserted,
            head_offset,
            anchor_offset,
        }
    }

    fn deleted_len(&self) -> usize {
        self.range.end.saturating_sub(self.range.start)
    }
}

#[derive(Clone, Debug)]
struct CaretUpdate {
    index: usize,
    head: usize,
    anchor: usize,
    preferred_column: Option<usize>,
}

impl CaretUpdate {
    fn new(index: usize, head: usize, anchor: usize, preferred_column: Option<usize>) -> Self {
        Self {
            index,
            head,
            anchor,
            preferred_column,
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct PointerModifiers {
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub super_key: bool,
}

/// Captured multi-caret state for undo/redo.
#[derive(Clone, Debug)]
struct CursorSetSnapshot {
    carets: Vec<Caret>,
    primary: usize,
}

impl CursorSetSnapshot {
    fn new(carets: Vec<Caret>, primary: usize) -> Self {
        let primary = if carets.is_empty() {
            0
        } else {
            primary.min(carets.len().saturating_sub(1))
        };
        Self { carets, primary }
    }

    fn primary_index(&self) -> usize {
        if self.carets.is_empty() {
            0
        } else {
            self.primary.min(self.carets.len().saturating_sub(1))
        }
    }

    fn carets(&self) -> &[Caret] {
        &self.carets
    }
}
/// Selection granularity placeholder (line/block modes can be added later).
#[derive(Clone, Copy, Debug, Default)]
enum SelectionMode {
    #[default]
    Character,
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

#[derive(Clone, Copy, Debug)]
struct EditGeometry {
    start: usize,
    end: usize,
    inserted_len: usize,
    removed_len: usize,
    delta: isize,
}

impl EditGeometry {
    fn new(edit: &TextEdit) -> Self {
        let start = edit.range.start;
        let end = edit.range.end;
        let removed_len = end.saturating_sub(start);
        let inserted_len = edit.inserted.chars().count();
        let delta = inserted_len as isize - removed_len as isize;

        Self {
            start,
            end,
            inserted_len,
            removed_len,
            delta,
        }
    }

    fn is_noop(&self) -> bool {
        self.removed_len == 0 && self.inserted_len == 0
    }

    fn shift_range(&self, range: &mut Range<usize>) {
        if self.delta == 0 {
            return;
        }

        let start = (range.start as isize + self.delta).max(0) as usize;
        let end = (range.end as isize + self.delta).max(0) as usize;
        range.start = start;
        range.end = end;
    }

    fn map_index(&self, pos: usize) -> usize {
        if pos <= self.start {
            pos
        } else if pos >= self.end {
            let new_pos = pos as isize + self.delta;
            if new_pos < 0 { 0 } else { new_pos as usize }
        } else {
            self.start + self.inserted_len
        }
    }
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
        let geometry = EditGeometry::new(edit);

        if geometry.is_noop() {
            return;
        }

        for layer in self.layers.values_mut() {
            let mut idx = 0;
            while idx < layer.spans.len() {
                let span = &mut layer.spans[idx];

                if span.range.end <= geometry.start {
                    idx += 1;
                    continue;
                }

                if span.range.start >= geometry.end {
                    geometry.shift_range(&mut span.range);
                    idx += 1;
                    continue;
                }

                let new_start = geometry.map_index(span.range.start);
                let new_end = geometry.map_index(span.range.end);

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
    hints: BTreeMap<InlineHintId, InlineHint>,
}

/// Immutable inline hint injected by tooling.
#[derive(Clone, Debug)]
pub struct InlineHint {
    pub id: InlineHintId,
    pub range: Range<usize>,
    pub text: Rope,
    pub style: Style,
    pub kind: InlineHintKind,
    pub placement: InlineHintPlacement,
}

impl Default for InlineHint {
    fn default() -> Self {
        Self {
            id: InlineHintId(0),
            range: 0..0,
            text: Rope::new(),
            style: Style::default(),
            kind: InlineHintKind::TypeAnnotation,
            placement: InlineHintPlacement::Inline,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InlineHintId(pub u64);

/// Kind of inline hint (type annotations, diagnostics, etc.).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum InlineHintKind {
    #[default]
    TypeAnnotation,
    ParameterName,
    Diagnostic,
    GitBlame,
    Diff,
    Other,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum InlineHintPlacement {
    #[default]
    Inline,
    BlockAbove,
    BlockBelow,
}

#[derive(Clone, Debug, PartialEq)]
struct PreparedInlineHint {
    id: InlineHintId,
    anchor: usize,
    text: String,
    width: usize,
    style: Style,
    placement: InlineHintPlacement,
    line: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum VisualRowEntry {
    Text { line_idx: usize },
    BlockHint { hint_index: usize },
}

#[derive(Clone, Debug, PartialEq)]
struct InlineHintLayout {
    inline_hints: Vec<PreparedInlineHint>,
    inline_ranges: Vec<Option<Range<usize>>>,
    block_hints: Vec<PreparedInlineHint>,
    visual_rows: Vec<VisualRowEntry>,
    content_line_count: usize,
}

impl InlineHintLayout {
    fn build(store: &InlineHintStore, rope: &Rope) -> Self {
        let content_line_count = rope.len_lines().max(1);

        let mut inline_grouped: Vec<Vec<PreparedInlineHint>> = vec![Vec::new(); content_line_count];
        let mut block_hints: Vec<PreparedInlineHint> = Vec::new();
        let mut block_above: Vec<Vec<usize>> = vec![Vec::new(); content_line_count];
        let mut block_below: Vec<Vec<usize>> = vec![Vec::new(); content_line_count];

        for mut hint in store.prepared_hints(rope) {
            let line = hint.line.min(content_line_count.saturating_sub(1));
            hint.line = line;
            match hint.placement {
                InlineHintPlacement::Inline => inline_grouped[line].push(hint),
                InlineHintPlacement::BlockAbove => {
                    let idx = block_hints.len();
                    block_hints.push(hint);
                    block_above[line].push(idx);
                }
                InlineHintPlacement::BlockBelow => {
                    let idx = block_hints.len();
                    block_hints.push(hint);
                    block_below[line].push(idx);
                }
            }
        }

        let mut inline_hints: Vec<PreparedInlineHint> = Vec::new();
        let mut inline_ranges: Vec<Option<Range<usize>>> = vec![None; content_line_count];
        for (line, mut hints) in inline_grouped.into_iter().enumerate() {
            if hints.is_empty() {
                continue;
            }
            hints.sort_by(|a, b| a.anchor.cmp(&b.anchor).then_with(|| a.id.cmp(&b.id)));
            let start = inline_hints.len();
            inline_hints.extend(hints.into_iter());
            let end = inline_hints.len();
            inline_ranges[line] = Some(start..end);
        }

        let mut visual_rows = Vec::with_capacity(content_line_count + block_hints.len());
        for line in 0..content_line_count {
            for &idx in &block_above[line] {
                visual_rows.push(VisualRowEntry::BlockHint { hint_index: idx });
            }
            visual_rows.push(VisualRowEntry::Text { line_idx: line });
            for &idx in &block_below[line] {
                visual_rows.push(VisualRowEntry::BlockHint { hint_index: idx });
            }
        }

        if visual_rows.is_empty() {
            visual_rows.push(VisualRowEntry::Text { line_idx: 0 });
        }

        Self {
            inline_hints,
            inline_ranges,
            block_hints,
            visual_rows,
            content_line_count,
        }
    }

    fn inline_hints_for_line(&self, line_idx: usize) -> &[PreparedInlineHint] {
        if line_idx >= self.inline_ranges.len() {
            return &[];
        }
        match &self.inline_ranges[line_idx] {
            Some(range) => &self.inline_hints[range.clone()],
            None => &[],
        }
    }

    fn block_hint(&self, idx: usize) -> &PreparedInlineHint {
        &self.block_hints[idx]
    }

    fn block_hints(&self) -> &[PreparedInlineHint] {
        &self.block_hints
    }

    fn visual_rows(&self) -> &[VisualRowEntry] {
        &self.visual_rows
    }

    fn visual_row_count(&self) -> usize {
        self.visual_rows.len()
    }

    fn content_line_count(&self) -> usize {
        self.content_line_count
    }
}

impl InlineHintStore {
    fn is_empty(&self) -> bool {
        self.hints.is_empty()
    }

    fn set(&mut self, hint: InlineHint) {
        self.hints.insert(hint.id, hint);
    }

    fn remove(&mut self, id: InlineHintId) -> bool {
        self.hints.remove(&id).is_some()
    }

    fn clear(&mut self) {
        self.hints.clear();
    }

    fn ordered(&self) -> Vec<InlineHint> {
        let mut hints: Vec<InlineHint> = self.hints.values().cloned().collect();
        hints.sort_by(|a, b| {
            a.range
                .start
                .cmp(&b.range.start)
                .then_with(|| a.range.end.cmp(&b.range.end))
                .then_with(|| a.id.cmp(&b.id))
        });
        hints
    }

    fn prepared_hints(&self, rope: &Rope) -> Vec<PreparedInlineHint> {
        let len_chars = rope.len_chars();
        let mut prepared: Vec<PreparedInlineHint> = self
            .hints
            .values()
            .map(|hint| Self::prepare_hint(hint, rope, len_chars))
            .collect();

        prepared.sort_by(|a, b| {
            a.anchor
                .cmp(&b.anchor)
                .then_with(|| a.placement.cmp(&b.placement))
                .then_with(|| a.id.cmp(&b.id))
        });
        prepared
    }

    fn inline_segments_for_line(
        &self,
        rope: &Rope,
        len_chars: usize,
        line_index: usize,
    ) -> Vec<PreparedInlineHint> {
        if self.hints.is_empty() {
            return Vec::new();
        }

        let line_start = rope.line_to_char(line_index).min(len_chars);
        let line_end = rope
            .line_to_char((line_index + 1).min(rope.len_lines()))
            .min(len_chars);

        let mut prepared: Vec<PreparedInlineHint> = self
            .hints
            .values()
            .filter(|hint| matches!(hint.placement, InlineHintPlacement::Inline))
            .filter_map(|hint| {
                let anchor = hint.range.start.min(len_chars);
                if anchor < line_start || anchor > line_end {
                    return None;
                }
                Some(Self::prepare_hint(hint, rope, len_chars))
            })
            .collect();

        prepared.sort_by(|a, b| a.anchor.cmp(&b.anchor).then_with(|| a.id.cmp(&b.id)));
        prepared
    }

    fn apply_edit(&mut self, transaction: &EditTransaction) {
        if self.is_empty() || transaction.is_empty() {
            return;
        }

        for edit in transaction.edits.iter().rev() {
            self.apply_text_edit(edit);
        }
    }

    fn apply_text_edit(&mut self, edit: &TextEdit) {
        let geometry = EditGeometry::new(edit);
        if geometry.is_noop() {
            return;
        }

        for hint in self.hints.values_mut() {
            let new_start = geometry.map_index(hint.range.start);
            let new_end = geometry.map_index(hint.range.end);
            hint.range.start = new_start.min(new_end);
            hint.range.end = new_start.max(new_end);
        }
    }

    fn prepare_hint(hint: &InlineHint, rope: &Rope, len_chars: usize) -> PreparedInlineHint {
        let anchor = hint.range.start.min(len_chars);
        let text = Self::sanitize_text(&hint.text, hint.placement);
        let width = text.chars().map(InputState::char_width).sum();
        let line = Self::line_for_anchor(rope, len_chars, anchor);

        PreparedInlineHint {
            id: hint.id,
            anchor,
            text,
            width,
            style: hint.style.clone(),
            placement: hint.placement,
            line,
        }
    }

    fn sanitize_text(text: &Rope, placement: InlineHintPlacement) -> String {
        let raw = text.to_string();
        match placement {
            InlineHintPlacement::Inline => raw.replace('\n', " "),
            InlineHintPlacement::BlockAbove | InlineHintPlacement::BlockBelow => raw,
        }
    }

    fn line_for_anchor(rope: &Rope, len_chars: usize, anchor: usize) -> usize {
        if rope.len_lines() == 0 {
            return 0;
        }

        if len_chars == 0 {
            return 0;
        }

        let max_index = len_chars.saturating_sub(1);
        let clamped = anchor.min(max_index);
        rope.char_to_line(clamped)
    }
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

/// Individual edit that can participate in undo/redo.
#[derive(Clone, Debug)]
struct EditCommand {
    before: CursorSetSnapshot,
    after: CursorSetSnapshot,
    transaction: EditTransaction,
}

impl EditCommand {
    fn new(
        before: CursorSetSnapshot,
        after: CursorSetSnapshot,
        transaction: EditTransaction,
    ) -> Self {
        Self {
            before,
            after,
            transaction,
        }
    }

    fn inverse(&self) -> Self {
        Self {
            before: self.after.clone(),
            after: self.before.clone(),
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

    fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }

    fn apply(&self, rope: &mut Rope) {
        for edit in self.edits.iter().rev() {
            edit.apply(rope);
        }
    }

    fn inverse(&self) -> Self {
        let edits = self.edits.iter().rev().map(|edit| edit.inverse()).collect();
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

    fn cursor_set(&self) -> &CursorSet {
        &self.primary_view().cursor_set
    }

    fn cursor_set_mut(&mut self) -> &mut CursorSet {
        &mut self.primary_view_mut().cursor_set
    }

    fn primary_caret(&self) -> &Caret {
        self.primary_view().primary_caret()
    }

    fn primary_caret_mut(&mut self) -> &mut Caret {
        self.primary_view_mut().primary_caret_mut()
    }

    fn cursor_position_mut(&mut self) -> &mut usize {
        &mut self.primary_caret_mut().head
    }

    fn anchor_position_mut(&mut self) -> &mut usize {
        &mut self.primary_caret_mut().anchor
    }

    fn caret_snapshot(&self) -> CursorSetSnapshot {
        self.primary_view().cursor_set.snapshot()
    }

    fn set_caret_snapshot(&mut self, snapshot: &CursorSetSnapshot) {
        self.primary_view_mut().cursor_set.restore(snapshot);
    }

    #[cfg(test)]
    fn snapshot_with_primary<F>(&self, f: F) -> CursorSetSnapshot
    where
        F: FnOnce(&mut Caret),
    {
        let cursor_set = self.cursor_set();
        let mut carets: Vec<Caret> = cursor_set.iter().cloned().collect();
        if carets.is_empty() {
            carets.push(Caret::default());
        }
        let primary = cursor_set
            .primary_index()
            .min(carets.len().saturating_sub(1));
        if let Some(caret) = carets.get_mut(primary) {
            f(caret);
        }
        CursorSetSnapshot::new(carets, primary)
    }

    fn caret_infos(&self) -> Vec<CaretInfo> {
        self.cursor_set()
            .iter()
            .enumerate()
            .map(|(index, caret)| CaretInfo::new(index, caret))
            .collect()
    }

    fn commit_caret_edits(&mut self, edits: Vec<CaretEdit>) -> bool {
        if edits.is_empty() {
            return false;
        }

        let mut edits = edits;
        edits.sort_by(|a, b| {
            a.range
                .start
                .cmp(&b.range.start)
                .then_with(|| a.range.end.cmp(&b.range.end))
                .then_with(|| a.index.cmp(&b.index))
        });

        debug_assert!(
            edits
                .windows(2)
                .all(|pair| pair[0].range.end <= pair[1].range.start),
            "overlapping caret edits are not supported"
        );

        let before = self.caret_snapshot();
        let primary_index = before.primary_index();
        let mut carets_after: Vec<Caret> = before.carets().to_vec();

        let mut text_edits = Vec::with_capacity(edits.len());
        let mut cumulative_delta: isize = 0;
        for edit in edits.iter() {
            let deleted = self.rope_slice_to_string(edit.range.clone());
            let adjusted_start = (edit.range.start as isize + cumulative_delta).max(0) as usize;
            let insert_len = edit.inserted.chars().count();
            let deleted_len = edit.deleted_len();
            let anchor_offset = edit.anchor_offset;
            let head_offset = edit.head_offset;

            if let Some(caret) = carets_after.get_mut(edit.index) {
                caret.head = adjusted_start + head_offset;
                caret.anchor = adjusted_start + anchor_offset;
                caret.preferred_column = None;
            }

            text_edits.push(TextEdit::new(
                edit.range.clone(),
                deleted,
                edit.inserted.clone(),
            ));

            cumulative_delta += insert_len as isize - deleted_len as isize;
        }

        let transaction = EditTransaction::new(text_edits);
        if transaction.is_empty() {
            return false;
        }

        let after = CursorSetSnapshot::new(carets_after, primary_index);
        let command = EditCommand::new(before, after, transaction);
        self.commit_edit(command)
    }

    fn build_caret_edits<F>(&self, mut f: F) -> Vec<CaretEdit>
    where
        F: FnMut(&CaretInfo) -> Option<CaretEdit>,
    {
        self.caret_infos()
            .into_iter()
            .filter_map(|info| f(&info))
            .collect()
    }

    fn build_caret_updates<F>(&self, mut f: F) -> Vec<CaretUpdate>
    where
        F: FnMut(&CaretInfo) -> Option<CaretUpdate>,
    {
        self.caret_infos()
            .into_iter()
            .filter_map(|info| f(&info))
            .collect()
    }

    fn apply_caret_updates(&mut self, updates: Vec<CaretUpdate>) -> bool {
        if updates.is_empty() {
            return false;
        }

        let cursor_set = self.cursor_set_mut();
        let mut changed = false;
        for update in updates {
            if let Some(caret) = cursor_set.get_mut(update.index) {
                if caret.head == update.head
                    && caret.anchor == update.anchor
                    && caret.preferred_column == update.preferred_column
                {
                    continue;
                }
                caret.head = update.head;
                caret.anchor = update.anchor;
                caret.preferred_column = update.preferred_column;
                changed = true;
            }
        }
        changed
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
        self.inline_hints.apply_edit(&command.transaction);
        self.last_change = Some(TextChangeSet::from_transaction(&command.transaction));
        self.set_caret_snapshot(&command.after);
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        self.refresh_gutter_metrics();
    }

    fn mark_document_dirty(&mut self) {
        self.document.revision = self.document.revision.wrapping_add(1);
        self.document.is_dirty = true;
    }

    pub fn revision(&self) -> u64 {
        self.document.revision
    }

    fn rope_slice_to_string(&self, range: Range<usize>) -> String {
        let len = self.document.rope.len_chars();
        let start = range.start.min(len);
        let end = range.end.min(len);
        if start >= end {
            String::new()
        } else {
            self.document.rope.slice(start..end).to_string()
        }
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

    pub fn set_inline_hint(&mut self, hint: InlineHint) {
        self.inline_hints.set(hint);
    }

    pub fn remove_inline_hint(&mut self, id: InlineHintId) -> bool {
        self.inline_hints.remove(id)
    }

    pub fn clear_inline_hints(&mut self) {
        self.inline_hints.clear();
    }

    pub fn inline_hints(&self) -> Vec<InlineHint> {
        self.inline_hints.ordered()
    }

    pub fn cursor(&self) -> usize {
        self.primary_caret().head
    }

    pub fn carets(&self) -> Vec<CursorPosition> {
        self.cursor_set()
            .iter()
            .map(|caret| CursorPosition {
                head: caret.head,
                anchor: caret.anchor,
            })
            .collect()
    }

    pub fn primary_caret_position(&self) -> CursorPosition {
        let caret = self.primary_caret();
        CursorPosition {
            head: caret.head,
            anchor: caret.anchor,
        }
    }

    pub fn primary_caret_index(&self) -> usize {
        self.cursor_set().primary_index()
    }

    pub fn cursor_row(&self) -> usize {
        self.cursor_line()
    }

    pub fn cursor_column(&self) -> usize {
        let cursor_line = self.cursor_line();
        self.column_in_line(cursor_line, self.cursor())
    }

    pub fn set_carets<I>(&mut self, carets: I, primary_index: usize)
    where
        I: IntoIterator<Item = CursorPosition>,
    {
        let len = self.len_chars();
        let mut entries: Vec<(Caret, bool)> = carets
            .into_iter()
            .enumerate()
            .map(|(idx, pos)| {
                let caret = Caret {
                    head: pos.head.min(len),
                    anchor: pos.anchor.min(len),
                    preferred_column: None,
                };
                (caret, idx == primary_index)
            })
            .collect();

        if entries.is_empty() {
            entries.push((Caret::default(), true));
        } else {
            entries.sort_by(|(a, _), (b, _)| {
                let a_start = a.head.min(a.anchor);
                let b_start = b.head.min(b.anchor);
                a_start
                    .cmp(&b_start)
                    .then_with(|| a.head.cmp(&b.head))
                    .then_with(|| a.anchor.cmp(&b.anchor))
            });

            let mut merged: Vec<(Caret, bool)> = Vec::with_capacity(entries.len());
            for (caret, is_primary) in entries.into_iter() {
                if let Some((last_caret, last_primary)) = merged.last_mut()
                    && last_caret.head == caret.head
                    && last_caret.anchor == caret.anchor
                {
                    if is_primary {
                        *last_primary = true;
                    }
                    continue;
                }
                merged.push((caret, is_primary));
            }
            entries = merged;
        }

        let primary_idx = entries
            .iter()
            .position(|(_, is_primary)| *is_primary)
            .unwrap_or(0);

        let new_carets: Vec<Caret> = entries.into_iter().map(|(caret, _)| caret).collect();

        {
            let cursor_set = self.cursor_set_mut();
            cursor_set.replace_all(new_carets, primary_idx);
            cursor_set.reset_primary_preferred_column();
        }
        self.ensure_cursor_visible();
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
        self.inline_hints.clear();
        let caret = Caret {
            head: 0,
            anchor: 0,
            preferred_column: None,
        };
        {
            let cursor_set = self.cursor_set_mut();
            cursor_set.replace_all(vec![caret], 0);
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
        self.inline_hints.clear();
        let len = self.len_chars();
        let caret = Caret {
            head: len,
            anchor: len,
            preferred_column: None,
        };
        {
            let cursor_set = self.cursor_set_mut();
            cursor_set.replace_all(vec![caret], 0);
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

    pub fn add_caret_above(&mut self) -> bool {
        self.add_caret_vertical(-1)
    }

    pub fn add_caret_below(&mut self) -> bool {
        self.add_caret_vertical(1)
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
        let rope = &self.document.rope;
        let len = self.len_chars();
        let start = start.min(len);
        let end = end.min(len);
        if start >= end {
            return 0;
        }

        if rope.slice(start..end).chars().any(|ch| ch == '\n') {
            return 0;
        }

        let mut width = rope
            .slice(start..end)
            .chars()
            .map(InputState::char_width)
            .sum::<usize>();

        let line_index = rope.char_to_line(start);
        let inline_hints = self
            .inline_hints
            .inline_segments_for_line(rope, len, line_index);

        for hint in inline_hints {
            if hint.anchor < start || hint.anchor >= end {
                continue;
            }
            width += hint.width;
        }

        width
    }

    fn index_at_line_column(&self, line_index: usize, column: usize) -> usize {
        let rope = &self.document.rope;
        let len = self.len_chars();
        let inline_hints = self
            .inline_hints
            .inline_segments_for_line(rope, len, line_index);
        self.index_at_line_column_with_hints(line_index, column, &inline_hints)
    }

    fn index_at_line_column_with_hints(
        &self,
        line_index: usize,
        column: usize,
        inline_hints: &[PreparedInlineHint],
    ) -> usize {
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
        let mut hint_idx = 0usize;

        while hint_idx < inline_hints.len() && inline_hints[hint_idx].anchor < line_start {
            hint_idx += 1;
        }

        while idx < line_end {
            while hint_idx < inline_hints.len() && inline_hints[hint_idx].anchor == idx {
                let hint = &inline_hints[hint_idx];
                if col + hint.width > column {
                    return idx;
                }
                col += hint.width;
                hint_idx += 1;
            }

            let ch = self.document.rope.char(idx);
            if ch == '\n' {
                break;
            }
            let width = Self::char_width(ch);
            if col + width > column {
                return idx;
            }
            col += width;
            idx += 1;
        }

        while hint_idx < inline_hints.len() && inline_hints[hint_idx].anchor <= line_end {
            let hint = &inline_hints[hint_idx];
            if col + hint.width > column {
                return line_end;
            }
            col += hint.width;
            hint_idx += 1;
        }

        if col >= column { idx } else { line_end }
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
            InputMsg::MoveWordLeft { extend } => self.move_word_left(extend),
            InputMsg::MoveWordRight { extend } => self.move_word_right(extend),
            InputMsg::Pointer {
                column,
                row,
                click_count,
                modifiers,
                scroll_y,
            } => self.handle_pointer(
                column as usize,
                row as usize,
                click_count.max(1),
                modifiers,
                scroll_y as usize,
            ),
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
                let updates = self.build_caret_updates(|info| {
                    if info.has_selection() {
                        Some(CaretUpdate::new(
                            info.index,
                            info.head,
                            info.head,
                            info.preferred_column,
                        ))
                    } else {
                        None
                    }
                });

                if self.apply_caret_updates(updates) {
                    self.reset_preferred_column();
                    self.ensure_cursor_visible();
                    true
                } else {
                    false
                }
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

        let inserted_len = sanitized.chars().count();
        let len = self.len_chars();
        let edits = self.build_caret_edits(|info| {
            let range = info.clamped_range(len);
            Some(CaretEdit::new(
                info.index,
                range,
                sanitized.clone(),
                inserted_len,
                inserted_len,
            ))
        });

        self.commit_caret_edits(edits)
    }

    fn delete_backward(&mut self) -> bool {
        let len = self.len_chars();
        let edits = self.build_caret_edits(|info| {
            let (head, anchor) = info.clamped_positions(len);
            if head != anchor {
                return Some(CaretEdit::new(
                    info.index,
                    info.clamped_range(len),
                    String::new(),
                    0,
                    0,
                ));
            }

            if head == 0 {
                return None;
            }

            Some(CaretEdit::new(
                info.index,
                head - 1..head,
                String::new(),
                0,
                0,
            ))
        });

        self.commit_caret_edits(edits)
    }

    fn delete_word_backward(&mut self) -> bool {
        let len = self.len_chars();
        let edits = self.build_caret_edits(|info| {
            let (head, anchor) = info.clamped_positions(len);
            if head != anchor {
                return Some(CaretEdit::new(
                    info.index,
                    if head <= anchor {
                        head..anchor
                    } else {
                        anchor..head
                    },
                    String::new(),
                    0,
                    0,
                ));
            }

            if head == 0 {
                return None;
            }

            let boundary = self.word_start_before(head);
            if boundary == info.head {
                return None;
            }

            Some(CaretEdit::new(
                info.index,
                boundary..head,
                String::new(),
                0,
                0,
            ))
        });

        self.commit_caret_edits(edits)
    }

    fn delete_to_start(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        let primary = self.primary_caret_index();
        let len = self.len_chars();
        let edits = self.build_caret_edits(|info| {
            if info.index != primary {
                return None;
            }
            let head = info.head.min(len);
            if head == 0 {
                return None;
            }
            Some(CaretEdit::new(info.index, 0..head, String::new(), 0, 0))
        });

        if edits.is_empty() {
            self.ensure_cursor_visible();
            return false;
        }

        self.commit_caret_edits(edits)
    }

    fn delete_to_end(&mut self) -> bool {
        if self.delete_selection() {
            return true;
        }

        let len = self.len_chars();
        let primary = self.primary_caret_index();
        let edits = self.build_caret_edits(|info| {
            if info.index != primary {
                return None;
            }
            if info.head >= len {
                return None;
            }
            Some(CaretEdit::new(
                info.index,
                info.head.min(len)..len,
                String::new(),
                0,
                0,
            ))
        });

        if edits.is_empty() {
            return false;
        }

        self.commit_caret_edits(edits)
    }

    fn move_left(&mut self, extend: bool) -> bool {
        let len = self.len_chars();
        let updates = self.build_caret_updates(|info| {
            let (head, anchor) = info.clamped_positions(len);
            if !extend && head != anchor {
                let pos = if head <= anchor { head } else { anchor };
                return Some(CaretUpdate::new(info.index, pos, pos, None));
            }

            if head == 0 {
                return None;
            }

            let new_head = head - 1;
            let new_anchor = if extend { anchor } else { new_head };
            Some(CaretUpdate::new(info.index, new_head, new_anchor, None))
        });

        let changed = self.apply_caret_updates(updates);
        if changed {
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        } else if !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        changed
    }

    fn ensure_cursor_visible(&mut self) {
        // Cursor visibility is now handled by the scroll container wrapping the input.
        // Applications should use ScrollState::ensure_visible() with the cursor position.
    }

    fn move_right(&mut self, extend: bool) -> bool {
        let len = self.len_chars();
        let updates = self.build_caret_updates(|info| {
            let (head, anchor) = info.clamped_positions(len);
            if !extend && head != anchor {
                let pos = if head <= anchor { anchor } else { head };
                return Some(CaretUpdate::new(info.index, pos, pos, None));
            }

            if head >= len {
                return None;
            }

            let new_head = head + 1;
            let new_anchor = if extend { anchor } else { new_head };
            Some(CaretUpdate::new(info.index, new_head, new_anchor, None))
        });

        let changed = self.apply_caret_updates(updates);
        if changed {
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        } else if !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        changed
    }

    fn move_up(&mut self, extend: bool) -> bool {
        self.move_vertical(-1, extend)
    }

    fn move_down(&mut self, extend: bool) -> bool {
        self.move_vertical(1, extend)
    }

    fn add_caret_vertical(&mut self, line_delta: isize) -> bool {
        if line_delta == 0 {
            return false;
        }

        if !self.is_multiline() {
            return false;
        }

        let len_chars = self.len_chars();
        let rope = &self.document.rope;
        let line_count = self.line_count();

        let primary_head = self.primary_caret().head.min(len_chars);
        let current_line = rope.char_to_line(primary_head);

        let target_line = if line_delta < 0 {
            let delta = line_delta.unsigned_abs();
            if current_line < delta {
                return false;
            }
            current_line - delta
        } else {
            let delta = line_delta as usize;
            let target = current_line + delta;
            if target >= line_count {
                return false;
            }
            target
        };

        let desired_column = self
            .primary_caret()
            .preferred_column
            .unwrap_or_else(|| self.column_in_line(current_line, primary_head));
        let target_index = self.index_at_line_column(target_line, desired_column);

        let already_exists = self
            .cursor_set()
            .iter()
            .any(|caret| caret.head == target_index);

        {
            let view = self.primary_view_mut();
            let idx = view.cursor_set.upsert_caret(
                Caret {
                    head: target_index,
                    anchor: target_index,
                    preferred_column: Some(desired_column),
                },
                true,
            );
            if let Some(caret) = view.cursor_set.get_mut(idx) {
                caret.preferred_column = Some(desired_column);
            }
        }

        self.ensure_cursor_visible();
        !already_exists
    }

    fn move_vertical(&mut self, line_delta: isize, extend: bool) -> bool {
        if !self.is_multiline() {
            if !extend {
                let updates = self.build_caret_updates(|info| {
                    if info.anchor != info.head {
                        Some(CaretUpdate::new(info.index, info.head, info.head, None))
                    } else {
                        None
                    }
                });
                if !updates.is_empty() {
                    self.apply_caret_updates(updates);
                }
            }
            return false;
        }

        let line_count = self.line_count();
        if line_count <= 1 {
            if !extend {
                let updates = self.build_caret_updates(|info| {
                    if info.anchor != info.head {
                        Some(CaretUpdate::new(info.index, info.head, info.head, None))
                    } else {
                        None
                    }
                });
                if !updates.is_empty() {
                    self.apply_caret_updates(updates);
                }
            }
            return false;
        }

        let rope = &self.document.rope;
        let len_chars = self.len_chars();
        let mut updates = Vec::new();

        for info in self.caret_infos() {
            let head = info.head.min(len_chars);
            let current_line = rope.char_to_line(head);
            let target_line = if line_delta < 0 {
                let delta = line_delta.unsigned_abs();
                if current_line < delta {
                    None
                } else {
                    Some(current_line - delta)
                }
            } else if line_delta > 0 {
                let delta = line_delta as usize;
                let target = current_line + delta;
                if target >= line_count {
                    None
                } else {
                    Some(target)
                }
            } else {
                Some(current_line)
            };

            let Some(target_line) = target_line else {
                if !extend && info.anchor != info.head {
                    updates.push(CaretUpdate::new(
                        info.index,
                        info.head,
                        info.head,
                        info.preferred_column,
                    ));
                }
                continue;
            };

            let desired_column = info
                .preferred_column
                .unwrap_or_else(|| self.column_in_line(current_line, head));
            let target_index = self.index_at_line_column(target_line, desired_column);

            if target_index == info.head {
                if !extend && info.anchor != info.head {
                    updates.push(CaretUpdate::new(
                        info.index,
                        info.head,
                        info.head,
                        info.preferred_column,
                    ));
                }
                continue;
            }

            let new_anchor = if extend { info.anchor } else { target_index };
            updates.push(CaretUpdate::new(
                info.index,
                target_index,
                new_anchor,
                Some(desired_column),
            ));
        }

        let changed = self.apply_caret_updates(updates);
        if !changed && !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        self.ensure_cursor_visible();
        changed
    }

    fn move_to_index(&mut self, index: usize, extend: bool) -> bool {
        let len = self.len_chars();
        let clamped = index.min(len);
        let updates = self.build_caret_updates(|info| {
            let anchor = if extend {
                info.anchor.min(len)
            } else {
                clamped
            };
            if info.head == clamped && info.anchor.min(len) == anchor {
                return None;
            }
            Some(CaretUpdate::new(info.index, clamped, anchor, None))
        });

        let changed = self.apply_caret_updates(updates);
        if changed {
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        } else if !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        changed
    }

    fn move_line_start(&mut self, extend: bool) -> bool {
        let rope = &self.document.rope;
        let len = self.len_chars();
        let updates = self.build_caret_updates(|info| {
            let head = info.head.min(len);
            let line_index = rope.char_to_line(head);
            let (line_start, _) = self.line_bounds(line_index);
            let anchor = if extend {
                info.anchor.min(len)
            } else {
                line_start
            };
            if head == line_start && info.anchor.min(len) == anchor {
                return None;
            }
            Some(CaretUpdate::new(info.index, line_start, anchor, None))
        });

        let changed = self.apply_caret_updates(updates);
        if changed {
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        } else if !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        changed
    }

    fn move_line_end(&mut self, extend: bool) -> bool {
        let rope = &self.document.rope;
        let len = self.len_chars();
        let updates = self.build_caret_updates(|info| {
            let head = info.head.min(len);
            let line_index = rope.char_to_line(head);
            let (_, line_end) = self.line_bounds(line_index);
            let anchor = if extend {
                info.anchor.min(len)
            } else {
                line_end
            };
            if head == line_end && info.anchor.min(len) == anchor {
                return None;
            }
            Some(CaretUpdate::new(info.index, line_end, anchor, None))
        });

        let changed = self.apply_caret_updates(updates);
        if changed {
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        } else if !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        changed
    }

    fn move_word_left(&mut self, extend: bool) -> bool {
        let len = self.len_chars();
        let updates = self.build_caret_updates(|info| {
            let head = info.head.min(len);
            if head == 0 {
                if !extend && info.anchor != head {
                    Some(CaretUpdate::new(info.index, head, head, None))
                } else {
                    None
                }
            } else {
                let boundary = self.word_start_before(head);
                let anchor = if extend {
                    info.anchor.min(len)
                } else {
                    boundary
                };
                if head == boundary && info.anchor.min(len) == anchor {
                    return None;
                }
                Some(CaretUpdate::new(info.index, boundary, anchor, None))
            }
        });

        let changed = self.apply_caret_updates(updates);
        if changed {
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        } else if !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        changed
    }

    fn move_word_right(&mut self, extend: bool) -> bool {
        let len = self.len_chars();
        let updates = self.build_caret_updates(|info| {
            let head = info.head.min(len);
            if head >= len {
                if !extend && info.anchor != head {
                    Some(CaretUpdate::new(info.index, head, head, None))
                } else {
                    None
                }
            } else {
                let boundary = self.next_word_boundary(head);
                let anchor = if extend {
                    info.anchor.min(len)
                } else {
                    boundary
                };
                if head == boundary && info.anchor.min(len) == anchor {
                    return None;
                }
                Some(CaretUpdate::new(info.index, boundary, anchor, None))
            }
        });

        let changed = self.apply_caret_updates(updates);
        if changed {
            self.reset_preferred_column();
            self.ensure_cursor_visible();
        } else if !extend {
            self.cursor_set_mut().reset_primary_preferred_column();
        }
        changed
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
        let len = self.len_chars();
        let edits = self.build_caret_edits(|info| {
            if !info.has_selection() {
                return None;
            }
            Some(CaretEdit::new(
                info.index,
                info.clamped_range(len),
                String::new(),
                0,
                0,
            ))
        });

        self.commit_caret_edits(edits)
    }

    fn handle_pointer(
        &mut self,
        column: usize,
        row: usize,
        click_count: u8,
        modifiers: PointerModifiers,
        scroll_y: usize,
    ) -> bool {
        let index = self.column_to_index(column, row, scroll_y);

        if modifiers.alt {
            return self.add_pointer_caret(index, click_count.max(1));
        }

        match click_count {
            1 => {
                let len = self.len_chars();
                let anchor = if modifiers.shift {
                    self.primary_caret().anchor.min(len)
                } else {
                    index
                };
                self.set_carets(
                    [CursorPosition {
                        head: index,
                        anchor,
                    }],
                    0,
                );
                self.reset_preferred_column();
                true
            }
            2 => {
                let (start, end) = self.word_range_at(index);
                self.set_carets(
                    [CursorPosition {
                        head: end,
                        anchor: start,
                    }],
                    0,
                );
                self.reset_preferred_column();
                true
            }
            3 => {
                let len = self.len_chars();
                self.set_carets(
                    [CursorPosition {
                        head: len,
                        anchor: 0,
                    }],
                    0,
                );
                self.reset_preferred_column();
                true
            }
            _ => {
                self.set_carets(
                    [CursorPosition {
                        head: index,
                        anchor: index,
                    }],
                    0,
                );
                self.reset_preferred_column();
                true
            }
        }
    }

    fn column_to_index(&self, column: usize, row: usize, scroll_y: usize) -> usize {
        let gutter_width = self.gutter_width();
        let text_column = column.saturating_sub(gutter_width);

        if self.is_multiline() {
            let line_count = self.line_count();
            let max_index = line_count.saturating_sub(1);
            let absolute_row = scroll_y.saturating_add(row).min(max_index);
            self.index_at_line_column(absolute_row, text_column)
        } else {
            let mut consumed = 0usize;
            let mut index = 0usize;
            for ch in self.document.rope.chars() {
                let width = Self::char_width(ch);
                if text_column < consumed + width {
                    return index;
                }
                consumed += width;
                index += 1;
            }
            index
        }
    }

    fn add_pointer_caret(&mut self, index: usize, click_count: u8) -> bool {
        let click = click_count.max(1);
        let (head, anchor) = match click {
            1 => (index, index),
            2 => {
                let (start, end) = self.word_range_at(index);
                (end, start)
            }
            3 => {
                let len = self.len_chars();
                (len, 0)
            }
            _ => (index, index),
        };

        let caret = Caret {
            head,
            anchor,
            preferred_column: None,
        };

        let cursor_set = &mut self.primary_view_mut().cursor_set;
        cursor_set.upsert_caret(caret, true);
        self.reset_preferred_column();
        self.ensure_cursor_visible();
        true
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
        modifiers: PointerModifiers,
        scroll_y: u16,
    },
    Replace(String),
    SelectRange {
        start: usize,
        end: usize,
    },
    ClearSelection,
}

/// Classification of characters for word boundary detection.
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
    let mut node = renderable::<Msg>(input_renderable).with_id(id);

    let handler = Rc::new(map_msg);
    let mouse_handler = handler.clone();

    node = node.on_mouse(move |event: MouseEvent| {
        if event.buttons.left && event.click_count > 0 {
            let msg = InputMsg::Pointer {
                column: event.local_x,
                row: event.local_y,
                click_count: event.click_count,
                modifiers: PointerModifiers {
                    ctrl: event.ctrl,
                    alt: event.alt,
                    shift: event.shift,
                    super_key: event.super_key,
                },
                scroll_y: 0,
            };
            Some(mouse_handler(msg))
        } else {
            None
        }
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
    primary_cursor: usize,
    cursor_positions: HashSet<usize>,
    selection: Option<(usize, usize)>,
    selection_ranges: Vec<(usize, usize)>, // all caret selections
    base_style: Style,
    selection_style: Style,
    cursor_style: Style,
    max_width: usize,
    content_line_count: usize,
    visual_row_count: usize,
    cursor_line: usize,
    cursor_visual_line: usize,
    len_chars: usize,
    highlight_store: HighlightStore,
    gutter_metrics: GutterMetrics,
    gutter_style: Style,
    hint_layout: InlineHintLayout,
}

#[derive(Clone, Debug, PartialEq)]
enum RunStyle {
    Base,
    Selection,
    Cursor,
    Highlight(Style),
    InlineHint(Style),
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
        let cursor_positions: HashSet<usize> = state
            .carets()
            .into_iter()
            .map(|c| c.head.min(len_chars))
            .collect();

        let base_style = style.text.clone();
        let selection_style = style.selection.clone();
        let cursor_style = style.cursor.clone();

        let selection_ranges: Vec<(usize, usize)> = state
            .carets()
            .into_iter()
            .filter_map(|c| {
                if c.head == c.anchor {
                    None
                } else {
                    let (start, end) = if c.head < c.anchor {
                        (c.head, c.anchor)
                    } else {
                        (c.anchor, c.head)
                    };
                    Some((start.min(len_chars), end.min(len_chars)))
                }
            })
            .collect();

        let hint_layout = InlineHintLayout::build(&state.inline_hints, &rope);
        let content_line_count = hint_layout.content_line_count();
        let gutter_metrics = state.primary_view().gutter.metrics(content_line_count);
        let gutter_style = style.gutter.clone();
        let cursor_line = rope.char_to_line(cursor);
        let cursor_char = (cursor < len_chars).then(|| rope.char(cursor));

        let mut max_line_width = 0usize;
        for line_idx in 0..content_line_count {
            let inline_hints = hint_layout.inline_hints_for_line(line_idx);
            let mut width = Self::line_display_width(&rope, line_idx, inline_hints);
            if selection.is_none()
                && line_idx == cursor_line
                && (cursor == len_chars || matches!(cursor_char, Some('\n')))
            {
                width += 1;
            }
            max_line_width = max_line_width.max(width);
        }

        for hint in hint_layout.block_hints() {
            max_line_width = max_line_width.max(hint.width);
        }

        if max_line_width == 0 && selection.is_none() {
            max_line_width = 1;
        }

        max_line_width = max_line_width.max(1);
        let max_width = max_line_width + gutter_metrics.width;

        let visual_row_count = hint_layout.visual_row_count().max(1);
        let cursor_visual_line = hint_layout
            .visual_rows()
            .iter()
            .position(|entry| matches!(entry, VisualRowEntry::Text { line_idx } if *line_idx == cursor_line))
            .unwrap_or(0);

        Self {
            rope,
            primary_cursor: cursor,
            cursor_positions,
            selection,
            selection_ranges,
            base_style,
            selection_style,
            cursor_style,
            max_width,
            content_line_count,
            visual_row_count,
            cursor_line,
            cursor_visual_line,
            len_chars,
            highlight_store: state.highlights.clone(),
            gutter_metrics,
            gutter_style,
            hint_layout,
        }
    }

    fn line_display_width(
        rope: &Rope,
        line_idx: usize,
        inline_hints: &[PreparedInlineHint],
    ) -> usize {
        let text_width = rope
            .line(line_idx)
            .chars()
            .filter(|&ch| ch != '\n')
            .map(InputState::char_width)
            .sum::<usize>();
        let hint_width = inline_hints.iter().map(|hint| hint.width).sum::<usize>();
        text_width + hint_width
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
                if line_idx >= self.content_line_count {
                    return None;
                }
                let number = line_idx + 1;
                Some(format!(
                    "{number:>digits$} ",
                    number = number,
                    digits = self.gutter_metrics.digits
                ))
            }
        }
    }

    fn run_style_for_index(&self, idx: usize, highlight_runs: &[HighlightRun]) -> RunStyle {
        for (start, end) in &self.selection_ranges {
            if idx >= *start && idx < *end {
                return RunStyle::Selection;
            }
        }
        if self.cursor_positions.contains(&idx)
            && idx < self.len_chars
            && idx != self.primary_cursor
        {
            return RunStyle::Cursor;
        }
        if let Some(style) = Self::highlight_style_at(highlight_runs, idx) {
            return RunStyle::Highlight(style.clone());
        }
        RunStyle::Base
    }

    fn capture_cursor_position(
        &self,
        target_idx: usize,
        line_idx: usize,
        cursor_x: usize,
        y: usize,
        cursor_position: &mut Option<(usize, usize)>,
    ) {
        if self.selection.is_none()
            && cursor_position.is_none()
            && self.cursor_line == line_idx
            && self.primary_cursor == target_idx
        {
            cursor_position.replace((cursor_x, y));
        }
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
        run_start_idx: usize,
        primary_cursor: usize,
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
            RunStyle::InlineHint(hint_style) => {
                let attrs = ctx.style_to_attributes(hint_style);
                ctx.write_text(*run_start_x, y, run_text, &attrs);
            }
        }

        if matches!(style_ref, RunStyle::Cursor)
            && cursor_position.is_none()
            && run_start_idx == primary_cursor
        {
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
    fn process_character(
        &self,
        ctx: &mut RenderContext<'_>,
        style: &RunStyle,
        ch: char,
        width: usize,
        idx_for_cursor: usize,
        y: usize,
        run_style: &mut Option<RunStyle>,
        run_text: &mut String,
        run_width: &mut usize,
        run_start_x: &mut usize,
        run_start_idx: &mut usize,
        cursor_x: &mut usize,
        remaining: &mut usize,
        cursor_position: &mut Option<(usize, usize)>,
        base_attrs: &CellAttributes,
        selection_attrs: &CellAttributes,
        cursor_attrs: &CellAttributes,
    ) -> bool {
        if width == 0 {
            return false;
        }

        if !run_style.as_ref().is_some_and(|current| current == style) {
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
                *run_start_idx,
                self.primary_cursor,
            );
            if *remaining == 0 {
                return true;
            }
            *run_style = Some(style.clone());
            *run_start_x = *cursor_x;
            *run_start_idx = idx_for_cursor;
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
                *run_start_idx,
                self.primary_cursor,
            );
            return true;
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
                *run_start_idx,
                self.primary_cursor,
            );
            if *remaining == 0 {
                return true;
            }
            if run_style.is_none() {
                *run_style = Some(style.clone());
                *run_start_x = *cursor_x;
                *run_start_idx = idx_for_cursor;
            }
        }

        if idx_for_cursor == self.primary_cursor
            && cursor_position.is_none()
            && self.selection.is_none()
        {
            cursor_position.replace((*run_start_x + *run_width, y));
        }

        run_text.push(ch);
        *run_width += width;

        false
    }

    #[allow(clippy::too_many_arguments)]
    fn render_hint_segment(
        &self,
        ctx: &mut RenderContext<'_>,
        hint: &PreparedInlineHint,
        y: usize,
        line_skip: &mut usize,
        run_style: &mut Option<RunStyle>,
        run_text: &mut String,
        run_width: &mut usize,
        run_start_x: &mut usize,
        run_start_idx: &mut usize,
        cursor_x: &mut usize,
        remaining: &mut usize,
        cursor_position: &mut Option<(usize, usize)>,
        base_attrs: &CellAttributes,
        selection_attrs: &CellAttributes,
        cursor_attrs: &CellAttributes,
    ) -> bool {
        let style = RunStyle::InlineHint(hint.style.clone());
        let mut row_full = false;

        for ch in hint.text.chars() {
            let width = InputState::char_width(ch);
            if width == 0 {
                continue;
            }

            if *line_skip >= width {
                *line_skip -= width;
                continue;
            } else if *line_skip > 0 {
                *line_skip = 0;
            }

            if self.process_character(
                ctx,
                &style,
                ch,
                width,
                usize::MAX,
                y,
                run_style,
                run_text,
                run_width,
                run_start_x,
                run_start_idx,
                cursor_x,
                remaining,
                cursor_position,
                base_attrs,
                selection_attrs,
                cursor_attrs,
            ) {
                row_full = true;
                break;
            }
        }

        row_full
    }

    #[allow(clippy::too_many_arguments)]
    fn render_text_entry(
        &self,
        ctx: &mut RenderContext<'_>,
        y: usize,
        line_idx: usize,
        area_x: usize,
        area_width: usize,
        gutter_width: usize,
        skip_cols: usize,
        highlight_runs: &[HighlightRun],
        base_attrs: &CellAttributes,
        selection_attrs: &CellAttributes,
        cursor_attrs: &CellAttributes,
        gutter_attrs: &CellAttributes,
        cursor_position: &mut Option<(usize, usize)>,
    ) {
        if gutter_width > 0
            && let Some(mut text) = self.gutter_text(line_idx)
        {
            if text.len() > gutter_width {
                let start = text.len() - gutter_width;
                text.replace_range(..start, "");
            }
            ctx.write_text(area_x, y, &text, gutter_attrs);
        }

        let mut remaining = area_width.saturating_sub(gutter_width);
        if remaining == 0 {
            return;
        }

        let mut cursor_x = area_x + gutter_width;
        let mut line_skip = skip_cols;
        let line_start = self.rope.line_to_char(line_idx).min(self.len_chars);
        let next_line = if line_idx + 1 >= self.content_line_count {
            self.len_chars
        } else {
            self.rope.line_to_char(line_idx + 1).min(self.len_chars)
        };
        let line_end = next_line;

        let mut run_style: Option<RunStyle> = None;
        let mut run_text = String::new();
        let mut run_width = 0usize;
        let mut run_start_x = cursor_x;
        let mut run_start_idx = line_start;
        let inline_hints = self.hint_layout.inline_hints_for_line(line_idx);
        let mut inline_hint_idx = 0usize;
        let mut idx = line_start;
        let mut row_full = false;

        while idx < line_end && !row_full {
            while inline_hint_idx < inline_hints.len()
                && inline_hints[inline_hint_idx].anchor == idx
                && !row_full
            {
                let caret_x = if run_style.is_some() {
                    run_start_x + run_width
                } else {
                    cursor_x
                };
                self.capture_cursor_position(idx, line_idx, caret_x, y, cursor_position);
                row_full = self.render_hint_segment(
                    ctx,
                    &inline_hints[inline_hint_idx],
                    y,
                    &mut line_skip,
                    &mut run_style,
                    &mut run_text,
                    &mut run_width,
                    &mut run_start_x,
                    &mut run_start_idx,
                    &mut cursor_x,
                    &mut remaining,
                    cursor_position,
                    base_attrs,
                    selection_attrs,
                    cursor_attrs,
                );
                inline_hint_idx += 1;
            }

            if row_full {
                break;
            }

            let ch = self.rope.char(idx);
            let style = self.run_style_for_index(idx, highlight_runs);

            if ch == '\n' {
                if idx == self.primary_cursor
                    && self.selection.is_none()
                    && cursor_position.is_none()
                {
                    Self::flush_run(
                        ctx,
                        &mut run_style,
                        &mut run_text,
                        &mut run_width,
                        &mut run_start_x,
                        &mut cursor_x,
                        &mut remaining,
                        y,
                        cursor_position,
                        base_attrs,
                        selection_attrs,
                        cursor_attrs,
                        run_start_idx,
                        self.primary_cursor,
                    );
                    cursor_position.replace((cursor_x, y));
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

            row_full = self.process_character(
                ctx,
                &style,
                ch,
                width,
                idx,
                y,
                &mut run_style,
                &mut run_text,
                &mut run_width,
                &mut run_start_x,
                &mut run_start_idx,
                &mut cursor_x,
                &mut remaining,
                cursor_position,
                base_attrs,
                selection_attrs,
                cursor_attrs,
            );

            idx += 1;
        }

        if !row_full {
            let caret_x = if run_style.is_some() {
                run_start_x + run_width
            } else {
                cursor_x
            };
            self.capture_cursor_position(line_end, line_idx, caret_x, y, cursor_position);
            while inline_hint_idx < inline_hints.len()
                && inline_hints[inline_hint_idx].anchor == line_end
                && !row_full
            {
                row_full = self.render_hint_segment(
                    ctx,
                    &inline_hints[inline_hint_idx],
                    y,
                    &mut line_skip,
                    &mut run_style,
                    &mut run_text,
                    &mut run_width,
                    &mut run_start_x,
                    &mut run_start_idx,
                    &mut cursor_x,
                    &mut remaining,
                    cursor_position,
                    base_attrs,
                    selection_attrs,
                    cursor_attrs,
                );
                inline_hint_idx += 1;
            }
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
            cursor_position,
            base_attrs,
            selection_attrs,
            cursor_attrs,
            run_start_idx,
            self.primary_cursor,
        );
    }

    #[allow(clippy::too_many_arguments)]
    fn render_block_hint_entry(
        &self,
        ctx: &mut RenderContext<'_>,
        y: usize,
        hint_index: usize,
        area_x: usize,
        area_width: usize,
        gutter_width: usize,
        skip_cols: usize,
        base_attrs: &CellAttributes,
        selection_attrs: &CellAttributes,
        cursor_attrs: &CellAttributes,
        cursor_position: &mut Option<(usize, usize)>,
    ) {
        let mut remaining = area_width.saturating_sub(gutter_width);
        if remaining == 0 {
            return;
        }

        let mut cursor_x = area_x + gutter_width;
        let mut line_skip = skip_cols;
        let mut run_style: Option<RunStyle> = None;
        let mut run_text = String::new();
        let mut run_width = 0usize;
        let mut run_start_x = cursor_x;
        let mut run_start_idx = usize::MAX;

        let hint = self.hint_layout.block_hint(hint_index);
        let _ = self.render_hint_segment(
            ctx,
            hint,
            y,
            &mut line_skip,
            &mut run_style,
            &mut run_text,
            &mut run_width,
            &mut run_start_x,
            &mut run_start_idx,
            &mut cursor_x,
            &mut remaining,
            cursor_position,
            base_attrs,
            selection_attrs,
            cursor_attrs,
        );

        Self::flush_run(
            ctx,
            &mut run_style,
            &mut run_text,
            &mut run_width,
            &mut run_start_x,
            &mut cursor_x,
            &mut remaining,
            y,
            cursor_position,
            base_attrs,
            selection_attrs,
            cursor_attrs,
            run_start_idx,
            self.primary_cursor,
        );
    }
}

impl Renderable for InputRenderable {
    fn eq(&self, other: &dyn Renderable) -> bool {
        let Some(o) = other.as_any().downcast_ref::<Self>() else {
            return false;
        };

        if o.rope != self.rope
            || o.primary_cursor != self.primary_cursor
            || o.selection != self.selection
            || o.base_style != self.base_style
            || o.selection_style != self.selection_style
            || o.cursor_style != self.cursor_style
            || o.max_width != self.max_width
            || o.content_line_count != self.content_line_count
            || o.visual_row_count != self.visual_row_count
            || o.cursor_line != self.cursor_line
            || o.cursor_visual_line != self.cursor_visual_line
            || o.len_chars != self.len_chars
            || o.gutter_metrics != self.gutter_metrics
            || o.gutter_style != self.gutter_style
            || o.hint_layout != self.hint_layout
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

        let content_height = self.visual_row_count.max(1) as f32;
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
        let visual_rows = self.hint_layout.visual_rows();

        let blank_line = " ".repeat(area.width);
        let mut cursor_position: Option<(usize, usize)> = None;

        for row in 0..area.height {
            let y = area.y + row;
            if !blank_line.is_empty() {
                ctx.write_text(area.x, y, &blank_line, &base_attrs);
            }

            let visual_idx = scroll_y + row;
            let Some(entry) = visual_rows.get(visual_idx) else {
                if gutter_width > 0
                    && let Some(mut text) = self.gutter_text(visual_idx)
                {
                    if text.len() > gutter_width {
                        let start = text.len() - gutter_width;
                        text.replace_range(..start, "");
                    }
                    ctx.write_text(area.x, y, &text, &gutter_attrs);
                }
                continue;
            };

            match entry {
                VisualRowEntry::Text { line_idx } => {
                    self.render_text_entry(
                        ctx,
                        y,
                        *line_idx,
                        area.x,
                        area.width,
                        gutter_width,
                        skip_cols,
                        highlight_runs,
                        &base_attrs,
                        &selection_attrs,
                        &cursor_attrs,
                        &gutter_attrs,
                        &mut cursor_position,
                    );
                }
                VisualRowEntry::BlockHint { hint_index } => {
                    self.render_block_hint_entry(
                        ctx,
                        y,
                        *hint_index,
                        area.x,
                        area.width,
                        gutter_width,
                        skip_cols,
                        &base_attrs,
                        &selection_attrs,
                        &cursor_attrs,
                        &mut cursor_position,
                    );
                }
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
        KeyCode::Home => {
            if key.ctrl {
                Some(InputMsg::MoveToStart { extend: key.shift })
            } else if state.is_multiline() {
                Some(InputMsg::MoveLineStart { extend: key.shift })
            } else {
                Some(InputMsg::MoveToStart { extend: key.shift })
            }
        }
        KeyCode::End => {
            if key.ctrl {
                Some(InputMsg::MoveToEnd { extend: key.shift })
            } else if state.is_multiline() {
                Some(InputMsg::MoveLineEnd { extend: key.shift })
            } else {
                Some(InputMsg::MoveToEnd { extend: key.shift })
            }
        }
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
        KeyCode::PageUp | KeyCode::PageDown => None,
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
        let after = state.snapshot_with_primary(|caret| {
            caret.head = 0;
            caret.anchor = 0;
            caret.preferred_column = None;
        });
        let command = EditCommand::new(before, after, transaction);

        assert!(state.commit_edit(command));
        assert_eq!(state.value(), "12cdYZ");

        assert!(state.undo());
        assert_eq!(state.value(), "abcdef");
    }

    #[test]
    fn multi_caret_insert_text_inserts_at_each_caret() {
        let mut state = InputState::with_value("abcd");
        state.set_carets(
            [
                CursorPosition { head: 0, anchor: 0 },
                CursorPosition {
                    head: state.len_chars(),
                    anchor: state.len_chars(),
                },
            ],
            0,
        );

        assert!(state.update(InputMsg::InsertText("x".into())));
        assert_eq!(state.value(), "xabcdx");
        assert_eq!(
            state.carets(),
            vec![
                CursorPosition { head: 1, anchor: 1 },
                CursorPosition { head: 6, anchor: 6 },
            ]
        );
    }

    #[test]
    fn multi_caret_backspace_deletes_each_caret() {
        let mut state = InputState::with_value("abcdef");
        state.set_carets(
            [
                CursorPosition { head: 2, anchor: 2 },
                CursorPosition { head: 5, anchor: 5 },
            ],
            0,
        );

        assert!(state.update(InputMsg::DeleteBackward));
        assert_eq!(state.value(), "acdf");
        assert_eq!(
            state.carets(),
            vec![
                CursorPosition { head: 1, anchor: 1 },
                CursorPosition { head: 3, anchor: 3 },
            ]
        );
    }

    #[test]
    fn multi_caret_move_right_advances_each_caret() {
        let mut state = InputState::with_value("wxyz");
        state.set_carets(
            [
                CursorPosition { head: 0, anchor: 0 },
                CursorPosition { head: 2, anchor: 2 },
            ],
            0,
        );

        assert!(state.update(InputMsg::MoveRight { extend: false }));
        assert_eq!(
            state.carets(),
            vec![
                CursorPosition { head: 1, anchor: 1 },
                CursorPosition { head: 3, anchor: 3 },
            ]
        );
    }

    #[test]
    fn multi_caret_undo_restores_carets_and_text() {
        let mut state = InputState::with_value("hello");
        state.set_carets(
            [
                CursorPosition { head: 1, anchor: 1 },
                CursorPosition { head: 4, anchor: 4 },
            ],
            1,
        );
        let before_positions = state.carets();

        state.update(InputMsg::InsertChar('!'));
        assert_ne!(state.value(), "hello");

        assert!(state.undo());
        assert_eq!(state.value(), "hello");
        assert_eq!(state.carets(), before_positions);
    }

    #[test]
    fn set_carets_sorts_and_preserves_primary() {
        let mut state = InputState::with_value("abcd");
        state.set_carets(
            [
                CursorPosition { head: 3, anchor: 3 },
                CursorPosition { head: 1, anchor: 1 },
            ],
            0,
        );

        assert_eq!(
            state.carets(),
            vec![
                CursorPosition { head: 1, anchor: 1 },
                CursorPosition { head: 3, anchor: 3 },
            ]
        );
        assert_eq!(state.primary_caret_index(), 1);
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
            modifiers: PointerModifiers::default(),
            scroll_y: 0,
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
            modifiers: PointerModifiers::default(),
            scroll_y: 0,
        });
        assert_eq!(state.cursor(), 6);
    }

    #[test]
    fn pointer_accounts_for_gutter_offset() {
        let mut state = InputState::with_value_multiline("hello\nworld");
        state.set_line_number_gutter(true);

        let gutter_width = state.gutter_width();
        assert!(gutter_width > 0, "gutter should have non-zero width");

        state.update(InputMsg::Pointer {
            column: gutter_width as u16,
            row: 0,
            click_count: 1,
            modifiers: PointerModifiers::default(),
            scroll_y: 0,
        });
        assert_eq!(
            state.cursor(),
            0,
            "click at gutter edge should place cursor at start of line"
        );

        state.update(InputMsg::Pointer {
            column: (gutter_width + 2) as u16,
            row: 0,
            click_count: 1,
            modifiers: PointerModifiers::default(),
            scroll_y: 0,
        });
        assert_eq!(
            state.cursor(),
            2,
            "click at gutter+2 should place cursor at index 2"
        );
    }

    #[test]
    fn pointer_works_correctly_in_single_line_without_gutter() {
        let mut state = InputState::with_value("hello");

        state.update(InputMsg::Pointer {
            column: 0,
            row: 0,
            click_count: 1,
            modifiers: PointerModifiers::default(),
            scroll_y: 0,
        });
        assert_eq!(state.cursor(), 0);

        state.update(InputMsg::Pointer {
            column: 2,
            row: 0,
            click_count: 1,
            modifiers: PointerModifiers::default(),
            scroll_y: 0,
        });
        assert_eq!(state.cursor(), 2);
    }

    #[test]
    fn multiline_render_breaks_lines() {
        let state = InputState::with_value_multiline("foo\nbar");
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

        let style = InputStyle::default();

        let renderable = InputRenderable::new(&state, &style);
        assert_eq!(renderable.content_line_count, 2);

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

        assert!(first.starts_with("123"), "first line: {:?}", first);
        assert!(second.starts_with("4"), "second line: {:?}", second);
    }

    #[test]
    #[ignore = "Scrolling is now handled externally by scroll container"]
    fn multiline_vertical_scroll_keeps_cursor_visible() {
        // TODO: This test needs to be rewritten to test with external scroll state
        let mut state = InputState::with_value_multiline("a\nb\nc");
        state.update(InputMsg::MoveToStart { extend: false });
        // state.update(InputMsg::SetViewportSize { cols: 1, rows: 1 });
        // assert_eq!(state.scroll_y(), 0);

        state.update(InputMsg::MoveDown { extend: false });
        // assert_eq!(state.scroll_y(), 1);

        state.update(InputMsg::MoveDown { extend: false });
        // assert_eq!(state.scroll_y(), 2);
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
        let ctrl_a = Key::with_modifiers(KeyCode::Char('a'), true, false, false, false);
        let ctrl_shift_e = Key::with_modifiers(KeyCode::Char('e'), true, false, true, false);

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
        let ctrl_a = Key::with_modifiers(KeyCode::Char('a'), true, false, false, false);
        let ctrl_e = Key::with_modifiers(KeyCode::Char('e'), true, false, false, false);

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
    fn default_keybindings_home_end_single_line_move_buffer_bounds() {
        let state = InputState::default();
        let home = Key::new(KeyCode::Home);
        let shift_end = Key::with_modifiers(KeyCode::End, false, false, true, false);

        assert!(matches!(
            default_keybindings(&state, home, |m| m),
            Some(InputMsg::MoveToStart { extend: false })
        ));
        assert!(matches!(
            default_keybindings(&state, shift_end, |m| m),
            Some(InputMsg::MoveToEnd { extend: true })
        ));
    }

    #[test]
    fn default_keybindings_home_end_multiline_respect_ctrl() {
        let state = InputState::new_multiline();
        let home = Key::new(KeyCode::Home);
        let ctrl_home = Key::with_modifiers(KeyCode::Home, true, false, false, false);
        let end = Key::new(KeyCode::End);
        let ctrl_shift_end = Key::with_modifiers(KeyCode::End, true, false, true, false);

        assert!(matches!(
            default_keybindings(&state, home, |m| m),
            Some(InputMsg::MoveLineStart { extend: false })
        ));
        assert!(matches!(
            default_keybindings(&state, ctrl_home, |m| m),
            Some(InputMsg::MoveToStart { extend: false })
        ));
        assert!(matches!(
            default_keybindings(&state, end, |m| m),
            Some(InputMsg::MoveLineEnd { extend: false })
        ));
        assert!(matches!(
            default_keybindings(&state, ctrl_shift_end, |m| m),
            Some(InputMsg::MoveToEnd { extend: true })
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

        assert_eq!(cursor_cell.ch, 'a');
        assert_eq!(cursor_cell.attrs.background(), None);
        assert!(!cursor_cell.attrs.is_bold());

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
    #[ignore = "Scrolling is now handled externally by scroll container"]
    fn input_keeps_cursor_visible_with_horizontal_scroll() {
        // TODO: Rewrite this test to use external scroll state
        // Set long value and narrow viewport; ensure state scrolls to keep cursor visible
        let state = InputState::with_value("hello world this is long");
        // Simulate resize to 8 cols
        // state.update(InputMsg::SetViewportSize { cols: 8, rows: 1 });
        // Move cursor to end (already there by set_value)
        // Ensure scroll advanced
        // assert!(state.scroll_x() > 0);

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
    #[ignore = "Scrolling is now handled externally by scroll container"]
    fn input_windowing_handles_fullwidth_chars() {
        // TODO: Rewrite this test to use external scroll state
        // Include fullwidth char and verify it shows within window after horizontal scroll
        let mut state = InputState::with_value("ab漢cdef");
        // state.update(InputMsg::SetViewportSize { cols: 6, rows: 1 });
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

        // Remaining cells have no special styling - cursor is shown via terminal cursor
        for cell in &back[0][2..] {
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

        let text_len = state.len_chars();
        for cell in &row[..text_len] {
            assert_eq!(cell.attrs.background(), Some(expected_text_bg));
        }

        // All cells after the text should have the text background
        for cell in &row[text_len..] {
            assert_eq!(cell.ch, ' ');
            assert_eq!(cell.attrs.background(), Some(expected_text_bg));
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
        assert_eq!(row0[2].ch, ' ');
        assert_eq!(row0[3].ch, ' ');
        assert_eq!(row0[4].ch, 'f');
        assert_eq!(row0[5].ch, 'o');
        assert_eq!(row0[6].ch, 'o');
        assert!(row0[0].attrs.is_dim());
        assert!(!row0[4].attrs.is_dim());

        let row1 = &back[1];
        assert_eq!(row1[0].ch, '2');
        assert_eq!(row1[2].ch, ' ');
        assert_eq!(row1[4].ch, 'b');
        assert_eq!(row1[5].ch, 'a');
        assert_eq!(row1[6].ch, 'r');
    }
    // --- New multi-caret and pointer behavior tests (moved from text_editor) ---
    #[test]
    fn add_caret_above_adds_new_caret() {
        let mut state = InputState::with_value_multiline("line1\nline2\nline3");
        // Move cursor to second line (line2)
        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.carets().len(), 1, "should start with single caret");
        let added = state.add_caret_above();
        assert!(added, "add_caret_above should report a new caret added");
        assert_eq!(state.carets().len(), 2, "should now have two carets");
    }

    #[test]
    fn add_caret_below_adds_new_caret() {
        let mut state = InputState::with_value_multiline("line1\nline2\nline3");
        // Move caret to start (initial position is end-of-buffer for with_value_multiline),
        // then down to the middle line so there is a valid line below to add a caret on.
        state.update(InputMsg::MoveToStart { extend: false });
        state.update(InputMsg::MoveDown { extend: false });
        assert_eq!(state.carets().len(), 1, "should start with single caret");
        let added = state.add_caret_below();
        assert!(added, "add_caret_below should report a new caret added");
        assert_eq!(state.carets().len(), 2, "should now have two carets");
    }

    #[test]
    fn alt_click_adds_caret() {
        let mut state = InputState::with_value_multiline("line1\nline2\nline3");
        assert_eq!(state.carets().len(), 1);
        // Alt single click on second line (row = 1)
        state.update(InputMsg::Pointer {
            column: 0,
            row: 1,
            click_count: 1,
            modifiers: PointerModifiers {
                ctrl: false,
                alt: true,
                shift: false,
                super_key: false,
            },
            scroll_y: 0,
        });
        assert_eq!(
            state.carets().len(),
            2,
            "alt click should add a new caret without removing existing ones"
        );
    }

    #[test]
    fn plain_click_resets_to_single_caret() {
        let mut state = InputState::with_value_multiline("line1\nline2\nline3");
        // First add an extra caret via alt click
        state.update(InputMsg::Pointer {
            column: 0,
            row: 1,
            click_count: 1,
            modifiers: PointerModifiers {
                ctrl: false,
                alt: true,
                shift: false,
                super_key: false,
            },
            scroll_y: 0,
        });
        assert_eq!(
            state.carets().len(),
            2,
            "alt click should create second caret"
        );
        // Plain click (no alt) on third line should collapse to single caret
        state.update(InputMsg::Pointer {
            column: 0,
            row: 2,
            click_count: 1,
            modifiers: PointerModifiers {
                ctrl: false,
                alt: false,
                shift: false,
                super_key: false,
            },
            scroll_y: 0,
        });
        assert_eq!(
            state.carets().len(),
            1,
            "plain click should collapse back to a single caret"
        );
    }
}
