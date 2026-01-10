use std::boxed::Box;
use std::future::Future;
use std::io::Write;
use std::pin::Pin;
use std::rc::Rc;
use std::time::{Duration, Instant};

use crate::buffer::DoubleBuffer;
use crate::dom::RetainedNodeContent;
use crate::dom::patch::{PatchResult, patch_borrowed};

// Type alias for the borrowed node returned by view functions
pub type NodeRef<'a, Msg> = crate::dom::patch::NodeRef<'a, Msg>;
// Type alias for the retained node stored in the program
pub type RetainedNode<Msg> = crate::dom::RetainedNode<Msg>;
use crate::error::ProgramError;
use crate::event::{
    Event, Key, KeyCode, KeyEventKind, LocalMouseEvent, MediaKeyCode, ModifierKeyCode, MouseButton,
    MouseEvent, MouseEventKind, MouseScrollAxis, MouseScrollDirection, Size,
};
use crate::palette::Palette;
use crate::render::Renderer;
use crate::scroll::ScrollAlignment;

use smol::future;
use smol::stream::StreamExt;
use taffy::compute_root_layout;
use termina::EventStream;
use termina::{PlatformTerminal, Terminal};
use tracing::{info, warn};

pub type UpdateFn<Model, Msg> = Box<dyn FnMut(&mut Model, Msg) -> Transition<Msg>>;
/// View function now returns NodeRef which can borrow from the model
pub type ViewFn<Model, Msg> = Box<dyn for<'a> Fn(&'a Model) -> NodeRef<'a, Msg>>;
pub type EventFn<Msg> = Box<dyn Fn(Event) -> Option<Msg>>;
pub type TaskFn<Msg> = Pin<Box<dyn Future<Output = Msg> + 'static>>;

pub enum Transition<Msg> {
    Continue,
    Quit,
    Task(TaskFn<Msg>),
    Multiple(Vec<Transition<Msg>>),
    Bell,
    SetTitle(Option<String>),
    Notify { title: Option<String>, body: String },
}

impl<Msg> Transition<Msg> {
    pub fn combine(self, other: Self) -> Self {
        match (self, other) {
            (Transition::Multiple(mut xs), Transition::Multiple(ys)) => {
                xs.extend(ys);
                Transition::Multiple(xs)
            }
            (Transition::Multiple(mut ts), other) => {
                ts.push(other);
                Transition::Multiple(ts)
            }
            (s, Transition::Multiple(mut ts)) => {
                ts.push(s);
                Transition::Multiple(ts)
            }
            (x, y) => Transition::Multiple(vec![x, y]),
        }
    }
}

struct ScrollEffect<Msg> {
    callback: Rc<dyn Fn(f32) -> Msg>,
    offset: f32,
}

pub struct Program<'a, Model, Msg> {
    model: &'a mut Model,
    update: UpdateFn<Model, Msg>,
    view: ViewFn<Model, Msg>,
    event_mapper: EventFn<Msg>,
    current_size: Size,
    current_view: Option<RetainedNode<Msg>>,
    last_click: Option<LastClick>,
    palette: Palette,
    queued_quit: bool,
    executor: smol::LocalExecutor<'static>,
    task_queue_send: smol::channel::Sender<Msg>,
    task_queue_recv: smol::channel::Receiver<Msg>,
    pending_terminal_actions: Vec<TerminalAction>,
}

impl<'a, Model, Msg: 'static> Program<'a, Model, Msg> {
    pub fn new(
        model: &'a mut Model,
        update: impl FnMut(&mut Model, Msg) -> Transition<Msg> + 'static,
        view: impl for<'b> Fn(&'b Model) -> NodeRef<'b, Msg> + 'static,
    ) -> Self {
        let (snd, recv) = smol::channel::unbounded();
        Self {
            model,
            update: Box::new(update),
            view: Box::new(view),
            event_mapper: Box::new(|_| None),
            current_size: Size::new(DEFAULT_WIDTH, DEFAULT_HEIGHT),
            current_view: None,
            last_click: None,
            palette: Palette::default(),
            queued_quit: false,
            executor: smol::LocalExecutor::new(),
            task_queue_recv: recv,
            task_queue_send: snd,
            pending_terminal_actions: Vec::new(),
        }
    }

    pub fn set_palette(&mut self, new: Palette) {
        self.palette = new;
    }

    pub fn map_event(mut self, event_mapper: impl Fn(Event) -> Option<Msg> + 'static) -> Self {
        self.event_mapper = Box::new(event_mapper);
        self
    }

    pub fn run(mut self) -> Result<(), ProgramError> {
        // Keep a sync entrypoint for consumers; run the async loop on the smol executor.
        smol::block_on(self.run_async())
    }

    /// Async runtime: drives terminal I/O and event handling using termina's EventStream.
    pub async fn run_async(&mut self) -> Result<(), ProgramError> {
        let mut terminal = PlatformTerminal::new()
            .map_err(|e| ProgramError::terminal(format!("Failed to create terminal: {}", e)))?;

        // Enter raw mode for the duration of the loop.
        terminal
            .enter_raw_mode()
            .map_err(|e| ProgramError::terminal(format!("Failed to enter raw mode: {}", e)))?;

        self.detect_palette();

        // Enter alternate screen and enable mouse tracking
        // Also enable kitty keyboard protocol with DISAMBIGUATE_ESCAPE_CODES flag
        write!(
            terminal,
            "\x1b[?1049h\x1b[?1000h\x1b[?1002h\x1b[?1003h\x1b[?1004h\x1b[?1006h\x1b[>1u",
        )
        .map_err(|e| ProgramError::terminal(format!("Failed to setup terminal: {}", e)))?;
        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush terminal: {}", e)))?;

        let result = self.event_loop_async(&mut terminal).await;

        // Always attempt to restore terminal state.
        // Pop kitty keyboard flags, show cursor, disable mouse tracking, exit alternate screen.
        let _ = write!(
            terminal,
            "\x1b[<1u\x1b[?25h\x1b[?2004l\x1b[?1006l\x1b[?1004l\x1b[?1003l\x1b[?1002l\x1b[?1000l\x1b[?1049l",
        );
        let _ = terminal.flush();
        let _ = terminal.enter_cooked_mode();

        result
    }

    #[cfg(unix)]
    fn detect_palette(&mut self) {
        match Palette::query_from_tty() {
            Ok(palette) => {
                info!("Detected terminal palette from host");
                self.palette = palette;
            }
            Err(err) => warn!("Falling back to default palette: {:?}", err),
        }
    }

    #[cfg(not(unix))]
    fn detect_palette(&mut self) {}

    async fn event_loop_async(
        &mut self,
        terminal: &mut PlatformTerminal,
    ) -> Result<(), ProgramError> {
        let dimensions = terminal
            .get_dimensions()
            .map_err(|e| ProgramError::terminal(format!("Failed to get dimensions: {}", e)))?;
        let mut buffer = DoubleBuffer::new(dimensions.cols as usize, dimensions.rows as usize);

        // Ensure initial size is propagated to the model/view before first render
        let _ = self.handle_event(Event::Resize(Size::new(dimensions.cols, dimensions.rows)));
        self.render_view(&mut buffer)?;
        buffer
            .flush(terminal)
            .map_err(|e| ProgramError::terminal(format!("Failed to flush buffer: {:?}", e)))?;

        // Hide cursor
        write!(terminal, "\x1b[?25l")
            .map_err(|e| ProgramError::terminal(format!("Failed to hide cursor: {}", e)))?;
        {
            use termina::escape::csi;
            write!(
                terminal,
                "{}",
                csi::Csi::Mode(csi::Mode::SetDecPrivateMode(csi::DecPrivateMode::Code(
                    csi::DecPrivateModeCode::BracketedPaste
                ))),
            )
            .map_err(|e| ProgramError::terminal(format!("Failed to setup terminal: {}", e)))?;
        }
        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush terminal: {}", e)))?;

        // Consume events from termina asynchronously using its reader.
        let mut events = EventStream::new(terminal.event_reader(), |_| true);

        enum Awaited<Msg> {
            Event(Option<std::io::Result<termina::Event>>),
            Message(Result<Msg, smol::channel::RecvError>),
            Tick,
        }

        loop {
            let (should_quit, needs_render) = self.pump_pending_tasks();

            if needs_render {
                self.flush_render(&mut buffer, terminal)?;
            }
            if !self.pending_terminal_actions.is_empty() {
                self.flush_terminal_actions(terminal)?;
            }
            if should_quit {
                break;
            }

            let next = future::race(
                async { Awaited::Event(events.next().await) },
                future::race(
                    async { Awaited::Message(self.task_queue_recv.recv().await) },
                    async {
                        self.executor.tick().await;
                        Awaited::Tick
                    },
                ),
            )
            .await;

            let mut loop_needs_render = false;
            let mut loop_should_quit = false;

            match next {
                Awaited::Event(Some(event_res)) => {
                    let event = event_res
                        .map_err(|e| ProgramError::event(format!("Failed to read event: {}", e)))?;

                    if let Some(converted_event) = convert_input_event(event) {
                        let (transition, render_flag) = self.handle_event(converted_event);
                        let (task_quit, task_render) = self.resolve_transition(transition);
                        loop_needs_render |= render_flag | task_render;
                        loop_should_quit |= task_quit;
                    }
                }
                Awaited::Event(None) => break,
                Awaited::Message(Ok(msg)) => {
                    let transition = (self.update)(self.model, msg);
                    let (task_quit, task_render) = self.resolve_transition(transition);
                    loop_needs_render |= task_render;
                    loop_should_quit |= task_quit;
                }
                Awaited::Message(Err(_)) => break,
                Awaited::Tick => continue,
            }

            if loop_needs_render {
                self.flush_render(&mut buffer, terminal)?;
            }
            if !self.pending_terminal_actions.is_empty() {
                self.flush_terminal_actions(terminal)?;
            }

            if loop_should_quit {
                break;
            }
        }

        Ok(())
    }

    #[cfg(test)]
    fn process_event(
        &mut self,
        event: Event,
        mut buffer: Option<&mut DoubleBuffer>,
    ) -> Result<bool, ProgramError> {
        let (transition, mut needs_render) = self.handle_event(event);
        let (mut quit, transition_render) = self.resolve_transition(transition);
        needs_render |= transition_render;

        if needs_render {
            if let Some(buf) = buffer.as_mut() {
                self.render_view(*buf)?;
            } else {
                self.rebuild_view();
            }
        }

        let (task_quit, task_render) = self.drain_task_queue();
        quit |= task_quit;
        if task_render {
            if let Some(buf) = buffer.as_mut() {
                self.render_view(*buf)?;
            } else {
                self.rebuild_view();
            }
        }

        Ok(quit)
    }

    #[cfg(test)]
    fn drain_task_queue(&mut self) -> (bool, bool) {
        let mut quit = false;
        let mut needs_render = false;

        for _ in 0..100 {
            let (task_quit, task_render) = self.pump_pending_tasks();
            quit |= task_quit;
            needs_render |= task_render;

            if quit || needs_render {
                continue;
            }

            if self.executor.try_tick() {
                continue;
            }

            break;
        }

        (quit, needs_render)
    }

    fn handle_event(&mut self, event: Event) -> (Transition<Msg>, bool) {
        let mut needs_render = matches!(event, Event::Resize(_));
        let mut needs_quit = false;

        if let Event::Resize(size) = &event {
            self.current_size = *size;
        }

        let msg = (self.event_mapper)(event.clone());

        if let Event::Mouse(mouse_event) = &event
            && msg.is_none()
            && let Some(mouse_messages) = self.handle_mouse_event(mouse_event)
        {
            for message in mouse_messages {
                let transition = (self.update)(self.model, message);
                let (t_needs_quit, t_needs_render) = self.resolve_transition(transition);
                needs_render |= t_needs_render;
                needs_quit |= t_needs_quit;
            }
        }

        if let Some(message) = msg {
            let transition = (self.update)(self.model, message);
            let (t_needs_quit, t_needs_render) = self.resolve_transition(transition);
            needs_render |= t_needs_render;
            needs_quit |= t_needs_quit;
        }

        if needs_quit {
            return (Transition::Quit, needs_render);
        }

        if self.queued_quit {
            self.queued_quit = false;
            return (Transition::Quit, needs_render);
        }

        (Transition::Continue, needs_render)
    }

    fn resolve_transition(&mut self, transition: Transition<Msg>) -> (bool, bool) {
        let mut needs_render = false;
        let mut needs_quit = false;
        tracing::info!("Transition");
        match transition {
            Transition::Task(task) => {
                tracing::info!("task spawning");
                let send = self.task_queue_send.clone();
                self.executor
                    .spawn(async move {
                        tracing::info!("task spawned");
                        let msg = task.await;
                        tracing::info!("task finished");

                        // ignore the err if the ch is closed.
                        //
                        // there is nothing to do with a closed ch.
                        let err = send.send(msg).await;
                        if err.is_err() {
                            tracing::info!("could not send task result");
                        }
                    })
                    .detach();
                needs_render = true;
            }
            Transition::Continue => {
                tracing::info!("cont");
                needs_render = true;
            }
            Transition::Quit => {
                tracing::info!("quit");
                needs_quit = true;
                needs_render = true;
            }
            Transition::Multiple(ts) => {
                tracing::info!("mult");
                for t in ts {
                    let (t_quit, t_render) = self.resolve_transition(t);
                    needs_quit |= t_quit;
                    needs_render |= t_render;
                }
            }
            Transition::Bell => {
                self.pending_terminal_actions.push(TerminalAction::Bell);
            }
            Transition::SetTitle(title) => {
                self.pending_terminal_actions
                    .push(TerminalAction::SetTitle(title));
            }
            Transition::Notify { title, body } => {
                self.pending_terminal_actions
                    .push(TerminalAction::Notify { title, body });
            }
        }

        (needs_quit, needs_render)
    }

    fn pump_pending_tasks(&mut self) -> (bool, bool) {
        let mut quit = false;
        let mut needs_render = false;

        while self.executor.try_tick() {}

        while let Ok(msg) = self.task_queue_recv.try_recv() {
            let transition = (self.update)(self.model, msg);
            let (task_quit, task_render) = self.resolve_transition(transition);
            quit |= task_quit;
            needs_render |= task_render;
        }

        if self.queued_quit {
            quit = true;
            self.queued_quit = false;
        }

        (quit, needs_render)
    }

    fn render_view(&mut self, buffer: &mut DoubleBuffer) -> Result<(), ProgramError> {
        self.rebuild_view();
        if let Some(current_view) = self.current_view.as_ref() {
            Renderer::new(buffer, &self.palette).render(current_view, self.current_size)
        } else {
            Ok(())
        }
    }

    fn flush_terminal_actions(
        &mut self,
        terminal: &mut PlatformTerminal,
    ) -> Result<(), ProgramError> {
        let actions = std::mem::take(&mut self.pending_terminal_actions);
        for action in actions {
            match action {
                TerminalAction::Bell => {
                    write!(terminal, "\x07").map_err(|e| {
                        ProgramError::terminal(format!("Failed to write bell: {}", e))
                    })?;
                }
                TerminalAction::SetTitle(title) => {
                    let title = title.unwrap_or_default();
                    write!(terminal, "\x1b]0;{title}\x07").map_err(|e| {
                        ProgramError::terminal(format!("Failed to write title: {}", e))
                    })?;
                }
                TerminalAction::Notify { title, body } => {
                    if let Some(title) = title {
                        write!(terminal, "\x1b]9;{title};{body}\x07").map_err(|e| {
                            ProgramError::terminal(format!("Failed to write notification: {}", e))
                        })?;
                    } else {
                        write!(terminal, "\x1b]9;{body}\x07").map_err(|e| {
                            ProgramError::terminal(format!("Failed to write notification: {}", e))
                        })?;
                    }
                }
            }
        }
        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush terminal: {}", e)))?;
        Ok(())
    }

    fn flush_render(
        &mut self,
        buffer: &mut DoubleBuffer,
        terminal: &mut PlatformTerminal,
    ) -> Result<(), ProgramError> {
        self.render_view(buffer)?;
        write!(terminal, "\x1b[?2026h")
            .map_err(|e| ProgramError::terminal(format!("Failed to write sync start: {}", e)))?;
        buffer.flush(terminal)?;
        write!(terminal, "\x1b[?2026l")
            .map_err(|e| ProgramError::terminal(format!("Failed to write sync end: {}", e)))?;
        terminal
            .flush()
            .map_err(|e| ProgramError::terminal(format!("Failed to flush terminal: {}", e)))?;
        Ok(())
    }

    fn rebuild_view(&mut self) {
        // View now returns NodeRef which can borrow from the model
        let new_view: NodeRef<'_, Msg> = (self.view)(self.model);

        if let Some(existing_view) = self.current_view.take() {
            // Patch the retained node with the borrowed node
            let patched = patch_borrowed(existing_view, new_view);
            self.current_view = Some(match patched {
                PatchResult::Patched { node, .. } => node,
                PatchResult::Replaced(replacement) => replacement,
            });
        } else {
            // First render: convert borrowed node to owned
            self.current_view = Some(new_view.into());
        }

        if let Some(view) = self.current_view.as_mut() {
            info!("computing layout with: {:?}", self.current_size);
            compute_root_layout(
                view,
                u64::MAX.into(),
                taffy::Size {
                    width: taffy::AvailableSpace::Definite(self.current_size.width as f32),
                    height: taffy::AvailableSpace::Definite(self.current_size.height as f32),
                },
            );
            crate::dom::rounding::round_layout(view);
            crate::dom::print::print_tree(view);

            // TODO early exit if Transition::Quit
            let mut pending_resize_msgs: Vec<Msg> = Vec::new();
            view.report_changed(&mut |node: &RetainedNode<Msg>| {
                if let Some(resize_callback) = &node.on_resize
                    && let Some(msg) = resize_callback(&node.layout_state.layout)
                {
                    pending_resize_msgs.push(msg);
                }
            });

            let mut quit = false;
            for msg in pending_resize_msgs {
                let transition = (self.update)(self.model, msg);
                let (task_quit, _) = self.resolve_transition(transition);
                if task_quit {
                    quit = true;
                    break;
                }
            }
            if quit {
                self.queued_quit = true;
            }
        }

        let mut scroll_effects: Vec<ScrollEffect<Msg>> = Vec::new();
        if let Some(view) = self.current_view.as_mut() {
            Self::collect_pending_scrolls(view, &mut scroll_effects);
        }

        for effect in scroll_effects {
            let message = (effect.callback)(effect.offset);
            let transition = (self.update)(self.model, message);
            let (task_quit, _) = self.resolve_transition(transition);
            if task_quit {
                self.queued_quit = true;
            }
        }
    }

    fn collect_pending_scrolls(node: &mut RetainedNode<Msg>, effects: &mut Vec<ScrollEffect<Msg>>) {
        if let Some(pending) = node.take_pending_scroll()
            && let Some((target_top, target_height)) =
                Self::target_geometry(node, pending.target_hash)
        {
            let layout = node.layout_state.layout;
            let viewport_height = layout.size.height.max(0.0);
            let content_height = layout.content_size.height.max(viewport_height);
            let new_scroll = Self::resolve_scroll_offset(
                node.scroll_y,
                viewport_height,
                content_height,
                target_top,
                target_height,
                pending.alignment,
            );

            if (new_scroll - node.scroll_y).abs() > f32::EPSILON {
                node.scroll_y = new_scroll;
                effects.push(ScrollEffect {
                    callback: pending.callback,
                    offset: new_scroll,
                });
            }
        }

        if let RetainedNodeContent::Element(element) = &mut node.content {
            for child in &mut element.children {
                Self::collect_pending_scrolls(child, effects);
            }
        }
    }

    fn target_geometry(node: &RetainedNode<Msg>, target_hash: u64) -> Option<(f32, f32)> {
        fn helper<Msg>(
            node: &crate::dom::RetainedNode<Msg>,
            target_hash: u64,
            offset: f32,
        ) -> Option<(f32, f32)> {
            if node.hashed_id() == target_hash {
                let layout = node.layout_state.layout;
                return Some((offset, layout.size.height.max(0.0)));
            }

            match &node.content {
                crate::dom::RetainedNodeContent::Element(element) => {
                    for child in &element.children {
                        let child_layout = child.layout_state.layout;
                        let child_offset = offset + child_layout.location.y;
                        if let Some(result) = helper(child, target_hash, child_offset) {
                            return Some(result);
                        }
                    }
                    None
                }
                crate::dom::RetainedNodeContent::Text(_) => None,
                crate::dom::RetainedNodeContent::Renderable(_) => None,
            }
        }

        helper(node, target_hash, 0.0)
    }

    fn resolve_scroll_offset(
        current_scroll: f32,
        viewport_height: f32,
        content_height: f32,
        target_top: f32,
        target_height: f32,
        alignment: ScrollAlignment,
    ) -> f32 {
        if viewport_height <= f32::EPSILON {
            return 0.0;
        }

        let max_scroll = (content_height - viewport_height).max(0.0);
        let target_bottom = target_top + target_height;
        let viewport_bottom = current_scroll + viewport_height;

        let desired = match alignment {
            ScrollAlignment::Start => target_top,
            ScrollAlignment::End => target_bottom - viewport_height,
            ScrollAlignment::Nearest => {
                if target_top < current_scroll {
                    target_top
                } else if target_bottom > viewport_bottom {
                    target_bottom - viewport_height
                } else {
                    current_scroll
                }
            }
        };

        desired.clamp(0.0, max_scroll)
    }

    fn handle_mouse_event(&mut self, event: &MouseEvent) -> Option<Vec<Msg>> {
        let mut enriched = *event;
        enriched.click_count = 0;

        if matches!(event.kind, MouseEventKind::Down(MouseButton::Left)) {
            let timestamp = Instant::now();
            let click_count = if let Some(last) = &self.last_click
                && timestamp.duration_since(last.timestamp) <= DOUBLE_CLICK_INTERVAL
                && last.x == event.position.x
                && last.y == event.position.y
            {
                if last.count >= 3 { 1 } else { last.count + 1 }
            } else {
                1
            };

            enriched.click_count = click_count;
            self.last_click = Some(LastClick {
                timestamp,
                x: event.position.x,
                y: event.position.y,
                count: click_count,
            });
        }

        if let Some(view) = self.current_view.as_ref()
            && let Some(msg) = view.hit_test(
                event.position.x,
                event.position.y,
                &mut |target, origin_x, origin_y| {
                    let origin_x_rounded = origin_x.round() as i32;
                    let origin_y_rounded = origin_y.round() as i32;

                    let rel_x = i32::from(enriched.position.x).saturating_sub(origin_x_rounded);
                    let rel_y = i32::from(enriched.position.y).saturating_sub(origin_y_rounded);
                    let local_event =
                        LocalMouseEvent::new(enriched, rel_x.max(0) as u16, rel_y.max(0) as u16);

                    target.mouse_message(local_event)
                },
            )
        {
            return Some(msg);
        }

        None
    }
}

const DEFAULT_WIDTH: u16 = 80;
const DEFAULT_HEIGHT: u16 = 24;
const DOUBLE_CLICK_INTERVAL: Duration = Duration::from_millis(400);

enum TerminalAction {
    Bell,
    SetTitle(Option<String>),
    Notify { title: Option<String>, body: String },
}

struct LastClick {
    timestamp: Instant,
    x: u16,
    y: u16,
    count: u8,
}

fn convert_input_event(input: termina::Event) -> Option<Event> {
    tracing::debug!("termina input event: {:?}", input);
    match input {
        termina::Event::Key(key) => map_key_event(key),
        termina::Event::Mouse(mouse) => map_mouse_event(mouse),
        termina::Event::WindowResized(size) => Some(Event::Resize(Size::new(size.cols, size.rows))),
        termina::Event::FocusIn => Some(Event::FocusGained),
        termina::Event::FocusOut => Some(Event::FocusLost),
        termina::Event::Paste(text) => Some(Event::Paste(text)),
        _ => None,
    }
}

fn map_mouse_event(mouse: termina::event::MouseEvent) -> Option<Event> {
    use termina::event::{MouseButton as TermMouseButton, MouseEventKind as TermMouseEventKind};

    let kind = match mouse.kind {
        TermMouseEventKind::Down(btn) => MouseEventKind::Down(match btn {
            TermMouseButton::Left => MouseButton::Left,
            TermMouseButton::Right => MouseButton::Right,
            TermMouseButton::Middle => MouseButton::Middle,
        }),
        TermMouseEventKind::Up(btn) => MouseEventKind::Up(match btn {
            TermMouseButton::Left => MouseButton::Left,
            TermMouseButton::Right => MouseButton::Right,
            TermMouseButton::Middle => MouseButton::Middle,
        }),
        TermMouseEventKind::Drag(btn) => MouseEventKind::Drag(match btn {
            TermMouseButton::Left => MouseButton::Left,
            TermMouseButton::Right => MouseButton::Right,
            TermMouseButton::Middle => MouseButton::Middle,
        }),
        TermMouseEventKind::Moved => MouseEventKind::Move,
        TermMouseEventKind::ScrollDown => MouseEventKind::Scroll(crate::event::MouseScroll {
            axis: MouseScrollAxis::Vertical,
            direction: MouseScrollDirection::Negative,
        }),
        TermMouseEventKind::ScrollUp => MouseEventKind::Scroll(crate::event::MouseScroll {
            axis: MouseScrollAxis::Vertical,
            direction: MouseScrollDirection::Positive,
        }),
        TermMouseEventKind::ScrollLeft => MouseEventKind::Scroll(crate::event::MouseScroll {
            axis: MouseScrollAxis::Horizontal,
            direction: MouseScrollDirection::Negative,
        }),
        TermMouseEventKind::ScrollRight => MouseEventKind::Scroll(crate::event::MouseScroll {
            axis: MouseScrollAxis::Horizontal,
            direction: MouseScrollDirection::Positive,
        }),
    };

    Some(Event::Mouse(MouseEvent::with_modifiers(
        mouse.column,
        mouse.row,
        kind,
        mouse.modifiers.contains(termina::event::Modifiers::CONTROL),
        mouse.modifiers.contains(termina::event::Modifiers::ALT),
        mouse.modifiers.contains(termina::event::Modifiers::SHIFT),
        mouse.modifiers.contains(termina::event::Modifiers::SUPER),
    )))
}

fn map_key_event(key: termina::event::KeyEvent) -> Option<Event> {
    let code = map_key_code(key.code)?;
    let kind = map_key_event_kind(key.kind);
    let keypad = key.state.contains(termina::event::KeyEventState::KEYPAD);
    let shift = key.modifiers.contains(termina::event::Modifiers::SHIFT)
        || matches!(key.code, termina::event::KeyCode::BackTab);
    let event_key = Key::with_kitty_extras(
        code,
        key.modifiers.contains(termina::event::Modifiers::CONTROL),
        key.modifiers.contains(termina::event::Modifiers::ALT),
        shift,
        key.modifiers.contains(termina::event::Modifiers::SUPER),
        key.modifiers.contains(termina::event::Modifiers::HYPER),
        key.modifiers.contains(termina::event::Modifiers::META),
        kind,
        keypad,
    );
    Some(Event::Key(event_key))
}

fn map_key_event_kind(kind: termina::event::KeyEventKind) -> KeyEventKind {
    match kind {
        termina::event::KeyEventKind::Press => KeyEventKind::Press,
        termina::event::KeyEventKind::Release => KeyEventKind::Release,
        termina::event::KeyEventKind::Repeat => KeyEventKind::Repeat,
    }
}

fn map_key_code(code: termina::event::KeyCode) -> Option<KeyCode> {
    use termina::event::KeyCode as TnKeyCode;

    match code {
        TnKeyCode::Char(c) => Some(KeyCode::Char(c)),
        TnKeyCode::Enter => Some(KeyCode::Enter),
        TnKeyCode::Escape => Some(KeyCode::Esc),
        TnKeyCode::Backspace => Some(KeyCode::Backspace),
        TnKeyCode::Left => Some(KeyCode::Left),
        TnKeyCode::Right => Some(KeyCode::Right),
        TnKeyCode::Up => Some(KeyCode::Up),
        TnKeyCode::Down => Some(KeyCode::Down),
        TnKeyCode::PageUp => Some(KeyCode::PageUp),
        TnKeyCode::PageDown => Some(KeyCode::PageDown),
        TnKeyCode::Home => Some(KeyCode::Home),
        TnKeyCode::End => Some(KeyCode::End),
        TnKeyCode::Tab | TnKeyCode::BackTab => Some(KeyCode::Tab),
        TnKeyCode::Function(n) => Some(KeyCode::Function(n)),
        TnKeyCode::Insert => Some(KeyCode::Insert),
        TnKeyCode::Delete => Some(KeyCode::Delete),
        TnKeyCode::CapsLock => Some(KeyCode::CapsLock),
        TnKeyCode::ScrollLock => Some(KeyCode::ScrollLock),
        TnKeyCode::NumLock => Some(KeyCode::NumLock),
        TnKeyCode::PrintScreen => Some(KeyCode::PrintScreen),
        TnKeyCode::Pause => Some(KeyCode::Pause),
        TnKeyCode::Menu => Some(KeyCode::Menu),
        TnKeyCode::Modifier(m) => Some(KeyCode::Modifier(map_modifier_key_code(m))),
        TnKeyCode::Media(m) => Some(KeyCode::Media(map_media_key_code(m))),
        _ => None,
    }
}

fn map_modifier_key_code(code: termina::event::ModifierKeyCode) -> ModifierKeyCode {
    use termina::event::ModifierKeyCode as TnMod;
    match code {
        TnMod::LeftShift => ModifierKeyCode::LeftShift,
        TnMod::LeftControl => ModifierKeyCode::LeftControl,
        TnMod::LeftAlt => ModifierKeyCode::LeftAlt,
        TnMod::LeftSuper => ModifierKeyCode::LeftSuper,
        TnMod::LeftHyper => ModifierKeyCode::LeftHyper,
        TnMod::LeftMeta => ModifierKeyCode::LeftMeta,
        TnMod::RightShift => ModifierKeyCode::RightShift,
        TnMod::RightControl => ModifierKeyCode::RightControl,
        TnMod::RightAlt => ModifierKeyCode::RightAlt,
        TnMod::RightSuper => ModifierKeyCode::RightSuper,
        TnMod::RightHyper => ModifierKeyCode::RightHyper,
        TnMod::RightMeta => ModifierKeyCode::RightMeta,
        // Map IsoLevel keys to their closest equivalents
        TnMod::IsoLevel3Shift => ModifierKeyCode::RightAlt,
        TnMod::IsoLevel5Shift => ModifierKeyCode::RightAlt,
    }
}

fn map_media_key_code(code: termina::event::MediaKeyCode) -> MediaKeyCode {
    use termina::event::MediaKeyCode as TnMedia;
    match code {
        TnMedia::Play => MediaKeyCode::Play,
        TnMedia::Pause => MediaKeyCode::Pause,
        TnMedia::PlayPause => MediaKeyCode::PlayPause,
        TnMedia::Stop => MediaKeyCode::Stop,
        TnMedia::FastForward => MediaKeyCode::FastForward,
        TnMedia::Rewind => MediaKeyCode::Rewind,
        TnMedia::TrackNext => MediaKeyCode::TrackNext,
        TnMedia::TrackPrevious => MediaKeyCode::TrackPrevious,
        TnMedia::Record => MediaKeyCode::Record,
        TnMedia::LowerVolume => MediaKeyCode::LowerVolume,
        TnMedia::RaiseVolume => MediaKeyCode::RaiseVolume,
        TnMedia::MuteVolume => MediaKeyCode::MuteVolume,
        TnMedia::Reverse => MediaKeyCode::Rewind, // Map to closest equivalent
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dom::{Node, column, text};

    #[derive(Default)]
    struct CounterModel {
        count: i32,
    }

    enum Msg {
        Increment,
        Quit,
    }

    fn update(model: &mut CounterModel, msg: Msg) -> Transition<Msg> {
        match msg {
            Msg::Increment => {
                model.count += 1;
                Transition::Continue
            }
            Msg::Quit => Transition::Quit,
        }
    }

    fn view(model: &CounterModel) -> Node<'_, Msg> {
        column(vec![text(format!("count: {}", model.count))])
    }

    #[test]
    fn send_updates_model_and_rerenders() {
        let mut model = CounterModel::default();
        let mut program = Program::new(&mut model, update, view).map_event(|event| match event {
            Event::Key(key) if matches!(key.code, KeyCode::Char('+')) => Some(Msg::Increment),
            Event::Key(key) if matches!(key.code, KeyCode::Char('q')) => Some(Msg::Quit),
            _ => None,
        });
        let mut buffer = DoubleBuffer::new(10, 2);

        let quit = program
            .process_event(Event::key(KeyCode::Char('+')), Some(&mut buffer))
            .expect("send should succeed");
        assert!(!quit);

        let quit = program
            .process_event(Event::key(KeyCode::Char('q')), Some(&mut buffer))
            .expect("send should succeed");
        assert!(quit);

        let screen = buffer.to_string();
        assert!(screen.contains("count: 1"));
    }

    #[derive(Default)]
    struct ClickModel {
        clicks: usize,
    }

    enum ClickMsg {
        Click,
    }

    fn click_update(model: &mut ClickModel, msg: ClickMsg) -> Transition<ClickMsg> {
        match msg {
            ClickMsg::Click => {
                model.clicks += 1;
                Transition::Continue
            }
        }
    }

    fn click_view(_model: &ClickModel) -> Node<'_, ClickMsg> {
        text("button").on_click(|| ClickMsg::Click)
    }

    #[test]
    fn mouse_click_triggers_on_click_handler() {
        let mut model = ClickModel::default();
        let mut program = Program::new(&mut model, click_update, click_view);
        let mut buffer = DoubleBuffer::new(4, 2);

        program
            .process_event(Event::resize(4, 2), Some(&mut buffer))
            .expect("resize should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Down(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("mouse down should succeed");
        assert_eq!(program.model.clicks, 1);

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Up(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("mouse move up should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Down(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("second click should succeed");
        assert_eq!(program.model.clicks, 1);

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Up(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("mouse up should succeed");

        program
            .process_event(
                Event::mouse(1, 0, MouseEventKind::Down(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("third click should succeed");
        assert_eq!(program.model.clicks, 2);
    }

    #[derive(Default)]
    struct DoubleClickModel {
        single: usize,
        double: usize,
    }

    enum DoubleClickMsg {
        Single,
        Double,
    }

    fn double_click_update(
        model: &mut DoubleClickModel,
        msg: DoubleClickMsg,
    ) -> Transition<DoubleClickMsg> {
        match msg {
            DoubleClickMsg::Single => {
                model.single += 1;
                Transition::Continue
            }
            DoubleClickMsg::Double => {
                model.double += 1;
                Transition::Continue
            }
        }
    }

    fn double_click_view(_model: &DoubleClickModel) -> Node<'_, DoubleClickMsg> {
        text("button").on_mouse(|event| {
            if event.is_double_click() {
                Some(DoubleClickMsg::Double)
            } else if event.is_single_click() {
                Some(DoubleClickMsg::Single)
            } else {
                None
            }
        })
    }

    #[test]
    fn double_click_triggers_handler() {
        let mut model = DoubleClickModel::default();
        let mut program = Program::new(&mut model, double_click_update, double_click_view);
        let mut buffer = DoubleBuffer::new(4, 2);

        program
            .process_event(Event::resize(4, 2), Some(&mut buffer))
            .expect("resize should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Down(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("first click should succeed");
        assert_eq!(program.model.single, 1);
        assert_eq!(program.model.double, 0);

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Up(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("mouse up should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Down(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("second click should succeed");
        assert_eq!(program.model.single, 1);
        assert_eq!(program.model.double, 1);
    }

    #[test]
    fn double_click_requires_same_position() {
        let mut model = DoubleClickModel::default();
        let mut program = Program::new(&mut model, double_click_update, double_click_view);
        let mut buffer = DoubleBuffer::new(4, 2);

        program
            .process_event(Event::resize(4, 2), Some(&mut buffer))
            .expect("resize should succeed");

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Down(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("first click should succeed");
        assert_eq!(program.model.single, 1);
        assert_eq!(program.model.double, 0);

        program
            .process_event(
                Event::mouse(0, 0, MouseEventKind::Up(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("mouse up should succeed");

        program
            .process_event(
                Event::mouse(1, 0, MouseEventKind::Down(MouseButton::Left)),
                Some(&mut buffer),
            )
            .expect("second click should succeed");
        assert_eq!(program.model.single, 2);
        assert_eq!(program.model.double, 0);
    }

    #[test]
    fn focus_in_event_is_exposed() {
        let event = super::convert_input_event(termina::Event::FocusIn);
        assert_eq!(event, Some(Event::FocusGained));
    }

    #[test]
    fn paste_event_is_exposed() {
        let event = super::convert_input_event(termina::Event::Paste("paste".to_string()));
        assert_eq!(event, Some(Event::Paste("paste".to_string())));
    }

    #[test]
    fn page_navigation_keys_are_mapped() {
        assert_eq!(
            super::map_key_code(termina::event::KeyCode::PageUp),
            Some(KeyCode::PageUp)
        );
        assert_eq!(
            super::map_key_code(termina::event::KeyCode::PageDown),
            Some(KeyCode::PageDown)
        );
    }

    #[derive(Default)]
    struct TaskModel {
        started: bool,
        completed: bool,
    }

    enum TaskMsg {
        Start,
        Complete,
    }

    fn task_update(model: &mut TaskModel, msg: TaskMsg) -> Transition<TaskMsg> {
        match msg {
            TaskMsg::Start => {
                model.started = true;
                Transition::Task(Box::pin(async { TaskMsg::Complete }))
            }
            TaskMsg::Complete => {
                model.completed = true;
                Transition::Continue
            }
        }
    }

    fn task_view(_model: &TaskModel) -> Node<'_, TaskMsg> {
        text("task view")
    }

    #[test]
    fn task_transition_schedules_follow_up_message() {
        let mut model = TaskModel::default();
        let mut program =
            Program::new(&mut model, task_update, task_view).map_event(|event| match event {
                Event::Key(key) if matches!(key.code, KeyCode::Enter) => Some(TaskMsg::Start),
                _ => None,
            });

        let mut buffer = DoubleBuffer::new(4, 2);
        program
            .process_event(Event::resize(4, 2), Some(&mut buffer))
            .expect("resize should succeed");

        assert!(!program.model.started);
        assert!(!program.model.completed);

        let quit = program
            .process_event(Event::key(KeyCode::Enter), Some(&mut buffer))
            .expect("start should succeed");
        assert!(!quit);
        assert!(program.model.started);

        for _ in 0..100 {
            if program.model.completed {
                break;
            }
            program.drain_task_queue();
            if program.model.completed {
                break;
            }
            std::thread::sleep(Duration::from_millis(5));
        }
        assert!(program.model.completed);
    }

    #[test]
    fn backtab_is_mapped_to_shift_tab() {
        let key = termina::event::KeyEvent {
            code: termina::event::KeyCode::BackTab,
            modifiers: termina::event::Modifiers::empty(),
            kind: termina::event::KeyEventKind::Press,
            state: termina::event::KeyEventState::empty(),
        };
        let event = super::convert_input_event(termina::Event::Key(key));

        match event {
            Some(Event::Key(key)) => {
                assert_eq!(key.code, KeyCode::Tab);
                assert!(key.shift);
            }
            _ => panic!("Expected Key event, got {:?}", event),
        }
    }
}
