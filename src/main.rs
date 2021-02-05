#![cfg_attr(feature = "product",
    windows_subsystem = "windows")]  // prevent console

#![feature(bindings_after_at)]
#![feature(backtrace)]
#![feature(destructuring_assignment)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::too_many_arguments)]

#[macro_use] pub mod slotmap;
mod win_util;
pub mod win_win_reent;
mod text_layout;
pub mod gfx;
mod util;
pub mod types;
pub mod edit;
mod commands;
mod storage;

use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use fnv::{FnvHashSet, FnvHashMap};
use wio::com::ComPtr;
use winapi::shared::windef::*;
use winapi::shared::minwindef::*;
use winapi::shared::winerror::*;
use winapi::shared::windowsx::*;
use winapi::um::winuser::*;
use winapi::um::d2d1::*;
use winapi::um::dwrite::*;
use winapi::um::dcommon::*;
use win_msg_name::win_msg_name;
use crate::win_win_reent::*;
use crate::win_util::*;
use crate::util::*;
use crate::types::*;
use crate::edit::*;
use crate::commands::{CmdClass, CmdResult};
use crate::text_layout::Skew;

struct AppCtx {
    arrow_cursor: HCURSOR,
    hand_cursor: HCURSOR,
    beam_cursor: HCURSOR,

    render_target: ComPtr<ID2D1HwndRenderTarget>,
    dwrite_factory: ComPtr<IDWriteFactory>,
    header_text_format: ComPtr<IDWriteTextFormat>,
    normal_text_format: ComPtr<IDWriteTextFormat>,
    code_text_format: ComPtr<IDWriteTextFormat>,
    text_brush: ComPtr<ID2D1Brush>,
    monospace_text_brush: ComPtr<ID2D1Brush>,
    cursor_brush: ComPtr<ID2D1Brush>,
    sel_brush: ComPtr<ID2D1Brush>,
    header_rainbow_brushes: Vec<ComPtr<ID2D1Brush>>,
    bullet_brush: ComPtr<ID2D1Brush>,
    one_liner_bullet_fill_brush: ComPtr<ID2D1Brush>,
}

impl AppCtx {
    fn new(hwnd: HWND) -> Self {
        let arrow_cursor = load_cursor(IDC_ARROW);
        let hand_cursor = load_cursor(IDC_HAND);
        let beam_cursor = load_cursor(IDC_IBEAM);

        let d2d_factory = create_d2d_factory();
        let render_target = create_hwnd_render_target(&d2d_factory, hwnd);
        let dwrite_factory = create_dwrite_factory();
        let header_text_format = create_text_format(&dwrite_factory, "Arial", 18.0, true);
        let normal_text_format = create_text_format(&dwrite_factory, "Arial", 18.0, false);
        let code_text_format = create_text_format(&dwrite_factory, "Consolas", 18.0, false);
        let text_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 0.8, g: 0.8, b: 0.8, a: 1.0 });
        let monospace_text_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 0.6, g: 1.0, b: 0.6, a: 1.0 });
        let cursor_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 1.0, g: 1.0, b: 1.0, a: 1.0 });
        let sel_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 0.3, g: 0.3, b: 0.4, a: 1.0 });

        let bullet_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 1.0, g: 1.0, b: 1.0, a: 1.0 });
        let one_liner_bullet_fill_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 1.0, g: 1.0, b: 1.0, a: 0.4 });

        let header_rainbow_brushes = vec![
            create_solid_brush(
                &render_target, &D2D1_COLOR_F { r: 0.9, g: 1.0, b: 0.8, a: 1.0 }),
            create_solid_brush(
                &render_target, &D2D1_COLOR_F { r: 0.9, g: 0.9, b: 1.0, a: 1.0 }),
            create_solid_brush(
                &render_target, &D2D1_COLOR_F { r: 1.0, g: 0.9, b: 0.8, a: 1.0 }),
            create_solid_brush(
                &render_target, &D2D1_COLOR_F { r: 0.8, g: 1.0, b: 1.0, a: 1.0 }),
            create_solid_brush(
                &render_target, &D2D1_COLOR_F { r: 1.0, g: 1.0, b: 0.8, a: 1.0 }),
            create_solid_brush(
                &render_target, &D2D1_COLOR_F { r: 1.0, g: 0.9, b: 1.0, a: 1.0 }),
        ];

        AppCtx {
            arrow_cursor,
            hand_cursor,
            beam_cursor,

            render_target,
            dwrite_factory,
            header_text_format,
            normal_text_format,
            code_text_format,
            text_brush,
            monospace_text_brush,
            cursor_brush,
            sel_brush,
            header_rainbow_brushes,

            bullet_brush,
            one_liner_bullet_fill_brush,
        }        
    }
}

pub struct Cur {
    pub block: BlockKey,
    pub line: usize,
    pub pos_skew: (usize, Skew),
    pub anchor_x: f32,
    pub sel: Option<Sel>,
}

impl Cur {
    fn pos(&self) -> usize {
        self.pos_skew.0
    }
}

pub struct Sel {
    line: usize,
    pos: usize,

    anchor_path: Vec<(usize, usize)>,
}

struct Clipboard {
    sequence_number: u32,
    lines: Vec<Line>,
}

struct UndoGroup {
    cur_before: Waypoint,
    edits: Vec<Edit>,
    cur_after: Waypoint,
}

struct UndoGroupBuilder {
    cur_before: Waypoint,
    edits: Vec<Edit>,
}

impl Drop for UndoGroupBuilder {
    fn drop(&mut self) {
        assert!(self.edits.is_empty(), "forgot .finish()?");
    }
}

impl UndoGroupBuilder {
    fn new(cur_before: Waypoint) -> Self {
        UndoGroupBuilder {
            cur_before,
            edits: vec![],
        }
    }

    #[must_use]
    fn finish(mut self, cur_after: Waypoint) -> UndoGroup {
        let cur_before = std::mem::replace(
            &mut self.cur_before,
            Waypoint { path: vec![], pos_skew: Default::default() });
        UndoGroup {
            cur_before,
            edits: std::mem::replace(&mut self.edits, vec![]),
            cur_after,
        }
    }
}

pub struct Unsaved {
    cur: bool,  // cursor and scroll position.
    tree: bool,
    nodes: FnvHashSet<NodeKey>,
    // Unsaved nodes imply unsaved tree.
    // Unsaved tree implies unsaved cur.
}

impl Unsaved {
    fn new() -> Self {
        Unsaved {
            cur: false,
            tree: false,
            nodes: Default::default(),
        }
    }

    fn has_changes(&self) -> bool {
        self.cur || self.tree || !self.nodes.is_empty()
    }
}

impl std::fmt::Display for Unsaved {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.nodes.is_empty() {
            write!(f, "* ({})", self.nodes.len())
        } else if self.tree {
            write!(f, "+")
        } else if self.cur {
            write!(f, "_")
        } else {
            Ok(())
        }
    }
}

struct App {
    ctx: AppCtx,

    nodes: Nodes,
    blocks: Blocks,
    cblocks: CBlocks,

    conn: rusqlite::Connection,
    data_version: i64,
    db_key_to_node_key: FnvHashMap<i64, NodeKey>,

    unsaved: Unsaved,
    last_command_time: std::time::Instant,

    undo_buf: Vec<UndoGroup>,
    redo_buf: Vec<UndoGroup>,
    last_cmd_class: CmdClass,

    y_offset: f32,
    root_block: BlockKey,  // owned key
    cur: Cur,

    clipboard: Option<Clipboard>,
}

impl CmdResult {
    fn process(self, hwnd: HWND, app: &mut App) {
        if self.repaint {
            invalidate_rect(hwnd);

            // For simplicity we don't track changes to cursor
            // everywhere they are made.
            app.unsaved.cur = true;
        }
        if self.update_anchor_x {
            app.update_anchor();
        }

        if self.scroll_to_reveal_cursor {
            let rc = get_client_rect(hwnd);
            let height = (rc.bottom - rc.top) as f32;

            let cc = app.blocks[app.cur.block].abs_cursor_coord(
                app.cur.line, app.cur.pos_skew,
                &app.ctx, &app.blocks, &app.nodes);
            if app.y_offset + cc.top < 0.0 {
                app.y_offset = -cc.top;
            } else if app.y_offset + cc.top + cc.height > height {
                app.y_offset = height - cc.top - cc.height;
            }
        }

        app.last_command_time = std::time::Instant::now();
        // TODO: if unsaved.* is false, check that it matches DB content

        set_window_title(hwnd, &format!("e2{}", app.unsaved));

        dbg!(self.class);
        app.last_cmd_class = self.class;
        std::mem::forget(self);
    }
}

#[derive(Debug, PartialEq)]
struct Waypoint {
    path: Vec<usize>,  // line indices
    pos_skew: (usize, Skew),
}

fn expand_block(
    block: BlockKey,
    blocks: &mut Blocks, cblocks: &mut CBlocks, nodes: &mut Nodes,
    unsaved: &mut Unsaved,
) {
    let b = &mut blocks[block];
    assert!(!b.is_expanded());
    let cforest = b.collapsed.take().unwrap();
    push_block_children_from_cforest(block, cforest, blocks, cblocks, nodes);
    unsaved.tree = true;
}

fn destroy_block(
    block: BlockKey,
    blocks: &mut Blocks, cblocks: &mut CBlocks, nodes: &mut Nodes,
) {
    let was_there = nodes[blocks[block].node].blocks.remove(&block);
    assert!(was_there);

    let block = blocks.remove(block);
    for child in block.children {
        match child {
            BlockChild::Leaf => {}
            BlockChild::Block(b) => destroy_block(b, blocks, cblocks, nodes),
        }
    }
    if let Some(collapsed) = block.collapsed {
        for &cb in collapsed.0.values() {
            destroy_cblock(cb, cblocks, nodes);
        }
    }
}

fn destroy_cblock(cblock: CBlockKey, cblocks: &mut CBlocks, nodes: &mut Nodes) {
    let was_there = nodes[cblocks[cblock].node].cblocks.remove(&cblock);
    assert!(was_there);

    let cblock = cblocks.remove(cblock);
    for &child in cblock.children.0.values() {
        destroy_cblock(child, cblocks, nodes);
    }
}

// return None if resulting cblock .is_trivial()
fn block_to_cblock(
    block: BlockKey,
    blocks: &mut Blocks, cblocks: &mut CBlocks, nodes: &mut Nodes,
) -> Option<CBlockKey> {
    let node = blocks[block].node;
    let was_there = nodes[node].blocks.remove(&block);
    assert!(was_there);
    let block = blocks.remove(block);
    let cblock = match block.collapsed {
        Some(children) => {
            if children.0.is_empty() {
                return None;
            }
            CBlock {
                node,
                expanded: false,
                children,
            }
        },
        None => {
            let mut children = CForest::new();
            for (i, child) in block.children.into_iter().enumerate().skip(1) {
                match child {
                    BlockChild::Leaf => {},
                    BlockChild::Block(b) => {
                        let cb = block_to_cblock(b, blocks, cblocks, nodes);
                        if let Some(cb) = cb {
                            children.0.insert(i, cb);
                        }
                    }
                }
            }
            CBlock {
                node,
                expanded: true,
                children,
            }
        }
    };
    let cblock = cblocks.insert(cblock);
    let was_new = nodes[node].cblocks.insert(cblock);
    assert!(was_new);
    Some(cblock)
}

fn cblock_to_block(
    parent_idx: (BlockKey, usize),
    cblock: CBlockKey,
    blocks: &mut Blocks, cblocks: &mut CBlocks, nodes: &mut Nodes,
) -> BlockKey {
    let node = cblocks[cblock].node;
    let was_there = nodes[node].cblocks.remove(&cblock);
    assert!(was_there);
    let cblock = cblocks.remove(cblock);

    let block = Block {
        depth: blocks[parent_idx.0].depth + 1,
        parent_idx: Some(parent_idx),
        node,
        children: vec![BlockChild::Leaf],
        collapsed: None,
    };

    let block = blocks.insert(block);
    let was_new = nodes[node].blocks.insert(block);
    assert!(was_new);

    if cblock.expanded {
        push_block_children_from_cforest(
            block, cblock.children,
            blocks, cblocks, nodes);
    } else {
        blocks[block].collapsed = Some(cblock.children);
    }

    block
}

fn push_block_children_from_cforest(
    block: BlockKey,
    mut cforest: CForest,
    blocks: &mut Blocks, cblocks: &mut CBlocks, nodes: &mut Nodes,
) {
    let node = blocks[block].node;
    assert_eq!(blocks[block].children.len(), 1);
    for i in 0..nodes[node].lines.len() {
        let child = match nodes[node].lines[i].line {
            Line::Text {..} => BlockChild::Leaf,
            Line::Node { node: child_node, .. } => {
                let child_block = if let Some(cb) = cforest.0.remove(&(i + 1)) {
                    cblock_to_block((block, i + 1), cb, blocks, cblocks, nodes)
                } else {
                    let child_block = blocks.insert(Block {
                        depth: blocks[block].depth + 1,
                        parent_idx: Some((block, i + 1)),
                        node: child_node,
                        children: vec![BlockChild::Leaf],
                        collapsed: Some(CForest::new()),
                    });
                    let was_new = nodes[child_node].blocks.insert(child_block);
                    assert!(was_new);
                    child_block
                };
                BlockChild::Block(child_block)
            }
        };
        blocks[block].children.push(child);
    }
    assert!(cforest.0.is_empty());
}

impl App {
    fn new(hwnd: HWND) -> Self {
        let ctx = AppCtx::new(hwnd);

        let exe = std::env::current_exe().unwrap();
        let exe_dir = exe.parent().unwrap();
        let db_path = exe_dir.join("notes.db");

        let mut conn = rusqlite::Connection::open(db_path).unwrap();
        let tx = conn.transaction().unwrap();
        let storage::AppInit {
            nodes, blocks, cblocks,
            db_key_to_node_key,
            root_block,
            cur,
            y_offset,
        } = storage::load_or_create(&tx);
        tx.commit().unwrap();

        let data_version: i64 = conn
            .pragma_query_value(None, "data_version", |row| row.get(0))
            .unwrap();
        dbg!(data_version);

        let mut app = App {
            ctx,
            nodes,
            blocks,
            cblocks,
            conn,
            data_version,
            db_key_to_node_key,
            unsaved: Unsaved::new(),
            last_command_time: std::time::Instant::now(),
            undo_buf: vec![],
            redo_buf: vec![],
            last_cmd_class: CmdClass::Other,
            cur,
            root_block,
            y_offset,
            clipboard: None,
        };
        app.sink_cursor();
        app.update_anchor();
        app.check();
        app
    }

    fn save_changes(&mut self, hwnd: HWND) {
        assert!(self.unsaved.has_changes());
        println!("Saving...");

        let data_version: i64 = self.conn
            .pragma_query_value(None, "data_version", |row| row.get(0))
            .unwrap();
        assert_eq!(data_version, self.data_version,
            "db was modified by another process");
        // TODO: better error reporting

        let nodes = &self.nodes;
        let blocks = &self.blocks;
        let cblocks = &self.cblocks;

        let tx = self.conn.transaction().unwrap();
        for &node in &self.unsaved.nodes {
            let db_key = nodes[node].db_key;
            println!("  saving node {:?} (db key {})", node, db_key);
            assert_eq!(self.db_key_to_node_key[&db_key], node);
            let dn = storage::DiskNode::from(&nodes[node], nodes);
            storage::save_node(db_key, &dn, &tx);
        }
        if self.unsaved.tree || !self.unsaved.nodes.is_empty() {
            println!("  saving tree");
            let dt = storage::DiskBlock::from_block(self.root_block, blocks, cblocks);
            storage::save_tree(&dt, &tx);
        }
        if self.unsaved.cur || self.unsaved.tree || !self.unsaved.nodes.is_empty() {
            println!("  saving cur");
            let dc = storage::DiskCur::from(&self.cur, blocks, self.y_offset);
            storage::save_cur(&dc, &tx);
        }
        tx.commit().unwrap();

        self.unsaved = Unsaved::new();
        set_window_title(hwnd, &format!("e2{}", self.unsaved));
    }

    fn cur_waypoint(&self) -> Waypoint {
        let mut path = vec![self.cur.line];
        let mut b = self.cur.block;
        while b != self.root_block {
            let (parent, idx) = self.blocks[b].parent_idx.unwrap();
            path.push(idx);
            b = parent;
        }
        path.reverse();
        Waypoint {
            path,
            pos_skew: self.cur.pos_skew,
        }
    }

    fn set_cur_to_waypoint(&mut self, wp: &Waypoint) {
        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        let mut b = self.root_block;
        assert!(!wp.path.is_empty());
        for i in 0..wp.path.len() {
            if wp.path[i] > 0 && !blocks[b].is_expanded() {
                expand_block(b, blocks, cblocks, nodes, &mut self.unsaved);
            }
            if i < wp.path.len() - 1 {
                b = match blocks[b].children[wp.path[i]] {
                    BlockChild::Leaf => panic!(),
                    BlockChild::Block(b) => b,
                };
            }
        }
        self.cur.block = b;
        self.cur.line = *wp.path.last().unwrap();
        self.cur.pos_skew = wp.pos_skew;
        self.cur.sel = None;

        // It is possible that we recorded cursor position
        // with selection, and we didn't restore selection.
        self.sink_cursor();
    }

    fn check(&self) {
        fn rec_check_block(
            block: BlockKey, cnt: &mut Cnt,
            blocks: &Blocks, cblocks: &CBlocks, nodes: &Nodes,
        ) {
            cnt.block += 1;
            check_block(block, blocks, cblocks, nodes);
            for child in &blocks[block].children {
                match *child {
                    BlockChild::Leaf => {}
                    BlockChild::Block(b) => rec_check_block(
                        b, cnt, blocks, cblocks, nodes),
                }
            }
            if let Some(collapsed) = blocks[block].collapsed.as_ref() {
                for &cb in collapsed.0.values() {
                    rec_check_cblock(cb, cnt, cblocks, nodes);
                }
            }
        }
        fn rec_check_cblock(
            cblock: CBlockKey, cnt: &mut Cnt,
            cblocks: &CBlocks, nodes: &Nodes) {
            cnt.cblock += 1;
            check_cblock(cblock, cblocks, nodes);
            for &cb in cblocks[cblock].children.0.values() {
                rec_check_cblock(cb, cnt, cblocks, nodes);
            }
        }
        struct Cnt {
            block: usize,
            cblock: usize,
        }
        let mut cnt = Cnt {
            block: 0,
            cblock: 0,
        };
        rec_check_block(self.root_block, &mut cnt,
            &self.blocks, &self.cblocks, &self.nodes);
        assert_eq!(self.blocks.len(), cnt.block);
        assert_eq!(self.cblocks.len(), cnt.cblock);
        if let Some(sel) = self.cur.sel.as_ref() {
            assert_ne!((self.cur.line, self.cur.pos()), (sel.line, sel.pos));
        }

        // TODO: check for all reachable nodes:
        //  - parent consistency
        //  - db_key consistency
    }

    fn update_anchor(&mut self) {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        self.cur.anchor_x = blocks[self.cur.block].abs_cur_x(
            self.cur.line, self.cur.pos_skew,
            &self.ctx, blocks, nodes);
    }

    // If the cursor is on a VisTree::Node boundary and not on a line of text
    // (which should only happen when selecting),
    // move it to the inner line of text.
    fn sink_cursor(&mut self) {
        let blocks = &self.blocks;

        self.cur.sel = None;
        let b = &blocks[self.cur.block];
        match b.children[self.cur.line] {
            BlockChild::Leaf => {},
            BlockChild::Block(child_block) => {
                match self.cur.pos() {
                    0 => {
                        self.cur.block = child_block;
                        self.cur.line = 0;
                        self.cur.pos_skew = (0, Skew::default());
                    }
                    1 => {
                        let (block, line) = last_leaf(child_block, blocks);
                        self.cur.pos_skew = (
                            blocks[block].max_pos(line, blocks, &self.nodes),
                            Skew::default());
                        self.cur.line = line;
                        self.cur.block = block;
                    }
                    _ => panic!(),
                }
            }
        }
    }

    fn merge_undo_groups(&mut self) {
        let g2 = self.undo_buf.pop().unwrap();
        let mut g1 = self.undo_buf.pop().unwrap();
        assert_eq!(g1.cur_after, g2.cur_before);
        g1.edits.extend(g2.edits);
        g1.cur_after = g2.cur_after;
        self.undo_buf.push(g1);
    }
}

fn paint(app: &mut App) {
    app.check();
    let rt = &app.ctx.render_target;
    unsafe {
        rt.BeginDraw();
        rt.Clear(&D2D1_COLOR_F { r: 0.0, g: 0.0, b: 0.2, a: 1.0 });

        let mut v = gfx::DrawVisitor {
            ctx: &app.ctx,
            cur: &app.cur,
        };
        let mut y = app.y_offset;

        let blocks = &app.blocks;
        let nodes = &app.nodes;

        gfx::accept_block(app.root_block, &mut v, &mut y, &app.ctx, blocks, nodes);

        let hr = rt.EndDraw(null_mut(), null_mut());
        assert!(hr != D2DERR_RECREATE_TARGET, "TODO");
        assert!(hr == S_OK, "0x{:x}", hr);
    }
}

fn cut(hwnd: HWND, mut sr: StateRef<App>) {
    let mut app = sr.state_mut();
    let (lines, plain_text, res) = app.cut();
    drop(app);

    let mut cm = ClipboardManager::open(hwnd);
    cm.empty(sr.reent());
    cm.set_private();
    cm.set_text(&plain_text);
    cm.close();
    let mut app = sr.state_mut();
    app.clipboard = Some(Clipboard {
        sequence_number: get_clipboard_sequence_number(),
        lines,
    });
    res.process(hwnd, &mut app);
}

fn copy(hwnd: HWND, mut sr: StateRef<App>) {
    let app = sr.state_mut();
    let (lines, plain_text) = app.copy();
    drop(app);
    let mut cm = ClipboardManager::open(hwnd);
    cm.empty(sr.reent());
    cm.set_private();
    cm.set_text(&plain_text);
    cm.close();
    let mut app = sr.state_mut();
    app.clipboard = Some(Clipboard {
        sequence_number: get_clipboard_sequence_number(),
        lines,
    });
    CmdResult::regular().process(hwnd, &mut app);
}

fn paste(hwnd: HWND, mut sr: StateRef<App>) {
    let mut cm = ClipboardManager::open(hwnd);
    let has_private = cm.has_private();
    let plain_text = if has_private { None } else { cm.get_text() };
    cm.close();

    let mut app = sr.state_mut();
    let cmd_res = if has_private {
        let clipboard = app.clipboard.as_ref().unwrap(); 
        assert_eq!(clipboard.sequence_number, get_clipboard_sequence_number());                    
        let lines = clipboard.lines.clone();
        app.paste(lines)
    } else if let Some(plain_text) = plain_text {
        let lines: Vec<Line> = plain_text.split('\n')
            .map(|s| Line::Text { text: s.to_owned(), monospace: false })
            .collect();
        app.paste(lines)
    } else {
        CmdResult::nothing()
    };
    cmd_res.process(hwnd, &mut app);
}

impl WindowProcState for App {
    fn window_proc(
        mut sr: StateRef<Self>,
        hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM,
    )-> Option<LRESULT> {
        if PANICKING.load(Ordering::SeqCst) {
            return None;
        }
        match msg {
            WM_DESTROY => {
                eprintln!("{}", win_msg_name(msg));
                let app = &mut *sr.state_mut();
                if app.unsaved.has_changes() {
                    app.save_changes(hwnd);
                }
                unsafe {
                    PostQuitMessage(0);
                }
            }
            WM_SIZE => {
                println!("{}", win_msg_name(msg));
                let render_size = D2D_SIZE_U {
                    width: GET_X_LPARAM(lparam) as u32,
                    height: GET_Y_LPARAM(lparam) as u32,
                };
                let hr = unsafe {
                    sr.state_mut().ctx.render_target.Resize(&render_size)
                };
                assert!(hr == S_OK, "0x{:x}", hr);
            }
            WM_PAINT => {
                eprintln!("{}", win_msg_name(msg));
                paint(&mut *sr.state_mut());
            }
            WM_MOUSEWHEEL => {
                let delta = GET_WHEEL_DELTA_WPARAM(wparam);
                println!("{} {}", win_msg_name(msg), delta);
                let delta = f32::from(delta) / 120.0 * get_wheel_scroll_lines() as f32;
                let app = &mut *sr.state_mut();
                app.scroll(delta).process(hwnd, app);
            }
            WM_MOUSEMOVE => {
                let x = GET_X_LPARAM(lparam);
                let y = GET_Y_LPARAM(lparam);
                // println!("{} {} {}", win_msg_name(msg), x, y);
                let app = sr.state_mut();
                let mut v = gfx::MouseClickVisitor {
                    x: x as f32,
                    y: y as f32,
                    ctx: &app.ctx,
                    result: gfx::MouseResult::Nothing,
                };
                let mut yy = app.y_offset;
                gfx::accept_block(
                    app.root_block, &mut v, &mut yy,
                    &app.ctx,
                    &app.blocks, &app.nodes);
                let cur = match v.result {
                    gfx::MouseResult::Nothing => app.ctx.arrow_cursor,
                    gfx::MouseResult::Cur { .. } => app.ctx.beam_cursor,
                    gfx::MouseResult::Toggle { .. } => app.ctx.hand_cursor,
                };
                unsafe {
                    SetCursor(cur);
                }
            }
            WM_LBUTTONDOWN => {
                let x = GET_X_LPARAM(lparam);
                let y = GET_Y_LPARAM(lparam);
                println!("{} {} {}", win_msg_name(msg), x, y);
                let app = &mut *sr.state_mut();
                let mut v = gfx::MouseClickVisitor {
                    x: x as f32,
                    y: y as f32,
                    ctx: &app.ctx,
                    result: gfx::MouseResult::Nothing,
                };
                let mut yy = app.y_offset;
                gfx::accept_block(
                    app.root_block, &mut v, &mut yy,
                    &app.ctx,
                    &app.blocks, &app.nodes);
                match v.result {
                    gfx::MouseResult::Nothing => {}
                    gfx::MouseResult::Cur { block, line, pos_skew } => {
                        app.cur = Cur {
                            block,
                            line,
                            pos_skew,
                            anchor_x: 0.0,
                            sel: None,
                        };
                        CmdResult::regular().process(hwnd, app);
                    }
                    gfx::MouseResult::Toggle { block } => {
                        let blocks = &mut app.blocks;
                        let cblocks = &mut app.cblocks;
                        let nodes = &mut app.nodes;
                        let cmd_res = if blocks[block].is_expanded() {
                            app.collapse_block(block)
                        } else {
                            expand_block(block, blocks, cblocks, nodes, &mut app.unsaved);
                            CmdResult {
                                repaint: true,
                                update_anchor_x: true,
                                scroll_to_reveal_cursor: false,
                                class: CmdClass::Other,
                            }
                        };
                        cmd_res.process(hwnd, app);
                    }
                }
            }
            WM_KEYDOWN | WM_SYSKEYDOWN => {
                let key_code = wparam as i32;
                let scan_code = ((lparam >> 16) & 511) as i32;
                let ctrl_pressed = unsafe { GetKeyState(VK_CONTROL) } as u16 & 0x8000 != 0;
                let shift_pressed = unsafe { GetKeyState(VK_SHIFT) } as u16 & 0x8000 != 0;
                let alt_pressed = unsafe { GetKeyState(VK_MENU) } as u16 & 0x8000 != 0;
                println!("{} key=0x{:02x}, scan=0x{:02x}", win_msg_name(msg), key_code, scan_code);

                let mut app = sr.state_mut();
                let cmd_res = match key_code {
                    VK_LEFT => Some(
                        if alt_pressed {
                            app.alt_left()
                        } else if shift_pressed {
                            app.shift_left(ctrl_pressed)
                        } else {
                            app.left(ctrl_pressed)
                        }
                    ),
                    VK_RIGHT => Some(
                        if alt_pressed {
                            app.alt_right()
                        } else if shift_pressed {
                            app.shift_right(ctrl_pressed)
                        } else {
                            app.right(ctrl_pressed)
                        }
                    ),
                    VK_UP => Some(
                        if alt_pressed {
                            app.alt_up()
                        } else if shift_pressed {
                            app.shift_up()
                        } else {
                            app.up()
                        }
                    ),
                    VK_DOWN => Some(
                        if alt_pressed {
                            app.alt_down()
                        } else if shift_pressed {
                            app.shift_down()
                        } else {
                            app.down()
                        }
                    ),
                    VK_BACK => Some(app.backspace()),
                    VK_HOME => Some(
                        if shift_pressed {
                            app.shift_home()
                        } else {
                            app.home()
                        }
                    ),
                    VK_END => Some(
                        if shift_pressed {
                            app.shift_end()
                        } else {
                            app.end()
                        }
                    ),
                    VK_RETURN => if alt_pressed {
                        Some(app.alt_enter())
                    } else {
                        None
                    }
                    VK_INSERT => if ctrl_pressed {
                        drop(app);
                        copy(hwnd, sr);
                        return None;
                    } else if shift_pressed {
                        drop(app);
                        paste(hwnd, sr);
                        return None;
                    } else {
                        None
                    }
                    VK_DELETE => if shift_pressed {
                        drop(app);
                        cut(hwnd, sr);
                        return None;
                    } else {
                        Some(app.del())
                    }
                    VK_TAB => if ctrl_pressed {
                        Some(app.ctrl_tab(shift_pressed))
                    } else {
                        None
                    }
                    _ => None
                };
                if let Some(cmd_res) = cmd_res {
                    cmd_res.process(hwnd, &mut app);
                    return None;
                }

                if ctrl_pressed && key_code == 'P' as i32{
                    app.toggle_monospace().process(hwnd, &mut app);
                    return None;
                }

                if ctrl_pressed && key_code == 'Z' as i32{
                    app.undo().process(hwnd, &mut app);
                    return None;
                }
                if ctrl_pressed && key_code == 'Y' as i32 {
                    app.redo().process(hwnd, &mut app);
                    return None;
                }

                if ctrl_pressed && key_code == 'X' as i32 {
                    drop(app);
                    cut(hwnd, sr);
                    return None;
                }

                if ctrl_pressed && key_code == 'C' as i32 {
                    drop(app);
                    copy(hwnd, sr);
                    return None;
                }
                if ctrl_pressed && key_code == 'V' as i32 {
                    drop(app);
                    paste(hwnd, sr);
                    return None;
                }
            }
            WM_CHAR => {
                let c = std::char::from_u32(wparam as u32).unwrap();
                println!("{} {:?}", win_msg_name(msg), c);
                let mut app = sr.state_mut();
                if wparam >= 32 {
                    app.put_char(c).process(hwnd, &mut app);
                }
                if c == '\r' {
                    app.enter().process(hwnd, &mut app);
                }
                if c == '\t' {
                    app.tab().process(hwnd, &mut app);
                }
            }
            WM_SYSCHAR => {
                println!("{}", win_msg_name(msg));
                // Prevents annoying bell on unreckognized keys like Alt-Enter.
                // As a side effect, disables stuff like Alt-F to open file menu,
                // but we don't care about that.
                return Some(0);
            }
            WM_TIMER => {
                // println!("{}", win_msg_name(msg));
                let mut app = sr.state_mut();
                if app.unsaved.has_changes() &&
                   app.last_command_time.elapsed().as_secs_f64() > 3.0 {
                    app.save_changes(hwnd);
                }
            }
            WM_DESTROYCLIPBOARD => {
                println!("{}", win_msg_name(msg));
                let sn = get_clipboard_sequence_number();
                let app = &mut sr.state_mut();
                assert_eq!(sn, app.clipboard.as_ref().unwrap().sequence_number);
                app.clipboard = None;
            }
            _ => {}
        }
        None
    }
}

static STATIC_HWND: AtomicPtr<HWND__> = AtomicPtr::new(null_mut());
static PANICKING: AtomicBool = AtomicBool::new(false);

#[allow(dead_code)]
fn panic_hook(pi: &std::panic::PanicInfo) {
    PANICKING.store(true, Ordering::SeqCst);
    let payload: &str = 
        if let Some(&s) = pi.payload().downcast_ref::<&str>() {
            s
        } else if let Some(s) = pi.payload().downcast_ref::<String>() {
            &s
        } else {
            ""
        };
    let loc = match pi.location() {
        Some(loc) => format!("{}:{}:{}", loc.file(), loc.line(), loc.column()),
        None => "location unknown".to_owned()
    };

    // (anchor:aIMTMDTQfJDYrJxa)
    let exe = std::env::current_exe().unwrap();
    let exe_dir = exe.parent().unwrap();
    dbg!(&exe_dir);
    std::env::set_current_dir(exe_dir).unwrap();

    let bt = std::backtrace::Backtrace::force_capture();
    let message = format!(
        "panicked at {:?}, {}\nstack backtrace:\n{}",
        payload, loc, bt);
    eprintln!("{}", message);
    std::fs::write("crash.txt", message).unwrap();

    unsafe {
        use wio::wide::ToWide as _;
        MessageBoxW(
            STATIC_HWND.load(Ordering::SeqCst),
            "see crash.txt".to_wide_null().as_ptr(),
            "programming error".to_wide_null().as_ptr(),
            MB_OK | MB_ICONERROR);
    }

    std::process::exit(1);  
}

fn main() {
    #[cfg(feature="product")]
    std::panic::set_hook(Box::new(panic_hook));

    #[cfg(not(feature="product"))]
    {
        let default_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |pi| {
            // (anchor:aIMTMDTQfJDYrJxa)
            let exe = std::env::current_exe().unwrap();
            let exe_dir = exe.parent().unwrap();
            std::env::set_current_dir(exe_dir).unwrap();
            default_hook(pi)
        }));
    }

    let app = LazyState::new(|hwnd| {
        STATIC_HWND.store(hwnd, Ordering::SeqCst);
        set_timer(hwnd, 123, 1000);
        App::new(hwnd)
    });
    unsafe {
        let win_class = win_win::WindowClass::builder("e2 class")
            .build().unwrap();
        let hwnd = win_win::WindowBuilder::new(app, &win_class)
            .name("e2")
            .style(WS_OVERLAPPEDWINDOW)
            .build();
        ShowWindow(hwnd, SW_SHOWNORMAL);
        win_win::runloop(std::ptr::null_mut());
    }
}
