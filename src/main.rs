#![feature(bindings_after_at)]
#![feature(untagged_unions)]
#![allow(clippy::many_single_char_names)]

#[macro_use] pub mod slotmap;
mod win_util;
pub mod win_win_reent;
mod text_layout;
pub mod gfx;
mod util;
pub mod types;
pub mod edit;

use std::ptr::null_mut;
use once_cell::unsync::OnceCell;
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
    cursor_brush: ComPtr<ID2D1Brush>,
    sel_brush: ComPtr<ID2D1Brush>,
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
        let cursor_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 1.0, g: 1.0, b: 1.0, a: 1.0 });
        let sel_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 0.3, g: 0.3, b: 0.4, a: 1.0 });

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
            cursor_brush,
            sel_brush,
        }        
    }
}

pub struct Cur {
    pub block: BlockKey,
    pub line: usize,
    pub pos: usize,
    pub anchor_x: f32,
    pub sel: Option<Sel>,
}

pub struct Sel {
    pub pos: usize,
    pub line: usize,
}

struct App {
    ctx: AppCtx,

    nodes: Nodes,
    blocks: Blocks,

    y_offset: f32,
    root_block: BlockKey,  // owned key
    cur: Cur,
}

fn expand_block(block: BlockKey, blocks: &mut Blocks, nodes: &mut Nodes) {
    let b = &mut blocks[block];
    assert!(!b.expanded);
    b.expanded = true;
    assert_eq!(b.children.len(), 1);
    let node = b.node;
    for i in 0..nodes[node].lines.len() {
        match nodes[node].lines[i].line {
            Line::Text { .. } => blocks[block].children.push(BlockChild::Leaf),
            Line::Node { node: child_node, .. } => {
                let child_block = blocks.insert(Block {
                    expanded: false,
                    depth: blocks[block].depth + 1,
                    parent_idx: Some((block, blocks[block].children.len())),
                    node: child_node,
                    children: vec![BlockChild::Leaf],
                });
                nodes[child_node].blocks.push(child_block);
                blocks[block].children.push(BlockChild::Block(child_block));
            }
        }
    }
}

fn destroy_block(block: BlockKey, blocks: &mut Blocks, nodes: &mut Nodes) {
    let mut cnt = 0;
    nodes[blocks[block].node].blocks.retain(|&bb| {
        if bb == block {
            cnt += 1;
            false
        } else {
            true
        }
    });
    assert_eq!(cnt, 1);

    let block = blocks.remove(block);
    for child in block.children {
        match child {
            BlockChild::Leaf => {}
            BlockChild::Block(b) => destroy_block(b, blocks, nodes),
        }
    }
}

fn text_line(text: &str, monospace: bool) -> LineWithLayout {
    LineWithLayout {
        line: Line::Text {
            text: text.to_owned(),
            monospace,
        },
        layout: OnceCell::new(),
    }
}
fn node_line(local_header: &str, node: NodeKey) -> LineWithLayout {
    LineWithLayout {
        line: Line::Node {
            local_header: local_header.to_owned(),
            node,
        },
        layout: OnceCell::new(),
    }
}

impl App {
    fn new(hwnd: HWND) -> Self {
        let ctx = AppCtx::new(hwnd);

        let mut nodes = Nodes::new();
        let mut blocks = Blocks::new();

        let node2 = nodes.insert(Node {
            lines: vec![
                text_line("ccc", false),
            ],
            blocks: vec![],
        });

        let node1 = nodes.insert(Node {
            lines: vec![
                text_line("aaaa", false),
                node_line("node2", node2),
                text_line("bbb", false),
            ],
            blocks: vec![],
        });

        let root_node = nodes.insert(Node {
            lines: vec![
                text_line("Stuff", false),
                text_line("if name == '__main__':", true),
                text_line("    print('hello')", true),
                text_line("Stuff...", false),
                node_line("zzz", node1),
                node_line("zzz", node1),
                text_line("Stuff.", false),
            ],
            blocks: vec![],
        });
        let root_block = blocks.insert(Block {
            depth: 0,
            parent_idx: None,
            node: root_node,
            expanded: false,
            children: vec![BlockChild::Leaf],
        });
        nodes[root_node].blocks.push(root_block);

        expand_block(root_block, &mut blocks, &mut nodes);
        expand_block(nodes[node1].blocks[0], &mut blocks, &mut nodes);

        let mut app = App {
            ctx,
            nodes,
            blocks,
            cur: Cur {
                block: root_block,
                line: 5,
                pos: 1,
                anchor_x: 0.0,
                sel: Some(Sel {
                    line: 5,
                    pos: 0,
                })
            },
            root_block,
            y_offset: 10.0,
        };
        app.update_anchor();
        app
    }

    fn check(&self) {
        fn rec_check_block(
            block: BlockKey, cnt: &mut usize,
            blocks: &Blocks, nodes: &Nodes,
        ) {
            *cnt += 1;
            check_block(block, blocks, nodes);
            for child in &blocks[block].children {
                match *child {
                    BlockChild::Leaf => {}
                    BlockChild::Block(b) => rec_check_block(b, cnt, blocks, nodes),
                }
            }
        }
        let mut cnt = 0;
        rec_check_block(self.root_block, &mut cnt, &self.blocks, &self.nodes);
        assert_eq!(self.blocks.len(), cnt);
    }

    fn scroll(&mut self, delta: f32) {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        // TODO: use actual line height
        self.y_offset += delta * 18.0;
        self.y_offset = self.y_offset.min(10.0);
        let height = blocks[self.root_block].size(&self.ctx, blocks, nodes).1;
        let max_offset = 10.0 - height
            + blocks[self.root_block].last_line_height(&self.ctx, blocks, nodes);
        self.y_offset = self.y_offset.max(max_offset);
    }

    fn update_anchor(&mut self) {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        self.cur.anchor_x = blocks[self.cur.block].abs_cur_x(
            self.cur.line, self.cur.pos, &self.ctx, blocks, nodes);
    }

    fn collapse_block(&mut self, block: BlockKey) {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        assert!(blocks[block].expanded);
        if blocks[block].depth <= blocks[self.cur.block].depth {
            let mut b = self.cur.block;
            while blocks[block].depth < blocks[b].depth {
                b = blocks[b].parent_idx.unwrap().0;
            }
            if block == b {
                let first_line_len = blocks[block].max_pos(0, blocks, nodes);
                if blocks[block].depth < blocks[self.cur.block].depth {
                    self.cur.block = block;
                    self.cur.line = 0;
                    self.cur.pos = first_line_len;
                    self.cur.sel = None;
                } else {
                    assert_eq!(self.cur.block, block);
                    if let Some(sel) = self.cur.sel.as_mut() {
                        if sel.line > 0 {
                            sel.line = 0;
                            sel.pos = first_line_len;
                        }
                    }
                    if self.cur.line > 0 {
                        self.cur.line = 0;
                        self.cur.pos = first_line_len;
                    }
                    if let Some(sel) = self.cur.sel.as_ref() {
                        if self.cur.line == sel.line && self.cur.pos == sel.pos {
                            self.cur.sel = None;
                        }
                    }
                }
            }
        }

        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;

        let b = &mut blocks[block];
        b.expanded = false;

        let children_to_remove: Vec<_> = b.children.drain(1..).collect();
        for child in children_to_remove {
            match child {
                BlockChild::Leaf => {}
                BlockChild::Block(bc) => destroy_block(bc, blocks, nodes),
            }
        }
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
                match self.cur.pos {
                    0 => {
                        self.cur.block = child_block;
                        self.cur.line = 0;
                        self.cur.pos = 0;
                    }
                    1 => {
                        let (block, line) = last_leaf(child_block, blocks);
                        self.cur.pos = blocks[block].max_pos(line, blocks, &self.nodes);
                        self.cur.line = line;
                        self.cur.block = block;
                    }
                    _ => panic!(),
                }
            }
        }
    }

    fn left(&mut self) {
        self.sink_cursor();

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let b = &blocks[self.cur.block];
        match blocks[self.cur.block].children[self.cur.line] {
            BlockChild::Block(_) => panic!(),
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
                let node = &nodes[node];
                let text = &node.lines[line_idx].line.text();
                if let Some(pos) = prev_char_pos(text, self.cur.pos) {
                    self.cur.pos = pos;
                    return;
                }
                let leaf = (self.cur.block, self.cur.line);
                if let Some((prev_block, prev_line)) = prev_leaf(leaf, blocks) {
                    let b = &blocks[prev_block];
                    let (node, line_idx) = b.node_line_idx(prev_line, blocks).unwrap();
                    let node = &nodes[node];
                    let text = &node.lines[line_idx].line.text();
                    
                    self.cur.pos = text.len();
                    self.cur.line = prev_line;
                    self.cur.block = prev_block;
                }
            }
        }
    }

    fn right(&mut self) {
        self.sink_cursor();

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let b = &blocks[self.cur.block];
        match b.children[self.cur.line] {
            BlockChild::Block(_) => panic!(),
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
                let node = &nodes[node];
                let text = &node.lines[line_idx].line.text();
                if let Some(pos) = next_char_pos(text, self.cur.pos) {
                    self.cur.pos = pos;
                    return;
                }
                let leaf = (self.cur.block, self.cur.line);
                if let Some((next_block, next_line)) = next_leaf(leaf, blocks) {
                    self.cur.line = next_line;
                    self.cur.pos = 0;
                    self.cur.block = next_block;
                }
            }
        }
    }

    fn up(&mut self) {
        self.sink_cursor();

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let node = &nodes[node];
        let layout = node.line_layout(line_idx, &self.ctx);

        let eps = 3.0;
        let cc = layout.cursor_coord(self.cur.pos);
        if cc.top - eps > 0.0 {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos = layout.coords_to_pos(x, cc.top - eps);
        } else {
            let prev_leaf = prev_leaf((self.cur.block, self.cur.line), blocks);
            if let Some((prev_block, prev_idx)) = prev_leaf {
                let b = &blocks[prev_block];

                let y = b.size(&self.ctx, blocks, nodes).1 - eps;

                let (node, line_idx) = b.node_line_idx(prev_idx, blocks).unwrap();
                let node = &nodes[node];
                let layout = node.line_layout(line_idx, &self.ctx);

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);

                self.cur.line = prev_idx;
                self.cur.pos = layout.coords_to_pos(x, y);
                self.cur.block = prev_block;
            }
        }
    }

    fn down(&mut self) {
        self.sink_cursor();

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let node = &nodes[node];
        let layout = node.line_layout(line_idx, &self.ctx);

        let eps = 3.0;
        let cc = layout.cursor_coord(self.cur.pos);
        if cc.top + cc.height + eps < layout.height {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos = layout.coords_to_pos(x, cc.top + cc.height + eps);
        } else {
            let next_leaf = next_leaf((self.cur.block, self.cur.line), blocks);
            if let Some((next_block, next_idx)) = next_leaf {
                let b = &blocks[next_block];

                let y = eps;

                let (node, line_idx) = b.node_line_idx(next_idx, blocks).unwrap();
                let node = &nodes[node];
                let layout = node.line_layout(line_idx, &self.ctx);

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);

                self.cur.line = next_idx;
                self.cur.pos = layout.coords_to_pos(x, y);
                self.cur.block = next_block;
            }
        }
    }

    fn put_char(&mut self, c: char) {
        assert!(self.cur.sel.is_none(), "TODO");

        let blocks = &self.blocks;
        let nodes = &mut self.nodes;

        let (node, line_idx) = blocks[self.cur.block].node_line_idx(self.cur.line, blocks).unwrap();
        let line = &mut nodes[node].lines[line_idx];
        let text = line.line.text_mut();

        if c == ' ' && self.cur.line > 0 && self.cur.pos == 1 && text.starts_with("*") {
            let local_header = text[1..].to_owned();
            let blocks = &mut self.blocks;
            let new_node = nodes.insert(Node {
                lines: vec![],
                blocks: vec![],
            });
            let new_block = blocks.insert(Block {
                depth: blocks[self.cur.block].depth + 1,
                parent_idx: Some((self.cur.block, self.cur.line)),
                node: new_node,
                children: vec![BlockChild::Leaf],
                expanded: false,
            });
            nodes[new_node].blocks.push(new_block);
            nodes[node].lines[line_idx] = LineWithLayout {
                line: Line::Node { local_header, node: new_node },
                layout: OnceCell::new(),
            };
            blocks[self.cur.block].children[self.cur.line] = BlockChild::Block(new_block);
            self.cur.block = new_block;
            self.cur.line = 0;
            self.cur.pos = 0;
            return;
        }

        text.insert(self.cur.pos, c);
        self.cur.pos += c.len_utf8();

        line.layout = OnceCell::new();
    }

    fn enter(&mut self) {
        assert!(self.cur.sel.is_none(), "TODO");

        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;

        if self.cur.line == 0 && !blocks[self.cur.block].expanded {
            expand_block(self.cur.block, blocks, nodes);
        }

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let node = &mut nodes[node];
        let line = &mut node.lines[line_idx];
        let text = line.line.text_mut();

        let tail = text[self.cur.pos..].to_owned();
        text.truncate(self.cur.pos);
        line.layout = OnceCell::new();

        if self.cur.line == 0 {
            splice_node_lines(b.node, 0, 0, 
                vec![Line::Text { text: tail, monospace: false }],
                blocks, nodes);
        } else {
            let monospace = match line.line {
                Line::Text { monospace, .. } => monospace,
                Line::Node { .. } => panic!(),
            };
            splice_node_lines(b.node, line_idx + 1, line_idx + 1,
                vec![Line::Text { text: tail, monospace }],
                blocks, nodes);
        }
        self.cur.line += 1;
        self.cur.pos = 0;
    }

    fn tab(&mut self) {
        if self.cur.sel.is_some() {
            return;  // TODO?
        }
        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;
        if self.cur.line == 0 {
            if blocks[self.cur.block].expanded {
                self.collapse_block(self.cur.block);
            } else {
                expand_block(self.cur.block, blocks, nodes);
            }
        }
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

impl WindowProcState for App {
    #[allow(unused_variables)]
    fn window_proc(
        mut sr: StateRef<Self>,
        hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM,
    )-> Option<LRESULT> {
        if msg == WM_DESTROY {
            eprintln!("{}", win_msg_name(msg));
            unsafe {
                PostQuitMessage(0);
            }
        }
        if msg == WM_SIZE {
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
        if msg == WM_PAINT {
            eprintln!("{}", win_msg_name(msg));
            paint(&mut *sr.state_mut());
        }
        if msg == WM_MOUSEWHEEL {
            let delta = GET_WHEEL_DELTA_WPARAM(wparam);
            println!("{} {}", win_msg_name(msg), delta);
            let delta = f32::from(delta) / 120.0 * get_wheel_scroll_lines() as f32;
            sr.state_mut().scroll(delta);
            invalidate_rect(hwnd);
        }
        if msg == WM_MOUSEMOVE {
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
        if msg == WM_LBUTTONDOWN {
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
                gfx::MouseResult::Cur { block, line, pos } => {
                    app.cur = Cur {
                        block,
                        line,
                        pos,
                        anchor_x: 0.0,
                        sel: None,
                    };
                    app.update_anchor();
                    invalidate_rect(hwnd);
                }
                gfx::MouseResult::Toggle { block } => {
                    let blocks = &mut app.blocks;
                    let nodes = &mut app.nodes;
                    if blocks[block].expanded {
                        app.collapse_block(block);
                        app.update_anchor();
                        invalidate_rect(hwnd);
                    } else {
                        expand_block(block, blocks, nodes);
                        app.update_anchor();
                        invalidate_rect(hwnd);
                    }
                }
            }
        }
        if msg == WM_KEYDOWN {
            let key_code = wparam as i32;
            println!("{} {}", win_msg_name(msg), key_code);
            let mut app = sr.state_mut();
            if key_code == VK_LEFT {
                app.left();
                app.update_anchor();
            }
            if key_code == VK_RIGHT {
                app.right();
                app.update_anchor();
            }
            if key_code == VK_UP {
                app.up();
            }
            if key_code == VK_DOWN {
                app.down();
            }
            invalidate_rect(hwnd);
        }
        if msg == WM_CHAR {
            let c = std::char::from_u32(wparam as u32).unwrap();
            println!("{} {:?}", win_msg_name(msg), c);
            let mut app = sr.state_mut();
            if wparam >= 32 {
                app.put_char(c);
                app.update_anchor();
            }
            if c == '\r' {
                app.enter();
                app.update_anchor();
            }
            if c == '\t' {
                app.tab();
                app.update_anchor();
            }
            invalidate_rect(hwnd);
        }
        None
    }
}

fn main() {
    let app = LazyState::new(App::new);
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
