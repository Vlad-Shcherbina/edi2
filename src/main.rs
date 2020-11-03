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
    line: usize,
    pos: usize,

    anchor_path: Vec<usize>,
    anchor_line: usize,
    anchor_pos: usize,
}

struct Clipboard {
    sequence_number: u32,
    lines: Vec<Line>,
}

struct App {
    ctx: AppCtx,

    nodes: Nodes,
    blocks: Blocks,

    y_offset: f32,
    root_block: BlockKey,  // owned key
    cur: Cur,

    clipboard: Option<Clipboard>,
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
        nodes[node2].lines.push(node_line("recursion", node1));

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
                line: 1,
                pos: 0,
                anchor_x: 0.0,
                sel: None,
            },
            root_block,
            y_offset: 10.0,
            clipboard: None,
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

    fn shift_left(&mut self) {
        let line = self.cur.line;
        let pos = self.cur.pos;
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
            anchor_line: line,
            anchor_pos: pos,
        });
        let blocks = &self.blocks;
        let nodes = &self.nodes;
        if (self.cur.line, self.cur.pos) <= (sel.line, sel.pos) {
            match blocks[self.cur.block].children[self.cur.line] {
                BlockChild::Block(_) =>
                    if self.cur.pos == 1 {
                        self.cur.pos = 0;
                        return;
                    }
                BlockChild::Leaf => {
                    let (node, line_idx) = blocks[self.cur.block]
                        .node_line_idx(self.cur.line, blocks).unwrap();
                    let text = &nodes[node].lines[line_idx].line.text();
                    if let Some(pos) = prev_char_pos(text, self.cur.pos) {
                        self.cur.pos = pos;
                        return;
                    }                        
                }
            }
            if self.cur.block == self.root_block && self.cur.line == 1 {
                return;
            }
            if self.cur.line > 0 {
                self.cur.line -= 1;
                self.cur.pos = blocks[self.cur.block].max_pos(self.cur.line, blocks, nodes);
                return;
            }

            let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
            self.cur.block = parent;
            self.cur.line = i;
            self.cur.pos = 0;

            sel.line = i;
            sel.pos = 1;
            sel.anchor_path.push(i);
        } else {
            loop {
                match blocks[self.cur.block].children[self.cur.line] {
                    BlockChild::Block(_) =>
                        if self.cur.pos == 1 {
                            self.cur.pos = 0;
                            break;
                        }
                    BlockChild::Leaf => {
                        let (node, line_idx) = blocks[self.cur.block]
                            .node_line_idx(self.cur.line, blocks).unwrap();
                        let text = &nodes[node].lines[line_idx].line.text();
                        if let Some(pos) = prev_char_pos(text, self.cur.pos) {
                            self.cur.pos = pos;
                            break;
                        }                               
                    }
                }
                if self.cur.line > 0 {
                    self.cur.line -= 1;
                    self.cur.pos = blocks[self.cur.block].max_pos(self.cur.line, blocks, nodes);
                    break;
                }
                panic!("shouldn't happen when shrinking");
            }

            if self.cur.line == sel.line && self.cur.pos == sel.pos {
                if let Some(i) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[i] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = blocks[self.cur.block].children.len() - 1;
                    self.cur.pos = blocks[self.cur.block].max_pos(self.cur.line, blocks, nodes);

                    match sel.anchor_path.last() {
                        Some(&i) => {
                            sel.line = i;
                            sel.pos = 0;
                        }
                        None => {
                            sel.line = sel.anchor_line;
                            sel.pos = sel.anchor_pos;
                        }
                    }
                }
            }

            if self.cur.line == sel.line && self.cur.pos == sel.pos {
                self.cur.sel = None;
            }
        }
    }

    fn shift_right(&mut self) {
        let line = self.cur.line;
        let pos = self.cur.pos;
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
            anchor_line: line,
            anchor_pos: pos,
        });
        let blocks = &self.blocks;
        let nodes = &self.nodes;
        if (self.cur.line, self.cur.pos) >= (sel.line, sel.pos) {
            match blocks[self.cur.block].children[self.cur.line] {
                BlockChild::Block(_) =>
                    if self.cur.pos == 0 {
                        self.cur.pos = 1;
                        return;
                    }
                BlockChild::Leaf => {
                    let (node, line_idx) = blocks[self.cur.block]
                        .node_line_idx(self.cur.line, blocks).unwrap();
                    let text = &nodes[node].lines[line_idx].line.text();
                    if let Some(pos) = next_char_pos(text, self.cur.pos) {
                        self.cur.pos = pos;
                        return;
                    }                        
                }
            }
            if self.cur.line + 1 < blocks[self.cur.block].children.len() {
                self.cur.line += 1;
                self.cur.pos = 0;
                return;
            }
            if self.cur.block == self.root_block {
                return;
            }

            let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
            self.cur.block = parent;
            self.cur.line = i;
            self.cur.pos = 1;
            sel.line = i;
            sel.pos = 0;
            sel.anchor_path.push(i);
        } else {
            loop {
                match blocks[self.cur.block].children[self.cur.line] {
                    BlockChild::Block(_) =>
                        if self.cur.pos == 0 {
                            self.cur.pos = 1;
                            break;
                        }
                    BlockChild::Leaf => {
                        let (node, line_idx) = blocks[self.cur.block]
                            .node_line_idx(self.cur.line, blocks).unwrap();
                        let text = &nodes[node].lines[line_idx].line.text();
                        if let Some(pos) = next_char_pos(text, self.cur.pos) {
                            self.cur.pos = pos;
                            break;
                        }                               
                    }
                }
                if self.cur.line + 1 < blocks[self.cur.block].children.len() {
                    self.cur.line += 1;
                    self.cur.pos = 0;
                    break;
                }
                panic!("shouldn't happen when shrinking");
            }

            if self.cur.line == sel.line && self.cur.pos == sel.pos {
                if let Some(i) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[i] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = 0;
                    self.cur.pos = 0;

                    match sel.anchor_path.last() {
                        Some(&i) => {
                            sel.line = i;
                            sel.pos = blocks[self.cur.block].max_pos(i, blocks, nodes);
                        }
                        None => {
                            sel.line = sel.anchor_line;
                            sel.pos = sel.anchor_pos;
                        }
                    }
                }
            }

            if self.cur.line == sel.line && self.cur.pos == sel.pos {
                self.cur.sel = None;
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

        if c == ' ' && self.cur.line > 0 && self.cur.pos == 1 && text.starts_with('*') {
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

    fn backspace(&mut self) {
        assert!(self.cur.sel.is_none(), "TODO");

        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let line = &mut nodes[node].lines[line_idx];
        let text = line.line.text_mut();

        if let Some(prev_pos) = prev_char_pos(text, self.cur.pos) {
            text.remove(prev_pos);
            line.layout = OnceCell::new();
            self.cur.pos = prev_pos;
            return;
        }

        let prev_leaf = prev_leaf((self.cur.block, self.cur.line), blocks);
        let (prev_block, prev_idx) = match prev_leaf {
            Some(x) => x,
            None => return,
        };

        if  blocks[prev_block].depth > b.depth {
            if prev_idx == 0 && !blocks[prev_block].expanded {
                // TODO: silent autoexpand if it's one-line node
                expand_block(prev_block, blocks, nodes);
                return;
            }
        } else if prev_block != self.cur.block {
            assert_eq!(self.cur.line, 0);

            let (parent_block, idx_in_parent) = b.parent_idx.unwrap();
            let mut ancestor = parent_block;
            loop {
                if blocks[ancestor].node == b.node {
                    self.cur.block = ancestor;
                    return;
                }
                ancestor = match blocks[ancestor].parent_idx {
                    None => break,
                    Some(x) => x.0,
                };
            }

            if !b.expanded {
                // TODO: silent autoexpand if it's one-line node
                expand_block(self.cur.block, blocks, nodes);
                return;
            }

            let mut lines = splice_node_lines(
                blocks[self.cur.block].node,
                0, nodes[blocks[self.cur.block].node].lines.len(),
                vec![],
                blocks, nodes);

            assert!(idx_in_parent > 0);
            let parent_node = blocks[parent_block].node;
            let line = &nodes[parent_node].lines[idx_in_parent - 1].line;
            let local_header = match line {
                Line::Text {..} => panic!(),
                Line::Node { ref local_header, ..} => local_header.to_owned(),
            };

            lines.insert(0, Line::Text { text: local_header, monospace: false });

            splice_node_lines(
                blocks[parent_block].node,
                idx_in_parent - 1, idx_in_parent,
                lines,
                blocks, nodes);
            self.cur.block = parent_block;
            self.cur.line = idx_in_parent;
            self.cur.pos = 0;
            return;
        }

        let text = text.to_owned();
        let (prev_node, prev_line_idx) = blocks[prev_block].node_line_idx(prev_idx, blocks).unwrap();
        let prev_line = &mut nodes[prev_node].lines[prev_line_idx];
        let prev_text = prev_line.line.text_mut();
        self.cur.block = prev_block;
        self.cur.line = prev_idx;
        self.cur.pos = prev_text.len();
        prev_line.layout = OnceCell::new();
        prev_text.push_str(&text);
        splice_node_lines(node, line_idx, line_idx + 1, vec![], blocks, nodes);
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

    fn copy(&mut self) -> (Vec<Line>, String) {
        let sel = match self.cur.sel.as_ref() {
            Some(sel) => sel,
            None => return (vec![], String::new()),  // TODO: copy whole line/node?
        };
        let (line1, pos1) = (self.cur.line, self.cur.pos).min((sel.line, sel.pos));
        let (line2, pos2) = (self.cur.line, self.cur.pos).max((sel.line, sel.pos));

        let blocks = &self.blocks;
        let nodes = &self.nodes;
        let b = &blocks[self.cur.block];

        let mut lines = vec![];
        let mut plain_text = String::new();
        for line in line1..=line2 {
            let start_pos = if line == line1 { pos1 } else { 0 };
            let end_pos = if line == line2 { pos2 } else { b.max_pos(line, blocks, nodes) };
            // TODO: use reuse slice_block_line() here?
            match b.children[line] {
                BlockChild::Leaf => {
                    let (node, line_idx) = b.node_line_idx(line, blocks).unwrap();
                    let line = nodes[node].lines[line_idx].line.slice(start_pos, end_pos);
                    plain_text.push_str(&line.text());
                    lines.push(line);
                },
                BlockChild::Block(_) => {
                    match (start_pos, end_pos) {
                        (0, 0) | (1, 1) => {
                            lines.push(Line::new_empty());
                        }
                        (0, 1) => {
                            let line = &nodes[b.node].lines[line - 1].line;
                            let text = line.text();
                            plain_text.push_str("* ");
                            plain_text.push_str(text);
                            lines.push(line.clone());
                        }
                        other => panic!("{:?}", other),
                    }
                }
            }

            if line != line2 {
                plain_text.push('\n');
            }
        }
        (lines, plain_text)
    }

    fn paste(&mut self, lines: Vec<Line>) {
        let cur_line_pos = (self.cur.line, self.cur.pos);
        let sel_line_pos = match self.cur.sel.as_ref() {
            Some(sel) => (sel.line, sel.pos),
            None => cur_line_pos,
        };
        let (line1, pos1) = cur_line_pos.min(sel_line_pos);
        let (line2, pos2) = cur_line_pos.max(sel_line_pos);

        // TODO: check that there are no self-references

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let b = &blocks[self.cur.block];

        let mut new_lines = vec![Line::new_empty()];

        let mut cur_line_pos = None;
        assert!(!lines.is_empty());
        let num_lines = lines.len();
        for (i, line) in lines.into_iter().enumerate() {
            if i == 0 {
                concatenate_with_last_line(
                    &mut new_lines,
                    slice_block_line(b, line1, 0, pos1, blocks, nodes));
            }
            concatenate_with_last_line(
                &mut new_lines,
                line);
            if i + 1 == num_lines {
                let new_cur_pos = match new_lines.last().unwrap() {
                    Line::Text { text, .. } => text.len(),
                    Line::Node { .. } => 1,
                };
                cur_line_pos = Some((line1 + new_lines.len() - 1, new_cur_pos));

                let max_pos = b.max_pos(line2, blocks, nodes);
                concatenate_with_last_line(
                    &mut new_lines,
                    slice_block_line(b, line2, pos2, max_pos, blocks, nodes));
            } else {
                new_lines.push(Line::new_empty());
            }
        }
        let cur_line_pos = cur_line_pos.unwrap();
        self.cur.line = cur_line_pos.0;
        self.cur.pos = cur_line_pos.1;
        self.cur.sel = None;

        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;

        let line1 = if line1 == 0 {
            let new_header_text = match new_lines.first().unwrap() {
                Line::Text { .. } =>
                    new_lines.remove(0).text().to_owned(),
                Line::Node { .. } => {
                    self.cur.line += 1;
                    String::new()
                }
            };
            let b = &blocks[self.cur.block];
            let (node_key, node_line) = b.node_line_idx(0, blocks).unwrap();
            let ll = &mut nodes[node_key].lines[node_line];
            ll.layout = OnceCell::new();
            match &mut ll.line {
                Line::Text { .. } => panic!(),
                Line::Node { local_header, .. } => {
                    *local_header = new_header_text;
                }
            }
            line1 + 1
        } else {
            line1
        };

        let node_key = blocks[self.cur.block].node;
        splice_node_lines(
            node_key,
            line1 - 1, line2 + 1 - 1, new_lines,
            blocks, nodes);

        self.sink_cursor();
    }
}

fn slice_block_line(
    b: &Block, line: usize,
    start_pos: usize, end_pos: usize,
    blocks: &Blocks, nodes: &Nodes,
) -> Line {
    match b.children[line] {
        BlockChild::Leaf => {
            let (node, line_idx) = b.node_line_idx(line, blocks).unwrap();
            nodes[node].lines[line_idx].line.slice(start_pos, end_pos)
        },
        BlockChild::Block(_) => {
            match (start_pos, end_pos) {
                (0, 0) | (1, 1) =>
                    Line::Text { text: String::new(), monospace: false },
                (0, 1) =>
                    nodes[b.node].lines[line - 1].line.clone(),
                other => panic!("{:?}", other),
            }
        }
    }
}

fn concatenate_with_last_line(lines: &mut Vec<Line>, line: Line) {
    if line.is_empty() {
        return;
    }
    let mut last = lines.pop().unwrap();
    if last.is_empty() {
        lines.push(line);
        return;
    }
    match (&mut last, &line) {
        (Line::Text { text: text1, .. }, Line::Text { text: text2, .. }) => {
            text1.push_str(text2);
            lines.push(last);
        },
        _ => {
            lines.push(last);
            lines.push(line);
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
            let scan_code = ((lparam >> 16) & 511) as i32;
            let ctrl_pressed = unsafe { GetKeyState(VK_CONTROL) } as u16 & 0x8000 != 0;
            let shift_pressed = unsafe { GetKeyState(VK_SHIFT) } as u16 & 0x8000 != 0;
            println!("{} key=0x{:02x}, scan=0x{:02x}", win_msg_name(msg), key_code, scan_code);

            let mut app = sr.state_mut();
            if key_code == VK_LEFT {
                if shift_pressed {
                    app.shift_left();
                } else {
                    app.left();
                }
                app.update_anchor();
            }
            if key_code == VK_RIGHT {
                if shift_pressed {
                    app.shift_right();
                } else {
                    app.right();
                }
                app.update_anchor();
            }
            if key_code == VK_UP {
                app.up();
            }
            if key_code == VK_DOWN {
                app.down();
            }
            if key_code == VK_BACK {
                app.backspace();
                app.update_anchor();
            }
            if ctrl_pressed && scan_code == 0x2e {  // Ctrl-C
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
                return None;
            }
            if ctrl_pressed && scan_code == 0x2f {  // Ctrl-V
                drop(app);

                let mut cm = ClipboardManager::open(hwnd);
                let has_private = cm.has_private();
                let plain_text = if has_private { None } else { cm.get_text() };
                cm.close();

                let mut app = sr.state_mut();
                if has_private {
                    let clipboard = app.clipboard.as_ref().unwrap(); 
                    assert_eq!(clipboard.sequence_number, get_clipboard_sequence_number());                    
                    let lines = clipboard.lines.clone();
                    app.paste(lines);
                } else if let Some(plain_text) = plain_text {
                    let lines: Vec<Line> = plain_text.split('\n')
                        .map(|s| Line::Text { text: s.to_owned(), monospace: false })
                        .collect();
                    app.paste(lines);
                }
                return None;
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
        if msg == WM_DESTROYCLIPBOARD {
            println!("{}", win_msg_name(msg));
            let sn = get_clipboard_sequence_number();
            let app = &mut sr.state_mut();
            assert_eq!(sn, app.clipboard.as_ref().unwrap().sequence_number);
            app.clipboard = None;
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
