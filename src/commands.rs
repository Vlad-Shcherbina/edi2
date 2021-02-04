use fnv::FnvHashMap;
use super::*;
use crate::gfx::MAX_WIDTH;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmdClass {
    PutChar,
    PutWhitespace,
    BackspaceChar,
    BackspaceNewLine,
    DelChar,
    Other,
}

#[must_use]
pub struct CmdResult {
    pub repaint: bool,
    pub update_anchor_x: bool,
    pub scroll_to_reveal_cursor: bool,
    pub class: CmdClass,
}

impl CmdResult {
    pub fn nothing() -> Self {
        CmdResult {
            repaint: false,
            update_anchor_x: false,
            scroll_to_reveal_cursor: false,
            class: CmdClass::Other,
        }
    }
    pub fn regular() -> Self {
        CmdResult {
            repaint: true,
            update_anchor_x: true,
            scroll_to_reveal_cursor: true,
            class: CmdClass::Other,
        }
    }
}

impl Drop for CmdResult {
    fn drop(&mut self) {
        panic!("must call .process()");
    }
}

impl App {
    fn clamp_y_offset(&mut self) {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        self.y_offset = self.y_offset.min(10.0);
        let height = blocks[self.root_block].size(&self.ctx, blocks, nodes).1;
        let max_offset = 10.0 - height
            + blocks[self.root_block].last_line_height(&self.ctx, blocks, nodes);
        self.y_offset = self.y_offset.max(max_offset);        
    }

    pub fn scroll(&mut self, delta: f32) -> CmdResult {
        // TODO: use actual line height
        self.y_offset += delta * 18.0;
        self.clamp_y_offset();

        CmdResult {
            repaint: true,
            update_anchor_x: false,
            scroll_to_reveal_cursor: false,
            class: CmdClass::Other,
        }
    }

    pub fn collapse_block(&mut self, block: BlockKey) -> CmdResult {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        assert!(blocks[block].is_expanded());
        self.unsaved.tree = true;
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
                    self.cur.pos_skew = (first_line_len, Skew::default());
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
                        self.cur.pos_skew = (first_line_len, Skew::default());
                    }
                    if let Some(sel) = self.cur.sel.as_ref() {
                        if self.cur.line == sel.line && self.cur.pos() == sel.pos {
                            self.cur.sel = None;
                        }
                    }
                }
            }
        }

        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        let mut cforest = CForest::new();

        let children_to_remove: Vec<_> = blocks[block].children.drain(1..).collect();
        for (i, child) in children_to_remove.into_iter().enumerate() {
            let i = i + 1;
            match child {
                BlockChild::Leaf => {}
                BlockChild::Block(bc) => {
                    let cb = block_to_cblock(bc, blocks, cblocks, nodes);
                    if let Some(cb) = cb {
                        cforest.0.insert(i, cb);
                    }
                }
            }
        }

        blocks[block].collapsed = Some(cforest);

        CmdResult {
            repaint: true,
            update_anchor_x: true,
            scroll_to_reveal_cursor: false,
            class: CmdClass::Other,
        }
    }

    pub fn left(&mut self, ctrl: bool) -> CmdResult {
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
                let prev_pos = if ctrl {
                    prev_word_pos(text, self.cur.pos())
                } else {
                    prev_char_pos(text, self.cur.pos())
                };
                if let Some(pos) = prev_pos {
                    self.cur.pos_skew = (pos, Skew::Righty);
                    return CmdResult::regular();
                }
                let leaf = (self.cur.block, self.cur.line);
                if let Some((prev_block, prev_line)) = prev_leaf(leaf, blocks) {
                    let b = &blocks[prev_block];
                    let (node, line_idx) = b.node_line_idx(prev_line, blocks).unwrap();
                    let node = &nodes[node];
                    let text = &node.lines[line_idx].line.text();
                    
                    self.cur.pos_skew = (text.len(), Skew::default());
                    self.cur.line = prev_line;
                    self.cur.block = prev_block;
                }
            }
        }

        CmdResult::regular()
    }

    pub fn right(&mut self, ctrl: bool) -> CmdResult {
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
                let next_pos = if ctrl {
                    next_word_pos(text, self.cur.pos())
                } else {
                    next_char_pos(text, self.cur.pos())
                };
                if let Some(pos) = next_pos {
                    self.cur.pos_skew = (pos, Skew::Righty);
                    return CmdResult::regular();
                }
                let leaf = (self.cur.block, self.cur.line);
                if let Some((next_block, next_line)) = next_leaf(leaf, blocks) {
                    self.cur.line = next_line;
                    self.cur.pos_skew = (0, Skew::default());
                    self.cur.block = next_block;
                }
            }
        }

        CmdResult::regular()
    }

    pub fn shift_left(&mut self, ctrl: bool) -> CmdResult {
        let line = self.cur.line;
        let pos = self.cur.pos();
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
        });
        let blocks = &self.blocks;
        let nodes = &self.nodes;
        if (self.cur.line, pos) <= (sel.line, sel.pos) {
            match blocks[self.cur.block].children[self.cur.line] {
                BlockChild::Block(_) =>
                    if pos == 1 {
                        self.cur.pos_skew = (0, Skew::default());
                        return CmdResult::regular();
                    }
                BlockChild::Leaf => {
                    let (node, line_idx) = blocks[self.cur.block]
                        .node_line_idx(self.cur.line, blocks).unwrap();
                    let text = &nodes[node].lines[line_idx].line.text();
                    let prev_pos = if ctrl {
                        prev_word_pos(text, pos)
                    } else {
                        prev_char_pos(text, pos)
                    };
                    if let Some(pos) = prev_pos {
                        self.cur.pos_skew = (pos, Skew::Righty);
                        return CmdResult::regular();
                    }
                }
            }
            if self.cur.block == self.root_block && self.cur.line == 1 {
                return CmdResult::nothing();
            }
            if self.cur.line > 0 {
                self.cur.line -= 1;
                self.cur.pos_skew = (
                    blocks[self.cur.block].max_pos(self.cur.line, blocks, nodes),
                    Skew::default());
                return CmdResult::regular();
            }

            let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
            self.cur.block = parent;
            self.cur.line = i;
            self.cur.pos_skew = (0, Skew::default());

            sel.anchor_path.push((sel.line, sel.pos));
            sel.line = i;
            sel.pos = 1;
        } else {
            loop {
                match blocks[self.cur.block].children[self.cur.line] {
                    BlockChild::Block(_) =>
                        if pos == 1 {
                            self.cur.pos_skew = (0, Skew::default());
                            break;
                        }
                    BlockChild::Leaf => {
                        let (node, line_idx) = blocks[self.cur.block]
                            .node_line_idx(self.cur.line, blocks).unwrap();
                        let text = &nodes[node].lines[line_idx].line.text();
                        let prev_pos = if ctrl {
                            prev_word_pos(text, pos)
                        } else {
                            prev_char_pos(text, pos)
                        };
                        if let Some(pos) = prev_pos {
                            self.cur.pos_skew = (pos, Skew::Righty);
                            break;
                        }                               
                    }
                }
                if self.cur.line > 0 {
                    self.cur.line -= 1;
                    self.cur.pos_skew = (
                        blocks[self.cur.block].max_pos(self.cur.line, blocks, nodes),
                        Skew::default());
                    break;
                }
                panic!("shouldn't happen when shrinking");
            }

            if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                if let Some((line, pos)) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[self.cur.line] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = blocks[self.cur.block].children.len() - 1;
                    self.cur.pos_skew = (
                        blocks[self.cur.block].max_pos(self.cur.line, blocks, nodes),
                        Skew::default());
                    sel.line = line;
                    sel.pos = pos;
                }
            }

            if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                self.cur.sel = None;
                // Necessary because maybe we were at the block boundary,
                // but didn't go inside because sel.anchor_path was cleared
                // by, say, alt_left() or alt_right().
                self.sink_cursor();
            }
        }

        CmdResult::regular()
    }

    pub fn shift_right(&mut self, ctrl: bool) -> CmdResult {
        let line = self.cur.line;
        let pos = self.cur.pos();
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
        });
        let blocks = &self.blocks;
        let nodes = &self.nodes;
        if (self.cur.line, pos) >= (sel.line, sel.pos) {
            match blocks[self.cur.block].children[self.cur.line] {
                BlockChild::Block(_) =>
                    if pos == 0 {
                        self.cur.pos_skew = (1, Skew::default());
                        return CmdResult::regular();
                    }
                BlockChild::Leaf => {
                    let (node, line_idx) = blocks[self.cur.block]
                        .node_line_idx(self.cur.line, blocks).unwrap();
                    let text = &nodes[node].lines[line_idx].line.text();
                    let next_pos = if ctrl {
                        next_word_pos(text, pos)
                    } else {
                        next_char_pos(text, pos)
                    };
                    if let Some(pos) = next_pos {
                        self.cur.pos_skew = (pos, Skew::Righty);
                        return CmdResult::regular();
                    }                        
                }
            }
            if self.cur.line + 1 < blocks[self.cur.block].children.len() {
                self.cur.line += 1;
                self.cur.pos_skew = (0, Skew::default());
                return CmdResult::regular();
            }
            if self.cur.block == self.root_block {
                if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                    self.cur.sel = None;
                }
                return CmdResult::regular();
            }

            let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
            self.cur.block = parent;
            self.cur.line = i;
            self.cur.pos_skew = (1, Skew::default());
            sel.anchor_path.push((sel.line, sel.pos));
            sel.line = i;
            sel.pos = 0;
        } else {
            loop {
                match blocks[self.cur.block].children[self.cur.line] {
                    BlockChild::Block(_) =>
                        if pos == 0 {
                            self.cur.pos_skew = (1, Skew::default());
                            break;
                        }
                    BlockChild::Leaf => {
                        let (node, line_idx) = blocks[self.cur.block]
                            .node_line_idx(self.cur.line, blocks).unwrap();
                        let text = &nodes[node].lines[line_idx].line.text();
                        let next_pos = if ctrl {
                            next_word_pos(text, pos)
                        } else {
                            next_char_pos(text, pos)
                        };
                        if let Some(pos) = next_pos {
                            self.cur.pos_skew = (pos, Skew::Lefty);
                            break;
                        }                               
                    }
                }
                if self.cur.line + 1 < blocks[self.cur.block].children.len() {
                    self.cur.line += 1;
                    self.cur.pos_skew = (0, Skew::default());
                    break;
                }
                panic!("shouldn't happen when shrinking");
            }

            if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                if let Some((line, pos)) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[self.cur.line] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = 0;
                    self.cur.pos_skew = (0, Skew::default());
                    sel.line = line;
                    sel.pos = pos;
                }
            }

            if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                self.cur.sel = None;
                // Necessary because maybe we were at the block boundary,
                // but didn't go inside because sel.anchor_path was cleared
                // by, say, alt_left() or alt_right().
                self.sink_cursor();
            }
        }

        CmdResult::regular()
    }

    pub fn up(&mut self) -> CmdResult {
        self.sink_cursor();

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let node = &nodes[node];
        let layout = node.line_layout(line_idx, &self.ctx);

        let eps = 3.0;
        let cc = layout.cursor_coord(self.cur.pos_skew);
        if cc.top - eps > 0.0 {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos_skew = layout.coords_to_pos(x, cc.top - eps);
        } else {
            let prev_leaf = prev_leaf((self.cur.block, self.cur.line), blocks);
            if let Some((prev_block, prev_idx)) = prev_leaf {
                let b = &blocks[prev_block];

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = b.child_size(prev_idx, &self.ctx, blocks, nodes).1 - eps;

                self.cur.line = prev_idx;
                self.cur.pos_skew = b.child_coords_to_pos(
                    prev_idx, (x, y),
                    &self.ctx, blocks, nodes);
                self.cur.block = prev_block;
            }
        }

        CmdResult {
            repaint: true,
            update_anchor_x: false,
            scroll_to_reveal_cursor: true,
            class: CmdClass::Other,
        }
    }

    pub fn down(&mut self) -> CmdResult {
        self.sink_cursor();

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let node = &nodes[node];
        let layout = node.line_layout(line_idx, &self.ctx);

        let eps = 3.0;
        let cc = layout.cursor_coord(self.cur.pos_skew);
        if cc.top + cc.height + eps < layout.height {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos_skew = layout.coords_to_pos(x, cc.top + cc.height + eps);
        } else {
            let next_leaf = next_leaf((self.cur.block, self.cur.line), blocks);
            if let Some((next_block, next_idx)) = next_leaf {
                let b = &blocks[next_block];

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = eps;

                self.cur.line = next_idx;
                self.cur.pos_skew = b.child_coords_to_pos(
                    next_idx, (x, y),
                    &self.ctx, blocks, nodes);
                self.cur.block = next_block;
            }
        }

        CmdResult {
            repaint: true,
            update_anchor_x: false,
            scroll_to_reveal_cursor: true,
            class: CmdClass::Other,
        }
    }

    pub fn shift_up(&mut self) -> CmdResult {
        let line = self.cur.line;
        let pos = self.cur.pos();
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
        });
        let blocks = &self.blocks;
        let nodes = &self.nodes;
        let eps = 3.0;

        let b = &blocks[self.cur.block];
        match b.children[self.cur.line] {
            BlockChild::Block(_) => {}
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
                let node = &nodes[node];
                let layout = node.line_layout(line_idx, &self.ctx);
                let cc = layout.cursor_coord(self.cur.pos_skew);
                if cc.top - eps > 0.0 {
                    let x = self.cur.anchor_x
                        - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                    self.cur.pos_skew = layout.coords_to_pos(x, cc.top - eps);
                    if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                        self.cur.sel = None;
                    }
                    return CmdResult {
                        repaint: true,
                        update_anchor_x: false,
                        scroll_to_reveal_cursor: true,
                        class: CmdClass::Other,
                    };
                }
            }
        }

        if self.cur.line <= sel.line {
            if self.cur.block == self.root_block && self.cur.line == 1 {
                self.cur.pos_skew = (0, Skew::default());
                if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                   self.cur.sel = None;
                }
                return CmdResult {
                    repaint: true,
                    update_anchor_x: false,
                    scroll_to_reveal_cursor: true,
                    class: CmdClass::Other,
                };                
            }
            if self.cur.line > 0 {
                let b = &blocks[self.cur.block];
                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = b.child_size(self.cur.line - 1, &self.ctx, blocks, nodes).1 - eps;
                self.cur.line -= 1;
                self.cur.pos_skew = b.child_coords_to_pos(
                    self.cur.line, (x, y),
                    &self.ctx, blocks, nodes);
                assert_ne!(self.cur.line, sel.line);
                return CmdResult {
                    repaint: true,
                    update_anchor_x: false,
                    scroll_to_reveal_cursor: true,
                    class: CmdClass::Other,
                };
            }

            let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
            assert!(i > 0);
            let pb = &blocks[parent];
            self.cur.block = parent;

            if parent == self.root_block && i == 1 {
                self.cur.line = 1;
                self.cur.pos_skew = (0, Skew::default());
            } else {
                self.cur.line = i - 1;
                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * pb.depth as f32);
                let y = pb.child_size(i - 1, &self.ctx, blocks, nodes).1 - eps;
                self.cur.pos_skew = pb.child_coords_to_pos(
                    i - 1, (x, y),
                    &self.ctx, blocks, nodes);                
            }

            sel.anchor_path.push((sel.line, sel.pos));
            sel.line = i;
            sel.pos = 1;
        } else {
            assert!(self.cur.line > sel.line);
            self.cur.line -= 1;
            loop {
                if sel.line < self.cur.line {
                    break;
                }
                assert_eq!(sel.line, self.cur.line);
                if let Some((line, pos)) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[self.cur.line] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = blocks[self.cur.block].children.len() - 1;
                    sel.line = line;
                    sel.pos = pos;
                } else {
                    break;
                }
            }
            let b = &blocks[self.cur.block];
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            let y = b.child_size(self.cur.line, &self.ctx, blocks, nodes).1 - eps;
            self.cur.pos_skew = b.child_coords_to_pos(
                self.cur.line, (x, y),
                &self.ctx, blocks, nodes);
            if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                self.cur.sel = None;
                // Necessary because maybe we were at the block boundary,
                // but didn't go inside because sel.anchor_path was cleared
                // by, say, alt_left() or alt_right().
                self.sink_cursor();
            }
        }
        CmdResult {
            repaint: true,
            update_anchor_x: false,
            scroll_to_reveal_cursor: true,
            class: CmdClass::Other,
        }
    }

    pub fn shift_down(&mut self) -> CmdResult {
        let line = self.cur.line;
        let pos = self.cur.pos();
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
        });
        let blocks = &self.blocks;
        let nodes = &self.nodes;
        let eps = 3.0;

        let b = &blocks[self.cur.block];
        match b.children[self.cur.line] {
            BlockChild::Block(_) => {}
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
                let node = &nodes[node];
                let layout = node.line_layout(line_idx, &self.ctx);
                let cc = layout.cursor_coord(self.cur.pos_skew);
                if cc.top + cc.height + eps < layout.height {
                    let x = self.cur.anchor_x
                        - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                    self.cur.pos_skew = layout.coords_to_pos(x, cc.top + cc.height + eps);
                    if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                        self.cur.sel = None;
                    }
                    return CmdResult {
                        repaint: true,
                        update_anchor_x: false,
                        scroll_to_reveal_cursor: true,
                        class: CmdClass::Other,
                    };
                }
            }
        }

        if self.cur.line >= sel.line {
            let b = &blocks[self.cur.block];
            if self.cur.line + 1 < b.children.len() {
                let b = &blocks[self.cur.block];
                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = eps;
                self.cur.line += 1;
                self.cur.pos_skew = b.child_coords_to_pos(
                    self.cur.line, (x, y),
                    &self.ctx, blocks, nodes);
                assert_ne!(self.cur.line, sel.line);
                return CmdResult {
                    repaint: true,
                    update_anchor_x: false,
                    scroll_to_reveal_cursor: true,
                    class: CmdClass::Other,
                };
            }

            let pb = loop {
                if self.cur.block == self.root_block {
                    let b = &blocks[self.cur.block];
                    self.cur.line = b.children.len() - 1;
                    self.cur.pos_skew = (
                        b.max_pos(self.cur.line, blocks, nodes),
                        Skew::default());
                    if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                       self.cur.sel = None;
                    }
                    return CmdResult {
                        repaint: true,
                        update_anchor_x: false,
                        scroll_to_reveal_cursor: true,
                        class: CmdClass::Other,
                    };
                }
                let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
                sel.anchor_path.push((sel.line, sel.pos));
                sel.line = i;
                sel.pos = 0;

                let pb = &blocks[parent];
                self.cur.block = parent;
                self.cur.line = i + 1;
                if self.cur.line < pb.children.len() {
                    break pb;
                }
            };

            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * pb.depth as f32);
            let y = eps;
            self.cur.pos_skew = pb.child_coords_to_pos(
                self.cur.line, (x, y),
                &self.ctx, blocks, nodes);
        } else {
            assert!(self.cur.line < sel.line);
            self.cur.line += 1;
            loop {
                if sel.line > self.cur.line {
                    break;
                }
                assert_eq!(sel.line, self.cur.line);
                if let Some((line, pos)) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[self.cur.line] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = 0;
                    sel.line = line;
                    sel.pos = pos;
                } else {
                    break;
                }
            }
            let b = &blocks[self.cur.block];
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            let y = eps;
            self.cur.pos_skew = b.child_coords_to_pos(
                self.cur.line, (x, y),
                &self.ctx, blocks, nodes);
            if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                self.cur.sel = None;
                // Necessary because maybe we were at the block boundary,
                // but didn't go inside because sel.anchor_path was cleared
                // by, say, alt_left() or alt_right().
                self.sink_cursor();
            }
        }
        CmdResult {
            repaint: true,
            update_anchor_x: false,
            scroll_to_reveal_cursor: true,
            class: CmdClass::Other,
        }
    }

    pub fn alt_left(&mut self) -> CmdResult {
        let blocks = &self.blocks;

        let mut shift_line = match self.cur.sel.as_ref() {
            Some(sel) => self.cur.line.min(sel.line),
            None => self.cur.line,
        };
        let mut cur_block = self.cur.block;
        let stepped_up = if shift_line == 0 {
            match blocks[cur_block].parent_idx {
                Some((p, i)) => {
                    cur_block = p;
                    shift_line = i;
                }
                None => panic!(),
            }
            true
        } else { false };

        assert!(shift_line > 0);

        let (parent_block, pos_in_parent) = match blocks[cur_block].parent_idx {
            Some((p, i)) => (p, i),
            None => return CmdResult::nothing(),
        };

        let cur_node = blocks[cur_block].node;
        let parent_node = blocks[parent_block].node;
        if cur_node == parent_node {
            println!("TODO: alt_left for self-referencing node");
            return CmdResult::nothing();
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        let lines = splice_node_lines(
            cur_node, shift_line - 1, nodes[cur_node].lines.len(),
            vec![],
            blocks, cblocks, nodes,
            &mut undo_group.edits,
            &mut self.unsaved);

        assert!(pos_in_parent > 0);
        splice_node_lines(
            parent_node, pos_in_parent - 1 + 1, pos_in_parent - 1 + 1,
            lines,
            blocks, cblocks, nodes,
            &mut undo_group.edits,
            &mut self.unsaved);

        if stepped_up {
            let child_block = match blocks[parent_block].children[pos_in_parent + 1] {
                BlockChild::Leaf => panic!(),
                BlockChild::Block(b) => b,
            };
            self.cur.block = child_block;
            let mut need_expand = self.cur.line > 0;
            if let Some(sel) = self.cur.sel.as_mut() {
                if sel.line > 0 {
                    need_expand = true;
                }
                sel.anchor_path.clear();
            }
            if need_expand {
                expand_block(child_block, blocks, cblocks, nodes, &mut self.unsaved);
            }
        } else {
            self.cur.block = parent_block;
            self.cur.line = self.cur.line - shift_line + pos_in_parent + 1;
            if let Some(sel) = self.cur.sel.as_mut() {
                sel.line = sel.line - shift_line + pos_in_parent + 1;
                sel.anchor_path.clear();
            }
        }
        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        CmdResult::regular()
    }

    pub fn alt_right(&mut self) -> CmdResult {
        let blocks = &self.blocks;

        let (mut first_shift_line, mut last_shift_line) = match self.cur.sel.as_ref() {
            Some(sel) => (self.cur.line.min(sel.line), self.cur.line.max(sel.line)),
            None => (self.cur.line, self.cur.line),
        };
        let mut cur_block = self.cur.block;
        let mut pos_in_parent = None;
        let stepped_up = if first_shift_line == 0 {
            match blocks[cur_block].parent_idx {
                Some((p, i)) => {
                    cur_block = p;
                    first_shift_line = i;
                    last_shift_line = i;
                    pos_in_parent = Some(i);
                }
                None => panic!(),
            }
            true
        } else { false };

        assert!(first_shift_line > 0);
        let inner_line = (1..first_shift_line).rev()
            .find(|&i| match blocks[cur_block].children[i] {
                BlockChild::Leaf => false,
                BlockChild::Block(_) => true,
            });
        let inner_line = match inner_line {
            Some(c) => c,
            None => {
                return CmdResult::nothing();
            }
        };
        first_shift_line = inner_line + 1;

        let inner_block = match blocks[cur_block].children[inner_line] {
            BlockChild::Leaf => panic!(),
            BlockChild::Block(b) => b,
        };

        let cur_node = blocks[cur_block].node;
        let inner_node = blocks[inner_block].node;
        if cur_node == inner_node {
            println!("alt-right for self-referencing node");
            return CmdResult::nothing();
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());
        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        if !blocks[inner_block].is_expanded() {
            // TODO: silent autoexpand if it's one-line node
            expand_block(inner_block, blocks, cblocks, nodes, &mut self.unsaved);
            return CmdResult::regular();
        }

        let num_inner_lines = blocks[inner_block].children.len();

        let lines = splice_node_lines(cur_node,
            first_shift_line - 1, last_shift_line + 1 - 1, vec![],
            blocks, cblocks, nodes,
            &mut undo_group.edits,
            &mut self.unsaved);

        splice_node_lines(inner_node,
            num_inner_lines - 1, num_inner_lines - 1, lines,
            blocks, cblocks, nodes,
            &mut undo_group.edits,
            &mut self.unsaved);

        if stepped_up {
            let pos_in_parent = pos_in_parent.unwrap();
            let i = pos_in_parent - first_shift_line + num_inner_lines;
            let child_block = match blocks[inner_block].children[i] {
                BlockChild::Leaf => panic!(),
                BlockChild::Block(b) => b,
            };
            self.cur.block = child_block;
            let mut need_expand = self.cur.line > 0;
            if let Some(sel) = self.cur.sel.as_mut() {
                if sel.line > 0 {
                    need_expand = true;
                }
                sel.anchor_path.clear();
            }
            if need_expand {
                expand_block(child_block, blocks, cblocks, nodes, &mut self.unsaved);
            }
        } else {
            self.cur.block = inner_block;
            self.cur.line = self.cur.line - first_shift_line + num_inner_lines;
            if let Some(sel) = self.cur.sel.as_mut() {
                sel.line = sel.line - first_shift_line + num_inner_lines;
                sel.anchor_path.clear();
            }
        }

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        CmdResult::regular()
    }

    pub fn alt_up(&mut self) -> CmdResult {
        let block;
        let first_line;
        let last_line;
        let stepped_up;
        match self.cur.sel.as_ref() {
            Some(sel) => {
                if self.cur.line == 0 || sel.line == 0 {
                    (block, first_line) = self.blocks[self.cur.block].parent_idx.unwrap();
                    last_line = first_line + 1;
                    stepped_up = true;
                } else {
                    block = self.cur.block;
                    first_line = self.cur.line.min(sel.line);
                    let last_line_pos =
                        (self.cur.line, self.cur.pos()).max((sel.line, sel.pos));
                    last_line = if last_line_pos.1 == 0 {
                        last_line_pos.0
                    } else {
                        last_line_pos.0 + 1
                    };
                    stepped_up = false;
                }
            }
            None => {
                if self.cur.line == 0 {
                    (block, first_line) = self.blocks[self.cur.block].parent_idx.unwrap();
                    last_line = first_line + 1;
                    stepped_up = true;
                } else {
                    block = self.cur.block;
                    first_line = self.cur.line;
                    last_line = self.cur.line + 1;
                    stepped_up = false;
                }
            }
        }

        let mut pb = block;
        while let Some((p, _)) = self.blocks[pb].parent_idx {
            pb = p;
            if pb == block {
                println!("TODO: alt_up() with cycles");
                return CmdResult::nothing();
            }
        }

        assert!(first_line > 0);
        if first_line == 1 {
            return CmdResult::nothing();
        }
        assert!(first_line > 1);
        assert!(first_line < last_line);
        assert!(last_line <= self.blocks[block].children.len());

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());
        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;
        let cblocks = &mut self.cblocks;

        let node = blocks[block].node;

        let moved_line = splice_node_lines(
            node,
            first_line - 1 - 1, first_line - 1,
            vec![],
            blocks, cblocks, nodes,
            &mut undo_group.edits, &mut self.unsaved);
        let last_line = last_line - 1;
        splice_node_lines(
            node,
            last_line - 1, last_line - 1,
            moved_line,
            blocks, cblocks, nodes,
            &mut undo_group.edits, &mut self.unsaved);

        if stepped_up {
            // cursor will be in the right place automatically
        } else {
            assert!(self.cur.line > 0);
            self.cur.line -= 1;
            if let Some(sel) = self.cur.sel.as_mut() {
                assert!(sel.line > 0);
                sel.line -= 1;
            }
        }

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();

        CmdResult::regular()
    }

    pub fn alt_down(&mut self) -> CmdResult {
        let block;
        let first_line;
        let last_line;
        let stepped_up;
        match self.cur.sel.as_ref() {
            Some(sel) => {
                if self.cur.line == 0 || sel.line == 0 {
                    (block, first_line) = self.blocks[self.cur.block].parent_idx.unwrap();
                    last_line = first_line + 1;
                    stepped_up = true;
                } else {
                    block = self.cur.block;
                    first_line = self.cur.line.min(sel.line);
                    let last_line_pos =
                        (self.cur.line, self.cur.pos()).max((sel.line, sel.pos));
                    last_line = if last_line_pos.1 == 0 {
                        last_line_pos.0
                    } else {
                        last_line_pos.0 + 1
                    };
                    stepped_up = false;
                }
            }
            None => {
                if self.cur.line == 0 {
                    (block, first_line) = self.blocks[self.cur.block].parent_idx.unwrap();
                    last_line = first_line + 1;
                    stepped_up = true;
                } else {
                    block = self.cur.block;
                    first_line = self.cur.line;
                    last_line = self.cur.line + 1;
                    stepped_up = false;
                }
            }
        }

        let mut pb = block;
        while let Some((p, _)) = self.blocks[pb].parent_idx {
            pb = p;
            if pb == block {
                println!("TODO: alt_down() with cycles");
                return CmdResult::nothing();
            }
        }

        assert!(first_line >= 1);
        assert!(first_line < last_line);
        assert!(last_line <= self.blocks[block].children.len());
        if last_line == self.blocks[block].children.len() {
            return CmdResult::nothing();
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());
        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;
        let cblocks = &mut self.cblocks;

        let node = blocks[block].node;

        let moved_line = splice_node_lines(
            node,
            last_line - 1, last_line - 1 + 1,
            vec![],
            blocks, cblocks, nodes,
            &mut undo_group.edits, &mut self.unsaved);
        splice_node_lines(
            node,
            first_line - 1, first_line - 1,
            moved_line,
            blocks, cblocks, nodes,
            &mut undo_group.edits, &mut self.unsaved);

        if stepped_up {
            // cursor will be in the right place automatically
        } else {
            self.cur.line += 1;
            if let Some(sel) = self.cur.sel.as_mut() {
                sel.line += 1;

                if self.cur.line == blocks[block].children.len() {
                    assert_eq!(self.cur.pos_skew.0, 0);
                    self.cur.line -= 1;
                    self.cur.pos_skew = (
                        blocks[block].max_pos(self.cur.line, blocks, nodes),
                        Skew::default());
                }

                if sel.line == blocks[block].children.len() {
                    assert_eq!(sel.pos, 0);
                    sel.line -= 1;
                    sel.pos = blocks[block].max_pos(sel.line, blocks, nodes);
                }

                assert!(sel.line < blocks[block].children.len());

                if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
                    self.cur.sel = None;
                }
            }
            assert!(self.cur.line < blocks[block].children.len());
        }

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();

        CmdResult::regular()
    }

    pub fn home(&mut self) -> CmdResult {
        self.sink_cursor();

        let blocks = &self.blocks;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let layout = self.nodes[node].line_layout(line_idx, &self.ctx);
        let cc = layout.cursor_coord(self.cur.pos_skew);

        self.cur.pos_skew = layout.coords_to_pos(0.0, cc.top + 0.5 * cc.height);

        CmdResult::regular()
    }

    pub fn end(&mut self) -> CmdResult {
        self.sink_cursor();

        let blocks = &self.blocks;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let layout = self.nodes[node].line_layout(line_idx, &self.ctx);
        let cc = layout.cursor_coord(self.cur.pos_skew);

        self.cur.pos_skew = layout.coords_to_pos(MAX_WIDTH + 100.0, cc.top + 0.5 * cc.height);

        CmdResult::regular()
    }

    pub fn shift_home(&mut self) -> CmdResult {
        let line = self.cur.line;
        let pos = self.cur.pos();
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
        });

        let blocks = &self.blocks;

        let b = &blocks[self.cur.block];
        match b.children[self.cur.line] {
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
                let layout = self.nodes[node].line_layout(line_idx, &self.ctx);
                let cc = layout.cursor_coord(self.cur.pos_skew);

                self.cur.pos_skew = layout.coords_to_pos(0.0, cc.top + 0.5 * cc.height);
            },
            BlockChild::Block(_) => {
                self.cur.pos_skew = (0, Skew::default());
            },
        }

        if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
            self.cur.sel = None;
            self.sink_cursor();
        }
        CmdResult::regular()
    }

    pub fn shift_end(&mut self) -> CmdResult {
        let line = self.cur.line;
        let pos = self.cur.pos();
        let sel = self.cur.sel.get_or_insert_with(|| Sel {
            line,
            pos,
            anchor_path: vec![],
        });

        let blocks = &self.blocks;

        let b = &blocks[self.cur.block];
        match b.children[self.cur.line] {
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
                let layout = self.nodes[node].line_layout(line_idx, &self.ctx);
                let cc = layout.cursor_coord(self.cur.pos_skew);

                self.cur.pos_skew = layout.coords_to_pos(MAX_WIDTH + 100.0, cc.top + 0.5 * cc.height);
            },
            BlockChild::Block(_) => {
                self.cur.pos_skew = (1, Skew::default());
            },
        }

        if self.cur.line == sel.line && self.cur.pos_skew.0 == sel.pos {
            self.cur.sel = None;
            self.sink_cursor();
        }
        CmdResult::regular()
    }

    pub fn put_char(&mut self, c: char) -> CmdResult {
        if self.cur.sel.is_some() {
            return self.replace_selection_with(vec![Line::Text {
                text: c.to_string(),
                monospace: false,
            }]);
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &self.blocks;
        let nodes = &mut self.nodes;

        let (node, line_idx) = blocks[self.cur.block].node_line_idx(self.cur.line, blocks).unwrap();
        let text = nodes[node].lines[line_idx].line.text();

        if c == ' ' && self.cur.line > 0 && self.cur.pos() == 1 && text.starts_with('*') {
            let local_header = text[1..].to_owned();
            let blocks = &mut self.blocks;
            let new_node = create_empty_node(
                nodes, &mut self.db_key_to_node_key, &mut self.unsaved);
            splice_node_lines(
                node, line_idx, line_idx + 1,
                vec![Line::Node { local_header, node: new_node }],
                blocks, &mut self.cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);
            let new_block = match blocks[self.cur.block].children[self.cur.line] {
                BlockChild::Leaf => unreachable!(),
                BlockChild::Block(b) => b,
            };
            assert_eq!(blocks[new_block].node, new_node);
            self.cur.block = new_block;
            self.cur.line = 0;
            self.cur.pos_skew = (0, Skew::default());
            self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
            self.redo_buf.clear();
            return CmdResult::regular();
        }

        splice_line_text(
            node,
            line_idx, self.cur.pos(), self.cur.pos(),
            &c.to_string(),
            nodes, &mut undo_group.edits,
            &mut self.unsaved);
        self.cur.pos_skew = (self.cur.pos() + c.len_utf8(), Skew::Righty);
        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        let mut res = CmdResult::regular();
        if c == ' ' {
            if self.last_cmd_class == CmdClass::PutWhitespace {
                self.merge_undo_groups();
            }
            res.class = CmdClass::PutWhitespace;
        } else {
            if self.last_cmd_class == CmdClass::PutWhitespace ||
               self.last_cmd_class == CmdClass::PutChar {
                self.merge_undo_groups();
            }
            res.class = CmdClass::PutChar;
        }
        res
    }

    pub fn enter(&mut self) -> CmdResult {
        if self.cur.sel.is_some() {
            return self.replace_selection_with(vec![
                Line::Text { text: String::new(), monospace: false },
                Line::Text { text: String::new(), monospace: false },
            ]);
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        if self.cur.line == 0 && !blocks[self.cur.block].is_expanded() {
            expand_block(self.cur.block, blocks, cblocks, nodes, &mut self.unsaved);
        }

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let end_pos = nodes[node].lines[line_idx].line.text().len();

        let tail = splice_line_text(
            node, line_idx,
            self.cur.pos(), end_pos,
            "",
            nodes, &mut undo_group.edits,
            &mut self.unsaved);

        if self.cur.line == 0 {
            splice_node_lines(b.node, 0, 0, 
                vec![Line::Text { text: tail, monospace: false }],
                blocks, cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);
        } else {
            let monospace = match nodes[node].lines[line_idx].line {
                Line::Text { monospace, .. } => monospace,
                Line::Node { .. } => panic!(),
            };
            splice_node_lines(b.node, line_idx + 1, line_idx + 1,
                vec![Line::Text { text: tail, monospace }],
                blocks, cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);
        }
        self.cur.line += 1;
        self.cur.pos_skew = (0, Skew::default());

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        if self.last_cmd_class == CmdClass::PutWhitespace {
            self.merge_undo_groups();
        }
        let mut res = CmdResult::regular();
        res.class = CmdClass::PutWhitespace;
        res
    }

    pub fn alt_enter(&mut self) -> CmdResult {
        if self.cur.sel.is_some() {
            // TODO
            return CmdResult::nothing();
        }
        if self.cur.line != 0 {
            // TODO
            return CmdResult::nothing();
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;

        let b = &blocks[self.cur.block];

        // let expanded = b.is_expanded() || nodes[b.node].lines.is_empty();

        let new_node = create_empty_node(
            nodes, &mut self.db_key_to_node_key, &mut self.unsaved);

        let (parent_block, idx_in_parent) = b.parent_idx.unwrap();
        assert!(idx_in_parent > 0);

        if self.cur.pos() == b.max_pos(0, blocks, nodes) &&
           (!b.is_expanded() || nodes[b.node].lines.is_empty()) {
            // There is nothing after the cursor in this block.
            // Create new bullet after.
            splice_node_lines(
                blocks[parent_block].node,
                idx_in_parent - 1 + 1, idx_in_parent - 1 + 1,
                vec![Line::Node { local_header: String::new(), node: new_node }],
                blocks, &mut self.cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);
            self.cur.block = match blocks[parent_block].children[idx_in_parent + 1] {
                BlockChild::Leaf => panic!(),
                BlockChild::Block(b) => b,
            };
            self.cur.line = 0;
            self.cur.pos_skew = (0, Skew::default());
        } else {
            // Create new bullet before.
            let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
            let prefix = splice_line_text(node, line_idx,
                0, self.cur.pos(), "",
                nodes,
                &mut undo_group.edits,
                &mut self.unsaved);
            splice_node_lines(
                blocks[parent_block].node,
                idx_in_parent - 1, idx_in_parent - 1,
                vec![Line::Node { local_header: prefix, node: new_node }],
                blocks, &mut self.cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);
            self.cur.pos_skew = (0, Skew::default());
        }

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        CmdResult::regular()
    }

    pub fn backspace(&mut self) -> CmdResult {
        if self.cur.sel.is_some() {
            return self.replace_selection_with(vec![
                Line::Text { text: String::new(), monospace: false }]);
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let text = nodes[node].lines[line_idx].line.text();

        if let Some(prev_pos) = prev_char_pos(text, self.cur.pos()) {
            splice_line_text(node, line_idx,
                prev_pos, self.cur.pos(), "",
                nodes, &mut undo_group.edits, &mut self.unsaved);
            self.cur.pos_skew = (prev_pos, Skew::Righty);
            self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
            self.redo_buf.clear();
            if self.last_cmd_class == CmdClass::BackspaceChar ||
               self.last_cmd_class == CmdClass::BackspaceNewLine {
                self.merge_undo_groups();
            }
            let mut res = CmdResult::regular();
            res.class = CmdClass::BackspaceChar;
            return res;
        }

        if self.cur.line == 0 {
            let (parent_block, idx_in_parent) = b.parent_idx.unwrap();
            let mut ancestor = parent_block;
            loop {
                if blocks[ancestor].node == b.node {
                    self.cur.block = ancestor;
                    return CmdResult::regular();
                }
                ancestor = match blocks[ancestor].parent_idx {
                    None => break,
                    Some(x) => x.0,
                };
            }

            if !b.is_expanded() {
                // TODO: silent autoexpand if it's one-line node
                expand_block(self.cur.block, blocks, cblocks, nodes, &mut self.unsaved);
                return CmdResult::regular();
            }

            let mut lines = splice_node_lines(
                blocks[self.cur.block].node,
                0, nodes[blocks[self.cur.block].node].lines.len(),
                vec![],
                blocks, cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);

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
                blocks, cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);
            self.cur.block = parent_block;
            self.cur.line = idx_in_parent;
            self.cur.pos_skew = (0, Skew::default());
            self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
            self.redo_buf.clear();
            return CmdResult::regular();            
        }

        let pl = prev_leaf((self.cur.block, self.cur.line), blocks);
        let (prev_block, prev_idx) = match pl {
            Some(x) => x,
            None => return CmdResult::nothing(),
        };

        if text.is_empty() {
            splice_node_lines(node,
                line_idx, line_idx + 1, vec![],
                blocks, cblocks, nodes,
                &mut undo_group.edits,
                &mut self.unsaved);

            // Recompute prev leaf in case it's self-referencing node:
            // * rec
            //   * rec
            //     + rec
            //     <empty>
            //   |<empty>
            let pl = prev_leaf((self.cur.block, self.cur.line), blocks);
            let (prev_block, prev_idx) = match pl {
                Some(x) => x,
                None => panic!(),
            };

            self.cur.block = prev_block;
            self.cur.line = prev_idx;
            self.cur.pos_skew = (
                blocks[prev_block].max_pos(prev_idx, blocks, nodes),
                Skew::default());

            self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
            self.redo_buf.clear();
            return CmdResult::regular();
        }

        #[allow(clippy::collapsible_if)]
        if blocks[prev_block].depth > b.depth {
            if prev_idx == 0 && !blocks[prev_block].is_expanded() {
                // TODO: silent autoexpand if it's one-line node
                expand_block(prev_block, blocks, cblocks, nodes, &mut self.unsaved);
                return CmdResult::regular();
            }
            if blocks[prev_block].node == blocks[self.cur.block].node {
                self.cur.block = prev_block;
                self.cur.line = prev_idx;
                self.cur.pos_skew = (
                    blocks[prev_block].max_pos(prev_idx, blocks, nodes),
                    Skew::default());
                return CmdResult::regular();
            }
        }

        let text = text.to_owned();
        let (prev_node, prev_line_idx) = blocks[prev_block].node_line_idx(prev_idx, blocks).unwrap();

        let prev_text_len = nodes[prev_node].lines[prev_line_idx].line.text().len();
        splice_line_text(prev_node, prev_line_idx,
            prev_text_len, prev_text_len, &text,
            nodes, &mut undo_group.edits, &mut self.unsaved);

        self.cur.block = prev_block;
        self.cur.line = prev_idx;
        self.cur.pos_skew = (prev_text_len, Skew::default());
        splice_node_lines(
            node, line_idx, line_idx + 1, vec![],
            blocks, cblocks, nodes,
            &mut undo_group.edits,
            &mut self.unsaved);

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        let mut res = CmdResult::regular();
        res.class = CmdClass::BackspaceNewLine;
        res
    }

    pub fn del(&mut self) -> CmdResult {
        if self.cur.sel.is_some() {
            return self.replace_selection_with(vec![
                Line::Text { text: String::new(), monospace: false }]);
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let text = nodes[node].lines[line_idx].line.text();

        if let Some(next_pos) = next_char_pos(text, self.cur.pos()) {
            splice_line_text(node, line_idx,
                self.cur.pos(), next_pos, "",
                nodes, &mut undo_group.edits, &mut self.unsaved);
            self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
            self.redo_buf.clear();
            if self.last_cmd_class == CmdClass::DelChar {
                self.merge_undo_groups();
            }
            let mut res = CmdResult::regular();
            res.class = CmdClass::DelChar;
            return res;
        }

        // TODO        
        CmdResult::nothing()
    }

    pub fn toggle_monospace(&mut self) -> CmdResult {
        let first_line;
        let last_line;
        match self.cur.sel.as_ref() {
            Some(sel) => {
                first_line = self.cur.line.min(sel.line);
                let last_line_pos =
                    (self.cur.line, self.cur.pos()).max((sel.line, sel.pos));
                last_line = if last_line_pos.1 == 0 {
                    last_line_pos.0
                } else {
                    last_line_pos.0 + 1
                };
            }
            None => {
                first_line = self.cur.line;
                last_line = self.cur.line + 1;
            }
        }

        let first_line = first_line.max(1);
        let last_line = last_line.max(1);

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let mut has_proportional = false;
        let mut has_monospace = false;
        for i in first_line..last_line {
            let b = &blocks[self.cur.block];
            match b.children[i] {
                BlockChild::Leaf => {}
                BlockChild::Block(_) => continue,
            }
            let (node, line_idx) = b.node_line_idx(i, blocks).unwrap();
            let line = &nodes[node].lines[line_idx].line;
            match line {
                Line::Text { monospace: false, .. } => has_proportional = true,
                Line::Text { monospace: true, .. } => has_monospace = true,
                Line::Node { .. } => {}
            };
        }

        if !has_monospace && !has_proportional {
            return CmdResult::nothing();
        }

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let nodes = &mut self.nodes;

        let monospace = has_proportional;

        for i in first_line..last_line {
            let b = &blocks[self.cur.block];
            match b.children[i] {
                BlockChild::Leaf => {}
                BlockChild::Block(_) => continue,
            }
            let (node, line_idx) = b.node_line_idx(i, blocks).unwrap();
            let line = &nodes[node].lines[line_idx].line;
            match line {
                Line::Text { text, monospace: _ } => {
                    let new_line = Line::Text {
                        text: text.to_owned(),
                        monospace,
                    };
                    // TODO: this is potentially quadratic
                    splice_node_lines(
                        node,
                        line_idx, line_idx + 1, vec![new_line],
                        blocks, &mut self.cblocks, nodes,
                        &mut undo_group.edits,
                        &mut self.unsaved);
                }
                Line::Node {..} => {}
            };
        }

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        CmdResult::regular()
    }

    pub fn tab(&mut self) -> CmdResult {
        if self.cur.sel.is_some() {
            return CmdResult::nothing();  // TODO?
        }
        if self.cur.line != 0 {
            return CmdResult::nothing();
        }
        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;
        if blocks[self.cur.block].is_expanded() {
            self.collapse_block(self.cur.block)
        } else {
            expand_block(self.cur.block, blocks, cblocks, nodes, &mut self.unsaved);
            CmdResult::regular()
        }
    }

    pub fn ctrl_tab(&mut self, shift: bool) -> CmdResult {
        if self.cur.sel.is_some() {
            return CmdResult::nothing();  // TODO?
        }
        if self.cur.line != 0 {
            return CmdResult::nothing();
        }
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let orig_cc = blocks[self.cur.block].abs_cursor_coord(
            self.cur.line, (0, Skew::default()),
            &self.ctx, blocks, nodes);

        let mut candidates = vec![];

        let node = blocks[self.cur.block].node;
        for &parent in nodes[node].parents.keys() {
            if !reachable_from(blocks[self.root_block].node, parent, nodes) {
                continue;
            }
            for (i, line) in nodes[parent].lines.iter().enumerate() {
                match line.line {
                    Line::Text { .. } => {}
                    Line::Node { node: n, .. } =>
                        if n == node {
                            candidates.push((parent, i + 1));
                        }
                }
            }
        }

        // Find next candidaty in the cycle.
        candidates.sort_by_key(|&(node, line)| (nodes[node].db_key, line));
        if shift {
            candidates.reverse();
        }
        let (parent_block, idx) = blocks[self.cur.block].parent_idx.unwrap();
        let parent_node = blocks[parent_block].node;
        let pos = candidates.iter().position(|&t| t == (parent_node, idx)).unwrap();
        let (parent_node, line) = candidates[(pos + 1) % candidates.len()];

        // Find expanded ancestor block nearest to the candidate.
        let mut visited = FnvHashMap::default();
        visited.insert(parent_node, None);
        let mut frontier = vec![parent_node];
        let (mut block, mut node) = 'outer: loop {
            assert!(!frontier.is_empty());
            let mut next_frontier = vec![];
            for u in frontier {
                for &block in &nodes[u].blocks {
                    if blocks[block].is_expanded() {
                        break 'outer (block, u);
                    }
                }
                for &p in nodes[u].parents.keys() {
                    visited.entry(p).or_insert_with(|| {
                        next_frontier.push(p);
                        Some(u)
                    });
                }
            }
            frontier = next_frontier;
        };

        // Expand all blocks on the path to the candidate.
        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;
        while let Some(next_node) = visited[&node] {
            assert_eq!(blocks[block].node, node);
            let next_block = blocks[block].children.iter().find_map(|child| {
                match *child {
                    BlockChild::Leaf => None,
                    BlockChild::Block(b) => if blocks[b].node == next_node {
                        Some(b)
                    } else {
                        None
                    }
                }
            }).unwrap();
            if !blocks[next_block].is_expanded() {
                expand_block(next_block, blocks, cblocks, nodes, &mut self.unsaved);
            }
            block = next_block;
            node = next_node;
        }

        assert_eq!(blocks[block].node, parent_node);
        self.cur.block = match blocks[block].children[line] {
            BlockChild::Leaf => panic!(),
            BlockChild::Block(b) => b,
        };
        self.cur.line = 0;
        self.cur.pos_skew = (0, Skew::default());

        let new_cc = blocks[self.cur.block].abs_cursor_coord(
            self.cur.line, (0, Skew::default()),
            &self.ctx, blocks, nodes);

        self.y_offset -= new_cc.top - orig_cc.top;
        self.clamp_y_offset();

        CmdResult::regular()
    }

    pub fn copy(&mut self) -> (Vec<Line>, String) {
        let sel = match self.cur.sel.as_ref() {
            Some(sel) => sel,
            None => {
                let lines = vec![Line::Text { text: String::new(), monospace: false }];
                return (lines, String::new());
                // TODO: copy whole line/node?
            }
        };
        let (line1, pos1) = (self.cur.line, self.cur.pos()).min((sel.line, sel.pos));
        let (line2, pos2) = (self.cur.line, self.cur.pos()).max((sel.line, sel.pos));

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

    pub fn paste(&mut self, lines: Vec<Line>) -> CmdResult {
        self.replace_selection_with(lines)
    }

    pub fn replace_selection_with(&mut self, lines: Vec<Line>) -> CmdResult {
        let cur_line_pos = (self.cur.line, self.cur.pos());
        let sel_line_pos = match self.cur.sel.as_ref() {
            Some(sel) => (sel.line, sel.pos),
            None => cur_line_pos,
        };
        let (line1, pos1) = cur_line_pos.min(sel_line_pos);
        let (line2, pos2) = cur_line_pos.max(sel_line_pos);

        let blocks = &self.blocks;
        let nodes = &self.nodes;

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let mut p = self.cur.block;
        while let Some((parent, idx)) = blocks[p].parent_idx {
            if blocks[parent].node == blocks[self.cur.block].node &&
               line1 <= idx && idx <= line2 {
                self.cur.block = parent;
                return CmdResult::regular();
            }
            p = parent;
        }

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
        self.cur.pos_skew = (cur_line_pos.1, Skew::default());
        self.cur.sel = None;

        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
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
            let header_len = match nodes[node_key].lines[node_line].line {
                Line::Text {..} => panic!(),
                Line::Node { ref local_header, .. } => local_header.len(),
            };
            splice_line_text(node_key, node_line,
                0, header_len, &new_header_text,
                nodes, &mut undo_group.edits, &mut self.unsaved);
            if self.cur.line > 0 && !blocks[self.cur.block].is_expanded() {
                expand_block(self.cur.block, blocks, cblocks, nodes, &mut self.unsaved);
            }
            line1 + 1
        } else {
            line1
        };

        let node_key = blocks[self.cur.block].node;
        splice_node_lines(
            node_key,
            line1 - 1, line2 + 1 - 1, new_lines,
            blocks, cblocks, nodes,
            &mut undo_group.edits,
            &mut self.unsaved);

        self.sink_cursor();

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        CmdResult::regular()
    }

    fn undo_or_redo(&mut self, is_undo: bool) -> CmdResult {
        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        let buf1 = if is_undo { &mut self.undo_buf } else { &mut self.redo_buf };
        if let Some(mut g) = buf1.pop() {
            let mut redo_group = UndoGroupBuilder::new(g.cur_after);
            while let Some(edit) = g.edits.pop() {
                apply_edit(edit,
                    blocks, cblocks, nodes,
                    &mut redo_group.edits,
                    &mut self.unsaved);
            }
            self.set_cur_to_waypoint(&g.cur_before);
            let redo_group = redo_group.finish(g.cur_before);
            if is_undo {
                self.redo_buf.push(redo_group);
            } else {
                self.undo_buf.push(redo_group);
            }
            CmdResult::regular()
        } else {
            CmdResult::nothing()
        }
    }

    pub fn undo(&mut self) -> CmdResult {
        self.undo_or_redo(true)
    }

    pub fn redo(&mut self) -> CmdResult {
        self.undo_or_redo(false)
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
