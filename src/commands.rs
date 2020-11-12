use super::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CmdClass {
    PutChar,
    PutWhitespace,
    BackspaceChar,
    BackspaceNewLine,
    Other,
}

#[must_use]
pub struct CmdResult {
    pub repaint: bool,
    pub update_anchor_x: bool,
    pub class: CmdClass,
}

impl CmdResult {
    pub fn nothing() -> Self {
        CmdResult {
            repaint: false,
            update_anchor_x: false,
            class: CmdClass::Other,
        }
    }
    pub fn regular() -> Self {
        CmdResult {
            repaint: true,
            update_anchor_x: true,
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
    pub fn scroll(&mut self, delta: f32) -> CmdResult {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        // TODO: use actual line height
        self.y_offset += delta * 18.0;
        self.y_offset = self.y_offset.min(10.0);
        let height = blocks[self.root_block].size(&self.ctx, blocks, nodes).1;
        let max_offset = 10.0 - height
            + blocks[self.root_block].last_line_height(&self.ctx, blocks, nodes);
        self.y_offset = self.y_offset.max(max_offset);

        CmdResult {
            repaint: true,
            update_anchor_x: false,
            class: CmdClass::Other,
        }
    }

    pub fn collapse_block(&mut self, block: BlockKey) -> CmdResult {
        let blocks = &self.blocks;
        let nodes = &self.nodes;

        assert!(blocks[block].is_expanded());
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

        CmdResult::regular()
    }

    pub fn left(&mut self) -> CmdResult {
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
                    return CmdResult::regular();
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

        CmdResult::regular()
    }

    pub fn right(&mut self) -> CmdResult {
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
                    return CmdResult::regular();
                }
                let leaf = (self.cur.block, self.cur.line);
                if let Some((next_block, next_line)) = next_leaf(leaf, blocks) {
                    self.cur.line = next_line;
                    self.cur.pos = 0;
                    self.cur.block = next_block;
                }
            }
        }

        CmdResult::regular()
    }

    pub fn shift_left(&mut self) -> CmdResult {
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
                        return CmdResult::regular();
                    }
                BlockChild::Leaf => {
                    let (node, line_idx) = blocks[self.cur.block]
                        .node_line_idx(self.cur.line, blocks).unwrap();
                    let text = &nodes[node].lines[line_idx].line.text();
                    if let Some(pos) = prev_char_pos(text, self.cur.pos) {
                        self.cur.pos = pos;
                        return CmdResult::regular();
                    }                        
                }
            }
            if self.cur.block == self.root_block && self.cur.line == 1 {
                return CmdResult::nothing();
            }
            if self.cur.line > 0 {
                self.cur.line -= 1;
                self.cur.pos = blocks[self.cur.block].max_pos(self.cur.line, blocks, nodes);
                return CmdResult::regular();
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

        CmdResult::regular()
    }

    pub fn shift_right(&mut self) -> CmdResult {
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
                        return CmdResult::regular();
                    }
                BlockChild::Leaf => {
                    let (node, line_idx) = blocks[self.cur.block]
                        .node_line_idx(self.cur.line, blocks).unwrap();
                    let text = &nodes[node].lines[line_idx].line.text();
                    if let Some(pos) = next_char_pos(text, self.cur.pos) {
                        self.cur.pos = pos;
                        return CmdResult::regular();
                    }                        
                }
            }
            if self.cur.line + 1 < blocks[self.cur.block].children.len() {
                self.cur.line += 1;
                self.cur.pos = 0;
                return CmdResult::regular();
            }
            if self.cur.block == self.root_block {
                return CmdResult::regular();
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
        let cc = layout.cursor_coord(self.cur.pos);
        if cc.top - eps > 0.0 {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos = layout.coords_to_pos(x, cc.top - eps);
        } else {
            let prev_leaf = prev_leaf((self.cur.block, self.cur.line), blocks);
            if let Some((prev_block, prev_idx)) = prev_leaf {
                let b = &blocks[prev_block];

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = b.child_size(prev_idx, &self.ctx, blocks, nodes).1 - eps;

                self.cur.line = prev_idx;
                self.cur.pos = b.child_coords_to_pos(prev_idx, (x, y), &self.ctx, blocks, nodes);
                self.cur.block = prev_block;
            }
        }

        CmdResult {
            repaint: true,
            update_anchor_x: false,
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
        let cc = layout.cursor_coord(self.cur.pos);
        if cc.top + cc.height + eps < layout.height {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos = layout.coords_to_pos(x, cc.top + cc.height + eps);
        } else {
            let next_leaf = next_leaf((self.cur.block, self.cur.line), blocks);
            if let Some((next_block, next_idx)) = next_leaf {
                let b = &blocks[next_block];

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = eps;

                self.cur.line = next_idx;
                self.cur.pos = b.child_coords_to_pos(
                    next_idx, (x, y),
                    &self.ctx, blocks, nodes);
                self.cur.block = next_block;
            }
        }

        CmdResult {
            repaint: true,
            update_anchor_x: false,
            class: CmdClass::Other,
        }
    }

    pub fn shift_up(&mut self) -> CmdResult {
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
        let eps = 3.0;
        if self.cur.line <= sel.line {
            if self.cur.block == self.root_block && self.cur.line == 1 {
                self.cur.pos = 0;
                if self.cur.line == sel.line && self.cur.pos == sel.pos {
                   self.cur.sel = None;
                }
                return CmdResult {
                    repaint: true,
                    update_anchor_x: false,
                    class: CmdClass::Other,
                };                
            }
            if self.cur.line > 0 {
                let b = &blocks[self.cur.block];
                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = b.child_size(self.cur.line - 1, &self.ctx, blocks, nodes).1 - eps;
                self.cur.line -= 1;
                self.cur.pos = b.child_coords_to_pos(
                    self.cur.line, (x, y),
                    &self.ctx, blocks, nodes);
                assert_ne!(self.cur.line, sel.line);
                return CmdResult {
                    repaint: true,
                    update_anchor_x: false,
                    class: CmdClass::Other,
                };
            }

            let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
            assert!(i > 0);
            let pb = &blocks[parent];
            self.cur.block = parent;
            self.cur.line = i - 1;
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * pb.depth as f32);
            let y = pb.child_size(i - 1, &self.ctx, blocks, nodes).1 - eps;
            self.cur.pos = pb.child_coords_to_pos(
                i - 1, (x, y),
                &self.ctx, blocks, nodes);

            sel.line = i;
            sel.pos = 1;
            sel.anchor_path.push(i);
        } else {
            assert!(self.cur.line > sel.line);
            self.cur.line -= 1;
            loop {
                if sel.line < self.cur.line {
                    break;
                }
                if let Some(i) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[i] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = blocks[self.cur.block].children.len() - 1;

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
                } else {
                    break;
                }
            }
            let b = &blocks[self.cur.block];
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            let y = b.child_size(self.cur.line, &self.ctx, blocks, nodes).1 - eps;
            self.cur.pos = b.child_coords_to_pos(
                self.cur.line, (x, y),
                &self.ctx, blocks, nodes);
            if self.cur.line == sel.line && self.cur.pos == sel.pos {
               self.cur.sel = None;
            }
        }
        CmdResult {
            repaint: true,
            update_anchor_x: false,
            class: CmdClass::Other,
        }
    }

    pub fn shift_down(&mut self) -> CmdResult {
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
        let eps = 3.0;
        if self.cur.line >= sel.line {
            let b = &blocks[self.cur.block];
            if self.cur.line + 1 < b.children.len() {
                let b = &blocks[self.cur.block];
                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
                let y = eps;
                self.cur.line += 1;
                self.cur.pos = b.child_coords_to_pos(
                    self.cur.line, (x, y),
                    &self.ctx, blocks, nodes);
                assert_ne!(self.cur.line, sel.line);
                return CmdResult {
                    repaint: true,
                    update_anchor_x: false,
                    class: CmdClass::Other,
                };
            }

            let pb = loop {
                if self.cur.block == self.root_block {
                    let b = &blocks[self.cur.block];
                    self.cur.line = b.children.len() - 1;
                    self.cur.pos = b.max_pos(self.cur.line, blocks, nodes);
                    if self.cur.line == sel.line && self.cur.pos == sel.pos {
                       self.cur.sel = None;
                    }
                    return CmdResult {
                        repaint: true,
                        update_anchor_x: false,
                        class: CmdClass::Other,
                    };  
                }
                let (parent, i) = blocks[self.cur.block].parent_idx.unwrap();
                sel.anchor_path.push(i);
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
            self.cur.pos = pb.child_coords_to_pos(
                self.cur.line, (x, y),
                &self.ctx, blocks, nodes);
        } else {
            assert!(self.cur.line < sel.line);
            self.cur.line += 1;
            loop {
                if sel.line > self.cur.line {
                    break;
                }
                if let Some(i) = sel.anchor_path.pop() {
                    self.cur.block = match blocks[self.cur.block].children[i] {
                        BlockChild::Leaf => panic!(),
                        BlockChild::Block(b) => b,
                    };
                    self.cur.line = 0;

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
                } else {
                    break;
                }
            }
            let b = &blocks[self.cur.block];
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            let y = eps;
            self.cur.pos = b.child_coords_to_pos(
                self.cur.line, (x, y),
                &self.ctx, blocks, nodes);
            if self.cur.line == sel.line && self.cur.pos == sel.pos {
               self.cur.sel = None;
            }
        }
        CmdResult {
            repaint: true,
            update_anchor_x: false,
            class: CmdClass::Other,
        }
    }

    pub fn put_char(&mut self, c: char) -> CmdResult {
        assert!(self.cur.sel.is_none(), "TODO");

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &self.blocks;
        let nodes = &mut self.nodes;

        let (node, line_idx) = blocks[self.cur.block].node_line_idx(self.cur.line, blocks).unwrap();
        let text = nodes[node].lines[line_idx].line.text();

        if c == ' ' && self.cur.line > 0 && self.cur.pos == 1 && text.starts_with('*') {
            let local_header = text[1..].to_owned();
            let blocks = &mut self.blocks;
            let new_node = nodes.insert(Node {
                lines: vec![],
                blocks: Default::default(),
                cblocks: Default::default(),
            });
            splice_node_lines(
                node, line_idx, line_idx + 1,
                vec![Line::Node { local_header, node: new_node }],
                blocks, &mut self.cblocks, nodes,
                &mut undo_group.edits);
            let new_block = match blocks[self.cur.block].children[self.cur.line] {
                BlockChild::Leaf => unreachable!(),
                BlockChild::Block(b) => b,
            };
            assert_eq!(blocks[new_block].node, new_node);
            self.cur.block = new_block;
            self.cur.line = 0;
            self.cur.pos = 0;
            self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
            self.redo_buf.clear();
            return CmdResult::regular();
        }

        splice_line_text(
            node,
            line_idx, self.cur.pos, self.cur.pos,
            &c.to_string(),
            nodes, &mut undo_group.edits);
        self.cur.pos += c.len_utf8();
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
        assert!(self.cur.sel.is_none(), "TODO");

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        if self.cur.line == 0 && !blocks[self.cur.block].is_expanded() {
            expand_block(self.cur.block, blocks, cblocks, nodes);
        }

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let end_pos = nodes[node].lines[line_idx].line.text().len();

        let tail = splice_line_text(
            node, line_idx,
            self.cur.pos, end_pos,
            "",
            nodes, &mut undo_group.edits);

        if self.cur.line == 0 {
            splice_node_lines(b.node, 0, 0, 
                vec![Line::Text { text: tail, monospace: false }],
                blocks, cblocks, nodes,
                &mut undo_group.edits);
        } else {
            let monospace = match nodes[node].lines[line_idx].line {
                Line::Text { monospace, .. } => monospace,
                Line::Node { .. } => panic!(),
            };
            splice_node_lines(b.node, line_idx + 1, line_idx + 1,
                vec![Line::Text { text: tail, monospace }],
                blocks, cblocks, nodes,
                &mut undo_group.edits);
        }
        self.cur.line += 1;
        self.cur.pos = 0;

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        if self.last_cmd_class == CmdClass::PutWhitespace {
            self.merge_undo_groups();
        }
        let mut res = CmdResult::regular();
        res.class = CmdClass::PutWhitespace;
        res
    }

    pub fn backspace(&mut self) -> CmdResult {
        assert!(self.cur.sel.is_none(), "TODO");

        let mut undo_group = UndoGroupBuilder::new(self.cur_waypoint());

        let blocks = &mut self.blocks;
        let cblocks = &mut self.cblocks;
        let nodes = &mut self.nodes;

        let b = &blocks[self.cur.block];
        let (node, line_idx) = b.node_line_idx(self.cur.line, blocks).unwrap();
        let text = nodes[node].lines[line_idx].line.text();

        if let Some(prev_pos) = prev_char_pos(text, self.cur.pos) {
            splice_line_text(node, line_idx,
                prev_pos, self.cur.pos, "",
                nodes, &mut undo_group.edits);
            self.cur.pos = prev_pos;
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

        let prev_leaf = prev_leaf((self.cur.block, self.cur.line), blocks);
        let (prev_block, prev_idx) = match prev_leaf {
            Some(x) => x,
            None => return CmdResult::nothing(),
        };

        if blocks[prev_block].depth > b.depth {
            if prev_idx == 0 && !blocks[prev_block].is_expanded() {
                // TODO: silent autoexpand if it's one-line node
                expand_block(prev_block, blocks, cblocks, nodes);
                return CmdResult::regular();
            }
        } else if prev_block != self.cur.block {
            assert_eq!(self.cur.line, 0);

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
                expand_block(self.cur.block, blocks, cblocks, nodes);
                return CmdResult::regular();
            }

            let mut lines = splice_node_lines(
                blocks[self.cur.block].node,
                0, nodes[blocks[self.cur.block].node].lines.len(),
                vec![],
                blocks, cblocks, nodes,
                &mut undo_group.edits);

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
                &mut undo_group.edits);
            self.cur.block = parent_block;
            self.cur.line = idx_in_parent;
            self.cur.pos = 0;
            self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
            self.redo_buf.clear();
            return CmdResult::regular();
        }

        let text = text.to_owned();
        let (prev_node, prev_line_idx) = blocks[prev_block].node_line_idx(prev_idx, blocks).unwrap();

        let prev_text_len = nodes[prev_node].lines[prev_line_idx].line.text().len();
        splice_line_text(prev_node, prev_line_idx,
            prev_text_len, prev_text_len, &text,
            nodes, &mut undo_group.edits);

        self.cur.block = prev_block;
        self.cur.line = prev_idx;
        self.cur.pos = prev_text_len;
        splice_node_lines(
            node, line_idx, line_idx + 1, vec![],
            blocks, cblocks, nodes,
            &mut undo_group.edits);

        self.undo_buf.push(undo_group.finish(self.cur_waypoint()));
        self.redo_buf.clear();
        let mut res = CmdResult::regular();
        res.class = CmdClass::BackspaceNewLine;
        res
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
            expand_block(self.cur.block, blocks, cblocks, nodes);
            CmdResult::regular()
        }
    }

    pub fn copy(&mut self) -> (Vec<Line>, String) {
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

    pub fn paste(&mut self, lines: Vec<Line>) -> CmdResult {
        let cur_line_pos = (self.cur.line, self.cur.pos);
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
        self.cur.pos = cur_line_pos.1;
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
                nodes, &mut undo_group.edits);
            if self.cur.line > 0 && !blocks[self.cur.block].is_expanded() {
                expand_block(self.cur.block, blocks, cblocks, nodes);
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
            &mut undo_group.edits);

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
                apply_edit(edit, blocks, cblocks, nodes, &mut redo_group.edits);
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