use std::ptr::null_mut;
use winapi::um::d2d1::*;
use wio::com::ComPtr;
use crate::{AppCtx, Cur};
use crate::text_layout::{TextLayout, CursorCoord};
use crate::types::*;

impl Node {
    pub(crate) fn line_layout(&self, idx: usize, ctx: &AppCtx) -> &TextLayout {
        let line = &self.lines[idx];
        let layout = &line.layout;
        let line = &line.line;
        layout.get_or_init(|| {
            match line {
                Line::Text { text, monospace } =>
                    TextLayout::new(
                        &ctx.dwrite_factory,
                        if *monospace {
                            &ctx.code_text_format
                        } else {
                            &ctx.normal_text_format
                        },
                        text,
                        500.0),
                Line::Node { local_header, .. } => 
                    TextLayout::new(
                        &ctx.dwrite_factory,
                        &ctx.header_text_format,
                        local_header, 500.0),
            }
        })
    }
}

fn layout_size(layout: &TextLayout) -> (f32, f32) {
    let mut w = 0.0f32;
    let mut h = 0.0f32;
    let rects = layout.hit_test_text_range(
        0, layout.text.len(), /*include newline*/true);
    for rect in rects {
        w = w.max(rect.left + rect.width);
        h = h.max(rect.top + rect.height);
    }
    (w, h)
}

impl Block {
    pub(crate) fn size(
        &self,
        ctx: &AppCtx,
        blocks: &Blocks, nodes: &Nodes,
    ) -> (f32, f32) {
        let mut w = 0.0f32;
        let mut h = 0.0f32;
        for i in 0..self.children.len() {
            let (ww, hh) = self.child_size(i, ctx, blocks, nodes);
            h += hh;
            w = w.max(ww);
        }
        (w + INDENT, h)
    }

    pub(crate) fn last_line_height(
        &self, ctx: &AppCtx,
        blocks: &Blocks, nodes: &Nodes,
    ) -> f32 {
        match *self.children.last().unwrap() {
            BlockChild::Leaf => {
                let (node, line_idx) = self.node_line_idx(
                    self.children.len() - 1, blocks).unwrap();
                let layout = nodes[node].line_layout(line_idx, ctx);
                layout.cursor_coord(layout.text.len()).height
            }
            BlockChild::Block(b) => blocks[b].last_line_height(ctx, blocks, nodes),
        }
    }

    pub(crate) fn child_size(
        &self,
        idx: usize,
        ctx: &AppCtx,
        blocks: &Blocks, nodes: &Nodes,
    ) -> (f32, f32) {
        match self.children[idx] {
            BlockChild::Leaf => {
                if let Some((node, line_idx)) = self.node_line_idx(idx, blocks) {
                    let layout = nodes[node].line_layout(line_idx, ctx);
                    layout_size(layout)
                } else {
                    (0.0, 0.0)  // header of the root block is not displayed
                }
            }
            BlockChild::Block(b) => blocks[b].size(ctx, blocks, nodes),
        }
    }

    pub(crate) fn max_pos(
        &self, idx: usize,
        blocks: &Blocks, nodes: &Nodes,
    ) -> usize {
        match self.children[idx] {
            BlockChild::Leaf => {
                let (node, line_idx) = self.node_line_idx(idx, blocks).unwrap();
                match nodes[node].lines[line_idx].line {
                    Line::Text { ref text, .. } => text.len(),
                    Line::Node { ref local_header, .. } => local_header.len(),
                }
            }
            BlockChild::Block(_) => 1,
        }
    }

    pub(crate) fn child_coords_to_pos(
        &self,
        idx: usize,
        (x, y): (f32, f32),
        ctx: &AppCtx, blocks: &Blocks, nodes: &Nodes,
    ) -> usize {
        match self.children[idx] {
            BlockChild::Leaf => {
                let (node, line_idx) = self.node_line_idx(idx, blocks).unwrap();
                let layout = nodes[node].line_layout(line_idx, ctx);
                layout.coords_to_pos(x, y)
            }
            BlockChild::Block(_) => {
                let w = self.child_size(idx, ctx, blocks, nodes).0;
                if x < 0.5 * w {
                    0
                } else {
                    1
                }
            }
        }
    }
}

pub trait BlockVisitor {
    fn visit_child(
        &mut self, block: BlockKey, idx: usize, x: f32, y: f32,
        blocks: &Blocks, nodes: &Nodes,
    );
}

pub(crate) fn accept_block(
    block: BlockKey,
    visitor: &mut dyn BlockVisitor,
    y: &mut f32,
    ctx: &AppCtx,
    blocks: &Blocks, nodes: &Nodes,
) {
    let b = &blocks[block];
    let x = X_OFFSET + b.depth as f32 * INDENT;
    for (i, child) in b.children.iter().enumerate() {
        visitor.visit_child(block, i, x, *y, blocks, nodes);
        match *child {
            BlockChild::Leaf =>
                *y += b.child_size(i, ctx, blocks, nodes).1,
            BlockChild::Block(b) =>
                accept_block(b, visitor, y, ctx, blocks, nodes),
        }
    }
}

fn draw_cursor(
    rt: &ComPtr<ID2D1HwndRenderTarget>,
    brush: &ComPtr<ID2D1Brush>,
    x: f32, y: f32,
    cc: &CursorCoord,
) {
    let x = (x + cc.x + 0.5).floor();
    unsafe {
        rt.DrawLine(
            D2D1_POINT_2F { x, y: y + cc.top },
            D2D1_POINT_2F { x, y: y + cc.top + cc.height },
            brush.as_raw(),
            2.0,  // stroke width,
            null_mut(),  // stroke style
        );
    }
}

pub const X_OFFSET: f32 = 10.0;
pub const INDENT: f32 = 20.0;
const BULLET_OFFSET_X: f32 = 7.5;
const BULLET_OFFSET_Y: f32 = 7.5;
const BULLET_SIZE: f32 = 5.0;
const BULLET_MOUSE_RADIUS: f32 = 7.0;

pub struct DrawVisitor<'a> {
    pub(crate) ctx: &'a AppCtx,
    pub(crate) cur: &'a Cur,
}

impl<'a> BlockVisitor for DrawVisitor<'a> {
    fn visit_child(
        &mut self, block: BlockKey, idx: usize, x: f32, y: f32,
        blocks: &Blocks,
        nodes: &Nodes,
    ) {
        let mut cur_pos = None;
        let mut sel = None;
        if block == self.cur.block {
            if idx == self.cur.line {
                cur_pos = Some(self.cur.pos);
            }

            if let Some(s) = &self.cur.sel {
                let (start_line, start_pos) =
                    (self.cur.line, self.cur.pos).min((s.line, s.pos));
                let (end_line, end_pos) =
                    (self.cur.line, self.cur.pos).max((s.line, s.pos));
                if start_line <= idx && idx <= end_line {
                    let sel_start_pos = if start_line == idx {
                        start_pos
                    } else {
                        0
                    };
                    let (sel_end_pos, include_newline) = if end_line == idx {
                        assert!(end_pos <= blocks[block].max_pos(idx, blocks, nodes));
                        (end_pos, false)
                    } else {
                        (blocks[block].max_pos(idx, blocks, nodes), true)
                    };
                    sel = Some((sel_start_pos, sel_end_pos, include_newline));
                }
            }
        }

        let b = &blocks[block];
        match b.children[idx] {
            BlockChild::Leaf => {
                if let Some((node, line_idx)) = b.node_line_idx(idx, blocks) {
                    let node = &nodes[node];
                    let layout = node.line_layout(line_idx, self.ctx);

                    if let Some((sel_start_pos, sel_end_pos, include_newline)) = sel {
                        let rects = layout.hit_test_text_range(
                            sel_start_pos, sel_end_pos, include_newline);
                        for rect in rects {
                            let rect = D2D1_RECT_F {
                                left: x + rect.left,
                                top: y + rect.top,
                                right: x + rect.left + rect.width,
                                bottom : y + rect.top + rect.height,
                            };
                            unsafe {
                                self.ctx.render_target.FillRectangle(
                                    &rect, self.ctx.sel_brush.as_raw());
                            }
                        }
                    }

                    let brush = if idx == 0 {
                        assert!(b.depth > 0);
                        let brushes = &self.ctx.header_rainbow_brushes;
                        brushes[(b.depth - 1) as usize % brushes.len()].as_raw()
                    } else {
                        self.ctx.text_brush.as_raw()
                    };

                    unsafe {
                        self.ctx.render_target.DrawTextLayout(
                            D2D1_POINT_2F { x, y },
                            layout.raw.as_raw(),
                            brush,
                            D2D1_DRAW_TEXT_OPTIONS_NONE);
                    }                
                }
            }
            BlockChild::Block(b) => {
                if let Some((0, 1, _)) = sel {
                    let (w, h) = blocks[b].size(self.ctx, blocks, nodes);
                    let rect = D2D1_RECT_F {
                        left: x,
                        top: y,
                        right: x + w,
                        bottom : y + h,
                    };
                    unsafe {
                        self.ctx.render_target.FillRectangle(
                            &rect, self.ctx.sel_brush.as_raw());
                    }
                }

                let xx = (x + BULLET_OFFSET_X).floor() + 0.5;
                let yy = (y + BULLET_OFFSET_Y).floor() + 0.5;
                let rect = D2D1_RECT_F {
                    left: xx,
                    top: yy,
                    right: xx + BULLET_SIZE,
                    bottom: yy + BULLET_SIZE,
                };
                unsafe {
                    if !blocks[b].is_expanded() {
                        self.ctx.render_target.FillRectangle(
                            &rect, self.ctx.text_brush.as_raw());
                    }
                    self.ctx.render_target.DrawRectangle(
                        &rect, self.ctx.text_brush.as_raw(), 1.0, null_mut());
                }

                let num_parents = nodes[blocks[b].node].parents.values().sum::<u32>();
                if num_parents > 1 {
                    for i in 0..num_parents - 1 {
                        let yy = y + BULLET_OFFSET_Y + BULLET_SIZE * 0.5
                            + 2.0 * (i as f32 - 0.5 * (num_parents - 2) as f32);
                        let yy = yy.floor() + 0.5;
                        unsafe {
                            self.ctx.render_target.DrawLine(
                                D2D1_POINT_2F {
                                    x: 600.0,
                                    y: yy,
                                },
                                D2D1_POINT_2F {
                                    x: 630.0,
                                    y: yy,
                                },
                                self.ctx.text_brush.as_raw(),
                                1.0,  // stroke width,
                                null_mut(),  // stroke style
                            );
                        }
                    }
                }
            }
        }
        if let Some(pos) = cur_pos {
            let cc = b.cursor_coord(idx, pos, self.ctx, blocks, nodes);
            draw_cursor(&self.ctx.render_target, &self.ctx.cursor_brush, x, y, &cc);
        }        
    }
}

impl Block {
    pub(crate) fn cursor_coord(
        &self, idx: usize, pos: usize,
        ctx: &AppCtx,
        blocks: &Blocks, nodes: &Nodes,
    ) -> CursorCoord {
        match self.children[idx] {
            BlockChild::Leaf => {
                let (node, line_idx) = self.node_line_idx(idx, blocks).unwrap();
                let layout = nodes[node].line_layout(line_idx, ctx);
                layout.cursor_coord(pos)
            }
            BlockChild::Block(b) => {
                // TODO: get correct cursor height from child
                match pos {
                    0 => CursorCoord { x: 0.0, top: 0.0, height: 18.0 },
                    1 => {
                        let (w, h) = blocks[b].size(ctx, blocks, nodes);
                        CursorCoord {
                            x: w,
                            top: h - 18.0,
                            height: 18.0,
                        }
                    }
                    _ => panic!("{}", pos),
                }
            }
        }
    }

    pub(crate) fn abs_cur_x(
        &self, idx: usize, pos: usize,
        ctx: &AppCtx,
        blocks: &Blocks, nodes: &Nodes,
    ) -> f32 {
        let cc = self.cursor_coord(idx, pos, ctx, blocks, nodes);
        X_OFFSET + self.depth as f32 * INDENT + cc.x
    }

    pub(crate) fn abs_cursor_coord(
        &self, idx: usize, pos: usize,
        ctx: &AppCtx,
        blocks: &Blocks, nodes: &Nodes,
    ) -> CursorCoord {
        let mut cc = self.cursor_coord(idx, pos, ctx, blocks, nodes);
        cc.x += X_OFFSET + self.depth as f32 * INDENT;

        for child in 0..idx {
            cc.top += self.child_size(child, ctx, blocks, nodes).1;
        }

        let mut b = self;
        while let Some((p, i)) = b.parent_idx {
            b = &blocks[p];
            for child in 0..i {
                cc.top += b.child_size(child, ctx, blocks, nodes).1;
            }
        }
        cc
    }
}

pub enum MouseResult {
    Nothing,
    Cur {
        block: BlockKey,
        line: usize,
        pos: usize,
    },
    Toggle {
        block: BlockKey,
    }
}

pub(crate) struct MouseClickVisitor<'a> {
    pub(crate) x: f32,
    pub(crate) y: f32,
    pub(crate) ctx: &'a AppCtx,
    pub(crate) result: MouseResult,
}

impl<'a> BlockVisitor for MouseClickVisitor<'a> {
    fn visit_child(
        &mut self, block: BlockKey, idx: usize, x: f32, y: f32,
        blocks: &Blocks, nodes: &Nodes,
    ) {
        let b = &blocks[block];
        match b.children[idx] {
            BlockChild::Leaf => {
                if let Some((node, line_idx)) = b.node_line_idx(idx, blocks) {
                    let node = &nodes[node];
                    let layout = node.line_layout(line_idx, self.ctx);
                    if y <= self.y && self.y <= y + layout.height {
                        let pos = layout.coords_to_pos(self.x - x, self.y - y);
                        if let MouseResult::Nothing = self.result {
                            self.result = MouseResult::Cur { block, line: idx, pos };
                        }
                    }
                }
            }
            BlockChild::Block(b) => {
                let dx = x + BULLET_OFFSET_X + 0.5 * BULLET_SIZE - self.x;
                let dy = y + BULLET_OFFSET_Y + 0.5 * BULLET_SIZE - self.y;
                if dx * dx + dy * dy <= BULLET_MOUSE_RADIUS * BULLET_MOUSE_RADIUS {
                    self.result = MouseResult::Toggle { block: b };
                }
            }
        }
    }
}
