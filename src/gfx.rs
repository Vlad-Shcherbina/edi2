use std::ptr::null_mut;
use winapi::um::d2d1::*;
use wio::com::ComPtr;
use crate::{AppCtx, Cur};
use crate::text_layout::{TextLayout, CursorCoord};
use crate::types::*;
use crate::owned_ref::{Owned, Refed};

impl Node {
    pub(crate) fn line_layout(&mut self, idx: usize, ctx: &AppCtx) -> &TextLayout {
        let line = &mut self.lines[idx];
        let layout = &mut line.layout;
        let line = &line.line;
        layout.get_or_insert_with(|| {
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
    pub(crate) fn size(&self, ctx: &AppCtx) -> (f32, f32) {
        let mut w = 0.0f32;
        let mut h = 0.0f32;
        for i in 0..self.children.len() {
            let (ww, hh) = self.child_size(i, ctx);
            h += hh;
            w = w.max(ww);
        }
        (w + INDENT, h)
    }

    pub(crate) fn last_line_height(&self, ctx: &AppCtx) -> f32 {
        match self.children.last().unwrap() {
            BlockChild::Leaf => {
                let (node, line_idx) = self.node_line_idx(self.children.len() - 1).unwrap();
                let mut node = node.borrow_mut();
                let layout = node.line_layout(line_idx, ctx);
                layout.cursor_coord(layout.text.len()).height
            }
            BlockChild::Block(ref b) => b.borrow().last_line_height(ctx),
        }
    }

    pub(crate) fn child_size(&self, idx: usize, ctx: &AppCtx) -> (f32, f32) {
        match self.children[idx] {
            BlockChild::Leaf => {
                if let Some((node, line_idx)) = self.node_line_idx(idx) {
                    let mut node = node.borrow_mut();
                    let layout = node.line_layout(line_idx, ctx);
                    layout_size(layout)
                } else {
                    (0.0, 0.0)  // header of the root block is not displayed
                }
            }
            BlockChild::Block(ref b) => b.borrow().size(ctx),
        }
    }

    pub(crate) fn max_pos(&self, idx: usize) -> usize {
        match self.children[idx] {
            BlockChild::Leaf => {
                let (node, line_idx) = self.node_line_idx(idx).unwrap();
                let node = node.borrow();
                match node.lines[line_idx].line {
                    Line::Text { ref text, .. } => text.len(),
                    Line::Node { ref local_header, .. } => local_header.len(),
                }
            }
            BlockChild::Block(_) => 1,
        }
    }
}

pub trait BlockVisitor {
    fn visit_child(&mut self, block: Refed<Block>, idx: usize, x: f32, y: f32);
}

pub(crate) fn accept_block(
    block: &Owned<Block>,
    visitor: &mut dyn BlockVisitor,
    y: &mut f32,
    ctx: &AppCtx,
) {
    let b = block.borrow();
    let x = X_OFFSET + b.depth as f32 * INDENT;
    for (i, child) in b.children.iter().enumerate() {
        visitor.visit_child(block.make_ref(), i, x, *y);
        match child {
            BlockChild::Leaf =>
                *y += b.child_size(i, ctx).1,
            BlockChild::Block(b) =>
                accept_block(b, visitor, y, ctx),
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
    fn visit_child(&mut self, block: Refed<Block>, idx: usize, x: f32, y: f32) {
        let mut cur_pos = None;
        let mut sel = None;
        if block.ptr_eq(&self.cur.block) {
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
                        assert!(end_pos <= block.borrow().max_pos(idx));
                        (end_pos, false)
                    } else {
                        (block.borrow().max_pos(idx), true)
                    };
                    sel = Some((sel_start_pos, sel_end_pos, include_newline));
                }
            }
        }

        let b = block.borrow();
        match b.children[idx] {
            BlockChild::Leaf => {
                if let Some((node, line_idx)) = b.node_line_idx(idx) {
                    let mut node = node.borrow_mut();
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

                    unsafe {
                        self.ctx.render_target.DrawTextLayout(
                            D2D1_POINT_2F { x, y },
                            layout.raw.as_raw(),
                            self.ctx.text_brush.as_raw(),
                            D2D1_DRAW_TEXT_OPTIONS_NONE);
                    }                
                }
            }
            BlockChild::Block(ref b) => {
                if let Some((0, 1, _)) = sel {
                    let (w, h) = b.borrow().size(self.ctx);
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
                    self.ctx.render_target.DrawRectangle(
                        &rect, self.ctx.text_brush.as_raw(), 1.0, null_mut());
                }
            }
        }
        if let Some(pos) = cur_pos {
            let cc = b.cursor_coord(idx, pos, self.ctx);
            draw_cursor(&self.ctx.render_target, &self.ctx.cursor_brush, x, y, &cc);
        }        
    }
}

impl Block {
    pub(crate) fn cursor_coord(&self, idx: usize, pos: usize, ctx: &AppCtx) -> CursorCoord {
        match self.children[idx] {
            BlockChild::Leaf => {
                let (node, line_idx) = self.node_line_idx(idx).unwrap();
                let mut node = node.borrow_mut();
                let layout = node.line_layout(line_idx, ctx);
                layout.cursor_coord(pos)
            }
            BlockChild::Block(ref b) => {
                // TODO: get correct cursor height from child
                match pos {
                    0 => CursorCoord { x: 0.0, top: 0.0, height: 18.0 },
                    1 => {
                        let (w, h) = b.borrow().size(ctx);
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

    pub(crate) fn abs_cur_x(&self, idx: usize, pos: usize, ctx: &AppCtx) -> f32 {
        let cc = self.cursor_coord(idx, pos, ctx);
        X_OFFSET + self.depth as f32 * INDENT + cc.x
    }
}

pub enum MouseResult {
    Nothing,
    Cur {
        block: Refed<Block>,
        line: usize,
        pos: usize,
    },
    Toggle {
        block: Refed<Block>,
    }
}

pub(crate) struct MouseClickVisitor<'a> {
    pub(crate) x: f32,
    pub(crate) y: f32,
    pub(crate) ctx: &'a AppCtx,
    pub(crate) result: MouseResult,
}

impl<'a> BlockVisitor for MouseClickVisitor<'a> {
    fn visit_child(&mut self, block: Refed<Block>, idx: usize, x: f32, y: f32) {
        let b = block.borrow();
        match b.children[idx] {
            BlockChild::Leaf => {
                if let Some((node, line_idx)) = b.node_line_idx(idx) {
                    let mut node = node.borrow_mut();
                    let layout = node.line_layout(line_idx, self.ctx);
                    if y <= self.y && self.y <= y + layout.height {
                        let pos = layout.coords_to_pos(self.x - x, self.y - y);
                        drop(b);
                        if let MouseResult::Nothing = self.result {
                            self.result = MouseResult::Cur { block, line: idx, pos };
                        }
                    }                    
                }
            }
            BlockChild::Block(ref b) => {
                let dx = x + BULLET_OFFSET_X + 0.5 * BULLET_SIZE - self.x;
                let dy = y + BULLET_OFFSET_Y + 0.5 * BULLET_SIZE - self.y;
                if dx * dx + dy * dy <= BULLET_MOUSE_RADIUS * BULLET_MOUSE_RADIUS {
                    self.result = MouseResult::Toggle { block: b.make_ref() };
                }
            }
        }
    }
}
