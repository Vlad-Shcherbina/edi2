use std::ptr::null_mut;
use winapi::um::d2d1::*;
use wio::com::ComPtr;
use crate::{AppCtx, Cursor};
use crate::text_layout::CursorCoord;
use crate::vis_tree::{VisTree, VisTreeVisitor};

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

pub struct DrawVisitor<'a> {
    pub(crate) ctx: &'a AppCtx,
    pub(crate) cur: &'a Cursor,
}

impl<'a> VisTreeVisitor for DrawVisitor<'a> {
    fn visit(&mut self, path: &[usize], tree: &VisTree, x: f32, y: f32) {
        let (&line, pth) = path.split_last().unwrap();        
        let mut cur_pos = None;
        let mut sel = None;
        if pth == self.cur.path {
            if line == self.cur.pos.line {
                cur_pos = Some(self.cur.pos.pos)
            }
            if let Some(s) = &self.cur.sel {
                let sel_start = s.pos.min(self.cur.pos);
                let sel_end = s.pos.max(self.cur.pos);
                if sel_start.line <= line && line <= sel_end.line {
                    let sel_start_pos = if sel_start.line == line {
                        sel_start.pos
                    } else {
                        0
                    };
                    let (sel_end_pos, include_newline) = if sel_end.line == line {
                        assert!(sel_end.pos <= tree.len());
                        (sel_end.pos, false)
                    } else {
                        (tree.len(), true)
                    };
                    sel = Some((sel_start_pos, sel_end_pos, include_newline));
                }
            }
        }

        match tree {
            VisTree::Leaf { layout } => {
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
            VisTree::Node { .. } => {
                if let Some((0, 1, _)) = sel {
                    let (w, h) = tree.size();
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

                let bullet_offset_x = 7.5;
                let bullet_offset_y = 7.5;
                let bullet_size = 5.0;
                let xx = (x + bullet_offset_x).floor() + 0.5;
                let yy = (y + bullet_offset_y).floor() + 0.5;
                let rect = D2D1_RECT_F {
                    left: xx,
                    top: yy,
                    right: xx + bullet_size,
                    bottom: yy + bullet_size,
                };
                unsafe {
                    self.ctx.render_target.DrawRectangle(
                        &rect, self.ctx.text_brush.as_raw(), 1.0, null_mut());
                }
            }
        }
        if let Some(pos) = cur_pos {
            let cc = tree.cursor_coord(pos);
            draw_cursor(&self.ctx.render_target, &self.ctx.cursor_brush, x, y, &cc);
        }
    }
}
