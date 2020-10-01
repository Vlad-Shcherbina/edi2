mod win_util;
pub mod win_win_reent;
mod text_layout;

use std::ptr::null_mut;
use wio::com::ComPtr;
use winapi::shared::windef::*;
use winapi::shared::minwindef::*;
use winapi::shared::winerror::*;
use winapi::shared::windowsx::*;
use winapi::um::winuser::*;
use winapi::um::d2d1::*;
use winapi::um::dwrite::*;
use winapi::um::dcommon::*;
use winapi::um::dcommon::{D2D1_POINT_2F, D2D1_RECT_F};
use crate::win_win_reent::*;
use crate::win_util::*;
use crate::text_layout::{TextLayout, CursorCoord};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct TextPos {
    line: usize,
    pos: usize,
}

struct Cursor {
    path: Vec<usize>,
    pos: TextPos,
    sel: Option<Selection>,
}

struct Selection {
    pos: TextPos,
}

enum VisTree {
    Leaf {
        layout: TextLayout,
    },
    Node {
        children: Vec<VisTree>,
    },
}

struct AppCtx {
    render_target: ComPtr<ID2D1HwndRenderTarget>,
    dwrite_factory: ComPtr<IDWriteFactory>,
    normal_text_format: ComPtr<IDWriteTextFormat>,
    code_text_format: ComPtr<IDWriteTextFormat>,
    text_brush: ComPtr<ID2D1Brush>,
    cursor_brush: ComPtr<ID2D1Brush>,
    sel_brush: ComPtr<ID2D1Brush>,
}

impl AppCtx {
    fn new(hwnd: HWND) -> Self {
        let d2d_factory = create_d2d_factory();
        let render_target = create_hwnd_render_target(&d2d_factory, hwnd);
        let dwrite_factory = create_dwrite_factory();
        let normal_text_format = create_text_format(&dwrite_factory, "Arial", 18.0);
        let code_text_format = create_text_format(&dwrite_factory, "Consolas", 18.0);
        let text_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 0.8, g: 0.8, b: 0.8, a: 1.0 });
        let cursor_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 1.0, g: 1.0, b: 1.0, a: 1.0 });
        let sel_brush = create_solid_brush(
            &render_target, &D2D1_COLOR_F { r: 0.3, g: 0.3, b: 0.4, a: 1.0 });
        AppCtx {
            render_target,
            dwrite_factory,
            normal_text_format,
            code_text_format,
            text_brush,
            cursor_brush,
            sel_brush,
        }        
    }
}

struct App {
    ctx: AppCtx,
    vis_forest: Vec<VisTree>,
    cur: Cursor,
}

impl App {
    fn new(hwnd: HWND) -> Self {
        let ctx = AppCtx::new(hwnd);
        let vis_forest = vec![
            VisTree::Leaf {
                layout: TextLayout::new(
                    &ctx.dwrite_factory,
                    &ctx.normal_text_format,
                    "Stuff", 500.0),
            },
            VisTree::Leaf {
                layout: TextLayout::new(
                    &ctx.dwrite_factory,
                    &ctx.code_text_format,
                    "if name == '__main__':", 500.0),
            },
            VisTree::Leaf {
                layout: TextLayout::new(
                    &ctx.dwrite_factory,
                    &ctx.code_text_format,
                    "    print('hello')", 500.0),
            },
            VisTree::Leaf {
                layout: TextLayout::new(
                    &ctx.dwrite_factory,
                    &ctx.normal_text_format,
                    "Stuff...", 500.0),
            },
            VisTree::Node {
                children: vec![
                    VisTree::Leaf {
                        layout: TextLayout::new(
                            &ctx.dwrite_factory,
                            &ctx.normal_text_format,
                            "zzz\nNode", 500.0),
                    },
                ],
            }
        ];
        App {
            ctx,
            vis_forest,
            cur: Cursor {
                path: vec![],
                pos: TextPos { line: 4, pos: 1 },
                sel: Some(Selection {
                    pos: TextPos { line: 3, pos: 8 },
                }),
            }
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

const INDENT: f32 = 20.0;

impl VisTree {
    fn size(&self) -> (f32, f32) {
        let mut w = 0.0f32;
        let mut h = 0.0f32;
        match self {
            VisTree::Leaf { layout } => {
                let rects = layout.hit_test_text_range(
                    0, layout.text.len(), /*include newline*/true);
                for rect in rects {
                    w = w.max(rect.left + rect.width);
                    h = h.max(rect.top + rect.height);
                }
            }
            VisTree::Node { children } => {
                for child in children {
                    let (ww, hh) = child.size();
                    w = w.max(ww);
                    h += hh;
                }
                w += INDENT;
            }
        }
        (w, h)
    }

    fn draw(&self, ctx: &AppCtx, cur: &Cursor, x: f32, y: &mut f32, path: &mut Vec<usize>) {
        let (&line, pth) = path.split_last().unwrap();        
        let mut cur_pos = None;
        let mut sel = None;
        if pth == cur.path {
            if line == cur.pos.line {
                cur_pos = Some(cur.pos.pos)
            }
            if let Some(s) = &cur.sel {
                let sel_start = s.pos.min(cur.pos);
                let sel_end = s.pos.max(cur.pos);
                if sel_start.line <= line && line <= sel_end.line {
                    let sel_start_pos = if sel_start.line == line {
                        sel_start.pos
                    } else {
                        0
                    };
                    let len = match self {
                        VisTree::Leaf { layout } => layout.text.len(),
                        VisTree::Node { .. } => 1,
                    };
                    let (sel_end_pos, include_newline) = if sel_end.line == line {
                        assert!(sel_end.pos <= len);
                        (sel_end.pos, false)
                    } else {
                        (len, true)
                    };
                    sel = Some((sel_start_pos, sel_end_pos, include_newline));
                }
            }
        }

        match self {
            VisTree::Leaf { layout } => {
                if let Some((sel_start_pos, sel_end_pos, include_newline)) = sel {
                    let rects = layout.hit_test_text_range(
                        sel_start_pos, sel_end_pos, include_newline);
                    for rect in rects {
                        let rect = D2D1_RECT_F {
                            left: x + rect.left,
                            top: *y + rect.top,
                            right: x + rect.left + rect.width,
                            bottom : *y + rect.top + rect.height,
                        };
                        unsafe {
                            ctx.render_target.FillRectangle(&rect, ctx.sel_brush.as_raw());
                        }
                    }
                }
                unsafe {
                    ctx.render_target.DrawTextLayout(
                        D2D1_POINT_2F { x, y: *y },
                        layout.raw.as_raw(),
                        ctx.text_brush.as_raw(),
                        D2D1_DRAW_TEXT_OPTIONS_NONE);
                }
                if let Some(pos) = cur_pos {
                    let cc = layout.cursor_coord(pos);
                    draw_cursor(&ctx.render_target, &ctx.cursor_brush, x, *y, &cc);
                }
                *y += layout.height;
            }
            VisTree::Node { children } => {
                if let Some((0, 1, _)) = sel {
                    let (w, h) = self.size();
                    let rect = D2D1_RECT_F {
                        left: x,
                        top: *y,
                        right: x + w,
                        bottom : *y + h,
                    };
                    unsafe {
                        ctx.render_target.FillRectangle(&rect, ctx.sel_brush.as_raw());
                    }
                }

                let bullet_offset_x = 7.5;
                let bullet_offset_y = 7.5;
                let bullet_size = 5.0;
                let xx = (x + bullet_offset_x).floor() + 0.5;
                let yy = (*y + bullet_offset_y).floor() + 0.5;
                let rect = D2D1_RECT_F {
                    left: xx,
                    top: yy,
                    right: xx + bullet_size,
                    bottom: yy + bullet_size,
                };
                unsafe {
                    ctx.render_target.DrawRectangle(
                        &rect, ctx.text_brush.as_raw(), 1.0, null_mut());
                }

                if let Some(pos) = cur_pos {
                    // TODO: get correct height from child
                    let cc = match pos {
                        0 => CursorCoord { x: 0.0, top: 0.0, height: 18.0 },
                        1 => {
                            let (w, h) = self.size();
                            CursorCoord {
                                x: w,
                                top: h - 18.0,
                                height: 18.0,
                            }
                        }
                        _ => panic!("{}", pos),
                    };
                    draw_cursor(
                        &ctx.render_target, &ctx.cursor_brush, x, *y,
                        &cc);
                }

                for (i, child) in children.iter().enumerate() {
                    path.push(i);
                    child.draw(ctx, cur, x + INDENT, y, path);
                    path.pop();
                }
            }
        }
    }
}

fn paint(app: &mut App) {
    let rt = &app.ctx.render_target;
    unsafe {
        rt.BeginDraw();
        rt.Clear(&D2D1_COLOR_F { r: 0.0, g: 0.0, b: 0.2, a: 1.0 });

        let x = 10.0;
        let mut y = 10.0;
        let mut path = vec![];
        for (i, vis_tree) in app.vis_forest.iter().enumerate() {
            path.push(i);
            vis_tree.draw(&app.ctx, &app.cur, x, &mut y, &mut path);
            path.pop();
        }
        assert!(path.is_empty());

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
            eprintln!("{}", win_msg_name::win_msg_name(msg));
            unsafe {
                PostQuitMessage(0);
            }
        }
        if msg == WM_SIZE {
            println!("{}", win_msg_name::win_msg_name(msg));
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
            eprintln!("{}", win_msg_name::win_msg_name(msg));
            paint(&mut *sr.state_mut());
        }
        None
    }
}

fn main() {
    let app = LazyState::new(App::new);
    unsafe {
        let win_class = win_win::WindowClass::builder("e2 class").build().unwrap();
        let hwnd = win_win::WindowBuilder::new(app, &win_class)
            .name("e2")
            .style(WS_OVERLAPPEDWINDOW)
            .build();
        ShowWindow(hwnd, SW_SHOWNORMAL);
        win_win::runloop(std::ptr::null_mut());
    }
}
