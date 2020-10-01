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
use crate::text_layout::TextLayout;

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
        AppCtx {
            render_target,
            dwrite_factory,
            normal_text_format,
            code_text_format,
            text_brush,
        }        
    }
}

struct App {
    ctx: AppCtx,
    vis_forest: Vec<VisTree>,
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
                            "Node", 500.0),
                    },
                ],
            }
        ];
        App {
            ctx,
            vis_forest,
        }        
    }
}

impl VisTree {
    fn draw(&self, ctx: &AppCtx, x: f32, y: &mut f32) {
        match self {
            VisTree::Leaf { layout } => {
                unsafe {
                    ctx.render_target.DrawTextLayout(
                        D2D1_POINT_2F { x, y: *y },
                        layout.raw.as_raw(),
                        ctx.text_brush.as_raw(),
                        D2D1_DRAW_TEXT_OPTIONS_NONE);
                }
                *y += layout.height;
            }
            VisTree::Node { children } => {
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
                for child in children {
                    child.draw(ctx, x + 20.0, y);
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
        for vis_tree in &app.vis_forest {
            vis_tree.draw(&app.ctx, x, &mut y);
        }

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
