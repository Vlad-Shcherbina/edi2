#![feature(bindings_after_at)]

mod win_util;
pub mod win_win_reent;
mod text_layout;
mod vis_tree;
pub mod gfx;
mod util;

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
use win_msg_name::win_msg_name;
use crate::win_win_reent::*;
use crate::win_util::*;
use crate::text_layout::TextLayout;
use crate::util::*;
use crate::vis_tree::{VisTree, VisTreeVisitor, next_leaf, prev_leaf};
use crate::gfx::{X_OFFSET, INDENT, DrawVisitor};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct TextPos {
    line: usize,
    pos: usize,
}

#[derive(Debug)]
struct Cursor {
    path: Vec<usize>,
    pos: TextPos,
    sel: Option<Selection>,
    anchor_x: f32,
}

#[derive(Debug)]
struct Selection {
    pos: TextPos,
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
    y_offset: f32,
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
                    VisTree::Leaf {
                        layout: TextLayout::new(
                            &ctx.dwrite_factory,
                            &ctx.normal_text_format,
                            "aaa", 500.0),
                    },
                ],
            },
            VisTree::Leaf {
                layout: TextLayout::new(
                    &ctx.dwrite_factory,
                    &ctx.normal_text_format,
                    "Stuff...", 500.0),
            },
        ];
        let mut app = App {
            ctx,
            vis_forest,
            cur: Cursor {
                path: vec![],
                pos: TextPos { line: 4, pos: 0 },
                sel: Some(Selection {
                    pos: TextPos { line: 3, pos: 8 },
                }),
                anchor_x: 0.0,
            },
            y_offset: 10.0,
        };
        app.update_anchor();
        app
    }

    fn scroll(&mut self, delta: f32) {
        // TODO: use actual line height
        self.y_offset += delta * 18.0;

        self.y_offset = self.y_offset.min(10.0);

        let height: f32 = self.vis_forest.iter().map(|t| t.size().1).sum();
        let max_offset = 10.0 - height + self.vis_forest.last().unwrap().last_line_height();
        self.y_offset = self.y_offset.max(max_offset);
    }

    fn update_anchor(&mut self) {
        let mut f = &self.vis_forest;
        for &idx in &self.cur.path {
            match &f[idx] {
                VisTree::Leaf { .. } => panic!(),
                VisTree::Node { children } => f = children,
            }
        }
        self.cur.anchor_x = X_OFFSET + self.cur.path.len() as f32 * INDENT
            + f[self.cur.pos.line].cursor_coord(self.cur.pos.pos).x;
    }

    // If the cursor is on a VisTree::Node and not on a line of text
    // (which should only happen when selecting),
    // move it to the inner line of text.
    fn sink_cursor(&mut self) {
        self.cur.sel = None;
        let mut f = &self.vis_forest;
        for &idx in &self.cur.path {
            f = match &f[idx] {
                VisTree::Node { children } => children,
                VisTree::Leaf { .. } => panic!(),
            }
        }
        match &f[self.cur.pos.line] {
            VisTree::Leaf { .. } => {}
            t @ VisTree::Node { .. } => {
                match self.cur.pos.pos {
                    0 => {
                        self.cur.path.push(self.cur.pos.line);
                        self.cur.pos = TextPos { line: 0, pos: 0 };
                    }
                    1 => {
                        let mut t = t;
                        self.cur.path.push(self.cur.pos.line);
                        loop {
                            match t {
                                VisTree::Node { children } => {
                                    self.cur.path.push(children.len() - 1);
                                    t = children.last().unwrap();
                                }
                                VisTree::Leaf { layout } => {
                                    self.cur.pos = TextPos {
                                        line: self.cur.path.pop().unwrap(),
                                        pos: layout.text.len(),
                                    };
                                    break;
                                }
                            }
                        }
                    }
                    _ => panic!("{:?}", self.cur),
                }
            }
        }
    }

    fn left(&mut self) {
        self.sink_cursor();
        let mut f = &self.vis_forest;
        for &idx in &self.cur.path {
            match &f[idx] {
                VisTree::Leaf { .. } => panic!(),
                VisTree::Node { children } => f = children,
            }
        }
        match &f[self.cur.pos.line] {
            VisTree::Node { .. } => panic!("should have been handled by sink_cursor()"),
            VisTree::Leaf { layout } => {
                if let Some(pos) = prev_char_pos(&layout.text, self.cur.pos.pos) {
                    self.cur.pos.pos = pos;
                    return;
                }
                if let Some(prev_leaf) = prev_leaf(
                    &self.vis_forest, &mut self.cur.path, &mut self.cur.pos.line) {
                    self.cur.pos.pos = match prev_leaf {
                        VisTree::Leaf { layout } => layout.text.len(),
                        VisTree::Node { .. } => panic!(),
                    };
                }
            }
        }
    }

    fn right(&mut self) {
        self.sink_cursor();
        let mut f = &self.vis_forest;
        for &idx in &self.cur.path {
            match &f[idx] {
                VisTree::Leaf { .. } => panic!(),
                VisTree::Node { children } => f = children,
            }
        }
        match &f[self.cur.pos.line] {
            VisTree::Node { .. } => panic!("should have been handled by sink_cursor()"),
            VisTree::Leaf { layout } => {
                if let Some(pos) = next_char_pos(&layout.text, self.cur.pos.pos) {
                    self.cur.pos.pos = pos;
                    return;
                }
                if let Some(_next_leaf) = next_leaf(
                    &self.vis_forest, &mut self.cur.path, &mut self.cur.pos.line) {
                    self.cur.pos.pos = 0;
                }
            }
        }
    }

    fn up(&mut self) {
        self.sink_cursor();
        let mut f = &self.vis_forest;
        for &idx in &self.cur.path {
            match &f[idx] {
                VisTree::Leaf { .. } => panic!(),
                VisTree::Node { children } => f = children,
            }
        }
        match &f[self.cur.pos.line] {
            VisTree::Node { .. } => panic!("should have been handled by sink_cursor()"),
            leaf @ VisTree::Leaf { layout } => {
                let eps = 3.0;
                let cc = leaf.cursor_coord(self.cur.pos.pos);
                if cc.top - eps > 0.0 {
                    let x = self.cur.anchor_x
                        - (X_OFFSET + INDENT * self.cur.path.len() as f32);
                    self.cur.pos.pos = layout.coords_to_pos(x, cc.top - eps);
                } else {
                    let prev_leaf = prev_leaf(
                        &self.vis_forest, &mut self.cur.path, &mut self.cur.pos.line);
                    if let Some(prev_leaf) = prev_leaf {
                        let x = self.cur.anchor_x
                            - (X_OFFSET + INDENT * self.cur.path.len() as f32);
                        let y = prev_leaf.size().1 - eps;
                        match prev_leaf {
                            VisTree::Node { .. } => panic!(),
                            VisTree::Leaf { layout } =>
                                self.cur.pos.pos = layout.coords_to_pos(x, y),
                        }
                    }
                }
            }
        }
    }

    fn down(&mut self) {
        self.sink_cursor();
        let mut f = &self.vis_forest;
        for &idx in &self.cur.path {
            match &f[idx] {
                VisTree::Leaf { .. } => panic!(),
                VisTree::Node { children } => f = children,
            }
        }
        match &f[self.cur.pos.line] {
            VisTree::Node { .. } => panic!("should have been handled by sink_cursor()"),
            leaf @ VisTree::Leaf { layout } => {
                let eps = 3.0;
                let cc = leaf.cursor_coord(self.cur.pos.pos);
                if cc.top + cc.height + eps < leaf.size().1 {
                    let x = self.cur.anchor_x
                        - (X_OFFSET + INDENT * self.cur.path.len() as f32);
                    self.cur.pos.pos = layout.coords_to_pos(x, cc.top + cc.height + eps);
                } else {
                    let next_leaf = next_leaf(
                        &self.vis_forest, &mut self.cur.path, &mut self.cur.pos.line);
                    if let Some(next_leaf) = next_leaf {
                        let x = self.cur.anchor_x
                            - (X_OFFSET + INDENT * self.cur.path.len() as f32);
                        let y = eps;
                        match next_leaf {
                            VisTree::Node { .. } => panic!(),
                            VisTree::Leaf { layout } =>
                                self.cur.pos.pos = layout.coords_to_pos(x, y),
                        }
                    }
                }
            }
        }
    }
}

struct MouseClickVisitor {
    x: f32,
    y: f32,
    result: Option<(Vec<usize>, TextPos)>,
}

impl VisTreeVisitor for MouseClickVisitor {
    fn visit(&mut self, path: &[usize], tree: &VisTree, x: f32, y: f32) {
        match tree {
            VisTree::Leaf { layout } => {
                if y <= self.y && self.y <= y + layout.height {
                    let pos = layout.coords_to_pos(self.x - x, self.y - y);
                    let (&line, path) = path.split_last().unwrap();
                    self.result = Some((path.to_owned(), TextPos { line, pos }));
                }
            }
            VisTree::Node { .. } => {}
        }
    }
}

fn paint(app: &mut App) {
    let rt = &app.ctx.render_target;
    unsafe {
        rt.BeginDraw();
        rt.Clear(&D2D1_COLOR_F { r: 0.0, g: 0.0, b: 0.2, a: 1.0 });

        let mut v = DrawVisitor {
            ctx: &app.ctx,
            cur: &app.cur,
        };
        let mut y = app.y_offset;
        VisTree::forest_accept(&app.vis_forest, &mut v, &mut vec![], X_OFFSET, &mut y);

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
        if msg == WM_LBUTTONDOWN {
            let x = GET_X_LPARAM(lparam);
            let y = GET_Y_LPARAM(lparam);
            println!("{} {} {}", win_msg_name(msg), x, y);
            let mut v = MouseClickVisitor {
                x: x as f32,
                y: y as f32,
                result: None,
            };
            let mut app = sr.state_mut();
            let mut yy = app.y_offset;
            VisTree::forest_accept(&app.vis_forest, &mut v, &mut vec![], X_OFFSET, &mut yy);
            if let Some((path, pos)) = v.result {
                app.cur = Cursor {
                    path,
                    pos,
                    sel: None,
                    anchor_x: 0.0,
                };
                app.update_anchor();
                invalidate_rect(hwnd);
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
        None
    }
}

fn main() {
    let app = LazyState::new(App::new);
    let arrow_cursor = load_cursor(IDC_IBEAM);
    unsafe {
        let win_class = win_win::WindowClass::builder("e2 class")
            .cursor(arrow_cursor)
            .build().unwrap();
        let hwnd = win_win::WindowBuilder::new(app, &win_class)
            .name("e2")
            .style(WS_OVERLAPPEDWINDOW)
            .build();
        ShowWindow(hwnd, SW_SHOWNORMAL);
        win_win::runloop(std::ptr::null_mut());
    }
}
