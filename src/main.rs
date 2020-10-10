#![feature(bindings_after_at)]
#![allow(clippy::many_single_char_names)]

mod win_util;
pub mod win_win_reent;
mod text_layout;
pub mod gfx;
mod util;
pub mod owned_ref;
pub mod types;

use std::cell::RefCell;
use std::rc::Rc;
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
use crate::util::*;
use crate::types::*;
use crate::owned_ref::{Owned, Refed};

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

pub struct Cur {
    pub block: Refed<Block>,
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

    y_offset: f32,
    root_block: Owned<Block>,
    cur: Cur,
}

fn expand_block(block: &Refed<Block>) {
    let b = &mut *block.borrow_mut();
    assert!(!b.expanded);
    b.expanded = true;
    assert_eq!(b.children.len(), 1);
    let node = b.node.borrow();
    for line in &node.lines {
        match line.line {
            Line::Text { .. } => b.children.push(BlockChild::Leaf),
            Line::Node { node: ref child_node, .. } => {
                let child_block = Owned::new(Block {
                    expanded: false,
                    depth: b.depth + 1,
                    parent_idx: Some((Refed::clone(block), b.children.len())),
                    node: Rc::clone(child_node),
                    children: vec![BlockChild::Leaf],
                });
                child_node.borrow_mut().blocks.push(child_block.make_ref());
                b.children.push(BlockChild::Block(child_block));
            }
        }
    }
}

fn text_line(text: &str, monospace: bool) -> LineWithLayout {
    LineWithLayout {
        line: Line::Text {
            text: text.to_owned(),
            monospace,
        },
        layout: None,
    }
}
fn node_line(local_header: &str, node: &Rc<RefCell<Node>>) -> LineWithLayout {
    LineWithLayout {
        line: Line::Node {
            local_header: local_header.to_owned(),
            node: Rc::clone(node),
        },
        layout: None,
    }
}

impl App {
    fn new(hwnd: HWND) -> Self {
        let ctx = AppCtx::new(hwnd);

        let node1 = Rc::new(RefCell::new(Node {
            lines: vec![
                text_line("aaaa", false),
            ],
            blocks: vec![],
        }));

        let root_node = Rc::new(RefCell::new(Node {
            lines: vec![
                text_line("Stuff", false),
                text_line("if name == '__main__':", true),
                text_line("    print('hello')", true),
                text_line("Stuff...", false),
                node_line("zzz\nnode", &node1),
                text_line("Stuff.", false),
            ],
            blocks: vec![],
        }));
        let root_block = Owned::new(Block {
            depth: 0,
            parent_idx: None,
            node: Rc::clone(&root_node),
            expanded: false,
            children: vec![BlockChild::Leaf],
        });
        root_node.borrow_mut().blocks.push(root_block.make_ref());

        expand_block(&root_block.make_ref());
        for block in &node1.borrow().blocks {
            expand_block(block);
        }

        let mut app = App {
            ctx,
            cur: Cur {
                block: root_block.make_ref(),
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

    fn scroll(&mut self, delta: f32) {
        // TODO: use actual line height
        self.y_offset += delta * 18.0;
        self.y_offset = self.y_offset.min(10.0);
        let height = self.root_block.borrow().size(&self.ctx).1;
        dbg!(self.root_block.borrow().last_line_height(&self.ctx));
        let max_offset = 10.0 - height + self.root_block.borrow().last_line_height(&self.ctx);
        self.y_offset = self.y_offset.max(max_offset);
    }

    fn update_anchor(&mut self) {
        self.cur.anchor_x = self.cur.block.borrow().abs_cur_x(
            self.cur.line, self.cur.pos, &self.ctx);
    }

    // If the cursor is on a VisTree::Node boundary and not on a line of text
    // (which should only happen when selecting),
    // move it to the inner line of text.
    fn sink_cursor(&mut self) {
        self.cur.sel = None;
        let b = self.cur.block.borrow();
        match b.children[self.cur.line] {
            BlockChild::Leaf => {},
            BlockChild::Block(ref child_block) => {
                match self.cur.pos {
                    0 => {
                        let child_block = child_block.make_ref();
                        drop(b);
                        self.cur.block = child_block;
                        self.cur.line = 0;
                        self.cur.pos = 0;
                    }
                    1 => {
                        let child_block = child_block.make_ref();
                        drop(b);
                        let (block, line) = last_leaf(child_block);
                        self.cur.pos = block.borrow().max_pos(line);
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
        let b = self.cur.block.borrow();
        match b.children[self.cur.line] {
            BlockChild::Block(_) => panic!(),
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line).unwrap();
                let node = node.borrow();
                let text = &node.lines[line_idx].line.text();
                if let Some(pos) = prev_char_pos(text, self.cur.pos) {
                    self.cur.pos = pos;
                    return;
                }
                drop(b);
                let leaf = (Refed::clone(&self.cur.block), self.cur.line);
                if let Some((prev_block, prev_line)) = prev_leaf(leaf) {
                    let b = prev_block.borrow();
                    let (node, line_idx) = b.node_line_idx(prev_line).unwrap();
                    let node = node.borrow();
                    let text = &node.lines[line_idx].line.text();
                    
                    self.cur.pos = text.len();
                    self.cur.line = prev_line;

                    drop(b);
                    self.cur.block = prev_block;
                }
            }
        }
    }

    fn right(&mut self) {
        self.sink_cursor();
        let b = self.cur.block.borrow();
        match b.children[self.cur.line] {
            BlockChild::Block(_) => panic!(),
            BlockChild::Leaf => {
                let (node, line_idx) = b.node_line_idx(self.cur.line).unwrap();
                let node = node.borrow();
                let text = &node.lines[line_idx].line.text();
                if let Some(pos) = next_char_pos(text, self.cur.pos) {
                    self.cur.pos = pos;
                    return;
                }
                drop(b);
                let leaf = (Refed::clone(&self.cur.block), self.cur.line);
                if let Some((next_block, next_line)) = next_leaf(leaf) {
                    self.cur.line = next_line;
                    self.cur.pos = 0;
                    self.cur.block = next_block;
                }
            }
        }
    }

    fn up(&mut self) {
        self.sink_cursor();
        let b = self.cur.block.borrow();
        let (node, line_idx) = b.node_line_idx(self.cur.line).unwrap();
        let mut node = node.borrow_mut();
        let layout = node.line_layout(line_idx, &self.ctx);

        let eps = 3.0;
        let cc = layout.cursor_coord(self.cur.pos);
        if cc.top - eps > 0.0 {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos = layout.coords_to_pos(x, cc.top - eps);
        } else {
            let prev_leaf = prev_leaf((Refed::clone(&self.cur.block), self.cur.line));
            if let Some((prev_block, prev_idx)) = prev_leaf {
                drop(node);
                drop(b);
                let b = prev_block.borrow();

                let y = b.size(&self.ctx).1 - eps;

                let (node, line_idx) = b.node_line_idx(prev_idx).unwrap();
                let mut node = node.borrow_mut();
                let layout = node.line_layout(line_idx, &self.ctx);

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);

                self.cur.line = prev_idx;
                self.cur.pos = layout.coords_to_pos(x, y);
                drop(b);
                self.cur.block = prev_block;
            }
        }
    }

    fn down(&mut self) {
        self.sink_cursor();
        let b = self.cur.block.borrow();
        let (node, line_idx) = b.node_line_idx(self.cur.line).unwrap();
        let mut node = node.borrow_mut();
        let layout = node.line_layout(line_idx, &self.ctx);

        let eps = 3.0;
        let cc = layout.cursor_coord(self.cur.pos);
        if cc.top + cc.height + eps < layout.height {
            let x = self.cur.anchor_x
                - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);
            self.cur.pos = layout.coords_to_pos(x, cc.top + cc.height + eps);
        } else {
            let next_leaf = next_leaf((Refed::clone(&self.cur.block), self.cur.line));
            if let Some((next_block, next_idx)) = next_leaf {
                drop(node);
                drop(b);
                let b = next_block.borrow();

                let y = eps;

                let (node, line_idx) = b.node_line_idx(next_idx).unwrap();
                let mut node = node.borrow_mut();
                let layout = node.line_layout(line_idx, &self.ctx);

                let x = self.cur.anchor_x
                    - (gfx::X_OFFSET + gfx::INDENT * b.depth as f32);

                self.cur.line = next_idx;
                self.cur.pos = layout.coords_to_pos(x, y);
                drop(b);
                self.cur.block = next_block;
            }
        }
    }

    fn put_char(&mut self, c: char) {
        assert!(self.cur.sel.is_none(), "TODO");

        let (node, line_idx) = self.cur.block.borrow().node_line_idx(self.cur.line).unwrap();
        let mut node = node.borrow_mut();
        let line = &mut node.lines[line_idx];
        let text = line.line.text_mut();

        text.insert(self.cur.pos, c);
        self.cur.pos += c.len_utf8();

        line.layout = None;
    }

    fn enter(&mut self) {
        assert!(self.cur.sel.is_none(), "TODO");

        let mut b = self.cur.block.borrow_mut();
        let (node, line_idx) = b.node_line_idx(self.cur.line).unwrap();
        let mut node = node.borrow_mut();
        let line = &mut node.lines[line_idx];
        let text = line.line.text_mut();

        let tail = text[self.cur.pos..].to_owned();
        text.truncate(self.cur.pos);
        line.layout = None;

        if self.cur.line == 0 {
            drop(node);

            b.node.borrow_mut().lines.insert(0, LineWithLayout {
                line: Line::Text { text: tail, monospace: false },
                layout: None,
            });

            assert!(b.expanded, "TODO");
            b.children.insert(1, BlockChild::Leaf);

            // update parent idx for all children
            for child in &mut b.children[1..] {
                match child {
                    BlockChild::Leaf => {},
                    BlockChild::Block(b) => {
                        b.borrow_mut().parent_idx.as_mut().unwrap().1 += 1;
                    }
                }
            }
            self.cur.line += 1;
            self.cur.pos = 0;
        } else {
            let monospace = match line.line {
                Line::Text { monospace, .. } => monospace,
                Line::Node { .. } => panic!(),
            };

            node.lines.insert(line_idx + 1, LineWithLayout {
                line: Line::Text { text: tail, monospace },
                layout: None,
            });

            self.cur.line += 1;
            self.cur.pos = 0;
            b.children.insert(self.cur.line, BlockChild::Leaf);

            // update parent idx for all children
            for child in &mut b.children[self.cur.line + 1..] {
                match child {
                    BlockChild::Leaf => {},
                    BlockChild::Block(b) => {
                        b.borrow_mut().parent_idx.as_mut().unwrap().1 += 1;
                    }
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

        let mut v = gfx::DrawVisitor {
            ctx: &app.ctx,
            cur: &app.cur,
        };
        let mut y = app.y_offset;
        gfx::accept_block(&app.root_block, &mut v, &mut y, &app.ctx);

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
            // TODO: all Refed<> in app state should be cleaned up,
            // otherwise Owned<> destructors will report dangling refs.
            std::process::exit(0);
            // unsafe {
            //     PostQuitMessage(0);
            // }
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
            let mut app = sr.state_mut();
            let mut v = gfx::MouseClickVisitor {
                x: x as f32,
                y: y as f32,
                ctx: &app.ctx,
                result: None,
            };
            let mut yy = app.y_offset;
            gfx::accept_block(&app.root_block, &mut v, &mut yy, &app.ctx);
            if let Some((block, line, pos)) = v.result {
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
