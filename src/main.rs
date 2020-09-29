mod win_util;
pub mod win_win_reent;

use std::ptr::null_mut;
use wio::com::ComPtr;
use winapi::shared::windef::*;
use winapi::shared::minwindef::*;
use winapi::shared::winerror::*;
use winapi::um::winuser::*;
use winapi::um::d2d1::*;
use win_win_reent::*;
use win_util::*;

struct App {
    render_target: ComPtr<ID2D1HwndRenderTarget>,
}

impl App {
    fn new(hwnd: HWND) -> Self {
        let d2d_factory = create_d2d_factory();
        let render_target = create_hwnd_render_target(&d2d_factory, hwnd);
        App {
            render_target,
        }        
    }
}

fn paint(app: &mut App) {
    let rt = &app.render_target;
    unsafe {
        rt.BeginDraw();
        rt.Clear(&D2D1_COLOR_F { r: 0.0, g: 0.0, b: 0.2, a: 1.0 });

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
