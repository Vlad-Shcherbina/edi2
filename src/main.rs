mod win_util;
pub mod win_win_reent;

use winapi::shared::windef::*;
use winapi::shared::minwindef::*;
use winapi::um::winuser::*;
use win_win_reent::*;

struct App;

impl WindowProcState for App {
    #[allow(unused_variables)]
    fn window_proc(
        sr: StateRef<Self>,
        hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM,
    )-> Option<LRESULT> {
        if msg == WM_DESTROY {
            eprintln!("{}", win_msg_name::win_msg_name(msg));
            unsafe {
                PostQuitMessage(0);
            }
        }        
        None
    }
}

fn main() {
    let app = OuterState::new(App);
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
