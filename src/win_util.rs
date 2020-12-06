#![allow(unused_imports)]  // TODO: remove and clean up
#![allow(dead_code)]
use std::ptr::{null, null_mut};
use std::io::Error;
use std::convert::TryInto;

use winapi::shared::minwindef::{HINSTANCE, LPARAM, LRESULT, UINT, WPARAM};
use winapi::shared::windef::HWND;
use winapi::um::winuser::*;
use winapi::um::winnt::LPCWSTR;
use winapi::ctypes::c_int;

use winapi::Interface;
use winapi::shared::minwindef::*;
use winapi::shared::windef::*;
use winapi::shared::winerror::*;
use winapi::shared::dxgiformat::*;
use winapi::shared::windowsx::*;
use winapi::um::errhandlingapi::*;
use winapi::um::winbase::*;
use winapi::um::winuser::*;
use winapi::um::dcommon::*;
use winapi::um::d2d1::*;
use winapi::um::dwrite::*;
use winapi::um::shellapi::ShellExecuteW;
use winapi::um::d2d1::{
    D2D1_SIZE_U,
    D2D1_POINT_2F,
};

use wio::wide::{ToWide, FromWide};
use wio::com::ComPtr;

use crate::win_win_reent::Reent;


pub fn set_timer(hwnd: HWND, id: usize, interval_ms: u32) -> usize {
    let res = unsafe {
        SetTimer(hwnd, id, interval_ms, None)
    };
    assert!(res != 0, "{}", Error::last_os_error());
    res
}

pub fn set_window_title(hwnd: HWND, title: &str) {
    let res = unsafe {
        SetWindowTextW(hwnd, title.to_wide_null().as_ptr())
    };
    assert!(res != 0, "{}", Error::last_os_error());
}


pub fn shell_execute(
    _reent: Reent,
    hwnd: HWND,
    operation: &str,
    file: &str,
    params: Option<&str>,
    dir: Option<&str>,
    show_cmd: i32,
) {
    let hi = unsafe {
        ShellExecuteW(
            hwnd,
            operation.to_wide_null().as_ptr(),
            file.to_wide_null().as_ptr(),
            params.map_or(null(), |s| s.to_wide_null().as_ptr()),
            dir.map_or(null(), |s| s.to_wide_null().as_ptr()),
            show_cmd,
        )
    } as usize;
    assert!(hi>= 32, "{}", hi);
}

pub fn message_box(
    _reent: Reent,
    hwnd: HWND,
    title: &str,
    message: &str,
    u_type: UINT,
) -> c_int {
    let res = unsafe {
        MessageBoxW(
            hwnd,
            message.to_wide_null().as_ptr(),
            title.to_wide_null().as_ptr(),
            u_type)
    };
    assert!(res != 0, "{}", std::io::Error::last_os_error());
    res
}

pub fn load_cursor(cursor_name: LPCWSTR) -> HCURSOR {
    let res = unsafe { LoadCursorW(0 as HINSTANCE, cursor_name) };
    assert!(!res.is_null(), "{}", std::io::Error::last_os_error());
    res
}

pub fn get_client_rect(hwnd: HWND) -> RECT {
    let mut rc: RECT = unsafe { std::mem::zeroed() };
    let res = unsafe { GetClientRect(hwnd, &mut rc) };
    assert!(res != 0, "{}", Error::last_os_error());
    rc
}

pub fn create_d2d_factory() -> ComPtr<ID2D1Factory> {
    unsafe {
        let factory_options = D2D1_FACTORY_OPTIONS {
            debugLevel: D2D1_DEBUG_LEVEL_NONE,
        };
        let mut d2d_factory = null_mut();
        let hr = D2D1CreateFactory(
            D2D1_FACTORY_TYPE_SINGLE_THREADED,
            &ID2D1Factory::uuidof(),
            &factory_options as *const D2D1_FACTORY_OPTIONS,
            &mut d2d_factory as *mut _ as *mut *mut _,
        );
        assert!(hr == S_OK, "0x{:x}", hr);
        ComPtr::from_raw(d2d_factory)
    }
}

pub fn create_dwrite_factory() -> ComPtr<IDWriteFactory> {
    unsafe {
        let mut dwrite_factory = null_mut();
        let hr = DWriteCreateFactory(
            DWRITE_FACTORY_TYPE_SHARED,
            &IDWriteFactory::uuidof(),
            &mut dwrite_factory,
        );
        assert!(hr == S_OK, "0x{:x}", hr);
        ComPtr::from_raw(dwrite_factory as * mut _)
    }
}

pub fn create_hwnd_render_target(d2d_factory: &ComPtr<ID2D1Factory>, hwnd: HWND) -> ComPtr<ID2D1HwndRenderTarget> {
    unsafe {
        let render_properties = D2D1_RENDER_TARGET_PROPERTIES {
            _type: D2D1_RENDER_TARGET_TYPE_DEFAULT,
            pixelFormat: D2D1_PIXEL_FORMAT {
                format: DXGI_FORMAT_B8G8R8A8_UNORM,
                alphaMode: D2D1_ALPHA_MODE_IGNORE,
            },
            dpiX: 0.0,
            dpiY: 0.0,
            usage: D2D1_RENDER_TARGET_USAGE_NONE,
            minLevel: D2D1_FEATURE_LEVEL_DEFAULT,
        };
        let rc = get_client_rect(hwnd);
        let hwnd_render_properties = D2D1_HWND_RENDER_TARGET_PROPERTIES {
            hwnd,
            pixelSize: D2D1_SIZE_U {
                width: (rc.right - rc.left) as u32,
                height: (rc.bottom - rc.top) as u32,
            },
            presentOptions: D2D1_PRESENT_OPTIONS_NONE,
        };
        dbg!(hwnd_render_properties.pixelSize.width);
        dbg!(hwnd_render_properties.pixelSize.height);
        let mut render_target = null_mut();
        let hr = d2d_factory.CreateHwndRenderTarget(
            &render_properties,
            &hwnd_render_properties,
            &mut render_target,
        );
        assert!(hr == S_OK, "0x{:x}", hr);
        ComPtr::from_raw(render_target)
    }
}

pub fn create_solid_brush(rt: &ComPtr<ID2D1HwndRenderTarget>, color: &D2D1_COLOR_F) -> ComPtr<ID2D1Brush> {
    unsafe {
        let mut brush = null_mut();
        let hr = rt.CreateSolidColorBrush(color, null(), &mut brush);
        assert!(hr == S_OK, "0x{:x}", hr);
        ComPtr::from_raw(brush)
    }.up()
}

pub fn create_text_format(
    dwrite_factory: &ComPtr<IDWriteFactory>,
    font_family_name: &str,
    size: f32,
    bold: bool,
) -> ComPtr<IDWriteTextFormat> {
    unsafe {
        let mut text_format = null_mut();
        let hr = dwrite_factory.CreateTextFormat(
            font_family_name.to_wide_null().as_ptr(),
            null_mut(),
            if bold { DWRITE_FONT_WEIGHT_BOLD }
            else { DWRITE_FONT_WEIGHT_REGULAR },
            DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_STRETCH_NORMAL,
            size,
            "en-us".to_wide_null().as_ptr(),
            &mut text_format,
        );
        assert!(hr == S_OK, "0x{:x}", hr);
        ComPtr::from_raw(text_format)
    }
}

pub fn create_text_layout(
    dwrite_factory: &ComPtr<IDWriteFactory>,
    text: &[u16],
    text_format: &ComPtr<IDWriteTextFormat>,
    max_width: f32,
    max_height: f32,
) -> ComPtr<IDWriteTextLayout> {
    unsafe {
        let mut text_layout = null_mut();
        let hr = dwrite_factory.CreateTextLayout(
            text.as_ptr(),
            text.len() as u32,
            text_format.as_raw(),
            max_width,
            max_height,
            &mut text_layout,
        );
        assert!(hr == S_OK, "0x{:x}", hr);
        ComPtr::from_raw(text_layout)
    }
}

pub fn hit_test_text_range(
    text_layout: &ComPtr<IDWriteTextLayout>,
    start: usize, end: usize,
) -> Vec<DWRITE_HIT_TEST_METRICS> {
    assert!(start <= end, "{} {}", start, end);
    let len = end - start;
    let mut metrics = vec![unsafe { std::mem::zeroed() }; 1];
    let mut actual_count = 0;
    let mut hr = unsafe {
        text_layout.HitTestTextRange(
            start.try_into().unwrap(), len.try_into().unwrap(),
            0.0, 0.0,  // origin
            metrics.as_mut_ptr(),
            metrics.len() as u32,
            &mut actual_count,
        )
    };
    if hr == HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER) {
        metrics.resize(actual_count as usize, unsafe { std::mem::zeroed() });
        hr = unsafe {
            text_layout.HitTestTextRange(
                start.try_into().unwrap(), len.try_into().unwrap(),
                0.0, 0.0,  // origin
                metrics.as_mut_ptr(),
                metrics.len() as u32,
                &mut actual_count,
            )
        };
    }

    assert!(hr == S_OK, "0x{:x}", hr);
    metrics.truncate(actual_count as usize);

    metrics
}

pub fn invalidate_rect(hwnd: HWND) {
    unsafe {
        let res = InvalidateRect(hwnd, null(), 1);
        assert!(res != 0, "{}", Error::last_os_error());
    }
}

pub fn get_wheel_scroll_lines() -> u32 {
    let mut scroll_lines: UINT = 0;
    let res = unsafe {
        SystemParametersInfoW(
            SPI_GETWHEELSCROLLLINES,
            0,
            &mut scroll_lines as *mut _ as *mut _,
            0)};
    assert!(res != 0, "{}", std::io::Error::last_os_error());
    scroll_lines    
}

pub fn get_clipboard_sequence_number() -> u32 {
    unsafe { GetClipboardSequenceNumber() }
}

// Why not just write `p as *mut T2`?
// Because then when casting from say *mut void to *mut u16,
// Clippy complains about pointer alignment
// https://rust-lang.github.io/rust-clippy/master/index.html#cast_ptr_alignment
fn cast_ptr<T1, T2>(p: *mut T1) -> *mut T2 {
    assert!(p as usize % std::mem::align_of::<T2>() == 0);
    p as *mut T2
}

pub struct ClipboardManager;

impl ClipboardManager {
    pub fn open(hwnd: HWND) -> Self {
        let res = unsafe { OpenClipboard(hwnd) };
        assert!(res != 0, "{}", Error::last_os_error());
        Self
    }

    pub fn empty(&mut self, _reent: Reent) {
        let res = unsafe { EmptyClipboard() };
        assert!(res != 0, "{}", Error::last_os_error());
    }

    pub fn set_private(&mut self) {
        let _res = unsafe { SetClipboardData(CF_PRIVATEFIRST, null_mut()) };
        // can't just check res.is_null(), because it's null on success too
        let e = unsafe { GetLastError() };
        let hr = HRESULT_FROM_WIN32(e);
        assert_eq!(hr, S_OK, "{}", Error::last_os_error());
    }

    pub fn has_private(&mut self) -> bool {
        unsafe {
            IsClipboardFormatAvailable(CF_PRIVATEFIRST) != 0
        }
    }

    pub fn set_text(&mut self, s: &str) {
        let data = s.to_wide_null();
        unsafe {
            let h = GlobalAlloc(GMEM_MOVEABLE, data.len() * 2);
            assert!(!h.is_null(), "{}", Error::last_os_error());

            let pdata: *mut u16 = cast_ptr(GlobalLock(h));
            assert!(!pdata.is_null());
            for (i, c) in data.into_iter().enumerate() {
                *pdata.add(i) = c;
            }
            let res = GlobalUnlock(pdata as *mut _);
            if res == 0 {
                let e = Error::last_os_error();
                assert!(e.raw_os_error() == Some(0), "{}", e);
            }

            let res = SetClipboardData(CF_UNICODETEXT, h);
            assert!(!res.is_null(), "{}", Error::last_os_error());
        }
    }

    pub fn get_text(&mut self) -> Option<String> {
        unsafe {
            let h = GetClipboardData(CF_UNICODETEXT);
            if h.is_null() {
                let e = Error::last_os_error();
                assert!(e.raw_os_error() == Some(0), "{}", e);
                return None;
            }
            let pdata: *mut u16 = cast_ptr(GlobalLock(h));
            assert!(!pdata.is_null());
            let mut data = Vec::new();
            let mut pos = 0;
            while *pdata.offset(pos) != 0 {
                data.push(*pdata.offset(pos));
                pos += 1;
            }
            let s = std::ffi::OsString::from_wide(&data);
            let s = s.into_string().unwrap();
            let res = GlobalUnlock(pdata as *mut _);
            if res == 0 {
                let e = Error::last_os_error();
                assert!(e.raw_os_error() == Some(0), "{}", e);
            }
            Some(s.replace("\r\n", "\n"))
        }        
    }

    pub fn close(self) {
        let res = unsafe { CloseClipboard() };
        assert!(res != 0, "{}", Error::last_os_error());
        std::mem::forget(self);
    }
}

impl Drop for ClipboardManager {
    fn drop(&mut self) {
        panic!("did you forget to .close()?");
    }
}
