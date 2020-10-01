use std::convert::TryInto;
use wio::com::ComPtr;
use wio::wide::ToWide;
use winapi::um::dwrite::*;
use winapi::shared::winerror::*;
use crate::win_util::*;

pub struct TextLayout {
    pub raw: ComPtr<IDWriteTextLayout>,
    pub text: String,
    pub height: f32,
}

pub struct CursorCoord {
    pub x: f32,
    pub top: f32,
    pub height: f32,
}

impl TextLayout {
    pub fn new(
        dwrite_factory: &ComPtr<IDWriteFactory>,
        text_format: &ComPtr<IDWriteTextFormat>,
        text: &str,
        max_width: f32,
    ) -> TextLayout {
        let raw = create_text_layout(
            dwrite_factory, &text.to_wide(), text_format, max_width, 0.0);

        let mut text_metrics = unsafe { std::mem::zeroed() };
        let hr = unsafe { raw.GetMetrics(&mut text_metrics) };
        assert!(hr == S_OK, "0x{:x}", hr);

        TextLayout {
            raw,
            text: text.to_string(),
            height: text_metrics.height,
        }
    }

    pub fn hit_test_text_range(
        &self,
        start: usize, end: usize,
        include_newline: bool,
    ) -> Vec<DWRITE_HIT_TEST_METRICS> {
        let wide_start = wide_len(&self.text[..start]);
        let wide_end = wide_len(&self.text[start..end]);
        let mut res = hit_test_text_range(&self.raw, wide_start, wide_end);
        if include_newline {
            assert_eq!(end, self.text.len());
            let last = res.last_mut().unwrap();
            last.width += last.height * 0.5;
        }
        res
    }

    pub fn cursor_coord(&self, pos: usize) -> CursorCoord {
        let wide_pos = wide_len(&self.text[..pos]);

        let mut x = 0.0;
        let mut y = 0.0;
        let mut metrics = unsafe { std::mem::zeroed() };
        let hr = unsafe {
            self.raw.HitTestTextPosition(
                wide_pos.try_into().unwrap(),
                0,  // isTrailingHit
                &mut x, &mut y,
                &mut metrics,
            )
        };
        assert!(hr == S_OK, "0x{:x}", hr);
        CursorCoord {
            x,
            top: y,
            height: metrics.height,
        }
    }

    pub fn coords_to_pos(&self, x: f32, y: f32) -> usize {
        let mut is_trailing_hit = 0;
        let mut is_inside = 0;
        let mut metrics = unsafe { std::mem::zeroed() };
        let hr = unsafe {
            self.raw.HitTestPoint(
                x, y,
                &mut is_trailing_hit,
                &mut is_inside,
                &mut metrics,
            )
        };
        assert!(hr == S_OK, "0x{:x}", hr);
        let wide_pos = metrics.textPosition as usize + is_trailing_hit as usize;

        wide_pos_to_pos(wide_pos, &self.text)
    }    
}

fn wide_len(s: &str) -> usize {
    s.chars().map(char::len_utf16).sum()   
}

fn wide_pos_to_pos(wide_pos: usize, s: &str) -> usize {
    let mut w = 0;
    let mut pos = 0;
    for c in s.chars() {
        if wide_pos == w {
            return pos;
        }
        w += c.len_utf16();
        pos += c.len_utf8();
    }
    if wide_pos == w {
        return pos;
    }
    panic!("{:?} {}", s, wide_pos);
}
