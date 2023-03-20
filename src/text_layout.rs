use std::convert::TryInto;
use wio::com::ComPtr;
use wio::wide::ToWide;
use winapi::shared::minwindef::*;
use winapi::um::dwrite::*;
use winapi::um::d2d1::*;
use winapi::shared::winerror::*;
use crate::win_util::*;
use crate::util::*;

pub struct TextLayout {
    pub raw: ComPtr<IDWriteTextLayout>,
    pub text: String,
    pub height: f32,
    url_rects: Vec<((f32, f32, f32, f32), String)>,
}

pub struct CursorCoord {
    pub x: f32,
    pub top: f32,
    pub height: f32,
}

// text="ab", pos=1, skew=Lefty  means  the cursor is to the right of 'a'
// text="ab", pos=1, skew=Righty  means  the cursor is to the left of 'b'
// This matters if 'a' and 'b' are on different lines because of line wrapping.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Skew {
    Lefty,
    Righty,
}

// Use explicit 'Skew::Righty' when we really care (e.g. in `fn left()`),
// and 'Skew::default()' where it doesn't matter.
impl Default for Skew {
    fn default() -> Self { Skew::Righty }
}

impl TextLayout {
    pub fn new(
        dwrite_factory: &ComPtr<IDWriteFactory>,
        text_format: &ComPtr<IDWriteTextFormat>,
        link_brush: &ComPtr<ID2D1Brush>,
        text: &str,
        max_width: f32,
    ) -> TextLayout {
        let raw = create_text_layout(
            dwrite_factory, &text.to_wide(), text_format, max_width, 0.0);

        let mut url_rects = vec![];
        for (start, end) in find_urls(text) {
            let w_start = wide_len(&text[..start]);  // TODO: not very efficient
            let w_len = wide_len(&text[start..end]);
            let range = DWRITE_TEXT_RANGE {
                startPosition: w_start.try_into().unwrap(),
                length: w_len.try_into().unwrap(),
            };
            let hr = unsafe {
                raw.SetDrawingEffect(link_brush.clone().up().up().as_raw(), range)
            };
            assert!(hr == S_OK, "0x{:x}", hr);
            let hr = unsafe { raw.SetUnderline(TRUE, range) };
            assert!(hr == S_OK, "0x{:x}", hr);

            for rect in hit_test_text_range(&raw, w_start, w_start + w_len) {
                url_rects.push((
                    (rect.left, rect.top, rect.width, rect.height),
                    text[start..end].to_string(),
                ));
            }
        }

        let mut text_metrics = unsafe { std::mem::zeroed() };
        let hr = unsafe { raw.GetMetrics(&mut text_metrics) };
        assert!(hr == S_OK, "0x{:x}", hr);

        TextLayout {
            raw,
            text: text.to_string(),
            height: text_metrics.height,
            url_rects,
        }
    }

    pub fn hit_test_text_range(
        &self,
        start: usize, end: usize,
        include_newline: bool,
    ) -> Vec<DWRITE_HIT_TEST_METRICS> {
        let wide_start = wide_len(&self.text[..start]);
        let wide_end = wide_start + wide_len(&self.text[start..end]);
        let mut res = hit_test_text_range(&self.raw, wide_start, wide_end);
        if include_newline {
            assert_eq!(end, self.text.len());
            let last = res.last_mut().unwrap();
            last.width += last.height * 0.5;
        }
        res
    }

    pub fn cursor_coord(&self, (pos, skew): (usize, Skew)) -> CursorCoord {
        let (pos, is_trailing_hit) = match skew {
            Skew::Righty => (pos, 0),
            Skew::Lefty => (prev_char_pos(&self.text, pos).unwrap(), 1),
        };
        let wide_pos = wide_len(&self.text[..pos]);

        let mut x = 0.0;
        let mut y = 0.0;
        let mut metrics = unsafe { std::mem::zeroed() };
        let hr = unsafe {
            self.raw.HitTestTextPosition(
                wide_pos.try_into().unwrap(),
                is_trailing_hit,
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

    pub fn coords_to_pos(&self, x: f32, y: f32) -> (usize, Skew) {
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

        let pos = wide_pos_to_pos(metrics.textPosition.try_into().unwrap(), &self.text);
        match is_trailing_hit {
            0 => (pos, Skew::Righty),
            1 => (next_char_pos(&self.text, pos).unwrap(), Skew::Lefty),
            _ => panic!(),
        }
    }

    pub fn hover_url(&self, x: f32, y: f32) -> Option<&str> {
        for (rect, url) in &self.url_rects {
            let (left, top, width, height) = *rect;
            if left <= x && x <= left + width && top <= y && y <= top + height {
                return Some(url);
            }
        }
        None
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
