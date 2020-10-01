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
