pub fn prev_char_pos(s: &str, pos: usize) -> Option<usize> {
    if pos == 0 {
        None
    } else {
        Some(s[..pos].char_indices().rev().next().unwrap().0)
    }
}

pub fn next_char_pos(s: &str, pos: usize) -> Option<usize> {
    if pos == s.len() {
        None
    } else {
        Some(pos + s[pos..].chars().next().unwrap().len_utf8())
    }
}
