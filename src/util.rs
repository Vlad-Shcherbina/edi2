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

pub fn prev_word_pos(s: &str, mut pos: usize) -> Option<usize> {
    pos = prev_char_pos(s, pos)?;
    while pos > 0 {
        if char_before(s, pos).unwrap().is_whitespace() &&
           !char_after(s, pos).unwrap().is_whitespace() {
            break;
        }
        pos = prev_char_pos(s, pos).unwrap();
    }
    Some(pos)
}

pub fn next_word_pos(s: &str, mut pos: usize) -> Option<usize> {
    pos = next_char_pos(s, pos)?;
    while pos < s.len() {
        if !char_before(s, pos).unwrap().is_whitespace() &&
           char_after(s, pos).unwrap().is_whitespace() {
            break;
        }
        pos = next_char_pos(s, pos).unwrap();
    }
    Some(pos)
}

fn char_after(s: &str, pos: usize) -> Option<char> {
    s[pos..].chars().next()
}

fn char_before(s: &str, pos: usize) -> Option<char> {
    s[..pos].chars().rev().next()    
}