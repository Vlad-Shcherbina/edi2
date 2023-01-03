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

pub fn find_urls(mut s: &str) -> Vec<(usize, usize)> {
    let mut urls = Vec::new();
    let mut offset = 0;
    loop {
        let start = s.find("http://").into_iter()
            .chain(s.find("https://"))
            .min();
        let Some(start) = start else { break };
        offset += start;
        s = &s[start..];
        let len = s.find(|c: char| c.is_whitespace()).unwrap_or(s.len());
        urls.push((offset, offset + len));
        offset += len;
        s = &s[len..];
    }
    urls
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_find_urls(s: &str, expected: &[&str]) {
        let urls = find_urls(s);
        let actual: Vec<&str> = urls.iter().map(|&(start, end)| &s[start..end]).collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_find_urls() {
        check_find_urls("zzz", &[]);
        check_find_urls("http://example.com", &["http://example.com"]);
        check_find_urls(" http://a", &["http://a"]);
        check_find_urls("hello https://aaa world", &["https://aaa"]);
        check_find_urls("  http://aaa  http://bbb  ", &["http://aaa", "http://bbb"]);
    }
}
