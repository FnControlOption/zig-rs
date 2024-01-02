/// Same as [`<[u8]>::trim_ascii_start()`](https://doc.rust-lang.org/std/primitive.slice.html#method.trim_ascii_start)
#[inline]
pub const fn trim_ascii_start(bytes: &[u8]) -> &[u8] {
    let mut bytes = bytes;
    while let [first, rest @ ..] = bytes {
        if first.is_ascii_whitespace() {
            bytes = rest;
        } else {
            break;
        }
    }
    bytes
}

/// Same as [`<[u8]>::trim_ascii_end()`](https://doc.rust-lang.org/std/primitive.slice.html#method.trim_ascii_end)
#[inline]
pub const fn trim_ascii_end(bytes: &[u8]) -> &[u8] {
    let mut bytes = bytes;
    while let [rest @ .., last] = bytes {
        if last.is_ascii_whitespace() {
            bytes = rest;
        } else {
            break;
        }
    }
    bytes
}

/// Similar to [`str::find()`](https://doc.rust-lang.org/std/primitive.str.html#method.find)
pub fn find<T: PartialEq>(haystack: &[T], needle: &[T]) -> Option<usize> {
    haystack.windows(needle.len()).position(|s| s == needle)
}

pub fn find_scalar<T: PartialEq>(haystack: &[T], value: T) -> Option<usize> {
    haystack.iter().position(|b| *b == value)
}

/// Similar to [`str::contains()`](https://doc.rust-lang.org/std/primitive.str.html#method.contains)
pub fn contains<T: PartialEq>(haystack: &[T], needle: &[T]) -> bool {
    haystack.windows(needle.len()).any(|s| s == needle)
}

pub fn contains_at_least<T: PartialEq>(
    haystack: &[T],
    expected_count: usize,
    needle: &[T],
) -> bool {
    haystack
        .windows(needle.len())
        .filter(|&s| s == needle)
        .take(expected_count)
        .count()
        == expected_count
}
