use std::collections::HashSet;

/// Set of primitive type and value names.
/// Does not include `_` or integer type names.
pub fn names() -> &'static HashSet<&'static [u8]> {
    let list = [
        "anyerror",
        "anyframe",
        "anyopaque",
        "bool",
        "c_int",
        "c_long",
        "c_longdouble",
        "c_longlong",
        "c_char",
        "c_short",
        "c_uint",
        "c_ulong",
        "c_ulonglong",
        "c_ushort",
        "comptime_float",
        "comptime_int",
        "f128",
        "f16",
        "f32",
        "f64",
        "f80",
        "false",
        "isize",
        "noreturn",
        "null",
        "true",
        "type",
        "undefined",
        "usize",
        "void",
    ];

    // TODO(zig-rs): use phf, or LazyLock after it is stabilized
    use std::sync::OnceLock;
    static LOCK: OnceLock<HashSet<&'static [u8]>> = OnceLock::new();
    LOCK.get_or_init(|| HashSet::from_iter(list.map(str::as_bytes)))
}

/// Returns true if a name matches a primitive type or value, excluding `_`.
/// Integer type names like `u8` or `i32` are only matched for syntax,
/// so this will still return true when they have an oversized bit count
/// or leading zeroes.
pub fn is_primitive(name: &[u8]) -> bool {
    if names().contains(name) {
        return true;
    }
    if name.len() < 2 {
        return false;
    }
    let [b'i' | b'u', rest @ ..] = name else {
        return false;
    };
    rest.iter().all(|&c| matches!(c, b'0'..=b'9'))
}

#[test]
fn test_is_primitive() {
    assert_eq!(is_primitive(b""), false);
    assert_eq!(is_primitive(b"_"), false);
    assert_eq!(is_primitive(b"haberdasher"), false);
    assert!(is_primitive(b"bool"));
    assert!(is_primitive(b"false"));
    assert!(is_primitive(b"comptime_float"));
    assert!(is_primitive(b"u1"));
    assert!(is_primitive(b"i99999999999999"));
}
