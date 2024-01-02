use std::io::{Error, Result, Write};

use crate::Token;

pub fn fmt_id(writer: &mut dyn Write, bytes: &[u8]) -> Result<()> {
    if is_valid_id(bytes) {
        return writer.write_all(bytes);
    }
    write!(writer, "@\"")?;
    fmt_escapes(writer, bytes, Quotes::Double)?;
    write!(writer, "\"")
}

pub fn is_valid_id(bytes: &[u8]) -> bool {
    if bytes.len() == 0 {
        return false;
    }
    if bytes == b"_" {
        return false;
    }
    for (i, &c) in bytes.iter().enumerate() {
        match c {
            b'_' | b'a'..=b'z' | b'A'..=b'Z' => {}
            b'0'..=b'9' if i == 0 => return false,
            b'0'..=b'9' => {}
            _ => return false,
        }
    }
    Token::get_keyword(bytes).is_none()
}

#[test]
fn test_is_valid_id() {
    assert!(!is_valid_id(b""));
    assert!(is_valid_id(b"foobar"));
    assert!(!is_valid_id(b"a b c"));
    assert!(!is_valid_id(b"3d"));
    assert!(!is_valid_id(b"enum"));
    assert!(is_valid_id(b"i386"));
}

pub enum Quotes {
    Single,
    Double,
}

pub fn fmt_escapes(writer: &mut dyn Write, bytes: &[u8], quotes: Quotes) -> Result<()> {
    for &byte in bytes {
        match byte {
            b'\n' => write!(writer, "\\n")?,
            b'\r' => write!(writer, "\\r")?,
            b'\t' => write!(writer, "\\t")?,
            b'\\' => write!(writer, "\\\\")?,
            b'"' => match quotes {
                Quotes::Single => write!(writer, "\"")?,
                Quotes::Double => write!(writer, "\\\"")?,
            },
            b'\'' => match quotes {
                Quotes::Single => write!(writer, "\\'")?,
                Quotes::Double => write!(writer, "'")?,
            },
            b' ' | b'!' | b'#'..=b'&' | b'('..=b'[' | b']'..=b'~' => {
                writer.write_all(&[byte])?;
            }
            // Use hex escapes for rest any unprintable characters.
            _ => write!(writer, "\\x{byte:02x}")?,
        }
    }
    Ok(())
}

#[track_caller]
fn assert_fmt_id(expected: &str, input: &str) {
    let mut buffer = Vec::new();
    fmt_id(&mut buffer, input.as_bytes()).unwrap();
    assert_eq!(expected.as_bytes(), buffer.as_slice());
}

#[track_caller]
fn assert_fmt_escapes(expected: &str, quotes: Quotes, input: &str) {
    let mut buffer = Vec::new();
    fmt_escapes(&mut buffer, input.as_bytes(), quotes).unwrap();
    assert_eq!(expected.as_bytes(), buffer.as_slice());
}

#[test]
fn test_escape_invalid_identifiers() {
    assert_fmt_id("@\"while\"", "while");
    assert_fmt_id("hello", "hello");
    assert_fmt_id("@\"11\\\"23\"", "11\"23");
    assert_fmt_id("@\"11\\x0f23\"", "11\x0F23");

    assert_fmt_escapes("\\x0f", Quotes::Double, "\x0F");
    assert_fmt_escapes(
        r#" \\ hi \x07 \x11 " derp \'"#,
        Quotes::Single,
        " \\ hi \x07 \x11 \" derp '",
    );
    assert_fmt_escapes(
        r#" \\ hi \x07 \x11 \" derp '"#,
        Quotes::Double,
        " \\ hi \x07 \x11 \" derp '",
    );
}
