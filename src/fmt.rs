use std::fmt::{Display, Error, Formatter, Result, Write};

use crate::Token;

pub struct FormatId<'a> {
    bytes: &'a [u8],
}

impl<'a> FormatId<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self { bytes }
    }
}

impl Display for FormatId<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if is_valid_id(self.bytes) {
            return f.write_str(std::str::from_utf8(self.bytes).map_err(|_| Error)?);
        }
        f.write_str("@\"")?;
        FormatEscapes::double_quoted(self.bytes).fmt(f)?;
        f.write_char('"')
    }
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

pub struct FormatEscapes<'a> {
    bytes: &'a [u8],
    single_quoted: bool,
}

impl<'a> FormatEscapes<'a> {
    pub fn double_quoted(bytes: &'a [u8]) -> Self {
        Self {
            bytes,
            single_quoted: false,
        }
    }

    pub fn single_quoted(bytes: &'a [u8]) -> Self {
        Self {
            bytes,
            single_quoted: true,
        }
    }
}

impl Display for FormatEscapes<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for &byte in self.bytes {
            match byte {
                b'\n' => f.write_str("\\n")?,
                b'\r' => f.write_str("\\r")?,
                b'\t' => f.write_str("\\t")?,
                b'\\' => f.write_str("\\\\")?,
                b'"' => {
                    if self.single_quoted {
                        f.write_char('"')?;
                    } else {
                        f.write_str("\\\"")?;
                    }
                }
                b'\'' => {
                    if self.single_quoted {
                        f.write_str("\\'")?;
                    } else {
                        f.write_char('\'')?;
                    }
                }
                b' ' | b'!' | b'#'..=b'&' | b'('..=b'[' | b']'..=b'~' => {
                    f.write_char(byte as char)?;
                }
                // Use hex escapes for rest any unprintable characters.
                _ => {
                    f.write_str("\\x")?;
                    write!(f, "{byte:02x}")?;
                }
            }
        }
        Ok(())
    }
}

#[test]
fn test_escape_invalid_identifiers() {
    assert_eq!("@\"while\"", format!("{}", FormatId::new(b"while")));
    assert_eq!("hello", format!("{}", FormatId::new(b"hello")));
    assert_eq!("@\"11\\\"23\"", format!("{}", FormatId::new(b"11\"23")));
    assert_eq!("@\"11\\x0f23\"", format!("{}", FormatId::new(b"11\x0F23")));

    assert_eq!(
        "\\x0f",
        format!("{}", FormatEscapes::double_quoted(b"\x0F"))
    );
    assert_eq!(
        r#"" \\ hi \x07 \x11 " derp \'""#,
        format!(
            "\"{}\"",
            FormatEscapes::single_quoted(b" \\ hi \x07 \x11 \" derp '")
        )
    );
    assert_eq!(
        r#"" \\ hi \x07 \x11 \" derp '""#,
        format!(
            "\"{}\"",
            FormatEscapes::double_quoted(b" \\ hi \x07 \x11 \" derp '")
        )
    );
}
