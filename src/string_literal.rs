#[derive(Debug)]
pub enum Error {
    /// The character after backslash is missing or not recognized.
    InvalidEscapeCharacter(usize),
    /// Expected hex digit at this index.
    ExpectedHexDigit(usize),
    /// Unicode escape sequence had no digits with rbrace at this index.
    EmptyUnicodeEscapeSequence(usize),
    /// Expected hex digit or '}' at this index.
    ExpectedHexDigitOrRBrace(usize),
    /// Invalid unicode codepoint at this index.
    InvalidUnicodeCodepoint(usize),
    /// Expected '{' at this index.
    ExpectedLBrace(usize),
    /// Expected '}' at this index.
    ExpectedRBrace(usize),
    /// Expected '\'' at this index.
    ExpectedSingleQuote(usize),
    /// The character at this index cannot be represented without an escape sequence.
    InvalidCharacter(usize),
}

pub fn parse_char_literal(slice: &[u8]) -> Result<char, Error> {
    todo!("parse_char_literal")
}

pub fn parse_escape_sequence(slice: &[u8], offset: &mut usize) -> Result<char, Error> {
    debug_assert!(slice.len() > *offset);
    debug_assert_eq!(slice[*offset], b'\\');

    if slice.len() == *offset + 1 {
        return Err(Error::InvalidEscapeCharacter(*offset + 1));
    }

    *offset += 2;
    match slice[*offset - 1] {
        b'n' => return Ok('\n'),
        b'r' => return Ok('\r'),
        b'\\' => return Ok('\\'),
        b't' => return Ok('\t'),
        b'\'' => return Ok('\''),
        b'"' => return Ok('"'),
        b'x' => {
            let mut value: u8 = 0;
            let end: usize = *offset + 2;
            for i in *offset..end {
                if i == slice.len() {
                    return Err(Error::ExpectedHexDigit(i));
                }

                let c = slice[i];
                match c {
                    b'0'..=b'9' => {
                        value *= 16;
                        value += c - b'0';
                    }
                    b'a'..=b'f' => {
                        value *= 16;
                        value += c - b'a' + 10;
                    }
                    b'A'..=b'F' => {
                        value *= 16;
                        value += c - b'A' + 10;
                    }
                    _ => {
                        return Err(Error::ExpectedHexDigit(i));
                    }
                }
            }
            *offset = end;
            return Ok(value as char);
        }
        b'u' => {
            let mut start = *offset;
            if start >= slice.len() || slice[start] != b'{' {
                return Err(Error::ExpectedLBrace(start));
            }
            start += 1;
            if start >= slice.len() {
                return Err(Error::ExpectedHexDigitOrRBrace(start));
            }
            if slice[start] == b'}' {
                return Err(Error::EmptyUnicodeEscapeSequence(start));
            }

            let mut value: u32 = 0;
            let end: usize = slice.len();
            for i in start..end {
                let c = slice[i];
                match c {
                    b'0'..=b'9' => {
                        value *= 16;
                        value += (c - b'0') as u32;
                    }
                    b'a'..=b'f' => {
                        value *= 16;
                        value += (c - b'a' + 10) as u32;
                    }
                    b'A'..=b'F' => {
                        value *= 16;
                        value += (c - b'A' + 10) as u32;
                    }
                    b'}' => {
                        *offset = i + 1;
                        return char::from_u32(value).ok_or(Error::InvalidUnicodeCodepoint(i));
                    }
                    _ => {
                        return Err(Error::ExpectedHexDigitOrRBrace(i));
                    }
                }
                if value > 0x10ffff {
                    return Err(Error::InvalidUnicodeCodepoint(i));
                }
            }
            return Err(Error::ExpectedRBrace(end));
        }
        _ => return Err(Error::InvalidEscapeCharacter(*offset - 1)),
    }
}
