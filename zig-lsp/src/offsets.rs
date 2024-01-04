// based on https://github.com/zigtools/zls/blob/master/src/offsets.zig

use zig::ast::{ByteOffset, TokenIndex};

use super::*;

pub fn offset_to_position(
    text: &[u8],
    index: ByteOffset,
    encoding: &PositionEncodingKind,
) -> Position {
    index_to_position(text, index as usize, encoding)
}

pub fn index_to_position(text: &[u8], index: usize, encoding: &PositionEncodingKind) -> Position {
    let last_line_start = text[0..index]
        .iter()
        .rposition(|&c| c == b'\n')
        .map(|line| line + 1)
        .unwrap_or(0);

    let line_count = text[0..last_line_start]
        .iter()
        .filter(|&&c| c == b'\n')
        .count();

    Position {
        line: line_count as u32,
        // TODO: handle position encoding
        // character: count_code_units(&text[last_line_start..index], encoding) as u32,
        character: (index - last_line_start) as u32,
    }
}

pub fn position_to_index(
    text: &[u8],
    position: Position,
    encoding: &PositionEncodingKind,
) -> usize {
    let mut line: u32 = 0;
    let mut line_start_index: usize = 0;

    for (i, &c) in text.iter().enumerate() {
        if line == position.line {
            break;
        }
        if c == b'\n' {
            line += 1;
            line_start_index = i + 1;
        }
    }

    debug_assert_eq!(line, position.line);

    // TODO: handle position encoding
    // let line_end_index = text[line_start_index..]
    //     .iter()
    //     .position(|&c| c == b'\n')
    //     .unwrap_or(text.len());
    // let line_text = &text[line_start_index..line_end_index];
    // let line_byte_length = get_n_code_unit_byte_count(line_text, position.character, encoding);
    let line_byte_length = position.character as usize;

    line_start_index + line_byte_length
}

pub fn source_index_to_token_index(tree: &Ast, source_index: usize) -> TokenIndex {
    assert!(source_index < tree.source.len());

    let token_starts = &tree.token_starts;
    let mut upper_index = token_starts.len() - 1;
    let mut mid: usize = upper_index / 2;

    if token_starts.len() < 600 {
        let mid_tok_start = token_starts[mid] as usize;
        if mid_tok_start < source_index {
            let quart_index = mid + (mid / 2);
            let quart_tok_start = token_starts[quart_index] as usize;
            if quart_tok_start < source_index {
            } else {
                upper_index = quart_index;
            }
        } else {
            let quart_index = mid / 2;
            let quart_tok_start = token_starts[quart_index] as usize;
            if quart_tok_start < source_index {
                upper_index = mid;
            } else {
                upper_index = quart_index;
            }
        }
    } else {
        let threshold = 168;
        let mut lower_index: usize = 0;
        loop {
            let mid_tok_start = token_starts[mid] as usize;
            if mid_tok_start < source_index {
                if (upper_index - mid) < threshold {
                    break;
                }
                lower_index = mid;
            } else {
                upper_index = mid;
                if (mid - lower_index) < threshold {
                    break;
                }
            }
            mid = lower_index + (upper_index - lower_index) / 2;
        }
    }

    while upper_index > 0 {
        upper_index -= 1;
        let token_start = token_starts[upper_index] as usize;
        if token_start > source_index {
            continue;
        }
        if upper_index == token_starts.len() - 1 {
            break;
        }
        let is_within_current_token = source_index > (token_start - 1)
            && source_index < token_starts[upper_index + 1] as usize;
        if !is_within_current_token {
            upper_index += 1;
        }
        break;
    }

    upper_index as TokenIndex
}
