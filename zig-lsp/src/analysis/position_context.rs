use zig::token::Tag as T;
use zig::{token, Token, Tokenizer};

use crate::offsets;

#[derive(Debug)]
pub enum PositionContext {
    Builtin(token::Loc),
    Comment,
    ImportStringLiteral(token::Loc),
    CincludeStringLiteral(token::Loc),
    EmbedfileStringLiteral(token::Loc),
    StringLiteral(token::Loc),
    FieldAccess(token::Loc),
    VarAccess(token::Loc),
    GlobalErrorSet,
    EnumLiteral(token::Loc),
    PreLabel,
    Label(bool),
    Other,
    Empty,
}

impl PositionContext {
    pub fn loc(&self) -> Option<&token::Loc> {
        match self {
            PositionContext::Builtin(loc)
            | PositionContext::ImportStringLiteral(loc)
            | PositionContext::CincludeStringLiteral(loc)
            | PositionContext::EmbedfileStringLiteral(loc)
            | PositionContext::StringLiteral(loc)
            | PositionContext::FieldAccess(loc)
            | PositionContext::VarAccess(loc)
            | PositionContext::EnumLiteral(loc) => Some(loc),
            _ => None,
        }
    }
}

struct StackState {
    ctx: PositionContext,
    stack_id: StackId,
}

enum StackId {
    Paren,
    Bracket,
    Global,
}

fn peek(arr: &mut Vec<StackState>) -> &mut StackState {
    if arr.is_empty() {
        arr.push(StackState {
            ctx: PositionContext::Empty,
            stack_id: StackId::Global,
        });
    }
    arr.last_mut().unwrap()
}

fn token_loc_append(prev: &token::Loc, token: &Token) -> token::Loc {
    token::Loc {
        start: prev.start,
        end: token.loc.end,
    }
}

pub fn is_symbol_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

pub fn get_position_context(text: &[u8], doc_index: usize, lookahead: bool) -> PositionContext {
    let mut new_index = doc_index;
    if lookahead && new_index < text.len() && is_symbol_char(text[new_index]) {
        new_index += 1;
    } else if lookahead && new_index + 1 < text.len() && text[new_index] == b'@' {
        new_index += 2;
    }
    let prev_char = if new_index > 0 {
        text[new_index - 1]
    } else {
        0
    };

    let mut line_loc = if !lookahead {
        offsets::line_loc_at_index(text, new_index)
    } else {
        offsets::line_loc_until_index(text, new_index)
    };

    let line = offsets::loc_to_slice(text, &line_loc);
    if zig::utils::trim_ascii_start(line).starts_with(b"//") {
        return PositionContext::Comment;
    }

    // Check if trimmed line starts with a '.' indicating a continuation
    while line_loc.start > 0 {
        let line_slice = &text[line_loc.start..line_loc.end];
        let trimmed_line = line_slice
            .iter()
            .skip_while(|&&c| c == b' ' || c == b'\t' || c == b'\r')
            .copied()
            .collect::<Vec<u8>>();

        if trimmed_line.starts_with(&[b'.']) {
            if line_loc.start > 1 {
                line_loc.start -= 2; // Jump over a potential preceding '\n'
                while line_loc.start > 0 {
                    if text[line_loc.start] == b'\n' {
                        line_loc.start += 1; // Eat the '\n'
                        break;
                    }
                    line_loc.start -= 1;
                }
            } else {
                break;
            }
        } else {
            // Check if trimmed line starts with "//"
            let trimmed_comment_line = line_slice
                .iter()
                .skip_while(|&&c| c == b' ' || c == b'\t')
                .copied()
                .collect::<Vec<u8>>();

            if line_loc.start != 0 && trimmed_comment_line.starts_with(&[b'/', b'/']) {
                // Assuming line_loc_at_index is a function similar to Zig's offsets.lineLocAtIndex
                let prev_line_loc = offsets::line_loc_at_index(text, line_loc.start - 1);
                line_loc.start = prev_line_loc.start;
                continue;
            }
            break;
        }
    }

    let mut stack = Vec::with_capacity(8);

    {
        let held_line = &text[..line_loc.end];

        let mut tokenizer = Tokenizer {
            buffer: held_line,
            index: line_loc.start,
            pending_invalid_token: None,
        };

        loop {
            let mut tok = tokenizer.next();
            // Early exits.
            if tok.loc.start > new_index {
                break;
            }
            if tok.loc.start == new_index {
                // Tie-breaking, the cursor is exactly between two tokens, and
                // `tok` is the latter of the two.
                if tok.tag != T::Identifier {
                    break;
                }
            }
            match tok.tag {
                T::Invalid => {
                    // Single '@' do not return a builtin token so we check this on our own.
                    if prev_char == b'@' {
                        return PositionContext::Builtin(token::Loc {
                            start: line_loc.end - 1,
                            end: line_loc.end,
                        });
                    }
                    let s = &held_line[tok.loc.start..tok.loc.end];
                    match s.iter().position(|&c| c == b'\"') {
                        Some(q) if s.get(q.saturating_sub(1)) == Some(&b'@') => {
                            tok.tag = T::Identifier;
                        }
                        _ => {
                            tok.tag = T::StringLiteral;
                        }
                    }
                }
                T::DocComment | T::ContainerDocComment => return PositionContext::Comment,
                T::Eof => break,
                _ => {}
            }

            // State changes
            match tok.tag {
                T::StringLiteral | T::MultilineStringLiteralLine => 'string_lit_block: {
                    if matches!(peek(&mut stack).stack_id, StackId::Paren) {
                        if let [.., perhaps_builtin, _] = &stack[..] {
                            match &perhaps_builtin.ctx {
                                PositionContext::Builtin(loc) => {
                                    match &tokenizer.buffer[loc.start..loc.end] {
                                        b"@import" => {
                                            peek(&mut stack).ctx =
                                                PositionContext::ImportStringLiteral(
                                                    tok.loc.clone(),
                                                );
                                            break 'string_lit_block;
                                        }
                                        b"@cInclude" => {
                                            peek(&mut stack).ctx =
                                                PositionContext::CincludeStringLiteral(
                                                    tok.loc.clone(),
                                                );
                                            break 'string_lit_block;
                                        }
                                        b"@embedFile" => {
                                            peek(&mut stack).ctx =
                                                PositionContext::EmbedfileStringLiteral(
                                                    tok.loc.clone(),
                                                );
                                            break 'string_lit_block;
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    peek(&mut stack).ctx = PositionContext::StringLiteral(tok.loc.clone());
                }
                T::Identifier => match &peek(&mut stack).ctx {
                    PositionContext::Empty
                    | PositionContext::PreLabel
                    | PositionContext::VarAccess(_) => {
                        peek(&mut stack).ctx = PositionContext::VarAccess(tok.loc.clone());
                    }
                    PositionContext::Label(filled) => {
                        if !filled {
                            peek(&mut stack).ctx = PositionContext::Label(true);
                        } else {
                            peek(&mut stack).ctx = PositionContext::VarAccess(tok.loc.clone());
                        }
                    }
                    PositionContext::EnumLiteral(loc) => {
                        peek(&mut stack).ctx =
                            PositionContext::EnumLiteral(token_loc_append(&loc, &tok));
                    }
                    _ => {}
                },
                T::Builtin => match peek(&mut stack).ctx {
                    PositionContext::Empty | PositionContext::PreLabel => {
                        peek(&mut stack).ctx = PositionContext::Builtin(tok.loc.clone());
                    }
                    _ => {}
                },
                T::Period | T::PeriodAsterisk => match peek(&mut stack).ctx {
                    PositionContext::Empty | PositionContext::PreLabel => {
                        peek(&mut stack).ctx = PositionContext::EnumLiteral(tok.loc.clone());
                    }
                    PositionContext::EnumLiteral(_) => {
                        peek(&mut stack).ctx = PositionContext::Empty;
                    }
                    PositionContext::FieldAccess(_) => {}
                    PositionContext::Other => {}
                    PositionContext::GlobalErrorSet => {}
                    PositionContext::Label(filled) => {
                        if filled {
                            peek(&mut stack).ctx = PositionContext::EnumLiteral(tok.loc.clone());
                        }
                    }
                    _ => {
                        let loc = peek(&mut stack).ctx.loc().unwrap();
                        peek(&mut stack).ctx =
                            PositionContext::FieldAccess(token_loc_append(loc, &tok));
                    }
                },
                T::KeywordBreak | T::KeywordContinue => {
                    peek(&mut stack).ctx = PositionContext::PreLabel;
                }
                T::Colon => match peek(&mut stack).ctx {
                    PositionContext::PreLabel => {
                        peek(&mut stack).ctx = PositionContext::Label(false);
                    }
                    _ => peek(&mut stack).ctx = PositionContext::Empty,
                },
                T::QuestionMark => match peek(&mut stack).ctx {
                    PositionContext::FieldAccess(_) => {}
                    _ => peek(&mut stack).ctx = PositionContext::Empty,
                },
                T::LParen => stack.push(StackState {
                    ctx: PositionContext::Empty,
                    stack_id: StackId::Paren,
                }),
                T::LBracket => stack.push(StackState {
                    ctx: PositionContext::Empty,
                    stack_id: StackId::Bracket,
                }),
                T::RParen => match peek(&mut stack).stack_id {
                    StackId::Paren => {
                        stack.pop();
                    }
                    _ => {
                        stack.pop();
                        peek(&mut stack).ctx = PositionContext::Empty;
                    }
                },
                T::RBracket => match peek(&mut stack).stack_id {
                    StackId::Bracket => {
                        stack.pop();
                    }
                    _ => {
                        stack.pop();
                        peek(&mut stack).ctx = PositionContext::Empty;
                    }
                },
                T::KeywordError => {
                    peek(&mut stack).ctx = PositionContext::GlobalErrorSet;
                }
                _ => {
                    peek(&mut stack).ctx = PositionContext::Empty;
                }
            }

            match &peek(&mut stack).ctx {
                PositionContext::FieldAccess(r) => {
                    peek(&mut stack).ctx = PositionContext::FieldAccess(token_loc_append(&r, &tok))
                }
                _ => {}
            }
        }
    }

    if let Some(state) = stack.pop() {
        match state.ctx {
            PositionContext::Empty => {}
            PositionContext::Label(filled) => {
                // We need to check this because the state could be a filled
                // label if only a space follows it
                if !filled || prev_char != b' ' {
                    return state.ctx;
                }
            }
            _ => return state.ctx,
        }
    }

    if line.is_empty() {
        return PositionContext::Empty;
    }

    let held_line = offsets::loc_to_slice(text, &line_loc);

    match line[0] {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'@' => {}
        _ => return PositionContext::Empty,
    }

    let mut tokenizer = Tokenizer::new(held_line);
    let tok = tokenizer.next();

    if tok.tag == T::Identifier {
        PositionContext::VarAccess(tok.loc)
    } else {
        PositionContext::Empty
    }
}
