use std::fmt::{Error, Write};

use super::*;

type Result<T> = std::result::Result<T, Error>;

impl Ast<'_> {
    pub fn render(&self) -> Result<String> {
        let mut buffer = String::new();
        self.render_to_writer(&mut buffer, Default::default())?;
        Ok(buffer)
    }

    pub fn render_to_writer(&self, writer: &mut dyn Write, fixups: render::Fixups) -> Result<()> {
        render_tree(writer, self, fixups)
    }
}

const INDENT_DELTA: usize = 4;
const ASM_INDENT_DELTA: usize = 2;

#[derive(Default)]
pub struct Fixups {}

pub fn render_tree(writer: &mut dyn Write, tree: &Ast, fixups: Fixups) -> Result<()> {
    debug_assert_eq!(tree.errors.len(), 0);
    let mut ais = AutoIndentingStream::new(INDENT_DELTA, writer);
    let mut r = Render { ais, tree, fixups };

    let comment_end_loc = tree.token_start(0);
    r.render_comments(0, comment_end_loc as usize)?;

    if tree.token_tag(0) == T::ContainerDocComment {
        todo!("render_tree")
    }

    match tree.mode {
        Mode::Zon => todo!("render_tree"),
        Mode::Zig => r.render_members(tree.root_decls())?,
    }

    if let Some(disabled_offset) = r.ais.disabled_offset {
        write_fixing_whitespace(writer, &tree.source[disabled_offset..])?;
    }

    Ok(())
}

pub struct Render<'write, 'ast, 'src> {
    ais: AutoIndentingStream<'write>,
    tree: &'ast Ast<'src>,
    fixups: Fixups,
}

enum Container {
    Enum,
    Tuple,
    Other,
}

impl Render<'_, '_, '_> {
    fn render_members(&self, members: &[u32]) -> Result<()> {
        let tree = self.tree;
        if members.is_empty() {
            return Ok(());
        }
        let container = 'blk: {
            for member in members {
                if false {
                    break 'blk Container::Other;
                }
            }
            Container::Tuple
        };
        todo!("render_members")
    }

    fn render_comments(&mut self, start: usize, end: usize) -> Result<bool> {
        let tree = self.tree;
        let ais = &mut self.ais;

        let mut index = start;
        while let Some(offset) = tree.source[start..end].windows(2).position(|s| s == b"//") {
            todo!("render_comments")
        }

        if index != start
            && tree.source[index - 1..end]
                .iter()
                .filter(|&&b| b == b'\n')
                .take(2)
                .count()
                == 2
        {
            if end != tree.source.len() {
                ais.insert_new_line()?;
            }
        }

        Ok(index != start)
    }
}

fn write_fixing_whitespace(writer: &mut dyn Write, slice: &[u8]) -> Result<()> {
    match std::str::from_utf8(slice) {
        Ok(s) => s.chars().try_for_each(|c| match c {
            '\t' => writer.write_str("    "),
            '\r' => Ok(()),
            _ => writer.write_char(c),
        }),
        Err(_) => Err(Error),
    }
}

/// Automatically inserts indentation of written data by keeping
/// track of the current indentation level
struct AutoIndentingStream<'write> {
    underlying_writer: &'write mut dyn Write,

    /// Offset into the source at which formatting has been disabled with
    /// a `zig fmt: off` comment.
    ///
    /// If non-null, the AutoIndentingStream will not write any bytes
    /// to the underlying writer. It will however continue to track the
    /// indentation level.
    disabled_offset: Option<usize>,

    indent_count: usize,
    indent_delta: usize,
    current_line_empty: bool,
    /// automatically popped when applied
    indent_one_shot_count: usize,
    /// the most recently applied indent
    applied_indent: usize,
    /// not used until the next line
    indent_next_line: usize,
}

impl<'write> AutoIndentingStream<'write> {
    fn new(indent_delta: usize, underlying_writer: &'write mut dyn Write) -> Self {
        Self {
            underlying_writer,
            disabled_offset: None,
            indent_count: 0,
            indent_delta,
            current_line_empty: true,
            indent_one_shot_count: 0,
            applied_indent: 0,
            indent_next_line: 0,
        }
    }
}

impl Write for AutoIndentingStream<'_> {
    fn write_str(&mut self, s: &str) -> Result<()> {
        if s.is_empty() {
            return Ok(());
        }

        self.apply_indent()?;
        self.write_no_indent(s)
    }
}

impl AutoIndentingStream<'_> {
    fn set_indent_delta(&mut self, new_indent_delta: usize) {
        use std::cmp::Ordering;
        match self.indent_delta.cmp(&new_indent_delta) {
            Ordering::Equal => return,
            Ordering::Greater => {
                debug_assert_eq!(self.indent_delta % new_indent_delta, 0);
                self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
            }
            Ordering::Less => {
                debug_assert_eq!(
                    (self.indent_count * self.indent_delta) % new_indent_delta,
                    0
                );
                self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
            }
        }
        self.indent_delta = new_indent_delta;
    }

    fn write_no_indent(&mut self, s: &str) -> Result<()> {
        if s.is_empty() {
            return Ok(());
        }

        if self.disabled_offset.is_none() {
            self.underlying_writer.write_str(s)?;
        }
        if s.as_bytes().last() == Some(&b'\n') {
            self.reset_line();
        }
        Ok(())
    }

    fn insert_new_line(&mut self) -> Result<()> {
        self.write_no_indent("\n")
    }

    fn reset_line(&mut self) {
        self.current_line_empty = true;
        self.indent_next_line = 0;
    }

    /// Insert a newline unless the current line is blank
    fn maybe_insert_newline(&mut self) -> Result<()> {
        if !self.current_line_empty {
            self.insert_new_line()?;
        }
        Ok(())
    }

    /// Push default indentation
    /// Doesn't actually write any indentation.
    /// Just primes the stream to be able to write the correct indentation if it needs to.
    fn push_indent(&mut self) {
        self.indent_count += 1;
    }

    /// Push an indent that is automatically popped after being applied
    fn push_indent_one_shot(&mut self) {
        self.indent_one_shot_count += 1;
        self.push_indent();
    }

    /// Turns all one-shot indents into regular indents
    /// Returns number of indents that must now be manually popped
    fn lock_one_shot_indent(&mut self) -> usize {
        std::mem::replace(&mut self.indent_one_shot_count, 0)
    }

    /// Push an indent that should not take effect until the next line
    fn push_indent_next_line(&mut self) {
        self.indent_next_line += 1;
        self.push_indent();
    }

    /// Writes ' ' bytes if the current line is empty
    fn pop_indent(&mut self) {
        debug_assert_ne!(self.indent_count, 0);
        self.indent_count -= 1;

        if self.indent_next_line > 0 {
            self.indent_next_line -= 1;
        }
    }

    /// Checks to see if the most recent indentation exceeds the currently pushed indents
    fn apply_indent(&mut self) -> Result<()> {
        let current_indent = self.current_indent();
        if self.current_line_empty && current_indent > 0 {
            if self.disabled_offset.is_none() {
                (0..current_indent).try_for_each(|_| self.underlying_writer.write_char(' '))?;
            }
            self.applied_indent = current_indent;
        }

        self.indent_count -= self.indent_one_shot_count;
        self.indent_one_shot_count = 0;
        self.current_line_empty = false;
        Ok(())
    }

    fn is_line_over_indented(&self) -> bool {
        if self.current_line_empty {
            return false;
        }
        self.applied_indent > self.current_indent()
    }

    fn current_indent(&self) -> usize {
        if self.indent_count > 0 {
            let indent_count = self.indent_count - self.indent_next_line;
            return indent_count * self.indent_delta;
        }
        0
    }
}
