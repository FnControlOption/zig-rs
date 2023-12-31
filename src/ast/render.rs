use super::*;
use std::fmt::{Error, Write};

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

struct AutoIndentingStream<'write> {
    underlying_writer: &'write mut dyn Write,
    disabled_offset: Option<usize>,
    indent_count: usize,
    indent_delta: usize,
    current_line_empty: bool,
    indent_one_shot_count: usize,
    applied_indent: usize,
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
        if self.indent_delta == new_indent_delta {
            return;
        } else if self.indent_delta > new_indent_delta {
            debug_assert_eq!(self.indent_delta % new_indent_delta, 0);
            self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
        } else {
            debug_assert_eq!(
                (self.indent_count * self.indent_delta) % new_indent_delta,
                0
            );
            self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
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

    fn maybe_insert_newline(&mut self) -> Result<()> {
        if !self.current_line_empty {
            self.insert_new_line()?;
        }
        Ok(())
    }

    fn push_indent(&mut self) {
        self.indent_count += 1;
    }

    fn push_indent_one_shot(&mut self) {
        self.indent_one_shot_count += 1;
        self.push_indent();
    }

    fn lock_one_shot_indent(&mut self) -> usize {
        std::mem::replace(&mut self.indent_one_shot_count, 0)
    }

    fn push_indent_next_line(&mut self) {
        self.indent_next_line += 1;
        self.push_indent();
    }

    fn pop_indent(&mut self) {
        debug_assert_ne!(self.indent_count, 0);
        self.indent_count -= 1;

        if self.indent_next_line > 0 {
            self.indent_next_line -= 1;
        }
    }

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
