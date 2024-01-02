use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::{Error, Write};

use super::*;
use crate::{string_literal, utils};

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
pub struct Fixups {
    unused_var_decls: HashSet<TokenIndex>,
    gut_functions: HashSet<node::Index>,
    omit_nodes: HashSet<node::Index>,
    replace_nodes_with_string: HashMap<node::Index, String>,
    append_string_after_node: HashMap<node::Index, String>,
    replace_nodes_with_node: HashMap<node::Index, node::Index>,
    rename_identifiers: HashMap<String, String>,
    rebase_imported_paths: Option<String>,
}

impl Fixups {
    pub fn new() -> Fixups {
        Fixups::default()
    }

    pub fn len(&self) -> usize {
        self.unused_var_decls.len()
            + self.gut_functions.len()
            + self.omit_nodes.len()
            + self.replace_nodes_with_string.len()
            + self.append_string_after_node.len()
            + self.replace_nodes_with_node.len()
            + self.rename_identifiers.len()
            + self.rebase_imported_paths.is_some() as usize
    }

    pub fn clear(&mut self) {
        self.unused_var_decls.clear();
        self.gut_functions.clear();
        self.omit_nodes.clear();
        self.replace_nodes_with_string.clear();
        self.append_string_after_node.clear();
        self.replace_nodes_with_node.clear();
        self.rename_identifiers.clear();

        self.rebase_imported_paths = None;
    }
}

pub struct Render<'write, 'ast, 'src> {
    ais: AutoIndentingStream<'write>,
    tree: &'ast Ast<'src>,
    fixups: Fixups,
}

pub fn render_tree(writer: &mut dyn Write, tree: &Ast, fixups: Fixups) -> Result<()> {
    debug_assert_eq!(tree.errors.len(), 0);
    let mut ais = AutoIndentingStream::new(INDENT_DELTA, writer);
    let mut r = Render { ais, tree, fixups };

    let comment_end_loc = tree.token_start(0);
    r.render_comments(0, comment_end_loc as usize)?;

    if tree.token_tag(0) == T::ContainerDocComment {
        r.render_container_doc_comments(0)?;
    }

    match tree.mode {
        Mode::Zon => r.render_expression(tree.node(0).data.lhs, Space::Newline)?,
        Mode::Zig => r.render_members(tree.root_decls())?,
    }

    if let Some(disabled_offset) = r.ais.disabled_offset {
        write_fixing_whitespace(writer, &tree.source[disabled_offset..])?;
    }

    Ok(())
}

#[derive(Clone, Copy, PartialEq)]
enum Container {
    Enum,
    Tuple,
    Other,
}

#[derive(Clone, Copy, PartialEq)]
enum Space {
    /// Output the token lexeme only.
    None,
    /// Output the token lexeme followed by a single space.
    Space,
    /// Output the token lexeme followed by a newline.
    Newline,
    /// If the next token is a comma, render it as well. If not, insert one.
    /// In either case, a newline will be inserted afterwards.
    Comma,
    /// Additionally consume the next token if it is a comma.
    /// In either case, a space will be inserted afterwards.
    CommaSpace,
    /// Additionally consume the next token if it is a semicolon.
    /// In either case, a newline will be inserted afterwards.
    Semicolon,
    /// Skip rendering whitespace and comments. If this is used, the caller
    /// *must* handle whitespace and comments manually.
    Skip,
}

#[derive(Clone, Copy, PartialEq)]
enum QuoteBehavior {
    PreserveWhenShadowing,
    EagerlyUnquote,
    EagerlyUnquoteExceptUnderscore,
}

impl Render<'_, '_, '_> {
    /// Render all members in the given slice, keeping empty lines where appropriate
    fn render_members(&mut self, members: &[node::Index]) -> Result<()> {
        let [first_member, remaining_members @ ..] = &members[..] else {
            return Ok(());
        };
        let container = members
            .iter()
            .filter_map(|&m| self.tree.full_container_field(m))
            .all(|f| f.ast.tuple_like)
            .then_some(Container::Tuple)
            .unwrap_or(Container::Other);
        self.render_member(container, *first_member, Space::Newline)?;
        for member in remaining_members {
            self.render_extra_newline(*member)?;
            self.render_member(container, *member, Space::Newline)?;
        }
        Ok(())
    }

    fn render_member(
        &mut self,
        container: Container,
        decl: node::Index,
        space: Space,
    ) -> Result<()> {
        todo!("render_member")
    }

    /// Render all expressions in the slice, keeping empty lines where appropriate
    fn render_expressions(&mut self, expressions: &[node::Index], space: Space) -> Result<()> {
        todo!("render_expressions")
    }

    fn render_expression(&mut self, node: node::Index, space: Space) -> Result<()> {
        todo!("render_expression")
    }

    /// Same as `renderExpression`, but afterwards looks for any
    /// append_string_after_node fixups to apply
    fn render_expression_fixup(&mut self, node: node::Index, space: Space) -> Result<()> {
        todo!("render_expression_fixup")
    }

    fn render_array_type(&mut self, array_type: full::ArrayType, space: Space) -> Result<()> {
        todo!("render_array_type")
    }

    fn render_ptr_type(&mut self, ptr_type: full::PtrType, space: Space) -> Result<()> {
        todo!("render_ptr_type")
    }

    fn render_slice(
        &mut self,
        slice_node: node::Index,
        slice: full::Slice,
        space: Space,
    ) -> Result<()> {
        todo!("render_slice")
    }

    fn render_asm_output(&mut self, asm_output: node::Index, space: Space) -> Result<()> {
        todo!("render_asm_output")
    }

    fn render_asm_input(&mut self, asm_input: node::Index, space: Space) -> Result<()> {
        todo!("render_asm_input")
    }

    fn render_var_decl(
        &mut self,
        var_decl: full::VarDecl,
        // Destructures intentionally ignore leading `comptime` tokens.
        ignore_comptime_token: bool,
        // `comma_space` and `space` are used for destructure LHS decls.
        space: Space,
    ) -> Result<()> {
        todo!("render_var_decl")
    }

    fn render_var_decl_without_fixups(
        &mut self,
        var_decl: full::VarDecl,
        // Destructures intentionally ignore leading `comptime` tokens.
        ignore_comptime_token: bool,
        // `comma_space` and `space` are used for destructure LHS decls.
        space: Space,
    ) -> Result<()> {
        todo!("render_var_decl_without_fixups")
    }

    fn render_if(&mut self, if_node: full::If, space: Space) -> Result<()> {
        todo!("render_if")
    }

    /// Note that this function is additionally used to render if expressions, with
    /// respective values set to null.
    fn render_while(&mut self, while_node: full::While, space: Space) -> Result<()> {
        todo!("render_while")
    }

    fn render_then_else(
        &mut self,
        last_prefix_token: TokenIndex,
        then_expr: node::Index,
        else_token: TokenIndex,
        maybe_error_token: Option<TokenIndex>,
        else_expr: node::Index,
        space: Space,
    ) -> Result<()> {
        todo!("render_then_else")
    }

    fn render_for(&mut self, for_node: full::For, space: Space) -> Result<()> {
        todo!("render_for")
    }

    fn render_container_field(
        &mut self,
        container: Container,
        field_param: full::ContainerField,
        space: Space,
    ) -> Result<()> {
        todo!("render_container_field")
    }

    fn render_builtin_call(
        &mut self,
        builtin_token: TokenIndex,
        params: &[node::Index],
        space: Space,
    ) -> Result<()> {
        todo!("render_builtin_call")
    }

    fn render_fn_proto(&mut self, fn_proto: full::FnProto, space: Space) -> Result<()> {
        todo!("render_fn_proto")
    }

    fn render_switch_case(&mut self, switch_case: full::SwitchCase, space: Space) -> Result<()> {
        todo!("render_switch_case")
    }

    fn render_block(
        &mut self,
        block_node: node::Index,
        statements: &[node::Index],
        space: Space,
    ) -> Result<()> {
        todo!("render_block")
    }

    fn finish_render_block(
        &mut self,
        block_node: node::Index,
        statements: &[node::Index],
        space: Space,
    ) -> Result<()> {
        todo!("finish_render_block")
    }

    fn render_struct_init(
        &mut self,
        struct_node: node::Index,
        struct_init: full::StructInit,
        space: Space,
    ) -> Result<()> {
        todo!("render_struct_init")
    }

    fn render_array_init(&mut self, array_init: full::ArrayInit, space: Space) -> Result<()> {
        todo!("render_array_init")
    }

    fn render_container_decl(
        &mut self,
        container_decl_node: node::Index,
        container_decl: full::ContainerDecl,
        space: Space,
    ) -> Result<()> {
        todo!("render_container_decl")
    }

    fn render_asm(&mut self, asm_node: full::Asm, space: Space) -> Result<()> {
        todo!("render_asm")
    }

    fn render_call(&mut self, call: full::Call, space: Space) -> Result<()> {
        todo!("render_call")
    }

    fn render_param_list(
        &mut self,
        lparen: TokenIndex,
        params: &[node::Index],
        space: Space,
    ) -> Result<()> {
        todo!("render_param_list")
    }

    /// Renders the given expression indented, popping the indent before rendering
    /// any following line comments
    fn render_expression_indented(&mut self, node: node::Index, space: Space) -> Result<()> {
        todo!("render_expression_indented")
    }

    /// Render an expression, and the comma that follows it, if it is present in the source.
    /// If a comma is present, and `space` is `Space.comma`, render only a single comma.
    fn render_expression_comma(&mut self, node: node::Index, space: Space) -> Result<()> {
        todo!("render_expression_comma")
    }

    /// Render a token, and the comma that follows it, if it is present in the source.
    /// If a comma is present, and `space` is `Space.comma`, render only a single comma.
    fn render_token_comma(&mut self, token: TokenIndex, space: Space) -> Result<()> {
        todo!("render_token_comma")
    }

    /// Render an identifier, and the comma that follows it, if it is present in the source.
    /// If a comma is present, and `space` is `Space.comma`, render only a single comma.
    fn render_identifier_comma(
        &mut self,
        token: TokenIndex,
        space: Space,
        quote: QuoteBehavior,
    ) -> Result<()> {
        todo!("render_identifier_comma")
    }

    fn render_token(&mut self, token_index: TokenIndex, space: Space) -> Result<()> {
        todo!("render_token")
    }

    fn render_space(
        &mut self,
        token_index: TokenIndex,
        lexeme_len: usize,
        space: Space,
    ) -> Result<()> {
        todo!("render_space")
    }

    fn render_only_space(&mut self, space: Space) -> Result<()> {
        todo!("render_only_space")
    }

    fn render_identifier(
        &mut self,
        token_index: TokenIndex,
        space: Space,
        quote: QuoteBehavior,
    ) -> Result<()> {
        todo!("render_identifier")
        // self.render_quoted_identifier::<true>(token_index, space)
    }

    // Renders a @"" quoted identifier, normalizing escapes.
    // Unnecessary escapes are un-escaped, and \u escapes are normalized to \x when they fit.
    // If unquote is true, the @"" is removed and the result is a bare symbol whose validity is asserted.
    fn render_quoted_identifier<const UNQUOTE: bool>(
        &mut self,
        token_index: TokenIndex,
        space: Space,
    ) -> Result<()> {
        debug_assert_eq!(self.tree.token_tag(token_index), T::Identifier);
        let lexeme = token_slice_for_render(self.tree, token_index);
        let [b'@', b'"', contents @ .., b'"'] = &lexeme[..] else {
            unreachable!();
        };

        if !UNQUOTE {
            write!(self.ais, "@\"")?;
        }
        render_identifier_contents(&mut self.ais, contents)?;
        if !UNQUOTE {
            write!(self.ais, "\"")?;
        }

        self.render_space(token_index, lexeme.len(), space)
    }

    /// Assumes that start is the first byte past the previous token and
    /// that end is the last byte before the next token.
    fn render_comments(&mut self, start: usize, end: usize) -> Result<bool> {
        let mut index = start;
        while let Some(offset) = utils::find(&self.tree.source[start..end], b"//") {
            let comment_start = index + offset;

            // If there is no newline, the comment ends with EOF
            let newline_index = utils::find_scalar(&self.tree.source[comment_start..end], b'\n');
            let newline = newline_index.map(|i| comment_start + i);

            let untrimmed_comment = match newline {
                Some(n) => &self.tree.source[comment_start..n],
                None => &self.tree.source[comment_start..],
            };
            let trimmed_comment = utils::trim_ascii_end(untrimmed_comment);

            // Don't leave any whitespace at the start of the file
            if index != 0 {
                if index == start
                    && utils::contains_at_least(&self.tree.source[index..comment_start], 2, b"\n")
                {
                    // Leave up to one empty line before the first comment
                    self.ais.insert_new_line()?;
                    self.ais.insert_new_line()?;
                } else if utils::contains(&self.tree.source[index..comment_start], b"\n") {
                    // Respect the newline directly before the comment.
                    // Note: This allows an empty line between comments
                    self.ais.insert_new_line()?;
                } else if index == start {
                    // Otherwise if the first comment is on the same line as
                    // the token before it, prefix it with a single space.
                    write!(self.ais, " ")?;
                }
            }

            index = match newline {
                Some(n) => n + 1,
                None => end,
            };

            let comment_content = utils::trim_ascii_start(&trimmed_comment["//".len()..]);
            match self.ais.disabled_offset {
                Some(disabled_offset) if comment_content == b"zig fmt: on" => {
                    // Write the source for which formatting was disabled directly
                    // to the underlying writer, fixing up invalid whitespace.
                    let disabled_source = &self.tree.source[disabled_offset..comment_start];
                    write_fixing_whitespace(self.ais.underlying_writer, disabled_source)?;
                    // Write with the canonical single space.
                    write!(self.ais.underlying_writer, "// zig fmt: on\n")?;
                    self.ais.disabled_offset = None;
                }
                None if comment_content == b"zig fmt: off" => {
                    // Write with the canonical single space.
                    write!(self.ais, "// zig fmt: off\n")?;
                    self.ais.disabled_offset = Some(index);
                }
                _ => {
                    // Write the comment minus trailing whitespace.
                    write!(self.ais, "{}\n", str_from_utf8(trimmed_comment)?)?;
                }
            }
        }

        if index != start && utils::contains_at_least(&self.tree.source[index - 1..end], 2, b"\n") {
            // Don't leave any whitespace at the end of the file
            if end != self.tree.source.len() {
                self.ais.insert_new_line()?;
            }
        }

        Ok(index != start)
    }

    fn render_extra_newline(&mut self, node: node::Index) -> Result<()> {
        self.render_extra_newline_token(self.tree.first_token(node))
    }

    /// Check if there is an empty line immediately before the given token. If so, render it.
    fn render_extra_newline_token(&mut self, token_index: TokenIndex) -> Result<()> {
        let token_start = self.tree.token_start(token_index) as usize;
        if token_start == 0 {
            return Ok(());
        }
        let prev_token_end = match token_index {
            0 => 0,
            _ => {
                self.tree.token_start(token_index - 1) as usize
                    + token_slice_for_render(self.tree, token_index - 1).len()
            }
        };

        // If there is a immediately preceding comment or doc_comment,
        // skip it because required extra newline has already been rendered.
        if utils::contains(&self.tree.source[prev_token_end..token_start], b"//") {
            return Ok(());
        }
        if token_index > 0 && self.tree.token_tag(token_index - 1) == T::DocComment {
            return Ok(());
        }

        // Iterate backwards to the end of the previous token, stopping if a
        // non-whitespace character is encountered or two newlines have been found.
        let mut i = token_start - 1;
        let mut newlines = 0;
        while self.tree.source[i].is_ascii_whitespace() {
            if self.tree.source[i] == b'\n' {
                newlines += 1;
            }
            if newlines == 2 {
                return self.ais.insert_new_line();
            }
            if i == prev_token_end {
                break;
            }
            i -= 1;
        }

        Ok(())
    }

    /// end_token is the token one past the last doc comment token. This function
    /// searches backwards from there.
    fn render_doc_comments(&mut self, end_token: TokenIndex) -> Result<()> {
        // Search backwards for the first doc comment.
        if end_token == 0 {
            return Ok(());
        }
        let mut tok = end_token;
        while self.tree.token_tag(tok - 1) == T::DocComment {
            tok -= 1;
            if tok == 0 {
                break;
            }
        }
        let first_tok = tok;
        if first_tok == end_token {
            return Ok(());
        }

        if first_tok != 0 {
            let prev_token_tag = self.tree.token_tag(first_tok - 1);

            // Prevent accidental use of `renderDocComments` for a function argument doc comment
            debug_assert_ne!(prev_token_tag, T::LParen);

            if prev_token_tag != T::LBrace {
                self.render_extra_newline_token(first_tok)?;
            }
        }

        while self.tree.token_tag(tok) == T::DocComment {
            self.render_token(tok, Space::Newline)?;
            tok += 1;
        }

        Ok(())
    }

    /// start_token is first container doc comment token.
    fn render_container_doc_comments(&mut self, start_token: TokenIndex) -> Result<()> {
        let mut tok = start_token;
        while self.tree.token_tag(tok) == T::ContainerDocComment {
            self.render_token(tok, Space::Newline)?;
            tok += 1;
        }
        // Render extra newline if there is one between final container doc comment and
        // the next token. If the next token is a doc comment, that code path
        // will have its own logic to insert a newline.
        if self.tree.token_tag(tok) != T::DocComment {
            self.render_extra_newline_token(tok)?;
        }
        Ok(())
    }

    fn discard_all_params(&mut self, fn_proto_node: node::Index) -> Result<()> {
        let mut buf = [UNDEFINED_NODE];
        let fn_proto = self.tree.full_fn_proto(&mut buf, fn_proto_node).unwrap();
        for param in fn_proto.iterate(self.tree) {
            let name_ident = param.name_token.unwrap();
            debug_assert_eq!(self.tree.token_tag(name_ident), T::Identifier);
            write!(
                self.ais,
                "_ = {};\n",
                str_from_utf8(token_slice_for_render(self.tree, name_ident))?
            )?;
        }
        Ok(())
    }
}

fn render_identifier_contents(writer: &mut dyn Write, bytes: &[u8]) -> Result<()> {
    let mut pos: usize = 0;
    while pos < bytes.len() {
        let byte = bytes[pos];
        match byte {
            b'\\' => {
                let old_pos = pos;
                let res = string_literal::parse_escape_sequence(bytes, &mut pos);
                let escape_sequence = &bytes[old_pos..pos];
                match res {
                    Ok(codepoint) if codepoint <= 0x7f => {
                        let buf = [codepoint as u8];
                        write!(writer, "{}", crate::FormatEscapes::double_quoted(&buf))?;
                    }
                    _ => {
                        write!(writer, "{}", str_from_utf8(escape_sequence)?)?;
                    }
                }
            }
            0x00..=0x7f => {
                let buf = [byte];
                write!(writer, "{}", crate::FormatEscapes::double_quoted(&buf))?;
                pos += 1;
            }
            0x80..=0xff => {
                write!(writer, "{}", byte as char)?;
                pos += 1;
            }
        }
    }
    Ok(())
}

/// Returns true if there exists a line comment between any of the tokens from
/// `start_token` to `end_token`. This is used to determine if e.g. a
/// fn_proto should be wrapped and have a trailing comma inserted even if
/// there is none in the source.
fn has_comment(tree: Ast, start_token: TokenIndex, end_token: TokenIndex) -> bool {
    for i in start_token..end_token {
        let start = tree.token_start(i) as usize + tree.token_slice(i).len();
        let end = tree.token_start(i + 1) as usize;
        if tree.source[start..end].windows(2).any(|s| s == b"//") {
            return true;
        }
    }
    false
}

/// Returns true if there exists a multiline string literal between the start
/// of token `start_token` and the start of token `end_token`.
fn has_multiline_string(tree: Ast, start_token: TokenIndex, end_token: TokenIndex) -> bool {
    for token in start_token..end_token {
        match tree.token_tag(token) {
            T::MultilineStringLiteralLine => return true,
            _ => continue,
        }
    }
    false
}

fn token_slice_for_render<'src>(tree: &Ast<'src>, token_index: TokenIndex) -> &'src [u8] {
    let mut ret = tree.token_slice(token_index);
    match tree.token_tag(token_index) {
        T::MultilineStringLiteralLine => match &ret[..] {
            [without_newline @ .., b'\n'] => ret = without_newline,
            _ => {}
        },
        T::ContainerDocComment | T::DocComment => {
            ret = utils::trim_ascii_end(ret);
        }
        _ => {}
    }
    ret
}

fn has_same_line_comment(tree: &Ast, token_index: TokenIndex) -> bool {
    let between_source =
        tree.source(tree.token_start(token_index)..tree.token_start(token_index + 1));
    for byte in between_source {
        match byte {
            b'\n' => return false,
            b'/' => return true,
            _ => continue,
        }
    }
    false
}

/// Returns `true` if and only if there are any tokens or line comments between
/// start_token and end_token.
fn anything_between(tree: &Ast, start_token: TokenIndex, end_token: TokenIndex) -> bool {
    if start_token + 1 != end_token {
        return true;
    }
    let between_source =
        tree.source(tree.token_start(start_token)..tree.token_start(start_token + 1));
    for byte in between_source {
        match byte {
            b'/' => return true,
            _ => continue,
        }
    }
    false
}

fn write_fixing_whitespace(writer: &mut dyn Write, slice: &[u8]) -> Result<()> {
    str_from_utf8(slice)?.chars().try_for_each(|c| match c {
        '\t' => write!(writer, "    "),
        '\r' => Ok(()),
        _ => write!(writer, "{c}"),
    })
}

fn node_is_block(tag: node::Tag) -> bool {
    match tag {
        N::Block | N::BlockSemicolon | N::BlockTwo | N::BlockTwoSemicolon => true,
        _ => false,
    }
}

fn node_is_for_while_switch(tag: node::Tag) -> bool {
    match tag {
        N::If
        | N::IfSimple
        | N::For
        | N::ForSimple
        | N::While
        | N::WhileSimple
        | N::WhileCont
        | N::Switch
        | N::SwitchComma => true,
        _ => false,
    }
}

fn node_causes_slice_op_space(tag: node::Tag) -> bool {
    match tag {
        N::Catch
        | N::Add
        | N::AddWrap
        | N::ArrayCat
        | N::ArrayMult
        | N::Assign
        | N::AssignBitAnd
        | N::AssignBitOr
        | N::AssignShl
        | N::AssignShr
        | N::AssignBitXor
        | N::AssignDiv
        | N::AssignSub
        | N::AssignSubWrap
        | N::AssignMod
        | N::AssignAdd
        | N::AssignAddWrap
        | N::AssignMul
        | N::AssignMulWrap
        | N::BangEqual
        | N::BitAnd
        | N::BitOr
        | N::Shl
        | N::Shr
        | N::BitXor
        | N::BoolAnd
        | N::BoolOr
        | N::Div
        | N::EqualEqual
        | N::ErrorUnion
        | N::GreaterOrEqual
        | N::GreaterThan
        | N::LessOrEqual
        | N::LessThan
        | N::MergeErrorSets
        | N::Mod
        | N::Mul
        | N::MulWrap
        | N::Sub
        | N::SubWrap
        | N::Orelse => true,

        _ => false,
    }
}

/// Returns the number of nodes in `exprs` that are on the same line as `rtoken`.
fn row_size(tree: &Ast, exprs: &[node::Index], rtoken: TokenIndex) -> usize {
    let first_token = tree.first_token(exprs[0]);
    if tree.tokens_on_same_line(first_token, rtoken) {
        let maybe_comma = rtoken - 1;
        if tree.token_tag(maybe_comma) == T::Comma {
            return 1;
        }
        return exprs.len(); // no newlines
    }

    let mut count = 1;
    for (i, &expr) in exprs.iter().enumerate() {
        if i + 1 < exprs.len() {
            let expr_last_token = tree.last_token(expr) + 1;
            if !tree.tokens_on_same_line(expr_last_token, tree.first_token(exprs[i + 1])) {
                return count;
            }
            count += 1;
        } else {
            return count;
        }
    }
    unreachable!();
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

fn str_from_utf8(v: &[u8]) -> Result<&str> {
    std::str::from_utf8(v).map_err(|_| Error)
}
