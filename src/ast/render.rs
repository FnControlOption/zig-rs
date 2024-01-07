use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::io::{Error, ErrorKind, Result, Write};

use super::*;
use crate::fmt::Quotes;
use crate::{primitives, string_literal, utils};

impl Ast<'_> {
    pub fn render(&self) -> Result<Vec<u8>> {
        let mut buffer = Vec::new();
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
    /// The key is the mut token (`var`/`const`) of the variable declaration
    /// that should have a `_ = foo;` inserted afterwards.
    unused_var_decls: HashSet<TokenIndex>,
    /// The functions in this unordered set of AST fn decl nodes will render
    /// with a function body of `@trap()` instead, with all parameters
    /// discarded.
    gut_functions: HashSet<node::Index>,
    /// These global declarations will be omitted.
    omit_nodes: HashSet<node::Index>,
    /// These expressions will be replaced with the string value.
    replace_nodes_with_string: HashMap<node::Index, Vec<u8>>,
    /// The string value will be inserted directly after the node.
    append_string_after_node: HashMap<node::Index, Vec<u8>>,
    /// These nodes will be replaced with a different node.
    replace_nodes_with_node: HashMap<node::Index, node::Index>,
    /// Change all identifier names matching the key to be value instead.
    rename_identifiers: HashMap<Vec<u8>, Vec<u8>>,

    /// All `@import` builtin calls which refer to a file path will be prefixed
    /// with this path.
    rebase_imported_paths: Option<Vec<u8>>,
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
        if self.fixups.omit_nodes.contains(&decl) {
            return Ok(());
        }
        self.render_doc_comments(self.tree.first_token(decl))?;
        match self.tree.node(decl).tag {
            N::GlobalVarDecl | N::LocalVarDecl | N::SimpleVarDecl | N::AlignedVarDecl => {
                let var_decl = self.tree.full_var_decl(decl).unwrap();
                self.render_var_decl(var_decl, false, Space::Semicolon)
            }
            _ => {
                println!("render_member: {:?}", self.tree.node(decl).tag);
                std::process::exit(1)
            }
        }
    }

    /// Render all expressions in the slice, keeping empty lines where appropriate
    fn render_expressions(&mut self, expressions: &[node::Index], space: Space) -> Result<()> {
        let [first, rest @ ..] = expressions else {
            return Ok(());
        };
        self.render_expression(*first, space)?;
        for &expression in rest {
            self.render_extra_newline(expression)?;
            self.render_expression(expression, space)?;
        }
        Ok(())
    }

    fn render_expression(&mut self, node: node::Index, space: Space) -> Result<()> {
        if let Some(replacement) = self.fixups.replace_nodes_with_string.get(&node) {
            self.ais.write_all(replacement)?;
            return self.render_only_space(space);
        } else if let Some(&replacement) = self.fixups.replace_nodes_with_node.get(&node) {
            return self.render_expression(replacement, space);
        }
        match self.tree.node(node).tag {
            N::Identifier => {
                let token_index = self.tree.node(node).main_token;
                self.render_identifier(token_index, space, QuoteBehavior::PreserveWhenShadowing)
            }

            N::NumberLiteral
            | N::CharLiteral
            | N::UnreachableLiteral
            | N::AnyframeLiteral
            | N::StringLiteral => self.render_token(self.tree.node(node).main_token, space),

            N::MultilineStringLiteral => todo!("render_expression"),

            N::ErrorValue => todo!("render_expression"),

            N::BlockTwo | N::BlockTwoSemicolon => todo!("render_expression"),
            N::Block | N::BlockSemicolon => todo!("render_expression"),

            N::Errdefer => todo!("render_expression"),

            N::Defer => todo!("render_expression"),
            N::Comptime | N::Nosuspend => todo!("render_expression"),

            N::Suspend => todo!("render_expression"),

            N::Catch => todo!("render_expression"),

            N::FieldAccess => todo!("render_expression"),

            N::ErrorUnion | N::SwitchRange => todo!("render_expression"),
            N::ForRange => todo!("render_expression"),

            N::Add
            | N::AddWrap
            | N::AddSat
            | N::ArrayCat
            | N::ArrayMult
            | N::Assign
            | N::AssignBitAnd
            | N::AssignBitOr
            | N::AssignShl
            | N::AssignShlSat
            | N::AssignShr
            | N::AssignBitXor
            | N::AssignDiv
            | N::AssignSub
            | N::AssignSubWrap
            | N::AssignSubSat
            | N::AssignMod
            | N::AssignAdd
            | N::AssignAddWrap
            | N::AssignAddSat
            | N::AssignMul
            | N::AssignMulWrap
            | N::AssignMulSat
            | N::BangEqual
            | N::BitAnd
            | N::BitOr
            | N::Shl
            | N::ShlSat
            | N::Shr
            | N::BitXor
            | N::BoolAnd
            | N::BoolOr
            | N::Div
            | N::EqualEqual
            | N::GreaterOrEqual
            | N::GreaterThan
            | N::LessOrEqual
            | N::LessThan
            | N::MergeErrorSets
            | N::Mod
            | N::Mul
            | N::MulWrap
            | N::MulSat
            | N::Sub
            | N::SubWrap
            | N::SubSat
            | N::Orelse => todo!("render_expression"),

            N::AssignDestructure => todo!("render_expression"),

            N::BitNot
            | N::BoolNot
            | N::Negation
            | N::NegationWrap
            | N::OptionalType
            | N::AddressOf => todo!("render_expression"),

            N::Try | N::Resume | N::Await => todo!("render_expression"),

            N::ArrayType | N::ArrayTypeSentinel => todo!("render_expression"),

            N::PtrTypeAligned | N::PtrTypeSentinel | N::PtrType | N::PtrTypeBitRange => {
                todo!("render_expression")
            }

            N::ArrayInitOne
            | N::ArrayInitOneComma
            | N::ArrayInitDotTwo
            | N::ArrayInitDotTwoComma
            | N::ArrayInitDot
            | N::ArrayInitDotComma
            | N::ArrayInit
            | N::ArrayInitComma => todo!("render_expression"),

            N::StructInitOne
            | N::StructInitOneComma
            | N::StructInitDotTwo
            | N::StructInitDotTwoComma
            | N::StructInitDot
            | N::StructInitDotComma
            | N::StructInit
            | N::StructInitComma => todo!("render_expression"),

            N::CallOne
            | N::CallOneComma
            | N::AsyncCallOne
            | N::AsyncCallOneComma
            | N::Call
            | N::CallComma
            | N::AsyncCall
            | N::AsyncCallComma => todo!("render_expression"),

            N::ArrayAccess => todo!("render_expression"),

            N::SliceOpen | N::Slice | N::SliceSentinel => todo!("render_expression"),

            N::Deref => todo!("render_expression"),

            N::UnwrapOptional => todo!("render_expression"),

            N::Break => todo!("render_expression"),

            N::Continue => todo!("render_expression"),

            N::Return => todo!("render_expression"),

            N::GroupedExpression => todo!("render_expression"),

            N::ContainerDecl
            | N::ContainerDeclTrailing
            | N::ContainerDeclArg
            | N::ContainerDeclArgTrailing
            | N::ContainerDeclTwo
            | N::ContainerDeclTwoTrailing
            | N::TaggedUnion
            | N::TaggedUnionTrailing
            | N::TaggedUnionEnumTag
            | N::TaggedUnionEnumTagTrailing
            | N::TaggedUnionTwo
            | N::TaggedUnionTwoTrailing => todo!("render_expression"),

            N::ErrorSetDecl => todo!("render_expression"),

            N::BuiltinCallTwo | N::BuiltinCallTwoComma => todo!("render_expression"),
            N::BuiltinCall | N::BuiltinCallComma => todo!("render_expression"),

            N::FnProtoSimple | N::FnProtoMulti | N::FnProtoOne | N::FnProto => {
                todo!("render_expression")
            }

            N::AnyframeType => todo!("render_expression"),

            N::Switch | N::SwitchComma => todo!("render_expression"),

            N::SwitchCaseOne | N::SwitchCaseInlineOne | N::SwitchCase | N::SwitchCaseInline => {
                todo!("render_expression")
            }

            N::WhileSimple | N::WhileCont | N::While => todo!("render_expression"),

            N::ForSimple | N::For => todo!("render_expression"),

            N::IfSimple | N::If => todo!("render_expression"),

            N::AsmSimple | N::Asm => todo!("render_expression"),

            N::EnumLiteral => todo!("render_expression"),

            N::FnDecl => unreachable!(),
            N::ContainerField => unreachable!(),
            N::ContainerFieldInit => unreachable!(),
            N::ContainerFieldAlign => unreachable!(),
            N::Root => unreachable!(),
            N::GlobalVarDecl => unreachable!(),
            N::LocalVarDecl => unreachable!(),
            N::SimpleVarDecl => unreachable!(),
            N::AlignedVarDecl => unreachable!(),
            N::Usingnamespace => unreachable!(),
            N::TestDecl => unreachable!(),
            N::AsmOutput => unreachable!(),
            N::AsmInput => unreachable!(),
        }
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
        let name_token = var_decl.ast.mut_token + 1;
        self.render_var_decl_without_fixups(var_decl, ignore_comptime_token, space)?;
        if self.fixups.unused_var_decls.contains(&name_token) {
            // Discard the variable like this: `_ = foo;`
            let w = &mut self.ais;
            write!(w, "_ = ")?;
            w.write_all(token_slice_for_render(self.tree, name_token))?;
            write!(w, ";\n")?;
        }
        Ok(())
    }

    fn render_var_decl_without_fixups(
        &mut self,
        var_decl: full::VarDecl,
        // Destructures intentionally ignore leading `comptime` tokens.
        ignore_comptime_token: bool,
        // `comma_space` and `space` are used for destructure LHS decls.
        space: Space,
    ) -> Result<()> {
        if let Some(visib_token) = var_decl.visib_token {
            self.render_token(visib_token, Space::Space)?; // pub
        }

        if let Some(extern_export_token) = var_decl.extern_export_token {
            self.render_token(extern_export_token, Space::Space)?; // extern

            if let Some(lib_name) = var_decl.lib_name {
                self.render_token(lib_name, Space::Space)?; // "lib"
            }
        }

        if let Some(threadlocal_token) = var_decl.threadlocal_token {
            self.render_token(threadlocal_token, Space::Space)?; // threadlocal
        }

        if !ignore_comptime_token {
            if let Some(comptime_token) = var_decl.comptime_token {
                self.render_token(comptime_token, Space::Space)?; // comptime
            }
        }

        self.render_token(var_decl.ast.mut_token, Space::Space)?; // var

        if var_decl.ast.type_node != 0
            || var_decl.ast.align_node != 0
            || var_decl.ast.addrspace_node != 0
            || var_decl.ast.section_node != 0
            || var_decl.ast.init_node != 0
        {
            let name_space = if var_decl.ast.type_node == 0
                && (var_decl.ast.align_node != 0
                    || var_decl.ast.addrspace_node != 0
                    || var_decl.ast.section_node != 0
                    || var_decl.ast.init_node != 0)
            {
                Space::Space
            } else {
                Space::None
            };

            self.render_identifier(
                var_decl.ast.mut_token + 1,
                name_space,
                QuoteBehavior::PreserveWhenShadowing,
            )?; // name
        } else {
            return self.render_identifier(
                var_decl.ast.mut_token + 1,
                space,
                QuoteBehavior::PreserveWhenShadowing,
            ); // name
        }

        if var_decl.ast.type_node != 0 {
            todo!("render_var_decl_without_fixups")
        }

        if var_decl.ast.align_node != 0 {
            todo!("render_var_decl_without_fixups")
        }

        if var_decl.ast.addrspace_node != 0 {
            todo!("render_var_decl_without_fixups")
        }

        if var_decl.ast.section_node != 0 {
            todo!("render_var_decl_without_fixups")
        }

        debug_assert_ne!(var_decl.ast.init_node, 0);

        let eq_token = self.tree.first_token(var_decl.ast.init_node) - 1;
        let eq_space = if self.tree.tokens_on_same_line(eq_token, eq_token + 1) {
            Space::Space
        } else {
            Space::Newline
        };
        {
            self.ais.push_indent();
            self.render_token(eq_token, eq_space)?;
            self.ais.pop_indent();
        }
        self.ais.push_indent_one_shot();
        self.render_expression(var_decl.ast.init_node, space)
    }

    fn render_if(&mut self, if_node: full::If, space: Space) -> Result<()> {
        self.render_while(
            full::While {
                ast: full::WhileComponents {
                    while_token: if_node.ast.if_token,
                    cond_expr: if_node.ast.cond_expr,
                    cont_expr: 0,
                    then_expr: if_node.ast.then_expr,
                    else_expr: if_node.ast.else_expr,
                },
                inline_token: None,
                label_token: None,
                payload_token: if_node.payload_token,
                error_token: if_node.error_token,
                else_token: if_node.else_token,
            },
            space,
        )
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
        let lbrace = self.tree.node(block_node).main_token;

        if self.tree.token_tag(lbrace - 1) == T::Colon
            && self.tree.token_tag(lbrace - 2) == T::Identifier
        {
            self.render_identifier(lbrace - 2, Space::None, QuoteBehavior::EagerlyUnquote)?; // identifier
            self.render_token(lbrace - 1, Space::Space)?; // :
        }
        self.ais.push_indent_next_line();
        if statements.is_empty() {
            self.render_token(lbrace, Space::None)?;
            self.ais.pop_indent();
            return self.render_token(self.tree.last_token(block_node), space); // rbrace
        }
        self.render_token(lbrace, Space::Newline)?;
        self.finish_render_block(block_node, statements, space)
    }

    fn finish_render_block(
        &mut self,
        block_node: node::Index,
        statements: &[node::Index],
        space: Space,
    ) -> Result<()> {
        for (i, &stmt) in statements.iter().enumerate() {
            if i != 0 {
                self.render_extra_newline(stmt)?;
            }
            if self.fixups.omit_nodes.contains(&stmt) {
                continue;
            }
            match self.tree.node(stmt).tag {
                N::GlobalVarDecl | N::LocalVarDecl | N::SimpleVarDecl | N::AlignedVarDecl => {
                    let var_decl = self.tree.full_var_decl(stmt).unwrap();
                    self.render_var_decl(var_decl, false, Space::Semicolon)?;
                }

                _ => self.render_expression(stmt, Space::Semicolon)?,
            }
        }
        self.ais.pop_indent();

        self.render_token(self.tree.last_token(block_node), space) // rbrace
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
        if let Some(async_token) = call.async_token {
            self.render_token(async_token, Space::Space)?;
        }
        self.render_expression(call.ast.fn_expr, Space::None)?;
        self.render_param_list(call.ast.lparen, call.ast.params, space)
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
        let maybe_comma = self.tree.last_token(node) + 1;
        if self.tree.token_tag(maybe_comma) == T::Comma && space != Space::Comma {
            self.render_expression(node, Space::None)?;
            self.render_token(maybe_comma, space)
        } else {
            self.render_expression(node, space)
        }
    }

    /// Render a token, and the comma that follows it, if it is present in the source.
    /// If a comma is present, and `space` is `Space.comma`, render only a single comma.
    fn render_token_comma(&mut self, token: TokenIndex, space: Space) -> Result<()> {
        let maybe_comma = token + 1;
        if self.tree.token_tag(maybe_comma) == T::Comma && space != Space::Comma {
            self.render_token(token, Space::None)?;
            self.render_token(maybe_comma, space)
        } else {
            self.render_token(token, space)
        }
    }

    /// Render an identifier, and the comma that follows it, if it is present in the source.
    /// If a comma is present, and `space` is `Space.comma`, render only a single comma.
    fn render_identifier_comma(
        &mut self,
        token: TokenIndex,
        space: Space,
        quote: QuoteBehavior,
    ) -> Result<()> {
        let maybe_comma = token + 1;
        if self.tree.token_tag(maybe_comma) == T::Comma && space != Space::Comma {
            self.render_identifier(token, Space::None, quote)?;
            self.render_token(maybe_comma, space)
        } else {
            self.render_identifier(token, space, quote)
        }
    }

    fn render_token(&mut self, token_index: TokenIndex, space: Space) -> Result<()> {
        let lexeme = token_slice_for_render(self.tree, token_index);
        self.ais.write_all(lexeme)?;
        self.render_space(token_index, lexeme.len(), space)
    }

    fn render_space(
        &mut self,
        token_index: TokenIndex,
        lexeme_len: usize,
        space: Space,
    ) -> Result<()> {
        if matches!(space, Space::Skip) {
            return Ok(());
        }

        let token_start = self.tree.token_start(token_index);

        if matches!(space, Space::Comma) && self.tree.token_tag(token_index + 1) != T::Comma {
            write!(self.ais, ",")?;
        }

        let comment = self.render_comments(
            token_start as usize + lexeme_len,
            self.tree.token_start(token_index + 1) as usize,
        )?;
        match space {
            Space::None => {}
            Space::Space => {
                if !comment {
                    write!(self.ais, " ")?;
                }
            }
            Space::Newline => {
                if !comment {
                    self.ais.insert_newline()?;
                }
            }

            Space::Comma => {
                if self.tree.token_tag(token_index + 1) == T::Comma {
                    self.render_token(token_index + 1, Space::Newline)?;
                } else if !comment {
                    self.ais.insert_newline()?;
                }
            }

            Space::CommaSpace => {
                if self.tree.token_tag(token_index + 1) == T::Comma {
                    self.render_token(token_index + 1, Space::Space)?;
                } else if !comment {
                    write!(self.ais, " ")?;
                }
            }

            Space::Semicolon => {
                if self.tree.token_tag(token_index + 1) == T::Semicolon {
                    self.render_token(token_index + 1, Space::Newline)?;
                } else if !comment {
                    self.ais.insert_newline()?;
                }
            }

            Space::Skip => unreachable!(),
        }
        Ok(())
    }

    fn render_only_space(&mut self, space: Space) -> Result<()> {
        match space {
            Space::None => Ok(()),
            Space::Space => write!(self.ais, " "),
            Space::Newline => self.ais.insert_newline(),
            Space::Comma => write!(self.ais, ",\n"),
            Space::CommaSpace => write!(self.ais, ", "),
            Space::Semicolon => write!(self.ais, ";\n"),
            Space::Skip => unreachable!(),
        }
    }

    fn render_identifier(
        &mut self,
        token_index: TokenIndex,
        space: Space,
        quote: QuoteBehavior,
    ) -> Result<()> {
        debug_assert_eq!(self.tree.token_tag(token_index), T::Identifier);
        let lexeme = token_slice_for_render(self.tree, token_index);

        if let Some(mangled) = self.fixups.rename_identifiers.get(lexeme) {
            self.ais.write_all(mangled.as_slice())?;
            return self.render_space(token_index, lexeme.len(), space);
        }

        if lexeme[0] != b'@' {
            return self.render_token(token_index, space);
        }

        let [b'@', b'"', contents @ .., b'"'] = &lexeme[..] else {
            unreachable!();
        };

        // Empty name can't be unquoted.
        if contents.is_empty() {
            return self.render_quoted_identifier::<false>(token_index, space);
        }

        // Special case for _ which would incorrectly be rejected by isValidId below.
        if contents == b"_" {
            match quote {
                QuoteBehavior::EagerlyUnquote => {
                    return self.render_quoted_identifier::<true>(token_index, space);
                }
                QuoteBehavior::EagerlyUnquoteExceptUnderscore
                | QuoteBehavior::PreserveWhenShadowing => {
                    return self.render_quoted_identifier::<false>(token_index, space);
                }
            }
        }

        // Scan the entire name for characters that would (after un-escaping) be illegal in a symbol,
        // i.e. contents don't match: [A-Za-z_][A-Za-z0-9_]*
        let mut contents_i: usize = 0;
        while contents_i < contents.len() {
            match contents[contents_i] {
                b'0'..=b'9' => {
                    if contents_i == 0 {
                        return self.render_quoted_identifier::<false>(token_index, space);
                    }
                }
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => {}
                b'\\' => {
                    let mut esc_offset = contents_i;
                    let res = string_literal::parse_escape_sequence(contents, &mut esc_offset);
                    match res {
                        Ok(char) => match char {
                            '0'..='9' => {
                                if contents_i == 0 {
                                    return self
                                        .render_quoted_identifier::<false>(token_index, space);
                                }
                            }
                            'A'..='Z' | 'a'..='z' | '_' => {}
                            _ => return self.render_quoted_identifier::<false>(token_index, space),
                        },
                        Err(_) => {
                            return self.render_quoted_identifier::<false>(token_index, space)
                        }
                    }
                    contents_i += esc_offset;
                    continue;
                }
                _ => return self.render_quoted_identifier::<false>(token_index, space),
            }
            contents_i += 1;
        }

        // Read enough of the name (while un-escaping) to determine if it's a keyword or primitive.
        // If it's too long to fit in this buffer, we know it's neither and quoting is unnecessary.
        // If we read the whole thing, we have to do further checks.
        fn longest_keyword_or_primitive_len() -> usize {
            // TODO(zig-rs): use LazyLock after it is stabilized
            use std::sync::OnceLock;
            static LOCK: OnceLock<usize> = OnceLock::new();
            *LOCK.get_or_init(|| {
                primitives::names()
                    .iter()
                    .chain(token::keywords().keys())
                    .map(|s| s.len())
                    .max()
                    .unwrap_or(0)
            })
        }
        let mut buf = vec![u8::MAX; longest_keyword_or_primitive_len()];

        contents_i = 0;
        let mut buf_i: usize = 0;
        while contents_i < contents.len() && buf_i < longest_keyword_or_primitive_len() {
            if contents[contents_i] == b'\\' {
                let res = string_literal::parse_escape_sequence(contents, &mut contents_i).unwrap();
                buf[buf_i] = res as u8;
                buf_i += 1;
            } else {
                buf[buf_i] = contents[contents_i];
                contents_i += 1;
                buf_i += 1;
            }
        }

        // We read the whole thing, so it could be a keyword or primitive.
        if contents_i == contents.len() {
            if crate::is_valid_id(&buf[0..buf_i]) {
                return self.render_quoted_identifier::<false>(token_index, space);
            }
            if primitives::is_primitive(&buf[0..buf_i]) {
                match quote {
                    QuoteBehavior::EagerlyUnquote
                    | QuoteBehavior::EagerlyUnquoteExceptUnderscore => {
                        return self.render_quoted_identifier::<true>(token_index, space);
                    }
                    QuoteBehavior::PreserveWhenShadowing => {
                        return self.render_quoted_identifier::<false>(token_index, space);
                    }
                }
            }
        }

        self.render_quoted_identifier::<true>(token_index, space)
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
                    self.ais.insert_newline()?;
                    self.ais.insert_newline()?;
                } else if utils::contains(&self.tree.source[index..comment_start], b"\n") {
                    // Respect the newline directly before the comment.
                    // Note: This allows an empty line between comments
                    self.ais.insert_newline()?;
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
                    self.ais.write_all(trimmed_comment)?;
                    write!(self.ais, "\n")?;
                }
            }
        }

        if index != start && utils::contains_at_least(&self.tree.source[index - 1..end], 2, b"\n") {
            // Don't leave any whitespace at the end of the file
            if end != self.tree.source.len() {
                self.ais.insert_newline()?;
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
                return self.ais.insert_newline();
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
            let w = &mut self.ais;
            write!(w, "_ = ")?;
            w.write_all(token_slice_for_render(self.tree, name_ident))?;
            write!(w, ";\n")?;
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
                    Ok(codepoint) if codepoint.is_ascii() => {
                        crate::fmt_escapes(writer, &[codepoint as u8], Quotes::Double)?;
                    }
                    _ => {
                        writer.write_all(escape_sequence)?;
                    }
                }
            }
            0x00..=0x7f => {
                crate::fmt_escapes(writer, &[byte], Quotes::Double)?;
                pos += 1;
            }
            0x80..=0xff => {
                writer.write_all(&[byte])?;
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
        let start = tree.token_start(i) + tree.token_len(i);
        let end = tree.token_start(i + 1);
        if tree.source(start..end).windows(2).any(|s| s == b"//") {
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

fn token_slice_for_render<'a>(tree: &'a Ast, token_index: TokenIndex) -> &'a [u8] {
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
    let mut start = 0;
    for (i, &byte) in slice.iter().enumerate() {
        match byte {
            b'\t' => {
                writer.write_all(&slice[start..i])?;
                write!(writer, "    ")?;
                start = i + 1;
            }
            b'\r' => {
                writer.write_all(&slice[start..i])?;
                start = i + 1;
            }
            _ => {}
        }
    }
    writer.write_all(&slice[start..])
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
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }

        self.apply_indent()?;
        self.write_no_indent(buf)
    }

    fn flush(&mut self) -> Result<()> {
        self.underlying_writer.flush()
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

    fn write_no_indent(&mut self, bytes: &[u8]) -> Result<usize> {
        if bytes.is_empty() {
            return Ok(0);
        }

        if self.disabled_offset.is_none() {
            self.underlying_writer.write_all(bytes)?;
        }
        if bytes.last() == Some(&b'\n') {
            self.reset_line();
        }
        Ok(bytes.len())
    }

    fn insert_newline(&mut self) -> Result<()> {
        self.write_no_indent(b"\n")?;
        Ok(())
    }

    fn reset_line(&mut self) {
        self.current_line_empty = true;
        self.indent_next_line = 0;
    }

    /// Insert a newline unless the current line is blank
    fn maybe_insert_newline(&mut self) -> Result<()> {
        if !self.current_line_empty {
            self.insert_newline()?;
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
                (0..current_indent).try_for_each(|_| write!(self.underlying_writer, " "))?;
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
