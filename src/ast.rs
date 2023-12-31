use crate::parse::Parser;
use crate::token;
use crate::Tokenizer;

use crate::ast::error::Tag as E;
use crate::ast::node::Tag as N;
use crate::token::Tag as T;

pub mod visitor;
pub use visitor::Visitor;

pub mod display;
pub use display::Display;

mod render;

#[cfg(test)]
mod tests;

pub struct Ast<'src> {
    pub source: &'src [u8],
    pub token_tags: Vec<token::Tag>,
    pub token_starts: Vec<ByteOffset>,
    pub nodes: Vec<Node>,
    pub extra_data: Vec<node::Index>,
    pub mode: Mode,
    pub errors: Vec<Error>,
}

impl Ast<'_> {
    pub fn source(&self, start: ByteOffset) -> &[u8] {
        &self.source[start as usize..]
    }

    pub fn render(&self) -> Result<String, std::fmt::Error> {
        let mut buffer = String::new();
        self.render_to_writer(&mut buffer, Default::default())?;
        Ok(buffer)
    }

    pub fn render_to_writer(
        &self,
        writer: &mut dyn std::fmt::Write,
        fixups: render::Fixups,
    ) -> std::fmt::Result {
        render::render_tree(writer, self, fixups)
    }

    pub fn token_tag(&self, index: TokenIndex) -> token::Tag {
        self.token_tags[index as usize]
    }

    pub fn token_start(&self, index: TokenIndex) -> ByteOffset {
        self.token_starts[index as usize]
    }

    pub fn node(&self, index: node::Index) -> &Node {
        &self.nodes[index as usize]
    }
}

pub type TokenIndex = u32;
pub type ByteOffset = u32;

pub struct Location {
    pub line: usize,
    pub column: usize,
    pub line_start: usize,
    pub line_end: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Zig,
    Zon,
}

impl Ast<'_> {
    pub fn parse(source: &[u8], mode: Mode) -> Ast {
        // Empirically, the zig std lib has an 8:1 ratio of source bytes to token count.
        let estimated_token_count = source.len() / 8;

        let mut token_tags = Vec::with_capacity(estimated_token_count);
        let mut token_starts = Vec::with_capacity(estimated_token_count);

        let mut tokenizer = Tokenizer::new(source);
        loop {
            let token = tokenizer.next();
            token_tags.push(token.tag);
            token_starts.push(token.loc.start as u32);
            if token.tag == token::Tag::Eof {
                break;
            }
        }

        // Empirically, Zig source code has a 2:1 ratio of tokens to AST nodes.
        let estimated_node_count = (token_tags.len() + 2) / 2;

        let mut parser = Parser {
            source,
            token_tags: &token_tags,
            token_starts: &token_starts,
            errors: Vec::new(),
            nodes: Vec::with_capacity(estimated_node_count),
            extra_data: Vec::new(),
            tok_i: 0,
        };

        match mode {
            Mode::Zig => parser.parse_root(),
            Mode::Zon => parser.parse_zon(),
        }

        let Parser {
            nodes,
            extra_data,
            errors,
            ..
        } = parser;

        Ast {
            source,
            mode,
            token_tags,
            token_starts,
            nodes,
            extra_data,
            errors,
        }
    }

    pub fn error_offset(&self, parse_error: &Error) -> u32 {
        if parse_error.token_is_prev {
            self.token_slice(parse_error.token).len() as u32
        } else {
            0
        }
    }

    pub fn token_location(&self, start_offset: ByteOffset, token_index: TokenIndex) -> Location {
        let mut loc = Location {
            line: 0,
            column: 0,
            line_start: start_offset as usize,
            line_end: self.source.len(),
        };
        let token_start = self.token_start(token_index) as usize;

        // Scan by line until we go past the token start
        while let Some(i) = self.source[loc.line_start..]
            .iter()
            .position(|&c| c == b'\n')
        {
            if i + loc.line_start >= token_start {
                break; // Went past
            }
            loc.line += 1;
            loc.line_start += i + 1;
        }

        let offset = loc.line_start;
        for (i, &c) in self.source[offset..].iter().enumerate() {
            if i + offset == token_start {
                loc.line_end = i + offset;
                while loc.line_end < self.source.len() && self.source[loc.line_end] != b'\n' {
                    loc.line_end += 1;
                }
                return loc;
            }
            if c == b'\n' {
                loc.line += 1;
                loc.column = 0;
                loc.line_start = i + 1 + offset;
            } else {
                loc.column += 1;
            }
        }
        loc
    }

    pub fn token_slice(&self, token_index: TokenIndex) -> &[u8] {
        let token_tag = self.token_tag(token_index);

        if let Some(lexeme) = token_tag.lexeme() {
            return lexeme.as_bytes();
        }

        let mut tokenizer = Tokenizer {
            buffer: self.source,
            index: self.token_start(token_index) as usize,
            pending_invalid_token: None,
        };
        let token = tokenizer.find_tag_at_current_index(token_tag);
        assert!(token.tag == self.token_tag(token_index));
        &self.source[token.loc.start..token.loc.end]
    }
}

pub trait GetExtraData<T> {
    fn extra_data(&self, index: node::Index) -> T;
}

impl GetExtraData<node::Index> for Ast<'_> {
    fn extra_data(&self, index: node::Index) -> node::Index {
        self.extra_data[index as usize]
    }
}

impl Ast<'_> {
    pub fn root_decls(&self) -> &[node::Index] {
        &self.extra_data[self.node(0).data.lhs as usize..self.node(0).data.rhs as usize]
    }

    pub fn first_token(&self, mut index: node::Index) -> TokenIndex {
        let mut end_offset: TokenIndex = 0;
        loop {
            let n = self.node(index);
            index = match n.tag {
                N::Root => return 0,

                N::TestDecl
                | N::Errdefer
                | N::Defer
                | N::BoolNot
                | N::Negation
                | N::BitNot
                | N::NegationWrap
                | N::AddressOf
                | N::Try
                | N::Await
                | N::OptionalType
                | N::Switch
                | N::SwitchComma
                | N::IfSimple
                | N::If
                | N::Suspend
                | N::Resume
                | N::Continue
                | N::Break
                | N::Return
                | N::AnyframeType
                | N::Identifier
                | N::AnyframeLiteral
                | N::CharLiteral
                | N::NumberLiteral
                | N::UnreachableLiteral
                | N::StringLiteral
                | N::MultilineStringLiteral
                | N::GroupedExpression
                | N::BuiltinCallTwo
                | N::BuiltinCallTwoComma
                | N::BuiltinCall
                | N::BuiltinCallComma
                | N::ErrorSetDecl
                | N::Comptime
                | N::Nosuspend
                | N::AsmSimple
                | N::Asm
                | N::ArrayType
                | N::ArrayTypeSentinel
                | N::ErrorValue => return n.main_token - end_offset,

                N::ArrayInitDot
                | N::ArrayInitDotComma
                | N::ArrayInitDotTwo
                | N::ArrayInitDotTwoComma
                | N::StructInitDot
                | N::StructInitDotComma
                | N::StructInitDotTwo
                | N::StructInitDotTwoComma
                | N::EnumLiteral => return n.main_token - 1 - end_offset,

                N::Catch
                | N::FieldAccess
                | N::UnwrapOptional
                | N::EqualEqual
                | N::BangEqual
                | N::LessThan
                | N::GreaterThan
                | N::LessOrEqual
                | N::GreaterOrEqual
                | N::AssignMul
                | N::AssignDiv
                | N::AssignMod
                | N::AssignAdd
                | N::AssignSub
                | N::AssignShl
                | N::AssignShlSat
                | N::AssignShr
                | N::AssignBitAnd
                | N::AssignBitXor
                | N::AssignBitOr
                | N::AssignMulWrap
                | N::AssignAddWrap
                | N::AssignSubWrap
                | N::AssignMulSat
                | N::AssignAddSat
                | N::AssignSubSat
                | N::Assign
                | N::MergeErrorSets
                | N::Mul
                | N::Div
                | N::Mod
                | N::ArrayMult
                | N::MulWrap
                | N::MulSat
                | N::Add
                | N::Sub
                | N::ArrayCat
                | N::AddWrap
                | N::SubWrap
                | N::AddSat
                | N::SubSat
                | N::Shl
                | N::ShlSat
                | N::Shr
                | N::BitAnd
                | N::BitXor
                | N::BitOr
                | N::Orelse
                | N::BoolAnd
                | N::BoolOr
                | N::SliceOpen
                | N::Slice
                | N::SliceSentinel
                | N::Deref
                | N::ArrayAccess
                | N::ArrayInitOne
                | N::ArrayInitOneComma
                | N::ArrayInit
                | N::ArrayInitComma
                | N::StructInitOne
                | N::StructInitOneComma
                | N::StructInit
                | N::StructInitComma
                | N::CallOne
                | N::CallOneComma
                | N::Call
                | N::CallComma
                | N::SwitchRange
                | N::ForRange
                | N::ErrorUnion => n.data.lhs,

                N::AssignDestructure => {
                    let extra_idx = n.data.lhs;
                    let lhs_len: u32 = self.extra_data(extra_idx);
                    assert!(lhs_len > 0);
                    self.extra_data(extra_idx + 1)
                }

                N::FnDecl | N::FnProtoSimple | N::FnProtoMulti | N::FnProtoOne | N::FnProto => {
                    let mut i = n.main_token;
                    while i > 0 {
                        i -= 1;
                        match self.token_tag(i) {
                            T::KeywordExtern
                            | T::KeywordExport
                            | T::KeywordPub
                            | T::KeywordInline
                            | T::KeywordNoinline
                            | T::StringLiteral => continue,

                            _ => return i + 1 - end_offset,
                        }
                    }
                    return i - end_offset;
                }

                N::Usingnamespace => {
                    if n.main_token > 0 && self.token_tag(n.main_token - 1) == T::KeywordPub {
                        end_offset += 1;
                    }
                    return n.main_token - end_offset;
                }

                N::AsyncCallOne | N::AsyncCallOneComma | N::AsyncCall | N::AsyncCallComma => {
                    end_offset += 1;
                    n.data.lhs
                }

                N::ContainerFieldInit | N::ContainerFieldAlign | N::ContainerField => {
                    let name_token = n.main_token;
                    if self.token_tag(name_token) != T::KeywordComptime
                        && name_token > 0
                        && self.token_tag(name_token - 1) == T::KeywordComptime
                    {
                        end_offset += 1;
                    }
                    return name_token - end_offset;
                }

                N::GlobalVarDecl | N::LocalVarDecl | N::SimpleVarDecl | N::AlignedVarDecl => {
                    let mut i = n.main_token;
                    while i > 0 {
                        i -= 1;
                        match self.token_tag(i) {
                            T::KeywordExtern
                            | T::KeywordExport
                            | T::KeywordComptime
                            | T::KeywordPub
                            | T::KeywordThreadlocal
                            | T::StringLiteral => continue,

                            _ => return i + 1 - end_offset,
                        }
                    }
                    return i - end_offset;
                }

                N::Block | N::BlockSemicolon | N::BlockTwo | N::BlockTwoSemicolon => {
                    let lbrace = n.main_token;
                    if self.token_tag(lbrace - 1) == T::Colon
                        && self.token_tag(lbrace - 2) == T::Identifier
                    {
                        end_offset += 2;
                    }
                    return lbrace - end_offset;
                }

                N::ContainerDecl
                | N::ContainerDeclTrailing
                | N::ContainerDeclTwo
                | N::ContainerDeclTwoTrailing
                | N::ContainerDeclArg
                | N::ContainerDeclArgTrailing
                | N::TaggedUnion
                | N::TaggedUnionTrailing
                | N::TaggedUnionTwo
                | N::TaggedUnionTwoTrailing
                | N::TaggedUnionEnumTag
                | N::TaggedUnionEnumTagTrailing => {
                    match self.token_tag(n.main_token.saturating_sub(1)) {
                        T::KeywordPacked | T::KeywordExtern => end_offset += 1,
                        _ => {}
                    }
                    return n.main_token - end_offset;
                }

                N::PtrTypeAligned | N::PtrTypeSentinel | N::PtrType | N::PtrTypeBitRange => {
                    return (match self.token_tag(n.main_token) {
                        T::Asterisk | T::AsteriskAsterisk => {
                            match self.token_tag(n.main_token.saturating_sub(1)) {
                                T::LBracket => n.main_token.saturating_sub(1),
                                _ => n.main_token,
                            }
                        }
                        T::LBracket => n.main_token,
                        _ => unreachable!(),
                    }) - end_offset
                }

                N::SwitchCaseOne => {
                    if n.data.lhs == 0 {
                        return n.main_token - 1 - end_offset;
                    } else {
                        n.data.lhs
                    }
                }
                N::SwitchCaseInlineOne => {
                    if n.data.lhs == 0 {
                        return n.main_token - 2 - end_offset;
                    } else {
                        return self.first_token(n.data.lhs) - 1;
                    }
                }
                N::SwitchCase => {
                    let extra: node::SubRange = self.extra_data(n.data.lhs);
                    assert!(extra.end - extra.start > 0);
                    self.extra_data(extra.start)
                }
                N::SwitchCaseInline => {
                    let extra: node::SubRange = self.extra_data(n.data.lhs);
                    assert!(extra.end - extra.start > 0);
                    return self.first_token(self.extra_data(extra.start)) - 1;
                }

                N::AsmOutput | N::AsmInput => {
                    assert_eq!(self.token_tag(n.main_token - 1), T::LBracket);
                    return n.main_token - 1 - end_offset;
                }

                N::WhileSimple | N::WhileCont | N::While | N::ForSimple | N::For => {
                    let mut result = n.main_token;
                    if self.token_tag(result.saturating_sub(1)) == T::KeywordInline {
                        result -= 1;
                    }
                    if self.token_tag(result.saturating_sub(1)) == T::Colon {
                        result = result.saturating_sub(2);
                    }
                    return result - end_offset;
                }
            }
        }
    }

    pub fn last_token(&self, mut index: node::Index) -> TokenIndex {
        let mut end_offset: TokenIndex = 0;
        loop {
            let n = self.node(index);
            index = match n.tag {
                N::Root => return self.token_starts.len() as TokenIndex,

                N::Usingnamespace
                | N::BoolNot
                | N::Negation
                | N::BitNot
                | N::NegationWrap
                | N::AddressOf
                | N::Try
                | N::Await
                | N::OptionalType
                | N::Resume
                | N::Nosuspend
                | N::Comptime => n.data.lhs,

                N::TestDecl
                | N::Errdefer
                | N::Defer
                | N::Catch
                | N::EqualEqual
                | N::BangEqual
                | N::LessThan
                | N::GreaterThan
                | N::LessOrEqual
                | N::GreaterOrEqual
                | N::AssignMul
                | N::AssignDiv
                | N::AssignMod
                | N::AssignAdd
                | N::AssignSub
                | N::AssignShl
                | N::AssignShlSat
                | N::AssignShr
                | N::AssignBitAnd
                | N::AssignBitXor
                | N::AssignBitOr
                | N::AssignMulWrap
                | N::AssignAddWrap
                | N::AssignSubWrap
                | N::AssignMulSat
                | N::AssignAddSat
                | N::AssignSubSat
                | N::Assign
                | N::AssignDestructure
                | N::MergeErrorSets
                | N::Mul
                | N::Div
                | N::Mod
                | N::ArrayMult
                | N::MulWrap
                | N::MulSat
                | N::Add
                | N::Sub
                | N::ArrayCat
                | N::AddWrap
                | N::SubWrap
                | N::AddSat
                | N::SubSat
                | N::Shl
                | N::ShlSat
                | N::Shr
                | N::BitAnd
                | N::BitXor
                | N::BitOr
                | N::Orelse
                | N::BoolAnd
                | N::BoolOr
                | N::AnyframeType
                | N::ErrorUnion
                | N::IfSimple
                | N::WhileSimple
                | N::ForSimple
                | N::FnProtoSimple
                | N::FnProtoMulti
                | N::PtrTypeAligned
                | N::PtrTypeSentinel
                | N::PtrType
                | N::PtrTypeBitRange
                | N::ArrayType
                | N::SwitchCaseOne
                | N::SwitchCaseInlineOne
                | N::SwitchCase
                | N::SwitchCaseInline
                | N::SwitchRange => n.data.rhs,

                N::ForRange => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }

                N::FieldAccess
                | N::UnwrapOptional
                | N::GroupedExpression
                | N::MultilineStringLiteral
                | N::ErrorSetDecl
                | N::AsmSimple
                | N::AsmOutput
                | N::AsmInput
                | N::ErrorValue => return n.data.rhs + end_offset,

                N::AnyframeLiteral
                | N::CharLiteral
                | N::NumberLiteral
                | N::UnreachableLiteral
                | N::Identifier
                | N::Deref
                | N::EnumLiteral
                | N::StringLiteral => return n.main_token + end_offset,

                N::Return => {
                    if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }

                N::Call | N::AsyncCall => {
                    end_offset += 1;
                    let params: node::SubRange = self.extra_data(n.data.rhs);
                    if params.end - params.start == 0 {
                        return n.main_token + end_offset;
                    }
                    self.extra_data(params.end - 1)
                }
                N::TaggedUnionEnumTag => {
                    let members: node::SubRange = self.extra_data(n.data.rhs);
                    if members.end - members.start == 0 {
                        end_offset += 4;
                        n.data.lhs
                    } else {
                        end_offset += 1;
                        self.extra_data(members.end - 1)
                    }
                }
                N::CallComma | N::AsyncCallComma | N::TaggedUnionEnumTagTrailing => {
                    end_offset += 2;
                    let params: node::SubRange = self.extra_data(n.data.rhs);
                    assert!(params.end > params.start);
                    self.extra_data(params.end - 1)
                }
                N::Switch => {
                    let cases: node::SubRange = self.extra_data(n.data.rhs);
                    if cases.end - cases.start == 0 {
                        end_offset += 3;
                        n.data.lhs
                    } else {
                        end_offset += 1;
                        self.extra_data(cases.end - 1)
                    }
                }
                N::ContainerDeclArg => {
                    let members: node::SubRange = self.extra_data(n.data.rhs);
                    if members.end - members.start == 0 {
                        end_offset += 3;
                        n.data.lhs
                    } else {
                        end_offset += 1;
                        self.extra_data(members.end - 1)
                    }
                }

                _ => todo!("last_token"),
            }
        }
    }
}

pub mod full;

pub mod error;
pub use error::Error;

pub mod node;
pub use node::Node;

use self::node::ExtraData;
