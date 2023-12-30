use crate::macros::*;
use crate::parse::Parser;
use crate::token;
use crate::Tokenizer;

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
                node!(Root) => return 0,

                node!(TestDecl)
                | node!(Errdefer)
                | node!(Defer)
                | node!(BoolNot)
                | node!(Negation)
                | node!(BitNot)
                | node!(NegationWrap)
                | node!(AddressOf)
                | node!(Try)
                | node!(Await)
                | node!(OptionalType)
                | node!(Switch)
                | node!(SwitchComma)
                | node!(IfSimple)
                | node!(If)
                | node!(Suspend)
                | node!(Resume)
                | node!(Continue)
                | node!(Break)
                | node!(Return)
                | node!(AnyframeType)
                | node!(Identifier)
                | node!(AnyframeLiteral)
                | node!(CharLiteral)
                | node!(NumberLiteral)
                | node!(UnreachableLiteral)
                | node!(StringLiteral)
                | node!(MultilineStringLiteral)
                | node!(GroupedExpression)
                | node!(BuiltinCallTwo)
                | node!(BuiltinCallTwoComma)
                | node!(BuiltinCall)
                | node!(BuiltinCallComma)
                | node!(ErrorSetDecl)
                | node!(Comptime)
                | node!(Nosuspend)
                | node!(AsmSimple)
                | node!(Asm)
                | node!(ArrayType)
                | node!(ArrayTypeSentinel)
                | node!(ErrorValue) => return n.main_token - end_offset,

                node!(ArrayInitDot)
                | node!(ArrayInitDotComma)
                | node!(ArrayInitDotTwo)
                | node!(ArrayInitDotTwoComma)
                | node!(StructInitDot)
                | node!(StructInitDotComma)
                | node!(StructInitDotTwo)
                | node!(StructInitDotTwoComma)
                | node!(EnumLiteral) => return n.main_token - 1 - end_offset,

                node!(Catch)
                | node!(FieldAccess)
                | node!(UnwrapOptional)
                | node!(EqualEqual)
                | node!(BangEqual)
                | node!(LessThan)
                | node!(GreaterThan)
                | node!(LessOrEqual)
                | node!(GreaterOrEqual)
                | node!(AssignMul)
                | node!(AssignDiv)
                | node!(AssignMod)
                | node!(AssignAdd)
                | node!(AssignSub)
                | node!(AssignShl)
                | node!(AssignShlSat)
                | node!(AssignShr)
                | node!(AssignBitAnd)
                | node!(AssignBitXor)
                | node!(AssignBitOr)
                | node!(AssignMulWrap)
                | node!(AssignAddWrap)
                | node!(AssignSubWrap)
                | node!(AssignMulSat)
                | node!(AssignAddSat)
                | node!(AssignSubSat)
                | node!(Assign)
                | node!(MergeErrorSets)
                | node!(Mul)
                | node!(Div)
                | node!(Mod)
                | node!(ArrayMult)
                | node!(MulWrap)
                | node!(MulSat)
                | node!(Add)
                | node!(Sub)
                | node!(ArrayCat)
                | node!(AddWrap)
                | node!(SubWrap)
                | node!(AddSat)
                | node!(SubSat)
                | node!(Shl)
                | node!(ShlSat)
                | node!(Shr)
                | node!(BitAnd)
                | node!(BitXor)
                | node!(BitOr)
                | node!(Orelse)
                | node!(BoolAnd)
                | node!(BoolOr)
                | node!(SliceOpen)
                | node!(Slice)
                | node!(SliceSentinel)
                | node!(Deref)
                | node!(ArrayAccess)
                | node!(ArrayInitOne)
                | node!(ArrayInitOneComma)
                | node!(ArrayInit)
                | node!(ArrayInitComma)
                | node!(StructInitOne)
                | node!(StructInitOneComma)
                | node!(StructInit)
                | node!(StructInitComma)
                | node!(CallOne)
                | node!(CallOneComma)
                | node!(Call)
                | node!(CallComma)
                | node!(SwitchRange)
                | node!(ForRange)
                | node!(ErrorUnion) => n.data.lhs,

                node!(AssignDestructure) => {
                    let extra_idx = n.data.lhs;
                    let lhs_len: u32 = self.extra_data(extra_idx);
                    assert!(lhs_len > 0);
                    self.extra_data(extra_idx + 1)
                }

                node!(FnDecl)
                | node!(FnProtoSimple)
                | node!(FnProtoMulti)
                | node!(FnProtoOne)
                | node!(FnProto) => {
                    let mut i = n.main_token;
                    while i > 0 {
                        i -= 1;
                        match self.token_tag(i) {
                            token!(KeywordExtern)
                            | token!(KeywordExport)
                            | token!(KeywordPub)
                            | token!(KeywordInline)
                            | token!(KeywordNoinline)
                            | token!(StringLiteral) => continue,

                            _ => return i + 1 - end_offset,
                        }
                    }
                    return i - end_offset;
                }

                node!(Usingnamespace) => {
                    if n.main_token > 0 && self.token_tag(n.main_token - 1) == token!(KeywordPub) {
                        end_offset += 1;
                    }
                    return n.main_token - end_offset;
                }

                node!(AsyncCallOne)
                | node!(AsyncCallOneComma)
                | node!(AsyncCall)
                | node!(AsyncCallComma) => {
                    end_offset += 1;
                    n.data.lhs
                }

                node!(ContainerFieldInit) | node!(ContainerFieldAlign) | node!(ContainerField) => {
                    let name_token = n.main_token;
                    if self.token_tag(name_token) != token!(KeywordComptime)
                        && name_token > 0
                        && self.token_tag(name_token - 1) == token!(KeywordComptime)
                    {
                        end_offset += 1;
                    }
                    return name_token - end_offset;
                }

                node!(GlobalVarDecl)
                | node!(LocalVarDecl)
                | node!(SimpleVarDecl)
                | node!(AlignedVarDecl) => {
                    let mut i = n.main_token;
                    while i > 0 {
                        i -= 1;
                        match self.token_tag(i) {
                            token!(KeywordExtern)
                            | token!(KeywordExport)
                            | token!(KeywordComptime)
                            | token!(KeywordPub)
                            | token!(KeywordThreadlocal)
                            | token!(StringLiteral) => continue,

                            _ => return i + 1 - end_offset,
                        }
                    }
                    return i - end_offset;
                }

                node!(Block)
                | node!(BlockSemicolon)
                | node!(BlockTwo)
                | node!(BlockTwoSemicolon) => {
                    let lbrace = n.main_token;
                    if self.token_tag(lbrace - 1) == token!(Colon)
                        && self.token_tag(lbrace - 2) == token!(Identifier)
                    {
                        end_offset += 2;
                    }
                    return lbrace - end_offset;
                }

                node!(ContainerDecl)
                | node!(ContainerDeclTrailing)
                | node!(ContainerDeclTwo)
                | node!(ContainerDeclTwoTrailing)
                | node!(ContainerDeclArg)
                | node!(ContainerDeclArgTrailing)
                | node!(TaggedUnion)
                | node!(TaggedUnionTrailing)
                | node!(TaggedUnionTwo)
                | node!(TaggedUnionTwoTrailing)
                | node!(TaggedUnionEnumTag)
                | node!(TaggedUnionEnumTagTrailing) => {
                    match self.token_tag(n.main_token.saturating_sub(1)) {
                        token!(KeywordPacked) | token!(KeywordExtern) => end_offset += 1,
                        _ => {}
                    }
                    return n.main_token - end_offset;
                }

                node!(PtrTypeAligned)
                | node!(PtrTypeSentinel)
                | node!(PtrType)
                | node!(PtrTypeBitRange) => {
                    return (match self.token_tag(n.main_token) {
                        token!(Asterisk) | token!(AsteriskAsterisk) => {
                            match self.token_tag(n.main_token.saturating_sub(1)) {
                                token!(LBracket) => n.main_token.saturating_sub(1),
                                _ => n.main_token,
                            }
                        }
                        token!(LBracket) => n.main_token,
                        _ => unreachable!(),
                    }) - end_offset
                }

                node!(SwitchCaseOne) => {
                    if n.data.lhs == 0 {
                        return n.main_token - 1 - end_offset;
                    } else {
                        n.data.lhs
                    }
                }
                node!(SwitchCaseInlineOne) => {
                    if n.data.lhs == 0 {
                        return n.main_token - 2 - end_offset;
                    } else {
                        return self.first_token(n.data.lhs) - 1;
                    }
                }
                node!(SwitchCase) => {
                    let extra: node::SubRange = self.extra_data(n.data.lhs);
                    assert!(extra.end - extra.start > 0);
                    self.extra_data(extra.start)
                }
                node!(SwitchCaseInline) => {
                    let extra: node::SubRange = self.extra_data(n.data.lhs);
                    assert!(extra.end - extra.start > 0);
                    return self.first_token(self.extra_data(extra.start)) - 1;
                }

                node!(AsmOutput) | node!(AsmInput) => {
                    assert_eq!(self.token_tag(n.main_token - 1), token!(LBracket));
                    return n.main_token - 1 - end_offset;
                }

                node!(WhileSimple)
                | node!(WhileCont)
                | node!(While)
                | node!(ForSimple)
                | node!(For) => {
                    let mut result = n.main_token;
                    if self.token_tag(result.saturating_sub(1)) == token!(KeywordInline) {
                        result -= 1;
                    }
                    if self.token_tag(result.saturating_sub(1)) == token!(Colon) {
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
                node!(Root) => return self.token_starts.len() as TokenIndex,

                node!(Usingnamespace)
                | node!(BoolNot)
                | node!(Negation)
                | node!(BitNot)
                | node!(NegationWrap)
                | node!(AddressOf)
                | node!(Try)
                | node!(Await)
                | node!(OptionalType)
                | node!(Resume)
                | node!(Nosuspend)
                | node!(Comptime) => n.data.lhs,

                node!(TestDecl)
                | node!(Errdefer)
                | node!(Defer)
                | node!(Catch)
                | node!(EqualEqual)
                | node!(BangEqual)
                | node!(LessThan)
                | node!(GreaterThan)
                | node!(LessOrEqual)
                | node!(GreaterOrEqual)
                | node!(AssignMul)
                | node!(AssignDiv)
                | node!(AssignMod)
                | node!(AssignAdd)
                | node!(AssignSub)
                | node!(AssignShl)
                | node!(AssignShlSat)
                | node!(AssignShr)
                | node!(AssignBitAnd)
                | node!(AssignBitXor)
                | node!(AssignBitOr)
                | node!(AssignMulWrap)
                | node!(AssignAddWrap)
                | node!(AssignSubWrap)
                | node!(AssignMulSat)
                | node!(AssignAddSat)
                | node!(AssignSubSat)
                | node!(Assign)
                | node!(AssignDestructure)
                | node!(MergeErrorSets)
                | node!(Mul)
                | node!(Div)
                | node!(Mod)
                | node!(ArrayMult)
                | node!(MulWrap)
                | node!(MulSat)
                | node!(Add)
                | node!(Sub)
                | node!(ArrayCat)
                | node!(AddWrap)
                | node!(SubWrap)
                | node!(AddSat)
                | node!(SubSat)
                | node!(Shl)
                | node!(ShlSat)
                | node!(Shr)
                | node!(BitAnd)
                | node!(BitXor)
                | node!(BitOr)
                | node!(Orelse)
                | node!(BoolAnd)
                | node!(BoolOr)
                | node!(AnyframeType)
                | node!(ErrorUnion)
                | node!(IfSimple)
                | node!(WhileSimple)
                | node!(ForSimple)
                | node!(FnProtoSimple)
                | node!(FnProtoMulti)
                | node!(PtrTypeAligned)
                | node!(PtrTypeSentinel)
                | node!(PtrType)
                | node!(PtrTypeBitRange)
                | node!(ArrayType)
                | node!(SwitchCaseOne)
                | node!(SwitchCaseInlineOne)
                | node!(SwitchCase)
                | node!(SwitchCaseInline)
                | node!(SwitchRange) => n.data.rhs,

                node!(ForRange) => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }

                node!(FieldAccess)
                | node!(UnwrapOptional)
                | node!(GroupedExpression)
                | node!(MultilineStringLiteral)
                | node!(ErrorSetDecl)
                | node!(AsmSimple)
                | node!(AsmOutput)
                | node!(AsmInput)
                | node!(ErrorValue) => return n.data.rhs + end_offset,

                node!(AnyframeLiteral)
                | node!(CharLiteral)
                | node!(NumberLiteral)
                | node!(UnreachableLiteral)
                | node!(Identifier)
                | node!(Deref)
                | node!(EnumLiteral)
                | node!(StringLiteral) => return n.main_token + end_offset,

                node!(Return) => {
                    if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }

                node!(Call) | node!(AsyncCall) => {
                    end_offset += 1;
                    let params: node::SubRange = self.extra_data(n.data.rhs);
                    if params.end - params.start == 0 {
                        return n.main_token + end_offset;
                    }
                    self.extra_data(params.end - 1)
                }
                node!(TaggedUnionEnumTag) => {
                    let members: node::SubRange = self.extra_data(n.data.rhs);
                    if members.end - members.start == 0 {
                        end_offset += 4;
                        n.data.lhs
                    } else {
                        end_offset += 1;
                        self.extra_data(members.end - 1)
                    }
                }
                node!(CallComma) | node!(AsyncCallComma) | node!(TaggedUnionEnumTagTrailing) => {
                    end_offset += 2;
                    let params: node::SubRange = self.extra_data(n.data.rhs);
                    assert!(params.end > params.start);
                    self.extra_data(params.end - 1)
                }
                node!(Switch) => {
                    let cases: node::SubRange = self.extra_data(n.data.rhs);
                    if cases.end - cases.start == 0 {
                        end_offset += 3;
                        n.data.lhs
                    } else {
                        end_offset += 1;
                        self.extra_data(cases.end - 1)
                    }
                }
                node!(ContainerDeclArg) => {
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
