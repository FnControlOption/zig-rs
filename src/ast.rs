use crate::parse::Parser;
use crate::token;
use crate::Tokenizer;

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

impl<'src> Ast<'src> {
    pub fn token_tag(&self, index: TokenIndex) -> token::Tag {
        self.token_tags[index as usize]
    }

    pub fn token_start(&self, index: TokenIndex) -> ByteOffset {
        self.token_starts[index as usize]
    }

    pub fn node(&self, index: node::Index) -> &Node {
        &self.nodes[index as usize]
    }

    pub fn extra_data(&self, index: node::Index) -> node::Index {
        self.extra_data[index as usize]
    }
}

pub type TokenIndex = u32;
pub type ByteOffset = u32;

#[derive(Debug)]
pub enum Mode {
    Zig,
    Zon,
}

impl<'src> Ast<'src> {
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
}

pub struct Location {
    pub line: usize,
    pub column: usize,
    pub line_start: usize,
    pub line_end: usize,
}

pub mod full {
    use super::*;
}

pub struct Error {
    pub tag: error::Tag,
    pub is_note: bool,
    /// True if `token` points to the token before the token causing an issue.
    pub token_is_prev: bool,
    pub token: TokenIndex,
}

impl Default for Error {
    fn default() -> Error {
        Error {
            tag: error::Tag::Unknown,
            is_note: false,
            token_is_prev: false,
            token: 0,
        }
    }
}

pub mod error {
    use super::*;

    #[derive(Debug)]
    pub enum Tag {
        Unknown,

        AsteriskAfterPtrDeref,
        ChainedComparisonOperators,
        DeclBetweenFields,
        ExpectedBlock,
        ExpectedBlockOrAssignment,
        ExpectedBlockOrExpr,
        ExpectedBlockOrField,
        ExpectedContainerMembers,
        ExpectedExpr,
        ExpectedExprOrAssignment,
        ExpectedExprOrVarDecl,
        ExpectedFn,
        ExpectedInlinable,
        ExpectedLabelable,
        ExpectedParamList,
        ExpectedPrefixExpr,
        ExpectedPrimaryTypeExpr,
        ExpectedPubItem,
        ExpectedReturnType,
        ExpectedSemiOrElse,
        ExpectedSemiOrLbrace,
        ExpectedStatement,
        ExpectedSuffixOp,
        ExpectedTypeExpr,
        ExpectedVarDecl,
        ExpectedVarDeclOrFn,
        ExpectedLoopPayload,
        ExpectedContainer,
        ExternFnBody,
        ExtraAddrspaceQualifier,
        ExtraAlignQualifier,
        ExtraAllowzeroQualifier,
        ExtraConstQualifier,
        ExtraVolatileQualifier,
        PtrModOnArrayChildType,
        InvalidBitRange,
        SameLineDocComment,
        UnattachedDocComment,
        TestDocComment,
        ComptimeDocComment,
        VarargsNonfinal,
        ExpectedContinueExpr,
        ExpectedSemiAfterDecl,
        ExpectedSemiAfterStmt,
        ExpectedCommaAfterField,
        ExpectedCommaAfterArg,
        ExpectedCommaAfterParam,
        ExpectedCommaAfterInitializer,
        ExpectedCommaAfterSwitchProng,
        ExpectedCommaAfterForOperand,
        ExpectedCommaAfterCapture,
        ExpectedInitializer,
        MismatchedBinaryOpWhitespace,
        InvalidAmpersandAmpersand,
        CStyleContainer,
        ExpectedVarConst,
        WrongEqualVarDecl,
        VarConstDecl,
        ExtraForCapture,
        ForInputNotCaptured,

        ZigStyleContainer,
        PreviousField,
        NextField,

        ExpectedToken(token::Tag),
    }
}

pub struct Node {
    pub tag: node::Tag,
    pub main_token: TokenIndex,
    pub data: node::Data,
}

pub mod node {
    use super::*;

    pub type Index = u32;

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum Tag {
        /// sub_list[lhs...rhs]
        Root,
        /// `usingnamespace lhs;`. rhs unused. main_token is `usingnamespace`.
        Usingnamespace,
        /// lhs is test name token (must be string literal or identifier), if any.
        /// rhs is the body node.
        TestDecl,
        /// lhs is the index into extra_data.
        /// rhs is the initialization expression, if any.
        /// main_token is `var` or `const`.
        GlobalVarDecl,
        /// `var a: x align(y) = rhs`
        /// lhs is the index into extra_data.
        /// main_token is `var` or `const`.
        LocalVarDecl,
        /// `var a: lhs = rhs`. lhs and rhs may be unused.
        /// Can be local or global.
        /// main_token is `var` or `const`.
        SimpleVarDecl,
        /// `var a align(lhs) = rhs`. lhs and rhs may be unused.
        /// Can be local or global.
        /// main_token is `var` or `const`.
        AlignedVarDecl,
        /// lhs is the identifier token payload if any,
        /// rhs is the deferred expression.
        Errdefer,
        /// lhs is unused.
        /// rhs is the deferred expression.
        Defer,
        /// lhs catch rhs
        /// lhs catch |err| rhs
        /// main_token is the `catch` keyword.
        /// payload is determined by looking at the next token after the `catch` keyword.
        Catch,
        /// `lhs.a`. main_token is the dot. rhs is the identifier token index.
        FieldAccess,
        /// `lhs.?`. main_token is the dot. rhs is the `?` token index.
        UnwrapOptional,
        /// `lhs == rhs`. main_token is op.
        EqualEqual,
        /// `lhs != rhs`. main_token is op.
        BangEqual,
        /// `lhs < rhs`. main_token is op.
        LessThan,
        /// `lhs > rhs`. main_token is op.
        GreaterThan,
        /// `lhs <= rhs`. main_token is op.
        LessOrEqual,
        /// `lhs >= rhs`. main_token is op.
        GreaterOrEqual,
        /// `lhs *= rhs`. main_token is op.
        AssignMul,
        /// `lhs /= rhs`. main_token is op.
        AssignDiv,
        /// `lhs %= rhs`. main_token is op.
        AssignMod,
        /// `lhs += rhs`. main_token is op.
        AssignAdd,
        /// `lhs -= rhs`. main_token is op.
        AssignSub,
        /// `lhs <<= rhs`. main_token is op.
        AssignShl,
        /// `lhs <<|= rhs`. main_token is op.
        AssignShlSat,
        /// `lhs >>= rhs`. main_token is op.
        AssignShr,
        /// `lhs &= rhs`. main_token is op.
        AssignBitAnd,
        /// `lhs ^= rhs`. main_token is op.
        AssignBitXor,
        /// `lhs |= rhs`. main_token is op.
        AssignBitOr,
        /// `lhs *%= rhs`. main_token is op.
        AssignMulWrap,
        /// `lhs +%= rhs`. main_token is op.
        AssignAddWrap,
        /// `lhs -%= rhs`. main_token is op.
        AssignSubWrap,
        /// `lhs *|= rhs`. main_token is op.
        AssignMulSat,
        /// `lhs +|= rhs`. main_token is op.
        AssignAddSat,
        /// `lhs -|= rhs`. main_token is op.
        AssignSubSat,
        /// `lhs = rhs`. main_token is op.
        Assign,
        /// `a, b, ... = rhs`. main_token is op. lhs is index into `extra_data`
        /// of an lhs elem count followed by an array of that many `Node.Index`,
        /// with each node having one of the following types:
        /// * `global_var_decl`
        /// * `local_var_decl`
        /// * `simple_var_decl`
        /// * `aligned_var_decl`
        /// * Any expression node
        /// The first 3 types correspond to a `var` or `const` lhs node (note
        /// that their `rhs` is always 0). An expression node corresponds to a
        /// standard assignment LHS (which must be evaluated as an lvalue).
        /// There may be a preceding `comptime` token, which does not create a
        /// corresponding `comptime` node so must be manually detected.
        AssignDestructure,
        /// `lhs || rhs`. main_token is the `||`.
        MergeErrorSets,
        /// `lhs * rhs`. main_token is the `*`.
        Mul,
        /// `lhs / rhs`. main_token is the `/`.
        Div,
        /// `lhs % rhs`. main_token is the `%`.
        Mod,
        /// `lhs ** rhs`. main_token is the `**`.
        ArrayMult,
        /// `lhs *% rhs`. main_token is the `*%`.
        MulWrap,
        /// `lhs *| rhs`. main_token is the `*|`.
        MulSat,
        /// `lhs + rhs`. main_token is the `+`.
        Add,
        /// `lhs - rhs`. main_token is the `-`.
        Sub,
        /// `lhs ++ rhs`. main_token is the `++`.
        ArrayCat,
        /// `lhs +% rhs`. main_token is the `+%`.
        AddWrap,
        /// `lhs -% rhs`. main_token is the `-%`.
        SubWrap,
        /// `lhs +| rhs`. main_token is the `+|`.
        AddSat,
        /// `lhs -| rhs`. main_token is the `-|`.
        SubSat,
        /// `lhs << rhs`. main_token is the `<<`.
        Shl,
        /// `lhs <<| rhs`. main_token is the `<<|`.
        ShlSat,
        /// `lhs >> rhs`. main_token is the `>>`.
        Shr,
        /// `lhs & rhs`. main_token is the `&`.
        BitAnd,
        /// `lhs ^ rhs`. main_token is the `^`.
        BitXor,
        /// `lhs | rhs`. main_token is the `|`.
        BitOr,
        /// `lhs orelse rhs`. main_token is the `orelse`.
        Orelse,
        /// `lhs and rhs`. main_token is the `and`.
        BoolAnd,
        /// `lhs or rhs`. main_token is the `or`.
        BoolOr,
        /// `op lhs`. rhs unused. main_token is op.
        BoolNot,
        /// `op lhs`. rhs unused. main_token is op.
        Negation,
        /// `op lhs`. rhs unused. main_token is op.
        BitNot,
        /// `op lhs`. rhs unused. main_token is op.
        NegationWrap,
        /// `op lhs`. rhs unused. main_token is op.
        AddressOf,
        /// `op lhs`. rhs unused. main_token is op.
        Try,
        /// `op lhs`. rhs unused. main_token is op.
        Await,
        /// `?lhs`. rhs unused. main_token is the `?`.
        OptionalType,
        /// `[lhs]rhs`.
        ArrayType,
        /// `[lhs:a]b`. `ArrayTypeSentinel[rhs]`.
        ArrayTypeSentinel,
        /// `[*]align(lhs) rhs`. lhs can be omitted.
        /// `*align(lhs) rhs`. lhs can be omitted.
        /// `[]rhs`.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        PtrTypeAligned,
        /// `[*:lhs]rhs`. lhs can be omitted.
        /// `*rhs`.
        /// `[:lhs]rhs`.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        PtrTypeSentinel,
        /// lhs is index into ptr_type. rhs is the element type expression.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        PtrType,
        /// lhs is index into ptr_type_bit_range. rhs is the element type expression.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        PtrTypeBitRange,
        /// `lhs[rhs..]`
        /// main_token is the lbracket.
        SliceOpen,
        /// `lhs[b..c]`. rhs is index into Slice
        /// main_token is the lbracket.
        Slice,
        /// `lhs[b..c :d]`. rhs is index into SliceSentinel. Slice end "c" can be omitted.
        /// main_token is the lbracket.
        SliceSentinel,
        /// `lhs.*`. rhs is unused.
        Deref,
        /// `lhs[rhs]`.
        ArrayAccess,
        /// `lhs{rhs}`. rhs can be omitted.
        ArrayInitOne,
        /// `lhs{rhs,}`. rhs can *not* be omitted
        ArrayInitOneComma,
        /// `.{lhs, rhs}`. lhs and rhs can be omitted.
        ArrayInitDotTwo,
        /// Same as `array_init_dot_two` except there is known to be a trailing comma
        /// before the final rbrace.
        ArrayInitDotTwoComma,
        /// `.{a, b}`. `sub_list[lhs..rhs]`.
        ArrayInitDot,
        /// Same as `array_init_dot` except there is known to be a trailing comma
        /// before the final rbrace.
        ArrayInitDotComma,
        /// `lhs{a, b}`. `sub_range_list[rhs]`. lhs can be omitted which means `.{a, b}`.
        ArrayInit,
        /// Same as `array_init` except there is known to be a trailing comma
        /// before the final rbrace.
        ArrayInitComma,
        /// `lhs{.a = rhs}`. rhs can be omitted making it empty.
        /// main_token is the lbrace.
        StructInitOne,
        /// `lhs{.a = rhs,}`. rhs can *not* be omitted.
        /// main_token is the lbrace.
        StructInitOneComma,
        /// `.{.a = lhs, .b = rhs}`. lhs and rhs can be omitted.
        /// main_token is the lbrace.
        /// No trailing comma before the rbrace.
        StructInitDotTwo,
        /// Same as `struct_init_dot_two` except there is known to be a trailing comma
        /// before the final rbrace.
        StructInitDotTwoComma,
        /// `.{.a = b, .c = d}`. `sub_list[lhs..rhs]`.
        /// main_token is the lbrace.
        StructInitDot,
        /// Same as `struct_init_dot` except there is known to be a trailing comma
        /// before the final rbrace.
        StructInitDotComma,
        /// `lhs{.a = b, .c = d}`. `sub_range_list[rhs]`.
        /// lhs can be omitted which means `.{.a = b, .c = d}`.
        /// main_token is the lbrace.
        StructInit,
        /// Same as `struct_init` except there is known to be a trailing comma
        /// before the final rbrace.
        StructInitComma,
        /// `lhs(rhs)`. rhs can be omitted.
        /// main_token is the lparen.
        CallOne,
        /// `lhs(rhs,)`. rhs can be omitted.
        /// main_token is the lparen.
        CallOneComma,
        /// `async lhs(rhs)`. rhs can be omitted.
        AsyncCallOne,
        /// `async lhs(rhs,)`.
        AsyncCallOneComma,
        /// `lhs(a, b, c)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        Call,
        /// `lhs(a, b, c,)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        CallComma,
        /// `async lhs(a, b, c)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        AsyncCall,
        /// `async lhs(a, b, c,)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        AsyncCallComma,
        /// `switch(lhs) {}`. `SubRange[rhs]`.
        Switch,
        /// Same as switch except there is known to be a trailing comma
        /// before the final rbrace
        SwitchComma,
        /// `lhs => rhs`. If lhs is omitted it means `else`.
        /// main_token is the `=>`
        SwitchCaseOne,
        /// Same ast `switch_case_one` but the case is inline
        SwitchCaseInlineOne,
        /// `a, b, c => rhs`. `SubRange[lhs]`.
        /// main_token is the `=>`
        SwitchCase,
        /// Same ast `switch_case` but the case is inline
        SwitchCaseInline,
        /// `lhs...rhs`.
        SwitchRange,
        /// `while (lhs) rhs`.
        /// `while (lhs) |x| rhs`.
        WhileSimple,
        /// `while (lhs) : (a) b`. `WhileCont[rhs]`.
        /// `while (lhs) : (a) b`. `WhileCont[rhs]`.
        WhileCont,
        /// `while (lhs) : (a) b else c`. `While[rhs]`.
        /// `while (lhs) |x| : (a) b else c`. `While[rhs]`.
        /// `while (lhs) |x| : (a) b else |y| c`. `While[rhs]`.
        /// The cont expression part `: (a)` may be omitted.
        While,
        /// `for (lhs) rhs`.
        ForSimple,
        /// `for (lhs[0..inputs]) lhs[inputs + 1] else lhs[inputs + 2]`. `For[rhs]`.
        For,
        /// `lhs..rhs`. rhs can be omitted.
        ForRange,
        /// `if (lhs) rhs`.
        /// `if (lhs) |a| rhs`.
        IfSimple,
        /// `if (lhs) a else b`. `If[rhs]`.
        /// `if (lhs) |x| a else b`. `If[rhs]`.
        /// `if (lhs) |x| a else |y| b`. `If[rhs]`.
        If,
        /// `suspend lhs`. lhs can be omitted. rhs is unused.
        Suspend,
        /// `resume lhs`. rhs is unused.
        Resume,
        /// `continue`. lhs is token index of label if any. rhs is unused.
        Continue,
        /// `break :lhs rhs`
        /// both lhs and rhs may be omitted.
        Break,
        /// `return lhs`. lhs can be omitted. rhs is unused.
        Return,
        /// `fn (a: lhs) rhs`. lhs can be omitted.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        FnProtoSimple,
        /// `fn (a: b, c: d) rhs`. `sub_range_list[lhs]`.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        FnProtoMulti,
        /// `fn (a: b) rhs addrspace(e) linksection(f) callconv(g)`. `FnProtoOne[lhs]`.
        /// zero or one parameters.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        FnProtoOne,
        /// `fn (a: b, c: d) rhs addrspace(e) linksection(f) callconv(g)`. `FnProto[lhs]`.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        FnProto,
        /// lhs is the fn_proto.
        /// rhs is the function body block.
        /// Note that extern function declarations use the fn_proto tags rather
        /// than this one.
        FnDecl,
        /// `anyframe->rhs`. main_token is `anyframe`. `lhs` is arrow token index.
        AnyframeType,
        /// Both lhs and rhs unused.
        AnyframeLiteral,
        /// Both lhs and rhs unused.
        CharLiteral,
        /// Both lhs and rhs unused.
        NumberLiteral,
        /// Both lhs and rhs unused.
        UnreachableLiteral,
        /// Both lhs and rhs unused.
        /// Most identifiers will not have explicit AST nodes, however for expressions
        /// which could be one of many different kinds of AST nodes, there will be an
        /// identifier AST node for it.
        Identifier,
        /// lhs is the dot token index, rhs unused, main_token is the identifier.
        EnumLiteral,
        /// main_token is the string literal token
        /// Both lhs and rhs unused.
        StringLiteral,
        /// main_token is the first token index (redundant with lhs)
        /// lhs is the first token index; rhs is the last token index.
        /// Could be a series of multiline_string_literal_line tokens, or a single
        /// string_literal token.
        MultilineStringLiteral,
        /// `(lhs)`. main_token is the `(`; rhs is the token index of the `)`.
        GroupedExpression,
        /// `@a(lhs, rhs)`. lhs and rhs may be omitted.
        /// main_token is the builtin token.
        BuiltinCallTwo,
        /// Same as builtin_call_two but there is known to be a trailing comma before the rparen.
        BuiltinCallTwoComma,
        /// `@a(b, c)`. `sub_list[lhs..rhs]`.
        /// main_token is the builtin token.
        BuiltinCall,
        /// Same as builtin_call but there is known to be a trailing comma before the rparen.
        BuiltinCallComma,
        /// `error{a, b}`.
        /// rhs is the rbrace, lhs is unused.
        ErrorSetDecl,
        /// `struct {}`, `union {}`, `opaque {}`, `enum {}`. `extra_data[lhs..rhs]`.
        /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
        ContainerDecl,
        /// Same as ContainerDecl but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        ContainerDeclTrailing,
        /// `struct {lhs, rhs}`, `union {lhs, rhs}`, `opaque {lhs, rhs}`, `enum {lhs, rhs}`.
        /// lhs or rhs can be omitted.
        /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
        ContainerDeclTwo,
        /// Same as ContainerDeclTwo except there is known to be a trailing comma
        /// or semicolon before the rbrace.
        ContainerDeclTwoTrailing,
        /// `struct(lhs)` / `union(lhs)` / `enum(lhs)`. `SubRange[rhs]`.
        ContainerDeclArg,
        /// Same as container_decl_arg but there is known to be a trailing
        /// comma or semicolon before the rbrace.
        ContainerDeclArgTrailing,
        /// `union(enum) {}`. `sub_list[lhs..rhs]`.
        /// Note that tagged unions with explicitly provided enums are represented
        /// by `container_decl_arg`.
        TaggedUnion,
        /// Same as tagged_union but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        TaggedUnionTrailing,
        /// `union(enum) {lhs, rhs}`. lhs or rhs may be omitted.
        /// Note that tagged unions with explicitly provided enums are represented
        /// by `container_decl_arg`.
        TaggedUnionTwo,
        /// Same as tagged_union_two but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        TaggedUnionTwoTrailing,
        /// `union(enum(lhs)) {}`. `SubRange[rhs]`.
        TaggedUnionEnumTag,
        /// Same as tagged_union_enum_tag but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        TaggedUnionEnumTagTrailing,
        /// `a: lhs = rhs,`. lhs and rhs can be omitted.
        /// main_token is the field name identifier.
        /// lastToken() does not include the possible trailing comma.
        ContainerFieldInit,
        /// `a: lhs align(rhs),`. rhs can be omitted.
        /// main_token is the field name identifier.
        /// lastToken() does not include the possible trailing comma.
        ContainerFieldAlign,
        /// `a: lhs align(c) = d,`. `container_field_list[rhs]`.
        /// main_token is the field name identifier.
        /// lastToken() does not include the possible trailing comma.
        ContainerField,
        /// `comptime lhs`. rhs unused.
        Comptime,
        /// `nosuspend lhs`. rhs unused.
        Nosuspend,
        /// `{lhs rhs}`. rhs or lhs can be omitted.
        /// main_token points at the lbrace.
        BlockTwo,
        /// Same as block_two but there is known to be a semicolon before the rbrace.
        BlockTwoSemicolon,
        /// `{}`. `sub_list[lhs..rhs]`.
        /// main_token points at the lbrace.
        Block,
        /// Same as block but there is known to be a semicolon before the rbrace.
        BlockSemicolon,
        /// `asm(lhs)`. rhs is the token index of the rparen.
        AsmSimple,
        /// `asm(lhs, a)`. `Asm[rhs]`.
        Asm,
        /// `[a] "b" (c)`. lhs is 0, rhs is token index of the rparen.
        /// `[a] "b" (-> lhs)`. rhs is token index of the rparen.
        /// main_token is `a`.
        AsmOutput,
        /// `[a] "b" (lhs)`. rhs is token index of the rparen.
        /// main_token is `a`.
        AsmInput,
        /// `error.a`. lhs is token index of `.`. rhs is token index of `a`.
        ErrorValue,
        /// `lhs!rhs`. main_token is the `!`.
        ErrorUnion,
    }

    pub struct Data {
        pub lhs: Index,
        pub rhs: Index,
    }

    pub struct SubRange {
        pub start: Index,
        pub end: Index,
    }
}

impl<'src> std::fmt::Debug for Ast<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.dump(f, self.node(0), 0)
    }
}

impl<'src> Ast<'src> {
    /// For debugging purposes.
    pub fn dump(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        node: &Node,
        depth: usize,
    ) -> std::fmt::Result {
        enum DataType {
            Unknown,
            Unused,
            Node,
            Token,
            SubRange,
        }

        macro_rules! data {
            ($lhs:ident, $rhs:ident) => {
                (DataType::$lhs, DataType::$rhs)
            };
        }

        macro_rules! node {
            ($tag:ident) => {
                node::Tag::$tag
            };
        }

        let node_tag = node.tag;
        let node::Data { lhs, rhs } = node.data;
        let main_token = node.main_token;
        let main_token_tag = self.token_tag(main_token);
        let main_token_start = self.token_start(main_token);

        let indent = " ".repeat(depth * 2);
        write!(f, "{indent}{node_tag:?}")?;
        if node_tag == node!(Root) {
            write!(f, " ({:?})", self.mode)?;
        } else {
            write!(f, " @ source[{main_token_start}] / {main_token_tag:?}")?;
        }
        writeln!(f)?;

        let depth = depth + 1;
        let indent = " ".repeat(depth * 2);

        let (lhs_type, rhs_type) = match node_tag {
            node!(Root) => match self.mode {
                Mode::Zig => {
                    writeln!(f, "{indent}extra[{lhs}..{rhs}]")?;
                    for extra_index in lhs..rhs {
                        let node_index = self.extra_data(extra_index);
                        writeln!(f, "{indent}extra[{extra_index}] -> nodes[{node_index}]")?;
                        self.dump(f, self.node(node_index), depth + 1)?;
                    }
                    return Ok(());
                }
                Mode::Zon => data!(Node, Unused),
            },
            node!(Usingnamespace) => data!(Node, Unused),
            node!(TestDecl) => data!(Token, Node),
            // node!(GlobalVarDecl)
            // node!(LocalVarDecl)
            node!(SimpleVarDecl) => data!(Node, Node),
            // node!(AlignedVarDecl) => data!(Node, Node),
            // node!(Errdefer)
            // node!(Defer)
            node!(Catch) => data!(Node, Node),
            // node!(FieldAccess) => data!(Node, Token),
            // node!(UnwrapOptional) => data!(Node, Token),
            node!(EqualEqual) => data!(Node, Node),
            node!(BangEqual) => data!(Node, Node),
            node!(LessThan) => data!(Node, Node),
            node!(GreaterThan) => data!(Node, Node),
            node!(LessOrEqual) => data!(Node, Node),
            node!(GreaterOrEqual) => data!(Node, Node),
            node!(AssignMul) => data!(Node, Node),
            node!(AssignDiv) => data!(Node, Node),
            node!(AssignMod) => data!(Node, Node),
            node!(AssignAdd) => data!(Node, Node),
            node!(AssignSub) => data!(Node, Node),
            node!(AssignShl) => data!(Node, Node),
            node!(AssignShlSat) => data!(Node, Node),
            node!(AssignShr) => data!(Node, Node),
            node!(AssignBitAnd) => data!(Node, Node),
            node!(AssignBitXor) => data!(Node, Node),
            node!(AssignBitOr) => data!(Node, Node),
            node!(AssignMulWrap) => data!(Node, Node),
            node!(AssignAddWrap) => data!(Node, Node),
            node!(AssignSubWrap) => data!(Node, Node),
            node!(AssignMulSat) => data!(Node, Node),
            node!(AssignAddSat) => data!(Node, Node),
            node!(AssignSubSat) => data!(Node, Node),
            node!(Assign) => data!(Node, Node),
            // node!(AssignDestructure)
            node!(MergeErrorSets) => data!(Node, Node),
            node!(Mul) => data!(Node, Node),
            node!(Div) => data!(Node, Node),
            node!(Mod) => data!(Node, Node),
            node!(ArrayMult) => data!(Node, Node),
            node!(MulWrap) => data!(Node, Node),
            node!(MulSat) => data!(Node, Node),
            node!(Add) => data!(Node, Node),
            node!(Sub) => data!(Node, Node),
            node!(ArrayCat) => data!(Node, Node),
            node!(AddWrap) => data!(Node, Node),
            node!(SubWrap) => data!(Node, Node),
            node!(AddSat) => data!(Node, Node),
            node!(SubSat) => data!(Node, Node),
            node!(Shl) => data!(Node, Node),
            node!(ShlSat) => data!(Node, Node),
            node!(Shr) => data!(Node, Node),
            node!(BitAnd) => data!(Node, Node),
            node!(BitXor) => data!(Node, Node),
            node!(BitOr) => data!(Node, Node),
            node!(Orelse) => data!(Node, Node),
            node!(BoolAnd) => data!(Node, Node),
            node!(BoolOr) => data!(Node, Node),
            node!(BoolNot) => data!(Node, Unused),
            node!(Negation) => data!(Node, Unused),
            node!(BitNot) => data!(Node, Unused),
            node!(NegationWrap) => data!(Node, Unused),
            node!(AddressOf) => data!(Node, Unused),
            node!(Try) => data!(Node, Unused),
            node!(Await) => data!(Node, Unused),
            node!(OptionalType) => data!(Node, Unused),
            node!(ArrayType) => data!(Node, Node),
            // TODO
            node!(SliceOpen) => data!(Node, Node),
            // TODO
            node!(Deref) => data!(Node, Unused),
            node!(ArrayAccess) => data!(Node, Node),
            node!(ArrayInitOne) => data!(Node, Node),
            node!(ArrayInitOneComma) => data!(Node, Node),
            node!(ArrayInitDotTwo) => data!(Node, Node),
            node!(ArrayInitDotTwoComma) => data!(Node, Node),
            // TODO
            node!(StructInitOne) => data!(Node, Node),
            node!(StructInitOneComma) => data!(Node, Node),
            node!(StructInitDotTwo) => data!(Node, Node),
            node!(StructInitDotTwoComma) => data!(Node, Node),
            // TODO
            node!(Call) => data!(Node, SubRange),
            node!(CallOne) => data!(Node, Node),
            node!(CallOneComma) => data!(Node, Node),
            node!(AsyncCallOne) => data!(Node, Node),
            node!(AsyncCallOneComma) => data!(Node, Node),
            // TODO
            node!(SwitchCaseOne) => data!(Node, Node),
            node!(SwitchCaseInlineOne) => data!(Node, Node),
            // TODO
            node!(SwitchRange) => data!(Node, Node),
            node!(WhileSimple) => data!(Node, Node),
            // TODO
            node!(ForSimple) => data!(Node, Node),
            // TODO
            node!(ForRange) => data!(Node, Node),
            node!(IfSimple) => data!(Node, Node),
            node!(Suspend) => data!(Node, Unused),
            node!(Resume) => data!(Node, Unused),
            // TODO
            node!(Return) => data!(Node, Unused),
            node!(FnProtoSimple) => data!(Node, Node),
            // TODO
            node!(AnyframeLiteral) => data!(Unused, Unused),
            node!(CharLiteral) => data!(Unused, Unused),
            node!(NumberLiteral) => data!(Unused, Unused),
            node!(UnreachableLiteral) => data!(Unused, Unused),
            node!(Identifier) => data!(Unused, Unused),
            node!(StringLiteral) => data!(Unused, Unused),
            // TODO
            node!(BuiltinCallTwo) => data!(Node, Node),
            node!(BuiltinCallTwoComma) => data!(Node, Node),
            // TODO
            node!(ErrorSetDecl) => data!(Unused, Token),
            // TODO
            node!(ContainerDeclTwo) => data!(Node, Node),
            node!(ContainerDeclTwoTrailing) => data!(Node, Node),
            // TODO
            node!(TaggedUnionTwo) => data!(Node, Node),
            node!(TaggedUnionTwoTrailing) => data!(Node, Node),
            // TODO
            node!(ContainerFieldInit) => data!(Node, Node),
            // TODO
            node!(Comptime) => data!(Node, Unused),
            node!(Nosuspend) => data!(Node, Unused),
            node!(BlockTwo) => data!(Node, Node),
            node!(BlockTwoSemicolon) => data!(Node, Node),
            // TODO
            node!(ErrorUnion) => data!(Node, Node),
            _ => data!(Unknown, Unknown),
        };

        for (name, data, data_type) in [("lhs", lhs, lhs_type), ("rhs", rhs, rhs_type)] {
            match data_type {
                DataType::Unknown => writeln!(f, "{indent}{name}: ? / {data}")?,
                DataType::Unused => writeln!(f, "{indent}{name}: unused / {data}")?,
                DataType::Node => {
                    if data == 0 {
                        writeln!(f, "{indent}{name}: omitted / {data}")?;
                    } else {
                        writeln!(f, "{indent}{name}: nodes[{data}]")?;
                        self.dump(f, self.node(data), depth + 1)?;
                    }
                }
                DataType::Token => {
                    if data == 0 {
                        writeln!(f, "{indent}{name}: omitted / {data}")?;
                    } else {
                        let token_tag = self.token_tag(data);
                        let token_start = self.token_start(data);
                        writeln!(
                            f,
                            "{indent}{name}: tokens[{data}] @ source[{token_start}] / {token_tag:?}"
                        )?;
                    }
                }
                DataType::SubRange => {
                    let data_end = data + 1;
                    let start = self.extra_data(data);
                    let end = self.extra_data(data_end);
                    writeln!(
                        f,
                        "{indent}{name}: extra[{data}..={data_end}] -> extra[{start}..{end}]"
                    )?;
                    for extra_index in start..end {
                        let node_index = self.extra_data(extra_index);
                        writeln!(f, "{indent}extra[{extra_index}] -> nodes[{node_index}]")?;
                        self.dump(f, self.node(node_index), depth + 1)?;
                    }
                }
            }
        }

        Ok(())
    }
}
