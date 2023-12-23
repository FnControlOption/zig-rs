use super::*;

pub struct Node {
    pub tag: Tag,
    pub main_token: TokenIndex,
    pub data: Data,
}

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

pub trait ExtraData<const N: usize>: Sized {
    fn to_array(&self) -> [Index; N];

    fn from_slice(slice: &[Index]) -> Self;

    fn index_to_range(index: Index) -> std::ops::Range<usize> {
        let start = index as usize;
        let end = start + N;
        start..end
    }

    fn from_ast(ast: &Ast, index: Index) -> Self {
        Self::from_slice(&ast.extra_data[Self::index_to_range(index)])
    }
}

macro_rules! extra_data {
        ($name:ident, $size:literal, $($(#[$attr:meta])* $field:ident: Index $(,)?)*) => {
            pub struct $name {
                $($(#[$attr])* pub $field: Index,)*
            }

            impl ExtraData<$size> for $name {
                fn to_array(&self) -> [Index; $size] {
                    [$(self.$field,)*]
                }

                fn from_slice(slice: &[Index]) -> Self {
                    let mut index = 0;
                    $(let $field = slice[index]; index += 1;)*
                    Self { $($field,)* }
                }
            }
        };
    }

extra_data! {
    SubRange, 2,
    /// Index into sub_list.
    start: Index,
    /// Index into sub_list.
    end: Index,
}

extra_data! {
    If, 0,
}

extra_data! {
    ContainerField, 2,
    value_expr: Index,
    align_expr: Index,
}

extra_data! {
    GlobalVarDecl, 0,
}

extra_data! {
    Slice, 0,
}

extra_data! {
    SliceSentinel, 0,
}

extra_data! {
    While, 0,
}

extra_data! {
    WhileCont, 0,
}

extra_data! {
    For, 0,
}

extra_data! {
    FnProtoOne, 5,
    /// Populated if there is exactly 1 parameter. Otherwise there are 0 parameters.
    param: Index,
    /// Populated if align(A) is present.
    align_expr: Index,
    /// Populated if addrspace(A) is present.
    addrspace_expr: Index,
    /// Populated if linksection(A) is present.
    section_expr: Index,
    /// Populated if callconv(A) is present.
    callconv_expr: Index,
}

extra_data! {
    FnProto, 6,
    params_start: Index,
    params_end: Index,
    /// Populated if align(A) is present.
    align_expr: Index,
    /// Populated if addrspace(A) is present.
    addrspace_expr: Index,
    /// Populated if linksection(A) is present.
    section_expr: Index,
    /// Populated if callconv(A) is present.
    callconv_expr: Index,
}

extra_data! {
    Asm, 0,
}
