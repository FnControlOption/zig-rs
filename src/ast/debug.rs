use super::node::ExtraData;
use super::*;

use crate::macros::node;

const DUMP_UNUSED_DATA: bool = false;

// enum ExtraDataType {}

enum DataType {
    Unknown,

    Unused,
    Node,
    Token,
    ElemCount,

    GlobalVarDecl,
    LocalVarDecl,
    ArrayTypeSentinel,
    SubRange,
    PtrType,
}

enum PairType {
    SubList,
    Pair(DataType, DataType),
}

macro_rules! data {
    ($type:ident) => {
        PairType::$type
    };
    ($lhs:ident, $rhs:ident) => {
        PairType::Pair(DataType::$lhs, DataType::$rhs)
    };
}

fn data_types_of(mode: Mode, node_tag: node::Tag) -> PairType {
    match node_tag {
        node!(Root) => match mode {
            Mode::Zig => data!(SubList),
            Mode::Zon => data!(Node, Unused),
        },
        node!(Usingnamespace) => data!(Node, Unused),
        node!(TestDecl) => data!(Token, Node),
        node!(GlobalVarDecl) => data!(GlobalVarDecl, Node),
        node!(LocalVarDecl) => data!(LocalVarDecl, Node),
        node!(SimpleVarDecl) => data!(Node, Node),
        node!(AlignedVarDecl) => data!(Node, Node),
        node!(Errdefer) => data!(Token, Node),
        node!(Defer) => data!(Unused, Node),
        node!(Catch) => data!(Node, Node),
        node!(FieldAccess) => data!(Node, Token),
        node!(UnwrapOptional) => data!(Node, Token),
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
        node!(AssignDestructure) => data!(ElemCount, Node),
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
        node!(ArrayTypeSentinel) => data!(Node, ArrayTypeSentinel),
        node!(PtrTypeAligned) => data!(Node, Node),
        node!(PtrTypeSentinel) => data!(Node, Node),
        node!(PtrType) => data!(PtrType, Node),
        // node!(PtrTypeBitRange) => data!(PtrTypeBitRange, Node),
        node!(SliceOpen) => data!(Node, Node),
        // node!(Slice) => data!(Node, Slice),
        // node!(SliceSentinel) => data!(Node, SliceSentinel),
        node!(Deref) => data!(Node, Unused),
        node!(ArrayAccess) => data!(Node, Node),
        node!(ArrayInitOne) => data!(Node, Node),
        node!(ArrayInitOneComma) => data!(Node, Node),
        node!(ArrayInitDotTwo) => data!(Node, Node),
        node!(ArrayInitDotTwoComma) => data!(Node, Node),
        // node!(ArrayInitDot) => data!(SubList),
        // node!(ArrayInitDotComma) => data!(SubList),
        // node!(ArrayInit) => data!(SubList),
        // node!(ArrayInitComma) => data!(SubList),
        node!(StructInitOne) => data!(Node, Node),
        node!(StructInitOneComma) => data!(Node, Node),
        node!(StructInitDotTwo) => data!(Node, Node),
        node!(StructInitDotTwoComma) => data!(Node, Node),
        // node!(StructInitDot) => data!(SubList),
        // node!(StructInitDotComma) => data!(SubList),
        // node!(StructInit) => data!(SubList),
        // node!(StructInitComma) => data!(SubList),
        node!(CallOne) => data!(Node, Node),
        node!(CallOneComma) => data!(Node, Node),
        node!(AsyncCallOne) => data!(Node, Node),
        node!(AsyncCallOneComma) => data!(Node, Node),
        node!(Call) => data!(Node, SubRange),
        node!(CallComma) => data!(Node, SubRange),
        node!(AsyncCall) => data!(Node, SubRange),
        node!(AsyncCallComma) => data!(Node, SubRange),
        // node!(Switch) => data!(Node, SubRange),
        node!(SwitchCaseOne) => data!(Node, Node),
        node!(SwitchCaseInlineOne) => data!(Node, Node),
        // node!(SwitchCase) => data!(SubRange, Node),
        // node!(SwitchCaseInline) => data!(SubRange, Node),
        node!(SwitchRange) => data!(Node, Node),
        node!(WhileSimple) => data!(Node, Node),
        // node!(WhileCont) => data!(Node, WhileCont),
        // node!(While) => data!(Node, While),
        node!(ForSimple) => data!(Node, Node),
        // node!(For) => data!(Unknown, Unknown),
        node!(ForRange) => data!(Node, Node),
        node!(IfSimple) => data!(Node, Node),
        node!(Suspend) => data!(Node, Unused),
        node!(Resume) => data!(Node, Unused),
        // node!(Continue) => data!(Token, Unused),
        // node!(Break) => data!(Unknown, Node),
        node!(Return) => data!(Node, Unused),
        node!(FnProtoSimple) => data!(Node, Node),
        node!(FnProtoMulti) => data!(SubRange, Node),
        // node!(FnProtoOne) => data!(FnProtoOne, Node),
        // node!(FnProto) => data!(FnProto, Node),
        node!(FnDecl) => data!(Node, Node),
        // node!(AnyframeType) => data!(Token, Node),
        node!(AnyframeLiteral) => data!(Unused, Unused),
        node!(CharLiteral) => data!(Unused, Unused),
        node!(NumberLiteral) => data!(Unused, Unused),
        node!(UnreachableLiteral) => data!(Unused, Unused),
        node!(Identifier) => data!(Unused, Unused),
        node!(StringLiteral) => data!(Unused, Unused),
        // node!(MultilineStringLiteral) => data!(Token, Token),
        // node!(GroupedExpression) => data!(Node, Token),
        node!(BuiltinCallTwo) => data!(Node, Node),
        node!(BuiltinCallTwoComma) => data!(Node, Node),
        // node!(BuiltinCall) => data!(SubList),
        // node!(BuiltinCallComma) => data!(SubList),
        node!(ErrorSetDecl) => data!(Unused, Token),
        // node!(ContainerDecl) => data!(Unknown, Unknown),
        // node!(ContainerDeclTrailing) => data!(Unknown, Unknown),
        node!(ContainerDeclTwo) => data!(Node, Node),
        node!(ContainerDeclTwoTrailing) => data!(Node, Node),
        // node!(ContainerDeclArg) => data!(Node, SubRange),
        // node!(ContainerDeclArgTrailing) => data!(Node, SubRange),
        node!(TaggedUnion) => data!(SubList),
        node!(TaggedUnionTrailing) => data!(SubList),
        node!(TaggedUnionTwo) => data!(Node, Node),
        node!(TaggedUnionTwoTrailing) => data!(Node, Node),
        // node!(TaggedUnionEnumTag) => data!(Node, SubRange),
        // node!(TaggedUnionEnumTagTrailing) => data!(Node, SubRange),
        node!(ContainerFieldInit) => data!(Node, Node),
        // node!(ContainerFieldAlign) => data!(Node, Node),
        // node!(ContainerField) => data!(Node, Node),
        node!(Comptime) => data!(Node, Unused),
        node!(Nosuspend) => data!(Node, Unused),
        node!(BlockTwo) => data!(Node, Node),
        node!(BlockTwoSemicolon) => data!(Node, Node),
        node!(Block) => data!(SubList),
        node!(BlockSemicolon) => data!(SubList),
        // node!(AsmSimple) => data!(Unknown, Token),
        // node!(Asm) => data!(Unknown, Asm),
        // node!(AsmOutput) => data!(Unknown, Token),
        // node!(AsmInput) => data!(Unknown, Token),
        node!(ErrorValue) => data!(Token, Token),
        node!(ErrorUnion) => data!(Node, Node),
        _ => data!(Unknown, Unknown),
    }
}

impl std::fmt::Debug for Ast<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.dump(f, self.node(0), 0)
    }
}

macro_rules! indent {
    ($f:ident, $depth:expr, $($arg:tt)*) => {{
        let indent = " ".repeat($depth * 2);
        write!($f, "{indent}")?;
        writeln!($f, $($arg)*)
    }};
}

macro_rules! header {
    ($f:ident, $depth:expr, $name:expr, $origin:expr, $($arg:tt)*) => {{
        let indent = " ".repeat($depth * 2);
        write!($f, "{indent}")?;
        if let Some(name) = $name {
            write!($f, "{name}: ")?;
        }
        if let Some(origin) = $origin {
            write!($f, "extra[{origin:?}] -> ")?;
        }
        writeln!($f, $($arg)*)
    }};
    ($f:ident, $depth:expr, $name:expr, $($arg:tt)*) => {
        header!($f, $depth, $name, None::<()>, $($arg)*)
    };
}

pub(crate) use header;

impl Ast<'_> {
    /// For debugging purposes.
    pub fn dump(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        node: &Node,
        depth: usize,
    ) -> std::fmt::Result {
        let mode = self.mode;
        let node_tag = node.tag;
        let node::Data { lhs, rhs } = node.data;
        let main_token = node.main_token;
        let main_token_tag = self.token_tag(main_token);
        let main_token_start = self.token_start(main_token);

        if node_tag == node!(Root) {
            indent!(f, depth, "{node_tag:?} ({mode:?})")?;
        } else {
            indent!(
                f,
                depth,
                "{node_tag:?} @ source[{main_token_start}] / {main_token_tag:?}"
            )?;
        }

        let pair_type = data_types_of(mode, node_tag);
        let (lhs_type, rhs_type) = match pair_type {
            PairType::SubList => return self.dump_node_list(f, depth, None, None::<()>, lhs..rhs),
            PairType::Pair(l, r) => (l, r),
        };

        for (name, data, data_type) in [("lhs", lhs, lhs_type), ("rhs", rhs, rhs_type)] {
            match data_type {
                DataType::Unknown => header!(f, depth, Some(name), "? / {data}")?,
                DataType::Unused => {
                    if DUMP_UNUSED_DATA {
                        header!(f, depth, Some(name), "unused / {data}")?;
                    }
                }
                DataType::Node => self.dump_node_at(f, depth, Some(name), None, data)?,
                DataType::Token => self.dump_token_at(f, depth, Some(name), None, data)?,
                DataType::ElemCount => {
                    let count = self.extra_data(data);
                    let start = data + 1;
                    let end = start + count;
                    self.dump_node_list(f, depth, Some(name), Some(data), start..end)?;
                }
                DataType::GlobalVarDecl => {
                    let origin = node::GlobalVarDecl::field_range(data);
                    let decl = node::GlobalVarDecl::from_start(self, data);
                    decl.dump_as_nodes(self, f, depth, Some(name), Some(origin), data)?;
                }
                DataType::LocalVarDecl => {
                    let origin = node::LocalVarDecl::field_range(data);
                    let decl = node::LocalVarDecl::from_start(self, data);
                    decl.dump_as_nodes(self, f, depth, Some(name), Some(origin), data)?;
                }
                DataType::ArrayTypeSentinel => {
                    let origin = node::ArrayTypeSentinel::field_range(data);
                    let decl = node::ArrayTypeSentinel::from_start(self, data);
                    decl.dump_as_nodes(self, f, depth, Some(name), Some(origin), data)?;
                }
                DataType::PtrType => {
                    let origin = node::PtrType::field_range(data);
                    let decl = node::PtrType::from_start(self, data);
                    decl.dump_as_nodes(self, f, depth, Some(name), Some(origin), data)?;
                }
                DataType::SubRange => {
                    let origin = node::SubRange::field_range(data);
                    let range = node::SubRange::from_start(self, data);
                    let node::SubRange { start, end } = range;
                    self.dump_node_list(f, depth, Some(name), Some(origin), start..end)?;
                }
            }
        }

        Ok(())
    }

    pub fn dump_token_at(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        origin: Option<node::Index>,
        index: TokenIndex,
    ) -> std::fmt::Result {
        if index == 0 {
            header!(f, depth, name, origin, "omitted token")
        } else {
            let token_tag = self.token_tag(index);
            let token_start = self.token_start(index);
            header!(
                f,
                depth,
                name,
                origin,
                "token[{index}] @ source[{token_start}] / {token_tag:?}"
            )
        }
    }

    pub fn dump_node_at(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        origin: Option<node::Index>,
        index: node::Index,
    ) -> std::fmt::Result {
        if index == 0 {
            header!(f, depth, name, origin, "omitted node")
        } else {
            header!(f, depth, name, origin, "node[{index}]")?;
            self.dump(f, self.node(index), depth + 1)
        }
    }

    pub fn dump_node_list(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        origin: Option<impl std::fmt::Debug>,
        range: std::ops::Range<node::Index>,
    ) -> std::fmt::Result {
        header!(f, depth, name, origin, "extra[{range:?}]")?;
        for extra_index in range {
            let node_index = self.extra_data(extra_index);
            self.dump_node_at(f, depth, None, Some(extra_index), node_index)?;
        }
        Ok(())
    }
}

macro_rules! extra_data_impl {
    ($name:ident, $($field:ident),*) => {
        impl $name {
            pub fn dump_as_nodes(
                &self,
                tree: &crate::Ast,
                f: &mut std::fmt::Formatter<'_>,
                depth: usize,
                name: Option<&str>,
                origin: Option<std::ops::Range<usize>>,
                start_index: crate::ast::node::Index,
            ) -> std::fmt::Result {
                crate::ast::debug::header!(f, depth, name, origin, "{}", stringify!($name))?;
                let Self { $($field),* } = *self;
                let mut extra_index = start_index;
                $(
                    tree.dump_node_at(f, depth, Some(stringify!($field)), Some(extra_index), $field)?;
                    extra_index += 1;
                )*
                Ok(())
            }
        }
    };
}

pub(crate) use extra_data_impl;
