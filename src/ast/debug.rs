use super::node::ExtraData;
use super::*;

use crate::macros::node;

const DUMP_UNUSED_DATA: bool = false;

enum DataType {
    Unknown,

    Unused,
    Node,
    Token,
    NodeSlice,

    Extra(ExtraDataType),
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
    (extra!($lhs:ident), $rhs:ident) => {
        PairType::Pair(DataType::Extra(ExtraDataType::$lhs), DataType::$rhs)
    };
    ($lhs:ident, extra!($rhs:ident)) => {
        PairType::Pair(DataType::$lhs, DataType::Extra(ExtraDataType::$rhs))
    };
}

fn data_types_of(mode: Mode, tag: node::Tag) -> PairType {
    match tag {
        node!(Root) => match mode {
            Mode::Zig => data!(SubList),
            Mode::Zon => data!(Node, Unused),
        },
        node!(Usingnamespace) => data!(Node, Unused),
        node!(TestDecl) => data!(Token, Node),
        node!(GlobalVarDecl) => data!(extra!(GlobalVarDecl), Node),
        node!(LocalVarDecl) => data!(extra!(LocalVarDecl), Node),
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
        node!(AssignDestructure) => data!(NodeSlice, Node),
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
        node!(ArrayTypeSentinel) => data!(Node, extra!(ArrayTypeSentinel)),
        node!(PtrTypeAligned) => data!(Node, Node),
        node!(PtrTypeSentinel) => data!(Node, Node),
        node!(PtrType) => data!(extra!(PtrType), Node),
        // node!(PtrTypeBitRange) => data!(extra!(PtrTypeBitRange), Node),
        node!(SliceOpen) => data!(Node, Node),
        // node!(Slice) => data!(Node, extra!(Slice)),
        // node!(SliceSentinel) => data!(Node, extra!(SliceSentinel)),
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
        node!(Call) => data!(Node, extra!(SubRange)),
        node!(CallComma) => data!(Node, extra!(SubRange)),
        node!(AsyncCall) => data!(Node, extra!(SubRange)),
        node!(AsyncCallComma) => data!(Node, extra!(SubRange)),
        // node!(Switch) => data!(Node, extra!(SubRange)),
        node!(SwitchCaseOne) => data!(Node, Node),
        node!(SwitchCaseInlineOne) => data!(Node, Node),
        // node!(SwitchCase) => data!(extra!(SubRange), Node),
        // node!(SwitchCaseInline) => data!(extra!(SubRange), Node),
        node!(SwitchRange) => data!(Node, Node),
        node!(WhileSimple) => data!(Node, Node),
        // node!(WhileCont) => data!(Node, extra!(WhileCont)),
        // node!(While) => data!(Node, extra!(While)),
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
        node!(FnProtoMulti) => data!(extra!(SubRange), Node),
        node!(FnProtoOne) => data!(extra!(FnProtoOne), Node),
        node!(FnProto) => data!(extra!(FnProto), Node),
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
        // node!(ContainerDeclArg) => data!(Node, extra!(SubRange)),
        // node!(ContainerDeclArgTrailing) => data!(Node, extra!(SubRange)),
        node!(TaggedUnion) => data!(SubList),
        node!(TaggedUnionTrailing) => data!(SubList),
        node!(TaggedUnionTwo) => data!(Node, Node),
        node!(TaggedUnionTwoTrailing) => data!(Node, Node),
        // node!(TaggedUnionEnumTag) => data!(Node, extra!(SubRange)),
        // node!(TaggedUnionEnumTagTrailing) => data!(Node, extra!(SubRange)),
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
        // node!(Asm) => data!(Unknown, extra!(Asm)),
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

fn indent(depth: usize) -> String {
    " ".repeat(depth * 2)
}

macro_rules! dump_header {
    ($f:ident, $depth:expr, $name:expr, $origin:expr, $($arg:tt)*) => {{
        write!($f, "{}", indent($depth))?;
        if let Some(name) = $name {
            write!($f, "{name}: ")?;
        }
        if let Some(origin) = $origin {
            write!($f, "extra[{origin:?}] -> ")?;
        }
        writeln!($f, $($arg)*)
    }};
    ($f:ident, $depth:expr, $name:expr, $($arg:tt)*) => {
        dump_header!($f, $depth, $name, None::<()>, $($arg)*)
    };
}

impl Ast<'_> {
    pub fn dump_token(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        origin: Option<node::Index>,
        index: TokenIndex,
    ) -> std::fmt::Result {
        if index == 0 {
            dump_header!(f, depth, name, origin, "omitted token")
        } else {
            let token_tag = self.token_tag(index);
            let token_start = self.token_start(index);
            dump_header!(
                f,
                depth,
                name,
                origin,
                "token[{index}] @ source[{token_start}] / {token_tag:?}"
            )
        }
    }

    pub fn dump_child(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        origin: Option<node::Index>,
        index: node::Index,
    ) -> std::fmt::Result {
        if index == 0 {
            dump_header!(f, depth, name, origin, "omitted node")
        } else {
            dump_header!(f, depth, name, origin, "node[{index}]")?;
            self.dump(f, self.node(index), depth + 1)
        }
    }

    pub fn dump_children(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        origin: Option<impl std::fmt::Debug>,
        range: std::ops::Range<node::Index>,
    ) -> std::fmt::Result {
        dump_header!(f, depth, name, origin, "extra[{range:?}]")?;
        for extra_index in range {
            let node_index = self.extra_data(extra_index);
            self.dump_child(f, depth, None, Some(extra_index), node_index)?;
        }
        Ok(())
    }

    pub fn dump(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        node: &Node,
        depth: usize,
    ) -> std::fmt::Result {
        let mode = self.mode;

        let Node {
            tag,
            main_token,
            data: node::Data { lhs, rhs },
        } = *node;

        write!(f, "{}", indent(depth))?;
        if tag == node!(Root) {
            writeln!(f, "{tag:?} ({mode:?})")?;
        } else {
            let token_tag = self.token_tag(main_token);
            let token_start = self.token_start(main_token);
            writeln!(f, "{tag:?} @ source[{token_start}] / {token_tag:?}")?;
        }

        let pair_type = data_types_of(mode, tag);
        let (lhs_type, rhs_type) = match pair_type {
            PairType::SubList => return self.dump_children(f, depth, None, None::<()>, lhs..rhs),
            PairType::Pair(l, r) => (l, r),
        };

        for (name, data, data_type) in [("lhs", lhs, lhs_type), ("rhs", rhs, rhs_type)] {
            match data_type {
                DataType::Unknown => dump_header!(f, depth, Some(name), "? / {data}")?,
                DataType::Unused => {
                    if DUMP_UNUSED_DATA {
                        dump_header!(f, depth, Some(name), "unused / {data}")?;
                    }
                }
                DataType::Node => self.dump_child(f, depth, Some(name), None, data)?,
                DataType::Token => self.dump_token(f, depth, Some(name), None, data)?,
                DataType::NodeSlice => {
                    let count: node::Index = self.extra_data(data);
                    let start = data + 1;
                    let end = start + count;
                    self.dump_children(f, depth, Some(name), Some(data), start..end)?;
                }
                DataType::Extra(extra_data_type) => {
                    extra_data_type.dump_extra_data(self, f, depth, Some(name), data)?;
                }
            }
        }

        Ok(())
    }
}

impl node::SubRange {
    pub fn dump_data(
        &self,
        tree: &Ast,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        start: node::Index,
    ) -> std::fmt::Result {
        let origin = Self::field_range(start);
        let range = self.start..self.end;
        tree.dump_children(f, depth, name, Some(origin), range)
    }
}

impl node::FnProto {
    pub fn dump_data(
        &self,
        tree: &Ast,
        f: &mut std::fmt::Formatter<'_>,
        depth: usize,
        name: Option<&str>,
        start: node::Index,
    ) -> std::fmt::Result {
        let origin = Self::field_range(start);
        dump_header!(f, depth, name, Some(origin), "FnProto")?;

        let node::FnProto {
            params_start,
            params_end,
            align_expr,
            addrspace_expr,
            section_expr,
            callconv_expr,
        } = *self;

        {
            let name = "params";
            let origin = start + 0..start + 2;
            let range = params_start..params_end;
            tree.dump_children(f, depth, Some(name), Some(origin), range)?;
        }

        for (name, origin, index) in [
            ("align_expr", start + 2, align_expr),
            ("addrspace_expr", start + 3, addrspace_expr),
            ("section_expr", start + 4, section_expr),
            ("callconv_expr", start + 5, callconv_expr),
        ] {
            tree.dump_child(f, depth, Some(name), Some(origin), index)?;
        }

        Ok(())
    }
}

macro_rules! extra_data_all_nodes {
    ($type:ident) => {
        impl node::$type {
            pub fn dump_data(
                &self,
                tree: &Ast,
                f: &mut std::fmt::Formatter<'_>,
                depth: usize,
                name: Option<&str>,
                start: node::Index,
            ) -> std::fmt::Result {
                let origin = Self::field_range(start);
                dump_header!(f, depth, name, Some(origin), stringify!($type))?;
                self.dump_fields_as_nodes(tree, f, depth, start)
            }
        }
    };
}

macro_rules! extra_data_todo {
    ($type:ident) => {
        impl node::$type {
            pub fn dump_data(
                &self,
                tree: &Ast,
                f: &mut std::fmt::Formatter<'_>,
                depth: usize,
                name: Option<&str>,
                start: node::Index,
            ) -> std::fmt::Result {
                todo!(concat!(stringify!($type), "::dump_data"))
            }
        }
    };
}

macro_rules! extra_data_impl {
    ($type:ident, $($field:ident),*) => {
        impl crate::ast::node::$type {
            pub fn dump_fields_as_nodes(
                &self,
                tree: &crate::Ast,
                f: &mut std::fmt::Formatter<'_>,
                depth: usize,
                start: crate::ast::node::Index,
            ) -> std::fmt::Result {
                let Self { $($field),* } = *self;
                let mut origin = start;
                $(
                    tree.dump_child(f, depth, Some(stringify!($field)), Some(origin), $field)?;
                    origin += 1;
                )*
                Ok(())
            }
        }
    };
}

pub(crate) use extra_data_impl;

macro_rules! extra_data_types {
    ($($type:ident,)*) => {
        enum ExtraDataType {
            $($type),*
        }

        impl ExtraDataType {
            fn dump_extra_data(
                &self,
                tree: &crate::Ast,
                f: &mut std::fmt::Formatter<'_>,
                depth: usize,
                name: Option<&str>,
                start: crate::ast::node::Index,
            ) -> std::fmt::Result {
                match self {
                    $(
                        ExtraDataType::$type => {
                            let extra: node::$type = tree.extra_data(start);
                            extra.dump_data(tree, f, depth, name, start)
                        }
                    )*
                }
            }
        }
    };
}

extra_data_types! {
    LocalVarDecl,
    ArrayTypeSentinel,
    PtrType,
    PtrTypeBitRange,
    SubRange,
    If,
    ContainerField,
    GlobalVarDecl,
    Slice,
    SliceSentinel,
    While,
    WhileCont,
    For,
    FnProtoOne,
    FnProto,
    Asm,
}

extra_data_all_nodes!(LocalVarDecl);
extra_data_all_nodes!(ArrayTypeSentinel);
extra_data_all_nodes!(PtrType);
extra_data_all_nodes!(PtrTypeBitRange);
// extra_data_todo!(SubRange);
extra_data_all_nodes!(If);
extra_data_all_nodes!(ContainerField);
extra_data_all_nodes!(GlobalVarDecl);
extra_data_all_nodes!(Slice);
extra_data_all_nodes!(SliceSentinel);
extra_data_all_nodes!(While);
extra_data_all_nodes!(WhileCont);
extra_data_all_nodes!(For);
extra_data_all_nodes!(FnProtoOne);
// extra_data_todo!(FnProto);
extra_data_todo!(Asm);
