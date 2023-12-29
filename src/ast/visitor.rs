use super::*;

pub trait Visitor: Sized {
    fn visit_any(&mut self, tree: &Ast, node: &Node) -> bool {
        true
    }

    fn visit(&mut self, tree: &Ast, node: &Node) -> bool;

    fn end_visit(&mut self, tree: &Ast, node: &Node) {}

    fn end_visit_any(&mut self, tree: &Ast, node: &Node) {}

    fn accept(&mut self, tree: &Ast, node: &Node) {
        node.accept(tree, self)
    }

    fn accept_child(&mut self, tree: &Ast, index: node::Index) {
        if index != 0 {
            self.accept(tree, tree.node(index));
        }
    }

    fn accept_extra_child(&mut self, tree: &Ast, index: node::Index) {
        self.accept_child(tree, tree.extra_data(index));
    }

    fn accept_extra_children(&mut self, tree: &Ast, range: std::ops::Range<node::Index>) {
        for index in range {
            self.accept_extra_child(tree, index);
        }
    }

    fn accept_token(&mut self, tree: &Ast, index: TokenIndex) {}
}

impl Ast<'_> {
    pub fn accept(&self, visitor: &mut impl Visitor) {
        self.node(0).accept(self, visitor);
    }
}

impl Node {
    pub fn accept(&self, tree: &Ast, visitor: &mut impl Visitor) {
        if visitor.visit_any(tree, self) {
            if visitor.visit(tree, self) {
                self.accept_children(tree, visitor);
            }
            visitor.end_visit(tree, self);
            visitor.end_visit_any(tree, self);
        }
    }

    pub fn accept_children(&self, tree: &Ast, visitor: &mut impl Visitor) {
        let node::Data { lhs, rhs } = self.data;
        match get_data_types(tree.mode, self.tag) {
            PairType::SubList => visitor.accept_extra_children(tree, lhs..rhs),
            PairType::Pair(lhs_type, rhs_type) => {
                for (data, data_type) in [(lhs, lhs_type), (rhs, rhs_type)] {
                    match data_type {
                        DataType::Unused => {}
                        DataType::Node => visitor.accept_child(tree, data),
                        DataType::Token => visitor.accept_token(tree, data),
                        DataType::NodeSlice => {
                            let count: node::Index = tree.extra_data(data);
                            let start = data + 1;
                            let end = start + count;
                            visitor.accept_extra_children(tree, start..end);
                        }
                        DataType::Extra(extra_data_type) => {
                            extra_data_type.accept_children(tree, data, visitor);
                        }
                    }
                }
            }
            PairType::For => {
                let extra = node::For(rhs);
                let inputs = extra.get_inputs();
                let has_else = extra.get_has_else();
                visitor.accept_extra_children(tree, lhs..lhs + inputs);
                visitor.accept_extra_child(tree, lhs + inputs);
                if has_else {
                    visitor.accept_extra_child(tree, lhs + inputs + 1);
                } else {
                    visitor.accept_child(tree, 0);
                }
            }
        };
    }
}

impl node::SubRange {
    pub fn accept_children(&self, tree: &Ast, visitor: &mut impl Visitor) {
        visitor.accept_extra_children(tree, self.start..self.end)
    }
}

impl node::FnProto {
    pub fn accept_children(&self, tree: &Ast, visitor: &mut impl Visitor) {
        visitor.accept_extra_children(tree, self.params_start..self.params_end);
        visitor.accept_child(tree, self.align_expr);
        visitor.accept_child(tree, self.addrspace_expr);
        visitor.accept_child(tree, self.section_expr);
        visitor.accept_child(tree, self.callconv_expr);
    }
}

impl node::Asm {
    pub fn accept_children(&self, tree: &Ast, visitor: &mut impl Visitor) {
        visitor.accept_extra_children(tree, self.items_start..self.items_end);
        visitor.accept_token(tree, self.rparen);
    }
}

macro_rules! extra_data_all_nodes {
    ($type:ident) => {
        impl node::$type {
            pub fn accept_children(&self, tree: &Ast, visitor: &mut impl Visitor) {
                self.accept_all_fields_as_children(tree, visitor);
            }
        }
    };
}

macro_rules! extra_data_impl {
    ($type:ident, $($field:ident),*) => {
        impl crate::ast::node::$type {
            pub(crate) fn accept_all_fields_as_children(&self, tree: &crate::Ast, visitor: &mut impl Visitor) {
                $(visitor.accept_child(tree, self.$field);)*
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
            fn accept_children(
                &self,
                tree: &crate::Ast,
                start: crate::ast::node::Index,
                visitor: &mut impl Visitor,
            ) {
                match self {
                    $(
                        ExtraDataType::$type => {
                            let extra: node::$type = tree.extra_data(start);
                            extra.accept_children(tree, visitor);
                        }
                    )*
                }
            }
        }
    };
}

enum PairType {
    SubList,
    Pair(DataType, DataType),
    For,
}

enum DataType {
    Unused,
    Node,
    Token,
    NodeSlice,
    Extra(ExtraDataType),
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
    FnProtoOne,
    FnProto,
    Asm,
}

extra_data_all_nodes!(LocalVarDecl);
extra_data_all_nodes!(ArrayTypeSentinel);
extra_data_all_nodes!(PtrType);
extra_data_all_nodes!(PtrTypeBitRange);
extra_data_all_nodes!(If);
extra_data_all_nodes!(ContainerField);
extra_data_all_nodes!(GlobalVarDecl);
extra_data_all_nodes!(Slice);
extra_data_all_nodes!(SliceSentinel);
extra_data_all_nodes!(While);
extra_data_all_nodes!(WhileCont);
extra_data_all_nodes!(FnProtoOne);

fn get_data_types(mode: Mode, tag: node::Tag) -> PairType {
    use crate::macros::node;

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
        node!(PtrTypeBitRange) => data!(extra!(PtrTypeBitRange), Node),
        node!(SliceOpen) => data!(Node, Node),
        node!(Slice) => data!(Node, extra!(Slice)),
        node!(SliceSentinel) => data!(Node, extra!(SliceSentinel)),
        node!(Deref) => data!(Node, Unused),
        node!(ArrayAccess) => data!(Node, Node),
        node!(ArrayInitOne) => data!(Node, Node),
        node!(ArrayInitOneComma) => data!(Node, Node),
        node!(ArrayInitDotTwo) => data!(Node, Node),
        node!(ArrayInitDotTwoComma) => data!(Node, Node),
        node!(ArrayInitDot) => data!(SubList),
        node!(ArrayInitDotComma) => data!(SubList),
        node!(ArrayInit) => data!(SubList),
        node!(ArrayInitComma) => data!(SubList),
        node!(StructInitOne) => data!(Node, Node),
        node!(StructInitOneComma) => data!(Node, Node),
        node!(StructInitDotTwo) => data!(Node, Node),
        node!(StructInitDotTwoComma) => data!(Node, Node),
        node!(StructInitDot) => data!(SubList),
        node!(StructInitDotComma) => data!(SubList),
        node!(StructInit) => data!(SubList),
        node!(StructInitComma) => data!(SubList),
        node!(CallOne) => data!(Node, Node),
        node!(CallOneComma) => data!(Node, Node),
        node!(AsyncCallOne) => data!(Node, Node),
        node!(AsyncCallOneComma) => data!(Node, Node),
        node!(Call) => data!(Node, extra!(SubRange)),
        node!(CallComma) => data!(Node, extra!(SubRange)),
        node!(AsyncCall) => data!(Node, extra!(SubRange)),
        node!(AsyncCallComma) => data!(Node, extra!(SubRange)),
        node!(Switch) => data!(Node, extra!(SubRange)),
        node!(SwitchComma) => data!(Node, extra!(SubRange)),
        node!(SwitchCaseOne) => data!(Node, Node),
        node!(SwitchCaseInlineOne) => data!(Node, Node),
        node!(SwitchCase) => data!(extra!(SubRange), Node),
        node!(SwitchCaseInline) => data!(extra!(SubRange), Node),
        node!(SwitchRange) => data!(Node, Node),
        node!(WhileSimple) => data!(Node, Node),
        node!(WhileCont) => data!(Node, extra!(WhileCont)),
        node!(While) => data!(Node, extra!(While)),
        node!(ForSimple) => data!(Node, Node),
        node!(For) => data!(For),
        node!(ForRange) => data!(Node, Node),
        node!(IfSimple) => data!(Node, Node),
        node!(If) => data!(Node, extra!(If)),
        node!(Suspend) => data!(Node, Unused),
        node!(Resume) => data!(Node, Unused),
        node!(Continue) => data!(Token, Unused),
        node!(Break) => data!(Token, Node),
        node!(Return) => data!(Node, Unused),
        node!(FnProtoSimple) => data!(Node, Node),
        node!(FnProtoMulti) => data!(extra!(SubRange), Node),
        node!(FnProtoOne) => data!(extra!(FnProtoOne), Node),
        node!(FnProto) => data!(extra!(FnProto), Node),
        node!(FnDecl) => data!(Node, Node),
        node!(AnyframeType) => data!(Token, Node),
        node!(AnyframeLiteral) => data!(Unused, Unused),
        node!(CharLiteral) => data!(Unused, Unused),
        node!(NumberLiteral) => data!(Unused, Unused),
        node!(UnreachableLiteral) => data!(Unused, Unused),
        node!(Identifier) => data!(Unused, Unused),
        node!(EnumLiteral) => data!(Token, Unused),
        node!(StringLiteral) => data!(Unused, Unused),
        node!(MultilineStringLiteral) => data!(Token, Token),
        node!(GroupedExpression) => data!(Node, Token),
        node!(BuiltinCallTwo) => data!(Node, Node),
        node!(BuiltinCallTwoComma) => data!(Node, Node),
        node!(BuiltinCall) => data!(SubList),
        node!(BuiltinCallComma) => data!(SubList),
        node!(ErrorSetDecl) => data!(Unused, Token),
        node!(ContainerDecl) => data!(SubList),
        node!(ContainerDeclTrailing) => data!(SubList),
        node!(ContainerDeclTwo) => data!(Node, Node),
        node!(ContainerDeclTwoTrailing) => data!(Node, Node),
        node!(ContainerDeclArg) => data!(Node, extra!(SubRange)),
        node!(ContainerDeclArgTrailing) => data!(Node, extra!(SubRange)),
        node!(TaggedUnion) => data!(SubList),
        node!(TaggedUnionTrailing) => data!(SubList),
        node!(TaggedUnionTwo) => data!(Node, Node),
        node!(TaggedUnionTwoTrailing) => data!(Node, Node),
        node!(TaggedUnionEnumTag) => data!(Node, extra!(SubRange)),
        node!(TaggedUnionEnumTagTrailing) => data!(Node, extra!(SubRange)),
        node!(ContainerFieldInit) => data!(Node, Node),
        node!(ContainerFieldAlign) => data!(Node, Node),
        node!(ContainerField) => data!(Node, Node),
        node!(Comptime) => data!(Node, Unused),
        node!(Nosuspend) => data!(Node, Unused),
        node!(BlockTwo) => data!(Node, Node),
        node!(BlockTwoSemicolon) => data!(Node, Node),
        node!(Block) => data!(SubList),
        node!(BlockSemicolon) => data!(SubList),
        node!(AsmSimple) => data!(Node, Token),
        node!(Asm) => data!(Node, extra!(Asm)),
        node!(AsmOutput) => data!(Node, Token),
        node!(AsmInput) => data!(Node, Token),
        node!(ErrorValue) => data!(Token, Token),
        node!(ErrorUnion) => data!(Node, Node),
    }
}
