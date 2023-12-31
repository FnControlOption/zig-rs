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
                            let count: u32 = tree.extra_data(data);
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
                let inputs = extra.inputs();
                let has_else = extra.has_else();
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
        N::Root => match mode {
            Mode::Zig => data!(SubList),
            Mode::Zon => data!(Node, Unused),
        },
        N::Usingnamespace => data!(Node, Unused),
        N::TestDecl => data!(Token, Node),
        N::GlobalVarDecl => data!(extra!(GlobalVarDecl), Node),
        N::LocalVarDecl => data!(extra!(LocalVarDecl), Node),
        N::SimpleVarDecl => data!(Node, Node),
        N::AlignedVarDecl => data!(Node, Node),
        N::Errdefer => data!(Token, Node),
        N::Defer => data!(Unused, Node),
        N::Catch => data!(Node, Node),
        N::FieldAccess => data!(Node, Token),
        N::UnwrapOptional => data!(Node, Token),
        N::EqualEqual => data!(Node, Node),
        N::BangEqual => data!(Node, Node),
        N::LessThan => data!(Node, Node),
        N::GreaterThan => data!(Node, Node),
        N::LessOrEqual => data!(Node, Node),
        N::GreaterOrEqual => data!(Node, Node),
        N::AssignMul => data!(Node, Node),
        N::AssignDiv => data!(Node, Node),
        N::AssignMod => data!(Node, Node),
        N::AssignAdd => data!(Node, Node),
        N::AssignSub => data!(Node, Node),
        N::AssignShl => data!(Node, Node),
        N::AssignShlSat => data!(Node, Node),
        N::AssignShr => data!(Node, Node),
        N::AssignBitAnd => data!(Node, Node),
        N::AssignBitXor => data!(Node, Node),
        N::AssignBitOr => data!(Node, Node),
        N::AssignMulWrap => data!(Node, Node),
        N::AssignAddWrap => data!(Node, Node),
        N::AssignSubWrap => data!(Node, Node),
        N::AssignMulSat => data!(Node, Node),
        N::AssignAddSat => data!(Node, Node),
        N::AssignSubSat => data!(Node, Node),
        N::Assign => data!(Node, Node),
        N::AssignDestructure => data!(NodeSlice, Node),
        N::MergeErrorSets => data!(Node, Node),
        N::Mul => data!(Node, Node),
        N::Div => data!(Node, Node),
        N::Mod => data!(Node, Node),
        N::ArrayMult => data!(Node, Node),
        N::MulWrap => data!(Node, Node),
        N::MulSat => data!(Node, Node),
        N::Add => data!(Node, Node),
        N::Sub => data!(Node, Node),
        N::ArrayCat => data!(Node, Node),
        N::AddWrap => data!(Node, Node),
        N::SubWrap => data!(Node, Node),
        N::AddSat => data!(Node, Node),
        N::SubSat => data!(Node, Node),
        N::Shl => data!(Node, Node),
        N::ShlSat => data!(Node, Node),
        N::Shr => data!(Node, Node),
        N::BitAnd => data!(Node, Node),
        N::BitXor => data!(Node, Node),
        N::BitOr => data!(Node, Node),
        N::Orelse => data!(Node, Node),
        N::BoolAnd => data!(Node, Node),
        N::BoolOr => data!(Node, Node),
        N::BoolNot => data!(Node, Unused),
        N::Negation => data!(Node, Unused),
        N::BitNot => data!(Node, Unused),
        N::NegationWrap => data!(Node, Unused),
        N::AddressOf => data!(Node, Unused),
        N::Try => data!(Node, Unused),
        N::Await => data!(Node, Unused),
        N::OptionalType => data!(Node, Unused),
        N::ArrayType => data!(Node, Node),
        N::ArrayTypeSentinel => data!(Node, extra!(ArrayTypeSentinel)),
        N::PtrTypeAligned => data!(Node, Node),
        N::PtrTypeSentinel => data!(Node, Node),
        N::PtrType => data!(extra!(PtrType), Node),
        N::PtrTypeBitRange => data!(extra!(PtrTypeBitRange), Node),
        N::SliceOpen => data!(Node, Node),
        N::Slice => data!(Node, extra!(Slice)),
        N::SliceSentinel => data!(Node, extra!(SliceSentinel)),
        N::Deref => data!(Node, Unused),
        N::ArrayAccess => data!(Node, Node),
        N::ArrayInitOne => data!(Node, Node),
        N::ArrayInitOneComma => data!(Node, Node),
        N::ArrayInitDotTwo => data!(Node, Node),
        N::ArrayInitDotTwoComma => data!(Node, Node),
        N::ArrayInitDot => data!(SubList),
        N::ArrayInitDotComma => data!(SubList),
        N::ArrayInit => data!(SubList),
        N::ArrayInitComma => data!(SubList),
        N::StructInitOne => data!(Node, Node),
        N::StructInitOneComma => data!(Node, Node),
        N::StructInitDotTwo => data!(Node, Node),
        N::StructInitDotTwoComma => data!(Node, Node),
        N::StructInitDot => data!(SubList),
        N::StructInitDotComma => data!(SubList),
        N::StructInit => data!(SubList),
        N::StructInitComma => data!(SubList),
        N::CallOne => data!(Node, Node),
        N::CallOneComma => data!(Node, Node),
        N::AsyncCallOne => data!(Node, Node),
        N::AsyncCallOneComma => data!(Node, Node),
        N::Call => data!(Node, extra!(SubRange)),
        N::CallComma => data!(Node, extra!(SubRange)),
        N::AsyncCall => data!(Node, extra!(SubRange)),
        N::AsyncCallComma => data!(Node, extra!(SubRange)),
        N::Switch => data!(Node, extra!(SubRange)),
        N::SwitchComma => data!(Node, extra!(SubRange)),
        N::SwitchCaseOne => data!(Node, Node),
        N::SwitchCaseInlineOne => data!(Node, Node),
        N::SwitchCase => data!(extra!(SubRange), Node),
        N::SwitchCaseInline => data!(extra!(SubRange), Node),
        N::SwitchRange => data!(Node, Node),
        N::WhileSimple => data!(Node, Node),
        N::WhileCont => data!(Node, extra!(WhileCont)),
        N::While => data!(Node, extra!(While)),
        N::ForSimple => data!(Node, Node),
        N::For => data!(For),
        N::ForRange => data!(Node, Node),
        N::IfSimple => data!(Node, Node),
        N::If => data!(Node, extra!(If)),
        N::Suspend => data!(Node, Unused),
        N::Resume => data!(Node, Unused),
        N::Continue => data!(Token, Unused),
        N::Break => data!(Token, Node),
        N::Return => data!(Node, Unused),
        N::FnProtoSimple => data!(Node, Node),
        N::FnProtoMulti => data!(extra!(SubRange), Node),
        N::FnProtoOne => data!(extra!(FnProtoOne), Node),
        N::FnProto => data!(extra!(FnProto), Node),
        N::FnDecl => data!(Node, Node),
        N::AnyframeType => data!(Token, Node),
        N::AnyframeLiteral => data!(Unused, Unused),
        N::CharLiteral => data!(Unused, Unused),
        N::NumberLiteral => data!(Unused, Unused),
        N::UnreachableLiteral => data!(Unused, Unused),
        N::Identifier => data!(Unused, Unused),
        N::EnumLiteral => data!(Token, Unused),
        N::StringLiteral => data!(Unused, Unused),
        N::MultilineStringLiteral => data!(Token, Token),
        N::GroupedExpression => data!(Node, Token),
        N::BuiltinCallTwo => data!(Node, Node),
        N::BuiltinCallTwoComma => data!(Node, Node),
        N::BuiltinCall => data!(SubList),
        N::BuiltinCallComma => data!(SubList),
        N::ErrorSetDecl => data!(Unused, Token),
        N::ContainerDecl => data!(SubList),
        N::ContainerDeclTrailing => data!(SubList),
        N::ContainerDeclTwo => data!(Node, Node),
        N::ContainerDeclTwoTrailing => data!(Node, Node),
        N::ContainerDeclArg => data!(Node, extra!(SubRange)),
        N::ContainerDeclArgTrailing => data!(Node, extra!(SubRange)),
        N::TaggedUnion => data!(SubList),
        N::TaggedUnionTrailing => data!(SubList),
        N::TaggedUnionTwo => data!(Node, Node),
        N::TaggedUnionTwoTrailing => data!(Node, Node),
        N::TaggedUnionEnumTag => data!(Node, extra!(SubRange)),
        N::TaggedUnionEnumTagTrailing => data!(Node, extra!(SubRange)),
        N::ContainerFieldInit => data!(Node, Node),
        N::ContainerFieldAlign => data!(Node, Node),
        N::ContainerField => data!(Node, Node),
        N::Comptime => data!(Node, Unused),
        N::Nosuspend => data!(Node, Unused),
        N::BlockTwo => data!(Node, Node),
        N::BlockTwoSemicolon => data!(Node, Node),
        N::Block => data!(SubList),
        N::BlockSemicolon => data!(SubList),
        N::AsmSimple => data!(Node, Token),
        N::Asm => data!(Node, extra!(Asm)),
        N::AsmOutput => data!(Node, Token),
        N::AsmInput => data!(Node, Token),
        N::ErrorValue => data!(Token, Token),
        N::ErrorUnion => data!(Node, Node),
    }
}
