use super::node::ExtraData;
use super::*;

use crate::macros::node;

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

        let pair_type = match node_tag {
            node!(Root) => match self.mode {
                Mode::Zig => data!(SubList),
                Mode::Zon => data!(Node, Unused),
            },
            node!(Usingnamespace) => data!(Node, Unused),
            node!(TestDecl) => data!(Token, Node),
            // node!(GlobalVarDecl) => data!(Unknown, Unknown),
            // node!(LocalVarDecl) => data!(Unknown, Unknown),
            node!(SimpleVarDecl) => data!(Node, Node),
            // node!(AlignedVarDecl) => data!(Node, Node),
            // node!(Errdefer) => data!(Token, Node),
            node!(Defer) => data!(Unused, Node),
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
            // node!(AssignDestructure) => data!(Unknown, Unknown),
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
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(SliceOpen) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(Deref) => data!(Node, Unused),
            node!(ArrayAccess) => data!(Node, Node),
            node!(ArrayInitOne) => data!(Node, Node),
            node!(ArrayInitOneComma) => data!(Node, Node),
            node!(ArrayInitDotTwo) => data!(Node, Node),
            node!(ArrayInitDotTwoComma) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(StructInitOne) => data!(Node, Node),
            node!(StructInitOneComma) => data!(Node, Node),
            node!(StructInitDotTwo) => data!(Node, Node),
            node!(StructInitDotTwoComma) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(CallOne) => data!(Node, Node),
            node!(CallOneComma) => data!(Node, Node),
            node!(AsyncCallOne) => data!(Node, Node),
            node!(AsyncCallOneComma) => data!(Node, Node),
            node!(Call) => data!(Node, SubRange),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(SwitchCaseOne) => data!(Node, Node),
            node!(SwitchCaseInlineOne) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(SwitchRange) => data!(Node, Node),
            node!(WhileSimple) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(ForSimple) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            node!(ForRange) => data!(Node, Node),
            node!(IfSimple) => data!(Node, Node),
            node!(Suspend) => data!(Node, Unused),
            node!(Resume) => data!(Node, Unused),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(Return) => data!(Node, Unused),
            node!(FnProtoSimple) => data!(Node, Node),
            node!(FnProtoMulti) => data!(SubRange, Node),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(FnDecl) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            node!(AnyframeLiteral) => data!(Unused, Unused),
            node!(CharLiteral) => data!(Unused, Unused),
            node!(NumberLiteral) => data!(Unused, Unused),
            node!(UnreachableLiteral) => data!(Unused, Unused),
            node!(Identifier) => data!(Unused, Unused),
            node!(StringLiteral) => data!(Unused, Unused),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(BuiltinCallTwo) => data!(Node, Node),
            node!(BuiltinCallTwoComma) => data!(Node, Node),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(ErrorSetDecl) => data!(Unused, Token),
            // node!() => data!(Unknown, Unknown),
            // node!() => data!(Unknown, Unknown),
            node!(ContainerDeclTwo) => data!(Node, Node),
            node!(ContainerDeclTwoTrailing) => data!(Node, Node),
            // node!(ContainerDeclArg) => data!(Unknown, Unknown),
            // node!(ContainerDeclArgTrailing) => data!(Unknown, Unknown),
            node!(TaggedUnion) => data!(SubList),
            node!(TaggedUnionTrailing) => data!(SubList),
            node!(TaggedUnionTwo) => data!(Node, Node),
            node!(TaggedUnionTwoTrailing) => data!(Node, Node),
            // node!(TaggedUnionEnumTag) => data!(Unknown, Unknown),
            // node!(TaggedUnionEnumTagTrailing) => data!(Unknown, Unknown),
            node!(ContainerFieldInit) => data!(Node, Node),
            // node!(ContainerFieldAlign) => data!(Unknown, Unknown),
            // node!(ContainerField) => data!(Unknown, Unknown),
            node!(Comptime) => data!(Node, Unused),
            node!(Nosuspend) => data!(Node, Unused),
            node!(BlockTwo) => data!(Node, Node),
            node!(BlockTwoSemicolon) => data!(Node, Node),
            node!(Block) => data!(SubList),
            node!(BlockSemicolon) => data!(SubList),
            // node!(AsmSimple) => data!(Unknown, Unknown),
            // node!(Asm) => data!(Unknown, Unknown),
            // node!(AsmOutput) => data!(Unknown, Unknown),
            // node!(AsmInput) => data!(Unknown, Unknown),
            node!(ErrorValue) => data!(Token, Token),
            node!(ErrorUnion) => data!(Node, Node),
            _ => data!(Unknown, Unknown),
        };

        let (lhs_type, rhs_type) = match pair_type {
            PairType::SubList => {
                writeln!(f, "{indent}extra[{lhs}..{rhs}]")?;
                for extra_index in lhs..rhs {
                    let node_index = self.extra_data(extra_index);
                    writeln!(f, "{indent}extra[{extra_index}] -> node[{node_index}]")?;
                    self.dump(f, self.node(node_index), depth + 1)?;
                }
                return Ok(());
            }
            PairType::Pair(l, r) => (l, r),
        };

        for (name, data, data_type) in [("lhs", lhs, lhs_type), ("rhs", rhs, rhs_type)] {
            match data_type {
                DataType::Unknown => writeln!(f, "{indent}{name}: ? / {data}")?,
                DataType::Unused => writeln!(f, "{indent}{name}: unused / {data}")?,
                DataType::Node => {
                    if data == 0 {
                        writeln!(f, "{indent}{name}: omitted / {data}")?;
                    } else {
                        writeln!(f, "{indent}{name}: node[{data}]")?;
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
                    let range = node::SubRange::field_range(data);
                    let node::SubRange { start, end } = node::SubRange::from_start(self, data);
                    writeln!(
                        f,
                        "{indent}{name}: extra[{range:?}] -> extra[{start}..{end}]"
                    )?;
                    for extra_index in start..end {
                        let node_index = self.extra_data(extra_index);
                        writeln!(f, "{indent}extra[{extra_index}] -> node[{node_index}]")?;
                        self.dump(f, self.node(node_index), depth + 1)?;
                    }
                }
            }
        }

        Ok(())
    }
}
