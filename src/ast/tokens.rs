use super::*;

impl<'src> Ast<'src> {
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

    pub fn token_slice(&self, token_index: TokenIndex) -> &'src [u8] {
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
        debug_assert!(token.tag == self.token_tag(token_index));
        &self.source[token.loc.start..token.loc.end]
    }

    pub fn first_token(&self, node: node::Index) -> TokenIndex {
        let mut end_offset: TokenIndex = 0;
        let mut index = node;
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
                    debug_assert!(lhs_len > 0);
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
                    debug_assert!(extra.end - extra.start > 0);
                    self.extra_data(extra.start)
                }
                N::SwitchCaseInline => {
                    let extra: node::SubRange = self.extra_data(n.data.lhs);
                    debug_assert!(extra.end - extra.start > 0);
                    return self.first_token(self.extra_data(extra.start)) - 1;
                }

                N::AsmOutput | N::AsmInput => {
                    debug_assert_eq!(self.token_tag(n.main_token - 1), T::LBracket);
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

    pub fn last_token(&self, node: node::Index) -> TokenIndex {
        let mut index = node;
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
                    end_offset += 1; // for the rparen
                    let params: node::SubRange = self.extra_data(n.data.rhs);
                    if params.end - params.start == 0 {
                        return n.main_token + end_offset;
                    }
                    self.extra_data(params.end - 1) // last parameter
                }
                N::TaggedUnionEnumTag => {
                    let members: node::SubRange = self.extra_data(n.data.rhs);
                    if members.end - members.start == 0 {
                        end_offset += 4; // for the rparen + rparen + lbrace + rbrace
                        n.data.lhs
                    } else {
                        end_offset += 1; // for the rbrace
                        self.extra_data(members.end - 1) // last parameter
                    }
                }
                N::CallComma | N::AsyncCallComma | N::TaggedUnionEnumTagTrailing => {
                    end_offset += 2; // for the comma/semicolon + rparen/rbrace
                    let params: node::SubRange = self.extra_data(n.data.rhs);
                    debug_assert!(params.end > params.start);
                    self.extra_data(params.end - 1) // last parameter
                }
                N::Switch => {
                    let cases: node::SubRange = self.extra_data(n.data.rhs);
                    if cases.end - cases.start == 0 {
                        end_offset += 3; // rparen, lbrace, rbrace
                        n.data.lhs // condition expression
                    } else {
                        end_offset += 1; // for the rbrace
                        self.extra_data(cases.end - 1) // last case
                    }
                }
                N::ContainerDeclArg => {
                    let members: node::SubRange = self.extra_data(n.data.rhs);
                    if members.end - members.start == 0 {
                        end_offset += 3; // for the rparen + lbrace + rbrace
                        n.data.lhs
                    } else {
                        end_offset += 1; // for the rbrace
                        self.extra_data(members.end - 1) // last parameter
                    }
                }
                N::Asm => {
                    let extra: node::Asm = self.extra_data(n.data.rhs);
                    return extra.rparen + end_offset;
                }
                N::ArrayInit | N::StructInit => {
                    let elements: node::SubRange = self.extra_data(n.data.rhs);
                    debug_assert!(elements.end - elements.start > 0);
                    end_offset += 1; // for the rbrace
                    self.extra_data(elements.end - 1) // last element
                }
                N::ArrayInitComma
                | N::StructInitComma
                | N::ContainerDeclArgTrailing
                | N::SwitchComma => {
                    let members: node::SubRange = self.extra_data(n.data.rhs);
                    debug_assert!(members.end - members.start > 0);
                    end_offset += 2; // for the comma + rbrace
                    self.extra_data(members.end - 1) // last parameter
                }
                N::ArrayInitDot
                | N::StructInitDot
                | N::Block
                | N::ContainerDecl
                | N::TaggedUnion
                | N::BuiltinCall => {
                    debug_assert!(n.data.rhs - n.data.lhs > 0);
                    end_offset += 1; // for the rbrace
                    self.extra_data(n.data.rhs - 1) // last statement
                }
                N::ArrayInitDotComma
                | N::StructInitDotComma
                | N::BlockSemicolon
                | N::ContainerDeclTrailing
                | N::TaggedUnionTrailing
                | N::BuiltinCallComma => {
                    debug_assert!(n.data.rhs - n.data.lhs > 0);
                    end_offset += 2; // for the comma/semicolon + rbrace/rparen
                    self.extra_data(n.data.rhs - 1) // last member
                }
                N::CallOne | N::AsyncCallOne | N::ArrayAccess => {
                    end_offset += 1; // for the rparen/rbracket
                    if n.data.rhs == 0 {
                        return n.main_token + end_offset;
                    }
                    n.data.rhs
                }
                N::ArrayInitDotTwo
                | N::BlockTwo
                | N::BuiltinCallTwo
                | N::StructInitDotTwo
                | N::ContainerDeclTwo
                | N::TaggedUnionTwo => {
                    if n.data.rhs != 0 {
                        end_offset += 1; // for the rparen/rbrace
                        n.data.rhs
                    } else if n.data.lhs != 0 {
                        end_offset += 1; // for the rparen/rbrace
                        n.data.lhs
                    } else {
                        match n.tag {
                            N::ArrayInitDotTwo | N::BlockTwo | N::StructInitDotTwo => {
                                end_offset += 1 // rbrace
                            }
                            N::BuiltinCallTwo => end_offset += 2, // lparen/lbrace + rparen/rbrace
                            N::ContainerDeclTwo => {
                                let mut i: u32 = 2; // lbrace + rbrace
                                while self.token_tag(n.main_token + i) == T::ContainerDocComment {
                                    i += 1;
                                }
                                end_offset += i;
                            }
                            N::TaggedUnionTwo => {
                                let mut i: u32 = 5; // (enum) {}
                                while self.token_tag(n.main_token + i) == T::ContainerDocComment {
                                    i += 1;
                                }
                                end_offset += i;
                            }
                            _ => unreachable!(),
                        }
                        return n.main_token + end_offset;
                    }
                }
                N::ArrayInitDotTwoComma
                | N::BuiltinCallTwoComma
                | N::BlockTwoSemicolon
                | N::StructInitDotTwoComma
                | N::ContainerDeclTwoTrailing
                | N::TaggedUnionTwoTrailing => {
                    end_offset += 2; // for the comma/semicolon + rbrace/rparen
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        unreachable!();
                    }
                }
                N::SimpleVarDecl => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        end_offset += 1; // from mut token to name
                        return n.main_token + end_offset;
                    }
                }
                N::AlignedVarDecl => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else if n.data.lhs != 0 {
                        end_offset += 1; // for the rparen
                        n.data.lhs
                    } else {
                        end_offset += 1; // from mut token to name
                        return n.main_token + end_offset;
                    }
                }
                N::GlobalVarDecl => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else {
                        let extra: node::GlobalVarDecl = self.extra_data(n.data.lhs);
                        if extra.section_node != 0 {
                            end_offset += 1; // for the rparen
                            extra.section_node
                        } else if extra.align_node != 0 {
                            end_offset += 1; // for the rparen
                            extra.align_node
                        } else if extra.type_node != 0 {
                            extra.type_node
                        } else {
                            end_offset += 1; // from mut token to name
                            return n.main_token + end_offset;
                        }
                    }
                }
                N::LocalVarDecl => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else {
                        let extra: node::LocalVarDecl = self.extra_data(n.data.lhs);
                        if extra.align_node != 0 {
                            end_offset += 1; // for the rparen
                            extra.align_node
                        } else if extra.type_node != 0 {
                            extra.type_node
                        } else {
                            end_offset += 1; // from mut token to name
                            return n.main_token + end_offset;
                        }
                    }
                }
                N::ContainerFieldInit => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }
                N::ContainerFieldAlign => {
                    if n.data.rhs != 0 {
                        end_offset += 1; // for the rparen
                        n.data.rhs
                    } else if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }
                N::ContainerField => {
                    let extra: node::ContainerField = self.extra_data(n.data.rhs);
                    if extra.value_expr != 0 {
                        extra.value_expr
                    } else if extra.align_expr != 0 {
                        end_offset += 1; // for the rparen
                        extra.align_expr
                    } else if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }
                N::ArrayInitOne | N::StructInitOne => {
                    end_offset += 1; // rbrace
                    if n.data.rhs == 0 {
                        return n.main_token + end_offset;
                    } else {
                        n.data.rhs
                    }
                }
                N::SliceOpen
                | N::CallOneComma
                | N::AsyncCallOneComma
                | N::ArrayInitOneComma
                | N::StructInitOneComma => {
                    end_offset += 2; // ellipsis2 + rbracket, or comma + rparen
                    debug_assert_ne!(n.data.rhs, 0);
                    n.data.rhs
                }
                N::Slice => {
                    let extra: node::Slice = self.extra_data(n.data.rhs);
                    debug_assert_ne!(extra.end, 0); // should have used slice_open
                    end_offset += 1; // rbracket
                    extra.end
                }
                N::SliceSentinel => {
                    let extra: node::SliceSentinel = self.extra_data(n.data.rhs);
                    debug_assert_ne!(extra.sentinel, 0); // should have used slice
                    end_offset += 1; // rbracket
                    extra.sentinel
                }

                N::Continue => {
                    if n.data.lhs != 0 {
                        return n.data.lhs + end_offset;
                    } else {
                        return n.main_token + end_offset;
                    }
                }
                N::Break => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else if n.data.lhs != 0 {
                        return n.data.lhs + end_offset;
                    } else {
                        return n.main_token + end_offset;
                    }
                }
                N::FnDecl => {
                    if n.data.rhs != 0 {
                        n.data.rhs
                    } else {
                        n.data.lhs
                    }
                }
                N::FnProtoOne => {
                    let extra: node::FnProtoOne = self.extra_data(n.data.lhs);
                    // addrspace, linksection, callconv, align can appear in any order, so we
                    // find the last one here.
                    let mut max_node: node::Index = n.data.rhs;
                    let mut max_start = self.token_start(self.node(max_node).main_token);
                    let mut max_offset: TokenIndex = 0;
                    if extra.align_expr != 0 {
                        let start = self.token_start(self.node(extra.align_expr).main_token);
                        if start > max_start {
                            max_node = extra.align_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    if extra.addrspace_expr != 0 {
                        let start = self.token_start(self.node(extra.addrspace_expr).main_token);
                        if start > max_start {
                            max_node = extra.addrspace_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    if extra.section_expr != 0 {
                        let start = self.token_start(self.node(extra.section_expr).main_token);
                        if start > max_start {
                            max_node = extra.section_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    if extra.callconv_expr != 0 {
                        let start = self.token_start(self.node(extra.callconv_expr).main_token);
                        if start > max_start {
                            max_node = extra.callconv_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    end_offset += max_offset;
                    max_node
                }
                N::FnProto => {
                    let extra: node::FnProto = self.extra_data(n.data.lhs);
                    // addrspace, linksection, callconv, align can appear in any order, so we
                    // find the last one here.
                    let mut max_node: node::Index = n.data.rhs;
                    let mut max_start = self.token_start(self.node(max_node).main_token);
                    let mut max_offset: TokenIndex = 0;
                    if extra.align_expr != 0 {
                        let start = self.token_start(self.node(extra.align_expr).main_token);
                        if start > max_start {
                            max_node = extra.align_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    if extra.addrspace_expr != 0 {
                        let start = self.token_start(self.node(extra.addrspace_expr).main_token);
                        if start > max_start {
                            max_node = extra.addrspace_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    if extra.section_expr != 0 {
                        let start = self.token_start(self.node(extra.section_expr).main_token);
                        if start > max_start {
                            max_node = extra.section_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    if extra.callconv_expr != 0 {
                        let start = self.token_start(self.node(extra.callconv_expr).main_token);
                        if start > max_start {
                            max_node = extra.callconv_expr;
                            max_start = start;
                            max_offset = 1; // for the rparen
                        }
                    }
                    end_offset += max_offset;
                    max_node
                }
                N::WhileCont => {
                    let extra: node::WhileCont = self.extra_data(n.data.rhs);
                    debug_assert_ne!(extra.then_expr, 0);
                    extra.then_expr
                }
                N::While => {
                    let extra: node::While = self.extra_data(n.data.rhs);
                    debug_assert_ne!(extra.else_expr, 0);
                    extra.else_expr
                }
                N::If => {
                    let extra: node::If = self.extra_data(n.data.rhs);
                    debug_assert_ne!(extra.else_expr, 0);
                    extra.else_expr
                }
                N::For => {
                    // let extra: node::For = self.as_node_for(self.bit_cast_node(n.data.rhs));
                    // self.extra_data(n.data.lhs + extra.inputs + self.int_from_bool(extra.has_else))
                    todo!()
                }
                N::Suspend => {
                    if n.data.lhs != 0 {
                        n.data.lhs
                    } else {
                        return n.main_token + end_offset;
                    }
                }
                N::ArrayTypeSentinel => {
                    let extra: node::ArrayTypeSentinel = self.extra_data(n.data.rhs);
                    extra.elem_type
                }
            }
        }
    }

    pub fn tokens_on_same_line(&mut self, token1: TokenIndex, token2: TokenIndex) -> bool {
        let source = self.source(self.token_start(token1)..self.token_start(token2));
        source.iter().all(|&b| b != b'\n')
    }
}
