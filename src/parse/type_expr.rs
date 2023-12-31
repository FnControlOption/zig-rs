use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_type_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            T::QuestionMark => {
                let main_token = self.next_token();
                let lhs = self.expect_type_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::OptionalType,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::KeywordAnyframe => match self.token_tag(self.tok_i + 1) {
                T::Arrow => {
                    let main_token = self.next_token();
                    let lhs = self.next_token();
                    let rhs = self.expect_type_expr()?;
                    Ok(self.add_node(Node {
                        tag: N::AnyframeType,
                        main_token,
                        data: node::Data { lhs, rhs },
                    }))
                }
                _ => self.parse_error_union_expr(),
            },
            T::Asterisk => {
                let asterisk = self.next_token();
                let mods = self.parse_ptr_modifiers()?;
                let elem_type = self.expect_type_expr()?;
                if mods.bit_range_start != 0 {
                    let lhs = self.add_extra(node::PtrTypeBitRange {
                        sentinel: 0,
                        align_node: mods.align_node,
                        addrspace_node: mods.addrspace_node,
                        bit_range_start: mods.bit_range_start,
                        bit_range_end: mods.bit_range_end,
                    });
                    Ok(self.add_node(Node {
                        tag: N::PtrTypeBitRange,
                        main_token: asterisk,
                        data: node::Data {
                            lhs,
                            rhs: elem_type,
                        },
                    }))
                } else if mods.addrspace_node != 0 {
                    let lhs = self.add_extra(node::PtrType {
                        sentinel: 0,
                        align_node: mods.align_node,
                        addrspace_node: mods.addrspace_node,
                    });
                    Ok(self.add_node(Node {
                        tag: N::PtrType,
                        main_token: asterisk,
                        data: node::Data {
                            lhs,
                            rhs: elem_type,
                        },
                    }))
                } else {
                    Ok(self.add_node(Node {
                        tag: N::PtrTypeAligned,
                        main_token: asterisk,
                        data: node::Data {
                            lhs: mods.align_node,
                            rhs: elem_type,
                        },
                    }))
                }
            }
            T::AsteriskAsterisk => {
                let asterisk = self.next_token();
                let mods = self.parse_ptr_modifiers()?;
                let elem_type = self.expect_type_expr()?;
                let inner = {
                    if mods.bit_range_start != 0 {
                        let lhs = self.add_extra(node::PtrTypeBitRange {
                            sentinel: 0,
                            align_node: mods.align_node,
                            addrspace_node: mods.addrspace_node,
                            bit_range_start: mods.bit_range_start,
                            bit_range_end: mods.bit_range_end,
                        });
                        self.add_node(Node {
                            tag: N::PtrTypeBitRange,
                            main_token: asterisk,
                            data: node::Data {
                                lhs,
                                rhs: elem_type,
                            },
                        })
                    } else if mods.addrspace_node != 0 {
                        let lhs = self.add_extra(node::PtrType {
                            sentinel: 0,
                            align_node: mods.align_node,
                            addrspace_node: mods.addrspace_node,
                        });
                        self.add_node(Node {
                            tag: N::PtrType,
                            main_token: asterisk,
                            data: node::Data {
                                lhs,
                                rhs: elem_type,
                            },
                        })
                    } else {
                        self.add_node(Node {
                            tag: N::PtrTypeAligned,
                            main_token: asterisk,
                            data: node::Data {
                                lhs: mods.align_node,
                                rhs: elem_type,
                            },
                        })
                    }
                };
                Ok(self.add_node(Node {
                    tag: N::PtrTypeAligned,
                    main_token: asterisk,
                    data: node::Data { lhs: 0, rhs: inner },
                }))
            }
            T::LBracket => match self.token_tag(self.tok_i + 1) {
                T::Asterisk => {
                    self.next_token();
                    let asterisk = self.next_token();
                    let mut sentinel: node::Index = 0;
                    if let Some(ident) = self.eat_token(T::Identifier) {
                        let ident_slice =
                            self.source(self.token_start(ident)..self.token_start(ident + 1));
                        if trim_ascii_end(ident_slice) != b"c" {
                            self.tok_i -= 1;
                        }
                    } else if self.eat_token(T::Colon).is_some() {
                        sentinel = self.expect_expr()?;
                    }
                    self.expect_token(T::RBracket)?;
                    let mods = self.parse_ptr_modifiers()?;
                    let elem_type = self.expect_type_expr()?;
                    if mods.bit_range_start == 0 {
                        if sentinel == 0 && mods.addrspace_node == 0 {
                            Ok(self.add_node(Node {
                                tag: N::PtrTypeAligned,
                                main_token: asterisk,
                                data: node::Data {
                                    lhs: mods.align_node,
                                    rhs: elem_type,
                                },
                            }))
                        } else if mods.align_node == 0 && mods.addrspace_node == 0 {
                            Ok(self.add_node(Node {
                                tag: N::PtrTypeSentinel,
                                main_token: asterisk,
                                data: node::Data {
                                    lhs: sentinel,
                                    rhs: elem_type,
                                },
                            }))
                        } else {
                            let lhs = self.add_extra(node::PtrType {
                                sentinel,
                                align_node: mods.align_node,
                                addrspace_node: mods.addrspace_node,
                            });
                            Ok(self.add_node(Node {
                                tag: N::PtrType,
                                main_token: asterisk,
                                data: node::Data {
                                    lhs,
                                    rhs: elem_type,
                                },
                            }))
                        }
                    } else {
                        let lhs = self.add_extra(node::PtrTypeBitRange {
                            sentinel,
                            align_node: mods.align_node,
                            addrspace_node: mods.addrspace_node,
                            bit_range_start: mods.bit_range_start,
                            bit_range_end: mods.bit_range_end,
                        });
                        Ok(self.add_node(Node {
                            tag: N::PtrTypeBitRange,
                            main_token: asterisk,
                            data: node::Data {
                                lhs,
                                rhs: elem_type,
                            },
                        }))
                    }
                }
                _ => {
                    let lbracket = self.next_token();
                    let len_expr = self.parse_expr()?;
                    let sentinel = match self.eat_token(T::Colon) {
                        Some(_) => self.expect_expr()?,
                        None => 0,
                    };
                    self.expect_token(T::RBracket)?;
                    if len_expr == 0 {
                        let mods = self.parse_ptr_modifiers()?;
                        let elem_type = self.expect_type_expr()?;
                        if mods.bit_range_start != 0 {
                            self.warn_msg(Error::new(
                                E::InvalidBitRange,
                                self.node(mods.bit_range_start).main_token,
                            ));
                        }
                        if sentinel == 0 && mods.addrspace_node == 0 {
                            Ok(self.add_node(Node {
                                tag: N::PtrTypeAligned,
                                main_token: lbracket,
                                data: node::Data {
                                    lhs: mods.align_node,
                                    rhs: elem_type,
                                },
                            }))
                        } else if mods.align_node == 0 && mods.addrspace_node == 0 {
                            Ok(self.add_node(Node {
                                tag: N::PtrTypeSentinel,
                                main_token: lbracket,
                                data: node::Data {
                                    lhs: sentinel,
                                    rhs: elem_type,
                                },
                            }))
                        } else {
                            let lhs = self.add_extra(node::PtrType {
                                sentinel,
                                align_node: mods.align_node,
                                addrspace_node: mods.addrspace_node,
                            });
                            Ok(self.add_node(Node {
                                tag: N::PtrType,
                                main_token: lbracket,
                                data: node::Data {
                                    lhs,
                                    rhs: elem_type,
                                },
                            }))
                        }
                    } else {
                        match self.token_tag(self.tok_i) {
                            T::KeywordAlign
                            | T::KeywordConst
                            | T::KeywordVolatile
                            | T::KeywordAllowzero
                            | T::KeywordAddrspace => {
                                return self.fail(E::PtrModOnArrayChildType);
                            }
                            _ => {}
                        }
                        let elem_type = self.expect_type_expr()?;
                        if sentinel == 0 {
                            Ok(self.add_node(Node {
                                tag: N::ArrayType,
                                main_token: lbracket,
                                data: node::Data {
                                    lhs: len_expr,
                                    rhs: elem_type,
                                },
                            }))
                        } else {
                            let rhs = self.add_extra(node::ArrayTypeSentinel {
                                elem_type,
                                sentinel,
                            });
                            Ok(self.add_node(Node {
                                tag: N::ArrayTypeSentinel,
                                main_token: lbracket,
                                data: node::Data { lhs: len_expr, rhs },
                            }))
                        }
                    }
                }
            },
            _ => self.parse_error_union_expr(),
        }
    }

    pub(super) fn expect_type_expr(&mut self) -> Result<node::Index> {
        let node = self.parse_type_expr()?;
        if node == 0 {
            return self.fail(E::ExpectedTypeExpr);
        }
        Ok(node)
    }

    pub(super) fn parse_error_union_expr(&mut self) -> Result<node::Index> {
        let suffix_expr = self.parse_suffix_expr()?;
        if suffix_expr == 0 {
            return Ok(NULL_NODE);
        }
        let Some(bang) = self.eat_token(T::Bang) else {
            return Ok(suffix_expr);
        };
        let rhs = self.expect_type_expr()?;
        Ok(self.add_node(Node {
            tag: N::ErrorUnion,
            main_token: bang,
            data: node::Data {
                lhs: suffix_expr,
                rhs,
            },
        }))
    }

    pub(super) fn parse_suffix_expr(&mut self) -> Result<node::Index> {
        if self.eat_token(T::KeywordAsync).is_some() {
            let mut res = self.expect_primary_type_expr()?;
            loop {
                let node = self.parse_suffix_op(res)?;
                if node == 0 {
                    break;
                }
                res = node;
            }
            let Some(lparen) = self.eat_token(T::LParen) else {
                self.warn(E::ExpectedParamList);
                return Ok(res);
            };
            let mut params = Vec::new();
            loop {
                if self.eat_token(T::RParen).is_some() {
                    break;
                }
                let param = self.expect_expr()?;
                params.push(param);
                match self.token_tag(self.tok_i) {
                    T::Comma => self.tok_i += 1,
                    T::RParen => {
                        self.tok_i += 1;
                        break;
                    }
                    T::Colon | T::RBrace | T::RBracket => return self.fail_expected(T::RParen),
                    _ => self.warn(E::ExpectedCommaAfterArg),
                }
            }
            let comma = self.token_tag(self.tok_i - 2) == T::Comma;
            return Ok(match params[..] {
                [] => self.add_node(Node {
                    tag: match comma {
                        true => N::AsyncCallOneComma,
                        false => N::AsyncCallOne,
                    },
                    main_token: lparen,
                    data: node::Data { lhs: res, rhs: 0 },
                }),
                [rhs] => self.add_node(Node {
                    tag: match comma {
                        true => N::AsyncCallOneComma,
                        false => N::AsyncCallOne,
                    },
                    main_token: lparen,
                    data: node::Data { lhs: res, rhs },
                }),
                [..] => {
                    let span = self.list_to_span(&params);
                    let rhs = self.add_extra(span);
                    self.add_node(Node {
                        tag: match comma {
                            true => N::AsyncCallComma,
                            false => N::AsyncCall,
                        },
                        main_token: lparen,
                        data: node::Data { lhs: res, rhs },
                    })
                }
            });
        }

        let mut res = self.parse_primary_type_expr()?;
        if res == 0 {
            return Ok(res);
        }
        loop {
            let suffix_op = self.parse_suffix_op(res)?;
            if suffix_op != 0 {
                res = suffix_op;
                continue;
            }
            let Some(lparen) = self.eat_token(T::LParen) else {
                return Ok(res);
            };
            let mut params = Vec::new();
            loop {
                if self.eat_token(T::RParen).is_some() {
                    break;
                }
                let param = self.expect_expr()?;
                params.push(param);
                match self.token_tag(self.tok_i) {
                    T::Comma => self.tok_i += 1,
                    T::RParen => {
                        self.tok_i += 1;
                        break;
                    }
                    T::Colon | T::RBrace | T::RBracket => return self.fail_expected(T::RParen),
                    _ => self.warn(E::ExpectedCommaAfterArg),
                }
            }
            let comma = self.token_tag(self.tok_i - 2) == T::Comma;
            res = match params[..] {
                [] => self.add_node(Node {
                    tag: match comma {
                        true => N::CallOneComma,
                        false => N::CallOne,
                    },
                    main_token: lparen,
                    data: node::Data { lhs: res, rhs: 0 },
                }),
                [rhs] => self.add_node(Node {
                    tag: match comma {
                        true => N::CallOneComma,
                        false => N::CallOne,
                    },
                    main_token: lparen,
                    data: node::Data { lhs: res, rhs },
                }),
                [..] => {
                    let span = self.list_to_span(&params);
                    let rhs = self.add_extra(span);
                    self.add_node(Node {
                        tag: match comma {
                            true => N::CallComma,
                            false => N::Call,
                        },
                        main_token: lparen,
                        data: node::Data { lhs: res, rhs },
                    })
                }
            }
        }
    }

    pub(super) fn parse_suffix_op(&mut self, lhs: node::Index) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            T::LBracket => {
                let lbracket = self.next_token();
                let index_expr = self.expect_expr()?;

                if self.eat_token(T::Ellipsis2).is_some() {
                    let end_expr = self.parse_expr()?;
                    if self.eat_token(T::Colon).is_some() {
                        let sentinel = self.expect_expr()?;
                        self.expect_token(T::RBracket)?;
                        let rhs = self.add_extra(node::SliceSentinel {
                            start: index_expr,
                            end: end_expr,
                            sentinel,
                        });
                        return Ok(self.add_node(Node {
                            tag: N::SliceSentinel,
                            main_token: lbracket,
                            data: node::Data { lhs, rhs },
                        }));
                    }
                    self.expect_token(T::RBracket)?;
                    if end_expr == 0 {
                        return Ok(self.add_node(Node {
                            tag: N::SliceOpen,
                            main_token: lbracket,
                            data: node::Data {
                                lhs,
                                rhs: index_expr,
                            },
                        }));
                    }
                    let rhs = self.add_extra(node::Slice {
                        start: index_expr,
                        end: end_expr,
                    });
                    return Ok(self.add_node(Node {
                        tag: N::Slice,
                        main_token: lbracket,
                        data: node::Data { lhs, rhs },
                    }));
                }
                self.expect_token(T::RBracket)?;
                Ok(self.add_node(Node {
                    tag: N::ArrayAccess,
                    main_token: lbracket,
                    data: node::Data {
                        lhs,
                        rhs: index_expr,
                    },
                }))
            }
            T::PeriodAsterisk => {
                let main_token = self.next_token();
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::Deref,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::InvalidPeriodAsterisks => {
                self.warn(E::AsteriskAfterPtrDeref);
                let main_token = self.next_token();
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::Deref,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::Period => match self.token_tag(self.tok_i + 1) {
                T::Identifier => {
                    let main_token = self.next_token();
                    let rhs = self.next_token();
                    Ok(self.add_node(Node {
                        tag: N::FieldAccess,
                        main_token,
                        data: node::Data { lhs, rhs },
                    }))
                }
                T::QuestionMark => {
                    let main_token = self.next_token();
                    let rhs = self.next_token();
                    Ok(self.add_node(Node {
                        tag: N::UnwrapOptional,
                        main_token,
                        data: node::Data { lhs, rhs },
                    }))
                }
                T::LBrace => Ok(NULL_NODE),
                _ => {
                    self.tok_i += 1;
                    self.warn(E::ExpectedSuffixOp);
                    Ok(NULL_NODE)
                }
            },
            _ => Ok(NULL_NODE),
        }
    }

    pub(super) fn parse_primary_type_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            T::CharLiteral => {
                let main_token = self.next_token();
                Ok(self.add_node(Node {
                    tag: N::CharLiteral,
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }
            T::NumberLiteral => {
                let main_token = self.next_token();
                Ok(self.add_node(Node {
                    tag: N::NumberLiteral,
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }
            T::KeywordUnreachable => {
                let main_token = self.next_token();
                Ok(self.add_node(Node {
                    tag: N::UnreachableLiteral,
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }
            T::KeywordAnyframe => {
                let main_token = self.next_token();
                Ok(self.add_node(Node {
                    tag: N::AnyframeLiteral,
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }
            T::StringLiteral => {
                let main_token = self.next_token();
                Ok(self.add_node(Node {
                    tag: N::StringLiteral,
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }

            T::Builtin => self.parse_builtin_call(),
            T::KeywordFn => self.parse_fn_proto(),
            T::KeywordIf => self.parse_if(Self::expect_type_expr),
            T::KeywordSwitch => self.expect_switch_expr(),

            T::KeywordExtern | T::KeywordPacked => {
                self.tok_i += 1;
                self.parse_container_decl_auto()
            }

            T::KeywordStruct | T::KeywordOpaque | T::KeywordEnum | T::KeywordUnion => {
                self.parse_container_decl_auto()
            }

            T::KeywordComptime => {
                let main_token = self.next_token();
                let lhs = self.expect_type_expr()?;
                Ok(self.add_node(Node {
                    tag: N::Comptime,
                    main_token,
                    data: node::Data {
                        lhs,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }
            T::MultilineStringLiteralLine => {
                let first_line = self.next_token();
                while self.token_tag(self.tok_i) == T::MultilineStringLiteralLine {
                    self.tok_i += 1;
                }
                Ok(self.add_node(Node {
                    tag: N::MultilineStringLiteral,
                    main_token: first_line,
                    data: node::Data {
                        lhs: first_line,
                        rhs: self.tok_i - 1,
                    },
                }))
            }
            T::Identifier => match self.token_tag(self.tok_i + 1) {
                T::Colon => match self.token_tag(self.tok_i + 2) {
                    T::KeywordInline => {
                        self.tok_i += 3;
                        match self.token_tag(self.tok_i) {
                            T::KeywordFor => self.parse_for(Self::expect_type_expr),
                            T::KeywordWhile => self.parse_while_type_expr(),
                            _ => self.fail(E::ExpectedInlinable),
                        }
                    }
                    T::KeywordFor => {
                        self.tok_i += 2;
                        self.parse_for(Self::expect_type_expr)
                    }
                    T::KeywordWhile => {
                        self.tok_i += 2;
                        self.parse_while_type_expr()
                    }
                    T::LBrace => {
                        self.tok_i += 2;
                        self.parse_block()
                    }
                    _ => {
                        let main_token = self.next_token();
                        Ok(self.add_node(Node {
                            tag: N::Identifier,
                            main_token,
                            data: node::Data {
                                lhs: UNDEFINED_NODE,
                                rhs: UNDEFINED_NODE,
                            },
                        }))
                    }
                },
                _ => {
                    let main_token = self.next_token();
                    Ok(self.add_node(Node {
                        tag: N::Identifier,
                        main_token,
                        data: node::Data {
                            lhs: UNDEFINED_NODE,
                            rhs: UNDEFINED_NODE,
                        },
                    }))
                }
            },
            T::KeywordInline => {
                self.tok_i += 1;
                match self.token_tag(self.tok_i) {
                    T::KeywordFor => self.parse_for(Self::expect_type_expr),
                    T::KeywordWhile => self.parse_while_type_expr(),
                    _ => self.fail(E::ExpectedInlinable),
                }
            }
            T::KeywordFor => self.parse_for(Self::expect_type_expr),
            T::KeywordWhile => self.parse_while_type_expr(),
            T::Period => match self.token_tag(self.tok_i + 1) {
                T::Identifier => {
                    let lhs = self.next_token();
                    let main_token = self.next_token();
                    Ok(self.add_node(Node {
                        tag: N::EnumLiteral,
                        data: node::Data {
                            lhs,
                            rhs: UNDEFINED_NODE,
                        },
                        main_token,
                    }))
                }
                T::LBrace => {
                    let lbrace = self.tok_i + 1;
                    self.tok_i = lbrace + 1;

                    let mut inits = Vec::new();
                    let field_init = self.parse_field_init()?;
                    if field_init != 0 {
                        inits.push(field_init);
                        loop {
                            match self.token_tag(self.tok_i) {
                                T::Comma => self.tok_i += 1,
                                T::RBrace => {
                                    self.tok_i += 1;
                                    break;
                                }
                                T::Colon | T::RParen | T::RBracket => {
                                    return self.fail_expected(T::RBrace)
                                }
                                _ => self.warn(E::ExpectedCommaAfterInitializer),
                            }
                            if self.eat_token(T::RBrace).is_some() {
                                break;
                            }
                            let next = self.expect_field_init()?;
                            inits.push(next);
                        }
                        let comma = self.token_tag(self.tok_i - 2) == T::Comma;
                        return match inits[..] {
                            [] => unreachable!(),
                            [lhs] => Ok(self.add_node(Node {
                                tag: match comma {
                                    true => N::StructInitDotTwoComma,
                                    false => N::StructInitDotTwo,
                                },
                                main_token: lbrace,
                                data: node::Data { lhs, rhs: 0 },
                            })),
                            [lhs, rhs] => Ok(self.add_node(Node {
                                tag: match comma {
                                    true => N::StructInitDotTwoComma,
                                    false => N::StructInitDotTwo,
                                },
                                main_token: lbrace,
                                data: node::Data { lhs, rhs },
                            })),
                            [..] => {
                                let span = self.list_to_span(&inits);
                                Ok(self.add_node(Node {
                                    tag: match comma {
                                        true => N::StructInitDotComma,
                                        false => N::StructInitDot,
                                    },
                                    main_token: lbrace,
                                    data: node::Data {
                                        lhs: span.start,
                                        rhs: span.end,
                                    },
                                }))
                            }
                        };
                    }

                    loop {
                        if self.eat_token(T::RBrace).is_some() {
                            break;
                        }
                        let elem_init = self.expect_expr()?;
                        inits.push(elem_init);
                        match self.token_tag(self.tok_i) {
                            T::Comma => self.tok_i += 1,
                            T::RBrace => {
                                self.tok_i += 1;
                                break;
                            }
                            T::Colon | T::RParen | T::RBracket => {
                                return self.fail_expected(T::RBrace)
                            }
                            _ => self.warn(E::ExpectedCommaAfterInitializer),
                        }
                    }
                    let comma = self.token_tag(self.tok_i - 2) == T::Comma;
                    match inits[..] {
                        [] => Ok(self.add_node(Node {
                            tag: N::StructInitDotTwo,
                            main_token: lbrace,
                            data: node::Data { lhs: 0, rhs: 0 },
                        })),
                        [lhs] => Ok(self.add_node(Node {
                            tag: match comma {
                                true => N::ArrayInitDotTwoComma,
                                false => N::ArrayInitDotTwo,
                            },
                            main_token: lbrace,
                            data: node::Data { lhs, rhs: 0 },
                        })),
                        [lhs, rhs] => Ok(self.add_node(Node {
                            tag: match comma {
                                true => N::ArrayInitDotTwoComma,
                                false => N::ArrayInitDotTwo,
                            },
                            main_token: lbrace,
                            data: node::Data { lhs, rhs: 0 },
                        })),
                        [..] => {
                            let span = self.list_to_span(&inits);
                            Ok(self.add_node(Node {
                                tag: match comma {
                                    true => N::ArrayInitDotComma,
                                    false => N::ArrayInitDot,
                                },
                                main_token: lbrace,
                                data: node::Data {
                                    lhs: span.start,
                                    rhs: span.end,
                                },
                            }))
                        }
                    }
                }
                _ => Ok(NULL_NODE),
            },
            T::KeywordError => match self.token_tag(self.tok_i + 1) {
                T::LBrace => {
                    let error_token = self.tok_i;
                    self.tok_i += 2;
                    loop {
                        if self.eat_token(T::RBrace).is_some() {
                            break;
                        }
                        self.eat_doc_comments();
                        self.expect_token(T::Identifier)?;
                        match self.token_tag(self.tok_i) {
                            T::Comma => self.tok_i += 1,
                            T::RBrace => {
                                self.tok_i += 1;
                                break;
                            }
                            T::Colon | T::RParen | T::RBracket => {
                                return self.fail_expected(T::RBrace)
                            }
                            _ => self.warn(E::ExpectedCommaAfterField),
                        }
                    }
                    Ok(self.add_node(Node {
                        tag: N::ErrorSetDecl,
                        main_token: error_token,
                        data: node::Data {
                            lhs: UNDEFINED_NODE,
                            rhs: (self.tok_i - 1),
                        },
                    }))
                }
                _ => {
                    let main_token = self.next_token();
                    let period = self.eat_token(T::Period);
                    if period.is_none() {
                        self.warn_expected(T::Period);
                    }
                    let identifier = self.eat_token(T::Identifier);
                    if identifier.is_none() {
                        self.warn_expected(T::Identifier);
                    }
                    Ok(self.add_node(Node {
                        tag: N::ErrorValue,
                        main_token,
                        data: node::Data {
                            lhs: period.unwrap_or(0),
                            rhs: identifier.unwrap_or(0),
                        },
                    }))
                }
            },
            T::LParen => {
                let main_token = self.next_token();
                let lhs = self.expect_expr()?;
                let rhs = self.expect_token(T::RParen)?;
                Ok(self.add_node(Node {
                    tag: N::GroupedExpression,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            _ => Ok(NULL_NODE),
        }
    }

    pub(super) fn expect_primary_type_expr(&mut self) -> Result<node::Index> {
        let node = self.parse_primary_type_expr()?;
        if node == 0 {
            return self.fail(E::ExpectedPrimaryTypeExpr);
        }
        Ok(node)
    }

    pub(super) fn parse_builtin_call(&mut self) -> Result<node::Index> {
        let builtin_token = self.assert_token(T::Builtin);
        if self.eat_token(T::LParen).is_none() {
            self.warn(E::ExpectedParamList);
            return Ok(self.add_node(Node {
                tag: N::Identifier,
                main_token: builtin_token,
                data: node::Data {
                    lhs: UNDEFINED_NODE,
                    rhs: UNDEFINED_NODE,
                },
            }));
        }
        let mut params = Vec::new();
        loop {
            if self.eat_token(T::RParen).is_some() {
                break;
            }
            let param = self.expect_expr()?;
            params.push(param);
            match self.token_tag(self.tok_i) {
                T::Comma => self.tok_i += 1,
                T::RParen => {
                    self.tok_i += 1;
                    break;
                }
                _ => self.warn(E::ExpectedCommaAfterArg),
            }
        }
        let comma = self.token_tag(self.tok_i - 2) == T::Comma;
        match params[..] {
            [] => Ok(self.add_node(Node {
                tag: match comma {
                    true => N::BuiltinCallTwoComma,
                    false => N::BuiltinCallTwo,
                },
                main_token: builtin_token,
                data: node::Data { lhs: 0, rhs: 0 },
            })),
            [lhs] => Ok(self.add_node(Node {
                tag: match comma {
                    true => N::BuiltinCallTwoComma,
                    false => N::BuiltinCallTwo,
                },
                main_token: builtin_token,
                data: node::Data { lhs, rhs: 0 },
            })),
            [lhs, rhs] => Ok(self.add_node(Node {
                tag: match comma {
                    true => N::BuiltinCallTwoComma,
                    false => N::BuiltinCallTwo,
                },
                main_token: builtin_token,
                data: node::Data { lhs, rhs },
            })),
            [..] => {
                let span = self.list_to_span(&params);
                Ok(self.add_node(Node {
                    tag: match comma {
                        true => N::BuiltinCallComma,
                        false => N::BuiltinCall,
                    },
                    main_token: builtin_token,
                    data: node::Data {
                        lhs: span.start,
                        rhs: span.end,
                    },
                }))
            }
        }
    }
}

// https://github.com/rust-lang/rust/issues/94035
const fn trim_ascii_end(mut bytes: &[u8]) -> &[u8] {
    while let [rest @ .., last] = bytes {
        if last.is_ascii_whitespace() {
            bytes = rest;
        } else {
            break;
        }
    }
    bytes
}
