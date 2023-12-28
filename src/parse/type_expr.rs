use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_type_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(QuestionMark) => todo!("parse_type_expr"),
            token!(KeywordAnyframe) => todo!("parse_type_expr"),
            token!(Asterisk) => todo!("parse_type_expr"),
            token!(AsteriskAsterisk) => todo!("parse_type_expr"),
            token!(LBracket) => match self.token_tag(self.tok_i + 1) {
                token!(Asterisk) => todo!("parse_type_expr"),
                _ => {
                    let lbracket = self.next_token();
                    let len_expr = self.parse_expr()?;
                    let sentinel = match self.eat_token(token!(Colon)) {
                        Some(_) => self.expect_expr()?,
                        None => 0,
                    };
                    self.expect_token(token!(RBracket))?;
                    if len_expr == 0 {
                        let mods = self.parse_ptr_modifiers()?;
                        let elem_type = self.expect_type_expr()?;
                        if mods.bit_range_start != 0 {
                            self.warn_msg(Error {
                                tag: error!(InvalidBitRange),
                                token: self.node(mods.bit_range_start).main_token,
                                ..Default::default()
                            });
                        }
                        if sentinel == 0 && mods.addrspace_node == 0 {
                            Ok(self.add_node(Node {
                                tag: node!(PtrTypeAligned),
                                main_token: lbracket,
                                data: node::Data {
                                    lhs: mods.align_node,
                                    rhs: elem_type,
                                },
                            }))
                        } else if mods.align_node == 0 && mods.addrspace_node == 0 {
                            Ok(self.add_node(Node {
                                tag: node!(PtrTypeSentinel),
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
                                tag: node!(PtrType),
                                main_token: lbracket,
                                data: node::Data {
                                    lhs,
                                    rhs: elem_type,
                                },
                            }))
                        }
                    } else {
                        match self.token_tag(self.tok_i) {
                            token!(KeywordAlign)
                            | token!(KeywordConst)
                            | token!(KeywordVolatile)
                            | token!(KeywordAllowzero)
                            | token!(KeywordAddrspace) => {
                                return self.fail(error!(PtrModOnArrayChildType));
                            }
                            _ => {}
                        }
                        let elem_type = self.expect_type_expr()?;
                        if sentinel == 0 {
                            Ok(self.add_node(Node {
                                tag: node!(ArrayType),
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
                                tag: node!(ArrayTypeSentinel),
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
            return self.fail(error!(ExpectedTypeExpr));
        }
        Ok(node)
    }

    pub(super) fn parse_error_union_expr(&mut self) -> Result<node::Index> {
        let suffix_expr = self.parse_suffix_expr()?;
        if suffix_expr == 0 {
            return Ok(NULL_NODE);
        }
        let Some(bang) = self.eat_token(token!(Bang)) else {
            return Ok(suffix_expr);
        };
        let rhs = self.expect_type_expr()?;
        Ok(self.add_node(Node {
            tag: node!(ErrorUnion),
            main_token: bang,
            data: node::Data {
                lhs: suffix_expr,
                rhs,
            },
        }))
    }

    pub(super) fn parse_suffix_expr(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordAsync)).is_some() {
            todo!("parse_suffix_expr");
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
            let Some(lparen) = self.eat_token(token!(LParen)) else {
                return Ok(res);
            };
            let mut params = Vec::new();
            loop {
                if self.eat_token(token!(RParen)).is_some() {
                    break;
                }
                let param = self.expect_expr()?;
                params.push(param);
                match self.token_tag(self.tok_i) {
                    token!(Comma) => self.tok_i += 1,
                    token!(RParen) => {
                        self.tok_i += 1;
                        break;
                    }
                    token!(Colon) | token!(RBrace) | token!(RBracket) => {
                        return self.fail_expected(token!(RParen))
                    }
                    _ => self.warn(error!(ExpectedCommaAfterArg)),
                }
            }
            let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
            res = match params[..] {
                [] => self.add_node(Node {
                    tag: match comma {
                        true => node!(CallOneComma),
                        false => node!(CallOne),
                    },
                    main_token: lparen,
                    data: node::Data { lhs: res, rhs: 0 },
                }),
                [rhs] => self.add_node(Node {
                    tag: match comma {
                        true => node!(CallOneComma),
                        false => node!(CallOne),
                    },
                    main_token: lparen,
                    data: node::Data { lhs: res, rhs },
                }),
                _ => {
                    let span = self.list_to_span(&params);
                    let rhs = self.add_extra(node::SubRange { ..span });
                    self.add_node(Node {
                        tag: match comma {
                            true => node!(CallComma),
                            false => node!(Call),
                        },
                        main_token: lparen,
                        data: node::Data { lhs: res, rhs: rhs },
                    })
                }
            }
        }
    }

    pub(super) fn parse_suffix_op(&mut self, lhs: node::Index) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(LBracket) => {
                let lbracket = self.next_token();
                let index_expr = self.expect_expr()?;

                if self.eat_token(token!(Ellipsis2)).is_some() {
                    let end_expr = self.parse_expr()?;
                    if self.eat_token(token!(Colon)).is_some() {
                        let sentinel = self.expect_expr()?;
                        self.expect_token(token!(RBracket))?;
                        let rhs = self.add_extra(node::SliceSentinel {
                            start: index_expr,
                            end: end_expr,
                            sentinel,
                        });
                        return Ok(self.add_node(Node {
                            tag: node!(SliceSentinel),
                            main_token: lbracket,
                            data: node::Data { lhs, rhs },
                        }));
                    }
                    self.expect_token(token!(RBracket))?;
                    if end_expr == 0 {
                        return Ok(self.add_node(Node {
                            tag: node!(SliceOpen),
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
                        tag: node!(Slice),
                        main_token: lbracket,
                        data: node::Data { lhs, rhs },
                    }));
                }
                self.expect_token(token!(RBracket))?;
                Ok(self.add_node(Node {
                    tag: node!(ArrayAccess),
                    main_token: lbracket,
                    data: node::Data {
                        lhs,
                        rhs: index_expr,
                    },
                }))
            }
            token!(PeriodAsterisk) => {
                let main_token = self.next_token();
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: node!(Deref),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(InvalidPeriodAsterisks) => {
                self.warn(error!(AsteriskAfterPtrDeref));
                let main_token = self.next_token();
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: node!(Deref),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(Period) => match self.token_tag(self.tok_i + 1) {
                token!(Identifier) => {
                    let main_token = self.next_token();
                    let rhs = self.next_token();
                    Ok(self.add_node(Node {
                        tag: node!(FieldAccess),
                        main_token,
                        data: node::Data { lhs, rhs },
                    }))
                }
                token!(QuestionMark) => {
                    let main_token = self.next_token();
                    let rhs = self.next_token();
                    Ok(self.add_node(Node {
                        tag: node!(UnwrapOptional),
                        main_token,
                        data: node::Data { lhs, rhs },
                    }))
                }
                token!(LBrace) => Ok(NULL_NODE),
                _ => {
                    self.tok_i += 1;
                    self.warn(error!(ExpectedSuffixOp));
                    Ok(NULL_NODE)
                }
            },
            _ => Ok(NULL_NODE),
        }
    }

    pub(super) fn parse_primary_type_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(CharLiteral) => {
                let main_token = self.next_token();
                Ok(self.add_node(Node {
                    tag: node!(CharLiteral),
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }
            token!(NumberLiteral) => {
                let main_token = self.next_token();
                Ok(self.add_node(Node {
                    tag: node!(NumberLiteral),
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }))
            }
            token!(KeywordUnreachable) => todo!("parse_primary_type_expr"),
            token!(KeywordAnyframe) => todo!("parse_primary_type_expr"),
            token!(StringLiteral) => todo!("parse_primary_type_expr"),
            token!(Builtin) => todo!("parse_primary_type_expr"),
            token!(KeywordFn) => todo!("parse_primary_type_expr"),
            token!(KeywordIf) => todo!("parse_primary_type_expr"),
            token!(KeywordSwitch) => todo!("parse_primary_type_expr"),
            token!(KeywordExtern) | token!(KeywordPacked) => todo!("parse_primary_type_expr"),
            token!(KeywordStruct)
            | token!(KeywordOpaque)
            | token!(KeywordEnum)
            | token!(KeywordUnion) => self.parse_container_decl_auto(),
            token!(KeywordComptime) => todo!("parse_primary_type_expr"),
            token!(MultilineStringLiteralLine) => todo!("parse_primary_type_expr"),
            token!(Identifier) => match self.token_tag(self.tok_i + 1) {
                token!(Colon) => match self.token_tag(self.tok_i + 2) {
                    token!(KeywordInline) => {
                        self.tok_i += 3;
                        match self.token_tag(self.tok_i) {
                            token!(KeywordFor) => self.parse_for_type_expr(),
                            token!(KeywordWhile) => self.parse_while_type_expr(),
                            _ => self.fail(error!(ExpectedInlinable)),
                        }
                    }
                    token!(KeywordFor) => {
                        self.tok_i += 2;
                        self.parse_for_type_expr()
                    }
                    token!(KeywordWhile) => {
                        self.tok_i += 2;
                        self.parse_while_type_expr()
                    }
                    token!(LBrace) => {
                        self.tok_i += 2;
                        self.parse_block()
                    }
                    _ => {
                        let main_token = self.next_token();
                        Ok(self.add_node(Node {
                            tag: node!(Identifier),
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
                        tag: node!(Identifier),
                        main_token,
                        data: node::Data {
                            lhs: UNDEFINED_NODE,
                            rhs: UNDEFINED_NODE,
                        },
                    }))
                }
            },
            token!(KeywordInline) => todo!("parse_primary_type_expr"),
            token!(KeywordFor) => todo!("parse_primary_type_expr"),
            token!(KeywordWhile) => todo!("parse_primary_type_expr"),
            token!(Period) => match self.token_tag(self.tok_i + 1) {
                token!(Identifier) => {
                    let lhs = self.next_token();
                    let main_token = self.next_token();
                    Ok(self.add_node(Node {
                        tag: node!(EnumLiteral),
                        data: node::Data {
                            lhs,
                            rhs: UNDEFINED_NODE,
                        },
                        main_token,
                    }))
                }
                token!(LBrace) => {
                    let lbrace = self.tok_i + 1;
                    self.tok_i = lbrace + 1;

                    let mut inits = Vec::new();
                    let field_init = self.parse_field_init()?;
                    if field_init != 0 {
                        inits.push(field_init);
                        loop {
                            match self.token_tag(self.tok_i) {
                                token!(Comma) => self.tok_i += 1,
                                token!(RBrace) => {
                                    self.tok_i += 1;
                                    break;
                                }
                                token!(Colon) | token!(RParen) | token!(RBracket) => {
                                    return self.fail_expected(token!(RBrace))
                                }
                                _ => self.warn(error!(ExpectedCommaAfterInitializer)),
                            }
                            if self.eat_token(token!(RBrace)).is_some() {
                                break;
                            }
                            let next = self.expect_field_init()?;
                            inits.push(next);
                        }
                        let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
                        return match inits[..] {
                            [] => unreachable!(),
                            [lhs] => Ok(self.add_node(Node {
                                tag: match comma {
                                    true => node!(StructInitDotTwoComma),
                                    false => node!(StructInitDotTwo),
                                },
                                main_token: lbrace,
                                data: node::Data { lhs, rhs: 0 },
                            })),
                            [_, _] => todo!("parse_primary_type_expr"),
                            _ => todo!("parse_primary_type_expr"),
                        };
                    }

                    loop {
                        if self.eat_token(token!(RBrace)).is_some() {
                            break;
                        }
                        let elem_init = self.expect_expr()?;
                        inits.push(elem_init);
                        todo!("parse_primary_type_expr");
                    }
                    let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
                    match inits[..] {
                        [] => Ok(self.add_node(Node {
                            tag: node!(StructInitDotTwo),
                            main_token: lbrace,
                            data: node::Data { lhs: 0, rhs: 0 },
                        })),
                        [_] => todo!("parse_primary_type_expr"),
                        [_, _] => todo!("parse_primary_type_expr"),
                        _ => todo!("parse_primary_type_expr"),
                    }
                }
                _ => Ok(NULL_NODE),
            },
            token!(KeywordError) => match self.token_tag(self.tok_i + 1) {
                token!(LBrace) => {
                    let error_token = self.tok_i;
                    self.tok_i += 2;
                    loop {
                        if self.eat_token(token!(RBrace)).is_some() {
                            break;
                        }
                        todo!("parse_primary_type_expr")
                    }
                    Ok(self.add_node(Node {
                        tag: node!(ErrorSetDecl),
                        main_token: error_token,
                        data: node::Data {
                            lhs: UNDEFINED_NODE,
                            rhs: (self.tok_i - 1),
                        },
                    }))
                }
                _ => todo!("parse_primary_type_expr"),
            },
            token!(LParen) => todo!("parse_primary_type_expr"),
            _ => Ok(NULL_NODE),
        }
    }

    pub(super) fn expect_primary_type_expr(&mut self) -> Result<node::Index> {
        todo!("expect_primary_type_expr")
    }

    pub(super) fn parse_builtin_call(&mut self) -> Result<node::Index> {
        let builtin_token = self.assert_token(token!(Builtin));
        if self.eat_token(token!(LParen)).is_none() {
            self.warn(error!(ExpectedParamList));
            return Ok(self.add_node(Node {
                tag: node!(Identifier),
                main_token: builtin_token,
                data: node::Data {
                    lhs: UNDEFINED_NODE,
                    rhs: UNDEFINED_NODE,
                },
            }));
        }
        let mut params = Vec::new();
        loop {
            if self.eat_token(token!(RParen)).is_some() {
                break;
            }
            let param = self.expect_expr()?;
            params.push(param);
            match self.token_tag(self.tok_i) {
                token!(Comma) => self.tok_i += 1,
                token!(RParen) => {
                    self.tok_i += 1;
                    break;
                }
                _ => self.warn(error!(ExpectedCommaAfterArg)),
            }
        }
        let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
        match params[..] {
            [] => Ok(self.add_node(Node {
                tag: match comma {
                    true => node!(BuiltinCallTwoComma),
                    false => node!(BuiltinCallTwo),
                },
                main_token: builtin_token,
                data: node::Data { lhs: 0, rhs: 0 },
            })),
            [lhs] => Ok(self.add_node(Node {
                tag: match comma {
                    true => node!(BuiltinCallTwoComma),
                    false => node!(BuiltinCallTwo),
                },
                main_token: builtin_token,
                data: node::Data { lhs, rhs: 0 },
            })),
            [lhs, rhs] => Ok(self.add_node(Node {
                tag: match comma {
                    true => node!(BuiltinCallTwoComma),
                    false => node!(BuiltinCallTwo),
                },
                main_token: builtin_token,
                data: node::Data { lhs, rhs },
            })),
            _ => {
                let span = self.list_to_span(&params);
                Ok(self.add_node(Node {
                    tag: match comma {
                        true => node!(BuiltinCallComma),
                        false => node!(BuiltinCall),
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
