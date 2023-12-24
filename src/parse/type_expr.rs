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
                        return match inits.len() {
                            0 => unreachable!(),
                            1 => Ok(self.add_node(Node {
                                tag: match comma {
                                    true => node!(StructInitDotTwoComma),
                                    false => node!(StructInitDotTwo),
                                },
                                main_token: lbrace,
                                data: node::Data {
                                    lhs: inits[0],
                                    rhs: 0,
                                },
                            })),
                            2 => todo!("parse_primary_type_expr"),
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
                    match inits.len() {
                        0 => Ok(self.add_node(Node {
                            tag: node!(StructInitDotTwo),
                            main_token: lbrace,
                            data: node::Data { lhs: 0, rhs: 0 },
                        })),
                        1 => todo!("parse_primary_type_expr"),
                        2 => todo!("parse_primary_type_expr"),
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
}
