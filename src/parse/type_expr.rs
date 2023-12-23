use super::*;

impl<'src, 'tok> Parser<'src, 'tok> {
    pub(super) fn parse_type_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(QuestionMark)
            | token!(KeywordAnyframe)
            | token!(Asterisk)
            | token!(AsteriskAsterisk)
            | token!(LBracket) => todo!("parse_type_expr"),
            _ => return self.parse_error_union_expr(),
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
                return Ok(self.add_node(Node {
                    tag: node!(CharLiteral),
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            }
            token!(NumberLiteral) => {
                let main_token = self.next_token();
                return Ok(self.add_node(Node {
                    tag: node!(NumberLiteral),
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            }
            token!(KeywordUnreachable)
            | token!(KeywordAnyframe)
            | token!(StringLiteral)
            | token!(Builtin)
            | token!(KeywordFn)
            | token!(KeywordIf)
            | token!(KeywordSwitch)
            | token!(KeywordExtern)
            | token!(KeywordPacked) => todo!("parse_primary_type_expr"),
            token!(KeywordStruct)
            | token!(KeywordOpaque)
            | token!(KeywordEnum)
            | token!(KeywordUnion) => return self.parse_container_decl_auto(),
            token!(KeywordComptime) => todo!("parse_primary_type_expr"),
            token!(MultilineStringLiteralLine) => todo!("parse_primary_type_expr"),
            token!(Identifier) => match self.token_tag(self.tok_i + 1) {
                token!(Colon) => todo!("parse_primary_type_expr"),
                _ => {
                    let main_token = self.next_token();
                    return add_node!(self, {
                        tag: Identifier,
                        main_token: main_token,
                        data: {
                            lhs: UNDEFINED_TOKEN,
                            rhs: UNDEFINED_TOKEN,
                        }
                    });
                }
            },
            token!(KeywordInline) => todo!("parse_primary_type_expr"),
            token!(KeywordFor) => todo!("parse_primary_type_expr"),
            token!(KeywordWhile) => todo!("parse_primary_type_expr"),
            token!(Period) => match self.token_tag(self.tok_i + 1) {
                token!(Identifier) => {
                    let lhs = self.next_token();
                    let main_token = self.next_token();
                    return Ok(self.add_node(Node {
                        tag: node!(EnumLiteral),
                        data: node::Data {
                            lhs,
                            rhs: UNDEFINED_NODE,
                        },
                        main_token,
                    }));
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
                        match inits.len() {
                            0 => unreachable!(),
                            1 => {
                                return Ok(self.add_node(Node {
                                    tag: match comma {
                                        true => node!(StructInitDotTwoComma),
                                        false => node!(StructInitDotTwo),
                                    },
                                    main_token: lbrace,
                                    data: node::Data {
                                        lhs: inits[0],
                                        rhs: 0,
                                    },
                                }))
                            }
                            2 | _ => todo!("parse_primary_type_expr"),
                        }
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
                        0 => {
                            return Ok(self.add_node(Node {
                                tag: node!(StructInitDotTwo),
                                main_token: lbrace,
                                data: node::Data { lhs: 0, rhs: 0 },
                            }));
                        }
                        1 | 2 | _ => todo!("parse_primary_type_expr"),
                    }
                }
                _ => return Ok(NULL_NODE),
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
                    return add_node!(self, {
                        tag: ErrorSetDecl,
                        main_token: error_token,
                        data: {
                            lhs: UNDEFINED_TOKEN,
                            rhs: self.tok_i - 1,
                        },
                    });
                }
                _ => todo!("parse_primary_type_expr"),
            },
            token!(LParen) => todo!("parse_primary_type_expr"),
            _ => return Ok(NULL_NODE),
        }
    }

    pub(super) fn expect_primary_type_expr(&mut self) -> Result<node::Index> {
        todo!("expect_primary_type_expr")
    }
}
