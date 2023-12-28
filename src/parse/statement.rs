use super::*;

impl Parser<'_, '_> {
    pub(super) fn find_next_stmt(&mut self) {
        let mut level: u32 = 0;
        loop {
            let tok = self.next_token();
            match self.token_tag(tok) {
                token!(LBrace) => level += 1,
                token!(RBrace) => {
                    if level == 0 {
                        self.tok_i -= 1;
                        return;
                    }
                    level -= 1;
                }
                token!(Semicolon) => {
                    if level == 0 {
                        return;
                    }
                }
                token!(Eof) => {
                    self.tok_i -= 1;
                    return;
                }
                _ => {}
            }
        }
    }

    pub(super) fn expect_statement(&mut self, allow_defer_var: bool) -> Result<node::Index> {
        if let Some(comptime_token) = self.eat_token(token!(KeywordComptime)) {
            let block_expr = self.parse_block_expr()?;
            if block_expr != 0 {
                return Ok(self.add_node(Node {
                    tag: node!(Comptime),
                    main_token: comptime_token,
                    data: node::Data {
                        lhs: block_expr,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            }

            if allow_defer_var {
                return self.expect_var_decl_expr_statement(Some(comptime_token));
            } else {
                let assign = self.expect_assign_expr()?;
                self.expect_semicolon(error!(ExpectedSemiAfterStmt), true)?;
                return Ok(assign);
            }
        }

        match self.token_tag(self.tok_i) {
            token!(KeywordNosuspend) => {
                let token = self.next_token();
                let block_expr = self.expect_block_expr_statement()?;
                return Ok(self.add_node(Node {
                    tag: node!(Nosuspend),
                    main_token: token,
                    data: node::Data {
                        lhs: block_expr,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            }
            token!(KeywordSuspend) => {
                let token = self.next_token();
                let block_expr = self.expect_block_expr_statement()?;
                return Ok(self.add_node(Node {
                    tag: node!(Suspend),
                    main_token: token,
                    data: node::Data {
                        lhs: block_expr,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            }
            token!(KeywordDefer) => {
                if allow_defer_var {
                    let token = self.next_token();
                    let block_expr = self.expect_block_expr_statement()?;
                    return Ok(self.add_node(Node {
                        tag: node!(Defer),
                        main_token: token,
                        data: node::Data {
                            lhs: UNDEFINED_NODE,
                            rhs: block_expr,
                        },
                    }));
                }
            }
            token!(KeywordErrdefer) => {
                if allow_defer_var {
                    let token = self.next_token();
                    let payload = self.parse_payload()?;
                    let block_expr = self.expect_block_expr_statement()?;
                    return Ok(self.add_node(Node {
                        tag: node!(Errdefer),
                        main_token: token,
                        data: node::Data {
                            lhs: payload,
                            rhs: block_expr,
                        },
                    }));
                }
            }
            token!(KeywordSwitch) => return self.expect_switch_expr(),
            token!(KeywordIf) => return self.expect_if_statement(),
            token!(KeywordEnum) | token!(KeywordStruct) | token!(KeywordUnion) => {
                let identifier = self.tok_i + 1;
                if self.parse_c_style_container()? {
                    return Ok(self.add_node(Node {
                        tag: node!(Identifier),
                        main_token: identifier,
                        data: node::Data {
                            lhs: UNDEFINED_NODE,
                            rhs: UNDEFINED_NODE,
                        },
                    }));
                }
            }
            _ => {}
        }

        let labeled_statement = self.parse_labeled_statement()?;
        if labeled_statement != 0 {
            return Ok(labeled_statement);
        }

        if allow_defer_var {
            self.expect_var_decl_expr_statement(None)
        } else {
            let assign = self.expect_assign_expr()?;
            self.expect_semicolon(error!(ExpectedSemiAfterStmt), true)?;
            Ok(assign)
        }
    }

    pub(super) fn expect_comptime_statement(
        &mut self,
        comptime_token: TokenIndex,
    ) -> Result<node::Index> {
        let block_expr = self.parse_block_expr()?;
        if block_expr != 0 {
            return Ok(self.add_node(Node {
                tag: node!(Comptime),
                main_token: comptime_token,
                data: node::Data {
                    lhs: block_expr,
                    rhs: UNDEFINED_NODE,
                },
            }));
        }
        self.expect_var_decl_expr_statement(Some(comptime_token))
    }

    pub(super) fn expect_statement_recoverable(&mut self) -> Result<node::Index> {
        loop {
            return match self.expect_statement(true) {
                Ok(statement) => Ok(statement),
                Err(err) => {
                    assert!(matches!(err, ParseError));
                    self.find_next_stmt();
                    match self.token_tag(self.tok_i) {
                        token!(RBrace) => Ok(NULL_NODE),
                        token!(Eof) => Err(ParseError),
                        _ => continue,
                    }
                }
            };
        }
    }

    pub(super) fn parse_labeled_statement(&mut self) -> Result<node::Index> {
        let label_token = self.parse_block_label();
        let block = self.parse_block()?;
        if block != 0 {
            return Ok(block);
        }

        let loop_stmt = self.parse_loop_statement()?;
        if loop_stmt != 0 {
            return Ok(loop_stmt);
        }

        if label_token != 0 {
            let after_colon = self.tok_i;
            let node = self.parse_type_expr()?;
            if node != 0 {
                let a = self.parse_byte_align()?;
                let b = self.parse_addr_space()?;
                let c = self.parse_link_section()?;
                let d = match self.eat_token(token!(Equal)) {
                    None => 0,
                    Some(_) => self.expect_expr()?,
                };
                if a != 0 || b != 0 || c != 0 || d != 0 {
                    return self.fail_msg(Error {
                        tag: error!(ExpectedVarConst),
                        token: label_token,
                        ..Default::default()
                    });
                }
            }
            return self.fail_msg(Error {
                tag: error!(ExpectedLabelable),
                token: after_colon,
                ..Default::default()
            });
        }

        Ok(NULL_NODE)
    }

    pub(super) fn parse_loop_statement(&mut self) -> Result<node::Index> {
        let inline_token = self.eat_token(token!(KeywordInline));

        let for_statement = self.parse_for_statement()?;
        if for_statement != 0 {
            return Ok(for_statement);
        }

        let while_statement = self.parse_while_statement()?;
        if while_statement != 0 {
            return Ok(while_statement);
        }

        if inline_token.is_none() {
            return Ok(NULL_NODE);
        }

        self.fail(error!(ExpectedInlinable))
    }
}
