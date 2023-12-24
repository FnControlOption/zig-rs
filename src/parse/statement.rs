use super::*;

impl Parser<'_, '_> {
    pub(super) fn find_next_stmt(&mut self) {
        todo!("find_next_stmt")
    }

    pub(super) fn expect_statement(&mut self, allow_defer_var: bool) -> Result<node::Index> {
        if let Some(comptime_token) = eat_token!(self, KeywordComptime) {
            todo!("expect_statement")
        }

        match self.token_tag(self.tok_i) {
            token!(KeywordNosuspend) => todo!("expect_statement"),
            token!(KeywordSuspend) => todo!("expect_statement"),
            token!(KeywordDefer) => {
                if allow_defer_var {
                    let main_token = self.next_token();
                    let rhs = self.expect_block_expr_statement()?;
                    return Ok(self.add_node(Node {
                        tag: node!(Defer),
                        main_token,
                        data: node::Data {
                            lhs: UNDEFINED_NODE,
                            rhs,
                        },
                    }));
                }
            }
            token!(KeywordErrdefer) => todo!("expect_statement"),
            token!(KeywordSwitch) => todo!("expect_statement"),
            token!(KeywordIf) => todo!("expect_statement"),
            token!(KeywordEnum) | token!(KeywordStruct) | token!(KeywordUnion) => {
                let identifier = self.tok_i + 1;
                if self.parse_c_style_container()? {
                    return add_node!(self, {
                        tag: Identifier,
                        main_token: identifier,
                        data: {
                            lhs: UNDEFINED_NODE,
                            rhs: UNDEFINED_NODE,
                        }
                    });
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
            return add_node!(self, {
                tag: Comptime,
                main_token: comptime_token,
                data: { lhs: block_expr, rhs: UNDEFINED_NODE }
            });
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
