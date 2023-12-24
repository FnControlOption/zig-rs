use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_block_expr_statement(&mut self) -> Result<node::Index> {
        let block_expr = self.parse_block_expr()?;
        if block_expr != 0 {
            return Ok(block_expr);
        }
        let assign_expr = self.parse_assign_expr()?;
        if assign_expr != 0 {
            self.expect_semicolon(error!(ExpectedSemiAfterStmt), true)?;
            return Ok(assign_expr);
        }
        Ok(NULL_NODE)
    }

    pub(super) fn expect_block_expr_statement(&mut self) -> Result<node::Index> {
        let node = self.parse_block_expr_statement()?;
        if node == 0 {
            return self.fail(error!(ExpectedBlockOrExpr));
        }
        Ok(node)
    }

    pub(super) fn parse_block_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(Identifier) => {
                if self.token_tag(self.tok_i + 1) == token!(Colon)
                    && self.token_tag(self.tok_i + 2) == token!(LBrace)
                {
                    self.tok_i += 2;
                    self.parse_block()
                } else {
                    Ok(NULL_NODE)
                }
            }
            token!(LBrace) => self.parse_block(),
            _ => Ok(NULL_NODE),
        }
    }

    pub(super) fn parse_block(&mut self) -> Result<node::Index> {
        let Some(lbrace) = eat_token!(self, LBrace) else {
            return Ok(NULL_NODE);
        };
        let mut statements = Vec::new();
        loop {
            if self.token_tag(self.tok_i) == token!(RBrace) {
                break;
            }
            let statement = self.expect_statement_recoverable()?;
            if statement == 0 {
                break;
            }
            statements.push(statement);
        }
        expect_token!(self, RBrace)?;
        let semicolon = self.token_tag(self.tok_i - 2) == token!(Semicolon);
        match statements.len() {
            0 => add_node!(self, {
                tag: BlockTwo,
                main_token: lbrace,
                data: {
                    lhs: 0,
                    rhs: 0,
                }
            }),
            1 => add_node!(self, {
                tag: match semicolon {
                    true => node!(BlockTwoSemicolon),
                    false => node!(BlockTwo),
                },
                main_token: lbrace,
                data: {
                    lhs: statements[0],
                    rhs: 0,
                }
            }),
            2 => add_node!(self, {
                tag: match semicolon {
                    true => node!(BlockTwoSemicolon),
                    false => node!(BlockTwo),
                },
                main_token: lbrace,
                data: {
                    lhs: statements[0],
                    rhs: statements[1],
                }
            }),
            _ => {
                let span = self.list_to_span(&statements);
                add_node!(self, {
                    tag: match semicolon {
                        true => node!(BlockSemicolon),
                        false => node!(Block),
                    },
                    main_token: lbrace,
                    data: {
                        lhs: span.start,
                        rhs: span.end,
                    }
                })
            }
        }
    }

    pub(super) fn parse_block_label(&mut self) -> node::Index {
        if self.token_tag(self.tok_i) == token!(Identifier)
            && self.token_tag(self.tok_i + 1) == token!(Colon)
        {
            let identifier = self.tok_i;
            self.tok_i += 2;
            return identifier;
        }
        NULL_NODE
    }
}
