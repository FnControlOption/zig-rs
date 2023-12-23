use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_block_expr_statement(&mut self) -> Result<node::Index> {
        todo!("parse_block_expr_statement")
    }

    pub(super) fn expect_block_expr_statement(&mut self) -> Result<node::Index> {
        todo!("expect_block_expr_statement")
    }

    pub(super) fn parse_block_expr(&mut self) -> Result<node::Index> {
        todo!("parse_block_expr")
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
