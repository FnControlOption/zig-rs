use super::*;

impl Parser<'_, '_> {
    pub(super) fn expect_if_statement(&mut self) -> Result<node::Index> {
        let if_token = self.assert_token(T::KeywordIf);
        self.expect_token(T::LParen)?;
        let condition = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        self.parse_ptr_payload()?;

        let mut else_required = false;
        let then_expr = 'blk: {
            let block_expr = self.parse_block_expr()?;
            if block_expr != 0 {
                break 'blk block_expr;
            }
            let assign_expr = self.parse_assign_expr()?;
            if assign_expr == 0 {
                return self.fail(E::ExpectedBlockOrAssignment);
            }
            if self.eat_token(T::Semicolon).is_some() {
                return Ok(self.add_node(Node {
                    tag: N::IfSimple,
                    main_token: if_token,
                    data: node::Data {
                        lhs: condition,
                        rhs: assign_expr,
                    },
                }));
            }
            else_required = true;
            assign_expr
        };
        if self.eat_token(T::KeywordElse).is_none() {
            if else_required {
                self.warn(E::ExpectedSemiOrElse);
            }
            return Ok(self.add_node(Node {
                tag: N::IfSimple,
                main_token: if_token,
                data: node::Data {
                    lhs: condition,
                    rhs: then_expr,
                },
            }));
        }
        self.parse_payload()?;
        let else_expr = self.expect_statement(false)?;
        let lhs = condition;
        let rhs = self.add_extra(node::If {
            then_expr,
            else_expr,
        });
        Ok(self.add_node(Node {
            tag: N::If,
            main_token: if_token,
            data: node::Data { lhs, rhs },
        }))
    }

    pub(super) fn parse_if_expr(&mut self) -> Result<node::Index> {
        self.parse_if(Self::expect_expr)
    }

    pub(super) fn parse_if(
        &mut self,
        mut body_parse_fn: impl FnMut(&mut Self) -> Result<node::Index>,
    ) -> Result<node::Index> {
        let Some(if_token) = self.eat_token(T::KeywordIf) else {
            return Ok(NULL_NODE);
        };
        self.expect_token(T::LParen)?;
        let condition = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        self.parse_ptr_payload()?;

        let then_expr = body_parse_fn(self)?;
        assert!(then_expr != 0);

        if self.eat_token(T::KeywordElse).is_none() {
            return Ok(self.add_node(Node {
                tag: N::IfSimple,
                main_token: if_token,
                data: node::Data {
                    lhs: condition,
                    rhs: then_expr,
                },
            }));
        }
        self.parse_payload()?;
        let else_expr = body_parse_fn(self)?;
        assert!(else_expr != 0);

        let lhs = condition;
        let rhs = self.add_extra(node::If {
            then_expr,
            else_expr,
        });
        Ok(self.add_node(Node {
            tag: N::If,
            main_token: if_token,
            data: node::Data { lhs, rhs },
        }))
    }
}
