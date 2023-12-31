use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_while_statement(&mut self) -> Result<node::Index> {
        let Some(while_token) = self.eat_token(T::KeywordWhile) else {
            return Ok(NULL_NODE);
        };
        self.expect_token(T::LParen)?;
        let condition = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        self.parse_ptr_payload()?;
        let cont_expr = self.parse_while_continue_expr()?;

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
                if cont_expr == 0 {
                    return Ok(self.add_node(Node {
                        tag: N::WhileSimple,
                        main_token: while_token,
                        data: node::Data {
                            lhs: condition,
                            rhs: assign_expr,
                        },
                    }));
                } else {
                    let lhs = condition;
                    let rhs = self.add_extra(node::WhileCont {
                        cont_expr,
                        then_expr: assign_expr,
                    });
                    return Ok(self.add_node(Node {
                        tag: N::WhileCont,
                        main_token: while_token,
                        data: node::Data { lhs, rhs },
                    }));
                }
            }
            else_required = true;
            assign_expr
        };
        if self.eat_token(T::KeywordElse).is_none() {
            if else_required {
                self.warn(E::ExpectedSemiOrElse);
            }
            if cont_expr == 0 {
                return Ok(self.add_node(Node {
                    tag: N::WhileSimple,
                    main_token: while_token,
                    data: node::Data {
                        lhs: condition,
                        rhs: then_expr,
                    },
                }));
            } else {
                let lhs = condition;
                let rhs = self.add_extra(node::WhileCont {
                    cont_expr,
                    then_expr,
                });
                return Ok(self.add_node(Node {
                    tag: N::WhileCont,
                    main_token: while_token,
                    data: node::Data { lhs, rhs },
                }));
            }
        }
        self.parse_payload()?;
        let else_expr = self.expect_statement(false)?;
        let rhs = self.add_extra(node::While {
            cont_expr,
            then_expr,
            else_expr,
        });
        Ok(self.add_node(Node {
            tag: N::While,
            main_token: while_token,
            data: node::Data {
                lhs: condition,
                rhs,
            },
        }))
    }

    pub(super) fn parse_while_expr(&mut self) -> Result<node::Index> {
        let Some(while_token) = self.eat_token(T::KeywordWhile) else {
            return Ok(NULL_NODE);
        };
        self.expect_token(T::LParen)?;
        let condition = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        self.parse_ptr_payload()?;
        let cont_expr = self.parse_while_continue_expr()?;

        let then_expr = self.expect_expr()?;
        if self.eat_token(T::KeywordElse).is_none() {
            if cont_expr == 0 {
                return Ok(self.add_node(Node {
                    tag: N::WhileSimple,
                    main_token: while_token,
                    data: node::Data {
                        lhs: condition,
                        rhs: then_expr,
                    },
                }));
            } else {
                let rhs = self.add_extra(node::WhileCont {
                    cont_expr,
                    then_expr,
                });
                return Ok(self.add_node(Node {
                    tag: N::WhileCont,
                    main_token: while_token,
                    data: node::Data {
                        lhs: condition,
                        rhs,
                    },
                }));
            }
        }
        self.parse_payload()?;
        let else_expr = self.expect_expr()?;
        let rhs = self.add_extra(node::While {
            cont_expr,
            then_expr,
            else_expr,
        });
        Ok(self.add_node(Node {
            tag: N::While,
            main_token: while_token,
            data: node::Data {
                lhs: condition,
                rhs,
            },
        }))
    }

    pub(super) fn parse_while_type_expr(&mut self) -> Result<node::Index> {
        let Some(while_token) = self.eat_token(T::KeywordWhile) else {
            return Ok(NULL_NODE);
        };
        self.expect_token(T::LParen)?;
        let condition = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        self.parse_ptr_payload()?;
        let cont_expr = self.parse_while_continue_expr()?;

        let then_expr = self.expect_type_expr()?;
        if self.eat_token(T::KeywordElse).is_none() {
            if cont_expr == 0 {
                return Ok(self.add_node(Node {
                    tag: N::WhileSimple,
                    main_token: while_token,
                    data: node::Data {
                        lhs: condition,
                        rhs: then_expr,
                    },
                }));
            } else {
                let rhs = self.add_extra(node::WhileCont {
                    cont_expr,
                    then_expr,
                });
                return Ok(self.add_node(Node {
                    tag: N::WhileCont,
                    main_token: while_token,
                    data: node::Data {
                        lhs: condition,
                        rhs,
                    },
                }));
            }
        }
        self.parse_payload()?;
        let else_expr = self.expect_type_expr()?;
        let rhs = self.add_extra(node::While {
            cont_expr,
            then_expr,
            else_expr,
        });
        Ok(self.add_node(Node {
            tag: N::While,
            main_token: while_token,
            data: node::Data {
                lhs: condition,
                rhs,
            },
        }))
    }

    pub(super) fn parse_while_continue_expr(&mut self) -> Result<node::Index> {
        if self.eat_token(T::Colon).is_none() {
            if self.token_tag(self.tok_i) == T::LParen
                && self.tokens_on_same_line(self.tok_i - 1, self.tok_i)
            {
                return self.fail(E::ExpectedContinueExpr);
            }
            return Ok(NULL_NODE);
        }
        self.expect_token(T::LParen)?;
        let node = self.parse_assign_expr()?;
        if node == 0 {
            return self.fail(E::ExpectedExprOrAssignment);
        }
        self.expect_token(T::RParen)?;
        Ok(node)
    }
}
