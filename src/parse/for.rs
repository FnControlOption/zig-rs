use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_for_statement(&mut self) -> Result<node::Index> {
        let Some(for_token) = self.eat_token(T::KeywordFor) else {
            return Ok(NULL_NODE);
        };

        let mut scratch = Vec::new();
        let inputs = self.for_prefix(&mut scratch)?;

        let mut else_required = false;
        let mut seen_semicolon = false;
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
                seen_semicolon = true;
                break 'blk assign_expr;
            }
            else_required = true;
            assign_expr
        };
        let mut has_else = false;
        if !seen_semicolon && self.eat_token(T::KeywordElse).is_some() {
            scratch.push(then_expr);
            let else_stmt = self.expect_statement(false)?;
            scratch.push(else_stmt);
            has_else = true;
        } else if let [lhs] = scratch[..] {
            if else_required {
                self.warn(E::ExpectedSemiOrElse);
            }
            return Ok(self.add_node(Node {
                tag: N::ForSimple,
                main_token: for_token,
                data: node::Data {
                    lhs,
                    rhs: then_expr,
                },
            }));
        } else {
            if else_required {
                self.warn(E::ExpectedSemiOrElse);
            }
            scratch.push(then_expr);
        }
        let lhs = self.list_to_span(&scratch).start;
        let rhs = {
            let mut extra = node::For(UNDEFINED_NODE);
            extra.set_inputs(inputs as u32);
            extra.set_has_else(has_else);
            extra.0
        };
        Ok(self.add_node(Node {
            tag: N::For,
            main_token: for_token,
            data: node::Data { lhs, rhs },
        }))
    }

    pub(super) fn for_prefix(&mut self, scratch: &mut Vec<node::Index>) -> Result<usize> {
        let start = scratch.len();
        self.expect_token(T::LParen)?;

        loop {
            let mut input = self.expect_expr()?;
            if let Some(ellipsis) = self.eat_token(T::Ellipsis2) {
                let rhs = self.parse_expr()?;
                input = self.add_node(Node {
                    tag: N::ForRange,
                    main_token: ellipsis,
                    data: node::Data { lhs: input, rhs },
                });
            }

            scratch.push(input);
            match self.token_tag(self.tok_i) {
                T::Comma => self.tok_i += 1,
                T::RParen => {
                    self.tok_i += 1;
                    break;
                }
                T::Colon | T::RBrace | T::RBracket => return self.fail_expected(T::RParen),
                _ => self.warn(E::ExpectedCommaAfterForOperand),
            }
            if self.eat_token(T::RParen).is_some() {
                break;
            }
        }
        let inputs = scratch.len() - start;

        if self.eat_token(T::Pipe).is_none() {
            self.warn(E::ExpectedLoopPayload);
            return Ok(inputs);
        }

        let mut warned_excess = false;
        let mut captures: usize = 0;
        loop {
            self.eat_token(T::Asterisk);
            let identifier = self.expect_token(T::Identifier)?;
            captures += 1;
            if captures > inputs && !warned_excess {
                self.warn_msg(Error::new(E::ExtraForCapture, identifier));
                warned_excess = true;
            }
            match self.token_tag(self.tok_i) {
                T::Comma => self.tok_i += 1,
                T::Pipe => {
                    self.tok_i += 1;
                    break;
                }
                _ => self.warn(E::ExpectedCommaAfterCapture),
            }
            if self.eat_token(T::Pipe).is_some() {
                break;
            }
        }

        if captures < inputs {
            let index = scratch.len() - captures;
            let input = self.node(scratch[index]).main_token;
            self.warn_msg(Error::new(E::ForInputNotCaptured, input));
        }
        Ok(inputs)
    }

    pub(super) fn parse_for(
        &mut self,
        mut body_parse_fn: impl FnMut(&mut Self) -> Result<node::Index>,
    ) -> Result<node::Index> {
        let Some(for_token) = self.eat_token(T::KeywordFor) else {
            return Ok(NULL_NODE);
        };

        let mut scratch = Vec::new();
        let inputs = self.for_prefix(&mut scratch)?;

        let then_expr = body_parse_fn(self)?;
        let mut has_else = false;
        if self.eat_token(T::KeywordElse).is_some() {
            scratch.push(then_expr);
            let else_expr = body_parse_fn(self)?;
            scratch.push(else_expr);
            has_else = true;
        } else if let [lhs] = scratch[..] {
            return Ok(self.add_node(Node {
                tag: N::ForSimple,
                main_token: for_token,
                data: node::Data {
                    lhs,
                    rhs: then_expr,
                },
            }));
        } else {
            scratch.push(then_expr);
        }
        let lhs = self.list_to_span(&scratch).start;
        let rhs = {
            let mut extra = node::For(UNDEFINED_NODE);
            extra.set_inputs(inputs as u32);
            extra.set_has_else(has_else);
            extra.0
        };
        Ok(self.add_node(Node {
            tag: N::For,
            main_token: for_token,
            data: node::Data { lhs, rhs },
        }))
    }
}
