use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_for_statement(&mut self) -> Result<node::Index> {
        let Some(for_token) = self.eat_token(token!(KeywordFor)) else {
            return Ok(NULL_NODE);
        };

        todo!("parse_for_statement")
    }

    pub(super) fn parse_for_expr(&mut self) -> Result<node::Index> {
        todo!("parse_for_expr")
    }

    pub(super) fn parse_for_type_expr(&mut self) -> Result<node::Index> {
        todo!("parse_for_type_expr")
    }

    pub(super) fn for_prefix(&mut self, scratch: &mut Vec<node::Index>) -> Result<usize> {
        let start = scratch.len();
        self.expect_token(token!(LParen))?;

        loop {
            let mut input = self.expect_expr()?;
            if let Some(ellipsis) = self.eat_token(token!(Ellipsis2)) {
                let rhs = self.parse_expr()?;
                input = self.add_node(Node {
                    tag: node!(ForRange),
                    main_token: ellipsis,
                    data: node::Data { lhs: input, rhs },
                });
            }

            scratch.push(input);
            match self.token_tag(self.tok_i) {
                token!(Comma) => self.tok_i += 1,
                token!(RParen) => {
                    self.tok_i += 1;
                    break;
                }
                token!(Colon) | token!(RBrace) | token!(RBracket) => {
                    return self.fail_expected(token!(RParen))
                }
                _ => self.warn(error!(ExpectedCommaAfterForOperand)),
            }
            if self.eat_token(token!(RParen)).is_some() {
                break;
            }
        }
        let inputs = scratch.len() - start;

        if self.eat_token(token!(Pipe)).is_none() {
            self.warn(error!(ExpectedLoopPayload));
            return Ok(inputs);
        }

        let mut warned_excess = false;
        let mut captures: usize = 0;
        loop {
            self.eat_token(token!(Asterisk));
            let identifier = self.expect_token(token!(Identifier))?;
            captures += 1;
            if captures > inputs && !warned_excess {
                self.warn_msg(Error {
                    tag: error!(ExtraForCapture),
                    token: identifier,
                    ..Default::default()
                });
                warned_excess = true;
            }
            match self.token_tag(self.tok_i) {
                token!(Comma) => self.tok_i += 1,
                token!(Pipe) => {
                    self.tok_i += 1;
                    break;
                }
                _ => self.warn(error!(ExpectedCommaAfterCapture)),
            }
            if self.eat_token(token!(Pipe)).is_some() {
                break;
            }
        }

        if captures < inputs {
            let index = scratch.len() - captures;
            let input = self.node(scratch[index]).main_token;
            self.warn_msg(Error {
                tag: error!(ForInputNotCaptured),
                token: input,
                ..Default::default()
            });
        }
        Ok(inputs)
    }
}
