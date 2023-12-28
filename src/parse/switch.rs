use super::*;

impl Parser<'_, '_> {
    pub(super) fn expect_switch_expr(&mut self) -> Result<node::Index> {
        let switch_token = self.assert_token(token!(KeywordSwitch));
        self.expect_token(token!(LParen))?;
        let expr_node = self.expect_expr()?;
        self.expect_token(token!(RParen))?;
        self.expect_token(token!(LBrace))?;
        let cases = self.parse_switch_prong_list()?;
        let trailing_comma = self.token_tag(self.tok_i - 1) == token!(Comma);
        self.expect_token(token!(RBrace))?;

        let rhs = self.add_extra(node::SubRange {
            start: cases.start,
            end: cases.end,
        });
        Ok(self.add_node(Node {
            tag: match trailing_comma {
                true => node!(SwitchComma),
                false => node!(Switch),
            },
            main_token: switch_token,
            data: node::Data {
                lhs: expr_node,
                rhs,
            },
        }))
    }

    pub(super) fn parse_switch_prong(&mut self) -> Result<node::Index> {
        let mut items = Vec::new();

        let is_inline = self.eat_token(token!(KeywordInline)).is_some();

        if self.eat_token(token!(KeywordElse)).is_none() {
            loop {
                let item = self.parse_switch_item()?;
                if item == 0 {
                    break;
                }
                items.push(item);
                if self.eat_token(token!(Comma)).is_none() {
                    break;
                }
            }
            if items.is_empty() {
                if is_inline {
                    self.tok_i -= 1;
                }
                return Ok(NULL_NODE);
            }
        }
        let arrow_token = self.expect_token(token!(EqualAngleBracketRight))?;
        self.parse_ptr_index_payload()?;

        match items[..] {
            [] => {
                let rhs = self.expect_single_assign_expr()?;
                Ok(self.add_node(Node {
                    tag: match is_inline {
                        true => node!(SwitchCaseInlineOne),
                        false => node!(SwitchCaseOne),
                    },
                    main_token: arrow_token,
                    data: node::Data { lhs: 0, rhs },
                }))
            }
            [lhs] => {
                let rhs = self.expect_single_assign_expr()?;
                Ok(self.add_node(Node {
                    tag: match is_inline {
                        true => node!(SwitchCaseInlineOne),
                        false => node!(SwitchCaseOne),
                    },
                    main_token: arrow_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            [..] => {
                let span = self.list_to_span(&items);
                let lhs = self.add_extra(span);
                let rhs = self.expect_single_assign_expr()?;
                Ok(self.add_node(Node {
                    tag: match is_inline {
                        true => node!(SwitchCaseInline),
                        false => node!(SwitchCase),
                    },
                    main_token: arrow_token,
                    data: node::Data { lhs, rhs },
                }))
            }
        }
    }

    pub(super) fn parse_switch_item(&mut self) -> Result<node::Index> {
        let expr = self.parse_expr()?;
        if expr == 0 {
            return Ok(NULL_NODE);
        }

        if let Some(token) = self.eat_token(token!(Ellipsis3)) {
            let rhs = self.expect_expr()?;
            return Ok(self.add_node(Node {
                tag: node!(SwitchRange),
                main_token: token,
                data: node::Data { lhs: expr, rhs },
            }));
        }
        Ok(expr)
    }

    pub(super) fn parse_switch_prong_list(&mut self) -> Result<node::SubRange> {
        let mut scratch = Vec::new();

        loop {
            let item = self.parse_switch_prong()?;
            if item == 0 {
                break;
            }

            scratch.push(item);

            match self.token_tag(self.tok_i) {
                token!(Comma) => self.tok_i += 1,
                token!(Colon) | token!(RParen) | token!(RBrace) | token!(RBracket) => break,
                _ => self.warn(error!(ExpectedCommaAfterSwitchProng)),
            }
        }
        Ok(self.list_to_span(&scratch))
    }
}
