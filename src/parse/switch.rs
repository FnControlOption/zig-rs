use super::*;

impl Parser<'_, '_> {
    pub(super) fn expect_switch_expr(&mut self) -> Result<node::Index> {
        let switch_token = self.assert_token(T::KeywordSwitch);
        self.expect_token(T::LParen)?;
        let expr_node = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        self.expect_token(T::LBrace)?;
        let cases = self.parse_switch_prong_list()?;
        let trailing_comma = self.token_tag(self.tok_i - 1) == T::Comma;
        self.expect_token(T::RBrace)?;

        let rhs = self.add_extra(node::SubRange {
            start: cases.start,
            end: cases.end,
        });
        Ok(self.add_node(Node {
            tag: match trailing_comma {
                true => N::SwitchComma,
                false => N::Switch,
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

        let is_inline = self.eat_token(T::KeywordInline).is_some();

        if self.eat_token(T::KeywordElse).is_none() {
            loop {
                let item = self.parse_switch_item()?;
                if item == 0 {
                    break;
                }
                items.push(item);
                if self.eat_token(T::Comma).is_none() {
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
        let arrow_token = self.expect_token(T::EqualAngleBracketRight)?;
        self.parse_ptr_index_payload()?;

        match items[..] {
            [] => {
                let rhs = self.expect_single_assign_expr()?;
                Ok(self.add_node(Node {
                    tag: match is_inline {
                        true => N::SwitchCaseInlineOne,
                        false => N::SwitchCaseOne,
                    },
                    main_token: arrow_token,
                    data: node::Data { lhs: 0, rhs },
                }))
            }
            [lhs] => {
                let rhs = self.expect_single_assign_expr()?;
                Ok(self.add_node(Node {
                    tag: match is_inline {
                        true => N::SwitchCaseInlineOne,
                        false => N::SwitchCaseOne,
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
                        true => N::SwitchCaseInline,
                        false => N::SwitchCase,
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

        if let Some(token) = self.eat_token(T::Ellipsis3) {
            let rhs = self.expect_expr()?;
            return Ok(self.add_node(Node {
                tag: N::SwitchRange,
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
                T::Comma => self.tok_i += 1,
                T::Colon | T::RParen | T::RBrace | T::RBracket => break,
                _ => self.warn(E::ExpectedCommaAfterSwitchProng),
            }
        }
        Ok(self.list_to_span(&scratch))
    }
}
