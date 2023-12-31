use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_field_init(&mut self) -> Result<node::Index> {
        if self.token_tag(self.tok_i + 0) == T::Period
            && self.token_tag(self.tok_i + 1) == T::Identifier
            && self.token_tag(self.tok_i + 2) == T::Equal
        {
            self.tok_i += 3;
            self.expect_expr()
        } else {
            Ok(NULL_NODE)
        }
    }

    pub(super) fn expect_field_init(&mut self) -> Result<node::Index> {
        if self.token_tag(self.tok_i + 0) == T::Period
            && self.token_tag(self.tok_i + 1) == T::Identifier
            && self.token_tag(self.tok_i + 2) == T::Equal
        {
            self.tok_i += 3;
            self.expect_expr()
        } else {
            self.fail(E::ExpectedInitializer)
        }
    }

    pub(super) fn parse_link_section(&mut self) -> Result<node::Index> {
        if self.eat_token(T::KeywordLinksection).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(T::LParen)?;
        let expr_node = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        Ok(expr_node)
    }

    pub(super) fn parse_callconv(&mut self) -> Result<node::Index> {
        if self.eat_token(T::KeywordCallconv).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(T::LParen)?;
        let expr_node = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        Ok(expr_node)
    }

    pub(super) fn parse_addr_space(&mut self) -> Result<node::Index> {
        if self.eat_token(T::KeywordAddrspace).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(T::LParen)?;
        let expr_node = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        Ok(expr_node)
    }

    pub(super) fn parse_payload(&mut self) -> Result<TokenIndex> {
        if self.eat_token(T::Pipe).is_none() {
            return Ok(NULL_NODE);
        }
        let identifier = self.expect_token(T::Identifier)?;
        self.expect_token(T::Pipe)?;
        Ok(identifier)
    }

    pub(super) fn parse_byte_align(&mut self) -> Result<node::Index> {
        if self.eat_token(T::KeywordAlign).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(T::LParen)?;
        let expr = self.expect_expr()?;
        self.expect_token(T::RParen)?;
        Ok(expr)
    }
}
