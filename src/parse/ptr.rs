use super::*;

pub struct PtrModifiers {
    pub align_node: node::Index,
    pub addrspace_node: node::Index,
    pub bit_range_start: node::Index,
    pub bit_range_end: node::Index,
}

impl Parser<'_, '_> {
    pub(super) fn parse_ptr_payload(&mut self) -> Result<TokenIndex> {
        if self.eat_token(T::Pipe).is_none() {
            return Ok(NULL_NODE);
        }
        self.eat_token(T::Asterisk);
        let identifier = self.expect_token(T::Identifier)?;
        self.expect_token(T::Pipe)?;
        Ok(identifier)
    }

    pub(super) fn parse_ptr_index_payload(&mut self) -> Result<TokenIndex> {
        if self.eat_token(T::Pipe).is_none() {
            return Ok(NULL_NODE);
        }
        self.eat_token(T::Asterisk);
        let identifier = self.expect_token(T::Identifier)?;
        if self.eat_token(T::Comma).is_some() {
            self.expect_token(T::Identifier)?;
        }
        self.expect_token(T::Pipe)?;
        Ok(identifier)
    }

    pub(super) fn parse_ptr_modifiers(&mut self) -> Result<PtrModifiers> {
        let mut result = PtrModifiers {
            align_node: 0,
            addrspace_node: 0,
            bit_range_start: 0,
            bit_range_end: 0,
        };
        let mut saw_const = false;
        let mut saw_volatile = false;
        let mut saw_allowzero = false;
        loop {
            match self.token_tag(self.tok_i) {
                T::KeywordAlign => {
                    if result.align_node != 0 {
                        self.warn(E::ExtraAlignQualifier);
                    }
                    self.tok_i += 1;
                    self.expect_token(T::LParen)?;
                    result.align_node = self.expect_expr()?;

                    if self.eat_token(T::Colon).is_some() {
                        result.bit_range_start = self.expect_expr()?;
                        self.expect_token(T::Colon)?;
                        result.bit_range_start = self.expect_expr()?;
                    }

                    self.expect_token(T::RParen)?;
                }
                T::KeywordConst => {
                    if saw_const {
                        self.warn(E::ExtraConstQualifier);
                    }
                    self.tok_i += 1;
                    saw_const = true;
                }
                T::KeywordVolatile => {
                    if saw_volatile {
                        self.warn(E::ExtraVolatileQualifier);
                    }
                    self.tok_i += 1;
                    saw_volatile = true;
                }
                T::KeywordAllowzero => {
                    if saw_allowzero {
                        self.warn(E::ExtraAllowzeroQualifier);
                    }
                    self.tok_i += 1;
                    saw_allowzero = true;
                }
                T::KeywordAddrspace => {
                    if result.addrspace_node != 0 {
                        self.warn(E::ExtraAddrspaceQualifier);
                    }
                    result.addrspace_node = self.parse_addr_space()?;
                }
                _ => return Ok(result),
            }
        }
    }
}
