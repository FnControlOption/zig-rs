use super::*;

impl Parser<'_, '_> {
    pub(super) fn expect_if_statement(&mut self) -> Result<node::Index> {
        todo!("expect_if_statement")
    }

    pub(super) fn parse_if_expr(&mut self) -> Result<node::Index> {
        self.parse_if(Self::expect_expr)
    }

    pub(super) fn parse_if(
        &mut self,
        body_parse_fn: impl FnMut(&mut Self) -> Result<node::Index>,
    ) -> Result<node::Index> {
        todo!("parse_if")
    }
}
