use super::*;

impl<'src, 'tok> Parser<'src, 'tok> {
    pub(super) fn expect_if_statement(&mut self) -> Result<node::Index> {
        todo!("expect_if_statement")
    }

    pub(super) fn parse_if_expr(&mut self) -> Result<node::Index> {
        todo!("parse_if_expr")
    }

    pub(super) fn parse_if<F>(&mut self, body_parse_fn: F) -> Result<node::Index>
    where
        F: FnMut() -> Result<node::Index>,
    {
        todo!("parse_if")
    }
}
