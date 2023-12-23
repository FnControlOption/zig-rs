use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_assign_expr(&mut self) -> Result<node::Index> {
        todo!("parse_assign_expr")
    }

    pub(super) fn parse_single_assign_expr(&mut self) -> Result<node::Index> {
        todo!("parse_single_assign_expr")
    }

    pub(super) fn finish_assign_expr(&mut self, lhs: node::Index) -> Result<node::Index> {
        todo!("finish_assign_expr")
    }

    pub(super) fn assign_op_node(tok: token::Tag) -> Option<node::Tag> {
        todo!("assign_op_node")
    }

    pub(super) fn finish_assign_destructure_expr(
        &mut self,
        first_lhs: node::Index,
    ) -> Result<node::Index> {
        todo!("finish_assign_destructure_expr")
    }

    pub(super) fn expect_single_assign_expr(&mut self) -> Result<node::Index> {
        todo!("expect_single_assign_expr")
    }

    pub(super) fn expect_assign_expr(&mut self) -> Result<node::Index> {
        todo!("expect_assign_expr")
    }
}
