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

    pub(super) fn for_prefix(&mut self) -> Result<usize> {
        todo!("for_prefix")
    }

    pub(super) fn parse_for_type_expr(&mut self) -> Result<node::Index> {
        todo!("parse_for_type_expr")
    }
}
