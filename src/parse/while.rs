use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_while_statement(&mut self) -> Result<node::Index> {
        let Some(for_token) = self.eat_token(token!(KeywordWhile)) else {
            return Ok(NULL_NODE);
        };
        todo!("parse_while_statement")
    }

    pub(super) fn parse_while_expr(&mut self) -> Result<node::Index> {
        todo!("parse_while_expr")
    }

    pub(super) fn parse_while_type_expr(&mut self) -> Result<node::Index> {
        todo!("parse_while_type_expr")
    }

    pub(super) fn parse_while_continue_expr(&mut self) -> Result<node::Index> {
        todo!("parse_while_continue_expr")
    }
}
