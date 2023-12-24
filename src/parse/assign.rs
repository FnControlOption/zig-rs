use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_assign_expr(&mut self) -> Result<node::Index> {
        todo!("parse_assign_expr")
    }

    pub(super) fn parse_single_assign_expr(&mut self) -> Result<node::Index> {
        todo!("parse_single_assign_expr")
    }

    pub(super) fn finish_assign_expr(&mut self, lhs: node::Index) -> Result<node::Index> {
        let tok = self.token_tag(self.tok_i);
        if tok == token!(Comma) {
            return self.finish_assign_destructure_expr(lhs);
        }
        let Some(tag) = Self::assign_op_node(tok) else {
            return Ok(lhs);
        };
        let main_token = self.next_token();
        let rhs = self.expect_expr()?;
        Ok(self.add_node(Node {
            tag,
            main_token,
            data: node::Data { lhs, rhs },
        }))
    }

    pub(super) fn assign_op_node(tok: token::Tag) -> Option<node::Tag> {
        match tok {
            token!(AsteriskEqual) => Some(node!(AssignMul)),
            token!(SlashEqual) => Some(node!(AssignDiv)),
            token!(PercentEqual) => Some(node!(AssignMod)),
            token!(PlusEqual) => Some(node!(AssignAdd)),
            token!(MinusEqual) => Some(node!(AssignSub)),
            token!(AngleBracketAngleBracketLeftEqual) => Some(node!(AssignShl)),
            token!(AngleBracketAngleBracketLeftPipeEqual) => Some(node!(AssignShlSat)),
            token!(AngleBracketAngleBracketRightEqual) => Some(node!(AssignShr)),
            token!(AmpersandEqual) => Some(node!(AssignBitAnd)),
            token!(CaretEqual) => Some(node!(AssignBitXor)),
            token!(PipeEqual) => Some(node!(AssignBitOr)),
            token!(AsteriskPercentEqual) => Some(node!(AssignMulWrap)),
            token!(PlusPercentEqual) => Some(node!(AssignAddWrap)),
            token!(MinusPercentEqual) => Some(node!(AssignSubWrap)),
            token!(AsteriskPipeEqual) => Some(node!(AssignMulSat)),
            token!(PlusPipeEqual) => Some(node!(AssignAddSat)),
            token!(MinusPipeEqual) => Some(node!(AssignSubSat)),
            token!(Equal) => Some(node!(Assign)),
            _ => None,
        }
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
