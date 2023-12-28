use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_assign_expr(&mut self) -> Result<node::Index> {
        let expr = self.parse_expr()?;
        if expr == 0 {
            return Ok(NULL_NODE);
        }
        self.finish_assign_expr(expr)
    }

    pub(super) fn parse_single_assign_expr(&mut self) -> Result<node::Index> {
        let lhs = self.parse_expr()?;
        if lhs == 0 {
            return Ok(NULL_NODE);
        }
        let Some(tag) = Self::assign_op_node(self.token_tag(self.tok_i)) else {
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
        let mut scratch = Vec::new();

        scratch.push(first_lhs);

        while self.eat_token(token!(Comma)).is_some() {
            let expr = self.expect_expr()?;
            scratch.push(expr);
        }

        let equal_token = self.expect_token(token!(Equal))?;

        let rhs = self.expect_expr()?;

        let lhs_count = scratch.len();
        assert!(lhs_count > 1);

        let extra_start = self.extra_data.len();
        self.extra_data.reserve(lhs_count + 1);
        self.extra_data.push(lhs_count as node::Index);
        self.extra_data.append(&mut scratch);

        Ok(self.add_node(Node {
            tag: node!(AssignDestructure),
            main_token: equal_token,
            data: node::Data {
                lhs: extra_start as node::Index,
                rhs,
            },
        }))
    }

    pub(super) fn expect_single_assign_expr(&mut self) -> Result<node::Index> {
        let expr = self.parse_single_assign_expr()?;
        if expr == 0 {
            return self.fail(error!(ExpectedExprOrAssignment));
        }
        Ok(expr)
    }

    pub(super) fn expect_assign_expr(&mut self) -> Result<node::Index> {
        let expr = self.parse_assign_expr()?;
        if expr == 0 {
            return self.fail(error!(ExpectedExprOrAssignment));
        }
        Ok(expr)
    }
}
