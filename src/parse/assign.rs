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
        if tok == T::Comma {
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
            T::AsteriskEqual => Some(N::AssignMul),
            T::SlashEqual => Some(N::AssignDiv),
            T::PercentEqual => Some(N::AssignMod),
            T::PlusEqual => Some(N::AssignAdd),
            T::MinusEqual => Some(N::AssignSub),
            T::AngleBracketAngleBracketLeftEqual => Some(N::AssignShl),
            T::AngleBracketAngleBracketLeftPipeEqual => Some(N::AssignShlSat),
            T::AngleBracketAngleBracketRightEqual => Some(N::AssignShr),
            T::AmpersandEqual => Some(N::AssignBitAnd),
            T::CaretEqual => Some(N::AssignBitXor),
            T::PipeEqual => Some(N::AssignBitOr),
            T::AsteriskPercentEqual => Some(N::AssignMulWrap),
            T::PlusPercentEqual => Some(N::AssignAddWrap),
            T::MinusPercentEqual => Some(N::AssignSubWrap),
            T::AsteriskPipeEqual => Some(N::AssignMulSat),
            T::PlusPipeEqual => Some(N::AssignAddSat),
            T::MinusPipeEqual => Some(N::AssignSubSat),
            T::Equal => Some(N::Assign),
            _ => None,
        }
    }

    pub(super) fn finish_assign_destructure_expr(
        &mut self,
        first_lhs: node::Index,
    ) -> Result<node::Index> {
        let mut scratch = Vec::new();

        scratch.push(first_lhs);

        while self.eat_token(T::Comma).is_some() {
            let expr = self.expect_expr()?;
            scratch.push(expr);
        }

        let equal_token = self.expect_token(T::Equal)?;

        let rhs = self.expect_expr()?;

        let lhs_count = scratch.len();
        assert!(lhs_count > 1);

        let extra_start = self.extra_data.len();
        self.extra_data.reserve(lhs_count + 1);
        self.extra_data.push(lhs_count as node::Index);
        self.extra_data.append(&mut scratch);

        Ok(self.add_node(Node {
            tag: N::AssignDestructure,
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
            return self.fail(E::ExpectedExprOrAssignment);
        }
        Ok(expr)
    }

    pub(super) fn expect_assign_expr(&mut self) -> Result<node::Index> {
        let expr = self.parse_assign_expr()?;
        if expr == 0 {
            return self.fail(E::ExpectedExprOrAssignment);
        }
        Ok(expr)
    }
}
