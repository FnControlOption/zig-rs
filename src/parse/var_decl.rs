use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_var_decl_proto(&mut self) -> Result<node::Index> {
        let Some(mut_token) = self
            .eat_token(token!(KeywordConst))
            .or_else(|| self.eat_token(token!(KeywordVar)))
        else {
            return Ok(NULL_NODE);
        };

        self.expect_token(token!(Identifier))?;
        let type_node = match self.eat_token(token!(Colon)) {
            None => 0,
            Some(_) => self.expect_type_expr()?,
        };
        let align_node = self.parse_byte_align()?;
        let addrspace_node = self.parse_addr_space()?;
        let section_node = self.parse_link_section()?;

        if section_node == 0 && addrspace_node == 0 {
            if align_node == 0 {
                return Ok(self.add_node(Node {
                    tag: node!(SimpleVarDecl),
                    main_token: mut_token,
                    data: node::Data {
                        lhs: type_node,
                        rhs: 0,
                    },
                }));
            }

            if type_node == 0 {
                return Ok(self.add_node(Node {
                    tag: node!(AlignedVarDecl),
                    main_token: mut_token,
                    data: node::Data {
                        lhs: align_node,
                        rhs: 0,
                    },
                }));
            }

            todo!("parse_var_decl_proto")
        } else {
            todo!("parse_var_decl_proto")
        }
    }

    pub(super) fn parse_global_var_decl(&mut self) -> Result<node::Index> {
        let var_decl = self.parse_var_decl_proto()?;
        if var_decl == 0 {
            return Ok(NULL_NODE);
        }

        let init_node = match self.token_tag(self.tok_i) {
            token!(EqualEqual) => {
                self.warn(error!(WrongEqualVarDecl));
                self.tok_i += 1;
                self.expect_expr()?
            }
            token!(Equal) => {
                self.tok_i += 1;
                self.expect_expr()?
            }
            _ => 0,
        };

        self.node_mut(var_decl).data.rhs = init_node;

        self.expect_semicolon(error!(ExpectedSemiAfterDecl), false)?;
        Ok(var_decl)
    }

    pub(super) fn expect_var_decl_expr_statement(
        &mut self,
        comptime_token: Option<TokenIndex>,
    ) -> Result<node::Index> {
        let mut lhs = Vec::new();

        loop {
            let var_decl_proto = self.parse_var_decl_proto()?;
            if var_decl_proto != 0 {
                lhs.push(var_decl_proto);
            } else {
                let expr = self.parse_expr()?;
                if expr == 0 {
                    if lhs.len() == 0 {
                        return self.fail(error!(ExpectedStatement));
                    } else {
                        return self.fail(error!(ExpectedExprOrVarDecl));
                    }
                }
                lhs.push(expr);
            }
            if eat_token!(self, Comma).is_none() {
                break;
            }
        }

        assert!(lhs.len() > 0);

        // let equal_token = match
        todo!("expect_var_decl_expr_statement")
    }
}
