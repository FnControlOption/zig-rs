use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_var_decl_proto(&mut self) -> Result<node::Index> {
        let Some(mut_token) = self
            .eat_token(T::KeywordConst)
            .or_else(|| self.eat_token(T::KeywordVar))
        else {
            return Ok(NULL_NODE);
        };

        self.expect_token(T::Identifier)?;
        let type_node = match self.eat_token(T::Colon) {
            None => 0,
            Some(_) => self.expect_type_expr()?,
        };
        let align_node = self.parse_byte_align()?;
        let addrspace_node = self.parse_addr_space()?;
        let section_node = self.parse_link_section()?;

        if section_node == 0 && addrspace_node == 0 {
            if align_node == 0 {
                return Ok(self.add_node(Node {
                    tag: N::SimpleVarDecl,
                    main_token: mut_token,
                    data: node::Data {
                        lhs: type_node,
                        rhs: 0,
                    },
                }));
            }

            if type_node == 0 {
                return Ok(self.add_node(Node {
                    tag: N::AlignedVarDecl,
                    main_token: mut_token,
                    data: node::Data {
                        lhs: align_node,
                        rhs: 0,
                    },
                }));
            }

            let lhs = self.add_extra(node::LocalVarDecl {
                type_node,
                align_node,
            });
            Ok(self.add_node(Node {
                tag: N::LocalVarDecl,
                main_token: mut_token,
                data: node::Data { lhs, rhs: 0 },
            }))
        } else {
            let lhs = self.add_extra(node::GlobalVarDecl {
                type_node,
                align_node,
                addrspace_node,
                section_node,
            });
            Ok(self.add_node(Node {
                tag: N::GlobalVarDecl,
                main_token: mut_token,
                data: node::Data { lhs, rhs: 0 },
            }))
        }
    }

    pub(super) fn parse_global_var_decl(&mut self) -> Result<node::Index> {
        let var_decl = self.parse_var_decl_proto()?;
        if var_decl == 0 {
            return Ok(NULL_NODE);
        }

        let init_node = match self.token_tag(self.tok_i) {
            T::EqualEqual => {
                self.warn(E::WrongEqualVarDecl);
                self.tok_i += 1;
                self.expect_expr()?
            }
            T::Equal => {
                self.tok_i += 1;
                self.expect_expr()?
            }
            _ => 0,
        };

        self.node_mut(var_decl).data.rhs = init_node;

        self.expect_semicolon(E::ExpectedSemiAfterDecl, false)?;
        Ok(var_decl)
    }

    pub(super) fn expect_var_decl_expr_statement(
        &mut self,
        comptime_token: Option<TokenIndex>,
    ) -> Result<node::Index> {
        let mut scratch = Vec::new();

        loop {
            let var_decl_proto = self.parse_var_decl_proto()?;
            if var_decl_proto != 0 {
                scratch.push(var_decl_proto);
            } else {
                let expr = self.parse_expr()?;
                if expr == 0 {
                    if scratch.len() == 0 {
                        return self.fail(E::ExpectedStatement);
                    } else {
                        return self.fail(E::ExpectedExprOrVarDecl);
                    }
                }
                scratch.push(expr);
            }
            if self.eat_token(T::Comma).is_none() {
                break;
            }
        }

        let lhs_count = scratch.len();
        debug_assert!(lhs_count > 0);

        let equal_token = match self.eat_token(T::Equal) {
            Some(token) => token,
            None => 'eql: {
                let [lhs] = scratch[..] else {
                    if let Some(tok) = self.eat_token(T::EqualEqual) {
                        self.warn_msg(Error::new(E::WrongEqualVarDecl, tok));
                        break 'eql tok;
                    }
                    return self.fail_expected(T::Equal);
                };
                match self.node(lhs).tag {
                    N::GlobalVarDecl | N::LocalVarDecl | N::SimpleVarDecl | N::AlignedVarDecl => {
                        if let Some(tok) = self.eat_token(T::EqualEqual) {
                            self.warn_msg(Error::new(E::WrongEqualVarDecl, tok));
                            break 'eql tok;
                        }
                        return self.fail_expected(T::Equal);
                    }
                    _ => {}
                }

                let expr = self.finish_assign_expr(lhs)?;
                self.expect_semicolon(E::ExpectedSemiAfterStmt, true)?;
                if let Some(t) = comptime_token {
                    return Ok(self.add_node(Node {
                        tag: N::Comptime,
                        main_token: t,
                        data: node::Data {
                            lhs: expr,
                            rhs: UNDEFINED_NODE,
                        },
                    }));
                } else {
                    return Ok(expr);
                }
            }
        };

        let rhs = self.expect_expr()?;
        self.expect_semicolon(E::ExpectedSemiAfterStmt, true)?;

        if let [lhs] = scratch[..] {
            match self.node(lhs).tag {
                N::GlobalVarDecl | N::LocalVarDecl | N::SimpleVarDecl | N::AlignedVarDecl => {
                    self.node_mut(lhs).data.rhs = rhs;
                    return Ok(lhs);
                }
                _ => {}
            }
            let expr = self.add_node(Node {
                tag: N::Assign,
                main_token: equal_token,
                data: node::Data { lhs, rhs },
            });
            if let Some(t) = comptime_token {
                return Ok(self.add_node(Node {
                    tag: N::Comptime,
                    main_token: t,
                    data: node::Data {
                        lhs: expr,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            } else {
                return Ok(expr);
            }
        }

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
}
