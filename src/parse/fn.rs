use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_fn_proto(&mut self) -> Result<node::Index> {
        let Some(fn_token) = self.eat_token(T::KeywordFn) else {
            return Ok(NULL_NODE);
        };

        let fn_proto = self.add_node(Node {
            tag: N::FnProto,
            main_token: UNDEFINED_TOKEN,
            data: node::Data {
                lhs: UNDEFINED_NODE,
                rhs: UNDEFINED_NODE,
            },
        });

        self.eat_token(T::Identifier);
        let params = self.parse_param_decl_list()?;
        let align_expr = self.parse_byte_align()?;
        let addrspace_expr = self.parse_addr_space()?;
        let section_expr = self.parse_link_section()?;
        let callconv_expr = self.parse_callconv()?;
        self.eat_token(T::Bang);

        let return_type_expr = self.parse_type_expr()?;
        if return_type_expr == 0 {
            self.warn(E::ExpectedReturnType);
        }

        if align_expr == 0 && section_expr == 0 && callconv_expr == 0 && addrspace_expr == 0 {
            *self.node_mut(fn_proto) = match params {
                SmallSpan::ZeroOrOne(param) => Node {
                    tag: N::FnProtoSimple,
                    main_token: fn_token,
                    data: node::Data {
                        lhs: param,
                        rhs: return_type_expr,
                    },
                },
                SmallSpan::Multi(span) => {
                    let lhs = self.add_extra(span);
                    Node {
                        tag: N::FnProtoMulti,
                        main_token: fn_token,
                        data: node::Data {
                            lhs,
                            rhs: return_type_expr,
                        },
                    }
                }
            };
            return Ok(fn_proto);
        }
        *self.node_mut(fn_proto) = match params {
            SmallSpan::ZeroOrOne(param) => {
                let lhs = self.add_extra(node::FnProtoOne {
                    param,
                    align_expr,
                    addrspace_expr,
                    section_expr,
                    callconv_expr,
                });
                Node {
                    tag: N::FnProtoOne,
                    main_token: fn_token,
                    data: node::Data {
                        lhs,
                        rhs: return_type_expr,
                    },
                }
            }
            SmallSpan::Multi(span) => {
                let lhs = self.add_extra(node::FnProto {
                    params_start: span.start,
                    params_end: span.end,
                    align_expr,
                    addrspace_expr,
                    section_expr,
                    callconv_expr,
                });
                Node {
                    tag: N::FnProto,
                    main_token: fn_token,
                    data: node::Data {
                        lhs,
                        rhs: return_type_expr,
                    },
                }
            }
        };
        Ok(fn_proto)
    }

    pub(super) fn expect_param_decl(&mut self) -> Result<node::Index> {
        self.eat_doc_comments();
        match self.token_tag(self.tok_i) {
            T::KeywordNoalias | T::KeywordComptime => self.tok_i += 1,
            T::Ellipsis3 => {
                self.tok_i += 1;
                return Ok(NULL_NODE);
            }
            _ => {}
        }
        if self.token_tag(self.tok_i) == T::Identifier && self.token_tag(self.tok_i + 1) == T::Colon
        {
            self.tok_i += 2;
        }
        match self.token_tag(self.tok_i) {
            T::KeywordAnytype => {
                self.tok_i += 1;
                Ok(NULL_NODE)
            }
            _ => self.expect_type_expr(),
        }
    }

    pub(super) fn parse_param_decl_list(&mut self) -> Result<SmallSpan> {
        self.expect_token(T::LParen)?;
        let mut params = Vec::new();
        enum Varargs {
            None,
            Seen,
            Nonfinal(TokenIndex),
        }
        let mut varargs = Varargs::None;
        loop {
            if self.eat_token(T::RParen).is_some() {
                break;
            }
            if matches!(varargs, Varargs::Seen) {
                varargs = Varargs::Nonfinal(self.tok_i);
            }
            let param = self.expect_param_decl()?;
            if param != 0 {
                params.push(param);
            } else if self.token_tag(self.tok_i - 1) == T::Ellipsis3 {
                if matches!(varargs, Varargs::None) {
                    varargs = Varargs::Seen;
                }
            }
            match self.token_tag(self.tok_i) {
                T::Comma => self.tok_i += 1,
                T::RParen => {
                    self.tok_i += 1;
                    break;
                }
                T::Colon | T::RBrace | T::RBracket => return self.fail_expected(T::RParen),
                _ => self.warn(E::ExpectedCommaAfterParam),
            }
        }
        if let Varargs::Nonfinal(token) = varargs {
            self.warn_msg(Error::new(E::VarargsNonfinal, token));
        }
        Ok(match params[..] {
            [] => SmallSpan::ZeroOrOne(0),
            [param] => SmallSpan::ZeroOrOne(param),
            [..] => SmallSpan::Multi(self.list_to_span(&params)),
        })
    }
}
