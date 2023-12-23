use super::*;

impl<'src, 'tok> Parser<'src, 'tok> {
    pub(super) fn parse_fn_proto(&mut self) -> Result<node::Index> {
        let Some(fn_token) = self.eat_token(token!(KeywordFn)) else {
            return Ok(NULL_NODE);
        };

        let fn_proto = self.add_node(Node {
            tag: node!(FnProto),
            main_token: UNDEFINED_TOKEN,
            data: node::Data {
                lhs: UNDEFINED_TOKEN,
                rhs: UNDEFINED_TOKEN,
            },
        });

        self.eat_token(token!(Identifier));
        let params = self.parse_param_decl_list()?;
        let align_expr = self.parse_byte_align()?;
        let addrspace_expr = self.parse_addr_space()?;
        let section_expr = self.parse_link_section()?;
        let callconv_expr = self.parse_callconv()?;
        self.eat_token(token!(Bang));

        let return_type_expr = self.parse_type_expr()?;
        if return_type_expr == 0 {
            self.warn(error!(ExpectedReturnType));
        }

        if align_expr == 0 && section_expr == 0 && callconv_expr == 0 {
            *self.node_mut(fn_proto) = match params {
                SmallSpan::ZeroOrOne(param) => Node {
                    tag: node!(FnProtoSimple),
                    main_token: fn_token,
                    data: node::Data {
                        lhs: param,
                        rhs: return_type_expr,
                    },
                },
                SmallSpan::Multi(span) => {
                    let lhs = self.add_extra(node::SubRange { ..span });
                    Node {
                        tag: node!(FnProtoMulti),
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
                    tag: node!(FnProtoOne),
                    main_token: fn_token,
                    data: node::Data {
                        lhs,
                        rhs: return_type_expr,
                    },
                }
            }
            SmallSpan::Multi(span) => {
                // TODO: impl ExtraData for node::FnProto
                let lhs = self.add_extra(node::FnProto {
                    params_start: span.start,
                    params_end: span.end,
                    align_expr,
                    addrspace_expr,
                    section_expr,
                    callconv_expr,
                });
                Node {
                    tag: node!(FnProto),
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
        todo!("expect_param_decl")
    }

    pub(super) fn parse_param_decl_list(&mut self) -> Result<SmallSpan> {
        self.expect_token(token!(LParen))?;
        let mut params = Vec::new();
        enum Varargs {
            None,
            Seen,
            Nonfinal(TokenIndex),
        }
        let mut varargs = Varargs::None;
        loop {
            if self.eat_token(token!(RParen)).is_some() {
                break;
            }
            todo!("parse_param_decl_list")
        }
        if let Varargs::Nonfinal(token) = varargs {
            self.warn_msg(Error {
                tag: error!(VarargsNonfinal),
                token,
                ..Default::default()
            });
        }
        Ok(match params.len() {
            0 => SmallSpan::ZeroOrOne(0),
            1 => SmallSpan::ZeroOrOne(params[0]),
            _ => SmallSpan::Multi(self.list_to_span(&params)),
        })
    }
}
