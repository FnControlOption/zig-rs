use super::*;

impl<'src, 'tok> Parser<'src, 'tok> {
    pub(super) fn expect_top_level_decl(&mut self) -> Result<node::Index> {
        let extern_export_inline_token = self.next_token();
        let mut is_extern = false;
        let mut expect_fn = false;
        let mut expect_var_or_fn = false;
        match self.token_tag(extern_export_inline_token) {
            token!(KeywordExtern) => {
                self.eat_token(token!(StringLiteral));
                is_extern = true;
                expect_var_or_fn = true;
            }
            token!(KeywordExport) => expect_var_or_fn = true,
            token!(KeywordInline) | token!(KeywordNoinline) => expect_fn = true,
            _ => self.tok_i -= 1,
        }
        let fn_proto = self.parse_fn_proto()?;
        if fn_proto != 0 {
            match self.token_tag(self.tok_i) {
                token!(Semicolon) => {
                    self.tok_i += 1;
                    return Ok(fn_proto);
                }
                token!(LBrace) => {
                    if is_extern {
                        self.warn_msg(Error {
                            tag: error!(ExternFnBody),
                            token: extern_export_inline_token,
                            ..Default::default()
                        });
                        return Ok(NULL_NODE);
                    }
                    let fn_decl = self.add_node(Node {
                        tag: node!(FnDecl),
                        main_token: self.node(fn_proto).main_token,
                        data: node::Data {
                            lhs: fn_proto,
                            rhs: UNDEFINED_TOKEN,
                        },
                    });
                    let body_block = self.parse_block()?;
                    assert!(body_block != 0);
                    self.node_mut(fn_decl).data.rhs = body_block;
                    return Ok(fn_decl);
                }
                _ => {
                    self.warn(error!(ExpectedSemiOrLBrace));
                    return Ok(NULL_NODE);
                }
            }
        }
        if expect_fn {
            self.warn(error!(ExpectedFn));
            return Err(ParseError);
        }

        let thread_local_token = self.eat_token(token!(KeywordThreadlocal));
        let var_decl = self.parse_global_var_decl()?;
        if var_decl != 0 {
            return Ok(var_decl);
        }
        if thread_local_token.is_some() {
            return self.fail(error!(ExpectedVarDecl));
        }
        if expect_var_or_fn {
            return self.fail(error!(ExpectedVarDeclOrFn));
        }
        if self.token_tag(self.tok_i) != token!(KeywordUsingnamespace) {
            return self.fail(error!(ExpectedPubItem));
        }
        return self.expect_using_namespace();
    }

    pub(super) fn expect_top_level_decl_recoverable(&mut self) -> node::Index {
        self.expect_top_level_decl().unwrap_or_else(|err| {
            assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }
}
