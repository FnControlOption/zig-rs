use super::*;

pub struct Members {
    len: usize,
    lhs: node::Index,
    rhs: node::Index,
    trailing: bool,
}

impl Members {
    pub fn to_span(&self, p: &mut Parser) -> node::SubRange {
        if self.len <= 2 {
            let nodes = [self.lhs, self.rhs];
            p.list_to_span(&nodes[0..self.len])
        } else {
            node::SubRange {
                start: self.lhs,
                end: self.rhs,
            }
        }
    }
}

impl Parser<'_, '_> {
    pub(super) fn parse_container_members(&mut self) -> Members {
        let mut items = Vec::new();

        enum FieldState {
            None,
            Seen,
            End(node::Index),
            Err,
        }

        let mut field_state = FieldState::None;

        let mut last_field = UNDEFINED_TOKEN;

        while self.eat_token(token!(ContainerDocComment)).is_some() {}

        let mut trailing = false;
        loop {
            let doc_comment = self.eat_doc_comments();

            match self.token_tag(self.tok_i) {
                token!(KeywordTest) => {
                    if let Some(some) = doc_comment {
                        self.warn_msg(Error {
                            tag: error!(TestDocComment),
                            token: some,
                            ..Default::default()
                        });
                    }
                    let test_decl_node = self.expect_test_decl_recoverable();
                    if test_decl_node != 0 {
                        if matches!(field_state, FieldState::Seen) {
                            field_state = FieldState::End(test_decl_node);
                        }
                        items.push(test_decl_node);
                    }
                    trailing = false;
                }
                token!(KeywordComptime) => todo!("parse_container_members"),
                token!(KeywordPub) => {
                    self.tok_i += 1;
                    let top_level_decl = self.expect_top_level_decl_recoverable();
                    if top_level_decl != 0 {
                        if matches!(field_state, FieldState::Seen) {
                            field_state = FieldState::End(top_level_decl);
                        }
                        items.push(top_level_decl);
                    }
                    trailing = self.token_tag(self.tok_i - 1) == token!(Semicolon);
                }
                token!(KeywordUsingnamespace) => {
                    let node = self.expect_using_namespace_recoverable();
                    if node != 0 {
                        if matches!(field_state, FieldState::Seen) {
                            field_state = FieldState::End(node);
                        }
                        items.push(node);
                    }
                    trailing = self.token_tag(self.tok_i - 1) == token!(Semicolon);
                }
                token!(KeywordConst)
                | token!(KeywordVar)
                | token!(KeywordThreadlocal)
                | token!(KeywordExport)
                | token!(KeywordExtern)
                | token!(KeywordInline)
                | token!(KeywordNoinline)
                | token!(KeywordFn) => {
                    let top_level_decl = self.expect_top_level_decl_recoverable();
                    if top_level_decl != 0 {
                        if matches!(field_state, FieldState::Seen) {
                            field_state = FieldState::End(top_level_decl);
                        }
                        items.push(top_level_decl);
                    }
                    trailing = self.token_tag(self.tok_i - 1) == token!(Semicolon);
                }
                token!(Eof) | token!(RBrace) => {
                    if let Some(token) = doc_comment {
                        self.warn_msg(Error {
                            tag: error!(UnattachedDocComment),
                            token,
                            ..Default::default()
                        });
                    }
                    break;
                }
                _ => {
                    let c_container = self.parse_c_style_container().unwrap_or(false);
                    if c_container {
                        continue;
                    }

                    let identifier = self.tok_i;
                    let previous_field = last_field;
                    last_field = identifier;
                    let Ok(container_field) = self.expect_container_field() else {
                        self.find_next_container_member();
                        continue;
                    };
                    match field_state {
                        FieldState::None => field_state = FieldState::Seen,
                        FieldState::Err | FieldState::Seen => {}
                        FieldState::End(node) => {
                            self.warn_msg(Error {
                                tag: error!(DeclBetweenFields),
                                token: self.node(node).main_token,
                                ..Default::default()
                            });
                            self.warn_msg(Error {
                                tag: error!(PreviousField),
                                is_note: true,
                                token: previous_field,
                                ..Default::default()
                            });
                            self.warn_msg(Error {
                                tag: error!(NextField),
                                is_note: true,
                                token: identifier,
                                ..Default::default()
                            });
                            field_state = FieldState::Err;
                        }
                    }
                    items.push(container_field);
                    match self.token_tag(self.tok_i) {
                        token!(Comma) => {
                            self.tok_i += 1;
                            trailing = true;
                            continue;
                        }
                        token!(RBrace) | token!(Eof) => {
                            trailing = false;
                            break;
                        }
                        _ => {}
                    }
                    self.warn(error!(ExpectedCommaAfterField));
                    if self.token_tag(self.tok_i) == token!(Semicolon)
                        && self.token_tag(identifier) == token!(Identifier)
                    {
                        self.warn_msg(Error {
                            tag: error!(VarConstDecl),
                            is_note: true,
                            token: identifier,
                            ..Default::default()
                        });
                    }
                    self.find_next_container_member();
                    continue;
                }
            }
        }

        let len = items.len();
        match items[..] {
            [] => Members {
                len,
                lhs: 0,
                rhs: 0,
                trailing,
            },
            [lhs] => Members {
                len,
                lhs,
                rhs: 0,
                trailing,
            },
            [lhs, rhs] => Members {
                len,
                lhs,
                rhs,
                trailing,
            },
            [..] => {
                let span = self.list_to_span(&items);
                Members {
                    len,
                    lhs: span.start,
                    rhs: span.end,
                    trailing,
                }
            }
        }
    }

    pub(super) fn find_next_container_member(&mut self) {
        todo!("find_next_container_member")
    }

    pub(super) fn expect_test_decl(&mut self) -> Result<node::Index> {
        let test_token = self.assert_token(token!(KeywordTest));
        let name_token = match self.token_tag(self.tok_i) {
            token!(StringLiteral) | token!(Identifier) => Some(self.next_token()),
            _ => None,
        };
        let block_node = self.parse_block()?;
        if block_node == 0 {
            return self.fail(error!(ExpectedBlock));
        }
        Ok(self.add_node(Node {
            tag: node!(TestDecl),
            main_token: test_token,
            data: node::Data {
                lhs: name_token.unwrap_or(0),
                rhs: block_node,
            },
        }))
    }

    pub(super) fn expect_test_decl_recoverable(&mut self) -> node::Index {
        self.expect_test_decl().unwrap_or_else(|err| {
            assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }

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
                            rhs: UNDEFINED_NODE,
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

    pub(super) fn expect_using_namespace(&mut self) -> Result<node::Index> {
        let usingnamespace_token = self.assert_token(token!(KeywordUsingnamespace));
        let expr = self.expect_expr()?;
        self.expect_semicolon(error!(ExpectedSemiAfterDecl), false)?;
        Ok(self.add_node(Node {
            tag: node!(Usingnamespace),
            main_token: usingnamespace_token,
            data: node::Data {
                lhs: expr,
                rhs: UNDEFINED_NODE,
            },
        }))
    }

    pub(super) fn expect_using_namespace_recoverable(&mut self) -> node::Index {
        self.expect_using_namespace().unwrap_or_else(|err| {
            assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }

    pub(super) fn expect_container_field(&mut self) -> Result<node::Index> {
        let mut main_token = self.tok_i;
        self.eat_token(token!(KeywordComptime));
        let tuple_like = self.token_tag(self.tok_i) != token!(Identifier)
            || self.token_tag(self.tok_i + 1) != token!(Colon);
        if !tuple_like {
            main_token = self.assert_token(token!(Identifier));
        }

        let mut align_expr: node::Index = 0;
        let mut type_expr: node::Index = 0;
        if self.eat_token(token!(Colon)).is_some() || tuple_like {
            type_expr = self.expect_type_expr()?;
            align_expr = self.parse_byte_align()?;
        }

        let value_expr = match self.eat_token(token!(Equal)) {
            None => 0,
            Some(_) => self.expect_expr()?,
        };

        if align_expr == 0 {
            Ok(self.add_node(Node {
                tag: node!(ContainerFieldInit),
                main_token,
                data: node::Data {
                    lhs: type_expr,
                    rhs: value_expr,
                },
            }))
        } else if value_expr == 0 {
            Ok(self.add_node(Node {
                tag: node!(ContainerFieldAlign),
                main_token,
                data: node::Data {
                    lhs: type_expr,
                    rhs: align_expr,
                },
            }))
        } else {
            let rhs = self.add_extra(node::ContainerField {
                value_expr,
                align_expr,
            });
            Ok(self.add_node(Node {
                tag: node!(ContainerField),
                main_token,
                data: node::Data {
                    lhs: type_expr,
                    rhs,
                },
            }))
        }
    }

    pub(super) fn parse_c_style_container(&mut self) -> Result<bool> {
        let main_token = self.tok_i;
        match self.token_tag(self.tok_i) {
            token!(KeywordEnum) | token!(KeywordUnion) | token!(KeywordStruct) => {}
            _ => return Ok(false),
        }
        let identifier = self.tok_i + 1;
        if self.token_tag(identifier) != token!(Identifier) {
            return Ok(false);
        }
        self.tok_i += 2;

        self.warn_msg(Error {
            tag: error!(CStyleContainer(self.token_tag(main_token))),
            token: identifier,
            ..Default::default()
        });

        self.warn_msg(Error {
            tag: error!(ZigStyleContainer(self.token_tag(main_token))),
            is_note: true,
            token: identifier,
            ..Default::default()
        });

        self.expect_token(token!(LBrace))?;
        self.parse_container_members();
        self.expect_token(token!(RBrace))?;
        self.expect_semicolon(error!(ExpectedSemiAfterDecl), true)?;
        Ok(true)
    }

    pub(super) fn parse_container_decl_auto(&mut self) -> Result<node::Index> {
        let main_token = self.next_token();
        let arg_expr = match self.token_tag(main_token) {
            token!(KeywordOpaque) => NULL_NODE,
            token!(KeywordStruct) | token!(KeywordEnum) => match self.eat_token(token!(LParen)) {
                Some(_) => {
                    let expr = self.expect_expr()?;
                    self.expect_token(token!(RParen))?;
                    expr
                }
                None => NULL_NODE,
            },
            token!(KeywordUnion) => match self.eat_token(token!(LParen)) {
                Some(_) => todo!("parse_container_decl_auto"),
                None => NULL_NODE,
            },
            _ => {
                self.tok_i -= 1;
                return self.fail(error!(ExpectedContainer));
            }
        };
        self.expect_token(token!(LBrace))?;
        let members = self.parse_container_members();
        self.expect_token(token!(RBrace))?;

        if arg_expr == 0 {
            if members.len <= 2 {
                return Ok(self.add_node(Node {
                    tag: match members.trailing {
                        true => node!(ContainerDeclTwoTrailing),
                        false => node!(ContainerDeclTwo),
                    },
                    main_token,
                    data: node::Data {
                        lhs: members.lhs,
                        rhs: members.rhs,
                    },
                }));
            } else {
                let span = members.to_span(self);
                return Ok(self.add_node(Node {
                    tag: match members.trailing {
                        true => node!(ContainerDeclTrailing),
                        false => node!(ContainerDecl),
                    },
                    main_token,
                    data: node::Data {
                        lhs: span.start,
                        rhs: span.end,
                    },
                }));
            }
        } else {
            let span = members.to_span(self);
            let rhs = self.add_extra(node::SubRange { ..span });
            return Ok(self.add_node(Node {
                tag: match members.trailing {
                    true => node!(ContainerDeclArgTrailing),
                    false => node!(ContainerDeclArg),
                },
                main_token,
                data: node::Data { lhs: arg_expr, rhs },
            }));
        }
    }
}
