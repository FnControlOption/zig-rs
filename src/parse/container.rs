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

impl<'src, 'tok> Parser<'src, 'tok> {
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
                token!(KeywordPub) => todo!("parse_container_members"),
                token!(KeywordUsingnamespace) => todo!("parse_container_members"),
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

        match items.len() {
            0 => Members {
                len: 0,
                lhs: 0,
                rhs: 0,
                trailing,
            },
            1 => Members {
                len: 1,
                lhs: items[0],
                rhs: 0,
                trailing,
            },
            2 => Members {
                len: 2,
                lhs: items[0],
                rhs: items[1],
                trailing,
            },
            len => {
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
            add_node!(self, {
                tag: ContainerFieldInit,
                main_token: main_token,
                data: {
                    lhs: type_expr,
                    rhs: value_expr,
                }
            })
        } else if value_expr == 0 {
            add_node!(self, {
                tag: ContainerFieldAlign,
                main_token: main_token,
                data: {
                    lhs: type_expr,
                    rhs: align_expr,
                }
            })
        } else {
            let rhs = self.add_extra(node::ContainerField {
                value_expr,
                align_expr,
            });
            add_node!(self, {
                tag: ContainerField,
                main_token: main_token,
                data: {
                    lhs: type_expr,
                    rhs: rhs,
                }
            })
        }
    }

    pub(super) fn parse_c_style_container(&mut self) -> Result<bool> {
        let main_token = self.tok_i;
        match self.token_tag(self.tok_i) {
            token!(KeywordEnum) | token!(KeywordUnion) | token!(KeywordStruct) => {}
            _ => return Ok(false),
        }
        todo!("parse_c_style_container")
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
