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

        while self.eat_token(T::ContainerDocComment).is_some() {}

        let mut trailing = false;
        loop {
            let doc_comment = self.eat_doc_comments();

            match self.token_tag(self.tok_i) {
                T::KeywordTest => {
                    if let Some(some) = doc_comment {
                        self.warn_msg(Error::new(E::TestDocComment, some));
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
                T::KeywordComptime => match self.token_tag(self.tok_i + 1) {
                    T::LBrace => {
                        if let Some(some) = doc_comment {
                            self.warn_msg(Error::new(E::ComptimeDocComment, some));
                        }
                        let comptime_token = self.next_token();
                        let block = self.parse_block().unwrap_or_else(|err| {
                            debug_assert!(matches!(err, ParseError));
                            self.find_next_container_member();
                            NULL_NODE
                        });
                        if block != 0 {
                            let comptime_node = self.add_node(Node {
                                tag: N::Comptime,
                                main_token: comptime_token,
                                data: node::Data {
                                    lhs: block,
                                    rhs: UNDEFINED_NODE,
                                },
                            });
                            if matches!(field_state, FieldState::Seen) {
                                field_state = FieldState::End(comptime_node);
                            }
                            items.push(comptime_node);
                        }
                        trailing = false;
                    }
                    _ => {
                        let identifier = self.tok_i;
                        let Ok(container_field) = self.expect_container_field() else {
                            self.find_next_container_member();
                            continue;
                        };
                        match field_state {
                            FieldState::None => field_state = FieldState::Seen,
                            FieldState::Err | FieldState::Seen => {}
                            FieldState::End(node) => {
                                self.warn_msg(Error::new(
                                    E::DeclBetweenFields,
                                    self.node(node).main_token,
                                ));
                                self.warn_msg(Error::note(E::PreviousField, last_field));
                                self.warn_msg(Error::note(E::NextField, identifier));
                                field_state = FieldState::Err;
                            }
                        }
                        items.push(container_field);
                        match self.token_tag(self.tok_i) {
                            T::Comma => {
                                self.tok_i += 1;
                                trailing = true;
                                continue;
                            }
                            T::RBrace | T::Eof => {
                                trailing = false;
                                break;
                            }
                            _ => {}
                        }
                        self.warn(E::ExpectedCommaAfterField);
                        self.find_next_container_member();
                    }
                },
                T::KeywordPub => {
                    self.tok_i += 1;
                    let top_level_decl = self.expect_top_level_decl_recoverable();
                    if top_level_decl != 0 {
                        if matches!(field_state, FieldState::Seen) {
                            field_state = FieldState::End(top_level_decl);
                        }
                        items.push(top_level_decl);
                    }
                    trailing = self.token_tag(self.tok_i - 1) == T::Semicolon;
                }
                T::KeywordUsingnamespace => {
                    let node = self.expect_using_namespace_recoverable();
                    if node != 0 {
                        if matches!(field_state, FieldState::Seen) {
                            field_state = FieldState::End(node);
                        }
                        items.push(node);
                    }
                    trailing = self.token_tag(self.tok_i - 1) == T::Semicolon;
                }
                T::KeywordConst
                | T::KeywordVar
                | T::KeywordThreadlocal
                | T::KeywordExport
                | T::KeywordExtern
                | T::KeywordInline
                | T::KeywordNoinline
                | T::KeywordFn => {
                    let top_level_decl = self.expect_top_level_decl_recoverable();
                    if top_level_decl != 0 {
                        if matches!(field_state, FieldState::Seen) {
                            field_state = FieldState::End(top_level_decl);
                        }
                        items.push(top_level_decl);
                    }
                    trailing = self.token_tag(self.tok_i - 1) == T::Semicolon;
                }
                T::Eof | T::RBrace => {
                    if let Some(token) = doc_comment {
                        self.warn_msg(Error::new(E::UnattachedDocComment, token));
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
                            self.warn_msg(Error::new(
                                E::DeclBetweenFields,
                                self.node(node).main_token,
                            ));
                            self.warn_msg(Error::note(E::PreviousField, previous_field));
                            self.warn_msg(Error::note(E::NextField, identifier));
                            field_state = FieldState::Err;
                        }
                    }
                    items.push(container_field);
                    match self.token_tag(self.tok_i) {
                        T::Comma => {
                            self.tok_i += 1;
                            trailing = true;
                            continue;
                        }
                        T::RBrace | T::Eof => {
                            trailing = false;
                            break;
                        }
                        _ => {}
                    }
                    self.warn(E::ExpectedCommaAfterField);
                    if self.token_tag(self.tok_i) == T::Semicolon
                        && self.token_tag(identifier) == T::Identifier
                    {
                        self.warn_msg(Error::note(E::VarConstDecl, identifier));
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
        let mut level: u32 = 0;
        loop {
            let tok = self.next_token();
            match self.token_tag(tok) {
                T::KeywordTest
                | T::KeywordComptime
                | T::KeywordPub
                | T::KeywordExport
                | T::KeywordExtern
                | T::KeywordInline
                | T::KeywordNoinline
                | T::KeywordUsingnamespace
                | T::KeywordThreadlocal
                | T::KeywordConst
                | T::KeywordVar
                | T::KeywordFn => {
                    if level == 0 {
                        self.tok_i -= 1;
                        return;
                    }
                }
                T::Identifier => {
                    if self.token_tag(tok + 1) == T::Comma && level == 0 {
                        self.tok_i -= 1;
                        return;
                    }
                }
                T::Comma | T::Semicolon => {
                    if level == 0 {
                        return;
                    }
                }
                T::LParen | T::LBracket | T::LBrace => level += 1,
                T::RParen | T::RBracket => {
                    if level != 0 {
                        level -= 1;
                    }
                }
                T::RBrace => {
                    if level == 0 {
                        self.tok_i -= 1;
                        return;
                    }
                    level -= 1;
                }
                T::Eof => {
                    self.tok_i -= 1;
                    return;
                }
                _ => {}
            }
        }
    }

    pub(super) fn expect_test_decl(&mut self) -> Result<node::Index> {
        let test_token = self.assert_token(T::KeywordTest);
        let name_token = match self.token_tag(self.tok_i) {
            T::StringLiteral | T::Identifier => Some(self.next_token()),
            _ => None,
        };
        let block_node = self.parse_block()?;
        if block_node == 0 {
            return self.fail(E::ExpectedBlock);
        }
        Ok(self.add_node(Node {
            tag: N::TestDecl,
            main_token: test_token,
            data: node::Data {
                lhs: name_token.unwrap_or(0),
                rhs: block_node,
            },
        }))
    }

    pub(super) fn expect_test_decl_recoverable(&mut self) -> node::Index {
        self.expect_test_decl().unwrap_or_else(|err| {
            debug_assert!(matches!(err, ParseError));
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
            T::KeywordExtern => {
                self.eat_token(T::StringLiteral);
                is_extern = true;
                expect_var_or_fn = true;
            }
            T::KeywordExport => expect_var_or_fn = true,
            T::KeywordInline | T::KeywordNoinline => expect_fn = true,
            _ => self.tok_i -= 1,
        }
        let fn_proto = self.parse_fn_proto()?;
        if fn_proto != 0 {
            match self.token_tag(self.tok_i) {
                T::Semicolon => {
                    self.tok_i += 1;
                    return Ok(fn_proto);
                }
                T::LBrace => {
                    if is_extern {
                        self.warn_msg(Error::new(E::ExternFnBody, extern_export_inline_token));
                        return Ok(NULL_NODE);
                    }
                    let fn_decl = self.add_node(Node {
                        tag: N::FnDecl,
                        main_token: self.node(fn_proto).main_token,
                        data: node::Data {
                            lhs: fn_proto,
                            rhs: UNDEFINED_NODE,
                        },
                    });
                    let body_block = self.parse_block()?;
                    debug_assert!(body_block != 0);
                    self.node_mut(fn_decl).data.rhs = body_block;
                    return Ok(fn_decl);
                }
                _ => {
                    self.warn(E::ExpectedSemiOrLBrace);
                    return Ok(NULL_NODE);
                }
            }
        }
        if expect_fn {
            self.warn(E::ExpectedFn);
            return Err(ParseError);
        }

        let thread_local_token = self.eat_token(T::KeywordThreadlocal);
        let var_decl = self.parse_global_var_decl()?;
        if var_decl != 0 {
            return Ok(var_decl);
        }
        if thread_local_token.is_some() {
            return self.fail(E::ExpectedVarDecl);
        }
        if expect_var_or_fn {
            return self.fail(E::ExpectedVarDeclOrFn);
        }
        if self.token_tag(self.tok_i) != T::KeywordUsingnamespace {
            return self.fail(E::ExpectedPubItem);
        }
        return self.expect_using_namespace();
    }

    pub(super) fn expect_top_level_decl_recoverable(&mut self) -> node::Index {
        self.expect_top_level_decl().unwrap_or_else(|err| {
            debug_assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }

    pub(super) fn expect_using_namespace(&mut self) -> Result<node::Index> {
        let usingnamespace_token = self.assert_token(T::KeywordUsingnamespace);
        let expr = self.expect_expr()?;
        self.expect_semicolon(E::ExpectedSemiAfterDecl, false)?;
        Ok(self.add_node(Node {
            tag: N::Usingnamespace,
            main_token: usingnamespace_token,
            data: node::Data {
                lhs: expr,
                rhs: UNDEFINED_NODE,
            },
        }))
    }

    pub(super) fn expect_using_namespace_recoverable(&mut self) -> node::Index {
        self.expect_using_namespace().unwrap_or_else(|err| {
            debug_assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }

    pub(super) fn expect_container_field(&mut self) -> Result<node::Index> {
        let mut main_token = self.tok_i;
        self.eat_token(T::KeywordComptime);
        let tuple_like = self.token_tag(self.tok_i) != T::Identifier
            || self.token_tag(self.tok_i + 1) != T::Colon;
        if !tuple_like {
            main_token = self.assert_token(T::Identifier);
        }

        let mut align_expr: node::Index = 0;
        let mut type_expr: node::Index = 0;
        if self.eat_token(T::Colon).is_some() || tuple_like {
            type_expr = self.expect_type_expr()?;
            align_expr = self.parse_byte_align()?;
        }

        let value_expr = match self.eat_token(T::Equal) {
            None => 0,
            Some(_) => self.expect_expr()?,
        };

        if align_expr == 0 {
            Ok(self.add_node(Node {
                tag: N::ContainerFieldInit,
                main_token,
                data: node::Data {
                    lhs: type_expr,
                    rhs: value_expr,
                },
            }))
        } else if value_expr == 0 {
            Ok(self.add_node(Node {
                tag: N::ContainerFieldAlign,
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
                tag: N::ContainerField,
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
            T::KeywordEnum | T::KeywordUnion | T::KeywordStruct => {}
            _ => return Ok(false),
        }
        let identifier = self.tok_i + 1;
        if self.token_tag(identifier) != T::Identifier {
            return Ok(false);
        }
        self.tok_i += 2;

        self.warn_msg(Error::new(
            E::CStyleContainer(self.token_tag(main_token)),
            identifier,
        ));

        self.warn_msg(Error::note(
            E::ZigStyleContainer(self.token_tag(main_token)),
            identifier,
        ));

        self.expect_token(T::LBrace)?;
        self.parse_container_members();
        self.expect_token(T::RBrace)?;
        self.expect_semicolon(E::ExpectedSemiAfterDecl, true)?;
        Ok(true)
    }

    pub(super) fn parse_container_decl_auto(&mut self) -> Result<node::Index> {
        let main_token = self.next_token();
        let arg_expr = match self.token_tag(main_token) {
            T::KeywordOpaque => NULL_NODE,
            T::KeywordStruct | T::KeywordEnum => match self.eat_token(T::LParen) {
                Some(_) => {
                    let expr = self.expect_expr()?;
                    self.expect_token(T::RParen)?;
                    expr
                }
                None => NULL_NODE,
            },
            T::KeywordUnion => match self.eat_token(T::LParen) {
                Some(_) => match self.eat_token(T::KeywordEnum) {
                    Some(_) => match self.eat_token(T::LParen) {
                        Some(_) => {
                            let enum_tag_expr = self.expect_expr()?;
                            self.expect_token(T::RParen)?;
                            self.expect_token(T::RParen)?;

                            self.expect_token(T::LBrace)?;
                            let members = self.parse_container_members();
                            let members_span = members.to_span(self);
                            self.expect_token(T::RBrace)?;

                            let lhs = enum_tag_expr;
                            let rhs = self.add_extra(members_span);
                            return Ok(self.add_node(Node {
                                tag: match members.trailing {
                                    true => N::TaggedUnionEnumTagTrailing,
                                    false => N::TaggedUnionEnumTag,
                                },
                                main_token,
                                data: node::Data { lhs, rhs },
                            }));
                        }
                        None => {
                            self.expect_token(T::RParen)?;

                            self.expect_token(T::LBrace)?;
                            let members = self.parse_container_members();
                            self.expect_token(T::RBrace)?;
                            if members.len <= 2 {
                                return Ok(self.add_node(Node {
                                    tag: match members.trailing {
                                        true => N::TaggedUnionTwoTrailing,
                                        false => N::TaggedUnionTwo,
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
                                        true => N::TaggedUnionTrailing,
                                        false => N::TaggedUnion,
                                    },
                                    main_token,
                                    data: node::Data {
                                        lhs: span.start,
                                        rhs: span.end,
                                    },
                                }));
                            }
                        }
                    },
                    None => {
                        let expr = self.expect_expr()?;
                        self.expect_token(T::RParen)?;
                        expr
                    }
                },
                None => NULL_NODE,
            },
            _ => {
                self.tok_i -= 1;
                return self.fail(E::ExpectedContainer);
            }
        };
        self.expect_token(T::LBrace)?;
        let members = self.parse_container_members();
        self.expect_token(T::RBrace)?;

        if arg_expr == 0 {
            if members.len <= 2 {
                return Ok(self.add_node(Node {
                    tag: match members.trailing {
                        true => N::ContainerDeclTwoTrailing,
                        false => N::ContainerDeclTwo,
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
                        true => N::ContainerDeclTrailing,
                        false => N::ContainerDecl,
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
            let rhs = self.add_extra(span);
            return Ok(self.add_node(Node {
                tag: match members.trailing {
                    true => N::ContainerDeclArgTrailing,
                    false => N::ContainerDeclArg,
                },
                main_token,
                data: node::Data { lhs: arg_expr, rhs },
            }));
        }
    }
}
