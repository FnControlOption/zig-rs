use std::cell::OnceCell;
use std::collections::HashMap;
use std::process::exit;
use std::sync::OnceLock;

use crate::ast;
use crate::ast::{error, node, Error, Node, TokenIndex};
use crate::token;

#[cfg(test)]
mod tests;

pub struct ParseError;

pub type Result<T> = std::result::Result<T, ParseError>;

enum SmallSpan {
    ZeroOrOne(node::Index),
    Multi(node::SubRange),
}

struct Members {
    len: usize,
    lhs: node::Index,
    rhs: node::Index,
    trailing: bool,
}

impl Members {
    fn to_span(&self, p: &mut Parser) -> node::SubRange {
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

pub struct Parser<'src, 'tok> {
    pub source: &'src [u8],
    pub token_tags: &'tok [token::Tag],
    pub token_starts: &'tok [ast::ByteOffset],
    pub tok_i: TokenIndex,
    pub errors: Vec<ast::Error>,
    pub nodes: Vec<Node>,
    pub extra_data: Vec<node::Index>,
}

impl<'src, 'tok> Parser<'src, 'tok> {
    fn token_tag(&self, index: TokenIndex) -> token::Tag {
        self.token_tags[index as usize]
    }

    fn token_start(&self, index: TokenIndex) -> ast::ByteOffset {
        self.token_starts[index as usize]
    }

    fn node(&self, index: node::Index) -> &Node {
        &self.nodes[index as usize]
    }

    fn node_mut(&mut self, index: node::Index) -> &mut Node {
        &mut self.nodes[index as usize]
    }

    /// For debugging purposes.
    fn dump_token(&self) {
        println!("{:?}", self.token_tag(self.tok_i));
    }
}

macro_rules! token {
    ($tag:ident) => {
        token::Tag::$tag
    };
}

macro_rules! node {
    ($tag:ident) => {
        node::Tag::$tag
    };
}

macro_rules! error {
    ($tag:ident) => {
        error::Tag::$tag
    };
}

macro_rules! add_node {
    ($p:ident, { tag: $tag:ident, main_token: $main_token:expr, data: { lhs: $lhs:expr, rhs: $rhs:expr $(,)? } $(,)? } $(,)?) => {
        add_node!($p, {
            tag: node!($tag),
            main_token: $main_token,
            data: {
                lhs: $lhs,
                rhs: $rhs,
            }
        })
    };
    ($p:ident, { tag: $tag:expr, main_token: $main_token:expr, data: { lhs: $lhs:expr, rhs: $rhs:expr $(,)? } $(,)? } $(,)?) => {
        Ok($p.add_node(Node {
            tag: $tag,
            main_token: $main_token,
            data: node::Data {
                lhs: $lhs,
                rhs: $rhs,
            },
        }))
    };
}

macro_rules! expect_token {
    ($p:ident, $tag:ident) => {
        $p.expect_token(token!($tag))
    };
}

macro_rules! eat_token {
    ($p:ident, $tag:ident) => {
        $p.eat_token(token!($tag))
    };
}

impl<'src, 'tok> Parser<'src, 'tok> {
    fn list_to_span(&mut self, list: &[node::Index]) -> node::SubRange {
        self.extra_data.extend_from_slice(list);
        node::SubRange {
            start: (self.extra_data.len() - list.len()) as node::Index,
            end: self.extra_data.len() as node::Index,
        }
    }

    fn add_node(&mut self, elem: Node) -> node::Index {
        let result = self.nodes.len() as node::Index;
        self.nodes.push(elem);
        result
    }

    fn set_node(&mut self, i: usize, elem: Node) -> node::Index {
        self.nodes[i] = elem;
        i as node::Index
    }

    fn reserve_node(&mut self, tag: node::Tag) -> usize {
        let result = self.nodes.len();
        self.nodes.push(Node {
            tag,
            main_token: UNDEFINED_TOKEN,
            data: node::Data {
                lhs: UNDEFINED_NODE,
                rhs: UNDEFINED_NODE,
            },
        });
        result
    }

    fn unreserve_node(&mut self, node_index: usize) {
        if self.nodes.len() == node_index {
            self.nodes.truncate(self.nodes.len() - 1);
        } else {
            self.nodes[node_index].tag = node!(UnreachableLiteral);
            self.nodes[node_index].main_token = self.tok_i;
        }
    }

    fn add_extra(&mut self, extra: &[node::Index]) -> node::Index {
        self.extra_data.reserve(extra.len());
        let result = self.extra_data.len() as u32;
        for &field in extra {
            self.extra_data.push(field);
        }
        result
    }

    #[cold]
    fn warn_expected(&mut self, expected_token: token::Tag) {
        self.warn_msg(Error {
            tag: error::Tag::ExpectedToken(expected_token),
            token: self.tok_i,
            ..Default::default()
        });
    }

    #[cold]
    fn warn(&mut self, error_tag: error::Tag) {
        self.warn_msg(Error {
            tag: error_tag,
            token: self.tok_i,
            ..Default::default()
        });
    }

    #[cold]
    fn warn_msg(&mut self, mut msg: Error) {
        match msg.tag {
            error::Tag::ExpectedSemiAfterDecl
            | error::Tag::ExpectedSemiAfterStmt
            | error::Tag::ExpectedCommaAfterField
            | error::Tag::ExpectedCommaAfterArg
            | error::Tag::ExpectedCommaAfterParam
            | error::Tag::ExpectedCommaAfterInitializer
            | error::Tag::ExpectedCommaAfterSwitchProng
            | error::Tag::ExpectedCommaAfterForOperand
            | error::Tag::ExpectedCommaAfterCapture
            | error::Tag::ExpectedSemiOrElse
            | error::Tag::ExpectedSemiOrLbrace
            | error::Tag::ExpectedToken(_)
            | error::Tag::ExpectedBlock
            | error::Tag::ExpectedBlockOrAssignment
            | error::Tag::ExpectedBlockOrExpr
            | error::Tag::ExpectedBlockOrField
            | error::Tag::ExpectedExpr
            | error::Tag::ExpectedExprOrAssignment
            | error::Tag::ExpectedFn
            | error::Tag::ExpectedInlinable
            | error::Tag::ExpectedLabelable
            | error::Tag::ExpectedParamList
            | error::Tag::ExpectedPrefixExpr
            | error::Tag::ExpectedPrimaryTypeExpr
            | error::Tag::ExpectedPubItem
            | error::Tag::ExpectedReturnType
            | error::Tag::ExpectedSuffixOp
            | error::Tag::ExpectedTypeExpr
            | error::Tag::ExpectedVarDecl
            | error::Tag::ExpectedVarDeclOrFn
            | error::Tag::ExpectedLoopPayload
            | error::Tag::ExpectedContainer => {
                if msg.token != 0 && !self.tokens_on_same_line(msg.token - 1, msg.token) {
                    msg.token_is_prev = true;
                    msg.token -= 1;
                }
            }
            _ => {}
        }
        self.errors.push(msg);
    }

    #[cold]
    fn fail<T>(&mut self, error_tag: error::Tag) -> Result<T> {
        self.fail_msg(Error {
            tag: error_tag,
            token: self.tok_i,
            ..Default::default()
        })
    }

    #[cold]
    fn fail_expected<T>(&mut self, expected_token: token::Tag) -> Result<T> {
        self.fail_msg(Error {
            tag: error::Tag::ExpectedToken(expected_token),
            token: self.tok_i,
            ..Default::default()
        })
    }

    #[cold]
    fn fail_msg<T>(&mut self, msg: Error) -> Result<T> {
        self.warn_msg(msg);
        Err(ParseError)
    }

    pub fn parse_root(&mut self) {
        self.nodes.push(Node {
            tag: node!(Root),
            main_token: 0,
            data: node::Data {
                lhs: UNDEFINED_NODE,
                rhs: UNDEFINED_NODE,
            },
        });
        let root_members = self.parse_container_members();
        let root_decls = root_members.to_span(self);
        if self.token_tag(self.tok_i) != token!(Eof) {
            self.warn_expected(token!(Eof));
        }
        self.nodes[0].data = node::Data {
            lhs: root_decls.start,
            rhs: root_decls.end,
        };
    }

    pub fn parse_zon(&mut self) {
        self.nodes.push(Node {
            tag: node!(Root),
            main_token: 0,
            data: node::Data {
                lhs: UNDEFINED_NODE,
                rhs: UNDEFINED_NODE,
            },
        });
        let Ok(node_index) = self.expect_expr() else {
            assert!(self.errors.len() > 0);
            return;
        };
        if self.token_tag(self.tok_i) != token!(Eof) {
            self.warn_expected(token!(Eof));
        }
        self.nodes[0].data = node::Data {
            lhs: node_index,
            rhs: UNDEFINED_NODE,
        };
    }

    fn parse_container_members(&mut self) -> Members {
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

    fn find_next_container_member(&mut self) {
        todo!("find_next_container_member")
    }

    fn expect_container_field(&mut self) -> Result<node::Index> {
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
            let rhs = self.add_extra(&[value_expr, align_expr]);
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

    fn parse_c_style_container(&mut self) -> Result<bool> {
        let main_token = self.tok_i;
        match self.token_tag(self.tok_i) {
            token!(KeywordEnum) | token!(KeywordUnion) | token!(KeywordStruct) => {}
            _ => return Ok(false),
        }
        todo!("parse_c_style_container")
    }

    fn expect_top_level_decl(&mut self) -> Result<node::Index> {
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
            todo!("expect_top_level_decl")
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

    fn expect_top_level_decl_recoverable(&mut self) -> node::Index {
        self.expect_top_level_decl().unwrap_or_else(|err| {
            assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }

    fn find_next_stmt(&mut self) {
        todo!("find_next_stmt")
    }

    fn expect_var_decl_expr_statement(
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

    fn expect_statement(&mut self, allow_defer_var: bool) -> Result<node::Index> {
        if let Some(comptime_token) = eat_token!(self, KeywordComptime) {
            todo!("expect_statement")
        }

        match self.token_tag(self.tok_i) {
            token!(KeywordNosuspend) => todo!("expect_statement"),
            token!(KeywordSuspend) => todo!("expect_statement"),
            token!(KeywordDefer) => todo!("expect_statement"),
            token!(KeywordErrdefer) => todo!("expect_statement"),
            token!(KeywordSwitch) => todo!("expect_statement"),
            token!(KeywordIf) => todo!("expect_statement"),
            token!(KeywordEnum) | token!(KeywordStruct) | token!(KeywordUnion) => {
                todo!("expect_statement")
            }
            _ => {}
        }

        let labeled_statement = self.parse_labeled_statement()?;
        if labeled_statement != 0 {
            return Ok(labeled_statement);
        }

        if allow_defer_var {
            self.expect_var_decl_expr_statement(None)
        } else {
            todo!("expect_statement")
        }
    }

    fn expect_statement_recoverable(&mut self) -> Result<node::Index> {
        loop {
            return match self.expect_statement(true) {
                Ok(statement) => Ok(statement),
                Err(err) => {
                    assert!(matches!(err, ParseError));
                    self.find_next_stmt();
                    match self.token_tag(self.tok_i) {
                        token!(RBrace) => Ok(NULL_NODE),
                        token!(Eof) => Err(ParseError),
                        _ => continue,
                    }
                }
            };
        }
    }

    fn parse_loop_statement(&mut self) -> Result<node::Index> {
        todo!("parse_loop_statement")
    }

    fn parse_labeled_statement(&mut self) -> Result<node::Index> {
        let label_token = self.parse_block_label();
        let block = self.parse_block()?;
        if block != 0 {
            return Ok(block);
        }

        let loop_stmt = self.parse_loop_statement()?;
        if loop_stmt != 0 {
            return Ok(loop_stmt);
        }

        if label_token != 0 {
            todo!("parse_labeled_statement")
        }

        Ok(NULL_NODE)
    }

    fn parse_block_label(&mut self) -> node::Index {
        if self.token_tag(self.tok_i) == token!(Identifier)
            && self.token_tag(self.tok_i + 1) == token!(Colon)
        {
            let identifier = self.tok_i;
            self.tok_i += 2;
            return identifier;
        }
        NULL_NODE
    }

    fn parse_block(&mut self) -> Result<node::Index> {
        let Some(lbrace) = eat_token!(self, LBrace) else {
            return Ok(NULL_NODE);
        };
        let mut statements = Vec::new();
        loop {
            if self.token_tag(self.tok_i) == token!(RBrace) {
                break;
            }
            let statement = self.expect_statement_recoverable()?;
            if statement == 0 {
                break;
            }
            statements.push(statement);
        }
        expect_token!(self, RBrace)?;
        let semicolon = self.token_tag(self.tok_i - 2) == token!(Semicolon);
        match statements.len() {
            0 => add_node!(self, {
                tag: BlockTwo,
                main_token: lbrace,
                data: {
                    lhs: 0,
                    rhs: 0,
                }
            }),
            1 => add_node!(self, {
                tag: match semicolon {
                    true => node!(BlockTwoSemicolon),
                    false => node!(BlockTwo),
                },
                main_token: lbrace,
                data: {
                    lhs: statements[0],
                    rhs: 0,
                }
            }),
            2 => add_node!(self, {
                tag: match semicolon {
                    true => node!(BlockTwoSemicolon),
                    false => node!(BlockTwo),
                },
                main_token: lbrace,
                data: {
                    lhs: statements[0],
                    rhs: statements[1],
                }
            }),
            _ => {
                let span = self.list_to_span(&statements);
                add_node!(self, {
                    tag: match semicolon {
                        true => node!(BlockSemicolon),
                        false => node!(Block),
                    },
                    main_token: lbrace,
                    data: {
                        lhs: span.start,
                        rhs: span.end,
                    }
                })
            }
        }
    }

    fn expect_test_decl(&mut self) -> Result<node::Index> {
        let test_token = self.assert_token(token!(KeywordTest));
        let name_token = match self.token_tag(self.tok_i) {
            token!(StringLiteral) | token!(Identifier) => Some(self.next_token()),
            _ => None,
        };
        let block_node = self.parse_block()?;
        if block_node == 0 {
            return self.fail(error!(ExpectedBlock));
        }
        add_node!(self, {
            tag: TestDecl,
            main_token: test_token,
            data: {
                lhs: name_token.unwrap_or(0),
                rhs: block_node,
            }
        })
    }

    fn expect_test_decl_recoverable(&mut self) -> node::Index {
        self.expect_test_decl().unwrap_or_else(|err| {
            assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }

    fn expect_using_namespace(&mut self) -> Result<node::Index> {
        todo!("expect_using_namespace")
    }

    fn expect_using_namespace_recoverable(&mut self) -> node::Index {
        self.expect_using_namespace().unwrap_or_else(|err| {
            assert!(matches!(err, ParseError));
            self.find_next_container_member();
            NULL_NODE
        })
    }

    fn parse_fn_proto(&mut self) -> Result<node::Index> {
        let Some(fn_token) = self.eat_token(token!(KeywordFn)) else {
            return Ok(NULL_NODE);
        };

        todo!("parse_fn_proto")
    }

    fn parse_var_decl_proto(&mut self) -> Result<node::Index> {
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

    fn parse_link_section(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordLinksection)).is_none() {
            return Ok(NULL_NODE);
        }

        todo!("parse_link_section")
    }

    fn parse_addr_space(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordAddrspace)).is_none() {
            return Ok(NULL_NODE);
        }

        todo!("parse_addr_space")
    }

    fn parse_byte_align(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordAlign)).is_none() {
            return Ok(NULL_NODE);
        }

        todo!("parse_byte_align")
    }

    fn parse_global_var_decl(&mut self) -> Result<node::Index> {
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

    fn parse_expr(&mut self) -> Result<node::Index> {
        self.parse_expr_precedence(0)
    }

    fn expect_expr(&mut self) -> Result<node::Index> {
        let node = self.parse_expr()?;
        if node == 0 {
            self.fail(error::Tag::ExpectedExpr)
        } else {
            Ok(node)
        }
    }

    fn parse_expr_precedence(&mut self, min_prec: i8) -> Result<node::Index> {
        enum Assoc {
            Left,
            None,
        }

        struct OperInfo {
            prec: i8,
            tag: node::Tag,
            assoc: Assoc,
        }

        macro_rules! op {
            (prec: $prec:literal, tag: $tag:ident, assoc: $assoc:ident) => {{
                let prec = $prec;
                let tag = node::Tag::$tag;
                let assoc = Assoc::$assoc;
                OperInfo { prec, tag, assoc }
            }};

            // op! { prec: -1, tag: Root } => OperInfo
            (prec: $prec:literal, tag: $tag:ident) => {
                op! { prec: $prec, tag: $tag, assoc: None }
            };

            // op! { AngleBracketLeft, prec: 30, tag: LessThan, assoc: None } => (token::Tag, OperInfo)
            ($key:ident, prec: $prec:literal, tag: $tag:ident, assoc: $assoc:ident) => {{
                let info = op! { prec: $prec, tag: $tag, assoc: $assoc };
                (token::Tag::$key, info)
            }};

            // op! { Plus, prec: 60, tag: Add } => (token::Tag, OperInfo)
            ($key:ident, prec: $prec:literal, tag: $tag:ident) => {
                op! { $key, prec: $prec, tag: $tag, assoc: None }
            };
        }

        const DEFAULT: OperInfo = op! { prec: -1, tag: Root };
        const ENTRIES: [(token::Tag, OperInfo); 30] = [
            // prec 10
            op! { KeywordOr, prec: 10, tag: BoolOr },
            // prec 20
            op! { KeywordAnd, prec: 20, tag: BoolAnd },
            // prec 30
            op! { EqualEqual, prec: 30, tag: EqualEqual, assoc: None },
            op! { BangEqual, prec: 30, tag: BangEqual, assoc: None },
            op! { AngleBracketLeft, prec: 30, tag: LessThan, assoc: None },
            op! { AngleBracketRight, prec: 30, tag: GreaterThan, assoc: None },
            op! { AngleBracketLeftEqual, prec: 30, tag: LessOrEqual, assoc: None },
            op! { AngleBracketRightEqual, prec: 30, tag: GreaterOrEqual, assoc: None },
            // prec 40
            op! { Ampersand, prec: 40, tag: BitAnd },
            op! { Caret, prec: 40, tag: BitXor },
            op! { Pipe, prec: 40, tag: BitOr },
            op! { KeywordOrelse, prec: 40, tag: Orelse },
            op! { KeywordCatch, prec: 40, tag: Catch },
            // prec 50
            op! { AngleBracketAngleBracketLeft, prec: 50, tag: Shl },
            op! { AngleBracketAngleBracketLeftPipe, prec: 50, tag: ShlSat },
            op! { AngleBracketAngleBracketRight, prec: 50, tag: Shr },
            // prec 60
            op! { Plus, prec: 60, tag: Add },
            op! { Minus, prec: 60, tag: Sub },
            op! { PlusPlus, prec: 60, tag: ArrayCat },
            op! { PlusPercent, prec: 60, tag: AddWrap },
            op! { MinusPercent, prec: 60, tag: SubWrap },
            op! { PlusPipe, prec: 60, tag: AddSat },
            op! { MinusPipe, prec: 60, tag: SubSat },
            // prec 70
            op! { PipePipe, prec: 70, tag: MergeErrorSets },
            op! { Asterisk, prec: 70, tag: Mul },
            op! { Slash, prec: 70, tag: Div },
            op! { Percent, prec: 70, tag: Mod },
            op! { AsteriskAsterisk, prec: 70, tag: ArrayMult },
            op! { AsteriskPercent, prec: 70, tag: MulWrap },
            op! { AsteriskPipe, prec: 70, tag: MulSat },
        ];

        fn oper_table() -> &'static HashMap<token::Tag, OperInfo> {
            // TODO: use LazyLock after it is stabilized
            static OPER_TABLE: OnceLock<HashMap<token::Tag, OperInfo>> = OnceLock::new();
            OPER_TABLE.get_or_init(|| {
                let mut map = HashMap::with_capacity(ENTRIES.len());
                for (tag, info) in ENTRIES {
                    map.insert(tag, info);
                }
                map
            })
        }

        assert!(min_prec >= 0);
        let mut node = self.parse_prefix_expr()?;
        if node == 0 {
            return Ok(NULL_NODE);
        }

        let mut banned_prec: i8 = -1;

        loop {
            let tok_tag = self.token_tag(self.tok_i);
            let info = oper_table().get(&tok_tag).unwrap_or(&DEFAULT);
            if info.prec < min_prec {
                break;
            }
            if info.prec == banned_prec {
                return self.fail(error::Tag::ChainedComparisonOperators);
            }

            let oper_token = self.next_token();
            if tok_tag == token!(KeywordCatch) {
                todo!("parse_expr_precedence");
            }

            let rhs = self.parse_expr_precedence(info.prec + 1)?;
            if rhs == 0 {
                self.warn(error::Tag::ExpectedExpr);
                return Ok(node);
            }

            {
                let tok_len = tok_tag.lexeme().unwrap().len();
                let byte_before = self.source[self.token_start(oper_token) as usize - 1];
                let byte_after = self.source[self.token_start(oper_token) as usize + tok_len];
                if tok_tag == token!(Ampersand) && byte_after == b'&' {
                    self.warn_msg(Error {
                        tag: error!(InvalidAmpersandAmpersand),
                        token: oper_token,
                        ..Default::default()
                    });
                } else if byte_before.is_ascii_whitespace() != byte_after.is_ascii_whitespace() {
                    self.warn_msg(Error {
                        tag: error!(MismatchedBinaryOpWhitespace),
                        token: oper_token,
                        ..Default::default()
                    });
                }
            }

            node = self.add_node(Node {
                tag: info.tag,
                main_token: oper_token,
                data: node::Data { lhs: node, rhs },
            });

            if matches!(info.assoc, Assoc::None) {
                banned_prec = info.prec;
            }
        }

        Ok(node)
    }

    fn parse_prefix_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(Bang)
            | token!(Minus)
            | token!(Tilde)
            | token!(MinusPercent)
            | token!(Ampersand)
            | token!(KeywordTry)
            | token!(KeywordAwait) => todo!("parse_prefix_expr"),
            _ => return self.parse_primary_expr(),
        }
    }

    fn parse_type_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(QuestionMark)
            | token!(KeywordAnyframe)
            | token!(Asterisk)
            | token!(AsteriskAsterisk)
            | token!(LBracket) => todo!("parse_type_expr"),
            _ => return self.parse_error_union_expr(),
        }
    }

    fn expect_type_expr(&mut self) -> Result<node::Index> {
        let node = self.parse_type_expr()?;
        if node == 0 {
            return self.fail(error!(ExpectedTypeExpr));
        }
        Ok(node)
    }

    fn parse_primary_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(KeywordAsm) => todo!("parse_primary_expr"),
            token!(KeywordIf) => todo!("parse_primary_expr"),
            token!(KeywordBreak) => todo!("parse_primary_expr"),
            token!(KeywordContinue) => todo!("parse_primary_expr"),
            token!(KeywordComptime) => todo!("parse_primary_expr"),
            token!(KeywordNosuspend) => todo!("parse_primary_expr"),
            token!(KeywordResume) => todo!("parse_primary_expr"),
            token!(KeywordReturn) => todo!("parse_primary_expr"),
            token!(Identifier) => {
                if self.token_tag(self.tok_i + 1) == token!(Colon) {
                    todo!("parse_primary_expr")
                } else {
                    self.parse_curly_suffix_expr()
                }
            }
            token!(KeywordInline) => todo!("parse_primary_expr"),
            token!(KeywordFor) => todo!("parse_primary_expr"),
            token!(KeywordWhile) => todo!("parse_primary_expr"),
            token!(LBrace) => self.parse_block(),
            _ => self.parse_curly_suffix_expr(),
        }
    }

    fn parse_curly_suffix_expr(&mut self) -> Result<node::Index> {
        let lhs = self.parse_type_expr()?;
        if lhs == 0 {
            return Ok(NULL_NODE);
        }
        let Some(lbrace) = self.eat_token(token!(LBrace)) else {
            return Ok(lhs);
        };
        todo!("parse_curly_suffix_expr")
    }

    fn parse_error_union_expr(&mut self) -> Result<node::Index> {
        let suffix_expr = self.parse_suffix_expr()?;
        if suffix_expr == 0 {
            return Ok(NULL_NODE);
        }
        let Some(bang) = self.eat_token(token!(Bang)) else {
            return Ok(suffix_expr);
        };
        todo!("parse_error_union_expr")
    }

    fn parse_suffix_expr(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordAsync)).is_some() {
            todo!("parse_suffix_expr");
        }

        let mut res = self.parse_primary_type_expr()?;
        if res == 0 {
            return Ok(res);
        }
        loop {
            let suffix_op = self.parse_suffix_op(res)?;
            if suffix_op != 0 {
                res = suffix_op;
                continue;
            }
            let Some(lparen) = self.eat_token(token!(LParen)) else {
                return Ok(res);
            };
            let mut params = Vec::new();
            loop {
                if eat_token!(self, RParen).is_some() {
                    break;
                }
                let param = self.expect_expr()?;
                params.push(param);
                match self.token_tag(self.tok_i) {
                    token!(Comma) => self.tok_i += 1,
                    token!(RParen) => {
                        self.tok_i += 1;
                        break;
                    }
                    token!(Colon) | token!(RBrace) | token!(RBracket) => {
                        return self.fail_expected(token!(RParen))
                    }
                    _ => self.warn(error!(ExpectedCommaAfterArg)),
                }
            }
            let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
            res = match params.len() {
                0 => self.add_node(Node {
                    tag: match comma {
                        true => node!(CallOneComma),
                        false => node!(CallOne),
                    },
                    main_token: lparen,
                    data: node::Data { lhs: res, rhs: 0 },
                }),
                1 => self.add_node(Node {
                    tag: match comma {
                        true => node!(CallOneComma),
                        false => node!(CallOne),
                    },
                    main_token: lparen,
                    data: node::Data {
                        lhs: res,
                        rhs: params[0],
                    },
                }),
                _ => {
                    let span = self.list_to_span(&params);
                    let rhs = self.add_extra(&[span.start, span.end]);
                    self.add_node(Node {
                        tag: match comma {
                            true => node!(CallComma),
                            false => node!(Call),
                        },
                        main_token: lparen,
                        data: node::Data { lhs: res, rhs: rhs },
                    })
                }
            }
        }
    }

    fn parse_container_decl_auto(&mut self) -> Result<node::Index> {
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
            let rhs = self.add_extra(&[span.start, span.end]);
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

    fn parse_primary_type_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(CharLiteral) => {
                let main_token = self.next_token();
                return Ok(self.add_node(Node {
                    tag: node!(CharLiteral),
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            }
            token!(NumberLiteral) => {
                let main_token = self.next_token();
                return Ok(self.add_node(Node {
                    tag: node!(NumberLiteral),
                    main_token,
                    data: node::Data {
                        lhs: UNDEFINED_NODE,
                        rhs: UNDEFINED_NODE,
                    },
                }));
            }
            token!(KeywordUnreachable)
            | token!(KeywordAnyframe)
            | token!(StringLiteral)
            | token!(Builtin)
            | token!(KeywordFn)
            | token!(KeywordIf)
            | token!(KeywordSwitch)
            | token!(KeywordExtern)
            | token!(KeywordPacked) => todo!("parse_primary_type_expr"),
            token!(KeywordStruct)
            | token!(KeywordOpaque)
            | token!(KeywordEnum)
            | token!(KeywordUnion) => return self.parse_container_decl_auto(),
            token!(KeywordComptime) => todo!("parse_primary_type_expr"),
            token!(MultilineStringLiteralLine) => todo!("parse_primary_type_expr"),
            token!(Identifier) => match self.token_tag(self.tok_i + 1) {
                token!(Colon) => todo!("parse_primary_type_expr"),
                _ => {
                    let main_token = self.next_token();
                    return add_node!(self, {
                        tag: Identifier,
                        main_token: main_token,
                        data: {
                            lhs: UNDEFINED_TOKEN,
                            rhs: UNDEFINED_TOKEN,
                        }
                    });
                }
            },
            token!(KeywordInline) => todo!("parse_primary_type_expr"),
            token!(KeywordFor) => todo!("parse_primary_type_expr"),
            token!(KeywordWhile) => todo!("parse_primary_type_expr"),
            token!(Period) => match self.token_tag(self.tok_i + 1) {
                token!(Identifier) => {
                    let lhs = self.next_token();
                    let main_token = self.next_token();
                    return Ok(self.add_node(Node {
                        tag: node!(EnumLiteral),
                        data: node::Data {
                            lhs,
                            rhs: UNDEFINED_NODE,
                        },
                        main_token,
                    }));
                }
                token!(LBrace) => {
                    let lbrace = self.tok_i + 1;
                    self.tok_i = lbrace + 1;

                    let mut inits = Vec::new();
                    let field_init = self.parse_field_init()?;
                    if field_init != 0 {
                        inits.push(field_init);
                        loop {
                            match self.token_tag(self.tok_i) {
                                token!(Comma) => self.tok_i += 1,
                                token!(RBrace) => {
                                    self.tok_i += 1;
                                    break;
                                }
                                token!(Colon) | token!(RParen) | token!(RBracket) => {
                                    return self.fail_expected(token!(RBrace))
                                }
                                _ => self.warn(error!(ExpectedCommaAfterInitializer)),
                            }
                            if self.eat_token(token!(RBrace)).is_some() {
                                break;
                            }
                            let next = self.expect_field_init()?;
                            inits.push(next);
                        }
                        let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
                        match inits.len() {
                            0 => unreachable!(),
                            1 => {
                                return Ok(self.add_node(Node {
                                    tag: match comma {
                                        true => node!(StructInitDotTwoComma),
                                        false => node!(StructInitDotTwo),
                                    },
                                    main_token: lbrace,
                                    data: node::Data {
                                        lhs: inits[0],
                                        rhs: 0,
                                    },
                                }))
                            }
                            2 | _ => todo!("parse_primary_type_expr"),
                        }
                    }

                    loop {
                        if self.eat_token(token!(RBrace)).is_some() {
                            break;
                        }
                        let elem_init = self.expect_expr()?;
                        inits.push(elem_init);
                        todo!("parse_primary_type_expr");
                    }
                    let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
                    match inits.len() {
                        0 => {
                            return Ok(self.add_node(Node {
                                tag: node!(StructInitDotTwo),
                                main_token: lbrace,
                                data: node::Data { lhs: 0, rhs: 0 },
                            }));
                        }
                        1 | 2 | _ => todo!("parse_primary_type_expr"),
                    }
                }
                _ => return Ok(NULL_NODE),
            },
            token!(KeywordError) => match self.token_tag(self.tok_i + 1) {
                token!(LBrace) => {
                    let error_token = self.tok_i;
                    self.tok_i += 2;
                    loop {
                        if self.eat_token(token!(RBrace)).is_some() {
                            break;
                        }
                        todo!("parse_primary_type_expr")
                    }
                    return add_node!(self, {
                        tag: ErrorSetDecl,
                        main_token: error_token,
                        data: {
                            lhs: UNDEFINED_TOKEN,
                            rhs: self.tok_i - 1,
                        },
                    });
                }
                _ => todo!("parse_primary_type_expr"),
            },
            token!(LParen) => todo!("parse_primary_type_expr"),
            _ => return Ok(NULL_NODE),
        }
    }

    fn parse_field_init(&mut self) -> Result<node::Index> {
        if self.token_tag(self.tok_i + 0) == token!(Period)
            && self.token_tag(self.tok_i + 1) == token!(Identifier)
            && self.token_tag(self.tok_i + 2) == token!(Equal)
        {
            self.tok_i += 3;
            self.expect_expr()
        } else {
            Ok(NULL_NODE)
        }
    }

    fn expect_field_init(&mut self) -> Result<node::Index> {
        if self.token_tag(self.tok_i + 0) == token!(Period)
            && self.token_tag(self.tok_i + 1) == token!(Identifier)
            && self.token_tag(self.tok_i + 2) == token!(Equal)
        {
            self.tok_i += 3;
            self.expect_expr()
        } else {
            self.fail(error!(ExpectedInitializer))
        }
    }

    fn parse_suffix_op(&mut self, lhs: node::Index) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(LBracket)
            | token!(PeriodAsterisk)
            | token!(InvalidPeriodAsterisks)
            | token!(Period) => todo!("parse_suffix_op"),
            _ => return Ok(NULL_NODE),
        }
    }

    fn eat_doc_comments(&mut self) -> Option<TokenIndex> {
        if let Some(tok) = self.eat_token(token!(DocComment)) {
            let mut first_line = tok;
            if tok > 0 && self.tokens_on_same_line(tok - 1, tok) {
                self.warn_msg(Error {
                    tag: error::Tag::SameLineDocComment,
                    token: tok,
                    ..Default::default()
                });
                // TODO: is there a better way to assign to `first_line` without needing `tmp`?
                let Some(tmp) = self.eat_token(token!(DocComment)) else {
                    return None;
                };
                first_line = tmp;
            }
            while self.eat_token(token!(DocComment)).is_some() {}
            return Some(first_line);
        }
        None
    }

    fn tokens_on_same_line(&self, token1: TokenIndex, token2: TokenIndex) -> bool {
        let s = &self.source[self.token_start(token1) as usize..self.token_start(token2) as usize];
        s.iter().position(|&c| c == b'\n').is_none()
    }

    fn eat_token(&mut self, tag: token::Tag) -> Option<TokenIndex> {
        if self.token_tag(self.tok_i) == tag {
            Some(self.next_token())
        } else {
            None
        }
    }

    fn assert_token(&mut self, tag: token::Tag) -> TokenIndex {
        let token = self.next_token();
        assert_eq!(self.token_tag(token), tag);
        token
    }

    fn expect_token(&mut self, tag: token::Tag) -> Result<TokenIndex> {
        if self.token_tag(self.tok_i) != tag {
            return self.fail_msg(Error {
                tag: error::Tag::ExpectedToken(tag),
                token: self.tok_i,
                ..Default::default()
            });
        }
        Ok(self.next_token())
    }

    fn expect_semicolon(&mut self, error_tag: error::Tag, recoverable: bool) -> Result<()> {
        if self.token_tag(self.tok_i) == token!(Semicolon) {
            self.next_token();
            return Ok(());
        }
        self.warn(error_tag);
        if !recoverable {
            return Err(ParseError);
        }
        Ok(())
    }

    fn next_token(&mut self) -> TokenIndex {
        let result = self.tok_i;
        self.tok_i += 1;
        result
    }
}

const NULL_NODE: node::Index = 0;
const UNDEFINED_NODE: node::Index = node::Index::MAX;
const UNDEFINED_TOKEN: TokenIndex = TokenIndex::MAX;
