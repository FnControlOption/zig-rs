use crate::ast;
use crate::ast::{error, node, Error, Node, TokenIndex};
use crate::macros::*;
use crate::token;

mod misc;

mod asm;
mod assign;
mod block;
mod container;
mod expr;
mod r#fn;
mod r#for;
mod r#if;
mod ptr;
mod statement;
mod switch;
mod type_expr;
mod var_decl;
mod r#while;

pub struct ParseError;

pub type Result<T> = std::result::Result<T, ParseError>;

enum SmallSpan {
    ZeroOrOne(node::Index),
    Multi(node::SubRange),
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

impl Parser<'_, '_> {
    fn source(&self, range: std::ops::Range<ast::ByteOffset>) -> &[u8] {
        &self.source[range.start as usize..range.end as usize]
    }

    fn source_byte(&self, index: ast::ByteOffset) -> u8 {
        self.source[index as usize]
    }

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

impl Parser<'_, '_> {
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

    fn add_extra<const N: usize>(&mut self, extra: impl node::ExtraData<N>) -> node::Index {
        let result = self.extra_data.len() as u32;
        let data = extra.to_array();
        self.extra_data.extend_from_slice(&data);
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
            | error::Tag::ExpectedSemiOrLBrace
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
        panic!("{:?}", msg.tag);
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
        let s = self.source(self.token_start(token1)..self.token_start(token2));
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
