use super::*;

impl Parser<'_, '_> {
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

    pub(super) fn parse_break_label(&mut self) -> Result<TokenIndex> {
        match self.eat_token(token!(Colon)) {
            None => Ok(NULL_NODE),
            Some(_) => self.expect_token(token!(Identifier)),
        }
    }

    pub(super) fn parse_field_init(&mut self) -> Result<node::Index> {
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

    pub(super) fn expect_field_init(&mut self) -> Result<node::Index> {
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

    pub(super) fn parse_link_section(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordLinksection)).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(token!(LParen))?;
        let expr_node = self.expect_expr()?;
        self.expect_token(token!(RParen))?;
        Ok(expr_node)
    }

    pub(super) fn parse_callconv(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordCallconv)).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(token!(LParen))?;
        let expr_node = self.expect_expr()?;
        self.expect_token(token!(RParen))?;
        Ok(expr_node)
    }

    pub(super) fn parse_addr_space(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordAddrspace)).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(token!(LParen))?;
        let expr_node = self.expect_expr()?;
        self.expect_token(token!(RParen))?;
        Ok(expr_node)
    }

    pub(super) fn parse_payload(&mut self) -> Result<TokenIndex> {
        if self.eat_token(token!(Pipe)).is_none() {
            return Ok(NULL_NODE);
        }
        let identifier = self.expect_token(token!(Identifier))?;
        self.expect_token(token!(Pipe))?;
        Ok(identifier)
    }

    pub(super) fn parse_byte_align(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(KeywordAlign)).is_none() {
            return Ok(NULL_NODE);
        }
        self.expect_token(token!(LParen))?;
        let expr = self.expect_expr()?;
        self.expect_token(token!(RParen))?;
        Ok(expr)
    }
}
