use super::*;

impl Parser<'_, '_> {
    pub(super) fn expect_asm_expr(&mut self) -> Result<node::Index> {
        let asm_token = self.assert_token(token!(KeywordAsm));
        self.eat_token(token!(KeywordVolatile));
        self.expect_token(token!(LParen))?;
        let template = self.expect_expr()?;

        if let Some(rparen) = self.eat_token(token!(RParen)) {
            return Ok(self.add_node(Node {
                tag: node!(AsmSimple),
                main_token: asm_token,
                data: node::Data {
                    lhs: template,
                    rhs: rparen,
                },
            }));
        }

        self.expect_token(token!(Colon))?;

        let mut scratch = Vec::new();

        loop {
            let output_item = self.parse_asm_output_item()?;
            if output_item == 0 {
                break;
            }
            scratch.push(output_item);
            match self.token_tag(self.tok_i) {
                token!(Comma) => self.tok_i += 1,
                token!(Colon) | token!(RParen) | token!(RBrace) | token!(RBracket) => break,
                _ => self.warn_expected(token!(Comma)),
            }
        }
        if self.eat_token(token!(Colon)).is_some() {
            while self.eat_token(token!(StringLiteral)).is_some() {
                match self.token_tag(self.tok_i) {
                    token!(Comma) => self.tok_i += 1,
                    token!(Colon) | token!(RParen) | token!(RBrace) | token!(RBracket) => break,
                    _ => self.warn_expected(token!(Comma)),
                }
            }
        }
        let rparen = self.expect_token(token!(RParen))?;
        let span = self.list_to_span(&scratch);
        let rhs = self.add_extra(node::Asm {
            items_start: span.start,
            items_end: span.end,
            rparen,
        });
        Ok(self.add_node(Node {
            tag: node!(Asm),
            main_token: asm_token,
            data: node::Data { lhs: template, rhs },
        }))
    }

    pub(super) fn parse_asm_output_item(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(LBracket)).is_none() {
            return Ok(NULL_NODE);
        }
        let identifier = self.expect_token(token!(Identifier))?;
        self.expect_token(token!(RBracket))?;
        self.expect_token(token!(StringLiteral))?;
        self.expect_token(token!(LParen))?;
        let type_expr = if self.eat_token(token!(Arrow)).is_some() {
            Some(self.expect_type_expr()?)
        } else {
            self.expect_token(token!(Identifier))?;
            None
        };
        let rparen = self.expect_token(token!(RParen))?;
        Ok(self.add_node(Node {
            tag: node!(AsmOutput),
            main_token: identifier,
            data: node::Data {
                lhs: type_expr.unwrap_or(NULL_NODE),
                rhs: rparen,
            },
        }))
    }

    pub(super) fn parse_asm_input_item(&mut self) -> Result<node::Index> {
        if self.eat_token(token!(LBracket)).is_none() {
            return Ok(NULL_NODE);
        }
        let identifier = self.expect_token(token!(Identifier))?;
        self.expect_token(token!(RBracket))?;
        self.expect_token(token!(StringLiteral))?;
        self.expect_token(token!(LParen))?;
        let expr = self.expect_expr()?;
        let rparen = self.expect_token(token!(RParen))?;
        Ok(self.add_node(Node {
            tag: node!(AsmInput),
            main_token: identifier,
            data: node::Data {
                lhs: expr,
                rhs: rparen,
            },
        }))
    }
}
