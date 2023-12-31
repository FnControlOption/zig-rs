use super::*;

impl Parser<'_, '_> {
    pub(super) fn expect_asm_expr(&mut self) -> Result<node::Index> {
        let asm_token = self.assert_token(T::KeywordAsm);
        self.eat_token(T::KeywordVolatile);
        self.expect_token(T::LParen)?;
        let template = self.expect_expr()?;

        if let Some(rparen) = self.eat_token(T::RParen) {
            return Ok(self.add_node(Node {
                tag: N::AsmSimple,
                main_token: asm_token,
                data: node::Data {
                    lhs: template,
                    rhs: rparen,
                },
            }));
        }

        self.expect_token(T::Colon)?;

        let mut scratch = Vec::new();

        loop {
            let output_item = self.parse_asm_output_item()?;
            if output_item == 0 {
                break;
            }
            scratch.push(output_item);
            match self.token_tag(self.tok_i) {
                T::Comma => self.tok_i += 1,
                T::Colon | T::RParen | T::RBrace | T::RBracket => break,
                _ => self.warn_expected(T::Comma),
            }
        }
        if self.eat_token(T::Colon).is_some() {
            loop {
                let input_item = self.parse_asm_input_item()?;
                if input_item == 0 {
                    break;
                }
                scratch.push(input_item);
                match self.token_tag(self.tok_i) {
                    T::Comma => self.tok_i += 1,
                    T::Colon | T::RParen | T::RBrace | T::RBracket => break,
                    _ => self.warn_expected(T::Comma),
                }
            }
            if self.eat_token(T::Colon).is_some() {
                while self.eat_token(T::StringLiteral).is_some() {
                    match self.token_tag(self.tok_i) {
                        T::Comma => self.tok_i += 1,
                        T::Colon | T::RParen | T::RBrace | T::RBracket => break,
                        _ => self.warn_expected(T::Comma),
                    }
                }
            }
        }
        let rparen = self.expect_token(T::RParen)?;
        let span = self.list_to_span(&scratch);
        let rhs = self.add_extra(node::Asm {
            items_start: span.start,
            items_end: span.end,
            rparen,
        });
        Ok(self.add_node(Node {
            tag: N::Asm,
            main_token: asm_token,
            data: node::Data { lhs: template, rhs },
        }))
    }

    pub(super) fn parse_asm_output_item(&mut self) -> Result<node::Index> {
        if self.eat_token(T::LBracket).is_none() {
            return Ok(NULL_NODE);
        }
        let identifier = self.expect_token(T::Identifier)?;
        self.expect_token(T::RBracket)?;
        self.expect_token(T::StringLiteral)?;
        self.expect_token(T::LParen)?;
        let type_expr = match self.eat_token(T::Arrow) {
            Some(_) => self.expect_type_expr()?,
            None => {
                self.expect_token(T::Identifier)?;
                NULL_NODE
            }
        };
        let rparen = self.expect_token(T::RParen)?;
        Ok(self.add_node(Node {
            tag: N::AsmOutput,
            main_token: identifier,
            data: node::Data {
                lhs: type_expr,
                rhs: rparen,
            },
        }))
    }

    pub(super) fn parse_asm_input_item(&mut self) -> Result<node::Index> {
        if self.eat_token(T::LBracket).is_none() {
            return Ok(NULL_NODE);
        }
        let identifier = self.expect_token(T::Identifier)?;
        self.expect_token(T::RBracket)?;
        self.expect_token(T::StringLiteral)?;
        self.expect_token(T::LParen)?;
        let expr = self.expect_expr()?;
        let rparen = self.expect_token(T::RParen)?;
        Ok(self.add_node(Node {
            tag: N::AsmInput,
            main_token: identifier,
            data: node::Data {
                lhs: expr,
                rhs: rparen,
            },
        }))
    }
}
