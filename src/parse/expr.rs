use super::*;

use std::collections::HashMap;
use std::sync::OnceLock;

impl Parser<'_, '_> {
    pub(super) fn parse_expr(&mut self) -> Result<node::Index> {
        self.parse_expr_precedence(0)
    }

    pub(super) fn expect_expr(&mut self) -> Result<node::Index> {
        let node = self.parse_expr()?;
        if node == 0 {
            self.fail(error::Tag::ExpectedExpr)
        } else {
            Ok(node)
        }
    }

    pub(super) fn parse_expr_precedence(&mut self, min_prec: i8) -> Result<node::Index> {
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

        pub(super) fn oper_table() -> &'static HashMap<token::Tag, OperInfo> {
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
                self.parse_payload()?;
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

    pub(super) fn parse_prefix_expr(&mut self) -> Result<node::Index> {
        let tag = match self.token_tag(self.tok_i) {
            token!(Bang) => node!(BoolNot),
            token!(Minus) => node!(Negation),
            token!(Tilde) => node!(BitNot),
            token!(MinusPercent) => node!(NegationWrap),
            token!(Ampersand) => node!(AddressOf),
            token!(KeywordTry) => node!(Try),
            token!(KeywordAwait) => node!(Await),
            _ => return self.parse_primary_expr(),
        };
        let main_token = self.next_token();
        let lhs = self.expect_prefix_expr()?;
        let rhs = UNDEFINED_NODE;
        Ok(self.add_node(Node {
            tag,
            main_token,
            data: node::Data { lhs, rhs },
        }))
    }

    pub(super) fn expect_prefix_expr(&mut self) -> Result<node::Index> {
        let node = self.parse_prefix_expr()?;
        if node == 0 {
            return self.fail(error!(ExpectedPrefixExpr));
        }
        Ok(node)
    }

    pub(super) fn parse_primary_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(KeywordAsm) => self.expect_asm_expr(),
            token!(KeywordIf) => self.parse_if_expr(),
            token!(KeywordBreak) => {
                let main_token = self.next_token();
                let lhs = self.parse_break_label()?;
                let rhs = self.parse_expr()?;
                Ok(self.add_node(Node {
                    tag: node!(Break),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(KeywordContinue) => {
                let main_token = self.next_token();
                let lhs = self.parse_break_label()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: node!(Continue),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(KeywordComptime) => {
                let main_token = self.next_token();
                let lhs = self.expect_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: node!(Comptime),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(KeywordNosuspend) => {
                let main_token = self.next_token();
                let lhs = self.expect_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: node!(Nosuspend),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(KeywordResume) => {
                let main_token = self.next_token();
                let lhs = self.expect_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: node!(Resume),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(KeywordReturn) => {
                let main_token = self.next_token();
                let lhs = self.parse_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: node!(Return),
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            token!(Identifier) => {
                if self.token_tag(self.tok_i + 1) == token!(Colon) {
                    match self.token_tag(self.tok_i + 2) {
                        token!(KeywordInline) => {
                            self.tok_i += 3;
                            match self.token_tag(self.tok_i) {
                                token!(KeywordFor) => self.parse_for(Self::expect_expr),
                                token!(KeywordWhile) => self.parse_while_expr(),
                                _ => self.fail(error!(ExpectedInlinable)),
                            }
                        }
                        token!(KeywordFor) => {
                            self.tok_i += 2;
                            self.parse_for(Self::expect_expr)
                        }
                        token!(KeywordWhile) => {
                            self.tok_i += 2;
                            self.parse_while_expr()
                        }
                        token!(LBrace) => {
                            self.tok_i += 2;
                            self.parse_block()
                        }
                        _ => self.parse_curly_suffix_expr(),
                    }
                } else {
                    self.parse_curly_suffix_expr()
                }
            }
            token!(KeywordInline) => {
                self.tok_i += 1;
                match self.token_tag(self.tok_i) {
                    token!(KeywordFor) => self.parse_for(Self::expect_expr),
                    token!(KeywordWhile) => self.parse_while_expr(),
                    _ => self.fail(error!(ExpectedInlinable)),
                }
            }
            token!(KeywordFor) => self.parse_for(Self::expect_expr),
            token!(KeywordWhile) => self.parse_while_expr(),
            token!(LBrace) => self.parse_block(),
            _ => self.parse_curly_suffix_expr(),
        }
    }

    pub(super) fn parse_curly_suffix_expr(&mut self) -> Result<node::Index> {
        let lhs = self.parse_type_expr()?;
        if lhs == 0 {
            return Ok(NULL_NODE);
        }
        let Some(lbrace) = self.eat_token(token!(LBrace)) else {
            return Ok(lhs);
        };

        let mut inits = Vec::new();
        let field_init = self.parse_field_init()?;
        if field_init != 0 {
            inits.push(field_init);
            todo!("parse_curly_suffix_expr")
        }

        loop {
            if self.eat_token(token!(RBrace)).is_some() {
                break;
            }
            let elem_init = self.expect_expr()?;
            inits.push(elem_init);
            match self.token_tag(self.tok_i) {
                token!(Comma) => self.tok_i += 1,
                token!(RBrace) => {
                    self.tok_i += 1;
                    break;
                }
                token!(Colon) | token!(RParen) | token!(RBracket) => {
                    return self.fail_expected(token!(RBrace));
                }
                _ => self.warn(error!(ExpectedCommaAfterInitializer)),
            }
        }
        let comma = self.token_tag(self.tok_i - 2) == token!(Comma);
        match inits[..] {
            [] => Ok(self.add_node(Node {
                tag: node!(StructInitOne),
                main_token: lbrace,
                data: node::Data { lhs, rhs: 0 },
            })),
            [rhs] => Ok(self.add_node(Node {
                tag: match comma {
                    true => node!(ArrayInitOneComma),
                    false => node!(ArrayInitOne),
                },
                main_token: lbrace,
                data: node::Data { lhs, rhs },
            })),
            [..] => {
                let span = self.list_to_span(&inits);
                let rhs = self.add_extra(span);
                Ok(self.add_node(Node {
                    tag: match comma {
                        true => node!(ArrayInitComma),
                        false => node!(ArrayInit),
                    },
                    main_token: lbrace,
                    data: node::Data { lhs, rhs },
                }))
            }
        }
    }

    pub(super) fn parse_break_label(&mut self) -> Result<TokenIndex> {
        match self.eat_token(token!(Colon)) {
            None => Ok(NULL_NODE),
            Some(_) => self.expect_token(token!(Identifier)),
        }
    }
}
