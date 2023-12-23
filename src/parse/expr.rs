use super::*;

impl<'src, 'tok> Parser<'src, 'tok> {
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

    pub(super) fn parse_prefix_expr(&mut self) -> Result<node::Index> {
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

    pub(super) fn expect_prefix_expr(&mut self) -> Result<node::Index> {
        todo!("expect_prefix_expr")
    }

    pub(super) fn parse_primary_expr(&mut self) -> Result<node::Index> {
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

    pub(super) fn parse_curly_suffix_expr(&mut self) -> Result<node::Index> {
        let lhs = self.parse_type_expr()?;
        if lhs == 0 {
            return Ok(NULL_NODE);
        }
        let Some(lbrace) = self.eat_token(token!(LBrace)) else {
            return Ok(lhs);
        };
        todo!("parse_curly_suffix_expr")
    }

    pub(super) fn parse_error_union_expr(&mut self) -> Result<node::Index> {
        let suffix_expr = self.parse_suffix_expr()?;
        if suffix_expr == 0 {
            return Ok(NULL_NODE);
        }
        let Some(bang) = self.eat_token(token!(Bang)) else {
            return Ok(suffix_expr);
        };
        todo!("parse_error_union_expr")
    }

    pub(super) fn parse_suffix_expr(&mut self) -> Result<node::Index> {
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
                    let rhs = self.add_extra(node::SubRange { ..span });
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

    pub(super) fn parse_suffix_op(&mut self, lhs: node::Index) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            token!(LBracket)
            | token!(PeriodAsterisk)
            | token!(InvalidPeriodAsterisks)
            | token!(Period) => todo!("parse_suffix_op"),
            _ => return Ok(NULL_NODE),
        }
    }
}
