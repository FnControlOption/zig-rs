use std::collections::HashMap;
use std::sync::OnceLock;

use super::*;

impl Parser<'_, '_> {
    pub(super) fn parse_expr(&mut self) -> Result<node::Index> {
        self.parse_expr_precedence(0)
    }

    pub(super) fn expect_expr(&mut self) -> Result<node::Index> {
        let node = self.parse_expr()?;
        if node == 0 {
            self.fail(E::ExpectedExpr)
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
                op! { prec: $prec, tag: $tag, assoc: Left }
            };

            // op! { AngleBracketLeft, prec: 30, tag: LessThan, assoc: None } => (token::Tag, OperInfo)
            ($key:ident, prec: $prec:literal, tag: $tag:ident, assoc: $assoc:ident) => {{
                let info = op! { prec: $prec, tag: $tag, assoc: $assoc };
                (token::Tag::$key, info)
            }};

            // op! { Plus, prec: 60, tag: Add } => (token::Tag, OperInfo)
            ($key:ident, prec: $prec:literal, tag: $tag:ident) => {
                op! { $key, prec: $prec, tag: $tag, assoc: Left }
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
            // TODO(zig-rs): use LazyLock after it is stabilized
            static OPER_TABLE: OnceLock<HashMap<token::Tag, OperInfo>> = OnceLock::new();
            OPER_TABLE.get_or_init(|| {
                let mut map = HashMap::with_capacity(ENTRIES.len());
                for (tag, info) in ENTRIES {
                    map.insert(tag, info);
                }
                map
            })
        }

        debug_assert!(min_prec >= 0);
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
                return self.fail(E::ChainedComparisonOperators);
            }

            let oper_token = self.next_token();
            if tok_tag == T::KeywordCatch {
                self.parse_payload()?;
            }
            let rhs = self.parse_expr_precedence(info.prec + 1)?;
            if rhs == 0 {
                self.warn(E::ExpectedExpr);
                return Ok(node);
            }

            {
                let tok_start = self.token_start(oper_token) as usize;
                let tok_len = tok_tag.lexeme().unwrap().len();
                let byte_before = self.source[tok_start - 1];
                let byte_after = self.source[tok_start + tok_len];
                if tok_tag == T::Ampersand && byte_after == b'&' {
                    self.warn_msg(Error::new(E::InvalidAmpersandAmpersand, oper_token));
                } else if byte_before.is_ascii_whitespace() != byte_after.is_ascii_whitespace() {
                    self.warn_msg(Error::new(E::MismatchedBinaryOpWhitespace, oper_token));
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
            T::Bang => N::BoolNot,
            T::Minus => N::Negation,
            T::Tilde => N::BitNot,
            T::MinusPercent => N::NegationWrap,
            T::Ampersand => N::AddressOf,
            T::KeywordTry => N::Try,
            T::KeywordAwait => N::Await,
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
            return self.fail(E::ExpectedPrefixExpr);
        }
        Ok(node)
    }

    pub(super) fn parse_primary_expr(&mut self) -> Result<node::Index> {
        match self.token_tag(self.tok_i) {
            T::KeywordAsm => self.expect_asm_expr(),
            T::KeywordIf => self.parse_if_expr(),
            T::KeywordBreak => {
                let main_token = self.next_token();
                let lhs = self.parse_break_label()?;
                let rhs = self.parse_expr()?;
                Ok(self.add_node(Node {
                    tag: N::Break,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::KeywordContinue => {
                let main_token = self.next_token();
                let lhs = self.parse_break_label()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::Continue,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::KeywordComptime => {
                let main_token = self.next_token();
                let lhs = self.expect_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::Comptime,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::KeywordNosuspend => {
                let main_token = self.next_token();
                let lhs = self.expect_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::Nosuspend,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::KeywordResume => {
                let main_token = self.next_token();
                let lhs = self.expect_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::Resume,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::KeywordReturn => {
                let main_token = self.next_token();
                let lhs = self.parse_expr()?;
                let rhs = UNDEFINED_NODE;
                Ok(self.add_node(Node {
                    tag: N::Return,
                    main_token,
                    data: node::Data { lhs, rhs },
                }))
            }
            T::Identifier => {
                if self.token_tag(self.tok_i + 1) == T::Colon {
                    match self.token_tag(self.tok_i + 2) {
                        T::KeywordInline => {
                            self.tok_i += 3;
                            match self.token_tag(self.tok_i) {
                                T::KeywordFor => self.parse_for(Self::expect_expr),
                                T::KeywordWhile => self.parse_while_expr(),
                                _ => self.fail(E::ExpectedInlinable),
                            }
                        }
                        T::KeywordFor => {
                            self.tok_i += 2;
                            self.parse_for(Self::expect_expr)
                        }
                        T::KeywordWhile => {
                            self.tok_i += 2;
                            self.parse_while_expr()
                        }
                        T::LBrace => {
                            self.tok_i += 2;
                            self.parse_block()
                        }
                        _ => self.parse_curly_suffix_expr(),
                    }
                } else {
                    self.parse_curly_suffix_expr()
                }
            }
            T::KeywordInline => {
                self.tok_i += 1;
                match self.token_tag(self.tok_i) {
                    T::KeywordFor => self.parse_for(Self::expect_expr),
                    T::KeywordWhile => self.parse_while_expr(),
                    _ => self.fail(E::ExpectedInlinable),
                }
            }
            T::KeywordFor => self.parse_for(Self::expect_expr),
            T::KeywordWhile => self.parse_while_expr(),
            T::LBrace => self.parse_block(),
            _ => self.parse_curly_suffix_expr(),
        }
    }

    pub(super) fn parse_curly_suffix_expr(&mut self) -> Result<node::Index> {
        let lhs = self.parse_type_expr()?;
        if lhs == 0 {
            return Ok(NULL_NODE);
        }
        let Some(lbrace) = self.eat_token(T::LBrace) else {
            return Ok(lhs);
        };

        let mut inits = Vec::new();
        let field_init = self.parse_field_init()?;
        if field_init != 0 {
            inits.push(field_init);
            loop {
                match self.token_tag(self.tok_i) {
                    T::Comma => self.tok_i += 1,
                    T::RBrace => {
                        self.tok_i += 1;
                        break;
                    }
                    T::Colon | T::RParen | T::RBracket => {
                        return self.fail_expected(T::RBrace);
                    }
                    _ => self.warn(E::ExpectedCommaAfterInitializer),
                }
                if self.eat_token(T::RBrace).is_some() {
                    break;
                }
                let next = self.expect_field_init()?;
                inits.push(next);
            }
            let comma = self.token_tag(self.tok_i - 2) == T::Comma;
            return match inits[..] {
                [] => unreachable!(),
                [rhs] => Ok(self.add_node(Node {
                    tag: match comma {
                        true => N::StructInitOneComma,
                        false => N::StructInitOne,
                    },
                    main_token: lbrace,
                    data: node::Data { lhs, rhs },
                })),
                [..] => {
                    let span = self.list_to_span(&inits);
                    let rhs = self.add_extra(span);
                    Ok(self.add_node(Node {
                        tag: match comma {
                            true => N::StructInitComma,
                            false => N::StructInit,
                        },
                        main_token: lbrace,
                        data: node::Data { lhs, rhs },
                    }))
                }
            };
        }

        loop {
            if self.eat_token(T::RBrace).is_some() {
                break;
            }
            let elem_init = self.expect_expr()?;
            inits.push(elem_init);
            match self.token_tag(self.tok_i) {
                T::Comma => self.tok_i += 1,
                T::RBrace => {
                    self.tok_i += 1;
                    break;
                }
                T::Colon | T::RParen | T::RBracket => {
                    return self.fail_expected(T::RBrace);
                }
                _ => self.warn(E::ExpectedCommaAfterInitializer),
            }
        }
        let comma = self.token_tag(self.tok_i - 2) == T::Comma;
        match inits[..] {
            [] => Ok(self.add_node(Node {
                tag: N::StructInitOne,
                main_token: lbrace,
                data: node::Data { lhs, rhs: 0 },
            })),
            [rhs] => Ok(self.add_node(Node {
                tag: match comma {
                    true => N::ArrayInitOneComma,
                    false => N::ArrayInitOne,
                },
                main_token: lbrace,
                data: node::Data { lhs, rhs },
            })),
            [..] => {
                let span = self.list_to_span(&inits);
                let rhs = self.add_extra(span);
                Ok(self.add_node(Node {
                    tag: match comma {
                        true => N::ArrayInitComma,
                        false => N::ArrayInit,
                    },
                    main_token: lbrace,
                    data: node::Data { lhs, rhs },
                }))
            }
        }
    }

    pub(super) fn parse_break_label(&mut self) -> Result<TokenIndex> {
        match self.eat_token(T::Colon) {
            None => Ok(NULL_NODE),
            Some(_) => self.expect_token(T::Identifier),
        }
    }
}
