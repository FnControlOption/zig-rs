use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::OnceLock;

pub struct Token {
    pub tag: Tag,
    pub loc: Loc,
}

pub struct Loc {
    pub start: usize,
    pub end: usize,
}

fn keywords() -> &'static HashMap<&'static [u8], Tag> {
    let entries = [
        ("addrspace", Tag::KeywordAddrspace),
        ("align", Tag::KeywordAlign),
        ("allowzero", Tag::KeywordAllowzero),
        ("and", Tag::KeywordAnd),
        ("anyframe", Tag::KeywordAnyframe),
        ("anytype", Tag::KeywordAnytype),
        ("asm", Tag::KeywordAsm),
        ("async", Tag::KeywordAsync),
        ("await", Tag::KeywordAwait),
        ("break", Tag::KeywordBreak),
        ("callconv", Tag::KeywordCallconv),
        ("catch", Tag::KeywordCatch),
        ("comptime", Tag::KeywordComptime),
        ("const", Tag::KeywordConst),
        ("continue", Tag::KeywordContinue),
        ("defer", Tag::KeywordDefer),
        ("else", Tag::KeywordElse),
        ("enum", Tag::KeywordEnum),
        ("errdefer", Tag::KeywordErrdefer),
        ("error", Tag::KeywordError),
        ("export", Tag::KeywordExport),
        ("extern", Tag::KeywordExtern),
        ("fn", Tag::KeywordFn),
        ("for", Tag::KeywordFor),
        ("if", Tag::KeywordIf),
        ("inline", Tag::KeywordInline),
        ("noalias", Tag::KeywordNoalias),
        ("noinline", Tag::KeywordNoinline),
        ("nosuspend", Tag::KeywordNosuspend),
        ("opaque", Tag::KeywordOpaque),
        ("or", Tag::KeywordOr),
        ("orelse", Tag::KeywordOrelse),
        ("packed", Tag::KeywordPacked),
        ("pub", Tag::KeywordPub),
        ("resume", Tag::KeywordResume),
        ("return", Tag::KeywordReturn),
        ("linksection", Tag::KeywordLinksection),
        ("struct", Tag::KeywordStruct),
        ("suspend", Tag::KeywordSuspend),
        ("switch", Tag::KeywordSwitch),
        ("test", Tag::KeywordTest),
        ("threadlocal", Tag::KeywordThreadlocal),
        ("try", Tag::KeywordTry),
        ("union", Tag::KeywordUnion),
        ("unreachable", Tag::KeywordUnreachable),
        ("usingnamespace", Tag::KeywordUsingnamespace),
        ("var", Tag::KeywordVar),
        ("volatile", Tag::KeywordVolatile),
        ("while", Tag::KeywordWhile),
    ];

    // TODO(zig-rs): use LazyLock after it is stabilized
    static KEYWORDS: OnceLock<HashMap<&'static [u8], Tag>> = OnceLock::new();
    KEYWORDS.get_or_init(|| {
        let mut map = HashMap::new();
        for (string, tag) in entries {
            map.insert(string.as_bytes(), tag);
        }
        map
    })
}

impl Token {
    pub fn get_keyword(bytes: &[u8]) -> Option<Tag> {
        keywords().get(bytes).copied()
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Tag {
    Invalid,
    InvalidPeriodAsterisks,
    Identifier,
    StringLiteral,
    MultilineStringLiteralLine,
    CharLiteral,
    Eof,
    Builtin,
    Bang,
    Pipe,
    PipePipe,
    PipeEqual,
    Equal,
    EqualEqual,
    EqualAngleBracketRight,
    BangEqual,
    LParen,
    RParen,
    Semicolon,
    Percent,
    PercentEqual,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Period,
    PeriodAsterisk,
    Ellipsis2,
    Ellipsis3,
    Caret,
    CaretEqual,
    Plus,
    PlusPlus,
    PlusEqual,
    PlusPercent,
    PlusPercentEqual,
    PlusPipe,
    PlusPipeEqual,
    Minus,
    MinusEqual,
    MinusPercent,
    MinusPercentEqual,
    MinusPipe,
    MinusPipeEqual,
    Asterisk,
    AsteriskEqual,
    AsteriskAsterisk,
    AsteriskPercent,
    AsteriskPercentEqual,
    AsteriskPipe,
    AsteriskPipeEqual,
    Arrow,
    Colon,
    Slash,
    SlashEqual,
    Comma,
    Ampersand,
    AmpersandEqual,
    QuestionMark,
    AngleBracketLeft,
    AngleBracketLeftEqual,
    AngleBracketAngleBracketLeft,
    AngleBracketAngleBracketLeftEqual,
    AngleBracketAngleBracketLeftPipe,
    AngleBracketAngleBracketLeftPipeEqual,
    AngleBracketRight,
    AngleBracketRightEqual,
    AngleBracketAngleBracketRight,
    AngleBracketAngleBracketRightEqual,
    Tilde,
    NumberLiteral,
    DocComment,
    ContainerDocComment,
    KeywordAddrspace,
    KeywordAlign,
    KeywordAllowzero,
    KeywordAnd,
    KeywordAnyframe,
    KeywordAnytype,
    KeywordAsm,
    KeywordAsync,
    KeywordAwait,
    KeywordBreak,
    KeywordCallconv,
    KeywordCatch,
    KeywordComptime,
    KeywordConst,
    KeywordContinue,
    KeywordDefer,
    KeywordElse,
    KeywordEnum,
    KeywordErrdefer,
    KeywordError,
    KeywordExport,
    KeywordExtern,
    KeywordFn,
    KeywordFor,
    KeywordIf,
    KeywordInline,
    KeywordNoalias,
    KeywordNoinline,
    KeywordNosuspend,
    KeywordOpaque,
    KeywordOr,
    KeywordOrelse,
    KeywordPacked,
    KeywordPub,
    KeywordResume,
    KeywordReturn,
    KeywordLinksection,
    KeywordStruct,
    KeywordSuspend,
    KeywordSwitch,
    KeywordTest,
    KeywordThreadlocal,
    KeywordTry,
    KeywordUnion,
    KeywordUnreachable,
    KeywordUsingnamespace,
    KeywordVar,
    KeywordVolatile,
    KeywordWhile,
}

impl Tag {
    pub fn lexeme(self) -> Option<&'static str> {
        match self {
            Tag::Invalid
            | Tag::Identifier
            | Tag::StringLiteral
            | Tag::MultilineStringLiteralLine
            | Tag::CharLiteral
            | Tag::Eof
            | Tag::Builtin
            | Tag::NumberLiteral
            | Tag::DocComment
            | Tag::ContainerDocComment => None,

            _ => Some(self.symbol()),
        }
    }

    pub fn symbol(self) -> &'static str {
        match self {
            Tag::Invalid => "invalid bytes",
            Tag::Identifier => "an identifier",
            Tag::StringLiteral | Tag::MultilineStringLiteralLine => "a string literal",
            Tag::CharLiteral => "a character literal",
            Tag::Eof => "EOF",
            Tag::Builtin => "a builtin function",
            Tag::NumberLiteral => "a number literal",
            Tag::DocComment | Tag::ContainerDocComment => "a document comment",

            Tag::InvalidPeriodAsterisks => ".**",
            Tag::Bang => "!",
            Tag::Pipe => "|",
            Tag::PipePipe => "||",
            Tag::PipeEqual => "|=",
            Tag::Equal => "=",
            Tag::EqualEqual => "==",
            Tag::EqualAngleBracketRight => "=>",
            Tag::BangEqual => "!=",
            Tag::LParen => "(",
            Tag::RParen => ")",
            Tag::Semicolon => ";",
            Tag::Percent => "%",
            Tag::PercentEqual => "%=",
            Tag::LBrace => "{",
            Tag::RBrace => "}",
            Tag::LBracket => "[",
            Tag::RBracket => "]",
            Tag::Period => ".",
            Tag::PeriodAsterisk => ".*",
            Tag::Ellipsis2 => "..",
            Tag::Ellipsis3 => "...",
            Tag::Caret => "^",
            Tag::CaretEqual => "^=",
            Tag::Plus => "+",
            Tag::PlusPlus => "++",
            Tag::PlusEqual => "+=",
            Tag::PlusPercent => "+%",
            Tag::PlusPercentEqual => "+%=",
            Tag::PlusPipe => "+|",
            Tag::PlusPipeEqual => "+|=",
            Tag::Minus => "-",
            Tag::MinusEqual => "-=",
            Tag::MinusPercent => "-%",
            Tag::MinusPercentEqual => "-%=",
            Tag::MinusPipe => "-|",
            Tag::MinusPipeEqual => "-|=",
            Tag::Asterisk => "*",
            Tag::AsteriskEqual => "*=",
            Tag::AsteriskAsterisk => "**",
            Tag::AsteriskPercent => "*%",
            Tag::AsteriskPercentEqual => "*%=",
            Tag::AsteriskPipe => "*|",
            Tag::AsteriskPipeEqual => "*|=",
            Tag::Arrow => "->",
            Tag::Colon => ":",
            Tag::Slash => "/",
            Tag::SlashEqual => "/=",
            Tag::Comma => ",",
            Tag::Ampersand => "&",
            Tag::AmpersandEqual => "&=",
            Tag::QuestionMark => "?",
            Tag::AngleBracketLeft => "<",
            Tag::AngleBracketLeftEqual => "<=",
            Tag::AngleBracketAngleBracketLeft => "<<",
            Tag::AngleBracketAngleBracketLeftEqual => "<<=",
            Tag::AngleBracketAngleBracketLeftPipe => "<<|",
            Tag::AngleBracketAngleBracketLeftPipeEqual => "<<|=",
            Tag::AngleBracketRight => ">",
            Tag::AngleBracketRightEqual => ">=",
            Tag::AngleBracketAngleBracketRight => ">>",
            Tag::AngleBracketAngleBracketRightEqual => ">>=",
            Tag::Tilde => "~",
            Tag::KeywordAddrspace => "addrspace",
            Tag::KeywordAlign => "align",
            Tag::KeywordAllowzero => "allowzero",
            Tag::KeywordAnd => "and",
            Tag::KeywordAnyframe => "anyframe",
            Tag::KeywordAnytype => "anytype",
            Tag::KeywordAsm => "asm",
            Tag::KeywordAsync => "async",
            Tag::KeywordAwait => "await",
            Tag::KeywordBreak => "break",
            Tag::KeywordCallconv => "callconv",
            Tag::KeywordCatch => "catch",
            Tag::KeywordComptime => "comptime",
            Tag::KeywordConst => "const",
            Tag::KeywordContinue => "continue",
            Tag::KeywordDefer => "defer",
            Tag::KeywordElse => "else",
            Tag::KeywordEnum => "enum",
            Tag::KeywordErrdefer => "errdefer",
            Tag::KeywordError => "error",
            Tag::KeywordExport => "export",
            Tag::KeywordExtern => "extern",
            Tag::KeywordFn => "fn",
            Tag::KeywordFor => "for",
            Tag::KeywordIf => "if",
            Tag::KeywordInline => "inline",
            Tag::KeywordNoalias => "noalias",
            Tag::KeywordNoinline => "noinline",
            Tag::KeywordNosuspend => "nosuspend",
            Tag::KeywordOpaque => "opaque",
            Tag::KeywordOr => "or",
            Tag::KeywordOrelse => "orelse",
            Tag::KeywordPacked => "packed",
            Tag::KeywordPub => "pub",
            Tag::KeywordResume => "resume",
            Tag::KeywordReturn => "return",
            Tag::KeywordLinksection => "linksection",
            Tag::KeywordStruct => "struct",
            Tag::KeywordSuspend => "suspend",
            Tag::KeywordSwitch => "switch",
            Tag::KeywordTest => "test",
            Tag::KeywordThreadlocal => "threadlocal",
            Tag::KeywordTry => "try",
            Tag::KeywordUnion => "union",
            Tag::KeywordUnreachable => "unreachable",
            Tag::KeywordUsingnamespace => "usingnamespace",
            Tag::KeywordVar => "var",
            Tag::KeywordVolatile => "volatile",
            Tag::KeywordWhile => "while",
        }
    }
}

pub struct Tokenizer<'src> {
    pub buffer: &'src [u8],
    pub index: usize,
    pub pending_invalid_token: Option<Token>,
}

impl<'src> Tokenizer<'src> {
    /// For debugging purposes
    pub fn dump(&self, token: &Token) {
        let string = String::from_utf8_lossy(&self.buffer[token.loc.start..token.loc.end]);
        println!("{:?} \"{}\"", token.tag, string);
    }

    pub fn new(buffer: &'src [u8]) -> Self {
        // Skip the UTF-8 BOM if present
        let src_start = match buffer {
            &[0xEF, 0xBB, 0xBF, ..] => 3,
            _ => 0,
        };
        Self {
            buffer,
            index: src_start,
            pending_invalid_token: None,
        }
    }

    pub fn find_tag_at_current_index(&mut self, tag: Tag) -> Token {
        if tag == Tag::Invalid {
            todo!("find_tag_at_current_index")
        } else {
            self.next()
        }
    }

    pub fn next(&mut self) -> Token {
        enum State {
            Start,
            Identifier,
            Builtin,
            StringLiteral,
            StringLiteralBackslash,
            MultilineStringLiteralLine,
            CharLiteral,
            CharLiteralBackslash,
            CharLiteralHexEscape,
            CharLiteralUnicodeEscapeSawU,
            CharLiteralUnicodeEscape,
            CharLiteralUnicodeInvalid,
            CharLiteralUnicode,
            CharLiteralEnd,
            Backslash,
            Equal,
            Bang,
            Pipe,
            Minus,
            MinusPercent,
            MinusPipe,
            Asterisk,
            AsteriskPercent,
            AsteriskPipe,
            Slash,
            LineCommentStart,
            LineComment,
            DocCommentStart,
            DocComment,
            Int,
            IntExponent,
            IntPeriod,
            Float,
            FloatExponent,
            Ampersand,
            Caret,
            Percent,
            Plus,
            PlusPercent,
            PlusPipe,
            AngleBracketLeft,
            AngleBracketAngleBracketLeft,
            AngleBracketAngleBracketLeftPipe,
            AngleBracketRight,
            AngleBracketAngleBracketRight,
            Period,
            Period2,
            PeriodAsterisk,
            SawAtSign,
        }

        if let Some(token) = self.pending_invalid_token.take() {
            return token;
        }
        let mut state = State::Start;
        let mut result = Token {
            tag: Tag::Eof,
            loc: Loc {
                start: self.index,
                end: usize::MAX,
            },
        };
        let mut seen_escape_digits: usize = usize::MAX;
        let mut remaining_code_units: usize = usize::MAX;
        loop {
            let c = self.buffer.get(self.index).copied().unwrap_or(0);
            match state {
                State::Start => match c {
                    0 => {
                        if self.index != self.buffer.len() {
                            result.tag = Tag::Invalid;
                            result.loc.start = self.index;
                            self.index += 1;
                            result.loc.end = self.index;
                            return result;
                        }
                        break;
                    }
                    b' ' | b'\n' | b'\t' | b'\r' => {
                        result.loc.start = self.index + 1;
                    }
                    b'"' => {
                        state = State::StringLiteral;
                        result.tag = Tag::StringLiteral;
                    }
                    b'\'' => {
                        state = State::CharLiteral;
                    }
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                        state = State::Identifier;
                        result.tag = Tag::Identifier;
                    }
                    b'@' => {
                        state = State::SawAtSign;
                    }
                    b'=' => {
                        state = State::Equal;
                    }
                    b'!' => {
                        state = State::Bang;
                    }
                    b'|' => {
                        state = State::Pipe;
                    }
                    b'(' => {
                        result.tag = Tag::LParen;
                        self.index += 1;
                        break;
                    }
                    b')' => {
                        result.tag = Tag::RParen;
                        self.index += 1;
                        break;
                    }
                    b'[' => {
                        result.tag = Tag::LBracket;
                        self.index += 1;
                        break;
                    }
                    b']' => {
                        result.tag = Tag::RBracket;
                        self.index += 1;
                        break;
                    }
                    b';' => {
                        result.tag = Tag::Semicolon;
                        self.index += 1;
                        break;
                    }
                    b',' => {
                        result.tag = Tag::Comma;
                        self.index += 1;
                        break;
                    }
                    b'?' => {
                        result.tag = Tag::QuestionMark;
                        self.index += 1;
                        break;
                    }
                    b':' => {
                        result.tag = Tag::Colon;
                        self.index += 1;
                        break;
                    }
                    b'%' => {
                        state = State::Percent;
                    }
                    b'*' => {
                        state = State::Asterisk;
                    }
                    b'+' => {
                        state = State::Plus;
                    }
                    b'<' => {
                        state = State::AngleBracketLeft;
                    }
                    b'>' => {
                        state = State::AngleBracketRight;
                    }
                    b'^' => {
                        state = State::Caret;
                    }
                    b'\\' => {
                        state = State::Backslash;
                        result.tag = Tag::MultilineStringLiteralLine;
                    }
                    b'{' => {
                        result.tag = Tag::LBrace;
                        self.index += 1;
                        break;
                    }
                    b'}' => {
                        result.tag = Tag::RBrace;
                        self.index += 1;
                        break;
                    }
                    b'~' => {
                        result.tag = Tag::Tilde;
                        self.index += 1;
                        break;
                    }
                    b'.' => {
                        state = State::Period;
                    }
                    b'-' => {
                        state = State::Minus;
                    }
                    b'/' => {
                        state = State::Slash;
                    }
                    b'&' => {
                        state = State::Ampersand;
                    }
                    b'0'..=b'9' => {
                        state = State::Int;
                        result.tag = Tag::NumberLiteral;
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        result.loc.end = self.index;
                        self.index += 1;
                        return result;
                    }
                },
                State::SawAtSign => match c {
                    b'\"' => {
                        result.tag = Tag::Identifier;
                        state = State::StringLiteral;
                    }
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                        state = State::Builtin;
                        result.tag = Tag::Builtin;
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                },
                State::Ampersand => match c {
                    b'=' => {
                        result.tag = Tag::AmpersandEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Ampersand;
                        break;
                    }
                },
                State::Asterisk => match c {
                    b'=' => {
                        result.tag = Tag::AsteriskEqual;
                        self.index += 1;
                        break;
                    }
                    b'*' => {
                        result.tag = Tag::AsteriskAsterisk;
                        self.index += 1;
                        break;
                    }
                    b'%' => {
                        state = State::AsteriskPercent;
                    }
                    b'|' => {
                        state = State::AsteriskPipe;
                    }
                    _ => {
                        result.tag = Tag::Asterisk;
                        break;
                    }
                },
                State::AsteriskPercent => match c {
                    b'=' => {
                        result.tag = Tag::AsteriskPercentEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::AsteriskPercent;
                        break;
                    }
                },
                State::AsteriskPipe => match c {
                    b'=' => {
                        result.tag = Tag::AsteriskPipeEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::AsteriskPipe;
                        break;
                    }
                },
                State::Percent => match c {
                    b'=' => {
                        result.tag = Tag::PercentEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Percent;
                        break;
                    }
                },
                State::Plus => match c {
                    b'=' => {
                        result.tag = Tag::PlusEqual;
                        self.index += 1;
                        break;
                    }
                    b'+' => {
                        result.tag = Tag::PlusPlus;
                        self.index += 1;
                        break;
                    }
                    b'%' => {
                        state = State::PlusPercent;
                    }
                    b'|' => {
                        state = State::PlusPipe;
                    }
                    _ => {
                        result.tag = Tag::Plus;
                        break;
                    }
                },
                State::PlusPercent => match c {
                    b'=' => {
                        result.tag = Tag::PlusPercentEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::PlusPercent;
                        break;
                    }
                },
                State::PlusPipe => match c {
                    b'=' => {
                        result.tag = Tag::PlusPipeEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::PlusPipe;
                        break;
                    }
                },
                State::Caret => match c {
                    b'=' => {
                        result.tag = Tag::CaretEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Caret;
                        break;
                    }
                },
                State::Identifier => match c {
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9' => {}
                    _ => {
                        if let Some(tag) =
                            Token::get_keyword(&self.buffer[result.loc.start..self.index])
                        {
                            result.tag = tag;
                        }
                        break;
                    }
                },
                State::Builtin => match c {
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9' => {}
                    _ => break,
                },
                State::Backslash => match c {
                    b'\\' => {
                        state = State::MultilineStringLiteralLine;
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                },
                State::StringLiteral => match c {
                    b'\\' => {
                        state = State::StringLiteralBackslash;
                    }
                    b'\"' => {
                        self.index += 1;
                        break;
                    }
                    0 => {
                        if self.index == self.buffer.len() {
                            result.tag = Tag::Invalid;
                            break;
                        } else {
                            self.check_literal_character();
                        }
                    }
                    b'\n' => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    _ => self.check_literal_character(),
                },
                State::StringLiteralBackslash => match c {
                    0 | b'\n' => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    _ => {
                        state = State::StringLiteral;
                    }
                },
                State::CharLiteral => match c {
                    0 => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    b'\\' => {
                        state = State::CharLiteralBackslash;
                    }
                    b'\'' | 0x80..=0xbf | 0xf8..=0xff => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    // 110xxxxx
                    0xc0..=0xdf => {
                        remaining_code_units = 1;
                        state = State::CharLiteralUnicode;
                    }
                    // 1110xxxx
                    0xe0..=0xef => {
                        remaining_code_units = 2;
                        state = State::CharLiteralUnicode;
                    }
                    // 11110xxx
                    0xf0..=0xf7 => {
                        remaining_code_units = 3;
                        state = State::CharLiteralUnicode;
                    }
                    b'\n' => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    _ => {
                        state = State::CharLiteralEnd;
                    }
                },
                State::CharLiteralBackslash => match c {
                    0 | b'\n' => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    b'x' => {
                        state = State::CharLiteralHexEscape;
                        seen_escape_digits = 0;
                    }
                    b'u' => {
                        state = State::CharLiteralUnicodeEscapeSawU;
                    }
                    _ => {
                        state = State::CharLiteralEnd;
                    }
                },
                State::CharLiteralHexEscape => match c {
                    b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => {
                        seen_escape_digits += 1;
                        if seen_escape_digits == 2 {
                            state = State::CharLiteralEnd;
                        }
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                },
                State::CharLiteralUnicodeEscapeSawU => match c {
                    0 => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    b'{' => {
                        state = State::CharLiteralUnicodeEscape;
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        state = State::CharLiteralUnicodeInvalid;
                    }
                },
                State::CharLiteralUnicodeEscape => match c {
                    0 => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                    b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => {}
                    b'}' => {
                        state = State::CharLiteralEnd; // too many/few digits handled later
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        state = State::CharLiteralUnicodeInvalid;
                    }
                },
                State::CharLiteralUnicodeInvalid => match c {
                    // Keep consuming characters until an obvious stopping point.
                    // This consolidates e.g. `u{0ab1Q}` into a single invalid token
                    // instead of creating the tokens `u{0ab1`, `Q`, `}`
                    b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'}' => {}
                    _ => break,
                },
                State::CharLiteralEnd => match c {
                    b'\'' => {
                        result.tag = Tag::CharLiteral;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                },
                State::CharLiteralUnicode => match c {
                    0x80..=0xbf => {
                        remaining_code_units -= 1;
                        if remaining_code_units == 0 {
                            state = State::CharLiteralEnd;
                        }
                    }
                    _ => {
                        result.tag = Tag::Invalid;
                        break;
                    }
                },
                State::MultilineStringLiteralLine => match c {
                    0 => break,
                    b'\n' => {
                        self.index += 1;
                        break;
                    }
                    b'\t' => {}
                    _ => self.check_literal_character(),
                },
                State::Bang => match c {
                    b'=' => {
                        result.tag = Tag::BangEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Bang;
                        break;
                    }
                },
                State::Pipe => match c {
                    b'=' => {
                        result.tag = Tag::PipeEqual;
                        self.index += 1;
                        break;
                    }
                    b'|' => {
                        result.tag = Tag::PipePipe;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Pipe;
                        break;
                    }
                },
                State::Equal => match c {
                    b'=' => {
                        result.tag = Tag::EqualEqual;
                        self.index += 1;
                        break;
                    }
                    b'>' => {
                        result.tag = Tag::EqualAngleBracketRight;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Equal;
                        break;
                    }
                },
                State::Minus => match c {
                    b'>' => {
                        result.tag = Tag::Arrow;
                        self.index += 1;
                        break;
                    }
                    b'=' => {
                        result.tag = Tag::MinusEqual;
                        self.index += 1;
                        break;
                    }
                    b'%' => {
                        state = State::MinusPercent;
                    }
                    b'|' => {
                        state = State::MinusPipe;
                    }
                    _ => {
                        result.tag = Tag::Minus;
                        break;
                    }
                },
                State::MinusPercent => match c {
                    b'=' => {
                        result.tag = Tag::MinusPercentEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::MinusPercent;
                        break;
                    }
                },
                State::MinusPipe => match c {
                    b'=' => {
                        result.tag = Tag::MinusPipeEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::MinusPipe;
                        break;
                    }
                },
                State::AngleBracketLeft => match c {
                    b'<' => {
                        state = State::AngleBracketAngleBracketLeft;
                    }
                    b'=' => {
                        result.tag = Tag::AngleBracketLeftEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::AngleBracketLeft;
                        break;
                    }
                },
                State::AngleBracketAngleBracketLeft => match c {
                    b'=' => {
                        result.tag = Tag::AngleBracketAngleBracketLeftEqual;
                        self.index += 1;
                        break;
                    }
                    b'|' => {
                        state = State::AngleBracketAngleBracketLeftPipe;
                    }
                    _ => {
                        result.tag = Tag::AngleBracketAngleBracketLeft;
                        break;
                    }
                },
                State::AngleBracketAngleBracketLeftPipe => match c {
                    b'=' => {
                        result.tag = Tag::AngleBracketAngleBracketLeftPipeEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::AngleBracketAngleBracketLeftPipe;
                        break;
                    }
                },
                State::AngleBracketRight => match c {
                    b'>' => {
                        state = State::AngleBracketAngleBracketRight;
                    }
                    b'=' => {
                        result.tag = Tag::AngleBracketRightEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::AngleBracketRight;
                        break;
                    }
                },
                State::AngleBracketAngleBracketRight => match c {
                    b'=' => {
                        result.tag = Tag::AngleBracketAngleBracketRightEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::AngleBracketAngleBracketRight;
                        break;
                    }
                },
                State::Period => match c {
                    b'.' => {
                        state = State::Period2;
                    }
                    b'*' => {
                        state = State::PeriodAsterisk;
                    }
                    _ => {
                        result.tag = Tag::Period;
                        break;
                    }
                },
                State::Period2 => match c {
                    b'.' => {
                        result.tag = Tag::Ellipsis3;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Ellipsis2;
                        break;
                    }
                },
                State::PeriodAsterisk => match c {
                    b'*' => {
                        result.tag = Tag::InvalidPeriodAsterisks;
                        break;
                    }
                    _ => {
                        result.tag = Tag::PeriodAsterisk;
                        break;
                    }
                },
                State::Slash => match c {
                    b'/' => {
                        state = State::LineCommentStart;
                    }
                    b'=' => {
                        result.tag = Tag::SlashEqual;
                        self.index += 1;
                        break;
                    }
                    _ => {
                        result.tag = Tag::Slash;
                        break;
                    }
                },
                State::LineCommentStart => match c {
                    0 => {
                        if self.index != self.buffer.len() {
                            result.tag = Tag::Invalid;
                            self.index += 1;
                        }
                        break;
                    }
                    b'/' => {
                        state = State::DocCommentStart;
                    }
                    b'!' => {
                        result.tag = Tag::ContainerDocComment;
                        state = State::DocComment;
                    }
                    b'\n' => {
                        state = State::Start;
                        result.loc.start = self.index + 1;
                    }
                    b'\t' => {
                        state = State::LineComment;
                    }
                    _ => {
                        state = State::LineComment;
                        self.check_literal_character();
                    }
                },
                State::DocCommentStart => match c {
                    b'/' => {
                        state = State::LineComment;
                    }
                    0 | b'\n' => {
                        result.tag = Tag::DocComment;
                        break;
                    }
                    b'\t' => {
                        state = State::DocComment;
                        result.tag = Tag::DocComment;
                    }
                    _ => {
                        state = State::DocComment;
                        result.tag = Tag::DocComment;
                        self.check_literal_character();
                    }
                },
                State::LineComment => match c {
                    0 => {
                        if self.index != self.buffer.len() {
                            result.tag = Tag::Invalid;
                            self.index += 1;
                        }
                        break;
                    }
                    b'\n' => {
                        state = State::Start;
                        result.loc.start = self.index + 1;
                    }
                    b'\t' => {}
                    _ => self.check_literal_character(),
                },
                State::DocComment => match c {
                    0 | b'\n' => break,
                    b'\t' => {}
                    _ => self.check_literal_character(),
                },
                State::Int => match c {
                    b'.' => {
                        state = State::IntPeriod;
                    }
                    b'_'
                    | b'a'..=b'd'
                    | b'f'..=b'o'
                    | b'q'..=b'z'
                    | b'A'..=b'D'
                    | b'F'..=b'O'
                    | b'Q'..=b'Z'
                    | b'0'..=b'9' => {}
                    b'e' | b'E' | b'p' | b'P' => {
                        state = State::IntExponent;
                    }
                    _ => break,
                },
                State::IntExponent => match c {
                    b'-' | b'+' => {
                        state = State::Float;
                    }
                    _ => {
                        self.index -= 1;
                        state = State::Int;
                    }
                },
                State::IntPeriod => match c {
                    b'_'
                    | b'a'..=b'd'
                    | b'f'..=b'o'
                    | b'q'..=b'z'
                    | b'A'..=b'D'
                    | b'F'..=b'O'
                    | b'Q'..=b'Z'
                    | b'0'..=b'9' => {
                        state = State::Float;
                    }
                    b'e' | b'E' | b'p' | b'P' => {
                        state = State::FloatExponent;
                    }
                    _ => {
                        self.index -= 1;
                        break;
                    }
                },
                State::Float => match c {
                    b'_'
                    | b'a'..=b'd'
                    | b'f'..=b'o'
                    | b'q'..=b'z'
                    | b'A'..=b'D'
                    | b'F'..=b'O'
                    | b'Q'..=b'Z'
                    | b'0'..=b'9' => {}
                    b'e' | b'E' | b'p' | b'P' => {
                        state = State::FloatExponent;
                    }
                    _ => break,
                },
                State::FloatExponent => match c {
                    b'-' | b'+' => {
                        state = State::Float;
                    }
                    _ => {
                        self.index -= 1;
                        state = State::Float;
                    }
                },
            }
            self.index += 1;
        }

        if result.tag == Tag::Eof {
            if let Some(token) = self.pending_invalid_token.take() {
                return token;
            }
            result.loc.start = self.index;
        }

        result.loc.end = self.index;
        result
    }

    fn check_literal_character(&mut self) {
        if self.pending_invalid_token.is_some() {
            return;
        }
        let invalid_length = self.get_invalid_character_length();
        if invalid_length == 0 {
            return;
        }
        self.pending_invalid_token = Some(Token {
            tag: Tag::Invalid,
            loc: Loc {
                start: self.index,
                end: self.index + invalid_length,
            },
        });
    }

    fn get_invalid_character_length(&mut self) -> usize {
        let c0 = self.buffer[self.index];
        if c0.is_ascii() {
            if c0 == b'\r' {
                if self.index + 1 < self.buffer.len() && self.buffer[self.index + 1] == b'\n' {
                    // Carriage returns are *only* allowed just before a linefeed as part of a CRLF pair, otherwise
                    // they constitute an illegal byte!
                    0
                } else {
                    1
                }
            } else if c0.is_ascii_control() {
                // ascii control codes are never allowed
                // (note that \n was checked before we got here)
                1
            } else {
                // looks fine to me.
                0
            }
        } else {
            // TODO(zig-rs): find a more efficient way to check if the following codepoint is valid

            // check utf8-encoded character.
            match std::str::from_utf8(&self.buffer[self.index..]) {
                Ok(s) => match s.chars().next() {
                    Some(c) => match c {
                        | '\u{0085}' // U+0085 (NEL)
                        | '\u{2028}' // U+2028 (LS)
                        | '\u{2029}' // U+2029 (PS)
                        => c.len_utf8(),
                        _ => {
                            self.index += c.len_utf8() - 1;
                            0
                        }
                    },
                    None => 1,
                },
                Err(_) => 1,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        test_tokenize(
            "test const else",
            &[Tag::KeywordTest, Tag::KeywordConst, Tag::KeywordElse],
        );
    }

    #[test]
    fn test_line_comment_followed_by_top_level_comptime() {
        test_tokenize(
            "// line comment\ncomptime {}\n",
            &[Tag::KeywordComptime, Tag::LBrace, Tag::RBrace],
        );
    }

    #[test]
    fn test_unknown_length_pointer_and_then_c_pointer() {
        test_tokenize(
            "[*]u8\n[*c]u8",
            &[
                Tag::LBracket,
                Tag::Asterisk,
                Tag::RBracket,
                Tag::Identifier,
                Tag::LBracket,
                Tag::Asterisk,
                Tag::Identifier,
                Tag::RBracket,
                Tag::Identifier,
            ],
        );
    }

    #[test]
    fn test_code_point_literal_with_hex_escape() {
        test_tokenize(r#"'\x1b'"#, &[Tag::CharLiteral]);
        test_tokenize(r#"'\x1'"#, &[Tag::Invalid, Tag::Invalid]);
    }

    #[test]
    fn test_newline_in_char_literal() {
        test_tokenize("'\n'", &[Tag::Invalid, Tag::Invalid]);
    }

    #[test]
    fn test_newline_in_string_literal() {
        test_tokenize("\"\n\"", &[Tag::Invalid, Tag::Invalid]);
    }

    #[test]
    fn test_code_point_literal_with_unicode_escapes() {
        // Valid unicode escapes
        test_tokenize(r#"'\u{3}'"#, &[Tag::CharLiteral]);
        test_tokenize(r#"'\u{01}'"#, &[Tag::CharLiteral]);
        test_tokenize(r#"'\u{2a}'"#, &[Tag::CharLiteral]);
        test_tokenize(r#"'\u{3f9}'"#, &[Tag::CharLiteral]);
        test_tokenize(r#"'\u{6E09aBc1523}'"#, &[Tag::CharLiteral]);
        test_tokenize(r#""\u{440}""#, &[Tag::StringLiteral]);

        // Invalid unicode escapes
        test_tokenize(r#"'\u'"#, &[Tag::Invalid]);
        test_tokenize(r#"'\u{{'"#, &[Tag::Invalid, Tag::Invalid]);
        test_tokenize(r#"'\u{}'"#, &[Tag::CharLiteral]);
        test_tokenize(r#"'\u{s}'"#, &[Tag::Invalid, Tag::Invalid]);
        test_tokenize(r#"'\u{2z}'"#, &[Tag::Invalid, Tag::Invalid]);
        test_tokenize(r#"'\u{4a'"#, &[Tag::Invalid]);

        // Test old-style unicode literals
        test_tokenize(r#"'\u0333'"#, &[Tag::Invalid, Tag::Invalid]);
        test_tokenize(
            r#"'\U0333'"#,
            &[Tag::Invalid, Tag::NumberLiteral, Tag::Invalid],
        );
    }

    #[test]
    fn test_code_point_literal_with_unicode_code_point() {
        test_tokenize("'💩'", &[Tag::CharLiteral]);
    }

    #[test]
    fn test_float_literal_e_exponent() {
        test_tokenize(
            "a = 4.94065645841246544177e-324;\n",
            &[
                Tag::Identifier,
                Tag::Equal,
                Tag::NumberLiteral,
                Tag::Semicolon,
            ],
        );
    }

    #[test]
    fn test_float_literal_p_exponent() {
        test_tokenize(
            "a = 0x1.a827999fcef32p+1022;\n",
            &[
                Tag::Identifier,
                Tag::Equal,
                Tag::NumberLiteral,
                Tag::Semicolon,
            ],
        );
    }

    #[test]
    fn test_chars() {
        test_tokenize("'c'", &[Tag::CharLiteral]);
    }

    #[test]
    fn test_invalid_token_characters() {
        test_tokenize("#", &[Tag::Invalid]);
        test_tokenize("`", &[Tag::Invalid]);
        test_tokenize("'c", &[Tag::Invalid]);
        test_tokenize("'", &[Tag::Invalid]);
        test_tokenize("''", &[Tag::Invalid, Tag::Invalid]);
    }

    #[test]
    fn test_invalid_literal_comment_characters() {
        test_tokenize("\"\x00\"", &[Tag::StringLiteral, Tag::Invalid]);
        test_tokenize("//\x00", &[Tag::Invalid]);
        test_tokenize("//\x1f", &[Tag::Invalid]);
        test_tokenize("//\x7f", &[Tag::Invalid]);
    }

    #[test]
    fn test_utf8() {
        test_tokenize_bytes(&[b'/', b'/', 0xc2, 0x80], &[]);
        test_tokenize_bytes(&[b'/', b'/', 0xf4, 0x8f, 0xbf, 0xbf], &[]);
    }

    #[test]
    fn test_invalid_utf8() {
        test_tokenize_bytes(&[b'/', b'/', 0x80], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xbf], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xf8], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xff], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xc2, 0xc0], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xe0], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xf0], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xf0, 0x90, 0x80, 0xc0], &[Tag::Invalid]);
    }

    #[test]
    fn test_illegal_unicode_codepoints() {
        // Unicode newline characters: U+0085, U+2028, U+2029
        test_tokenize_bytes(&[b'/', b'/', 0xc2, 0x84], &[]);
        test_tokenize_bytes(&[b'/', b'/', 0xc2, 0x85], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xc2, 0x86], &[]);
        test_tokenize_bytes(&[b'/', b'/', 0xe2, 0x80, 0xa7], &[]);
        test_tokenize_bytes(&[b'/', b'/', 0xe2, 0x80, 0xa8], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xe2, 0x80, 0xa9], &[Tag::Invalid]);
        test_tokenize_bytes(&[b'/', b'/', 0xe2, 0x80, 0xaa], &[]);
    }

    #[test]
    fn test_string_identifier_and_builtin_fns() {
        test_tokenize(
            r#"const @"if" = @import("std");"#,
            &[
                Tag::KeywordConst,
                Tag::Identifier,
                Tag::Equal,
                Tag::Builtin,
                Tag::LParen,
                Tag::StringLiteral,
                Tag::RParen,
                Tag::Semicolon,
            ],
        );
    }

    #[test]
    fn test_multiline_string_literal_with_literal_tab() {
        test_tokenize(r#"\\foo	bar"#, &[Tag::MultilineStringLiteralLine]);
    }

    #[test]
    fn test_comments_with_literal_tab() {
        test_tokenize(
            &[
                "//foo	bar",
                "//!foo	bar",
                "///foo	bar",
                "//	foo",
                "///	foo",
                "///	/foo",
            ]
            .join("\n"),
            &[
                Tag::ContainerDocComment,
                Tag::DocComment,
                Tag::DocComment,
                Tag::DocComment,
            ],
        );
    }

    #[test]
    fn test_pipe_and_then_invalid() {
        test_tokenize("||=", &[Tag::PipePipe, Tag::Equal]);
    }

    #[test]
    fn test_line_comment_and_doc_comment() {
        test_tokenize("//", &[]);
        test_tokenize("// a / b", &[]);
        test_tokenize("// /", &[]);
        test_tokenize("/// a", &[Tag::DocComment]);
        test_tokenize("///", &[Tag::DocComment]);
        test_tokenize("////", &[]);
        test_tokenize("//!", &[Tag::ContainerDocComment]);
        test_tokenize("//!!", &[Tag::ContainerDocComment]);
    }

    #[test]
    fn test_line_comment_followed_by_identifier() {
        test_tokenize(
            "    Unexpected,\n    // another\n    Another,",
            &[Tag::Identifier, Tag::Comma, Tag::Identifier, Tag::Comma],
        );
    }

    #[test]
    fn test_utf8_bom_is_recognized_and_skipped() {
        test_tokenize_bytes(
            &[0xEF, 0xBB, 0xBF, b'a', b';', b'\n'],
            &[Tag::Identifier, Tag::Semicolon],
        );
    }

    #[test]
    fn test_correctly_parse_pointer_assignment() {
        test_tokenize(
            "b.*=3;\n",
            &[
                Tag::Identifier,
                Tag::PeriodAsterisk,
                Tag::Equal,
                Tag::NumberLiteral,
                Tag::Semicolon,
            ],
        );
    }

    #[test]
    fn test_correctly_parse_pointer_dereference_followed_by_asterisk() {
        test_tokenize(
            "\"b\".* ** 10",
            &[
                Tag::StringLiteral,
                Tag::PeriodAsterisk,
                Tag::AsteriskAsterisk,
                Tag::NumberLiteral,
            ],
        );
        test_tokenize(
            "(\"b\".*)** 10",
            &[
                Tag::LParen,
                Tag::StringLiteral,
                Tag::PeriodAsterisk,
                Tag::RParen,
                Tag::AsteriskAsterisk,
                Tag::NumberLiteral,
            ],
        );
        test_tokenize(
            "\"b\".*** 10",
            &[
                Tag::StringLiteral,
                Tag::InvalidPeriodAsterisks,
                Tag::AsteriskAsterisk,
                Tag::NumberLiteral,
            ],
        );
    }

    #[test]
    fn test_range_literals() {
        test_tokenize(
            "0...9",
            &[Tag::NumberLiteral, Tag::Ellipsis3, Tag::NumberLiteral],
        );
        test_tokenize(
            "'0'...'9'",
            &[Tag::CharLiteral, Tag::Ellipsis3, Tag::CharLiteral],
        );
        test_tokenize(
            "0x00...0x09",
            &[Tag::NumberLiteral, Tag::Ellipsis3, Tag::NumberLiteral],
        );
        test_tokenize(
            "0b00...0b11",
            &[Tag::NumberLiteral, Tag::Ellipsis3, Tag::NumberLiteral],
        );
        test_tokenize(
            "0o00...0o11",
            &[Tag::NumberLiteral, Tag::Ellipsis3, Tag::NumberLiteral],
        );
    }

    #[test]
    fn test_number_literals_decimal() {
        test_tokenize("0", &[Tag::NumberLiteral]);
        test_tokenize("1", &[Tag::NumberLiteral]);
        test_tokenize("2", &[Tag::NumberLiteral]);
        test_tokenize("3", &[Tag::NumberLiteral]);
        test_tokenize("4", &[Tag::NumberLiteral]);
        test_tokenize("5", &[Tag::NumberLiteral]);
        test_tokenize("6", &[Tag::NumberLiteral]);
        test_tokenize("7", &[Tag::NumberLiteral]);
        test_tokenize("8", &[Tag::NumberLiteral]);
        test_tokenize("9", &[Tag::NumberLiteral]);
        test_tokenize("1..", &[Tag::NumberLiteral, Tag::Ellipsis2]);
        test_tokenize("0a", &[Tag::NumberLiteral]);
        test_tokenize("9b", &[Tag::NumberLiteral]);
        test_tokenize("1z", &[Tag::NumberLiteral]);
        test_tokenize("1z_1", &[Tag::NumberLiteral]);
        test_tokenize("9z3", &[Tag::NumberLiteral]);

        test_tokenize("0_0", &[Tag::NumberLiteral]);
        test_tokenize("0001", &[Tag::NumberLiteral]);
        test_tokenize("01234567890", &[Tag::NumberLiteral]);
        test_tokenize("012_345_6789_0", &[Tag::NumberLiteral]);
        test_tokenize("0_1_2_3_4_5_6_7_8_9_0", &[Tag::NumberLiteral]);

        test_tokenize("00_", &[Tag::NumberLiteral]);
        test_tokenize("0_0_", &[Tag::NumberLiteral]);
        test_tokenize("0__0", &[Tag::NumberLiteral]);
        test_tokenize("0_0f", &[Tag::NumberLiteral]);
        test_tokenize("0_0_f", &[Tag::NumberLiteral]);
        test_tokenize("0_0_f_00", &[Tag::NumberLiteral]);
        test_tokenize("1_,", &[Tag::NumberLiteral, Tag::Comma]);

        test_tokenize("0.0", &[Tag::NumberLiteral]);
        test_tokenize("1.0", &[Tag::NumberLiteral]);
        test_tokenize("10.0", &[Tag::NumberLiteral]);
        test_tokenize("0e0", &[Tag::NumberLiteral]);
        test_tokenize("1e0", &[Tag::NumberLiteral]);
        test_tokenize("1e100", &[Tag::NumberLiteral]);
        test_tokenize("1.0e100", &[Tag::NumberLiteral]);
        test_tokenize("1.0e+100", &[Tag::NumberLiteral]);
        test_tokenize("1.0e-100", &[Tag::NumberLiteral]);
        test_tokenize("1_0_0_0.0_0_0_0_0_1e1_0_0_0", &[Tag::NumberLiteral]);

        test_tokenize("1.", &[Tag::NumberLiteral, Tag::Period]);
        test_tokenize("1e", &[Tag::NumberLiteral]);
        test_tokenize("1.e100", &[Tag::NumberLiteral]);
        test_tokenize("1.0e1f0", &[Tag::NumberLiteral]);
        test_tokenize("1.0p100", &[Tag::NumberLiteral]);
        test_tokenize("1.0p-100", &[Tag::NumberLiteral]);
        test_tokenize("1.0p1f0", &[Tag::NumberLiteral]);
        test_tokenize("1.0_,", &[Tag::NumberLiteral, Tag::Comma]);
        test_tokenize("1_.0", &[Tag::NumberLiteral]);
        test_tokenize("1._", &[Tag::NumberLiteral]);
        test_tokenize("1.a", &[Tag::NumberLiteral]);
        test_tokenize("1.z", &[Tag::NumberLiteral]);
        test_tokenize("1._0", &[Tag::NumberLiteral]);
        test_tokenize("1.+", &[Tag::NumberLiteral, Tag::Period, Tag::Plus]);
        test_tokenize("1._+", &[Tag::NumberLiteral, Tag::Plus]);
        test_tokenize("1._e", &[Tag::NumberLiteral]);
        test_tokenize("1.0e", &[Tag::NumberLiteral]);
        test_tokenize("1.0e,", &[Tag::NumberLiteral, Tag::Comma]);
        test_tokenize("1.0e_", &[Tag::NumberLiteral]);
        test_tokenize("1.0e+_", &[Tag::NumberLiteral]);
        test_tokenize("1.0e-_", &[Tag::NumberLiteral]);
        test_tokenize("1.0e0_+", &[Tag::NumberLiteral, Tag::Plus]);
    }

    #[test]
    fn test_number_literals_binary() {
        test_tokenize("0b0", &[Tag::NumberLiteral]);
        test_tokenize("0b1", &[Tag::NumberLiteral]);
        test_tokenize("0b2", &[Tag::NumberLiteral]);
        test_tokenize("0b3", &[Tag::NumberLiteral]);
        test_tokenize("0b4", &[Tag::NumberLiteral]);
        test_tokenize("0b5", &[Tag::NumberLiteral]);
        test_tokenize("0b6", &[Tag::NumberLiteral]);
        test_tokenize("0b7", &[Tag::NumberLiteral]);
        test_tokenize("0b8", &[Tag::NumberLiteral]);
        test_tokenize("0b9", &[Tag::NumberLiteral]);
        test_tokenize("0ba", &[Tag::NumberLiteral]);
        test_tokenize("0bb", &[Tag::NumberLiteral]);
        test_tokenize("0bc", &[Tag::NumberLiteral]);
        test_tokenize("0bd", &[Tag::NumberLiteral]);
        test_tokenize("0be", &[Tag::NumberLiteral]);
        test_tokenize("0bf", &[Tag::NumberLiteral]);
        test_tokenize("0bz", &[Tag::NumberLiteral]);

        test_tokenize("0b0000_0000", &[Tag::NumberLiteral]);
        test_tokenize("0b1111_1111", &[Tag::NumberLiteral]);
        test_tokenize("0b10_10_10_10", &[Tag::NumberLiteral]);
        test_tokenize("0b0_1_0_1_0_1_0_1", &[Tag::NumberLiteral]);
        test_tokenize("0b1.", &[Tag::NumberLiteral, Tag::Period]);
        test_tokenize("0b1.0", &[Tag::NumberLiteral]);

        test_tokenize("0B0", &[Tag::NumberLiteral]);
        test_tokenize("0b_", &[Tag::NumberLiteral]);
        test_tokenize("0b_0", &[Tag::NumberLiteral]);
        test_tokenize("0b1_", &[Tag::NumberLiteral]);
        test_tokenize("0b0__1", &[Tag::NumberLiteral]);
        test_tokenize("0b0_1_", &[Tag::NumberLiteral]);
        test_tokenize("0b1e", &[Tag::NumberLiteral]);
        test_tokenize("0b1p", &[Tag::NumberLiteral]);
        test_tokenize("0b1e0", &[Tag::NumberLiteral]);
        test_tokenize("0b1p0", &[Tag::NumberLiteral]);
        test_tokenize("0b1_,", &[Tag::NumberLiteral, Tag::Comma]);
    }

    #[test]
    fn test_number_literals_octal() {
        test_tokenize("0o0", &[Tag::NumberLiteral]);
        test_tokenize("0o1", &[Tag::NumberLiteral]);
        test_tokenize("0o2", &[Tag::NumberLiteral]);
        test_tokenize("0o3", &[Tag::NumberLiteral]);
        test_tokenize("0o4", &[Tag::NumberLiteral]);
        test_tokenize("0o5", &[Tag::NumberLiteral]);
        test_tokenize("0o6", &[Tag::NumberLiteral]);
        test_tokenize("0o7", &[Tag::NumberLiteral]);
        test_tokenize("0o8", &[Tag::NumberLiteral]);
        test_tokenize("0o9", &[Tag::NumberLiteral]);
        test_tokenize("0oa", &[Tag::NumberLiteral]);
        test_tokenize("0ob", &[Tag::NumberLiteral]);
        test_tokenize("0oc", &[Tag::NumberLiteral]);
        test_tokenize("0od", &[Tag::NumberLiteral]);
        test_tokenize("0oe", &[Tag::NumberLiteral]);
        test_tokenize("0of", &[Tag::NumberLiteral]);
        test_tokenize("0oz", &[Tag::NumberLiteral]);

        test_tokenize("0o01234567", &[Tag::NumberLiteral]);
        test_tokenize("0o0123_4567", &[Tag::NumberLiteral]);
        test_tokenize("0o01_23_45_67", &[Tag::NumberLiteral]);
        test_tokenize("0o0_1_2_3_4_5_6_7", &[Tag::NumberLiteral]);
        test_tokenize("0o7.", &[Tag::NumberLiteral, Tag::Period]);
        test_tokenize("0o7.0", &[Tag::NumberLiteral]);

        test_tokenize("0O0", &[Tag::NumberLiteral]);
        test_tokenize("0o_", &[Tag::NumberLiteral]);
        test_tokenize("0o_0", &[Tag::NumberLiteral]);
        test_tokenize("0o1_", &[Tag::NumberLiteral]);
        test_tokenize("0o0__1", &[Tag::NumberLiteral]);
        test_tokenize("0o0_1_", &[Tag::NumberLiteral]);
        test_tokenize("0o1e", &[Tag::NumberLiteral]);
        test_tokenize("0o1p", &[Tag::NumberLiteral]);
        test_tokenize("0o1e0", &[Tag::NumberLiteral]);
        test_tokenize("0o1p0", &[Tag::NumberLiteral]);
        test_tokenize("0o_,", &[Tag::NumberLiteral, Tag::Comma]);
    }

    #[test]
    fn test_number_literals_hexadecimal() {
        test_tokenize("0x0", &[Tag::NumberLiteral]);
        test_tokenize("0x1", &[Tag::NumberLiteral]);
        test_tokenize("0x2", &[Tag::NumberLiteral]);
        test_tokenize("0x3", &[Tag::NumberLiteral]);
        test_tokenize("0x4", &[Tag::NumberLiteral]);
        test_tokenize("0x5", &[Tag::NumberLiteral]);
        test_tokenize("0x6", &[Tag::NumberLiteral]);
        test_tokenize("0x7", &[Tag::NumberLiteral]);
        test_tokenize("0x8", &[Tag::NumberLiteral]);
        test_tokenize("0x9", &[Tag::NumberLiteral]);
        test_tokenize("0xa", &[Tag::NumberLiteral]);
        test_tokenize("0xb", &[Tag::NumberLiteral]);
        test_tokenize("0xc", &[Tag::NumberLiteral]);
        test_tokenize("0xd", &[Tag::NumberLiteral]);
        test_tokenize("0xe", &[Tag::NumberLiteral]);
        test_tokenize("0xf", &[Tag::NumberLiteral]);
        test_tokenize("0xA", &[Tag::NumberLiteral]);
        test_tokenize("0xB", &[Tag::NumberLiteral]);
        test_tokenize("0xC", &[Tag::NumberLiteral]);
        test_tokenize("0xD", &[Tag::NumberLiteral]);
        test_tokenize("0xE", &[Tag::NumberLiteral]);
        test_tokenize("0xF", &[Tag::NumberLiteral]);
        test_tokenize("0x0z", &[Tag::NumberLiteral]);
        test_tokenize("0xz", &[Tag::NumberLiteral]);

        test_tokenize("0x0123456789ABCDEF", &[Tag::NumberLiteral]);
        test_tokenize("0x0123_4567_89AB_CDEF", &[Tag::NumberLiteral]);
        test_tokenize("0x01_23_45_67_89AB_CDE_F", &[Tag::NumberLiteral]);
        test_tokenize("0x0_1_2_3_4_5_6_7_8_9_A_B_C_D_E_F", &[Tag::NumberLiteral]);

        test_tokenize("0X0", &[Tag::NumberLiteral]);
        test_tokenize("0x_", &[Tag::NumberLiteral]);
        test_tokenize("0x_1", &[Tag::NumberLiteral]);
        test_tokenize("0x1_", &[Tag::NumberLiteral]);
        test_tokenize("0x0__1", &[Tag::NumberLiteral]);
        test_tokenize("0x0_1_", &[Tag::NumberLiteral]);
        test_tokenize("0x_,", &[Tag::NumberLiteral, Tag::Comma]);

        test_tokenize("0x1.0", &[Tag::NumberLiteral]);
        test_tokenize("0xF.0", &[Tag::NumberLiteral]);
        test_tokenize("0xF.F", &[Tag::NumberLiteral]);
        test_tokenize("0xF.Fp0", &[Tag::NumberLiteral]);
        test_tokenize("0xF.FP0", &[Tag::NumberLiteral]);
        test_tokenize("0x1p0", &[Tag::NumberLiteral]);
        test_tokenize("0xfp0", &[Tag::NumberLiteral]);
        test_tokenize(
            "0x1.0+0xF.0",
            &[Tag::NumberLiteral, Tag::Plus, Tag::NumberLiteral],
        );

        test_tokenize("0x1.", &[Tag::NumberLiteral, Tag::Period]);
        test_tokenize("0xF.", &[Tag::NumberLiteral, Tag::Period]);
        test_tokenize(
            "0x1.+0xF.",
            &[
                Tag::NumberLiteral,
                Tag::Period,
                Tag::Plus,
                Tag::NumberLiteral,
                Tag::Period,
            ],
        );
        test_tokenize("0xff.p10", &[Tag::NumberLiteral]);

        test_tokenize("0x0123456.789ABCDEF", &[Tag::NumberLiteral]);
        test_tokenize("0x0_123_456.789_ABC_DEF", &[Tag::NumberLiteral]);
        test_tokenize("0x0_1_2_3_4_5_6.7_8_9_A_B_C_D_E_F", &[Tag::NumberLiteral]);
        test_tokenize("0x0p0", &[Tag::NumberLiteral]);
        test_tokenize("0x0.0p0", &[Tag::NumberLiteral]);
        test_tokenize("0xff.ffp10", &[Tag::NumberLiteral]);
        test_tokenize("0xff.ffP10", &[Tag::NumberLiteral]);
        test_tokenize("0xffp10", &[Tag::NumberLiteral]);
        test_tokenize("0xff_ff.ff_ffp1_0_0_0", &[Tag::NumberLiteral]);
        test_tokenize("0xf_f_f_f.f_f_f_fp+1_000", &[Tag::NumberLiteral]);
        test_tokenize("0xf_f_f_f.f_f_f_fp-1_00_0", &[Tag::NumberLiteral]);

        test_tokenize("0x1e", &[Tag::NumberLiteral]);
        test_tokenize("0x1e0", &[Tag::NumberLiteral]);
        test_tokenize("0x1p", &[Tag::NumberLiteral]);
        test_tokenize("0xfp0z1", &[Tag::NumberLiteral]);
        test_tokenize("0xff.ffpff", &[Tag::NumberLiteral]);
        test_tokenize("0x0.p", &[Tag::NumberLiteral]);
        test_tokenize("0x0.z", &[Tag::NumberLiteral]);
        test_tokenize("0x0._", &[Tag::NumberLiteral]);
        test_tokenize("0x0_.0", &[Tag::NumberLiteral]);
        test_tokenize(
            "0x0_.0.0",
            &[Tag::NumberLiteral, Tag::Period, Tag::NumberLiteral],
        );
        test_tokenize("0x0._0", &[Tag::NumberLiteral]);
        test_tokenize("0x0.0_", &[Tag::NumberLiteral]);
        test_tokenize("0x0_p0", &[Tag::NumberLiteral]);
        test_tokenize("0x0_.p0", &[Tag::NumberLiteral]);
        test_tokenize("0x0._p0", &[Tag::NumberLiteral]);
        test_tokenize("0x0.0_p0", &[Tag::NumberLiteral]);
        test_tokenize("0x0._0p0", &[Tag::NumberLiteral]);
        test_tokenize("0x0.0p_0", &[Tag::NumberLiteral]);
        test_tokenize("0x0.0p+_0", &[Tag::NumberLiteral]);
        test_tokenize("0x0.0p-_0", &[Tag::NumberLiteral]);
        test_tokenize("0x0.0p0_", &[Tag::NumberLiteral]);
    }

    #[test]
    fn test_multi_line_string_literal_with_only_1_backslash() {
        test_tokenize("x \\\n;", &[Tag::Identifier, Tag::Invalid, Tag::Semicolon]);
    }

    #[test]
    fn test_invalid_builtin_identifiers() {
        test_tokenize("@()", &[Tag::Invalid, Tag::LParen, Tag::RParen]);
        test_tokenize(
            "@0()",
            &[Tag::Invalid, Tag::NumberLiteral, Tag::LParen, Tag::RParen],
        );
    }

    #[test]
    fn test_invalid_token_with_unfinished_escape_right_before_eof() {
        test_tokenize("\"\\", &[Tag::Invalid]);
        test_tokenize("'\\", &[Tag::Invalid]);
        test_tokenize("'\\u", &[Tag::Invalid]);
    }

    #[test]
    fn test_saturating_operators() {
        test_tokenize("<<", &[Tag::AngleBracketAngleBracketLeft]);
        test_tokenize("<<|", &[Tag::AngleBracketAngleBracketLeftPipe]);
        test_tokenize("<<|=", &[Tag::AngleBracketAngleBracketLeftPipeEqual]);

        test_tokenize("*", &[Tag::Asterisk]);
        test_tokenize("*|", &[Tag::AsteriskPipe]);
        test_tokenize("*|=", &[Tag::AsteriskPipeEqual]);

        test_tokenize("+", &[Tag::Plus]);
        test_tokenize("+|", &[Tag::PlusPipe]);
        test_tokenize("+|=", &[Tag::PlusPipeEqual]);

        test_tokenize("-", &[Tag::Minus]);
        test_tokenize("-|", &[Tag::MinusPipe]);
        test_tokenize("-|=", &[Tag::MinusPipeEqual]);
    }

    #[test]
    fn test_null_byte_before_eof() {
        test_tokenize(
            "123 \x00 456",
            &[Tag::NumberLiteral, Tag::Invalid, Tag::NumberLiteral],
        );
        test_tokenize("//\x00", &[Tag::Invalid]);
        test_tokenize("\\\\\x00", &[Tag::MultilineStringLiteralLine, Tag::Invalid]);
        test_tokenize("\x00", &[Tag::Invalid]);
        test_tokenize("// NUL\x00\n", &[Tag::Invalid]);
        test_tokenize("///\x00\n", &[Tag::DocComment, Tag::Invalid]);
        test_tokenize("/// NUL\x00\n", &[Tag::DocComment, Tag::Invalid]);
    }

    fn test_tokenize(source: &str, expected_token_tags: &[Tag]) {
        test_tokenize_bytes(source.as_bytes(), expected_token_tags);
    }

    fn test_tokenize_bytes(source: &[u8], expected_token_tags: &[Tag]) {
        let mut tokenizer = Tokenizer::new(source);
        for &expected_token_tag in expected_token_tags {
            let token = tokenizer.next();
            assert_eq!(expected_token_tag, token.tag);
        }
        let last_token = tokenizer.next();
        assert_eq!(Tag::Eof, last_token.tag);
        assert_eq!(source.len(), last_token.loc.start);
        assert_eq!(source.len(), last_token.loc.end);
    }
}
