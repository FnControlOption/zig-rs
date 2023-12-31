use super::*;
use std::borrow::Cow;

impl Ast<'_> {
    /// Returns an extra offset for column and byte offset of errors that
    /// should point after the token in the error message.
    pub fn error_offset(&self, parse_error: &Error) -> u32 {
        if parse_error.token_is_prev {
            self.token_slice(parse_error.token).len() as u32
        } else {
            0
        }
    }

    pub fn render_error(&self, parse_error: &Error) -> Cow<'static, str> {
        match parse_error.tag {
            Tag::AsteriskAfterPtrDeref => {
                Cow::from("'.*' cannot be followed by '*'. Are you missing a space?")
            }
            Tag::ChainedComparisonOperators => Cow::from("comparison operators cannot be chained"),
            Tag::DeclBetweenFields => {
                Cow::from("declarations are not allowed between container fields")
            }
            Tag::ExpectedBlock => Cow::from(format!(
                "expected block, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedBlockOrAssignment => Cow::from(format!(
                "expected block or assignment, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedBlockOrExpr => Cow::from(format!(
                "expected block or expression, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedBlockOrField => Cow::from(format!(
                "expected block or field, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedContainerMembers => Cow::from(format!(
                "expected test, comptime, var decl, or container field, found '{}'",
                self.token_tag(parse_error.token).symbol(),
            )),
            Tag::ExpectedExpr => Cow::from(format!(
                "expected expression, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedExprOrAssignment => Cow::from(format!(
                "expected expression or assignment, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedExprOrVarDecl => Cow::from(format!(
                "expected expression or var decl, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedFn => Cow::from(format!(
                "expected function, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedInlinable => Cow::from(format!(
                "expected 'while' or 'for', found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedLabelable => Cow::from(format!(
                "expected 'while', 'for', 'inline', or '{{', found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedParamList => Cow::from(format!(
                "expected parameter list, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedPrefixExpr => Cow::from(format!(
                "expected prefix expression, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedPrimaryTypeExpr => Cow::from(format!(
                "expected primary type expression, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedPubItem => Cow::from(format!(
                "expected function or variable declaration after pub"
            )),
            Tag::ExpectedReturnType => Cow::from(format!(
                "expected return type expression, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedSemiOrElse => Cow::from("expected ';' or 'else' after statement"),
            Tag::ExpectedSemiOrLBrace => {
                Cow::from("expected ';' or block after function prototype")
            }
            Tag::ExpectedStatement => Cow::from(format!(
                "expected statement, found '{}'",
                self.token_tag(parse_error.token).symbol(),
            )),
            Tag::ExpectedSuffixOp => Cow::from(format!(
                "expected pointer dereference, optional unwrap, or field access, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedTypeExpr => Cow::from(format!(
                "expected type expression, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedVarDecl => Cow::from(format!(
                "expected variable declaration, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedVarDeclOrFn => Cow::from(format!(
                "expected variable declaration or function, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedLoopPayload => Cow::from(format!(
                "expected loop payload, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExpectedContainer => Cow::from(format!(
                "expected a struct, enum or union, found '{}'",
                self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex)
                    .symbol(),
            )),
            Tag::ExternFnBody => Cow::from("extern functions have no body"),
            Tag::ExtraAddrspaceQualifier => Cow::from("extra addrspace qualifier"),
            Tag::ExtraAlignQualifier => Cow::from("extra align qualifier"),
            Tag::ExtraAllowzeroQualifier => Cow::from("extra allowzero qualifier"),
            Tag::ExtraConstQualifier => Cow::from("extra const qualifier"),
            Tag::ExtraVolatileQualifier => Cow::from("extra volatile qualifier"),
            Tag::PtrModOnArrayChildType => Cow::from(format!(
                "pointer modifier '{}' not allowed on array child type",
                self.token_tag(parse_error.token).symbol(),
            )),
            Tag::InvalidBitRange => Cow::from("bit range not allowed on slices and arrays"),
            Tag::SameLineDocComment => Cow::from("same line documentation comment"),
            Tag::UnattachedDocComment => Cow::from("unattached documentation comment"),
            Tag::TestDocComment => Cow::from("documentation comments cannot be attached to tests"),
            Tag::ComptimeDocComment => {
                Cow::from("documentation comments cannot be attached to comptime blocks")
            }
            Tag::VarargsNonfinal => Cow::from("function prototype has parameter after varargs"),
            Tag::ExpectedContinueExpr => Cow::from("expected ':' before while continue expression"),

            Tag::ExpectedSemiAfterDecl => Cow::from("expected ';' after declaration"),
            Tag::ExpectedSemiAfterStmt => Cow::from("expected ';' after statement"),
            Tag::ExpectedCommaAfterField => Cow::from("expected ',' after field"),
            Tag::ExpectedCommaAfterArg => Cow::from("expected ',' after argument"),
            Tag::ExpectedCommaAfterParam => Cow::from("expected ',' after parameter"),
            Tag::ExpectedCommaAfterInitializer => Cow::from("expected ',' after initializer"),
            Tag::ExpectedCommaAfterSwitchProng => Cow::from("expected ',' after switch prong"),
            Tag::ExpectedCommaAfterForOperand => Cow::from("expected ',' after for operand"),
            Tag::ExpectedCommaAfterCapture => Cow::from("expected ',' after for capture"),
            Tag::ExpectedInitializer => Cow::from("expected field initializer"),
            Tag::MismatchedBinaryOpWhitespace => Cow::from(format!(
                "binary operator `{}` has whitespace on one side, but not the other.",
                self.token_tag(parse_error.token).symbol()
            )),
            Tag::InvalidAmpersandAmpersand => {
                // rustfmt gives up on chains if any line is too long
                // https://github.com/rust-lang/rustfmt/issues/3863
                const MESSAGE: &str = "ambiguous use of '&&'; use 'and' for logical AND, or change whitespace to ' & &' for bitwise AND";
                Cow::from(MESSAGE)
            }
            Tag::CStyleContainer(expected_tag) => Cow::from(format!(
                "'{} {}' is invalid",
                expected_tag.symbol(),
                String::from_utf8_lossy(self.token_slice(parse_error.token)),
            )),
            Tag::ZigStyleContainer(expected_tag) => Cow::from(format!(
                "to declare a container do 'const {} = {}'",
                String::from_utf8_lossy(self.token_slice(parse_error.token)),
                expected_tag.symbol(),
            )),
            Tag::PreviousField => Cow::from("field before declarations here"),
            Tag::NextField => Cow::from("field after declarations here"),
            Tag::ExpectedVarConst => {
                Cow::from("expected 'var' or 'const' before variable declaration")
            }
            Tag::WrongEqualVarDecl => Cow::from("variable initialized with '==' instead of '='"),
            Tag::VarConstDecl => Cow::from("use 'var' or 'const' to declare variable"),
            Tag::ExtraForCapture => Cow::from("extra capture in for loop"),
            Tag::ForInputNotCaptured => Cow::from("for input is not captured"),

            Tag::ExpectedToken(expected_tag) => {
                let found_tag =
                    self.token_tag(parse_error.token + parse_error.token_is_prev as TokenIndex);
                let expected_symbol = expected_tag.symbol();
                match found_tag {
                    token::Tag::Invalid => Cow::from(format!(
                        "expected '{}', found invalid bytes",
                        expected_symbol
                    )),
                    _ => Cow::from(format!(
                        "expected '{}', found '{}'",
                        expected_symbol,
                        found_tag.symbol()
                    )),
                }
            }
        }
    }
}

pub struct Error {
    pub tag: Tag,
    pub is_note: bool,
    /// True if `token` points to the token before the token causing an issue.
    pub token_is_prev: bool,
    pub token: TokenIndex,
}

impl Error {
    pub fn new(tag: Tag, token: TokenIndex) -> Error {
        Error {
            tag,
            is_note: false,
            token_is_prev: false,
            token,
        }
    }

    pub fn note(tag: Tag, token: TokenIndex) -> Error {
        Error {
            tag,
            is_note: true,
            token_is_prev: false,
            token,
        }
    }

    pub fn display<'err, 'file, 'ast, 'src>(
        &'err self,
        filename: &'file str,
        tree: &'ast Ast<'src>,
    ) -> Display<'err, 'file, 'ast, 'src> {
        Display {
            error: self,
            filename,
            tree,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)] // Goal is to keep this under one byte for efficiency.
pub enum Tag {
    AsteriskAfterPtrDeref,
    ChainedComparisonOperators,
    DeclBetweenFields,
    ExpectedBlock,
    ExpectedBlockOrAssignment,
    ExpectedBlockOrExpr,
    ExpectedBlockOrField,
    ExpectedContainerMembers,
    ExpectedExpr,
    ExpectedExprOrAssignment,
    ExpectedExprOrVarDecl,
    ExpectedFn,
    ExpectedInlinable,
    ExpectedLabelable,
    ExpectedParamList,
    ExpectedPrefixExpr,
    ExpectedPrimaryTypeExpr,
    ExpectedPubItem,
    ExpectedReturnType,
    ExpectedSemiOrElse,
    ExpectedSemiOrLBrace,
    ExpectedStatement,
    ExpectedSuffixOp,
    ExpectedTypeExpr,
    ExpectedVarDecl,
    ExpectedVarDeclOrFn,
    ExpectedLoopPayload,
    ExpectedContainer,
    ExternFnBody,
    ExtraAddrspaceQualifier,
    ExtraAlignQualifier,
    ExtraAllowzeroQualifier,
    ExtraConstQualifier,
    ExtraVolatileQualifier,
    PtrModOnArrayChildType,
    InvalidBitRange,
    SameLineDocComment,
    UnattachedDocComment,
    TestDocComment,
    ComptimeDocComment,
    VarargsNonfinal,
    ExpectedContinueExpr,
    ExpectedSemiAfterDecl,
    ExpectedSemiAfterStmt,
    ExpectedCommaAfterField,
    ExpectedCommaAfterArg,
    ExpectedCommaAfterParam,
    ExpectedCommaAfterInitializer,
    ExpectedCommaAfterSwitchProng,
    ExpectedCommaAfterForOperand,
    ExpectedCommaAfterCapture,
    ExpectedInitializer,
    MismatchedBinaryOpWhitespace,
    InvalidAmpersandAmpersand,
    CStyleContainer(token::Tag),
    ExpectedVarConst,
    WrongEqualVarDecl,
    VarConstDecl,
    ExtraForCapture,
    ForInputNotCaptured,

    ZigStyleContainer(token::Tag),
    PreviousField,
    NextField,

    ExpectedToken(token::Tag),
}

pub struct Display<'err, 'file, 'ast, 'src> {
    error: &'err Error,
    filename: &'file str,
    tree: &'ast Ast<'src>,
}

impl std::fmt::Display for Display<'_, '_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let filename = self.filename;
        let loc = self.tree.token_location(0, self.error.token);
        let line = loc.line + 1;
        let column = loc.column + 1 + self.tree.error_offset(self.error) as usize;
        let message = self.tree.render_error(self.error);
        write!(f, "{filename}:{line}:{column}: {message}")
    }
}
