use super::*;

pub struct Error {
    pub tag: Tag,
    pub is_note: bool,
    /// True if `token` points to the token before the token causing an issue.
    pub token_is_prev: bool,
    pub token: TokenIndex,
}

impl Default for Error {
    fn default() -> Error {
        Error {
            tag: Tag::Unknown,
            is_note: false,
            token_is_prev: false,
            token: 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Tag {
    Unknown,

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
