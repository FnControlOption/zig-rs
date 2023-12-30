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
        write!(f, "{filename}:{line}:{column}: ")?;
        self.tree.render_error(self.error, f)
    }
}

impl Error {
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
