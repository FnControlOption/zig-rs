use std::borrow::Cow;

use super::*;

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
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
        let s = self.error.render(self.tree);
        write!(f, "{filename}:{line}:{column}: {s}")
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

    pub fn render(&self, tree: &Ast) -> Cow<'static, str> {
        match self.tag {
            Tag::AsteriskAfterPtrDeref => {
                Cow::from("'.*' cannot be followed by '*'. Are you missing a space?")
            }
            Tag::ChainedComparisonOperators => Cow::from("comparison operators cannot be chained"),
            Tag::DeclBetweenFields => {
                Cow::from("declarations are not allowed between container fields")
            }
            Tag::ExpectedBlock => Cow::from(format!(
                "expected block, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedBlockOrAssignment => Cow::from(format!(
                "expected block or assignment, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedBlockOrExpr => Cow::from(format!(
                "expected block or expression, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedBlockOrField => Cow::from(format!(
                "expected block or field, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedContainerMembers => Cow::from(format!(
                "expected test, comptime, var decl, or container field, found '{}'",
                tree.token_tag(self.token).symbol(),
            )),
            Tag::ExpectedExpr => Cow::from(format!(
                "expected expression, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedExprOrAssignment => Cow::from(format!(
                "expected expression or assignment, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedExprOrVarDecl => Cow::from(format!(
                "expected expression or var decl, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedFn => Cow::from(format!(
                "expected function, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedInlinable => Cow::from(format!(
                "expected 'while' or 'for', found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedLabelable => Cow::from(format!(
                "expected 'while', 'for', 'inline', or '{{', found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedParamList => Cow::from(format!(
                "expected parameter list, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedPrefixExpr => Cow::from(format!(
                "expected prefix expression, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedPrimaryTypeExpr => Cow::from(format!(
                "expected primary type expression, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedPubItem => Cow::from(format!(
                "expected function or variable declaration after pub"
            )),
            Tag::ExpectedReturnType => Cow::from(format!(
                "expected return type expression, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedSemiOrElse => Cow::from("expected ';' or 'else' after statement"),
            Tag::ExpectedSemiOrLBrace => {
                Cow::from("expected ';' or block after function prototype")
            }
            Tag::ExpectedStatement => Cow::from(format!(
                "expected statement, found '{}'",
                tree.token_tag(self.token).symbol(),
            )),
            Tag::ExpectedSuffixOp => Cow::from(format!(
                "expected pointer dereference, optional unwrap, or field access, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedTypeExpr => Cow::from(format!(
                "expected type expression, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedVarDecl => Cow::from(format!(
                "expected variable declaration, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedVarDeclOrFn => Cow::from(format!(
                "expected variable declaration or function, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedLoopPayload => Cow::from(format!(
                "expected loop payload, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
                .symbol(),
            )),
            Tag::ExpectedContainer => Cow::from(format!(
                "expected a struct, enum or union, found '{}'",
                tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                })
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
                tree.token_tag(self.token).symbol(),
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
                tree.token_tag(self.token).symbol()
            )),
            Tag::InvalidAmpersandAmpersand => {
                // rustfmt gives up on chains if any line is too long
                // https://github.com/rust-lang/rustfmt/issues/3863
                const MSG: &str = "ambiguous use of '&&'; use 'and' for logical AND, or change whitespace to ' & &' for bitwise AND";
                Cow::from(MSG)
            }
            Tag::CStyleContainer(expected_tag) => Cow::from(format!(
                "'{} {}' is invalid",
                expected_tag.symbol(),
                String::from_utf8_lossy(tree.token_slice(self.token)),
            )),
            Tag::ZigStyleContainer(expected_tag) => Cow::from(format!(
                "to declare a container do 'const {} = {}'",
                String::from_utf8_lossy(tree.token_slice(self.token)),
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
                let found_tag = tree.token_tag(match self.token_is_prev {
                    false => self.token,
                    true => self.token + 1,
                });
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
