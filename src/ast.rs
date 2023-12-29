use crate::parse::Parser;
use crate::token;
use crate::Tokenizer;

#[cfg(test)]
mod tests;

pub struct Ast<'src> {
    pub source: &'src [u8],
    pub token_tags: Vec<token::Tag>,
    pub token_starts: Vec<ByteOffset>,
    pub nodes: Vec<Node>,
    pub extra_data: Vec<node::Index>,
    pub mode: Mode,
    pub errors: Vec<Error>,
}

impl Ast<'_> {
    pub fn source_slice(&self, start: ByteOffset) -> &[u8] {
        &self.source[start as usize..]
    }

    pub fn token_tag(&self, index: TokenIndex) -> token::Tag {
        self.token_tags[index as usize]
    }

    pub fn token_start(&self, index: TokenIndex) -> ByteOffset {
        self.token_starts[index as usize]
    }

    pub fn node(&self, index: node::Index) -> &Node {
        &self.nodes[index as usize]
    }
}

pub type TokenIndex = u32;
pub type ByteOffset = u32;

pub struct Location {
    pub line: usize,
    pub column: usize,
    pub line_start: usize,
    pub line_end: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    Zig,
    Zon,
}

impl Ast<'_> {
    pub fn parse(source: &[u8], mode: Mode) -> Ast {
        // Empirically, the zig std lib has an 8:1 ratio of source bytes to token count.
        let estimated_token_count = source.len() / 8;

        let mut token_tags = Vec::with_capacity(estimated_token_count);
        let mut token_starts = Vec::with_capacity(estimated_token_count);

        let mut tokenizer = Tokenizer::new(source);
        loop {
            let token = tokenizer.next();
            token_tags.push(token.tag);
            token_starts.push(token.loc.start as u32);
            if token.tag == token::Tag::Eof {
                break;
            }
        }

        // Empirically, Zig source code has a 2:1 ratio of tokens to AST nodes.
        let estimated_node_count = (token_tags.len() + 2) / 2;

        let mut parser = Parser {
            source,
            token_tags: &token_tags,
            token_starts: &token_starts,
            errors: Vec::new(),
            nodes: Vec::with_capacity(estimated_node_count),
            extra_data: Vec::new(),
            tok_i: 0,
        };

        match mode {
            Mode::Zig => parser.parse_root(),
            Mode::Zon => parser.parse_zon(),
        }

        let Parser {
            nodes,
            extra_data,
            errors,
            ..
        } = parser;

        Ast {
            source,
            mode,
            token_tags,
            token_starts,
            nodes,
            extra_data,
            errors,
        }
    }

    pub fn error_offset(&self, parse_error: &Error) -> u32 {
        if parse_error.token_is_prev {
            self.token_slice(parse_error.token).len() as u32
        } else {
            0
        }
    }

    pub fn token_location(&self, start_offset: ByteOffset, token_index: TokenIndex) -> Location {
        let mut loc = Location {
            line: 0,
            column: 0,
            line_start: start_offset as usize,
            line_end: self.source.len(),
        };
        let token_start = self.token_start(token_index) as usize;

        // Scan by line until we go past the token start
        while let Some(i) = self.source[loc.line_start..]
            .iter()
            .position(|&c| c == b'\n')
        {
            if i + loc.line_start >= token_start {
                break; // Went past
            }
            loc.line += 1;
            loc.line_start += i + 1;
        }

        let offset = loc.line_start;
        for (i, &c) in self.source[offset..].iter().enumerate() {
            if i + offset == token_start {
                loc.line_end = i + offset;
                while loc.line_end < self.source.len() && self.source[loc.line_end] != b'\n' {
                    loc.line_end += 1;
                }
                return loc;
            }
            if c == b'\n' {
                loc.line += 1;
                loc.column = 0;
                loc.line_start = i + 1 + offset;
            } else {
                loc.column += 1;
            }
        }
        loc
    }

    pub fn token_slice(&self, token_index: TokenIndex) -> &str {
        let token_tag = self.token_tag(token_index);

        if let Some(lexeme) = token_tag.lexeme() {
            return lexeme;
        }

        todo!("token_slice")
    }

    pub fn dump_error(&self, parse_error: &Error, filename: &str) {
        let loc = self.token_location(0, parse_error.token);
        let line = loc.line + 1;
        let column = loc.column + 1 + self.error_offset(parse_error) as usize;
        print!("{filename}:{line}:{column}: ");
        self.render_error(parse_error);
        println!();
    }

    pub fn render_error(&self, parse_error: &Error) {
        match parse_error.tag {
            error::Tag::AsteriskAfterPtrDeref => {
                print!("'.*' cannot be followed by '*'. Are you missing a space?")
            }
            error::Tag::ChainedComparisonOperators => {
                print!("comparison operators cannot be chained")
            }
            error::Tag::DeclBetweenFields => {
                print!("declarations are not allowed between container fields")
            }
            error::Tag::ExpectedBlock => {
                print!(
                    "expected block, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedBlockOrAssignment => {
                print!(
                    "expected block or assignment, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedBlockOrExpr => {
                print!(
                    "expected block or expression, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedBlockOrField => {
                print!(
                    "expected block or field, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedContainerMembers => {
                print!(
                    "expected test, comptime, var decl, or container field, found '{}'",
                    self.token_tag(parse_error.token).symbol(),
                )
            }
            error::Tag::ExpectedExpr => {
                print!(
                    "expected expression, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedExprOrAssignment => {
                print!(
                    "expected expression or assignment, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedExprOrVarDecl => {
                print!(
                    "expected expression or var decl, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedFn => {
                print!(
                    "expected function, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedInlinable => {
                print!(
                    "expected 'while' or 'for', found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedLabelable => {
                print!(
                    "expected 'while', 'for', 'inline', or '{{', found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedParamList => {
                print!(
                    "expected parameter list, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedPrefixExpr => {
                print!(
                    "expected prefix expression, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedPrimaryTypeExpr => {
                print!(
                    "expected primary type expression, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedPubItem => {
                print!("expected function or variable declaration after pub")
            }
            error::Tag::ExpectedReturnType => {
                print!(
                    "expected return type expression, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedSemiOrElse => {
                print!("expected ';' or 'else' after statement")
            }
            error::Tag::ExpectedSemiOrLBrace => {
                print!("expected ';' or block after function prototype")
            }
            error::Tag::ExpectedStatement => {
                print!(
                    "expected statement, found '{}'",
                    self.token_tag(parse_error.token).symbol(),
                )
            }
            error::Tag::ExpectedSuffixOp => {
                print!(
                    "expected pointer dereference, optional unwrap, or field access, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedTypeExpr => {
                print!(
                    "expected type expression, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedVarDecl => {
                print!(
                    "expected variable declaration, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedVarDeclOrFn => {
                print!(
                    "expected variable declaration or function, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedLoopPayload => {
                print!(
                    "expected loop payload, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExpectedContainer => {
                print!(
                    "expected a struct, enum or union, found '{}'",
                    self.token_tag(match parse_error.token_is_prev {
                        false => parse_error.token,
                        true => parse_error.token + 1,
                    })
                    .symbol(),
                )
            }
            error::Tag::ExternFnBody => {
                print!("extern functions have no body")
            }
            error::Tag::ExtraAddrspaceQualifier => {
                print!("extra addrspace qualifier")
            }
            error::Tag::ExtraAlignQualifier => {
                print!("extra align qualifier")
            }
            error::Tag::ExtraAllowzeroQualifier => {
                print!("extra allowzero qualifier")
            }
            error::Tag::ExtraConstQualifier => {
                print!("extra const qualifier")
            }
            error::Tag::ExtraVolatileQualifier => {
                print!("extra volatile qualifier")
            }
            error::Tag::PtrModOnArrayChildType => {
                print!(
                    "pointer modifier '{}' not allowed on array child type",
                    self.token_tag(parse_error.token).symbol(),
                )
            }
            error::Tag::InvalidBitRange => {
                print!("bit range not allowed on slices and arrays")
            }
            error::Tag::SameLineDocComment => {
                print!("same line documentation comment")
            }
            error::Tag::UnattachedDocComment => {
                print!("unattached documentation comment")
            }
            error::Tag::TestDocComment => {
                print!("documentation comments cannot be attached to tests")
            }
            error::Tag::ComptimeDocComment => {
                print!("documentation comments cannot be attached to comptime blocks")
            }
            error::Tag::VarargsNonfinal => {
                print!("function prototype has parameter after varargs")
            }
            error::Tag::ExpectedContinueExpr => {
                print!("expected ':' before while continue expression")
            }

            error::Tag::ExpectedSemiAfterDecl => {
                print!("expected ';' after declaration")
            }
            error::Tag::ExpectedSemiAfterStmt => {
                print!("expected ';' after statement")
            }
            error::Tag::ExpectedCommaAfterField => {
                print!("expected ',' after field")
            }
            error::Tag::ExpectedCommaAfterArg => {
                print!("expected ',' after argument")
            }
            error::Tag::ExpectedCommaAfterParam => {
                print!("expected ',' after parameter")
            }
            error::Tag::ExpectedCommaAfterInitializer => {
                print!("expected ',' after initializer")
            }
            error::Tag::ExpectedCommaAfterSwitchProng => {
                print!("expected ',' after switch prong")
            }
            error::Tag::ExpectedCommaAfterForOperand => {
                print!("expected ',' after for operand")
            }
            error::Tag::ExpectedCommaAfterCapture => {
                print!("expected ',' after for capture")
            }
            error::Tag::ExpectedInitializer => {
                print!("expected field initializer")
            }
            error::Tag::MismatchedBinaryOpWhitespace => {
                print!(
                    "binary operator `{}` has whitespace on one side, but not the other.",
                    self.token_tag(parse_error.token).symbol()
                )
            }
            error::Tag::InvalidAmpersandAmpersand => {
                print!("ambiguous use of '&&'; use 'and' for logical AND, or change whitespace to ' & &' for bitwise AND")
            }
            error::Tag::CStyleContainer(expected_tag) => {
                print!(
                    "'{} {}' is invalid",
                    expected_tag.symbol(),
                    self.token_slice(parse_error.token),
                )
            }
            error::Tag::ZigStyleContainer(expected_tag) => {
                print!(
                    "to declare a container do 'const {} = {}'",
                    self.token_slice(parse_error.token),
                    expected_tag.symbol(),
                )
            }
            error::Tag::PreviousField => {
                print!("field before declarations here")
            }
            error::Tag::NextField => {
                print!("field after declarations here")
            }
            error::Tag::ExpectedVarConst => {
                print!("expected 'var' or 'const' before variable declaration")
            }
            error::Tag::WrongEqualVarDecl => {
                print!("variable initialized with '==' instead of '='")
            }
            error::Tag::VarConstDecl => {
                print!("use 'var' or 'const' to declare variable")
            }
            error::Tag::ExtraForCapture => {
                print!("extra capture in for loop")
            }
            error::Tag::ForInputNotCaptured => {
                print!("for input is not captured")
            }

            error::Tag::ExpectedToken(expected_tag) => {
                let found_tag = self.token_tag(match parse_error.token_is_prev {
                    false => parse_error.token,
                    true => parse_error.token + 1,
                });
                let expected_symbol = expected_tag.symbol();
                match found_tag {
                    token::Tag::Invalid => {
                        print!("expected '{}', found invalid bytes", expected_symbol)
                    }
                    _ => print!(
                        "expected '{}', found '{}'",
                        expected_symbol,
                        found_tag.symbol()
                    ),
                }
            }

            _ => todo!("render_error"),
        }
    }
}

pub trait GetExtraData<T> {
    fn extra_data(&self, index: node::Index) -> T;
}

impl GetExtraData<node::Index> for Ast<'_> {
    fn extra_data(&self, index: node::Index) -> node::Index {
        self.extra_data[index as usize]
    }
}

pub mod full;

pub mod error;
pub use error::Error;

pub mod node;
pub use node::Node;

use self::node::ExtraData;

mod debug;
