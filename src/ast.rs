use crate::parse::Parser;
use crate::token;
use crate::Tokenizer;

pub mod visitor;
pub use visitor::Visitor;

pub mod display;
pub use display::Display;

mod render;

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
    pub fn source(&self, start: ByteOffset) -> &[u8] {
        &self.source[start as usize..]
    }

    pub fn render(&self) -> Result<String, std::fmt::Error> {
        let mut buffer = String::new();
        self.render_to_writer(&mut buffer, Default::default())?;
        Ok(buffer)
    }

    pub fn render_to_writer(
        &self,
        writer: &mut dyn std::fmt::Write,
        fixups: render::Fixups,
    ) -> std::fmt::Result {
        render::render_tree(writer, self, fixups)
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

    pub fn token_slice(&self, token_index: TokenIndex) -> &[u8] {
        let token_tag = self.token_tag(token_index);

        if let Some(lexeme) = token_tag.lexeme() {
            return lexeme.as_bytes();
        }

        let mut tokenizer = Tokenizer {
            buffer: self.source,
            index: self.token_start(token_index) as usize,
            pending_invalid_token: None,
        };
        let token = tokenizer.find_tag_at_current_index(token_tag);
        assert!(token.tag == self.token_tag(token_index));
        &self.source[token.loc.start..token.loc.end]
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

impl Ast<'_> {
    fn root_decls(&self) -> &[node::Index] {
        &self.extra_data[self.node(0).data.lhs as usize..self.node(0).data.rhs as usize]
    }

    pub fn first_token(&self, node: node::Index) -> TokenIndex {
        let mut end_offset: TokenIndex = 0;
        let mut n = node;
        loop {}
    }
}

pub mod full;

pub mod error;
pub use error::Error;

pub mod node;
pub use node::Node;

use self::node::ExtraData;
