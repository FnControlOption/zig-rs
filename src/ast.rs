use crate::ast::error::Tag as E;
use crate::ast::node::Tag as N;
use crate::parse::Parser;
use crate::token;
use crate::token::Tag as T;
use crate::Tokenizer;

pub mod display;
pub mod error;
pub mod full;
pub mod node;
mod render;
#[cfg(test)]
mod tests;
mod tokens;
pub mod visitor;

pub use display::Display;
pub use error::Error;
use node::ExtraData;
pub use node::Node;
pub use visitor::Visitor;

pub struct Ast<'src> {
    pub source: &'src [u8],
    pub token_tags: Vec<token::Tag>,
    pub token_starts: Vec<ByteOffset>,
    pub nodes: Vec<Node>,
    pub extra_data: Vec<node::Index>,
    pub mode: Mode,
    pub errors: Vec<Error>,
}
impl<'src> Ast<'src> {
    pub fn source(&self, range: std::ops::Range<ByteOffset>) -> &'src [u8] {
        &self.source[range.start as usize..range.end as usize]
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

impl<'src> Ast<'src> {
    pub fn parse(source: &'src [u8], mode: Mode) -> Self {
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

    pub fn root_decls(&self) -> &[node::Index] {
        &self.extra_data[self.node(0).data.lhs as usize..self.node(0).data.rhs as usize]
    }

    pub fn get_node_source(&self, node: node::Index) -> &'src [u8] {
        let first_token = self.first_token(node);
        let last_token = self.last_token(node);
        let start = self.token_start(first_token) as usize;
        let end = self.token_start(last_token) as usize + self.token_slice(last_token).len();
        &self.source[start..end]
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
