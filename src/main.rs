#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

pub mod token;
pub use token::{Token, Tokenizer};

pub mod ast;
pub use ast::Ast;

mod macros;
mod parse;

fn main() {
    let source = r#"const _ = { a(); b(); c(); d(); };"#;
    let tree = Ast::parse(source.as_bytes(), ast::Mode::Zig);
    for error in tree.errors.iter() {
        println!("{:?}", error.tag);
    }
    if tree.errors.is_empty() {
        print!("{tree:?}");
    }
}
