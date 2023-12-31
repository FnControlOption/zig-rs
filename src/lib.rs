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

mod parse;
