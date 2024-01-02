#![allow(clippy::assign_op_pattern)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::identity_op)]
#![allow(clippy::should_implement_trait)]
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

pub mod string_literal;
pub use string_literal::parse_char_literal;

pub mod fmt;
pub use fmt::{fmt_escapes, fmt_id, is_valid_id};

mod parse;
mod utils;
