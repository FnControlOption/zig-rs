macro_rules! error {
    ($tag:ident) => {
        crate::ast::error::Tag::$tag
    };
    ($tag:ident($value:expr)) => {
        crate::ast::error::Tag::$tag($value)
    };
    ($tag:ident(_)) => {
        crate::ast::error::Tag::$tag(_)
    };
}

macro_rules! node {
    ($tag:ident) => {
        crate::ast::node::Tag::$tag
    };
}

macro_rules! token {
    ($tag:ident) => {
        crate::token::Tag::$tag
    };
}

pub(crate) use error;
pub(crate) use node;
pub(crate) use token;
