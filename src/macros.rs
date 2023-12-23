macro_rules! error {
    ($tag:ident) => {
        crate::ast::error::Tag::$tag
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
