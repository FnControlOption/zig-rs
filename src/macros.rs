macro_rules! token {
    ($tag:ident) => {
        token::Tag::$tag
    };
}

pub(crate) use token;

macro_rules! node {
    ($tag:ident) => {
        node::Tag::$tag
    };
}

pub(crate) use node;

macro_rules! error {
    ($tag:ident) => {
        error::Tag::$tag
    };
}

pub(crate) use error;
