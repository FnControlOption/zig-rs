use super::*;

pub struct PtrModifiers {
    // TODO
}

impl<'src, 'tok> Parser<'src, 'tok> {
    pub(super) fn parse_ptr_payload(&mut self) -> Result<TokenIndex> {
        todo!("parse_ptr_payload")
    }

    pub(super) fn parse_ptr_index_payload(&mut self) -> Result<TokenIndex> {
        todo!("parse_ptr_index_payload")
    }

    pub(super) fn parse_ptr_modifiers(&mut self) -> Result<PtrModifiers> {
        todo!("parse_ptr_modifiers")
    }
}
