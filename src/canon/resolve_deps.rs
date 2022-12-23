use crate::{interp::ParsedFile, ir::ast::Ast};

pub struct Resolver<'a> {
    files: &'a mut Vec<ParsedFile>,
}

impl<'a> Resolver<'a> {
    pub fn new(files: &'a mut Vec<ParsedFile>) -> Self {
        Self { files }
    }
}

impl<'a> Resolver<'a> {
    pub fn resolve_dependencies(&mut self) -> Result<(), &'static str> {
        todo!()
    }
}
