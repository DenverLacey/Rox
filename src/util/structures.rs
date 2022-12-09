use std::path::PathBuf;

use crate::ir::ast::Ast;

#[derive(Debug)]
pub struct LoadedFile {
    pub filepath: PathBuf,
    pub source: String,
}

#[derive(Debug)]
pub struct ParsedFile {
    pub filepath: PathBuf,
    pub ast: Ast,
}
