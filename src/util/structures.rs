use std::{path::PathBuf, collections::HashMap};

use crate::{ir::ast::Ast, typing::value_type::Type};

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

pub type ScopeIndex = usize;

pub struct Scope {
    pub parent: Option<ScopeIndex>,
    pub bindings: HashMap<String, ScopeBinding>,
}

pub enum ScopeBinding {
    Var(VariableBinding),
    Func(FunctionBinding),
}

pub struct VariableBinding {
    pub is_mut: bool,
    pub typ: Type,
}

pub type FuncID = usize;

pub struct FunctionBinding {
    pub id: FuncID,
    pub typ: Type,
}

pub struct FunctionInfo {
    pub name: String,
    pub typ: Type,
    pub code: Box<[u8]>,
} 
