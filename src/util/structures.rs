use std::{collections::HashMap, path::PathBuf};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeIndex(pub usize);

pub struct Scope {
    pub parent: Option<ScopeIndex>,
    // pub children: Vec<ScopeIndex>,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncID(pub usize);

pub struct FunctionBinding {
    pub id: FuncID,
    pub typ: Type,
}

pub struct FunctionInfo {
    pub id: FuncID,
    pub name: String,
    pub typ: Type,
    pub code: Box<[u8]>,
}
