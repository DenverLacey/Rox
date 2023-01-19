use std::fmt::Debug;

use crate::{
    canon::scoping::{FuncID, ScopeIndex},
    parsing::tokenization::Token,
    typing::value_type::Type,
};

use super::annotations::Annotations;

#[derive(Debug)]
pub struct Ast {
    pub token: Token,
    pub scope: ScopeIndex,
    pub typ: Option<Type>,
    pub info: AstInfo,
}

#[derive(Debug)]
pub enum AstInfo {
    Literal,
    Unary(AstUnaryKind, Box<Ast>),
    Binary(AstBinaryKind, Box<Ast>, Box<Ast>),
    Block(AstBlockKind, Vec<Ast>),
    Fn(Box<AstInfoFn>),
    Var(Box<AstInfoVar>),
    Import(Box<AstInfoImport>),
    Struct(Box<AstInfoStruct>),
    TypeSignature(Box<AstInfoTypeSignature>),
    TypeValue(Type),
    If(Box<AstInfoIf>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AstUnaryKind {
    Neg,
    Not,
    Ref,
    RefMut,
    Deref,
    XXXPrint,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AstBinaryKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
    Call,
    Subscript,
    Param,
    ConstrainedVarDeclTarget,
    Field,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AstBlockKind {
    Block,
    Comma,
    Params,
    Args,
    Fields,
}

#[derive(Debug)]
pub struct AstInfoFn {
    pub annons: Annotations,
    pub ident: Ast,
    pub params: Ast,
    pub returns: Option<Ast>,
    pub body: Ast,
    pub id: Option<FuncID>,
}

#[derive(Debug)]
pub struct AstInfoVar {
    pub mutable: bool,
    pub targets: Vec<Ast>,
    pub initializers: Vec<Ast>,
}

#[derive(Debug)]
pub struct AstInfoImport {
    pub path: Ast,
    pub renamer: Option<Ast>,
    pub exposing: Option<Ast>,
}

#[derive(Debug)]
pub struct AstInfoStruct {
    pub annons: Annotations,
    pub ident: Ast,
    pub body: Ast,
}

#[derive(Debug)]
pub enum AstInfoTypeSignature {
    Function(Ast, Option<Ast>),
}

#[derive(Debug)]
pub struct AstInfoIf {
    pub condition: Ast,
    pub then_block: Ast,
    pub else_block: Option<Ast>,
}

impl Ast {
    pub fn new(token: Token, info: AstInfo) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info,
        }
    }

    pub fn new_literal(token: Token) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Literal,
        }
    }

    pub fn new_unary(kind: AstUnaryKind, token: Token, sub_expression: Box<Ast>) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Unary(kind, sub_expression),
        }
    }

    pub fn new_binary(kind: AstBinaryKind, token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Binary(kind, lhs, rhs),
        }
    }

    pub fn new_block(kind: AstBlockKind, token: Token, nodes: Vec<Ast>) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Block(kind, nodes),
        }
    }

    pub fn new_type_signature(token: Token, sig: AstInfoTypeSignature) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::TypeSignature(Box::new(sig)),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Dependency {
    pub parsed_file_idx: usize,
    pub queued_idx: usize,
}

impl Debug for Dependency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.parsed_file_idx, self.queued_idx)
    }
}

impl Dependency {
    pub fn new(parsed_file_idx: usize, queued_idx: usize) -> Self {
        Self {
            parsed_file_idx,
            queued_idx,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum QueuedProgress {
    Parsed,
    DependenciesFound,
    PartiallyTypechecked,
    Typechecked,
    Compiled,
}

#[derive(Debug)]
pub struct Queued {
    pub node: Ast,
    pub deps: Vec<Dependency>,
    pub inner_deps: Vec<Dependency>,
    pub progress: QueuedProgress,
}

impl Queued {
    pub fn new(node: Ast) -> Self {
        Self {
            node,
            deps: Vec::new(),
            inner_deps: Vec::new(),
            progress: QueuedProgress::Parsed,
        }
    }

    pub fn is_typechecked(&self) -> bool {
        self.progress >= QueuedProgress::Typechecked
    }

    pub fn is_compiled(&self) -> bool {
        self.progress >= QueuedProgress::Compiled
    }
}
