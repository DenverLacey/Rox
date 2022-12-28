use std::fmt::Debug;

use crate::{
    canon::scoping::ScopeIndex, interp::Interpreter, parsing::tokenization::Token,
    typing::value_type::Type,
};

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
    TypeSignature(Box<AstInfoTypeSignature>),
    TypeValue(Type),
}

#[derive(Clone, Copy, Debug)]
pub enum AstUnaryKind {
    Neg,
    Not,
    Ref,
    RefMut,
    Deref,
    XXXPrint,
}

#[derive(Clone, Copy, Debug)]
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
}

#[derive(Clone, Copy, Debug)]
pub enum AstBlockKind {
    Block,
    Comma,
    Params,
    Args,
    VarDeclTargets,
}

#[derive(Debug)]
pub struct AstInfoFn {
    pub ident: Ast,
    pub params: Ast,
    pub returns: Option<Ast>,
    pub body: Ast,
}

#[derive(Debug)]
pub struct AstInfoVar {
    pub mutable: bool,
    pub targets: Ast,
    pub initializer: VariableInitializer,
}

#[derive(Debug)]
pub enum VariableInitializer {
    TypeAndExpr(Ast, Ast),
    Type(Ast),
    Expr(Ast),
}

#[derive(Debug)]
pub struct AstInfoImport {
    pub path: Ast,
    pub renamer: Option<Ast>,
    pub exposing: Option<Ast>,
}

#[derive(Debug)]
pub enum AstInfoTypeSignature {
    Function(Ast, Option<Ast>),
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
    pub progress: QueuedProgress,
}

impl Queued {
    pub fn new(node: Ast) -> Self {
        Self {
            node,
            deps: Vec::new(),
            progress: QueuedProgress::Parsed,
        }
    }

    pub fn all_dependencies_typechecked(&self) -> bool {
        let interp = Interpreter::get();

        for dep in &self.deps {
            let dep = &interp.parsed_files[dep.parsed_file_idx].ast[dep.queued_idx];
            if dep.progress < QueuedProgress::PartiallyTypechecked {
                return false;
            }
        }

        true
    }

    pub fn is_typechecked(&self) -> bool {
        self.progress >= QueuedProgress::Typechecked
    }
}
