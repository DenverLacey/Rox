use crate::{canon::scoping::ScopeIndex, parsing::tokenization::Token, typing::value_type::Type};

#[derive(Debug)]
pub struct Ast {
    pub token: Token,
    pub scope: ScopeIndex,
    pub typ: Option<Type>,
    pub info: AstInfo,
    pub deps: Vec<DependencyLocator>,
    pub phase: QueuedPhase,
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
}

#[derive(Debug)]
pub enum AstUnaryKind {
    Neg,
    Not,
    XXXPrint,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum AstBlockKind {
    Program,
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

impl Ast {
    pub fn new(token: Token, info: AstInfo) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info,
            deps: vec![],
            phase: QueuedPhase::Parsed,
        }
    }

    pub fn new_program(nodes: Vec<Ast>) -> Self {
        Ast::new_block(AstBlockKind::Program, Token::default(), nodes)
    }

    pub fn program_nodes(&self) -> &[Ast] {
        if let Ast {
            token: _,
            scope: _,
            typ: _,
            info: AstInfo::Block(AstBlockKind::Program, nodes),
            deps: _,
            phase: _,
        } = self
        {
            return nodes.as_slice();
        } else {
            panic!("[INTERNAL ERR] File's `ast` is not a `Program` node.");
        }
    }

    pub fn program_nodes_mut(&mut self) -> &mut [Ast] {
        if let Ast {
            token: _,
            scope: _,
            typ: _,
            info: AstInfo::Block(AstBlockKind::Program, nodes),
            deps: _,
            phase: _,
        } = self
        {
            return nodes.as_mut_slice();
        } else {
            panic!("[INTERNAL ERR] File's `ast` is not a `Program` node.");
        }
    }

    pub fn new_literal(token: Token) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Literal,
            deps: vec![],
            phase: QueuedPhase::Parsed,
        }
    }

    pub fn new_unary(kind: AstUnaryKind, token: Token, sub_expression: Box<Ast>) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Unary(kind, sub_expression),
            deps: vec![],
            phase: QueuedPhase::Parsed,
        }
    }

    pub fn new_binary(kind: AstBinaryKind, token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Binary(kind, lhs, rhs),
            deps: vec![],
            phase: QueuedPhase::Parsed,
        }
    }

    pub fn new_block(kind: AstBlockKind, token: Token, nodes: Vec<Ast>) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info: AstInfo::Block(kind, nodes),
            deps: vec![],
            phase: QueuedPhase::Parsed,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DependencyLocator {
    pub parsed_file_idx: usize,
    pub queued_idx: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QueuedPhase {
    Parsed,
    DependenciesFound,
    Typechecked,
    Compiled,
}
