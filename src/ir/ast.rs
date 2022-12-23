use crate::{
    canon::scoping::ScopeIndex, interp::Pid, parsing::tokenization::Token, typing::value_type::Type,
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

impl Ast {
    pub fn new(token: Token, info: AstInfo) -> Self {
        Self {
            token,
            scope: ScopeIndex(0),
            typ: None,
            info,
        }
    }

    pub fn new_program(nodes: Vec<Ast>) -> Self {
        Ast::new_block(AstBlockKind::Program, Token::default(), nodes)
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
}

#[derive(Clone, Copy, Debug)]
pub struct DependencyLocator {
    pub file_idx: usize,
    pub queued_idx: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum QueuedPhase {
    Pending,
    Typechecked,
    Compiled,
}

#[derive(Debug)]
pub struct Queued {
    pub node: Ast,
    pub deps: Vec<DependencyLocator>,
    pub phase: QueuedPhase,
}

impl Queued {
    pub fn new(node: Ast) -> Self {
        Self {
            node,
            deps: vec![],
            phase: QueuedPhase::Pending,
        }
    }
}
