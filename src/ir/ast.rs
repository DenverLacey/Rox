use crate::parsing::tokenization::Token;

#[derive(Debug)]
pub struct Ast {
    pub token: Token,
    pub scope: (),
    pub typ: Option<()>,
    pub info: AstInfo,
}

#[derive(Debug)]
pub enum AstInfo {
    Literal,
    Unary(AstUnaryKind, Box<Ast>),
    Binary(AstBinaryKind, Box<Ast>, Box<Ast>),
    Block(AstBlockKind, Vec<Ast>),
    Fn(Box<AstInfoFn>),
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
}

#[derive(Debug)]
pub enum AstBlockKind {
    Program,
    Block,
    Params,
    Args,
}

#[derive(Debug)]
pub struct AstInfoFn {
    ident: Ast,
    params: Ast,
    body: Ast,
}

impl Ast {
    pub fn new_literal(token: Token) -> Self {
        Self {
            token,
            scope: (),
            typ: None,
            info: AstInfo::Literal,
        }
    }

    pub fn new_unary(kind: AstUnaryKind, token: Token, sub_expression: Box<Ast>) -> Self {
        Self {
            token,
            scope: (),
            typ: None,
            info: AstInfo::Unary(kind, sub_expression),
        }
    }

    pub fn new_binary(kind: AstBinaryKind, token: Token, lhs: Box<Ast>, rhs: Box<Ast>) -> Self {
        Self {
            token,
            scope: (),
            typ: None,
            info: AstInfo::Binary(kind, lhs, rhs),
        }
    }

    pub fn new_block(kind: AstBlockKind, token: Token, nodes: Vec<Ast>) -> Self {
        Self {
            token,
            scope: (),
            typ: None,
            info: AstInfo::Block(kind, nodes),
        }
    }
}
