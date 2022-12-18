use std::collections::HashMap;

use crate::{
    ir::ast::{Ast, AstBlockKind, AstInfo, VariableInitializer},
    typing::value_type::Type,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeIndex(pub usize);

#[derive(Debug, Default)]
pub struct Scope {
    pub parent: Option<ScopeIndex>,
    // pub children: Vec<ScopeIndex>,
    pub bindings: HashMap<String, ScopeBinding>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_parent(parent: ScopeIndex) -> Self {
        Self {
            parent: Some(parent),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
pub enum ScopeBinding {
    Var(VariableBinding),
    Func(FunctionBinding),
}

#[derive(Debug)]
pub struct VariableBinding {
    pub is_mut: bool,
    pub typ: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncID(pub usize);

#[derive(Debug)]
pub struct FunctionBinding {
    pub id: FuncID,
    pub typ: Type,
}

pub struct Scoper<'a> {
    scopes: &'a mut Vec<Scope>,
}

impl<'a> Scoper<'a> {
    pub fn new(scopes: &'a mut Vec<Scope>) -> Self {
        Self { scopes }
    }

    pub fn establish_scope_for_file(&mut self, nodes: &mut Vec<Ast>) -> ScoperResult {
        let file_scope = self.push_scope();
        self.establish_scope_for_nodes(file_scope, nodes)
    }
}

type ScoperResult = Result<(), &'static str>;

impl<'a> Scoper<'a> {
    fn push_scope(&mut self) -> ScopeIndex {
        let idx = ScopeIndex(self.scopes.len());
        self.scopes.push(Scope::new());

        idx
    }

    fn establish_scope_for_nodes(
        &mut self,
        current_scope: ScopeIndex,
        nodes: &mut Vec<Ast>,
    ) -> ScoperResult {
        for node in nodes {
            self.establish_scope_for_node(current_scope, node)?;
        }

        Ok(())
    }

    fn establish_scope_for_node(&mut self, current_scope: ScopeIndex, node: &mut Ast) -> ScoperResult {
        node.scope = current_scope;

        match &mut node.info {
            AstInfo::Literal => {}
            AstInfo::Unary(_, sub_node) => {
                self.establish_scope_for_node(current_scope, sub_node)?
            }
            AstInfo::Binary(_, lhs, rhs) => {
                self.establish_scope_for_node(current_scope, lhs.as_mut())?;
                self.establish_scope_for_node(current_scope, rhs.as_mut())?;
            }
            AstInfo::Block(AstBlockKind::Block, sub_nodes) => {
                let new_scope = ScopeIndex(self.scopes.len());
                self.scopes.push(Scope::with_parent(current_scope));

                self.establish_scope_for_nodes(new_scope, sub_nodes)?;
            }
            AstInfo::Block(_, sub_nodes) => {
                self.establish_scope_for_nodes(current_scope, sub_nodes)?
            }
            AstInfo::Fn(info) => {
                let func_scope = ScopeIndex(self.scopes.len());
                self.scopes.push(Scope::with_parent(current_scope));

                self.establish_scope_for_node(func_scope, &mut info.params)?;
                self.establish_scope_for_node(func_scope, &mut info.body)?;
            }
            AstInfo::Var(info) => {
                info.targets.scope = current_scope;

                match &mut info.targets.info {
                    AstInfo::Literal => {}
                    AstInfo::Block(AstBlockKind::VarDeclTargets, targets) => {
                        for target in targets {
                            assert!(matches!(target.info, AstInfo::Literal));
                            target.scope = current_scope;
                        }
                    }
                    _ => return Err("[ERR] Targets node of Var Decl node is not a Literal ident or a VarDeclTargets node."),
                }

                match &mut info.initializer {
                    VariableInitializer::TypeAndExpr(typ, expr) => {
                        self.establish_scope_for_node(current_scope, typ)?;
                        self.establish_scope_for_node(current_scope, expr)?;
                    }
                    VariableInitializer::Type(typ) => {
                        self.establish_scope_for_node(current_scope, typ)?
                    }
                    VariableInitializer::Expr(expr) => {
                        self.establish_scope_for_node(current_scope, expr)?
                    }
                }
            }
        }

        Ok(())
    }
}
