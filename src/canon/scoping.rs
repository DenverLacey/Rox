use std::collections::HashMap;

use crate::{
    interp::Interpreter,
    ir::ast::{Ast, AstBlockKind, AstInfo, AstInfoTypeSignature},
    parsing::tokenization::CodeLocation,
    runtime::vm::Addr,
    typing::value_type::Type,
    util::errors::{Result, SourceError},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeIndex(pub usize);

#[derive(Debug, Default)]
pub struct Scope {
    pub parent: Option<ScopeIndex>,
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

impl Scope {
    pub fn is_global(&self) -> bool {
        self.parent.filter(|p| p.0 == 0).is_some()
    }

    pub fn find_binding_mut(&mut self, ident: &str) -> Option<&mut ScopeBinding> {
        if let Some(binding) = self.bindings.get_mut(ident) {
            return Some(binding);
        }

        if let Some(parent) = self.parent {
            let interp = Interpreter::get_mut();
            let parent = &mut interp.scopes[parent.0];
            return parent.find_binding_mut(ident);
        }

        None
    }

    pub fn find_binding(&self, ident: &str) -> Option<&ScopeBinding> {
        if let Some(binding) = self.bindings.get(ident) {
            return Some(binding);
        }

        if let Some(parent) = self.parent {
            let interp = Interpreter::get();
            let parent = &interp.scopes[parent.0];
            return parent.find_binding(ident);
        }

        None
    }

    pub fn add_binding(
        &mut self,
        ident: impl Into<String>,
        binding: ScopeBinding,
        loc: CodeLocation,
        err: impl Into<String>,
    ) -> Result<()> {
        let ident = ident.into();

        if self.bindings.contains_key(&ident) {
            return Err(SourceError::new("Redclaration of identifier.", loc, err).into());
        }

        self.add_binding_unchecked(ident, binding);

        Ok(())
    }

    pub fn add_binding_unchecked(&mut self, ident: impl Into<String>, binding: ScopeBinding) {
        let ident = ident.into();
        self.bindings.insert(ident, binding);
    }
}

#[derive(Debug)]
pub enum ScopeBinding {
    Var(VariableBinding),
    Fn(FunctionBinding),
    Type(Type),
}

#[derive(Debug)]
pub struct VariableBinding {
    pub typ: Type,
    pub is_global: bool,
    pub addr: Addr,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
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

    pub fn establish_scope_for_file<'iter>(
        &mut self,
        nodes: impl Iterator<Item = &'iter mut Ast>,
    ) -> ScoperResult {
        let file_scope = self.push_scope(ScopeIndex(0));
        self.establish_scope_for_nodes(file_scope, nodes)
    }
}

type ScoperResult = Result<()>;

impl<'a> Scoper<'a> {
    fn push_scope(&mut self, parent: ScopeIndex) -> ScopeIndex {
        let idx = ScopeIndex(self.scopes.len());
        self.scopes.push(Scope::with_parent(parent));

        idx
    }

    fn establish_scope_for_nodes<'iter>(
        &mut self,
        current_scope: ScopeIndex,
        nodes: impl Iterator<Item = &'iter mut Ast>,
    ) -> ScoperResult {
        for node in nodes {
            self.establish_scope_for_node(current_scope, node)?;
        }

        Ok(())
    }

    fn establish_scope_for_node(
        &mut self,
        current_scope: ScopeIndex,
        node: &mut Ast,
    ) -> ScoperResult {
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
            AstInfo::Optional(_, sub_node) => {
                if let Some(expr) = sub_node {
                    self.establish_scope_for_node(current_scope, expr)?;
                }
            }
            AstInfo::Block(AstBlockKind::Block, sub_nodes) => {
                let new_scope = self.push_scope(current_scope);
                self.establish_scope_for_nodes(new_scope, sub_nodes.iter_mut())?;
            }
            AstInfo::Block(_, sub_nodes) => {
                self.establish_scope_for_nodes(current_scope, sub_nodes.iter_mut())?
            }
            AstInfo::Fn(info) => {
                let func_scope = self.push_scope(current_scope);
                self.establish_scope_for_node(func_scope, &mut info.params)?;
                self.establish_scope_for_node(func_scope, &mut info.body)?;
            }
            AstInfo::Var(info) => {
                self.establish_scope_for_nodes(current_scope, info.targets.iter_mut())?;
                self.establish_scope_for_nodes(current_scope, info.initializers.iter_mut())?;
            }
            AstInfo::Import(info) => {
                self.establish_scope_for_node(current_scope, &mut info.path)?;

                if let Some(rn) = &mut info.renamer {
                    self.establish_scope_for_node(current_scope, rn)?;
                }

                if let Some(ex) = &mut info.exposing {
                    self.establish_scope_for_node(current_scope, ex)?;
                }
            }
            AstInfo::Struct(info) => {
                let struct_scope = self.push_scope(current_scope);
                self.establish_scope_for_node(struct_scope, &mut info.body)?;
            }
            AstInfo::TypeValue(_) => unreachable!(),
            AstInfo::TypeSignature(sig) => match sig.as_mut() {
                AstInfoTypeSignature::Function(params, returns) => {
                    let AstInfo::Block(AstBlockKind::Params, params) = &mut params.info else {
                        panic!("[INTERNAL ERR] `params` node of type signature was not a `Params` node.");
                    };
                    self.establish_scope_for_nodes(current_scope, params.iter_mut())?;

                    if let Some(returns) = returns {
                        self.establish_scope_for_node(current_scope, returns)?;
                    }
                }
            },
            AstInfo::If(info) => {
                self.establish_scope_for_node(current_scope, &mut info.condition)?;
                self.establish_scope_for_node(current_scope, &mut info.then_block)?;

                if let Some(else_block) = &mut info.else_block {
                    self.establish_scope_for_node(current_scope, else_block)?;
                }
            }
            AstInfo::For(info) => {
                self.establish_scope_for_node(current_scope, &mut info.control)?;
                self.establish_scope_for_node(current_scope, &mut info.body)?;
            }
            AstInfo::ForControl(info) => {
                self.establish_scope_for_node(current_scope, &mut info.initializer)?;
                if let Some(cond) = &mut info.condition {
                    self.establish_scope_for_node(current_scope, cond)?;
                }
                if let Some(step) = &mut info.step {
                    self.establish_scope_for_node(current_scope, step)?;
                }
            }
        }

        Ok(())
    }
}
