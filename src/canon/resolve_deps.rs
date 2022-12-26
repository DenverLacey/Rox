use std::collections::HashMap;

use debug_print::debug_println as dprintln;

use crate::{
    interp::ParsedFile,
    ir::ast::{AstBlockKind, AstInfo, DependencyLocator, Queued, Ast, VariableInitializer},
    parsing::tokenization::TokenInfo,
};

pub struct Resolver<'files> {
    files: &'files mut Vec<ParsedFile>,
    globals: Vec<Vec<Global>>,
    scopes: Vec<Scope>,
}

impl<'files> Resolver<'files> {
    pub fn new(files: &'files mut Vec<ParsedFile>) -> Self {
        Self {
            files,
            globals: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn resolve_dependencies(&mut self) -> Result<(), &'static str> {
        self.register_all_globals();
        dprintln!("Globals = {:#?}", self.globals);

        if self.globals.len() <= 1 {
            return Ok(());
        }

        for file_idx in 0..self.files.len() {
            self.resolve_dependencies_for_file(file_idx)?;
        }

        todo!()
    }
}

impl<'files> Resolver<'files> {
    fn begin_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn end_scope(&mut self) {
        let None = self.scopes.pop() else {
            panic!("[INTERNAL ERR] call to `end_scope` when no scopes are present.");
        };
    }

    fn register_all_globals(&mut self) {
        for file in self.files.iter() {
            self.globals.push(Vec::new());
            let globals = self.globals.last_mut().unwrap();

            for (idx, node) in file.ast.iter().enumerate() {
                match &node.node.info {
                    AstInfo::Fn(info) => {
                        let TokenInfo::Ident(ident) = &info.ident.token.info else {
                            panic!("[INTERNAL ERR] `ident` node of `Fn` node not an `Ident` node.");
                        };

                        globals.push(Global {
                            name: ident.clone(),
                            queued_idx: idx,
                        });
                    }
                    AstInfo::Var(info) => {
                        if matches!(info.targets.info, AstInfo::Literal) {
                            let TokenInfo::Ident(ident) = &info.targets.token.info else {
                                panic!("[INTERNAL ERR] `targets` node of `Var` node notan `Ident` node but some other `Literal`.");
                            };

                            globals.push(Global {
                                name: ident.clone(),
                                queued_idx: idx,
                            });
                        } else if let AstInfo::Block(AstBlockKind::VarDeclTargets, targets) =
                            &info.targets.info
                        {
                            for target in targets {
                                let TokenInfo::Ident(ident) = &target.token.info else {
                                    panic!("[INTERNAL ERR] One of the targets of a `VarDeclTargets` node is not an `Ident` node.");
                                };

                                globals.push(Global {
                                    name: ident.clone(),
                                    queued_idx: idx,
                                });
                            }
                        } else {
                            panic!("[INTERNAL ERR] `targets` node of `Var` node not an `Ident` node or a `VarDeclTargets` node.");
                        }
                    }

                    AstInfo::Literal => {}
                    AstInfo::Unary(_, _) => {}
                    AstInfo::Binary(_, _, _) => {}
                    AstInfo::Block(_, _) => {}
                    AstInfo::Import(_) => {}
                }
            }
        }
    }

    fn resolve_dependencies_for_file(&mut self, file_idx: usize) -> Result<(), &'static str> {
        self.begin_scope();

        let current_scope = self.scopes.last_mut().unwrap();
        current_scope
            .locators
            .extend(self.globals[file_idx].iter().map(|global| {
                (
                    global.name.clone(),
                    DependencyLocator::new(file_idx, global.queued_idx),
                )
            }));

        let file = &mut self.files[file_idx];
        for node in file.ast.iter_mut() {
            Self::resolve_dependencies_for_queued(current_scope, node);
        }

        self.end_scope();
        Ok(())
    }

    fn resolve_dependencies_for_queued(current_scope: &mut Scope, node: &mut Queued) {
        match &node.node.info {
            AstInfo::Fn(info) => {
                let deps = &mut node.deps;

                let AstInfo::Block(AstBlockKind::Params, params) = &info.params.info else {
                    panic!("[INTERNAL ERR] `params` node is not a `Params` Block node.");
                };
                Self::resolve_dependencies_for_nodes(current_scope, deps, params);

                let AstInfo::Block(AstBlockKind::Block, body) = &info.body.info else {
                    panic!("[INTERNAL ERR] `body` node is not a `Block` Block node.");
                };
                Self::resolve_dependencies_for_nodes(current_scope, deps, body);
            }
            AstInfo::Var(info) => {
                let deps = &mut node.deps;

                match &info.initializer {
                    VariableInitializer::TypeAndExpr(typ, expr) => {
                        Self::resolve_dependencies_for_node(current_scope, deps, typ);
                        Self::resolve_dependencies_for_node(current_scope, deps, expr);
                    }
                    VariableInitializer::Type(typ) => Self::resolve_dependencies_for_node(current_scope, deps, typ),
                    VariableInitializer::Expr(expr) => Self::resolve_dependencies_for_node(current_scope, deps, expr),
                }
            }

            AstInfo::Literal => {}
            AstInfo::Unary(_, _) => {}
            AstInfo::Binary(_, _, _) => {}
            AstInfo::Block(_, _) => {}
            AstInfo::Import(_) => {}
        }
    }

    fn resolve_dependencies_for_nodes(current_scope: &mut Scope, deps: &mut Vec<DependencyLocator>, nodes: &[Ast]) {
        for node in nodes {
            Self::resolve_dependencies_for_node(current_scope, deps, node);
        }
    }

    fn resolve_dependencies_for_node(current_scope: &mut Scope, deps: &mut Vec<DependencyLocator>, node: &Ast) {
        todo!()
    }
}

#[derive(Debug)]
struct Global {
    name: String,
    queued_idx: usize,
}

struct Scope {
    locators: HashMap<String, DependencyLocator>,
}

impl Scope {
    fn new() -> Self {
        Self {
            locators: HashMap::new(),
        }
    }
}
