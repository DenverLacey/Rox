use std::collections::HashMap;

use debug_print::debug_println as dprintln;

use crate::{
    interp::ParsedFile,
    ir::ast::{Ast, AstBlockKind, AstInfo, Dependency, Queued, QueuedPhase, VariableInitializer},
    parsing::tokenization::TokenInfo,
    util::lformat,
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

        for file_idx in 0..self.files.len() {
            self.resolve_dependencies_for_file(file_idx)?;
        }

        if cfg!(debug_assertions) {
            dprintln!("\nFound dependencies before validation:");
            for (fi, ni, deps) in self.files.iter().enumerate().flat_map(|(fi, f)| {
                f.ast
                    .iter()
                    .enumerate()
                    .map(move |(ni, n)| (fi, ni, &n.deps))
            }) {
                dprintln!("({},{}):\n\t{:?}", fi, ni, deps);
            }
            dprintln!("");
        }

        self.detect_circular_dependencies()
    }
}

impl<'files> Resolver<'files> {
    fn begin_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn end_scope(&mut self) {
        self.scopes
            .pop()
            .expect("[INTERNAL ERR] call to `end_scope` when no scopes are present.");
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
                    Dependency::new(file_idx, global.queued_idx),
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
                    VariableInitializer::Type(typ) => {
                        Self::resolve_dependencies_for_node(current_scope, deps, typ);
                    }
                    VariableInitializer::Expr(expr) => {
                        Self::resolve_dependencies_for_node(current_scope, deps, expr);
                    }
                }
            }

            AstInfo::Literal => {}
            AstInfo::Unary(_, _) => {}
            AstInfo::Binary(_, _, _) => {}
            AstInfo::Block(_, _) => {}
            AstInfo::Import(_) => {}
        }

        node.phase = QueuedPhase::DependenciesFound;
    }

    fn resolve_dependencies_for_nodes(
        current_scope: &mut Scope,
        deps: &mut Vec<Dependency>,
        nodes: &[Ast],
    ) {
        for node in nodes {
            Self::resolve_dependencies_for_node(current_scope, deps, node);
        }
    }

    fn resolve_dependencies_for_node(
        current_scope: &mut Scope,
        deps: &mut Vec<Dependency>,
        node: &Ast,
    ) {
        match &node.info {
            AstInfo::Literal => {
                if let TokenInfo::Ident(ident) = &node.token.info {
                    if let Some(dep) = current_scope.find_ident(ident) {
                        deps.push(dep);
                    }
                }
            }
            AstInfo::Unary(_, sub_expr) => {
                Self::resolve_dependencies_for_node(current_scope, deps, sub_expr)
            }
            AstInfo::Binary(_, lhs, rhs) => {
                Self::resolve_dependencies_for_node(current_scope, deps, lhs);
                Self::resolve_dependencies_for_node(current_scope, deps, rhs);
            }
            AstInfo::Block(_, nodes) => {
                Self::resolve_dependencies_for_nodes(current_scope, deps, nodes)
            }
            AstInfo::Fn(info) => todo!(),
            AstInfo::Var(info) => match &info.initializer {
                VariableInitializer::TypeAndExpr(typ, expr) => {
                    Self::resolve_dependencies_for_node(current_scope, deps, typ);
                    Self::resolve_dependencies_for_node(current_scope, deps, expr);
                }
                VariableInitializer::Type(typ) => {
                    Self::resolve_dependencies_for_node(current_scope, deps, typ);
                }
                VariableInitializer::Expr(expr) => {
                    Self::resolve_dependencies_for_node(current_scope, deps, expr);
                }
            },
            AstInfo::Import(info) => todo!(),
        }
    }

    fn detect_circular_dependencies(&self) -> Result<(), &'static str> {
        for file_idx in 0..self.files.len() {
            let len_nodes = self.files[file_idx].ast.len();
            for node_idx in 0..len_nodes {
                let node = &self.files[file_idx].ast[node_idx];
                // @NOTE:
                // Im not 100% sure this is correct. The thinking is functions don't have circular
                // dependencies because of recursion kind of reasons and we'll detect actually bad
                // circular dependencies in the other kind of nodes.
                //
                if matches!(node.node.info, AstInfo::Fn(_)) {
                    continue;
                }

                self.detect_circular_dependencies_for_queued(Dependency::new(file_idx, node_idx))?;
            }
        }

        Ok(())
    }

    fn detect_circular_dependencies_for_queued(
        &self,
        queued_loc: Dependency,
    ) -> Result<(), &'static str> {
        let deps = &self.files[queued_loc.parsed_file_idx].ast[queued_loc.queued_idx].deps;
        for &dep in deps {
            self.detect_circular_dependency(queued_loc, dep, dep)?;
        }

        Ok(())
    }

    fn detect_circular_dependency(
        &self,
        needle: Dependency,
        parent_dep: Dependency,
        dep: Dependency,
    ) -> Result<(), &'static str> {
        if needle == dep {
            // @TODO:
            // Improve error message
            //
            return Err(lformat!(
                "Circular dependency detected between {:?} and {:?}.",
                needle,
                parent_dep
            ));
        }

        let deps = &self.files[dep.parsed_file_idx].ast[dep.queued_idx].deps;
        for child_dep in deps {
            self.detect_circular_dependency(needle, parent_dep, *child_dep)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
struct Global {
    name: String,
    queued_idx: usize,
}

struct Scope {
    locators: HashMap<String, Dependency>,
}

impl Scope {
    fn new() -> Self {
        Self {
            locators: HashMap::new(),
        }
    }

    fn find_ident(&self, ident: &String) -> Option<Dependency> {
        self.locators.get(ident).copied()
    }
}
