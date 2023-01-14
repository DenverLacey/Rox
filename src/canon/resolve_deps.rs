use std::collections::HashMap;

use debug_print::debug_println as dprintln;

use crate::{
    interp::ParsedFile,
    ir::ast::{
        Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoTypeSignature, Dependency, Queued,
        QueuedProgress,
    },
    parsing::tokenization::TokenInfo,
    util::errors::{Result, SourceError2},
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

    pub fn resolve_dependencies(&mut self) -> Result<()> {
        self.register_all_globals();
        dprintln!("Globals = {:#?}", self.globals);

        for file_idx in 0..self.files.len() {
            self.resolve_dependencies_for_file(file_idx)?;
        }

        if cfg!(debug_assertions) {
            dprintln!("\nFound dependencies before validation:");
            for (fi, ni, deps) in self.files.iter().enumerate().flat_map(|(fi, f)| {
                f.ast.iter().enumerate().map(move |(ni, n)| {
                    (
                        fi,
                        ni,
                        n.deps.iter().chain(n.inner_deps.iter()).collect::<Vec<_>>(),
                    )
                })
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
                        for target in &info.targets {
                            match &target.info {
                                AstInfo::Literal => {
                                    let TokenInfo::Ident(ident) = &target.token.info else {
                                        panic!("[INTERNAL ERR] `targets` node of `Var` node notan `Ident` node but some other `Literal`.");
                                    };

                                    globals.push(Global {
                                        name: ident.clone(),
                                        queued_idx: idx,
                                    });
                                }
                                AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, ident, _) => {
                                    let TokenInfo::Ident(ident) = &ident.token.info else {
                                        panic!("[INTERNAL ERR] `targets` node of `Var` node notan `Ident` node but some other `Literal`.");
                                    };

                                    globals.push(Global {
                                        name: ident.clone(),
                                        queued_idx: idx,
                                    });
                                }
                                _ => panic!("[INTERNAL ERR] target node in `Var` node not a `Literal` or `ConstrainedVarDeclTarget` node."),
                            }
                        }
                    }

                    AstInfo::Literal => {}
                    AstInfo::Unary(_, _) => {}
                    AstInfo::Binary(_, _, _) => {}
                    AstInfo::Block(_, _) => {}
                    AstInfo::Import(_) => {}
                    AstInfo::TypeValue(_) => unreachable!(),
                    AstInfo::TypeSignature(_) => {}
                }
            }
        }
    }

    fn resolve_dependencies_for_file(&mut self, file_idx: usize) -> Result<()> {
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
                let inner_deps = &mut node.inner_deps;

                let AstInfo::Block(AstBlockKind::Params, params) = &info.params.info else {
                    panic!("[INTERNAL ERR] `params` node is not a `Params` Block node.");
                };
                Self::resolve_dependencies_for_nodes(current_scope, deps, params);

                let AstInfo::Block(AstBlockKind::Block, body) = &info.body.info else {
                    panic!("[INTERNAL ERR] `body` node is not a `Block` Block node.");
                };
                Self::resolve_dependencies_for_nodes(current_scope, inner_deps, body);
            }
            AstInfo::Var(info) => {
                let deps = &mut node.deps;

                Self::resolve_dependencies_for_nodes(
                    current_scope,
                    deps,
                    info.targets.iter().filter(|t| {
                        let info = &t.info;
                        matches!(
                            info,
                            AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, _, _)
                        )
                    }),
                );
                Self::resolve_dependencies_for_nodes(current_scope, deps, &info.initializers);
            }

            AstInfo::Literal => {}
            AstInfo::Unary(_, _) => {}
            AstInfo::Binary(_, _, _) => {}
            AstInfo::Block(_, _) => {}
            AstInfo::Import(_) => {}
            AstInfo::TypeValue(_) => unreachable!(),
            AstInfo::TypeSignature(_) => {}
        }

        node.progress = QueuedProgress::DependenciesFound;
    }

    fn resolve_dependencies_for_nodes<'nodes>(
        current_scope: &mut Scope,
        deps: &mut Vec<Dependency>,
        nodes: impl IntoIterator<Item = &'nodes Ast>,
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
            AstInfo::Binary(kind, lhs, rhs) => {
                if *kind != AstBinaryKind::ConstrainedVarDeclTarget {
                    Self::resolve_dependencies_for_node(current_scope, deps, lhs);
                }
                Self::resolve_dependencies_for_node(current_scope, deps, rhs);
            }
            AstInfo::Block(_, nodes) => {
                Self::resolve_dependencies_for_nodes(current_scope, deps, nodes)
            }
            AstInfo::Fn(info) => todo!(),
            AstInfo::Var(info) => {
                Self::resolve_dependencies_for_nodes(
                    current_scope,
                    deps,
                    info.targets.iter().filter(|t| {
                        let info = &t.info;
                        matches!(
                            info,
                            AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, _, _)
                        )
                    }),
                );
                Self::resolve_dependencies_for_nodes(current_scope, deps, &info.initializers);
            }
            AstInfo::Import(info) => todo!(),
            AstInfo::TypeValue(_) => unreachable!(),
            AstInfo::TypeSignature(sig) => match sig.as_ref() {
                AstInfoTypeSignature::Function(params, returns) => {
                    let AstInfo::Block(AstBlockKind::Params, params) = &params.info else {
                        panic!("[INTERNAL ERR] `params` node in `TypeSignature` is not a `Params` node.");
                    };
                    Self::resolve_dependencies_for_nodes(current_scope, deps, params);

                    if let Some(returns) = returns {
                        Self::resolve_dependencies_for_node(current_scope, deps, returns);
                    }
                }
            },
        }
    }

    fn detect_circular_dependencies(&self) -> Result<()> {
        for file_idx in 0..self.files.len() {
            let len_nodes = self.files[file_idx].ast.len();
            for node_idx in 0..len_nodes {
                let queued_loc = Dependency::new(file_idx, node_idx);
                self.detect_circular_dependencies_for_queued(queued_loc)?;
            }
        }

        Ok(())
    }

    fn detect_circular_dependencies_for_queued(&self, queued_loc: Dependency) -> Result<()> {
        let queued = &self.files[queued_loc.parsed_file_idx].ast[queued_loc.queued_idx];
        let deps = &queued.deps;
        let mut seen = vec![];

        for &dep in deps {
            self.detect_circular_dependency(queued_loc, dep, dep, &mut seen)?;
        }

        Ok(())
    }

    fn detect_circular_dependency(
        &self,
        needle: Dependency,
        parent_dep: Dependency,
        dep: Dependency,
        seen: &mut Vec<Dependency>,
    ) -> Result<()> {
        if needle == dep {
            let err = self.generate_dependency_error(needle, parent_dep);
            return Err(err.into());
        }

        let recurse = !seen.contains(&dep);
        seen.push(dep);

        let needle_node = &self.files[needle.parsed_file_idx].ast[needle.queued_idx];
        let dep = &self.files[dep.parsed_file_idx].ast[dep.queued_idx];
        let deps = &dep.deps;
        let inner_deps = &dep.inner_deps;

        if recurse {
            for &child_dep in deps {
                self.detect_circular_dependency(needle, parent_dep, child_dep, seen)?;
            }

            if !matches!(needle_node.node.info, AstInfo::Fn(_)) {
                for &child_dep in inner_deps {
                    self.detect_circular_dependency(needle, parent_dep, child_dep, seen)?;
                }
            }
        }

        Ok(())
    }

    // @TODO:
    // Improve error message
    fn generate_dependency_error(&self, d1: Dependency, d2: Dependency) -> SourceError2 {
        let d1 = &self.files[d1.parsed_file_idx].ast[d1.queued_idx];
        let d2 = &self.files[d2.parsed_file_idx].ast[d2.queued_idx];

        SourceError2::new(
            "Circular dependency detected.",
            d1.node.token.loc,
            "This and...",
            d2.node.token.loc,
            "...this depend on each other.",
        )
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
