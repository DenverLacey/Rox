use std::collections::HashMap;

use crate::{
    interp::ParsedFile,
    ir::ast::{AstBlockKind, AstInfo, DependencyLocator},
    parsing::tokenization::TokenInfo,
};

pub struct Resolver<'files> {
    files: &'files mut Vec<ParsedFile>,
    globals: Vec<Vec<&'files str>>,
    scopes: Vec<Scope<'files>>,
}

impl<'files> Resolver<'files> {
    pub fn new(files: &'files mut Vec<ParsedFile>) -> Self {
        Self {
            files,
            globals: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn resolve_dependencies<'s: 'files>(&'s mut self) -> Result<(), &'static str> {
        self.register_all_globals()?;

        todo!()
    }
}

impl<'files> Resolver<'files> {
    fn register_all_globals<'s: 'files>(&'s mut self) -> Result<(), &'static str> {
        for file in self.files.iter() {
            self.globals.push(Vec::new());
            let globals = self.globals.last_mut().unwrap();

            let nodes = file.ast.program_nodes();

            for node in nodes {
                match &node.info {
                    AstInfo::Fn(info) => {
                        let TokenInfo::Ident(ident) = &info.ident.token.info else {
                            return Err("[INTERNAL ERR] `ident` node of `Fn` node not an `Ident` node.");
                        };

                        globals.push(ident);
                    }
                    AstInfo::Var(info) => {
                        if matches!(info.targets.info, AstInfo::Literal) {
                            let TokenInfo::Ident(ident) = &info.targets.token.info else {
                                return Err("[INTERNAL ERR] `targets` node of `Var` node not an `Ident` node but some other `Literal`.");
                            };

                            globals.push(ident);
                        } else if let AstInfo::Block(AstBlockKind::VarDeclTargets, targets) =
                            &info.targets.info
                        {
                            for target in targets {
                                let TokenInfo::Ident(ident) = &target.token.info else {
                                    return Err("[INTERNAL ERR] One of the targets of a `VarDeclTargets` node is not an `Ident` node.");
                                };

                                globals.push(ident);
                            }
                        } else {
                            return Err("[INTERNAL ERR] `targets` node of `Var` node not an `Ident` node or a `VarDeclTargets` node.");
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

        Ok(())
    }
}

struct Scope<'files> {
    locators: HashMap<&'files str, DependencyLocator>,
}
