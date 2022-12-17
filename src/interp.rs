use std::{collections::HashMap, ffi::OsStr, path::{PathBuf, Path}};

use debug_print::debug_println as dprintln;

use crate::{parsing::parsing::parse_file, ir::ast::{Ast, AstInfo, AstBlockKind}, typing::value_type::{Type, CompositeType}};

#[derive(Default)]
pub struct Interpreter {
    pub loaded_files: Vec<LoadedFile>,
    pub parsed_files: Vec<ParsedFile>,
    pub scopes: Vec<Scope>,
    pub funcs: Vec<FunctionInfo>,
    pub types: Vec<CompositeType>,
}

#[derive(Debug)]
pub struct LoadedFile {
    pub filepath: PathBuf,
    pub source: String,
}

#[derive(Debug)]
pub struct ParsedFile {
    pub filepath: PathBuf,
    pub ast: Ast,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeIndex(pub usize);

#[derive(Default)]
pub struct Scope {
    pub parent: Option<ScopeIndex>,
    // pub children: Vec<ScopeIndex>,
    pub bindings: HashMap<String, ScopeBinding>,
}

pub enum ScopeBinding {
    Var(VariableBinding),
    Func(FunctionBinding),
}

pub struct VariableBinding {
    pub is_mut: bool,
    pub typ: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncID(pub usize);

pub struct FunctionBinding {
    pub id: FuncID,
    pub typ: Type,
}

pub struct FunctionInfo {
    pub id: FuncID,
    pub name: String,
    pub typ: Type,
    pub code: Box<[u8]>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn generate_program(&mut self, path: impl AsRef<Path>) -> Result<(), &'static str> {
        self.parse_program(path)?;
        self.establish_scopes()?;

        Ok(())
    }

    pub fn execute_program(&mut self) -> Result<(), &'static str> {
        todo!()
    }
}

impl Interpreter {
    fn parse_program(&mut self, path: impl AsRef<Path>) -> Result<(), &'static str> {
        let path = path.as_ref();

        if !path.exists() {
            return Err("Given path does not exist.");
        } else if path.is_dir() {
            dprintln!("[INFO] Directory given as project path.");

            let dir = std::fs::read_dir(path).map_err(|_| "Failed to read directory.")?;
            for dir_entry in dir {
                let path = dir_entry
                    .map_err(|_| "Failed to walk project directory.")?
                    .path();

                if path.extension() != Some(OsStr::new("rox")) {
                    continue;
                }

                self.load_and_parse_file(path)?;
            }
        } else if path.is_file() {
            self.load_and_parse_file(path)?;
            dprintln!("[INFO] AST of parsed file {:?}\n{:?}", path, self.parsed_files[0].ast);
        } else {
            return Err("Given path is neither a file or a directory.");
        }

        Ok(())
    }

    fn load_and_parse_file(&mut self, path: impl AsRef<Path>) -> Result<(), &'static str> {
        let path = path.as_ref();

        let loaded_file = load_file(path)?;
        dprintln!("[INFO] Loaded file {:?}", path);

        let parsed_file = parse_file(&loaded_file)?;
        dprintln!("[INFO] Parsed file {:?}", path);

        self.loaded_files.push(loaded_file);
        self.parsed_files.push(parsed_file);

        Ok(())
    }

    fn establish_scopes(&mut self) -> Result<(), &'static str> {
        for file in &mut self.parsed_files {
            if let Ast { token: _, scope: _, typ: _, info: AstInfo::Block(AstBlockKind::Program, nodes) } = &mut file.ast {
                Self::establish_scope_for_file(&mut self.scopes, nodes)?;
            } else {
                return Err("[INTERNAL ERR] Ast node of parsed file not a Program ndoe.");
            }
        }

        Ok(())
    }

    fn establish_scope_for_file(scopes: &mut Vec<Scope>, nodes: &mut Vec<Ast>) -> Result<(), &'static str> {
        let global_scope = ScopeIndex(scopes.len());
        scopes.push(Scope::default());

        for node in nodes {
            node.scope = global_scope;

            match node.info {
                _ => todo!(),
            }
        }

        Ok(())
    }
}

fn load_file<P: AsRef<Path>>(path: P) -> Result<LoadedFile, &'static str> {
    let path = path.as_ref();

    let source = std::fs::read_to_string(path).map_err(|_| "Failed to read file.")?;
    Ok(LoadedFile {
        filepath: path
            .canonicalize()
            .map_err(|_| "Failed to canonicalize project path.")?,
        source,
    })
}
