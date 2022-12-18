use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use debug_print::debug_println as dprintln;

use crate::{
    canon::scoping::{FuncID, Scope, Scoper},
    ir::ast::{Ast, AstBlockKind, AstInfo},
    parsing::parsing::parse_file,
    typing::value_type::{CompositeType, Type},
};

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

#[derive(Debug)]
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

        self.parsed_files.iter().for_each(|f| {
            dprintln!("AST of {:?}:\n{:#?}\n", f.filepath, f.ast);
        });

        self.scopes.iter().enumerate().for_each(|(i, s)| {
            dprintln!("[{}]: {:?}", i, s);
        });

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
        let mut scoper = Scoper::new(&mut self.scopes);

        for file in &mut self.parsed_files {
            if let Ast {
                token: _,
                scope: _,
                typ: _,
                info: AstInfo::Block(AstBlockKind::Program, nodes),
            } = &mut file.ast
            {
                scoper.establish_scope_for_file(nodes)?;
            } else {
                return Err("[ERR] Ast node of parsed file not a Program ndoe.");
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
