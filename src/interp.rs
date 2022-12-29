use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use debug_print::debug_println as dprintln;

use crate::{
    canon::{
        resolve_deps::Resolver,
        scoping::{FuncID, Scope, Scoper},
    },
    ir::ast::Queued,
    parsing::parsing::parse_file,
    typing::{
        typecheck::typecheck_program,
        value_type::{
            Type, TypeInfo, TypeInfoArray, TypeInfoFunction, TypeInfoPointer, TypeInfoRecord,
        },
    }, codegen::compile::compile_program,
};

static mut INTERP: Interpreter = Interpreter::new();

#[derive(Default)]
pub struct Interpreter {
    pub loaded_files: Vec<LoadedFile>,
    pub parsed_files: Vec<ParsedFile>,
    pub scopes: Vec<Scope>,
    pub funcs: Vec<FunctionInfo>,
    pub types: Vec<TypeInfo>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pid(pub usize);

#[derive(Debug)]
pub struct LoadedFile {
    pub filepath: PathBuf,
    pub source: String,
}

#[derive(Debug)]
pub struct ParsedFile {
    pub filepath: PathBuf,
    pub ast: Vec<Queued>,
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub id: FuncID,
    pub name: String,
    pub typ: Type,
    pub code: Option<Box<[u8]>>,
}

impl Interpreter {
    const fn new() -> Self {
        Self {
            loaded_files: Vec::new(),
            parsed_files: Vec::new(),
            scopes: Vec::new(),
            funcs: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn get() -> &'static Self {
        unsafe { &INTERP }
    }

    pub fn get_mut() -> &'static mut Self {
        unsafe { &mut INTERP }
    }

    pub fn generate_program(&mut self, path: impl AsRef<Path>) -> Result<(), &'static str> {
        self.parse_program(path)?;

        self.establish_scopes()?;

        if cfg!(debug_assertions) {
            for f in &self.parsed_files {
                dprintln!("AST of {:?}:\n{:?}\n", f.filepath, f.ast);
            }

            for (i, s) in self.scopes.iter().enumerate() {
                dprintln!("[{}]: {:?}", i, s);
            }
            dprintln!("");
        }

        self.resolve_dependencies()?;

        typecheck_program(&mut self.parsed_files)?;
        dprintln!(
            "typechecked ast:\n{:?}\n",
            self.parsed_files.first().unwrap().ast
        );

        compile_program(&self.parsed_files)?;

        Ok(())
    }

    pub fn execute_program(&mut self) -> Result<(), &'static str> {
        todo!()
    }
}

impl Interpreter {
    pub fn create_function(&mut self, name: impl Into<String>, typ: Type) -> FuncID {
        let func_id = FuncID(self.funcs.len());

        let info = FunctionInfo {
            id: func_id,
            name: name.into(),
            typ,
            code: None,
        };

        self.funcs.push(info);

        func_id
    }

    pub fn get_or_create_pointer_type(&mut self, info: TypeInfoPointer) -> Type {
        for (idx, typ) in self.types.iter().enumerate() {
            if let TypeInfo::Pointer(type_info) = typ {
                if *type_info == info {
                    return Type::Composite(idx);
                }
            }
        }

        let idx = self.types.len();
        let new_type = TypeInfo::Pointer(info);
        self.types.push(new_type);

        Type::Composite(idx)
    }

    pub fn get_or_create_array_type(&mut self, info: TypeInfoArray) -> Type {
        for (idx, typ) in self.types.iter().enumerate() {
            if let TypeInfo::Array(type_info) = typ {
                if *type_info == info {
                    return Type::Composite(idx);
                }
            }
        }

        let idx = self.types.len();
        let new_type = TypeInfo::Array(info);
        self.types.push(new_type);

        Type::Composite(idx)
    }

    pub fn create_record_type(&mut self, info: TypeInfoRecord) -> Type {
        let idx = self.types.len();
        let new_type = TypeInfo::Record(info);
        self.types.push(new_type);

        Type::Composite(idx)
    }

    pub fn get_or_create_function_type(&mut self, info: TypeInfoFunction) -> Type {
        for (idx, typ) in self.types.iter().enumerate() {
            if let TypeInfo::Function(type_info) = typ {
                if info.returns == type_info.returns
                    && info.params.iter().eq(type_info.params.iter())
                {
                    return Type::Composite(idx);
                }
            }
        }

        let idx = self.types.len();
        let new_type = TypeInfo::Function(info);
        self.types.push(new_type);

        Type::Composite(idx)
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
        scoper.load_prelude();

        for file in &mut self.parsed_files {
            let nodes = file.ast.iter_mut().map(|q| &mut q.node);
            scoper.establish_scope_for_file(nodes)?;
        }

        Ok(())
    }

    fn resolve_dependencies(&mut self) -> Result<(), &'static str> {
        let mut resolver = Resolver::new(&mut self.parsed_files);
        resolver.resolve_dependencies()?;
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
