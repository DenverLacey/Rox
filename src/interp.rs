use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use debug_print::debug_println as dprintln;

use crate::{
    canon::{
        resolve_deps::Resolver,
        scoping::{FuncID, Scope, ScopeBinding, Scoper},
    },
    codegen::{compile::compile_executable, exe::Executable},
    ir::ast::Queued,
    parsing::parsing::parse_file,
    runtime::vm::VM,
    typing::{
        typecheck::typecheck_program,
        value_type::{
            Type, TypeInfo, TypeInfoArray, TypeInfoEnum, TypeInfoFunction, TypeInfoPointer,
            TypeInfoStruct, TypeKind,
        },
    },
    util::errors::{error, Result},
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
    pub index: usize,
    pub filepath: PathBuf,
    pub source: String,
}

#[derive(Debug)]
pub struct ParsedFile {
    pub index: usize,
    pub filepath: PathBuf,
    pub ast: Vec<Queued>,
}

#[derive(Clone, Debug)]
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

    fn initialize(&mut self) {
        self.create_global_scope();
        self.load_prelude();
    }

    fn create_global_scope(&mut self) {
        let typ = self.get_or_create_function_type(TypeInfoFunction {
            params: Box::new([]),
            returns: None,
        });
        let id = self.create_function("<MAIN>", typ);
        assert_eq!(id.0, 0);
    }

    fn load_prelude(&mut self) {
        let mut prelude = Scope::new();

        prelude.add_binding_unchecked("Bool", ScopeBinding::Type(Type::of(TypeKind::Bool)));
        prelude.add_binding_unchecked("Char", ScopeBinding::Type(Type::of(TypeKind::Char)));
        prelude.add_binding_unchecked("Int", ScopeBinding::Type(Type::of(TypeKind::Int)));
        prelude.add_binding_unchecked("Float", ScopeBinding::Type(Type::of(TypeKind::Float)));
        prelude.add_binding_unchecked("String", ScopeBinding::Type(Type::of(TypeKind::String)));
        prelude.add_binding_unchecked("Range", ScopeBinding::Type(Type::of(TypeKind::Range)));
        prelude.add_binding_unchecked("Type", ScopeBinding::Type(Type::of(TypeKind::Type)));

        if self.scopes.is_empty() {
            self.scopes.push(prelude);
        } else {
            self.scopes[0] = prelude;
        }
    }

    pub fn get() -> &'static Self {
        unsafe { &INTERP }
    }

    pub fn get_mut() -> &'static mut Self {
        unsafe { &mut INTERP }
    }

    pub fn generate_executable(&mut self, path: impl AsRef<Path>) -> Result<Executable> {
        self.initialize();

        self.parse_source_code(path)?;

        self.establish_scopes()?;

        if cfg!(debug_assertions) {
            for f in &self.parsed_files {
                dprintln!("AST of {:?}:\n{:#?}\n", f.filepath, f.ast);
            }
        }

        self.resolve_dependencies()?;

        typecheck_program(&mut self.parsed_files)?;
        dprintln!(
            "typechecked ast:\n{:?}\n",
            self.parsed_files.first().unwrap().ast
        );

        if cfg!(debug_assertions) {
            for (i, s) in self.scopes.iter().enumerate() {
                dprintln!("[{}]: {:?}", i, s);
            }
            dprintln!("");
        }

        let exe = compile_executable(&mut self.parsed_files)?;

        Ok(exe)
    }

    pub fn execute_executable(&mut self, exe: &Executable) -> Result<()> {
        if cfg!(debug_assertions) {
            exe.print_instructions();
        }

        let mut vm = VM::new(exe);
        vm.execute()
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
                    return Type::of(TypeKind::Composite(idx));
                }
            }
        }

        let idx = self.types.len();
        let new_type = TypeInfo::Pointer(info);
        self.types.push(new_type);

        Type::of(TypeKind::Composite(idx))
    }

    pub fn get_or_create_array_type(&mut self, info: TypeInfoArray) -> Type {
        for (idx, typ) in self.types.iter().enumerate() {
            if let TypeInfo::Array(type_info) = typ {
                if *type_info == info {
                    return Type::of(TypeKind::Composite(idx));
                }
            }
        }

        let idx = self.types.len();
        let new_type = TypeInfo::Array(info);
        self.types.push(new_type);

        Type::of(TypeKind::Composite(idx))
    }

    pub fn create_struct_type(&mut self, info: TypeInfoStruct) -> Type {
        let idx = self.types.len();
        let new_type = TypeInfo::Struct(info);
        self.types.push(new_type);

        Type::of(TypeKind::Composite(idx))
    }

    pub fn create_enum_type(&mut self, info: TypeInfoEnum) -> Type {
        let idx = self.types.len();
        let new_type = TypeInfo::Enum(info);
        self.types.push(new_type);

        Type::of(TypeKind::Composite(idx))
    }

    pub fn get_or_create_function_type(&mut self, info: TypeInfoFunction) -> Type {
        for (idx, typ) in self.types.iter().enumerate() {
            if let TypeInfo::Function(type_info) = typ {
                if info.returns == type_info.returns
                    && info.params.iter().eq(type_info.params.iter())
                {
                    return Type::of(TypeKind::Composite(idx));
                }
            }
        }

        let idx = self.types.len();
        let new_type = TypeInfo::Function(info);
        self.types.push(new_type);

        Type::of(TypeKind::Composite(idx))
    }
}

impl Interpreter {
    fn parse_source_code(&mut self, path: impl AsRef<Path>) -> Result<()> {
        let path = path.as_ref();

        if !path.exists() {
            return Err(error!("Given path `{:?}` does not exist.", path));
        } else if path.is_dir() {
            dprintln!("[INFO] Directory given as project path.");

            let dir = std::fs::read_dir(path).map_err(|_| error!("Failed to read directory."))?;
            for dir_entry in dir {
                let path = dir_entry
                    .map_err(|_| error!("Failed to walk project directory."))?
                    .path();

                if path.extension() != Some(OsStr::new("rox")) {
                    continue;
                }

                self.load_and_parse_file(path)?;
            }
        } else if path.is_file() {
            self.load_and_parse_file(path)?;
        } else {
            return Err(error!("Given path is neither a file or a directory."));
        }

        Ok(())
    }

    fn load_and_parse_file(&mut self, path: impl AsRef<Path>) -> Result<()> {
        let path = path.as_ref();

        let loaded_file = load_file(self.loaded_files.len(), path)?;
        self.loaded_files.push(loaded_file);
        dprintln!("[INFO] Loaded file {:?}", path);

        let parsed_file = parse_file(self.loaded_files.last().unwrap())?;
        self.parsed_files.push(parsed_file);
        dprintln!("[INFO] Parsed file {:?}", path);

        Ok(())
    }

    fn establish_scopes(&mut self) -> Result<()> {
        let mut scoper = Scoper::new(&mut self.scopes);

        for file in &mut self.parsed_files {
            let nodes = file.ast.iter_mut().map(|q| &mut q.node);
            scoper.establish_scope_for_file(nodes)?;
        }

        Ok(())
    }

    fn resolve_dependencies(&mut self) -> Result<()> {
        let mut resolver = Resolver::new(&mut self.parsed_files);
        resolver.resolve_dependencies()?;
        Ok(())
    }
}

fn load_file<P: AsRef<Path>>(index: usize, path: P) -> Result<LoadedFile> {
    let path = path.as_ref();

    let source = std::fs::read_to_string(path).map_err(|_| error!("Failed to read file."))?;
    Ok(LoadedFile {
        index,
        filepath: path
            .canonicalize()
            .map_err(|_| error!("Failed to canonicalize project path."))?,
        source,
    })
}
