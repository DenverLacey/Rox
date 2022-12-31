use crate::{
    interp::{ParsedFile, FunctionInfo},
    ir::{
        annotations::Annotations,
        ast::{Ast, AstInfo, AstInfoFn, QueuedProgress},
    },
    util::errors::{Result, SourceError}, parsing::tokenization::Token, typing::value_type::Type,
};

use super::{exe::{Executable, ExecutableBuilder}, inst::Instruction};

pub fn compile_executable(files: &mut [ParsedFile]) -> Result<Executable> {
    let mut compiler = Compiler::new();

    for queued in files
        .iter_mut()
        .flat_map(|file| file.ast.iter_mut())
        .filter(|queued| is_node_compilable(&queued.node))
    {
        compiler.compile_node(&queued.node)?;
        queued.progress = QueuedProgress::Compiled;
    }

    compiler.exe.build()
}

fn is_node_compilable(node: &Ast) -> bool {
    match node.info {
        AstInfo::Import(_) | AstInfo::TypeSignature(_) => false,
        _ => true,
    }
}

#[derive(Default)]
struct FunctionBuilder {
    code: Vec<u8>,
}

struct Compiler {
    exe: ExecutableBuilder,
    func: FunctionBuilder,
}

impl Compiler {
    fn new() -> Self {
        Self {
            exe: ExecutableBuilder::new(),
            func: Default::default(),
        }
    }

    fn emit_byte(&mut self, byte: u8) {
        self.func.code.push(byte);
    }

    fn emit_inst(&mut self, inst: Instruction) {
        assert_eq!(std::mem::size_of::<Instruction>(), std::mem::size_of::<u8>());
        let byte = inst as u8;
        self.emit_byte(byte);
    }

    fn emit_value<T>(&mut self, value: T) 
    where 
        T: Copy + Sized,
    {
        let size = std::mem::size_of_val(&value);

        unsafe {
            let mut p = &value as *const T as *const u8;
            for _ in 0..size {
                self.emit_byte(*p);
                p = p.add(1);
            }
        }
    }
}

impl Compiler {
    fn compile_node(&mut self, node: &Ast) -> Result<()> {
        match &node.info {
            AstInfo::Literal => todo!(),
            AstInfo::Unary(kind, expr) => todo!(),
            AstInfo::Binary(kind, lhs, rhs) => todo!(),
            AstInfo::Block(kind, nodes) => todo!(),
            AstInfo::Fn(info) => self.parse_fn_decl(&node.token, info),
            AstInfo::Var(info) => todo!(),
            AstInfo::TypeValue(typ) => todo!(),

            AstInfo::Import(_) | AstInfo::TypeSignature(_) => unreachable!(),
            
        }
    }

    fn parse_fn_decl(&mut self, token: &Token, info: &AstInfoFn) -> Result<()> {
        todo!();

        let func = FunctionInfo {
            id: todo!(),
            name: todo!(),
            typ: todo!(),
            code: todo!(),
        };

        let is_entry_point = info.annons.contains(Annotations::FN_ENTRY);
        if is_entry_point {
            if !self.exe.set_entry_point(func.id.0) {
                return Err(SourceError::new("Multiple entry points declared.", token.loc, "This was declared as the entry point when another function already has.").into());
            }
        }

        self.exe.add_func(func);
        Ok(())
    }
}
